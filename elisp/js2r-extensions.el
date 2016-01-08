;;; js2r-extensions.el --- List of my own functions

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Extensions for the js2r package

;;; Code:

(require 'js2-refactor)

(defun js2r-join-var-declaration ()
  "Join variable declarations into comma separated list."
  (interactive)
  (let* ((restore-point (point))
         (region (js2r--var-decl-region))
         (region-string (apply 'buffer-substring region))
         (region-replacement
          (with-temp-buffer
            (insert region-string)
            (while (search-backward-regexp
                    "\\(;\\)\\s-*\n\\s-*\\(var\\)" (point-min) t)
              (replace-match "," nil nil nil 1)
              (replace-match "   " nil t nil 2))
            (buffer-string))))
    (goto-char (car region))
    (apply 'delete-region region)
    (insert region-replacement)))

(defun js2r--var-decl-joint-region ()
  (let* ((decl (js2r--closest 'js2-var-decl-node-p))
         (stmt (js2-node-parent-stmt decl)))
    (list (js2-node-abs-pos stmt) (js2-node-abs-end stmt))))

(defun js2r--var-decl-split-region ()
  (let* ((declaration (js2r--closest 'js2-var-decl-node-p))
         (upper-limit (or (save-excursion (search-backward-regexp "^\s*$" (point-min) t)) (point-min)))
         (lower-limit (or (save-excursion (search-forward-regexp "^\s*$" (point-max) t)) (point-max)))
         (upper (save-excursion
                  (goto-char lower-limit)
                  (while (search-backward "var" upper-limit t))
                  (beginning-of-line)
                  (point)))
         (lower (save-excursion
                  (goto-char upper-limit)
                  (while (search-forward "var" lower-limit t))
                  (end-of-line)
                  (point))))
    (list upper lower)))

(defun js2r--var-decl-region ()
  (cond
   ((eq 'split (js2r--get-declaration-type)) (js2r--var-decl-split-region))
   ((eq 'join (js2r--get-declaration-type)) (js2r--var-decl-joint-region))
   (t nil)))

(defun js2r--goto-first-var-decl ()
  (goto-char (car (js2r--var-decl-region))))

(defun js2r--get-declaration-type ()
  (let* ((decl (js2r--closest 'js2-var-decl-node-p)))
    (when decl
      (if decl
          (if (> (length (js2-var-decl-node-kids decl)) 1)
              'join 'split)
        nil))))

(defun js2r-toggle-var-declaration ()
  (interactive)
  (save-excursion
    (let ((decl-type (js2r--get-declaration-type)))
      (cond
       ((eq decl-type 'split) (js2r-join-var-declaration))
       ((eq decl-type 'join)
        (js2r--goto-first-var-decl)
        (js2r-split-var-declaration))
       (t (error "Not currently in a variable block"))))))

(defun js2r--is-nth-joint-var-decl (f)
  (let* ((decl (js2r--closest 'js2-var-decl-node-p))
         (kids (js2-var-decl-node-kids decl))
         (nth-kid (js2-node-string (funcall f kids)))
         (line (buffer-substring
                      (line-beginning-position)
                      (line-end-position))))
    (string-match nth-kid line)))

(defun js2r--is-first-joint-var-decl ()
  (js2r--is-nth-joint-var-decl (lambda (k) (car k))))

(defun js2r--is-last-joint-var-decl ()
  (js2r--is-nth-joint-var-decl (lambda (k) (car (last k)))))

(defun js2r--drag-advice (f p)
  (let ((declaration-type (js2r--get-declaration-type))
        (restore-point (point))
        (should-split (funcall p)))
    (when should-split
      (js2r-split-var-declaration)
      (goto-char restore-point))
    (funcall f 1)
    (when should-split
      (let ((restore-point (point)))
        (js2r-join-var-declaration)
        (goto-char restore-point)))))

(defun js2r-drag-stuff-up ()
  (interactive)
  (js2r--drag-advice 'drag-stuff-up
   (lambda () (and (eq (js2r--get-declaration-type) 'join)
              (or (js2r--is-last-joint-var-decl)
                  (save-excursion (forward-line -1)
                                  (js2r--is-first-joint-var-decl)))))))

(defun js2r-drag-stuff-down ()
  (interactive)
  (js2r--drag-advice
   'drag-stuff-down
   (lambda () (and (eq (js2r--get-declaration-type) 'join)
              (or (js2r--is-first-joint-var-decl)
                  (save-excursion (forward-line 1)
                                  (js2r--is-last-joint-var-decl)))))))

(defun js2r--get-varname (s)
  (cadr (s-split-words s)))

(defun js2r-order-by (pred)
  (let ((declaration-type (js2r--get-declaration-type)))
    (when (eq 'join declaration-type)
      (js2r-split-var-declaration))
    (save-excursion
      (let* ((region (js2r--var-decl-region))
             (region-string (apply 'buffer-substring region))
             (string-list (split-string region-string "\n"))
             (sorted (mapconcat 'identity (-sort pred string-list) "\n")))
        (goto-char (car region))
        (apply 'delete-region region)
        (insert sorted)
        (when (eq 'join declaration-type)
          (js2r-join-var-declaration))))))

(defun js2r-order-vars-by-name-length ()
  (interactive)
  (js2r-order-by (lambda (it other) (< (length (js2r--get-varname it))
                                  (length (js2r--get-varname other))))))

(defun js2r-order-vars-by-full-length ()
  (interactive)
  (js2r-order-by (lambda (it other) (< (length it)
                                  (length other)))))

(defun js2r-order-vars-by-assigned-length ()
  (interactive)
  (js2r-order-by (lambda (it other)
                   (let ((l-it (length (s-split-words it)))
                         (l-other (length (s-split-words other))))
                     (if (eq l-it l-other)
                         (< (length it)
                            (length other))
                       (< l-it
                          l-other))))))

(defun js2r-order-vars-by-require-path ()
  (interactive)
  (js2r-order-by (lambda (it other)
                   (let* ((regex "require\('\\(.*?\\)'\)")
                          (path-it (if (string-match regex it)
                                       (match-string 1 it) ""))
                          (path-other (if (string-match regex other)
                                          (match-string 1 other) "")))
                     (string< path-it path-other)))))

(provide 'js2r-extensions)
;;; js2r-extensions.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
