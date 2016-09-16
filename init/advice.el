;;; advice.el --- List of function advice

;; Copyright (C) 2014  Dominic Charlesworth <dgc336@gmail.com>

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
;;; common functions used in Emacs

;;; Code:

;; Disable all themes before loading a new one
(defun disable-themes (&rest args)
  (disable-all-themes))

(defun preserve-font (orig-f &rest args)
  (let ((current-font (assoc (face-attribute 'default :family) font-list)))
    (apply orig-f args)
    (set-font current-font)))

(defun remove-mode-line-box (&rest args)
  (set-face-attribute 'mode-line nil :box nil :underline nil)
  (set-face-attribute 'mode-line-inactive nil :box nil :underline nil))

(defun markdown-style-themes (&rest args)
  (let ((class '((class color) (min-colors 89))))
    (custom-set-faces
     '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold))))
     '(markdown-header-face-1 ((t (:inherit font-lock-function-name-face :weight bold :height 1.8))))
     '(markdown-header-face-2 ((t (:inherit font-lock-function-name-face :weight bold :height 1.4))))
     '(markdown-header-face-3 ((t (:inherit font-lock-function-name-face :weight bold :height 1.2)))))))

(eval-after-load "neotree"
  (defun neo-buffer--insert-fold-symbol (name &optional file-name)
    "Custom overriding function for the fold symbol.
`NAME' decides what fold icon to use, while `FILE-NAME' decides
what file icon to use."
    (or (and (equal name 'open)  (insert (all-the-icons-icon-for-dir file-name "down")))
        (and (equal name 'close) (insert (all-the-icons-icon-for-dir file-name "right")))
        (and (equal name 'leaf)  (insert (format "\t\t\t%s\t" (all-the-icons-icon-for-file file-name))))))

  (defun neo-buffer--insert-dir-entry (node depth expanded)
    (let ((node-short-name (neo-path--file-short-name node)))
      (insert-char ?\s (* (- depth 1) 2)) ; indent
      (when (memq 'char neo-vc-integration)
        (insert-char ?\s 2))
      (neo-buffer--insert-fold-symbol
       (if expanded 'open 'close) node)
      (insert-button (concat node-short-name "/")
                     'follow-link t
                     'face neo-dir-link-face
                     'neo-full-path node
                     'keymap neotree-dir-button-keymap)
      (neo-buffer--node-list-set nil node)
      (neo-buffer--newline-and-begin)))

  (defun neo-buffer--insert-file-entry (node depth)
    (let ((node-short-name (neo-path--file-short-name node))
          (vc (when neo-vc-integration (neo-vc-for-node node))))
      (insert-char ?\s (* (- depth 1) 2)) ; indent
      (when (memq 'char neo-vc-integration)
        (insert-char (car vc))
        (insert-char ?\s))
      (neo-buffer--insert-fold-symbol 'leaf node-short-name)
      (insert-button node-short-name
                     'follow-link t
                     'face (if (memq 'face neo-vc-integration)
                               (cdr vc)
                             neo-file-link-face)
                     'neo-full-path node
                     'keymap neotree-file-button-keymap)
      (neo-buffer--node-list-set nil node)
      (neo-buffer--newline-and-begin))))

(advice-add 'load-theme :before 'disable-themes)
(advice-add 'counsel-load-theme :around 'preserve-font)
(advice-add 'counsel-load-theme :after 'markdown-style-themes)
(advice-add 'counsel-load-theme :after 'remove-mode-line-box)

(advice-add 'ansi-term :after '(lambda (&rest r) (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

(provide 'advice)
;;; advice.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
