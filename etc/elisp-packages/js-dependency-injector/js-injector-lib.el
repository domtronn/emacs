;;; js-injector-lib.el --- Common library funtions for js-injector modules

;; Copyright (C) 2014  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: convenience, abbrev, tools
;; Package-Requires: ((emacs "24") (dash "2.11.0") (s "1.11.0") (js2-mode "20140114"))

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

;; This module has functions common functions that are used between
;; the two modules of the js-injector

;;; Code:

;;; Utility definitions
(defun js-injector-replace-region (start end replacement)
  "Replace a region from START to END with REPLACEMENT string."
  (goto-char start)
  (delete-region start end)
  (insert replacement))

(defun js-injector--get-quote-char ()
  "Get the majority quote character used in a file."
  (if (> (count-matches "\"" (point-min) (point-max))
         (count-matches "'" (point-min) (point-max)))
      "\"" "'"))

(defun js-injector--parse-version (s)
  "Parse a semver version number S scheme seen in node package.json.
e.g.  `>=0.10.3 ~0.12` etc"
  (when s
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (search-forward-regexp ">\\|>=\\|^\\|~\\|v")
      (search-forward-regexp "\\([0-9]+\\(?:\.[0-9]+\\)+\\)")
      (match-string 0))))

(defun js-injector-relativise (files containing-dir)
  "Maps all file paths in FILES to be relative to CONTAINING-DIR."
  (cl-loop for file-alist in files
           for file = (car file-alist)
           for locations = (cdr file-alist)
    collect
    (cons file (--map (file-relative-name it containing-dir) locations))))

(defun js-injector--read-dependencies (module dependencies &optional popup-point)
  "Prompt user for a dependency from DEPENDENCIES.
If POPUP-POINT is non-nil, use a `popup-menu*` rather than a
  `completing-read`."
  (if (> (length dependencies) 1)
      (if popup-point
          (popup-menu* dependencies :point popup-point)
        (completing-read (format "Import '%s' from: " module) dependencies))
    (car dependencies)))

(defun js-injector--count-occurences (regex string start)
  (if (string-match regex string start)
      (+ (if (js2-string-node-p (js2-node-at-point (match-end 0))) 0 1)
         (js-injector--count-occurences regex string (match-end 0))) 0))

(defun js-injector-describe-modules (&optional pfx)
  "List imports of a module."
  (interactive "P")
  (let* ((modules (if (or pfx (js-injector--requirejs-file?))
                      (js-injector-get-dependency-alist)
                    (js-injector-get-relative-dependency-alist)))
         (module  (completing-read "Module: " modules))
         (imports (--map (cons it t) (cdr (eval `(assoc ,module modules)))))
         (import  (if (> (length imports) 1)
                      (completing-read "Import: " imports)
                    (caar imports))))
    (kill-new module)
    (kill-new import)
    (message "[js-injector] Adding '%s' as '%s' to the kill ring" module import)))

;;; 3rd Party helper functions
;; projectile
(defun js-injector--get-projectile-files-alist ()
  (--map (if (and (file-name-directory it)
                  (string-match "index" (file-name-nondirectory it)))

             (list (format "%s.js" (file-name-nondirectory (substring (file-name-directory it) 0 -1)))
                   (format "%s%s" (projectile-project-root) (substring (file-name-directory it) 0 -1)))

           (list (file-name-nondirectory it)
                 (format "%s%s" (projectile-project-root) it)))
         (projectile-current-project-files)))
(defun js-injector--get-projectile-relative-requirejs-alist ()
  (list (cons (projectile-project-name) (js-injector-relativise (js-injector--get-projectile-files-alist) (file-name-directory (buffer-file-name))))))
(defun js-injector--get-projectile-relative-requirejs-config ()
  (list (cons (projectile-project-name) "//-projectile/?")))

;; jpop
(defun js-injector--get-jpop-files-alist () (when (fboundp 'jpop-file-alist) jpop-file-alist))
(defun js-injector--get-jpop-relative-requirejs-alist ()
  (list (cons jpop-id (js-injector-relativise jpop-file-alist (buffer-file-name)))))
(defun js-injector--get-jpop-requirejs-alist ()
  (let ((lib-ids (--map (plist-get it :id) (append (plist-get jpop-project-plist :libs) nil))))
    (--filter (-contains? lib-ids (car it)) jpop-project-alist)))
(defun js-injector--get-jpop-relative-requirejs-config () (list (cons jpop-id "//-jpop/?")))
(defun js-injector--get-jpop-requirejs-config ()
  (let ((configs (append (plist-get jpop-project-plist :libs) nil)))
    (mapcar (lambda (config) (cons (plist-get config :id) (plist-get config :dir))) configs)))

;; Git
(defun js-injector--get-git-files-alist ()
  (let ((git-repo (locate-dominating-file (buffer-file-name) ".git")))

    (when (not git-repo)
      (error "[js-injector] File is not part of a git project [%s]"
             (file-name-directory (buffer-file-name))))

    (let* ((containing-dir (file-name-directory (buffer-file-name)))
           (git-cmd (format "git ls-files %s -zco --full-name --exclude-standard" git-repo))
           (files (--map
                   (expand-file-name (format "%s%s" git-repo it))
                   (split-string (shell-command-to-string git-cmd) "\0" t))))

      (--map (list (file-name-nondirectory it) it) files))))

(provide 'js-injector-lib)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (nameless-mode 1)
;; eval: (flycheck-mode 0)
;; End:
;;; js-injector-lib.el ends here
