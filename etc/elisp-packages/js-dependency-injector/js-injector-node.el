;;; js-injector-node.el --- Inject paths to JS classes for node projects

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

;; This module has functions specific to dependency
;; injection/requiring files for node projects

;;; Code:

(require 'dash)
(require 's)

(require 'js-injector-lib)

;;; Group Definitions
(defgroup js-injector-node nil
  "Manage the node specific customs for js-injector."
  :group 'js-injector
  :group 'convenience)

(defvar js-injector-node-executable (executable-find "node")
  "Executable path for `node`.")

(defcustom js-injector-node-lib-alias-alist
  '(("ramda" . "R")
    ("lodash" . "_")
    ("underscore" . "_")
    ("jquery" . "$")
    ("react" . "React")
    ("supertest" . "request")
    ("q" . "Q"))
  "Alist of node libraries and their 'nice' require names."
  :group 'js-injector-node
  :type '(alist :key-type string :value-type string))

(defcustom js-injector-node-lib-list
  '("fs" "child_process" "events" "url" "path")
  "A list of node global libraries like FS and child process."
  :group 'js-injector-node)

(defcustom js-injector-node-require-format "const %s = require('%s')"
  "The format to use for requiring in a package in node.
It expects two arguments for formatting, the module name and the module require path.")

(defun js-injector-node-toggle-es6-format ()
  "Toggle the format for requires between `import' and `const'."
  (interactive)
  (setq
   js-injector-node-require-format
   (if (string-match "^import" js-injector-node-require-format)
       "const %s = require('%s')"
     "import %s from '%s'"))
  (message "Require format is now set to '%s'" js-injector-node-require-format))

(defcustom js-injector-node-camelise '(lambda (s) (apply 's-concat (s-split-words s)))
  "How to sanitise the import names."
  :type '(radio
          (const :tag "Relative to file Name"           (lambda (s) (apply 's-concat (s-split-words s))))
          (const :tag "Upper camel case   e.g. FooBar"  s-upper-camel-case)
          (const :tag "Lower camel case   e.g. fooBar"  s-lower-camel-case)
          (const :tag "Snake case         e.g. foo_bar" s-snake-case))
  :group 'js-injector-node)

;;; Get definitions
;;  Functions to get various bits of information required for dependencies
(defun js-injector-node-get-node-modules (&optional get-dev-dependencies)
  "Get a list of node packages defined in `package.json`.
When called with GET-DEV-DEPENDENCIES, this function will return
a distinct list of both the dev and production dependencies."
  (let ((package-dir
         (locate-dominating-file default-directory "package.json")))

    (unless package-dir
      (error "Could not find a package.json in the current project"))

    (let* ((json-object-type 'alist)
           (json-alist (json-read-file (format "%s/package.json" package-dir))))
      
      (-distinct
       (-flatten-n 1
        (--map
         (-map 'car (cdr (assoc it json-alist)))
         (append '(dependencies)
                 (when get-dev-dependencies '(devDependencies)))))))))

(defun js-injector-node-get-node-module-alist ()
  "Get a list of node modules associated with their nice names."
  (let ((node-modules (js-injector-node-get-node-modules t)))
    (append
     (--map (cons it (list (symbol-name it))) node-modules)
     (--map (cons it (list it)) js-injector-node-lib-list))))

(defun js-injector-node-get-var-declarations ()
  "Get a list of the variable declarations in the current buffer."
  (save-excursion
    (let (result)
      (with-current-buffer (buffer-name)
        (goto-char (point-min))
        (while (search-forward-regexp
                "\\(var\\|let\\|const\\)\\s-+\\([a-z_][a-z_0-9]*\\)" nil t)
          (setq result (append (list (match-string-no-properties 2)) result))))
      result)))

(defun js-injector-node-get-var-declarations-count ()
  "Get a list of the variable declarations in the current buffer."
  (let ((vars (js-injector-node-get-var-declarations)))
    (--map (cons it (js-injector--count-occurences (format "\\b%s\\b" it) (buffer-string) 0)) vars)))

(defun js-injector-node-get-unused-vars ()
  "Get a list of the unused variables in the current buffer."
  (-map 'car (--filter (eq 1 (cdr it)) (js-injector-node-get-var-declarations-count))))

(defun js-injector-node--nice-name (name)
  "Return the nice node NAME defined in `js-injector-node-lib-alias-alist`."
  (or (cdr (assoc name js-injector-node-lib-alias-alist)) (funcall js-injector-node-camelise name)))

(defun js-injector-node--var-decl? ()
  "Check whether you're in a variable declaration.
If you are, return the variable name currently being defined."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (require-match (s-match "\\(var\\|const\\|let\\)\\s-+\\([a-z_][a-z_0-9]*\\)\\s-+=\\s-+require(.*" line))
         (assign-match (s-match "\\(var\\|const\\|let\\)\\s-+\\([a-z_][a-z_0-9]*\\)\\s-+=\\s-+\\([a-z_][a-z_0-9]*\\)" line))
         (declare-match (s-match "\\(var\\|const\\|let\\)\\s-+\\([a-z_][a-z_0-9]*\\)" line))
         (var-match (s-match "\\(var\\|let\\|const\\)" line)))
    (cond
     (require-match (cons 'require 'js-injector-node-update-module-at-point))
     (assign-match (cons 'assign (apply-partially 'js-injector-node-import (car (last assign-match)))))
     (declare-match (cons 'declare (apply-partially 'js-injector-node-import (car (last declare-match)))))
     (var-match (cons 'var 'js-injector-node-import-module)))))

(defun js-injector-node--get-var-require ()
  "Return the require path and the region it occupies."
  (goto-char (line-beginning-position))
  (let ((beg (search-forward-regexp "require(['\"]"))
        (end (- (search-forward-regexp "['\"])") 2)))
    (cons (file-name-base (buffer-substring-no-properties beg end)) (list beg end))))

;;; Version calculation definitions
;;  Functions to calculate the version of node in use in a project

(defun js-injector-node-version>4? ()
  "Guess the node version being used in the project.

This will try to read it from the `package.json` engine field,
  otherwise fall back to reading from `.node-version`, or finally
  executing `node --version`."
  (let ((node-version (or (ignore-errors (js-injector-node--package-version))
                          (ignore-errors (js-injector-node--dot-version))
                          (js-injector-node--version))))

    (version<= "4" node-version)))

(defun js-injector-node--package-version ()
  "Read the node version from the `package.json` file."
  (let ((package-dir (locate-dominating-file default-directory "package.json")))
    (unless package-dir (error "Project does not contain a `package.json`"))
    (let* ((json-object-type 'alist)
           (json-alist (json-read-file (format "%s/package.json" package-dir)))
           (engines (cdr (assoc 'engines json-alist))))
      (js-injector--parse-version (cdr (assoc 'node engines))))))

(defun js-injector-node--dot-version ()
  "Read the node version from the `.node-version` file."
  (let ((node-version-dir (locate-dominating-file default-directory ".node-version")))
    (unless node-version-dir (error "Project does not contain a `.node-version`"))
    (with-temp-buffer
      (insert-file-contents (format "%s/.node-version" node-version-dir))
      (s-trim (s-collapse-whitespace (buffer-string))))))

(defun js-injector-node--version ()
  "Find the node version by running `node --version`."
  (unless js-injector-node-executable (error "You do not have `node` executable available"))
  (js-injector--parse-version
   (shell-command-to-string (format "%s --version" js-injector-node-executable))))

;;; Navigation definitions
;;  Functions to navigate around the requirejs file

(defun js-injector-node--has-require? ()
  "Check whether the buffer has a require in it."
  (not (eq nil (string-match "require" (buffer-string)))))

(defun js-injector-node--goto-first-import ()
  "Navigate to the first import/require in the file."
  (goto-char (point-min))
  (search-forward-regexp "['\"]use strict[\"'].*?[;]\\{0,1\\}" nil t)
  (skip-chars-forward " \n\t")
  (search-forward-regexp "require(\\|import" nil t)
  (beginning-of-line))

(defun js-injector-node--goto-end-of-import ()
  "Navigate to the end import/require block at the end of the file."
  (if (not (js-injector-node--has-require?))
      (goto-char (point-min))
    (js-injector-node--goto-first-import)
    (while (search-forward-regexp "^\\(let\\|const\\|var\\).*require(.*$" nil t))
    (when (not (looking-back ";"))
      (search-forward-regexp ".*;" nil t))
    (forward-char 1)))

;;; Interactive Injector functions

(defun js-injector-node-import (module &optional prompt-name pos)
  "Import MODULE as a dependency relative to current file.
This function will look for MODULE in a dependency list relative
to the current file and add it as an import/require statement at
the top of the file.

If PROMPT-NAME is non-nil, this function will prompt the user for
the name they would like to import the module as.

If POS is non-nil, inject the dependency at position."
  (let* ((dependency-alist (append
                            (js-injector-get-relative-dependency-alist)
                            (ignore-errors (js-injector-node-get-node-module-alist))))
         (dependency-match (assoc-string module dependency-alist t))
         (dependencies (cdr dependency-match))

         (import-module (js-injector--read-dependencies module dependencies))
         (import-name (when (and import-module prompt-name)
                        (read-string (format "Import '%s' as: " module)))))

    (unless import-module
      (error "No module named '%s'" module))

    (js-injector-node--inject-module import-module
                     (or import-name (js-injector-node--nice-name module))
                     pos)))

(defun js-injector-node--inject-module (module module-name &optional pos)
  "Inject MODULE into the node file as MODULE-NAME.
If POS is non-nil, goto position before injecting module."
  (unless pos (js-injector-node--goto-end-of-import))
  (let* ((import         (format js-injector-node-require-format module-name module))
         (import-split   (split-string import " \\|(\\|)"))

         (separators     (--filter (string-match " \\|(\\|)" it)
                                   (split-string import "")))

         (partial        (s-trim (buffer-substring-no-properties (line-beginning-position) (point))))
         (partial-length (length (--filter (> (length it) 0)
                                           (split-string partial " \\|(\\|)")))))

    (if (not pos)
        (insert (format "%s\n" import))
      (goto-char pos)
      (delete-region pos (line-end-position))
      (when (and (> partial-length 0) (not (looking-back " "))) (insert " "))
      (insert
       (s-join "" (-zip-with 'concat
                             (-drop partial-length import-split)
                             (-drop partial-length separators)))))))

;;;###autoload
(defun js-injector-node-import-module-at-point (&optional pfx)
  "Import the module at point.
When called with a PFX argument, this will prompt the user for
what name they want to import the file as."
  (interactive "P")
  (let* ((module (word-at-point))
         (var-decl (js-injector-node--var-decl?))
         (pos (and var-decl (point))))

    (save-excursion
      (cond
       ((eq (car var-decl) 'require)  (funcall (cdr var-decl) pfx))
       ((eq (car var-decl) 'assign)   (funcall (cdr var-decl) pfx))
       ((eq (car var-decl) 'declare)  (funcall (cdr var-decl) pfx pos))
       ((eq (car var-decl) 'var)      (funcall (cdr var-decl)))
       (t (js-injector-node-import module pfx pos)))
      (indent-region (line-beginning-position) (line-end-position)) t)))

;;;###autoload
(defun js-injector-node-import-module (&optional pfx)
  "Import a module in the project.
When called with a PFX argument, this will prompt the user for
what name they want to import the file as."
  (interactive "P")
  (let* ((modules (-map 'car (append
                              (js-injector-get-relative-dependency-alist)
                              (ignore-errors (js-injector-node-get-node-module-alist)))))
         (module (completing-read "Import module: " (--map (cons it it) modules)))
         (var-decl (js-injector-node--var-decl?))
         (pos
          (or (and (eq (car var-decl) 'var) (point))
              (when (< (point) (save-excursion (js-injector-node--goto-end-of-import) (point))) (point)))))
    (save-excursion (js-injector-node-import module pfx pos))))

;;;###autoload
(defun js-injector-node-update-module-at-point (&optional pfx)
  "Reimport the current module in a require statement.
When PFX is non-nil, this will prompt to replace this path with
any other path in the project."
  (interactive "P")
  (save-excursion
    (let* ((cur-pos (unless pfx (point)))
           (var-require (js-injector-node--get-var-require))
           (module (car var-require))

           (dependency-alist (append
                              (js-injector-get-relative-dependency-alist)
                              (ignore-errors (js-injector-node-get-node-module-alist))))
           (dependency-match (assoc-string module dependency-alist t))
           (dependencies (if pfx (-flatten (-map 'cdr dependency-alist))
                           (cdr dependency-match)))

           (import
            (js-injector--read-dependencies module dependencies cur-pos)))

      (unless import
        (error "Could not reimport module '%s'" module))

      (js-injector-replace-region
       (cadr var-require) (caddr var-require) import))))

;;;###autoload
(defun js-injector-node-remove-unused-modules ()
  "Remove all unused modules from the current file."
  (interactive)
  (save-excursion
    (--map (and (goto-char (point-min))
                (flush-lines (format "require(.*%s.*)" it)))
           (js-injector-node-get-unused-vars))))

(provide 'js-injector-node)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (nameless-mode 1)
;; End:
;;; js-injector-node.el ends here
