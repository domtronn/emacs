;;; js-injector.el --- Inject paths to JS classes

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

;; This package allows you to perform the equivalent of dependency
;; injection for javascript files

;;; Code:

(require 'dash)
(require 's)
(require 'multi-line)

(require 'js-injector-lib)
(require 'js-injector-node)

;;; Group Definitions
(defgroup js-injector nil
  "Manage the minor mode to allow javascript injection."
  :group 'tools
  :group 'convenience)

;; common custom prefixes
(defcustom js-injector-sub-keymap-prefix (kbd "C-c j")
  "Js-Injector keymap prefix."
  :group 'js-injector
  :type 'key-sequence)

(defcustom js-injector-sup-keymap-prefix (kbd "C-c C-j")
  "Js-Injector keymap prefix."
  :group 'js-injector
  :type 'key-sequence)

(defvar js-injector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map js-injector-sub-keymap-prefix 'js-injector-sub-command-map)
    (define-key map js-injector-sup-keymap-prefix 'js-injector-sup-command-map)
    map)
  "Keymap for Projectile mode.")

(defvar js-injector-define-regexp "require\.def(\\|require(\\|define(\\|req.def(")

(defcustom js-injector-get-relative-func 'js-injector--get-jpop-files-alist
	"The function to get the current projects file from."
	:type '(radio
          (const :tag "Get the files from JPoP project" js-injector--get-jpop-files-alist)
          (const :tag "Get the files from Projectile project" js-injector--get-projectile-files-alist)
          (const :tag "Get the files using Git Command line" js-injector--get-git-files-alist))
  :group 'js-injector)

(defcustom js-injector-get-requirejs-func '(:files js-injector--get-jpop-requirejs-alist :config js-injector--get-jpop-requirejs-config)
	"The function to get the requirejs file alist from."
	:type '(radio
					(const :tag "Use the requirejs mapped files and config from JPoP project"
								 (:files js-injector--get-jpop-requirejs-alist :config js-injector--get-jpop-requirejs-config))
					(const :tag "Use the relative project files from JPoP project"
								 (:files js-injector--get-jpop-relative-requirejs-alist :config js-injector--get-jpop-relative-requirejs-config))
					(const :tag "Use the relative project files from Projectile project"
								 (:files js-injector--get-projectile-relative-requirejs-alist :config js-injector--get-projectile-relative-requirejs-config)))
  :group 'js-injector)

;;; Get definitions
;;  Functions to get various bits of information required for imports

(defalias 'js-injector-get-requirejs-config 'jpop-get-requirejs-config)

(defun js-injector-get-dependency-alist ()
  "Make dependency alist using `js-injector-get-requirejs-files-alist`.
It assossciates each file name to a list of locations of that file."
  (let ((requirejs-alist (funcall (plist-get js-injector-get-requirejs-func :config)))
        (requirejs-files-alist (funcall (plist-get js-injector-get-requirejs-func :files)))
        (result (list)))
    (mapc
     (lambda (project)
       (let* ((requirejs-id (car project))
              (requirejs-path (expand-file-name (cdr (assoc requirejs-id requirejs-alist)))))
         (mapc (lambda (file-alist)
                 (when (string-match "js[x]\\{0,1\\}" (or (file-name-extension (car file-alist)) ""))
                   (let* ((filename (file-name-sans-extension (car file-alist)))
                          (result-match (assoc filename result))
                          (normalised-paths
                           (--map
                            (file-name-sans-extension (replace-regexp-in-string requirejs-path requirejs-id it))
                            (cdr file-alist))))
                     (if result-match
                         (setf (cdr result-match) (append (cdr result-match) normalised-paths))
                       (add-to-list 'result (cons filename normalised-paths)))
                     )))
               (cdr project))))
     requirejs-files-alist)
    result))


(defun js-injector-get-relative-dependency-alist ()
  "Construct the relative dependency alist from project-files.
It assossciates each file name to a list of locations relative to
the current file."
  (let ((project-files (funcall js-injector-get-relative-func))
        (containing-dir (file-name-directory (buffer-file-name))))
    (-non-nil
     (cl-loop for file-alist in project-files
              for file = (car file-alist)
              for locations = (cdr file-alist)
							collect
							(when (string-match "\.js[xon]\\{0,2\\}$" file)
								(cons (file-name-sans-extension file)
											(--map (file-name-sans-extension
                              (if (string-match "^[a-zA-Z]" (file-relative-name it containing-dir))
                                  (concat "./" (file-relative-name it containing-dir))
                                (file-relative-name it containing-dir)))
                             locations)))))))

;;; Navigation definitions
;;  Functions to navigate around the requirejs file
(defun js-injector--goto-define ()
  "Navigate to the define block."
  (goto-char (point-min))
  (search-forward-regexp js-injector-define-regexp)
  (skip-chars-forward " \n\t"))

(defun js-injector--goto-import-block ()
  "Navigate to the define import block."
  (js-injector--goto-define)
  (search-forward "["))

(defun js-injector--goto-first-import ()
  "Navigate to the first import item."
  (js-injector--goto-import-block)
  (skip-chars-forward " \n\t"))

(defun js-injector--goto-last-import ()
  "Navigate to the first import item."
  (js-injector--goto-import-block)
  (search-forward "]")
  (backward-char)
  (skip-chars-backward " \n\t"))

(defun js-injector--goto-import-function ()
  "Navigate to the define import function."
  (js-injector--goto-import-block)
  (search-forward "function"))

(defun js-injector--goto-import-function-params ()
  "Navigate to the parameters of the import function."
  (js-injector--goto-import-function)
  (search-forward "("))

(defun js-injector--goto-import-block-end ()
  "Get the define import array."
  (js-injector--goto-import-block)
  (search-forward "]"))

;;; Get Define definitions
;;  Functions to get the contents of the require thing

(defun js-injector--get-node-contents (node)
  "Get the contents of a JS2 NODE."
  (let ((beg (js2-node-abs-pos node))
        (end (js2-node-abs-end node)))
    (buffer-substring-no-properties beg end)))

(defun js-injector--get-module-name ()
  "Get the define module name.  Return nil if no name is given."
  (save-excursion
    (js-injector--goto-define)
    (unless (js2-array-node-p (js2-node-at-point))
      (js-injector--get-node-contents (js2-node-at-point)))))

(defun js-injector--get-import-block ()
  "Get the define import array."
  (save-excursion
    (let ((beg (js-injector--goto-import-block))
          (end (- (search-forward "]") 1)))
      (s-trim (s-collapse-whitespace (buffer-substring-no-properties beg end))))))

(defun js-injector--get-import-block-as-list ()
  "Get the define import function parameters."
  (-filter 's-present? (split-string (js-injector--get-import-block) "[, ]")))

(defun js-injector--get-import-function-params ()
  "Get the define import function parameters."
  (save-excursion
    (let ((beg (js-injector--goto-import-function-params))
          (end (- (search-forward ")") 1)))
      (s-trim (s-collapse-whitespace (buffer-substring-no-properties beg end))))))

(defun js-injector--get-import-function-params-as-list ()
  "Get the define import function parameters."
  (-filter 's-present? (split-string (js-injector--get-import-function-params) ", ")))

;;; Injector definitions
;;  Functions that inject dependencies into the current file

(defun js-injector--insert-module-name (module &optional pos f)
  "Inject MODULE into the function params at index POS.

F should be a function to inject an item into a list somehow.  It
defaults to `-insert-at`."
  (js-injector--goto-import-function-params)
  (let* ((beg (point))
         (end (- (search-forward ")") 1))
         (modules (js-injector--get-import-function-params-as-list))
         
         (pos (or pos (length modules)))
         (f (or f '-insert-at)))
    
    (js-injector-replace-region
     beg end (mapconcat 'identity (funcall f pos module modules) ", "))))

(defun js-injector--import-module-name (import &optional pos f)
  "Import IMPORT into the import block at index POS.

F should be a function to inject an item into a list.  It
defaults to `-insert-at`."
  (let* ((beg (and (js-injector--goto-first-import) (point)))
         (end (and (js-injector--goto-last-import) (point)))
         (imports (js-injector--get-import-block-as-list))
         
         (pos (or pos (length imports)))
         (f (or f '-insert-at)))
    
    (js-injector-replace-region
     beg end
     (format "%s" (mapconcat 'identity (funcall f pos import imports) ",")))))

;;; Format definitions
;;  Functions that format certain areas of the module
(defun js-injector--format-import ()
  "Format the import block."
  (let ((beg (and (js-injector--goto-first-import) (point)))
        (end (and (js-injector--goto-last-import) (point))))
    (multi-line nil)
    (indent-region beg end)))

(defun js-injector--format-function-params ()
  "Format the function params to wrap."
  (js-injector--goto-import-function-params)
  (let ((beg (point))
        (end (search-forward ")")))
    (fill-region beg end)
    (indent-region beg end)))

(defun js-injector-format ()
  "Format both the imports block and the function params."
  (interactive)
  (js-injector--format-import)
  (js-injector--format-function-params))

(defun js-injector--replace-module (module import-module)
  "Replace the current MODULE and its IMPORT-MODULE in the file at its position."
  (let ((pos (-elem-index module (js-injector--get-import-function-params-as-list))))
    (js-injector--insert-module-name module pos '-replace-at)
    (js-injector--import-module-name import-module pos '-replace-at)
    (js-injector-format)))

(defun js-injector--insert-module (module import-module)
  "Insert MODULE and its IMPORT-MODULE into file."
  (js-injector--insert-module-name module)
  (js-injector--import-module-name import-module)
  (js-injector-format))

(defun js-injector--remove-module (module)
  "Remove MODULE and its import from the file."
  (let ((pos (-elem-index module (js-injector--get-import-function-params-as-list))))
    (js-injector--insert-module-name nil pos '(lambda (n x list) (-remove-at n list)))
    (js-injector--import-module-name nil pos '(lambda (n x list) (-remove-at n list)))
    (js-injector-format)))

(defun js-injector--remove-all ()
  "Remove all current import modules from the file."
  (cl-loop repeat (length (js-injector--get-import-function-params-as-list))
           do (js-injector--insert-module-name nil 0 '(lambda (n x list) (-remove-at n list)))
           (js-injector--import-module-name nil 0 '(lambda (n x list) (-remove-at n list))))
  (js-injector-format))

(defun js-injector-import (module &optional prompt-name popup-point)
  "Inject MODULE as dependency.
This function will look for MODULE it in the dependncy list.
If it exists, it will append it to the function list
and add the require path, if it is already used it will update the
current dependency.  If it does not exist, do nothing and print to the
minibuffer.

If PROMPT-NAME is non-nil, this function will prompt the user for
the name they would like to import the module as.

If POPUP-POINT is non-nil, use this value as the position to
place the popup menu, else use the current value of `point`."
  (let* ((dependency-alist (js-injector-get-dependency-alist))
         (dependency-match (assoc-string module dependency-alist t))
         (dependencies (cdr dependency-match))

         (qc (js-injector--get-quote-char))
         (import-module (js-injector--read-dependencies module dependencies popup-point))
         
         (imported-modules (js-injector--get-import-function-params-as-list))
         (import-name (when (and import-module prompt-name)
                        (read-string (format "Import '%s' as: " module)))))
    
    (unless import-module
      (error "No module named '%s'" module))

    (if (member module imported-modules)
        (js-injector--replace-module (or import-name module) (format "%s%s%s" qc import-module qc))
      (js-injector--insert-module (or import-name module) (format "%s%s%s" qc import-module qc)))
    (js-injector-format)))

;;;###autoload
(defun js-injector-import-module-at-point (&optional pfx)
  "Inject module at point.
When given a PFX argument, will prompt user for the module name to be imported as."
  (interactive "P")
  (js-injector--guard)
  (save-excursion (js-injector-import (word-at-point) pfx (point)) t))

;;;###autoload
(defun js-injector-import-module (&optional pfx)
  "Inject module at point.
When called with a PFX argument, this will prompt for the import name."
  (interactive "P")
  (js-injector--guard)
  (let* ((modules (-map 'car (js-injector-get-dependency-alist)))
         (module (s-upper-camel-case
                  (completing-read "Import module: " (--map (cons it it) modules)))))
    (save-excursion (js-injector-import module pfx))))

;;;###autoload
(defun js-injector-remove-module (&optional m)
  "Remove a module M or prompt for a module."
  (interactive)
  (js-injector--guard)
  (let* ((modules (js-injector--get-import-function-params-as-list))
         (module (or m (completing-read "Remove module: " (--map (cons it it) modules)))))
    (save-excursion (js-injector--remove-module module))))

;;;###autoload
(defun js-injector-remove-unused-modules ()
  "Remove all unused modules in a requirejs file."
  (interactive)
  (js-injector--guard)
  (let* ((modules (js-injector--get-import-function-params-as-list))
         (unused-modules (--filter (eq 1 (js-injector--count-occurences
                                          (format "\\b%s\\b" it)
                                          (buffer-string)
                                          0))
                                   modules)))
    (-map 'js-injector-remove-module unused-modules)))

(defun js-injector-remove-unused ()
  "Remove all unused modules from the file."
  (interactive)
  (if (js-injector--requirejs-file?) (js-injector-remove-unused-modules) (js-injector-node-remove-unused-modules)))

;;;###autoload
(defun js-injector-update-imports (pfx)
  "Run through each current import module and reimport them.
Prompting user for the paths to the modules they want to import.

When called with a PFX argument, this will prompt the user for
the name they want to import modules as."
  (interactive "P")
  (js-injector--guard)
  (let ((popup-point (point))
        (imports (js-injector--get-import-block-as-list))
        (modules (js-injector--get-import-function-params-as-list)))
    (save-excursion
      (js-injector--remove-all)
      (--map-indexed
       (js-injector-import (s-chop-suffixes'("'" "\"") (file-name-base (nth it-index imports))) pfx)
       modules))))

;;;###autoload
(defun js-injector-sort-imports ()
  "Sort the dependencies alphabetically."
  (interactive)
  (js-injector--guard)
  (save-excursion
    (let* ((zipped-modules
            (--sort (string< (car it) (car other))
                    (-zip-pair (js-injector--get-import-block-as-list)
                               (js-injector--get-import-function-params-as-list))))
           (sorted-imports (-map 'car zipped-modules))
           (sorted-modules (-map 'cdr zipped-modules)))
      (--map-indexed (js-injector--insert-module-name it it-index '-replace-at) sorted-modules)
      (--map-indexed (js-injector--import-module-name it it-index '-replace-at) sorted-imports)))
  (js-injector-format))

(defun js-injector--requirejs-file? ()
  "Guess whether current file is a requirejs file.
If not, we assume its a node module."
  (save-excursion
    (with-current-buffer (buffer-name)
      (goto-char (point-min))
      (and (s-matches? js-injector-define-regexp (buffer-string))
           (if (search-forward "require(" nil t)
               (when (js2-call-node-p (js2-node-at-point))
                 (> (length (js2-call-node-args (js2-node-at-point))) 1))
             t)))))

(defun js-injector--guard ()
  "Guard against running commands in unsupported files."
  (interactive)
  (unless (js-injector--requirejs-file?)
    (error "Not currently in a requirejs file")))

;;;###autoload
(defun js-injector-clever-import-module (&optional pfx)
  "Cleverly guess what kind of import to run given context.

This will guess whether to use the node import or the requirejs
import based on tell tale signs in the file, such as the presence
of the require/define/req.def blocks, or the
module.exports/exports of the node.

When called with a PFX argument, this will prompt user for import
on module namings."
  (interactive "P")
  (if (js-injector--requirejs-file?)
      (unless (ignore-errors (js-injector-import-module-at-point pfx))
        (js-injector-import-module pfx))
    (unless (ignore-errors (js-injector-node-import-module-at-point pfx))
			(js-injector-node-import-module pfx))))

;;;###autoload
(defun js-injector-drag-inject-file (event)
  "Function to apply mouse EVENT for dragging and dropping."
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point event)
  (let ((start-buf (window-buffer (posn-window (event-start event))))
        (start-pos (posn-point (event-start event)))

        (end-buf (window-buffer (posn-window (event-end event))))
        (end-pos (posn-point (event-end event)))

        file)

    ;; Pull the file from the dired/neotree buffer
    (with-current-buffer start-buf
      (goto-char start-pos)
      (setq file (file-name-nondirectory (dired-filename-at-point))))

    ;; Inject into the current file if it's a JS file and you're
    ;; injecting a js/json file
    (when (and (string-match "js[on]\\{0,2\\}$" (file-name-extension file))
               (string-match "js$" (buffer-file-name end-buf)))
      (with-current-buffer end-buf
        (goto-char end-pos)
        (goto-char (line-end-position))

        ;; Goto new line if you're not on a blank line already
        (when (not (string-match "^$" (buffer-substring (line-beginning-position) (line-end-position))))
          (smart-newline))

        (if (js-injector--requirejs-file?)
            (js-injector-import (file-name-base file))
          (js-injector-node-import (file-name-base file) nil (point)))))))

;; (define-key neotree-mode-map [drag-mouse-1] 'js-injector-drag-inject-file)
;; (define-key dired-mode-map [drag-mouse-1] 'js-injector-drag-inject-file)

(defvar js-injector-sup-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-i") 'js-injector-clever-import-module)
    (define-key map (kbd "i") 'js-injector-import-module)
    (define-key map (kbd "h") 'js-injector-describe-modules)
    (define-key map (kbd "k") 'js-injector-remove-module)
    (define-key map (kbd "K") 'js-injector-remove-unused)
    (define-key map (kbd "u") 'js-injector-update-imports)
    (define-key map (kbd "l") 'js-injector-sort-imports)
    (define-key map (kbd "n") 'js-injector-node-import-module)
    map)
  "Keymap for Js-Injector commands after `js-injector-sup-keymap-prefix'.")
(fset 'js-injector-sup-command-map js-injector-sup-command-map)

(defvar js-injector-sub-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'js-injector-import-module-at-point)
    (define-key map (kbd "k") 'js-injector-remove-unused-modules)
    (define-key map (kbd "n") 'js-injector-node-import-module-at-point)
    map)
  "Keymap for Js-Injector commands after `js-injector-sub-keymap-prefix'.")
(fset 'js-injector-sub-command-map js-injector-sub-command-map)

(define-minor-mode js-injector-minor-mode
  "Minor mode to help with js dependency injection.

When called interactively, toggle `js-injector-minor-mode'.  With prefix
ARG, enable `js-injector-minor-mode' if ARG is positive, otherwise disable
it.

\\{-mode-map}"
  :lighter "js-i"
  :keymap js-injector-mode-map
  :group 'js-injector
  :require 'js-injector)

(provide 'js-injector)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (nameless-mode 1)
;; End:
;;; js-injector.el ends here
