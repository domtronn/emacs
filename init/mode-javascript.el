;;; mode-javascript.el --- Setup files for working with JavaScript

;; Copyright (C) 2017  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth
;; Keywords: internal, lnaguages
;; Filename: mode-javascript.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; npm install -g eslint
;; npm install -g eslint_d

;;; Code:

;;;; Linting
(use-package eslint-reader :load-path "etc/elisp-packages/eslint-reader" :after js2-mode)

(use-package eslintd-fix :after js2-mode :ensure t
  :init
  (defun js2/should-enable-eslint-fix-mode ()
    "Test whether or not to enabled `eslintd-fix-mode'."
    (when (and
           (buffer-file-name)
           (or (locate-dominating-file (buffer-file-name) ".eslintrc.json")
               (locate-dominating-file (buffer-file-name) ".eslintrc")))
      (eslintd-fix-mode)))
  (add-hook 'js2-mode-hook 'js2/should-enable-eslint-fix-mode))

(defun js2/load-prettify-symbols-alist ()
  "Add all of the javascript pretty symbols."
  (push '("function" . ?𝒇) prettify-symbols-alist)
  (push '("=>" . ?⭢) prettify-symbols-alist)
  (push '("return" . ?⇐) prettify-symbols-alist)
  (push '("undefined" . ?∅) prettify-symbols-alist)
  ;; Maths symbols
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("!=" . ?≠) prettify-symbols-alist)
  (push '("!==" . ?≢) prettify-symbols-alist)
  (push '("===" . ?≡) prettify-symbols-alist)
  (prettify-symbols-mode t))

;;; Utilities
(use-package js2-refactor :after js2-mode :ensure t)
;; (use-package js-injector
;;   :disabled t
;;   :after js2-mode
;;   :load-path "etc/elisp-packages/js-dependency-injector"
;;   :config
;;   (setq js-injector-get-relative-func 'js-injector--get-projectile-files-alist)
;;   :bind (:map js2-mode-map ("s-I" . js-injector-clever-import-module)))

(use-package js-import :ensure t :after js2-mode
  :load-path "etc/elisp-packages/js-import"
  :config (setq js-import-quote "'")
  :bind (:map js2-mode-map
              ("s-i" . js-import)
              ("s-I" . js-import)))

(use-package tern :ensure t :after js2-mode :disabled t
  :config (add-hook 'js2-mode-hook 'tern-mode))

(defun npm--get-package-json ()
  "Get a package.json for a project and list its scripts."
  (when (not (locate-dominating-file (or (buffer-file-name) (projectile-project-root)) "package.json"))
    (error "Could not find `package.json'"))
  (let* ((package-path (locate-dominating-file (or (buffer-file-name)
                                                   (projectile-project-root))
                                               "package.json"))
         (package-file (format "%s/package.json" package-path))
         (package-json (json-read-file package-file)))
    (let-alist package-json (-map 'car .scripts))))

(defun npm--get-cmd-prefix (debug verbose)
  "Return the prefixed npm command based on whether you want DEBUG or VERBOSE."
  (format "%s%s%s%snpm run "
          (or (when debug "DEBUG=true") "")
          (or (when (and debug verbose) " ") "")
          (or (when verbose "VERBOSE=true") "")
          (or (when (or debug verbose) " ") "")))

(global-set-key (kbd "C-x C-n") 'npm-run)
(defun npm-run (&optional debug verbose)
  "Completing read a list of projects scripts."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-d") `(lambda () (interactive) (ivy-quit-and-run (npm-run ,(not debug) ,verbose))))
    (define-key map (kbd "C-v") `(lambda () (interactive) (ivy-quit-and-run (npm-run ,debug ,(not verbose)))))
    (ivy-read (format "%%d %s" (npm--get-cmd-prefix debug verbose)) (npm--get-package-json)
              :action `(lambda (match) (compile (concat (npm--get-cmd-prefix debug verbose) match)))
              :keymap map)))

(global-set-key (kbd "C-x M-n") 'npm-install)
(defun npm-install (&optional pfx)
  "Npm install with current directory.
When PFX is non-nil, run with --save or --save-dev"
  (interactive "p")
  (let* ((prefix (cl-case pfx
                   (4 "--save")
                   (0 "--save-dev")))
         (cmd (format "npm install %s " (or prefix "")))
         (package (read-string cmd (thing-at-point 'word))))
    (shell-command (format "%s %s" cmd package))))

;;; Major Modes
(use-package rjsx-mode :ensure t
  :mode (("\\.jsx?$" . rjsx-mode))
  :config
  (add-hook 'rjsx-mode-hook 'js2-mode-hide-warnings-and-errors)

  (defun key-combo--jsx-advice (orig-f &rest args)
    "Advice to put around `key-combo-pre-command-function'.
Applies ORIG-F with ARGS if the predicate passes."
    (when (member major-mode '(rjsx-mode))
      (unless (or
               (looking-at ".*/>$")
               (and (member (js2-node-type (js2-node-at-point)) (list rjsx-JSX rjsx-JSX-ATTR rjsx-JSX-IDENT rjsx-JSX-MEMBER))
                    (member (this-command-keys) '("=" "-" "+"))))
        (apply orig-f args))))
  ;; (advice-add 'key-combo-pre-command-function :around 'key-combo--jsx-advice )
  ;; (advice-remove 'key-combo-pre-command-function 'key-combo--jsx-advice )

  (bind-keys :map rjsx-mode-map ("s-w" . js2-mode)))

(use-package json :ensure json-mode
  :mode (("\\.json" . json-mode)
         ("\\.eslintrc$" . json-mode))
  :config
  (add-hook 'json-mode-hook
            '(lambda ()
               (setq-local js-indent-level 2)
               (local-set-key (kbd "<s-return>") 'send-to-repl)
               (local-set-key (kbd "<kp-enter>") '(lambda () (interactive) (dotimes (i 2) (smart-newline)))))))

(use-package js2-mode
  :config
  (setq js-switch-indent-offset 2)
  (setq js2-include-node-externs t)
  (setq js2-include-browser-externs t)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)

  (setq js2-jump-fallback-f '(lambda (thing &rest args) (counsel-ag thing (projectile-project-root))))

  (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
  (add-hook 'js2-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  ;; (remove-hook 'js2-mode-hook '(lambda () (flycheck-select-checker (flycheck--guess-checker))))
  (bind-keys :map js2-mode-map
             ("C-c x" . send-to-repl)
             ("M-=" . (lambda () (interactive) (insert "=")))

             ;; JS2 Refactor things
             ("C-c m" . prettify-symbols-mode)
             ("C-c C-o" . js2r-order-vars-by-length)
             ("C-c C-s" . js2r-toggle-var-declaration)
             ("C-c C-v" . js2r-extract-var)
             ("C-c C-a" . js2r-toggle-arrow-function-and-expression)
             ("C-c C-i" . js2r-inline-var)
             ("C-c C-f" . js2r-extract-function)
             ("C-c C-r" . js2r-rename-var)
             ("C-c ." . js2-jump-to-definition)
             ("C-k" . js2r-kill)
             ("s-w" . rjsx-mode)
             ("M-." . js2-jump-around)
             ("M-," . pop-tag-mark)
             ("<s-return>" . send-to-repl)
             ("<kp-enter>" . (lambda () (interactive) (dotimes (i 2) (smart-newline))))
             ("<C-backspace>" . (lambda () (interactive) (smart-backward) (js2r-kill)))))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(global-set-key (kbd "s-t") 'toggle-file-and-css)
(defun toggle-file-and-css ()
  "Toggle between JSX/JS and CSS file."
  (interactive)
  (when buffer-file-name
    (let* ((file-name (buffer-file-name))
           (css-p (s-ends-with? ".css" file-name))
           (jsx-p (s-ends-with? ".jsx" file-name))
           (js-p (s-ends-with? ".js" file-name)))
      (when jsx-p (find-file (s-replace ".jsx" ".css" file-name)))
      (when js-p (find-file (s-replace ".js" ".css" file-name)))
      (when (and css-p (file-exists-p (s-replace ".css" ".js" file-name)) )
        (find-file (s-replace ".css" ".js" file-name)))
      (when (and css-p (file-exists-p (s-replace ".css" ".jsx" file-name)) )
        (find-file (s-replace ".css" ".jsx" file-name))))))

(provide 'mode-javascript)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-javascript.el ends here
