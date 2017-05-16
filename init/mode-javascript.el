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
  :init (add-hook 'js2-mode-hook '(lambda () (interactive) (when (locate-dominating-file (buffer-file-name) ".eslintrc.json") (eslintd-fix-mode)))))

;;; Utilities
(use-package js2-refactor :after js2-mode :ensure t)
(use-package js-injector
  :after js2-mode
  :load-path "etc/elisp-packages/js-dependency-injector"
  :config
  (setq js-injector-get-relative-func 'js-injector--get-projectile-files-alist))

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
  (interactive "P")
  (let* ((prefix (cond
                  ((> (prefix-numeric-value pfx) 4) "--save")
                  ((> (prefix-numeric-value pfx) 0) "--save-dev")))
         (cmd (format "npm install %s " (or prefix "")))
         (package (read-string cmd (thing-at-point 'word))))
    (shell-command (format "%s %s" cmd package))))

;;; Major Modes
(use-package rjsx-mode :ensure t
  :mode (("\\.jsx?$" . rjsx-mode))
  :config
  (bind-keys :map rjsx-mode-map ("s-w" . js2-mode))
  ;; (advice-remove
  ;;  'key-combo-pre-command-function
  ;;  ;; :around
  ;;   '(lambda (orig-f &rest args)
  ;;      (when (member major-mode '(rjsx-mode))
  ;;        (unless (and (member (js2-node-type (js2-node-at-point)) (list rjsx-JSX rjsx-JSX-ATTR rjsx-JSX-IDENT rjsx-JSX-MEMBER))
  ;;                     (member (this-command-keys) '("=" "-" "+")))
  ;;          (apply orig-f args)))))
  )

(use-package json :ensure json-mode
  :mode ("\\.json" . json-mode)
  :config
  (add-hook 'json-mode-hook
            '(lambda ()
               (setq-local js-indent-level 2)
               (local-set-key (kbd "<s-return>") 'send-to-repl)
               (local-set-key (kbd "<kp-enter>") '(lambda () (interactive) (dotimes (i 2) (smart-newline)))))))

(use-package js2-mode
  :mode "\\.js$"
  :config
  (setq js-switch-indent-offset 2)
  (setq js2-include-node-externs t)
  (setq js2-include-browser-externs t)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)
  
  (setq js2-jump-fallback-f '(lambda (thing &rest args) (counsel-ag thing (projectile-project-root))))
  
  (add-hook 'js2-mode-hook 'js-injector-minor-mode)
  (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
  (add-hook 'js2-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'js2-mode-hook '(lambda () (key-combo-common-load-default)))
  (add-hook 'js2-mode-hook
            '(lambda () (flycheck-select-checker (flycheck--guess-checker))))
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
(add-hook 'js2-mode-hook 'auto-complete-mode)

(provide 'mode-javascript)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-javascript.el ends here
