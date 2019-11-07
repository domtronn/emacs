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

(defun quick-eval ()
  (interactive)
  (let* ((root (projectile-project-root))
         (src-file (buffer-file-name))
         (build-file (replace-regexp-in-string "/src/" "/build/" src-file)))
    (message root)
    (quickrun :source `((:command . "node")
                        (:tempfile . nil)
                        (:default-directory . ,root)
                        (:exec . (,(format "npx babel %s --out-file %s" src-file build-file)
                                  ,(format "cd %s; npx env-cmd development node %s" root build-file)
                                  ))))))

;;; Utilities
(use-package js2-refactor :ensure t
  :bind (:map js2-mode-map ("C-c r" . js2r-rename-var)))
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
  :bind (:map js2-mode-map ("s-i" . js-import)))

(use-package import-js :ensure t :after js2-mode
  :config (run-import-js)
  (defun import-js-import-word ()
    "Run import-js goto function, which returns a path to the specified module"
    (interactive)
    (import-js-check-daemon)
    (setq import-js-output "")
    (setq import-js-handler 'import-js-handle-imports)
    (import-js-send-input `(("command" . "word")
                            ("commandArg" . ,(completing-read "Module: "
                                                              (-uniq
                                                               (append
                                                                (--map (s-upper-camel-case
                                                                        (file-name-base
                                                                         (file-name-sans-extension it)))
                                                                       (projectile-current-project-files))
                                                                (-map 'first (imenu-anywhere-candidates)))))))))

  :bind (:map js2-mode-map
              ("s-I" . import-js-import)
              ("M-s-^" . import-js-import-word)
              ("M-s-≥" . imenu-anywhere)))

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
  :mode (("\\.m?jsx?$" . rjsx-mode))
  :config
  (key-combo-define rjsx-mode-map "=" '(" = " " => " " === "))
  (add-hook 'rjsx-mode-hook 'js2-mode-hide-warnings-and-errors)
  (add-hook 'rjsx-mode-hook '(lambda () (set-face-attribute 'rjsx-tag-bracket-face nil :inherit 'rjsx-tag)))

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
               (setq-local js-indent-level 2))))

(use-package auto-rename-tag :ensure t :disabled t
  :after rjsx-mode
  :config (global-auto-rename-tag-mode t))

(use-package js2-mode
  :config
  (setq js-switch-indent-offset 2)
  (setq js2-include-node-externs t)
  (setq js2-include-browser-externs t)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)

  (setq js2-jump-fallback-f '(lambda (thing &rest args) (counsel-ag thing (projectile-project-root))))
  (require 'js2-refactor)

  (key-combo-define js2-mode-map "=" '(" = " " => " " === "))
  (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
  (add-hook 'js2-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  ;; (remove-hook 'js2-mode-hook '(lambda () (flycheck-select-checker (flycheck--guess-checker))))
  (bind-keys :map js2-mode-map
             ("C-c x" . quick-eval)
             ("M-=" . (lambda () (interactive) (insert "=")))

             ;; JS2 Refactor things
             ("C-c m" . prettify-symbols-mode)
             ("C-c C-o" . js2r-order-vars-by-length)
             ("C-c C-s" . js2r-toggle-var-declaration)
             ("C-c C-v" . js2r-extract-var)
             ("C-c C-a" . js2r-toggle-arrow-function-and-expression)
             ("C-c C-i" . js2r-inline-var)
             ("C-c C-r" . js2r-rename-var)
             ("C-c ." . js2-jump-to-definition)
             ("C-k" . js2r-kill)
             ("s-w" . rjsx-mode)
             ("M-." . js2-jump-around)
             ("M-," . pop-tag-mark)
             ("<s-return>" . send-to-repl)
             ("<C-backspace>" . (lambda () (interactive) (smart-backward) (js2r-kill)))))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(global-set-key (kbd "s-t") 'toggle-file-and-css)
(defun toggle-file-and-css ()
  "Toggle between JSX/JS and CSS file."
  (interactive)
  (when buffer-file-name
    (let* ((file-name (buffer-file-name))
           (base-name (file-name-sans-extension (buffer-file-name)))
           (style-name
            (cond
             ((file-exists-p (format "%s.css" base-name)) (format "%s.css" base-name))
             ((file-exists-p (format "%s.scss" base-name)) (format "%s.scss" base-name))))
           (impl-name
            (cond
             ((file-exists-p (format "%s.jsx" base-name)) (format "%s.jsx" base-name))
             ((file-exists-p (format "%s.js" base-name)) (format "%s.js" base-name))))

           (impl-p (string-equal file-name impl-name))
           (style-p (string-equal file-name style-name)))

      (when impl-p (find-file style-name))
      (when style-p (find-file impl-name)))))

(provide 'mode-javascript)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-javascript.el ends here
