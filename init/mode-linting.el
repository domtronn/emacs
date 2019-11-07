;;; mode-linting.el --- Settings for working with Linting

;; Copyright (C) 2017  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth
;; Keywords: internal, lnaguages
;; Filename: mode-linting.el

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
;; npm install -g standard

;;; Code:

(use-package flycheck-stylelint
  :after (web-mode)
  :load-path "etc/elisp-packages/flycheck-stylelint"
  :config
  (flycheck-add-mode 'css-stylelint 'scss-mode)

  (setq flycheck-stylelint-args '("--syntax" "scss" "--formatter" "json")
        flycheck-css-stylelint-executable "stylelint"
        flycheck-stylelintrc ".stylelintc.json"))

(defun flycheck/eslint-use-local ()
  "Read the eslint executable locally."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root)))
         (eslintd (and root
                       (expand-file-name "node_modules/.bin/eslint_d"
                                         root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))

    (when (and eslintd (file-executable-p eslintd))
      (setq-local flycheck-javascript-eslint-executable eslintd))))

(defun flycheck/eslint-use-rc ()
  "Read the potential .eslintrc file and set it appropriately."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "package.json"))
         (rc (and root (expand-file-name ".eslintrc" root)))
         (rcjson (and root (expand-file-name ".eslintrc.json" root)))
         (final-rc (cond
                    ((and rc (file-exists-p rc)) rc)
                    ((and rcjson (file-exists-p rcjson)) rcjson)
                    (t nil))))

    (setq-local flycheck-eslintrc final-rc)))

(defun flycheck/eslint-verify-config ()
  "Verify that the config is valid for eslint."
  (interactive)
  (let ((cmd (format
              "%s --print-config %s"
              flycheck-javascript-eslint-executable
              default-directory)))
    (message (shell-command-to-string cmd))))

(use-package flycheck :ensure t
  :config (global-flycheck-mode)
  ;; npm install -g eslint_d
  (setq flycheck-indication-mode nil)
  (setq flycheck-javascript-standard-executable "standard")
  (setq flycheck-javascript-eslint-executable "eslint")
  (setq flycheck-stylelintrc ".styleintrc")
  (setq flycheck-eslintrc ".eslintrc")
  ;; In the case of broken Eslint configs, try running `(flycheck-eslint-config-exists-p)'
  ;; Also, `M-! eslint_d --print-config .'
  (setq-default flycheck-disabled-checkers '(javascript-jshint scss))
  (flycheck-add-mode 'javascript-standard 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (add-hook 'flycheck-mode-hook 'flycheck/eslint-use-local)
  (add-hook 'flycheck-mode-hook 'flycheck/eslint-use-rc)
  :bind (("M-}" . flycheck-mode)
         :map flycheck-mode-map
         ("C-c C-e" . flycheck-list-errors)
         ("C-c C-n" . flycheck-next-error)
         ("C-c C-p" . flycheck-previous-error)))

(use-package flycheck-inline :ensure t :after flycheck
  :config (add-hook 'flycheck-mode-hook 'flycheck-inline-mode))

(use-package flycheck-title :ensure t :after flycheck
  :config (add-hook 'flycheck-mode-hook 'flycheck-title-mode))

(use-package flyspell-popup :ensure t :after flyspell
  :bind (:map flyspell-mode-map
         ("±" . flyspell-popup-correct)))

(use-package flyspell :ensure t
  :init (defun flyspell-toggle ()
          (interactive)
          (if flyspell-mode (flyspell-mode-off) (flyspell-mode)))
  :config
  (setq ispell-dictionary "english")
  (dolist (hook '(text-mode-hook)) (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook)) (add-hook hook (lambda () (flyspell-mode -1))))
  (advice-add 'flyspell-mode-on :before 'flyspell-buffer)
  :bind ("M-{" . flyspell-toggle))

(provide 'mode-linting)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-linting.el ends here
