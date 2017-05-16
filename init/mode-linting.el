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

(use-package flycheck :ensure t
  :config (global-flycheck-mode)
  ;; npm install -g eslint_d
  (setq flycheck-indication-mode nil)
  (setq flycheck-javascript-standard-executable "standard")
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (setq flycheck-eslintrc ".eslintrc.json")
  (setq-default flycheck-disabled-checkers '(javascript-jshint scss))
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-standard 'rjsx-mode)
  :bind (("M-}" . flycheck-mode)
         :map flycheck-mode-map
         ("C-c C-e" . flycheck-list-errors)
         ("C-c C-n" . flycheck-next-error)
         ("C-c C-p" . flycheck-previous-error)))

(use-package flyspell-popup :ensure t :defer t :after flyspell
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
