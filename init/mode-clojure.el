;;; mode-clojure.el --- Settings for working with Clojure

;; Copyright (C) 2017  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth
;; Keywords: internal, lnaguages
;; Filename: mode-clojure.el

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

(use-package cider :ensure t
  :commands (cider-mode cider-jack-in)
  :config
  (setq nrepl-log-messages nil
        cider-repl-display-help-banner nil)
  (bind-keys :map cider-repl-mode-map
             ("C-r" . cider-repl-history)
             ("<up>" . cider-repl-previous-input)
             ("<down>" . cider-repl-next-input))
  (bind-keys :map cider-mode-map
             ("s-<return>" . cider-eval-defun-at-point)))

(use-package ac-cider :ensure t :after (cider clojure)
  :init
  (add-hook 'clojure-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup))

(use-package flycheck-clojure :ensure t :after (flycheck)
  :config (flycheck-clojure-setup)
  (flycheck-add-mode 'clojure-cider-kibit 'clojure-mode)
  (flycheck-add-mode 'clojure-cider-eastwood 'clojure-mode))

(add-hook 'clojure-mode-hook '(lambda () (interactive) (setq-local flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))
(add-hook 'clojure-mode-hook '(lambda () (interactive) (setq-local eldoc-idle-delay 1)))
(add-hook 'clojure-mode-hook 'eldoc-mode)

(provide 'mode-clojure)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-clojure.el ends here
