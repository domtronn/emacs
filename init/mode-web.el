;;; mode-web.el --- Setup files for working with Web & HTML

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

;;;; Web Mode

(use-package web-mode
  :ensure t
  :mode (("\\.moon$" . web-mode)
         ("\\.ejs$" . web-mode)
         ("\\.html$" . web-mode))
  :config (setq
           web-mode-code-indent-offset 2
           web-mode-markup-indent-offset 2)
  :bind (:map web-mode-map
              ("C-c C-c k" . web-mode-element-kill)
              ("C-c C-c n" . web-mode-element-next)
              ("C-c C-c p" . web-mode-element-previous)
              ("C-c C-c a" . web-mode-element-select)) )

(provide 'mode-web)

;;; mode-web.el ends here
