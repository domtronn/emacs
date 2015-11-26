;;; advice.el --- List of function advice

;; Copyright (C) 2014  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

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
;;; common functions used in Emacs

;;; Code:
(defadvice ido-switch-buffer (before toggle-ido-vertical nil activate)
  "Disable `ido-vertical-mode` when calling `ido-switch-buffer`."
  (disable-vertical)
  (flx-ido-mode 1))

(defadvice smex (before activate-ido-vertical nil activate)
  "Disable `ido-vertical-mode` when calling `smex`."
  (disable-vertical)
  (flx-ido-mode 1))

(defadvice ido-find-file (before activate-ido-vertical nil activate)
  "Enable `ido-vertical-mode` when calling `ido-find-file`."
  (enable-vertical))

(defun enable-vertical ()
  (setq flx-ido-use-faces t)
  (setq ido-use-faces nil)
  (flx-ido-mode 1)
  (ido-vertical-mode 1))

(defun disable-vertical ()
  (setq flx-ido-use-faces nil)
  (setq ido-use-faces t)
  (flx-ido-mode 0)
  (ido-vertical-mode 0))

;; Disable all themes before loading a new one
(defun disable-themes-and-update-powerline (orig-f &rest args)
  (disable-all-themes)
  (apply orig-f args)
  (update-powerline)
  (context-coloring-mode 0))

(advice-add 'load-theme :around 'disable-themes-and-update-powerline)

(defun enable-and-disable-vertical (orig-f &rest args)
  (enable-vertical)
  (call-interactively orig-f)
  (disable-vertical))

(mapc '(lambda (sym) (advice-add sym :around 'enable-and-disable-vertical))
      '(projectable-find-file projectable-find-test
        projectable-find-file-other-window projectable-find-test-other-window))

(provide 'advice)
;;; advice.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:
