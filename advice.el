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
(defun set-vertical (on-or-off)
  (setq ido-use-faces (eq 0 on-or-off))
  (ido-vertical-mode on-or-off))

(defun disable-vertical-temp (orig-f &rest args)
  (set-vertical 0)
  (apply orig-f args)
  (set-vertical 1))

(advice-add 'ido-switch-buffer :around 'disable-vertical-temp)
(advice-add 'smex :around 'disable-vertical-temp)

;; Disable all themes before loading a new one
(defun disable-themes-and-update-powerline (orig-f &rest args)
	(disable-all-themes)
	(apply orig-f args)
	(update-powerline))

(advice-add 'load-theme :around 'disable-themes-and-update-powerline)

(provide 'advice)
;;; advice.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:
