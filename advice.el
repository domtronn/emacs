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

(defadvice rails-script:generate-model (after refresh-cache activate)
	"Refreshes the cache to include the new generated model"
	(project-refresh))
(defadvice rails-script:generate-controller (after refresh-cache activate)
	"Refreshes the cache to include the new generated model"
	(message "ABOUT TO REFRESH THE PROJECt")
	(project-refresh))
(defadvice rails-script:generate-observer (after refresh-cache activate)
	"Refreshes the cache to include the new generated model"
	(project-refresh))

(defadvice ido-switch-buffer (before toggle-ido-vertical nil activate)
	"Disable `ido-vertical-mode` when calling `ido-switch-buffer`."
  (ido-vertical-mode 0))

(defadvice smex (before activate-ido-vertical nil activate)
	"Disable `ido-vertical-mode` when calling `smex`."
  (ido-vertical-mode 0))

(defadvice ido-find-file (before activate-ido-vertical nil activate)
	"Enable `ido-vertical-mode` when calling `ido-find-file`."
  (ido-vertical-mode 1))

(defun ido-vertical-project-change ()
	"Enable `ido-vertical-mode` when calling `project-change`."
	(interactive)
  (ido-vertical-mode 1)
	(call-interactively 'project-change))

(defun my-file-cache-ido-find-file ()
	"Wrapper to enable `ido-vertical-mode` before calling `file-cache-ido-find-file`."
  (interactive)
  (ido-vertical-mode 1)
  (call-interactively 'file-cache-ido-find-file))

(provide 'advice)
;;; advice.el ends here
