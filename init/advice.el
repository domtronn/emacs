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

;; Disable all themes before loading a new one
(defun disable-themes (orig-f &rest args)
  (let ((current-font (assoc (face-attribute 'default :family) font-list)))
    (disable-all-themes)
    (apply orig-f args)
    (set-font current-font)))

(advice-add 'load-theme :around 'disable-themes)

(defun enable-and-disable-vertical (orig-f &rest args)
  (set-vertical 1)
  (call-interactively orig-f)
  (set-vertical 0))

(defun disable-and-enable-vertical (orig-f &rest args)
  (set-vertical 0)
  (call-interactively orig-f)
  (set-vertical 1))

;; Disable ido vertical mode and add advice around certain functions
;; to renenable temporarily
(defun set-vertical (b)
  (setq flx-ido-use-faces (eq 1 b))
  (setq ido-use-faces (eq 0 b))
  (ido-vertical-mode b))

(set-vertical 0)
(mapc #'(lambda (sym) (advice-add sym :around 'enable-and-disable-vertical))
      '(ido-find-file projectable-find-file projectable-find-test
        projectable-find-file-other-window projectable-find-test-other-window))

(mapc #'(lambda (sym) (advice-add sym :around 'disable-and-enable-vertical))
      '(smex smex-major-mode-commands ido-switch-buffer ido-switch-buffer-other-window))

(advice-add 'ansi-term :after '(lambda (&rest r) (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

(provide 'advice)
;;; advice.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
