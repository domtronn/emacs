;;; ac-css.el --- Auto complete functions for CSS & SCSS

;; Copyright (C) 2010-2012 Johan Andersson

;; Author: Dom Charlesworth <dgc336@gmail.com>
;; Maintainer: Dom Charlesworth <dgc336@gmail.com>
;; Version: 0.1.0
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(require 'popup)
(require 'dash)
(require 's)

(defun ac-css--candidates (find filter)
	"Find CSS candidates of FIND type and which FILTER out unwanted ones."
  (-mapcat
	 (lambda (buf) (with-current-buffer (buffer-name buf)
							(let ((css-face 'css-selector)
										(results))
								(goto-char (point-max))
								(while (and
												(not (eq (point) (point-min)))
												(goto-char
												 (or (previous-single-property-change (point) 'face)
														 (point-min))))
									(when (equal (get-char-property (point) 'face) '(css-selector))
										(let ((prev-point (point)))
											(goto-char (next-single-property-change (point) 'face))
											(setq results
														(append results
																		(-distinct
																		 (--map (s-chop-suffix "," it)
																						 (--filter (not (equal it ""))
																											 (--filter (not (s-contains? filter it))
																																 (--mapcat (split-string it find) (split-string (s-trim (buffer-substring-no-properties prev-point (point)))))))))))
											(goto-char prev-point)))) (-distinct results))))
	 (--filter (eq (with-current-buffer it major-mode) 'scss-mode) (buffer-list))))

(defun ac-scss-color-names ()
	"Create candidates for auto complete of SCSS colours variables."
	(let ((results))
		(-map
		 (lambda (buf) (with-current-buffer (buffer-name buf)
				 (goto-char (point-min))
				 (while (re-search-forward "\\(\$[a-z\-_0-9]+\\):.*?\\(#.*\\);" (point-max) t)
					 (let ((var (match-string-no-properties 1))
								 (col (match-string-no-properties 2)))
						 (setq results
									 (append results
										(list (popup-make-item var :summary (propertize (format "●") 'face `(:foreground ,col :background "#2B3B40"))))))))))
		 (--filter (eq (with-current-buffer it major-mode) 'scss-mode) (buffer-list))) results))

(defvar ac-source-scss-colors '((candidates . ac-scss-color-names)
																(prefix . "$\\S-+")
																(requires . 0)))

(defun ac-css-selector-candidates () "CSS Selector Candidates." (ac-css--candidates "[.]" "#"))
(defun ac-css-id-candidates () "CSS ID Candidates." (ac-css--candidates "[#]" "."))

(defvar ac-source-css-selector '((candidates . ac-css-selector-candidates)
																 (candidate-face . ac-css-candidate-face)
																 (selection-face . ac-css-selection-face)
																 (symbol . ".")
																 (prefix . "\\.\\(.*\\)"))
  "An `auto-complete-mode` source that is built of the issues.")

(defvar ac-source-css-id '((candidates . ac-css-id-candidates)
													 (candidate-face . ac-css-candidate-face)
													 (selection-face . ac-css-selection-face)
													 (symbol . "#")
													 (prefix . "\\#\\(.*\\)"))
  "An `auto-complete-mode` source that is built of the issues.")

(defface ac-css-candidate-face
  '((((class color)) :background "#2B3B40" :foreground "#FC536F"))
  "*Face to highlight css candidates"
       :group 'auto-complete)

(defface ac-css-selection-face
  '((((class color)) :background "#2B3B40" :foreground "#B2394C"))
  "*Face to highlight css candidates"
  :group 'auto-complete)


(provide 'ac-css)
;;; ac-css.el ends here
