;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun ac-css-selector--candidates (find filter)
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
	 (--filter (eq (buffer-mode it) 'scss-mode) (buffer-list))))

(defun ac-scss-color-names ()
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
		 (--filter (eq (buffer-mode it) 'scss-mode) (buffer-list))) results))

(defvar ac-source-scss-colors '((candidates . ac-scss-color-names)
																(prefix . "$\\S-+")
																(requires . 0)))

(defun ac-css-selector-candidates () (ac-css-selector--candidates "[.]" "#"))
(defun ac-css-id-candidates () (ac-css-selector--candidates "[#]" "."))

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
  "*Face to highlight projectable files before expansion"
       :group 'auto-complete)

(defface ac-css-selection-face
  '((((class color)) :background "#2B3B40" :foreground "#B2394C"))
  "*Face to highlight projectable files before expansion"
  :group 'auto-complete)


(provide 'ac-css)
