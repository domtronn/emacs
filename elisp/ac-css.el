;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun ac-css-selector--candidates (find filter)
  (-mapcat
	 (lambda (buf) (with-current-buffer (buffer-name buf)
							(let ((css-face 'css-selector)
										(results))
								(goto-char (point-max))
								(while (ignore-errors (goto-char (previous-single-property-change (point) 'face)))
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
