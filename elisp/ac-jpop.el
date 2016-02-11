;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(require 'dash)

(defun ac-jpop-file-candidates ()
  (car (mapcar
		'(lambda (p)
			 (let ((project (car p))
						 (files (cdr p)))
				 (mapcar '(lambda (it)
										(popup-make-item
										 (file-name-sans-extension (car it))
										 :summary (file-name-extension (car it))
										 :document (format "Project: %s\n\n%s" project (ac-jpop-doc-directory (cadr it)))))
								 (--filter (s-matches? "\.js[on]\\{0,2\\}$" (car it)) files))))
		jpop-project-alist)))

(defun ac-jpop-doc-directory (dir)
	(let ((split-dir (reverse (split-string (file-name-directory dir) "/"))))
		(if (> (length split-dir) 5)
				(format ".../%s" (s-join "/" (reverse (car (-split-at 5 split-dir)))))
			dir)))
  
(defvar ac-source-project-files
	'((candidates . ac-jpop-file-candidates)
		(candidate-face . ac-jpop-candidate-face)
		(selection-face . ac-jpop-selection-face)
		(limit . 8)
		(action . require-relative-module-at-point)))

(defvar ac-source-requirejs-files
	'((candidates . ac-jpop-file-candidates)
		(candidate-face . ac-jpop-candidate-face)
		(selection-face . ac-jpop-selection-face)
		(limit . 5)
		(action . inject-dependency-at-point)))


(defface ac-jpop-json-candidate-face
  '((((class color)) :background "#2B3B40" :foreground "#F36428")
    (t :bold t))
  "*Face to highlight jpop files before expansion"
       :group 'auto-complete)

(defface ac-jpop-json-selection-face
  '((((class color)) :background "#2B3B40" :foreground "#BF4E1F")
    (t :bold t))
  "*Face to highlight jpop files before expansion"
       :group 'auto-complete)

(defface ac-jpop-candidate-face
  '((((class color)) :background "#2B3B40" :foreground "#99cc99")
    (t :bold t))
  "*Face to highlight jpop files before expansion"
       :group 'auto-complete)

(defface ac-jpop-selection-face
  '((((class color)) :background "#2B3B40" :foreground "#698b22")
    (t :bold t))
  "*Face to highlight jpop files before expansion"
  :group 'auto-complete)

(provide 'ac-jpop)
