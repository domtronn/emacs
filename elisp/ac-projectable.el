;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun ac-projectable-file-candidates ()
  (mapcar 'file-name-sans-extension (--filter (s-matches? "\.js$" it) (mapcar 'car projectable-file-alist))))

(defun ac-projectable-json-file-candidates ()
  (mapcar 'file-name-sans-extension (--filter (s-matches? "\.json$" it) (mapcar 'car projectable-file-alist))))

(defvar ac-source-project-files
	'((candidates . ac-projectable-file-candidates)
		(candidate-face . ac-projectable-candidate-face)
		(selection-face . ac-projectable-selection-face)
		(limit . 5)
		(symbol . "js")
		(action . require-relative-module-at-point)))

(defvar ac-source-json-project-files
	'((candidates . ac-projectable-json-file-candidates)
		(candidate-face . ac-projectable-json-candidate-face)
		(selection-face . ac-projectable-json-selection-face)
		(limit . 5)
		(symbol . "json")
		(action . require-relative-module-at-point)))

(defvar ac-source-requirejs-files
	'((candidates . ac-projectable-file-candidates)
		(candidate-face . ac-projectable-candidate-face)
		(selection-face . ac-projectable-selection-face)
		(limit . 5)
		(symbol . "R")
		(action . inject-dependency-at-point)))


(defface ac-projectable-json-candidate-face
  '((((class color)) :background "#2B3B40" :foreground "#F36428")
    (t :bold t))
  "*Face to highlight projectable files before expansion"
       :group 'auto-complete)

(defface ac-projectable-json-selection-face
  '((((class color)) :background "#2B3B40" :foreground "#BF4E1F")
    (t :bold t))
  "*Face to highlight projectable files before expansion"
       :group 'auto-complete)

(defface ac-projectable-candidate-face
  '((((class color)) :background "#2B3B40" :foreground "#99cc99")
    (t :bold t))
  "*Face to highlight projectable files before expansion"
       :group 'auto-complete)

(defface ac-projectable-selection-face
  '((((class color)) :background "#2B3B40" :foreground "#698b22")
    (t :bold t))
  "*Face to highlight projectable files before expansion"
  :group 'auto-complete)

(provide 'ac-projectable)
