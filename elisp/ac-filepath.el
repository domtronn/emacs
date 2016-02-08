(defvar ac-source-filepath
	'((init setq ac-filename-cache nil)
		(candidates . ac-filename-candidate)
		(candidate-face . ac-filepath-candidate-face)
		(selection-face . ac-filepath-selection-face)
		(requires . 0)
		(action . ac-start)
		(limit))
	"An `auto-complete-mode` source for filepaths.")

(defface ac-filepath-candidate-face
  '((((class color)) :background "#2B3B40" :foreground "#9A766F"))
  "*Face to highlight projectable files before expansion"
	:group 'auto-complete)

(defface ac-filepath-selection-face
  '((((class color)) :background "#2B3B40" :foreground "#745D52"))
  "*Face to highlight projectable files before expansion"
  :group 'auto-complete)

(provide 'ac-filepath)
