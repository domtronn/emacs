(defhydra hydra-expand-region (:color pink :hint nil)
  "
^Mark^                     ^Pairs^             ^Quotes^
^^^^^^-----------------------^-------------------^--------^---------------------------
_w_: word    _._: sentence   _p_: Inside Pairs   _O_: Inside Quotes 
_s_: symbol  _h_: paragraph  _P_: Outside Pairs  _o_: Outside Quotes
_d_: defun

"
  ("w" (er/mark-word) :exit t :color teal)
  ("s" (er/mark-symbol) :exit t :color teal)
  ("d" (er/mark-defun) :exit t :color teal)
  ("p" (er/mark-inside-pairs) :exit t :color teal)
  ("P" (er/mark-outside-pairs) :exit t :color teal)
  ("O" (er/mark-inside-quotes) :exit t :color teal)
  ("o" (er/mark-outside-quotes) :exit t :color teal)
  ("." (er/mark-text-sentence) :exit t :color teal)
  ("h" (er/mark-text-paragraph) :exit t :color teal)
  ("q" nil "quit" :color blue))

(provide 'hydra-expand-region)
