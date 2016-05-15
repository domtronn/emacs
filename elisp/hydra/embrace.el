(defun hydra-embrace/add (f)
  "Invoke exprand region function F before call `embrace-add`"
  (funcall `(lambda () (interactive) (,f) (embrace-add)))
  (hydra-embrace/nil))

(defhydra hydra-embrace (:color pink :hint nil)
  "
^Mark^                     ^Pairs^             ^Quotes^
^^^^^^-----------------------^-------------------^--------^---------------------------
_w_: word    _._: sentence   _P_: Inside Pairs   _Q_: Inside Quotes 
_s_: symbol  _h_: paragraph  _p_: Outside Pairs  _q_: Outside Quotes
_d_: defun

"
  ("w" (hydra-embrace/add 'er/mark-word) :exit t :color teal)
  ("s" (hydra-embrace/add 'er/mark-symbol) :exit t :color teal)
  ("d" (hydra-embrace/add 'er/mark-defun) :exit t :color teal)
  ("P" (hydra-embrace/add 'er/mark-inside-pairs) :exit t :color teal)
  ("p" (hydra-embrace/add 'er/mark-outside-pairs) :exit t :color teal)
  ("Q" (hydra-embrace/add 'er/mark-inside-quotes) :exit t :color teal)
  ("q" (hydra-embrace/add 'er/mark-outside-quotes) :exit t :color teal)
  ("." (hydra-embrace/add 'er/mark-sentence) :exit t :color teal)
  ("h" (hydra-embrace/add 'er/mark-paragraph) :exit t :color teal)
  ("q" nil "quit" :color blue))

(provide 'hydra-embrace)
