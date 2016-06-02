(defun hydra-embrace/add (f)
  "Invoke exprand region function F before call `embrace-add`"
  (funcall `(lambda () (interactive) (,f) (embrace-add)))
  (hydra-embrace/nil))

(defhydra hydra-embrace (:color pink :hint nil)
  "
^Mark^                     ^Pairs^             ^Quotes^
^^^^^^-----------------------^-------------------^--------^---------------------------
_w_: word    _._: sentence   _p_: Inside Pairs   _O_: Inside Quotes 
_s_: symbol  _h_: paragraph  _P_: Outside Pairs  _o_: Outside Quotes
_d_: defun

"
  ("w" (hydra-embrace/add 'er/mark-word) :exit t :color teal)
  ("s" (hydra-embrace/add 'er/mark-symbol) :exit t :color teal)
  ("d" (hydra-embrace/add 'er/mark-defun) :exit t :color teal)
  ("p" (hydra-embrace/add 'er/mark-inside-pairs) :exit t :color teal)
  ("P" (hydra-embrace/add 'er/mark-outside-pairs) :exit t :color teal)
  ("O" (hydra-embrace/add 'er/mark-inside-quotes) :exit t :color teal)
  ("o" (hydra-embrace/add 'er/mark-outside-quotes) :exit t :color teal)
  ("." (hydra-embrace/add 'er/mark-text-sentence) :exit t :color teal)
  ("h" (hydra-embrace/add 'er/mark-text-paragraph) :exit t :color teal)
  ("q" nil "quit" :color blue))

(defun hydra-embrace-or-native ()
  "Invokes `hydra-embrace/body` unless the region is active"
  (interactive)
  (if (region-active-p) (embrace-add) (hydra-embrace/body)))

(provide 'hydra-embrace)
