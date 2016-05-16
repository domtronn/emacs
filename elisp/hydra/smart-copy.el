(defun hydra-smart-copy (f)
  "Invoke exprand region function F before call `smart-copy-add`"
  (funcall
   `(lambda () (interactive)
      (save-excursion
        (,f)
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (keyboard-quit)))))

(defhydra hydra-smart-copy (:color pink :hint nil)
  "
^Mark^                     ^Pairs^             ^Quotes^
^^^^^^-----------------------^-------------------^--------^---------------------------
_w_: word    _._: sentence   _P_: Inside Pairs   _O_: Inside Quotes 
_s_: symbol  _h_: paragraph  _p_: Outside Pairs  _o_: Outside Quotes
_d_: defun

"
  ("w" (hydra-smart-copy 'er/mark-word) :exit t :color teal)
  ("s" (hydra-smart-copy 'er/mark-symbol) :exit t :color teal)
  ("d" (hydra-smart-copy 'er/mark-defun) :exit t :color teal)
  ("P" (hydra-smart-copy 'er/mark-inside-pairs) :exit t :color teal)
  ("p" (hydra-smart-copy 'er/mark-outside-pairs) :exit t :color teal)
  ("O" (hydra-smart-copy 'er/mark-inside-quotes) :exit t :color teal)
  ("o" (hydra-smart-copy 'er/mark-outside-quotes) :exit t :color teal)
  ("." (hydra-smart-copy 'er/mark-text-sentence) :exit t :color teal)
  ("h" (hydra-smart-copy 'er/mark-text-paragraph) :exit t :color teal)
  ("q" nil "quit" :color blue))

(provide 'hydra-smart-copy)
