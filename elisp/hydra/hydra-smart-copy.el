(defun hydra/smart-copy (f)
  "Invoke exprand region function F before call `smart-copy-add`"
  (funcall
   `(lambda () (interactive)
      (save-excursion
        (,f)
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (keyboard-quit)))))

(defhydra hydra-smart-copy (:color pink :hint nil)
  "
^Mark^                    | ^Files^        | ^Pairs^            | ^Quotes^
------------------------+--------------^+^------------------^+^-----------------------
_w_: word    _._: sentence  | _d_: file dir  | _P_: Inside Pairs  | _O_: Inside Quotes
_s_: symbol  _h_: paragraph | _f_: file name | _p_: Outside Pairs | _o_: Outside Quotes
_d_: defun   _u_: url       | _F_: full path

"
  ("w" (hydra/smart-copy 'er/mark-word) :exit t :color teal)
  ("s" (hydra/smart-copy 'er/mark-symbol) :exit t :color teal)
  ("d" (hydra/smart-copy 'er/mark-defun) :exit t :color teal)
  ("P" (hydra/smart-copy 'er/mark-inside-pairs) :exit t :color teal)
  ("p" (hydra/smart-copy 'er/mark-outside-pairs) :exit t :color teal)
  ("O" (hydra/smart-copy 'er/mark-inside-quotes) :exit t :color teal)
  ("o" (hydra/smart-copy 'er/mark-outside-quotes) :exit t :color teal)
  ("." (hydra/smart-copy 'er/mark-text-sentence) :exit t :color teal)
  ("h" (hydra/smart-copy 'er/mark-text-paragraph) :exit t :color teal)
  ("F" (kill-new (buffer-file-name)) :exit t :color teal)
  ("f" (kill-new (file-name-nondirectory (buffer-file-name))) :exit t :color teal)
  ("d" (kill-new (file-name-directory (buffer-file-name))) :exit t :color teal)
  ("u" (hydra-smart-copy 'er/mark-url) :exit t :color teal)
  ("m" avy-goto-char "move")
  ("q" nil "quit"))

(provide 'hydra-smart-copy)
