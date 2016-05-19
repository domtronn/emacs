(defun wlf:triple-split-layout ()
  (wlf:show (wlf:no-layout
    '(| (:left-size-ratio 0.6) file
        (- (:upper-size-ratio 0.4) runner compilation))
    '((:name file :buffer "file buffer")
      (:name runner :buffer "*runner*")
      (:name compilation :buffer "*compilation*")))))

(defun wlf:javascript-repls ()
  (let ((current (current-buffer))
        (repl-ramda (ramda-repl))
        (repl-lodash (lodash-repl))
        (repl-node (node-repl)))
    (wlf:show (wlf:no-layout
               '(| (:left-size-ratio 0.6) js
                   (- (:upper-size-ratio 0.4) ramda
                      (- (:upper-size-ratio 0.5) lodash node)))
               '((:name js :buffer current)
                 (:name ramda :buffer repl-ramda)
                 (:name lodash :buffer repl-lodash)
                 (:name node :buffer repl-node))))
    (select-window (get-buffer-window current))))

(defun wlf:ert-layout ()
  (let* ((test-buffer (format "%s-test.el" (file-name-base (buffer-file-name)))))
    (wlf:show (wlf:no-layout
               '(| (:left-size-ratio 0.6)
                   (- (:upper-size-ratio 0.5) file test)
                   (- (:upper-size-ratio 0.7) ert messages))
               '((:name file :buffer "file buffer")
                 (:name test :buffer test-buffer)
                 (:name messages :buffer "*Messages*")
                 (:name ert :buffer "*ert*"))))))

(defun wlf:codepen-layout ()
  (let ((scss-buf (get-buffer-create "codepen.scss"))
        (html-buf (get-buffer-create "codepen.html"))
        (js-buf (get-buffer-create "codepen.js")))
    (wlf:show (wlf:no-layout
               '(| (:left-size-ratio 0.3) html
                   (| (:left-size-ratio 0.5) scss js))
               '((:name html :buffer html-buf)
                 (:name scss :buffer scss-buf)
                 (:name js :buffer js-buf))))
    (with-current-buffer js-buf (js2-mode))
    (with-current-buffer html-buf (web-mode))
    (with-current-buffer scss-buf (scss-mode))))

(defun wlf:system-layout ()
  (ignore-errors (docker-containers))
  (ignore-errors (docker-images))
  (ignore-errors (jenkins))
  (let ((containers-buf (get-buffer-create "*docker-containers*"))
        (images-buf (get-buffer-create "*docker-images*"))
        (jenkins-buf (get-buffer-create "*jenkins-status*")))
    (wlf:show (wlf:no-layout
               '(- (:upper-size-ratio 0.7) jenkins
                   (- (:upper-size-ratio 0.5) containers images))
               '((:name jenkins :buffer jenkins-buf)
                 (:name containers :buffer containers-buf)
                 (:name images :buffer images-buf))))))

(defun wlf:select-window ()
  "Prompt use to select a window"
  (interactive)
  (let* ((windows-n (length (window-list)))
         (window-nums (number-sequence 49 (+ 48 windows-n))))
    (select-window-by-number (- (read-char-choice "Window: " window-nums) 48))))

(defhydra hydra-window-layout (:color pink :hint nil)
  "
 ^Layouts^           ^|^ ^Sizing^  ^|^ Splitting
-------------------^+^---------^+^----------------
 _S_: System         |  ^ ^ _k_ ^ ^  | _d_: Kill
 _T_: Triple Split   |  _h_ ^+^ _l_  | _x_: Remove
 _C_: CodePen        |  ^ ^ _j_ ^ ^  | _-_: Horizontal
 _E_: Ert Runner     |         | _|_: Vertical
 _R_: REPLs
                         
"
  ("S" (wlf:system-layout))
  ("T" (wlf:triple-split-layout))
  ("C" (wlf:codepen-layout))
  ("E" (wlf:ert-layout))
  ("R" (wlf:javascript-repls))
  ("r" (wlf:select-window) "select")
  ("k" (enlarge-window 1))
  ("j" (shrink-window 1))
  ("l" (enlarge-window-horizontally 1))
  ("h" (shrink-window-horizontally 1))
  ("K" (enlarge-window 10))
  ("J" (shrink-window 10))
  ("L" (enlarge-window-horizontally 10))
  ("H" (shrink-window-horizontally 10))
  ("x" (delete-window))
  ("d" (lambda () (kill-buffer) (delete-window)))
  ("|" (split-window-horizontally))
  ("-" (split-window-vertically))
  ("q" nil "quit"))

(provide 'hydra-window-layout)
