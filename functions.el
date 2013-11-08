;;; functions.el --- 
;;; common functions used in emacs

;; History
;; 

;; ============================================================================
(defun dgc-kill-line ()
  "Kill from beginning of line to beginning of next."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((bol (point)))
      (end-of-line)
      (kill-region bol (1+ (point))))))

;; ============================================================================
(defun dgc-copy-line ()
  "Copy from beginning of line to end of line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((bol (point)))
      (end-of-line)
      (forward-char)
      (copy-region-as-kill bol (point)))))

;; ============================================================================
(defun dgc-scroll-line-one (event)
  "Scroll the line in the file maintaining cursor position"
  (interactive "e")
  (condition-case nil
      (if (< (car (cdr (cdr event))) 0)
	  (dgc-scroll-up-in-place 1)
	(dgc-scroll-down-in-place 1))
    (error nil)))

;; ============================================================================
(defun domtronn-timestamp ()
  "Insert history entry"
  (interactive)
  (insert (format-time-string "%d-%b-%Y")))

;; ============================================================================
(defun domtronn-sign ()
	"Insert my name and data"
	(interactive)
	(insert "-- Dominic Charlesworth (dgc336@gmail.com)\n   ")
	(domtronn-timestamp))

;; ============================================================================
(defun domtronn-sign-professional ()
	"Insert my name and data"
	(interactive)
	(insert "-- Dominic Charlesworth (DominicCharlesworth@bbc.co.uk)\n   ")
	(domtronn-timestamp))

;; ============================================================================
(defun dgc-scroll-line-five (event)
  "Scroll the line in the file maintaining cursor position"
  (interactive "e")
  (condition-case nil
      (if (< (car (cdr (cdr event))) 0)
	  (dgc-scroll-up-in-place 5)
	(dgc-scroll-down-in-place 5))
    (error nil)))

;; ============================================================================
(defun dgc-scroll-up-in-place (x)
  "Scroll (up) one line in the file maintaining cursor position in window"
  (interactive)
  (scroll-up x)
  (next-line x))
    
;; ============================================================================
(defun dgc-scroll-down-in-place (x)
  "Scroll down one line in the file maintaining cursor position in window"
  (interactive)
  (scroll-down x)
  (previous-line x))

;; ============================================================================
(defun dgc-forward-word ()
   "Move one word forward. Leave the pointer at start of word"
   (interactive)
   (forward-char 1)
   (backward-word 1)
   (forward-word 2)
   (backward-word 1)
   (backward-char 1)
   (cond ((looking-at "_") (forward-char 1) (dgc-forward-word))
         (t (forward-char 1))))

;; ============================================================================
(defun dgc-forward-word-2 ()
   "Move one word forward. Leave the pointer at end of word"
   (interactive)
   (forward-word 1))

;; ============================================================================
(defun dgc-backward-word ()
   "Move one word backward. Leave the pointer at start of word"
   (interactive)
   (backward-word 1)
   (backward-char 1)
   (cond ((looking-at "_") (dgc-backward-word))
         (t (forward-char 1))))

;; ============================================================================
(defun dgc-backward-word-2 ()
   "Move one word backward. Leave the pointer at end of word"
   (interactive)
   (backward-word 2)
   (forward-word 1))

;; ============================================================================
(defun dgc-indent-buffer ()
  "indents whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun dgc-indent-newline ()
  "indents to the correct amount"
  (interactive)
  (progn (newline-and-indent)
         (backward-char 2)))

;; ============================================================================
(defun dgc-comment ()
  "comment or uncomment highlighted region or line" 
  (interactive)																			
  (if mark-active																		
      (comment-or-uncomment-region (region-beginning) (region-end)) 
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))) 

;; ============================================================================
(defun dgc-scratch ()										
  "Switches buffer to *scratch*"
  (interactive)
  (switch-to-buffer) "*scratch*")

;; ============================================================================
(defun close-all-buffers ()
  "Kills all open buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; ============================================================================
(defun random-colour ()
	"Returns a random colour"
	(stringp)
	(random t)
	(message (nth (random (length (defined-colors))) (defined-colors)) ""))

(defun random-hex ()
  "Return a string in the form of #FFFFFF. Choose the number for
   #xffffff randomly using Emacs Lisp's builtin function (random)."
  ;; seed our random number generator: current datetime plus Emacs's
  ;; process ID
	(interactive)
  (random t)
  (message "%s" (format "#%06x" (random #xffffff)))
  )
;; ============================================================================
(defun ahahah ()
	"You know what it displays..."
	(interactive)
	(random t)
	(setq clr (nth (random (length (defined-colors))) (defined-colors)))
	(message "%s" (propertize "Ah ah ah, you didn't say the magic word!" 
											'face 
											`(:foreground ,clr))))

;; ============================================================================
(defun toggle-transparency ()
  "Toggles transpacernt"
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      ;; ('active 'inactive)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(40 15))))

(defun ido-sort-mtime ()
	(setq ido-temp-list
				(sort ido-temp-list 
							(lambda (a b)
								(time-less-p
								 (sixth (file-attributes (concat ido-current-directory b)))
								 (sixth (file-attributes (concat ido-current-directory a)))))))
	(ido-to-end  ;; move . files to end (again)
	 (delq nil (mapcar
							(lambda (x) (and (char-equal (string-to-char x) ?.) x))
							ido-temp-list))))

(defun rmr ()
	(interactive)
	(progn (rainbow-delimiters-mode 0)
				 (rainbow-delimiters-mode 1)))

 (defun rotate-windows ()
   "Rotate your windows" 
   (interactive) 
   (cond 
    ((not (> (count-windows) 1))
     (message "You can't rotate a single window!"))
    (t
     (setq i 1)
     (setq numWindows (count-windows))
     (while  (< i numWindows)
       (let* (
              (w1 (elt (window-list) i))
              (w2 (elt (window-list) (+ (% i numWindows) 1)))
              (b1 (window-buffer w1))
              (b2 (window-buffer w2))
              (s1 (window-start w1))
              (s2 (window-start w2)))
         (set-window-buffer w1  b2)
         (set-window-buffer w2 b1)
         (set-window-start w1 s2)
         (set-windowe-start w2 s1)
         (setq i (1+ i)))))))

(defun open-test ()
	"Open the test file of currently viewed .js file"
	(interactive)
	(ido-find-file-in-dir (replace-regexp-in-string
												 (buffer-name)
												 ""
												 (replace-regexp-in-string 
													"script"
													"script-tests\/tests"
													(buffer-file-name)))))

;; ============================================================================
;; Functions to set appearance of emacs
;; ============================================================================
(defun dgc-set-default ()
  "Default is the dark bright theme"
  (interactive)
  (load-file (concat USERPATH "/cs_darkbright.el")))
;; ============================================================================
(defun dgc-set-dark ()
  "Dark theme"
  (interactive)
  (load-file (concat USERPATH "/cs_dark.el")))
;; ============================================================================
(defun dgc-set-chalkboard ()
  "Chalkboard theme"
  (interactive)
  (load-file (concat USERPATH "/cs_chalkboard.el")))
;; ============================================================================
(defun dgc-set-light ()
  "Light theme"
  (interactive)
  (load-file (concat USERPATH "/cs_light.el")))

;; ============================================================================
(defun json-format ()
	(interactive)
	(save-excursion
		(shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
		)
	)

;; ----------------------------------------------------------------------------
;; MACROS
;; ----------------------------------------------------------------------------

; Clears the double blank lines from a file leaving only single lines
(fset 'clear-all-double-lines
   [?\M-> ?\C-u ?0 ?\M-x ?c ?l ?e ?a ?r ?- ?d ?o ?u ?b ?l ?e ?- ?l ?i ?n ?e return ?\C-x ?\C-o ?\C-x ?\C-o ?\C-x ?\C-o])

(fset 'clear-double-line
   "\C-r^\C-q\C-j$\C-x\C-o")

; Hide all functions in a file using hs minor mode
(fset 'hide-prev-function
   [?\C-r ?: ?. ?* ?f ?u ?n ?c ?t ?i ?o ?n ?\C-e ?\s-- ?\C-a])

(fset 'hide-all-functions
   [?\M-> ?\C-u ?0 ?\M-x ?h ?i ?d ?e ?- ?p ?r ?e ?v ?- ?f ?u ?n ?c ?t ?i ?o ?n return ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e])
