;;; cs_darkbright.el -- 

;; Sets the colour scheme for emacs to a Dark style
;; 
;; Available colours can be found at :
;; http://raebear.net/comp/emacscolors.html

;; Font
(set-face-attribute 'default nil :font "-outline-Lucida Console-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")

;; Font locking. Turn it on in all modes that support it.
(cond ((fboundp 'global-font-lock-mode)
       (global-font-lock-mode t)
       ;; Maximum colours would be nice!
       (setq font-lock-maximum-decoration t)))
(font-lock-mode 1)

;; Font Locking Colours
(set-face-foreground (quote font-lock-function-name-face) "pink")
(set-face-foreground (quote font-lock-comment-face) "lightblue")
(set-face-foreground (quote font-lock-constant-face) "orange")
(set-face-foreground (quote font-lock-type-face) "aquamarine")
(set-face-foreground (quote font-lock-keyword-face) "yellow")
(set-face-foreground (quote font-lock-string-face) "lightfreen")
(set-face-foreground (quote font-lock-variable-name-face) "lightgoldenrod");

;; Switch off the maximum font locking limit.
(setq font-lock-maximum-size nil)

;; My colours
(setq fg "white")
(setq bg "black")
(setq bg-2 "black")
(setq link "skyblue")
(setq hover "red")
(setq heading "lightgreen")

(set-background-color bg)
(set-foreground-color fg)

(set-face-foreground (quote mode-line) "black")
(set-face-background (quote mode-line) "white")
(set-face-foreground (quote region) fg)
(set-face-background (quote region) bg-2)
(set-face-foreground (quote highlight) hover)
(set-face-background (quote highlight) bg)
(set-face-foreground (quote secondary-selection) fg)
(set-face-background (quote secondary-selection) bg-2)
(set-face-foreground (quote underline) link)
(set-face-foreground (quote italic) link)
(set-face-foreground (quote bold-italic) link)
(set-face-foreground (quote bold) heading)
(set-face-background (quote fringe) bg-2)

(set-cursor-color "yellow")

(blink-cursor-mode 0)

