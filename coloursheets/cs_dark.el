;;; cs_dark.el -- 

;; Sets the colour scheme for emacs to a Dark style
;; 
;; Available colours can be found at :
;; http://raebear.net/comp/emacscolors.html

;; Font

(set-face-attribute 'default nil :font "-outline-Lucida Console-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1" :height 110)

;; Font locking. Turn it on in all modes that support it.
(cond ((fboundp 'global-font-lock-mode)
       (global-font-lock-mode t)
       ;; Maximum colours would be nice!
       (setq font-lock-maximum-decoration t)))
(font-lock-mode 1)

(highlight-current-line-on t) 
(set-face-background 'highlight-current-line-face "gray25")

;; Font Locking Colours
(set-face-foreground (quote font-lock-comment-face) "dimgray")
(set-face-foreground (quote font-lock-comment-delimiter-face) "gray25")
(set-face-foreground (quote font-lock-builtin-face) "steelblue")

(set-face-foreground (quote font-lock-function-name-face) "darkorange2")
(set-face-foreground (quote font-lock-constant-face) "tomato2")
(set-face-foreground (quote font-lock-type-face) "lightsteelblue")
(set-face-foreground (quote font-lock-keyword-face) "gold")
(set-face-foreground (quote font-lock-string-face) "darkolivegreen3")
(set-face-foreground (quote font-lock-variable-name-face) "lightgoldenrod2");

;; Font colours for JS2 minor mode

;; Switch off the maximum font locking limit.
(setq font-lock-maximum-size nil)

;; My colours
(setq fg "azure2")
(setq bg "gray18")
(setq bg-2 "gray40")
(setq link "powderblue")
(setq hover "skyblue")
(setq heading "lightgreen")

(set-background-color bg)
(set-foreground-color fg)

(set-face-foreground (quote mode-line) fg)
(set-face-background (quote mode-line) bg)
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

(set-cursor-color "whitesmoke")

(blink-cursor-mode 0)

