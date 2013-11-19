;; my (dgc) .emacs file setup change
;; Games Dev Websites
;;;; enginuity, nehe productions, opengl tutorial

(add-to-list 'load-path (concat USERPATH "/elisp"))
;; Some various useful functions to remember! ;;
;;rgrep                                        ;
;;    - find files containing a regexp         ;
;;      in a directory                         ;
;;describe-key & describe-function             ;
;;    - does what it says on the tin           ;
;; vc-ediff   (C-x v =)                        ;
;; vc-revert  (C-x v u)                        ;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------
;; Load Files
;;------------------
(load-file (concat USERPATH "/elisp/buffer_move.el"))
(load-file (concat USERPATH "/elisp/framemove.el"))
(load-file (concat USERPATH "/elisp/move_line_region.el"))
(load-file (concat USERPATH "/elisp/highlight_current_line.el"))
(load-file (concat USERPATH "/elisp/js2-mode.el"))
(load-file (concat USERPATH "/elisp/actionscript-mode.el"))

;;------------------
;; Requires
;;------------------
(require 'sunrise-commander)  ; Used for showing tree view of directories
(require 'sunrise-x-tree)     ; use sunrise and then sr-tree-view

(require 'rainbow-delimiters)
(require 'paren)
(require 'anything-match-plugin)
(require 'anything-config)

(require 'multiple-cursors)   ; Amazing package to allow simultaneous multiline editiing

(require 'hideshowvis)
(autoload 'hideshowvis-enable "hideshowvis" )
(autoload 'hideshowvis-minor-mode "hideshowvis" 'interactive)
(hideshowvis-symbols)

(autoload 'ibuffer "ibuffer" "List buffers." t)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat USERPATH "/elisp/ac-dict"))
(ac-config-default)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse) ; Used for unique buffer names 
(setq uniquify-separator "/")              ; including parts of the path
(setq uniquify-after-kill-buffer-p t)      ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")   ; don't muck with special buffers

(if (= JSHINTMODE 1)
		(progn 
			;; (add-to-list 'load-path (concat USERPATH "/elisp/jshint-mode"))
			;; (require 'flymake-jshint)
			;; (require 'flymake-node-jshint)
			(message (concat "JSHINT MODE IS -> JSHINTMODE"))
			(add-to-list 'load-path "/usr/local/lib/node_modules/jshint-mode")
			;; (require 'flymake-jshint)
			(add-hook 'javascript-mode-hook
						(lambda () (flymake-mode t)))
			;; (eval-after-load "flycheck"
			;; 	'(add-hook 'flycheck-mode 'flycheck-color-mode-line))
			)
		nil)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path
      '(
    "/usr/local/bin"
    "/usr/bin"
		"/bin"
    ))

(if (require 'package)
		(progn (require 'package)
			 (add-to-list 'package-archives 
										'("marmalade" .
											"http://marmalade-repo.org/packages/"))
			 (package-initialize))
	(message "Package is not installed - Are you using Emacs v24 or later?"))


;; (load-file "~/skeletons.el") ; NO LONGER USED

(load-file (concat USERPATH "/cacheproject.el"))

;; Load js2 mode for javascript files
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;---------------
;; Mode Hooks
;;---------------
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(setq js2-basic-offset 2)
(setq js2-enter-indents-newline t)
;;(setq js2-use-font-lock-faces t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'hideshowvis-enable)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'flycheck-mode);

(add-hook 'js-mode-hook 'js2-minor-mode)

(add-to-list 'js2-global-externs "require")
(add-to-list 'js2-global-externs "log")

;; Java Script 2 mode to load accepted variables to stop warnings for known globals
(add-hook 'js2-post-parse-callbacks
	  (lambda ()
	    (when (> (buffer-size) 0)
	      (let ((btext (replace-regexp-in-string
			    ": *true" " "
			    (replace-regexp-in-string "[\n\t ]+" " " 
						      (buffer-substring-no-properties 1 (buffer-size)) t t))))
		(mapc (apply-partially 'add-to-list 'js2-additional-externs)
		      (split-string
		       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) 
			   (match-string-no-properties 1 btext) "")
		       " *, *" t))
		))))

;; change vc-diff to use vc-ediff
(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'vc-ediff))
(setq ediff-split-window-function (quote split-window-horizontally))
(setq ediff-keep-variants nil)
(add-hook 'ediff-startup-hook
					'ediff-swap-buffers)

;; Startup variables
(setq shift-select-mode t)                  ; Allow for shift selection mode
(setq inhibit-startup_message t)            ; disable start up message
(setq inhibit-startup-echo-area-message t)
(setq inhibit-splash-screen t)              ; disable splash screen
(setq make-backup-files nil)                ; don't make backup files
(setq auto-save-default nil)                ; don't autosave
(setq visible-bell t)                       ; Disbales beep and use visible bell 
(setq ns-function-modifier 'hyper)          ; set Hyper to Mac's Fn key

(fset 'yes-or-no-p 'y-or-n-p)               ; Use y or n instead of yes or no

(setq-default cursor-type 'bar)             ; Change cursor to bar
(setq-default indent-tabs-mode t)           ; always replace tabs with spaces
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq dired-listing-switches "-alk")        ; dired less info

;; Get rid of stupid menu bar and Tool Bar.. 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;(setq skeleton-pair t)
;(setq skeleton-pair-on-word t)

(show-paren-mode t)   ; Show paranthesis matching
(desktop-save-mode 1) ; Used to restore previous sessions

;; Ido Support
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)                     ; For dired use C-j to quit at that path
(setq ido-enable-regexp t)
(setq ido-use-finename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".js" ".json" ".css" ".as" ".php" ".emacs" ".ini" ".el" ".ini" ".cfg" ".cnf"))

;; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

;; Global Mode Stuff
(setq global-linum-mode t) ; enable line numbers
(global-linum-mode 1) ; enable line numbers
(global-rainbow-delimiters-mode 1)

;; Set start up dimesnions in characters
(maximize-frame)

;; Set translucency
;;(set-frame-parameter (selected-frame) 'alpha '(85 50))
;;(add-to-list 'default-frame-alist '(alpha 85 50))

;;------------------
;; My Key Shortcuts
;;------------------
(load-file (concat USERPATH "/functions.el"))
(load-file (concat USERPATH "/keys.el"))

;; Load Theme
;; (dgc-set-chalkboard)
(dgc-set-dark)
