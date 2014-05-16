;;; Emacs --- my .emacs file setup change


;;; Commentary:

;; Games Dev Websites
;;  enginuity, nehe productions, opengl tutorial


;;; Code:
(add-to-list 'load-path (concat USERPATH "/elisp"))
(load-file (concat USERPATH "/functions.el"))

;;------------------
;; Load Files
;;------------------
(load-file (concat USERPATH "/elisp/buffer_move.el"))
(load-file (concat USERPATH "/elisp/framemove.el"))
(load-file (concat USERPATH "/elisp/move_line_region.el"))
(load-file (concat USERPATH "/elisp/highlight_current_line.el"))
(load-file (concat USERPATH "/elisp/js2-mode.el"))
(load-file (concat USERPATH "/elisp/shell-pop.el"))
(load-file (concat USERPATH "/elisp/popwin.el"))
(load-file (concat USERPATH "/elisp/linum-off.el"))
(load-file (concat USERPATH "/elisp/mon-css-color.el"))
(load-file (concat USERPATH "/elisp/etags-select.el"))
(load-file (concat USERPATH "/elisp/sticky-windows.el"))

;;------------------
;; Requires
;;------------------
(if (require 'package)
		(progn (require 'package)
			 (add-to-list 'package-archives 
										'("marmalade" .
											"http://marmalade-repo.org/packages/"))
			 (add-to-list 'package-archives
										'("melpa" . 
											"http://melpa.milkbox.net/packages/") t)
			 (package-initialize))
	(message "Package is not installed - Are you using Emacs v24 or later?"))

(require 'rainbow-delimiters)
(require 'paren)

(require 'multiple-cursors)   ; Amazing package to allow simultaneous multiline editiing

(require 'hideshowvis)
(autoload 'hideshowvis-enable "hideshowvis")
(autoload 'hideshowvis-minor-mode "hideshowvis" 'interactive)
(hideshowvis-symbols)

(autoload 'ibuffer "ibuffer" "List buffers." t)

(autoload 'css-color-mode "mon-css-color" "" t)

(css-color-global-mode)

;; ;; Automatically put pairs of delimiters in
;; (require 'autopair)
;; (autopair-global-mode)

;; (require 'key-chord)
;; (key-chord-mode 1)

;; Kevs Packages
(require 'scala-mode2)  ;; https://github.com/hvesalai/scala-mode2
(require 'feature-mode) ;; https://github.com/michaelklishin/cucumber.el
(require 'groovy-mode)  ;; http://groovy.codehaus.org/Emacs+Groovy+Mode

(require 'popup)
(require 'popwin)
(popwin-mode 1)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'tabbar-ruler)
(setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
(setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
(setq *tabbar-ignore-buffers* 
  '("*dirtree*" "*Completions*" "*Ido Completions*" "*Help*" "*ansi-term*" "*ansi-term-1*" "Ibuffer" "*Messages*" "*scratch*" "*Minibuf-0*" "*Minibuf-1*" "*Minibuf-2*"))
(setq tabbar-buffer-list-function
	  (lambda ()
		(remove-if
		 (lambda (buffer)
		   (and (not (eq (current-buffer) buffer)) ; Always include the current buffer.
				(loop for name in *tabbar-ignore-buffers* ;remove buffer name in this list.
					  thereis (string-equal (buffer-name buffer) name))))
          (buffer-list))))

;; (autoload 'dash-at-point "dash-at-point"
;;           "Search the word at point with Dash." t nil)

(require 'rfringe)
(require 'flycheck-tip)
(require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode) ;; Enable flycheck globally

(add-hook 'javascript-mode-hook
         (lambda () (flycheck-mode t)))
(flycheck-tip-use-timer 'verbose)

(add-hook 'js-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'scala-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'java-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'groovy-mode-hook '(lambda () (find-tags-file-upwards)))

(define-key isearch-mode-map (kbd "s-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "s-F") 'isearch-repeat-backward) 

(require 'yasnippet)
(setq yas-snippet-dirs (concat USERPATH "/snippets"))
(yas/load-directory (concat USERPATH "/snippets"))

(require 'auto-complete-config)
(require 'ac-dabbrev)
(add-to-list 'ac-dictionary-directories (concat USERPATH "/elisp/ac-dict"))
(ac-config-default)

(ac-set-trigger-key "TAB")
(define-key ac-complete-mode-map [tab] 'ac-expand-common)
(define-key ac-completing-map "\e" 'ac-stop) ; use esc key to exit completion
(define-key ac-complete-mode-map [return] 'ac-complete)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-b") 'ac-previous)
(global-set-key "\C-f" 'ac-isearch)

(set-default 'ac-sources '(
									 ac-source-yasnippet
									 ac-source-semantic
									 ac-source-dabbrev
									 ac-source-files-in-current-dir
									 ))
;; Smart mode line causes troubles with flymake modes
;; (setq sml/theme 'dark)
;; (require 'smart-mode-line)
;; (sml/setup)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse) ; Used for unique buffer names 
(setq uniquify-separator "/")              ; including parts of the path
(setq uniquify-after-kill-buffer-p t)      ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")   ; don't muck with special buffers

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path
      '(
    "/usr/local/bin"
    "/usr/bin"
		"/bin"
    ))

;;---------------
;; Mode Hooks
;;---------------
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))

(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(setq js2-basic-offset 2)
(setq js2-enter-indents-newline t)
;;(setq js2-use-font-lock-faces t)

(setq framemove-hook-into-windmove t)
(setq truncate-lines t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'hideshowvis-enable)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'css-color-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook '(lambda () (modify-syntax-entry ?_ "w"))) ; Add Underscore as part of word syntax

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

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

;; ;; Java Mode - Malabar Mode
;; (require 'cedet)
;; (require 'semantic)
;; (load "semantic/loaddefs.el")
;; (semantic-mode 1);;
;; (require 'malabar-mode)
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))       

;; (add-to-list 'load-path (concat USERPATH "/elisp/jdee/lisp"))
;; (load "jde")

;; Load stuff to do with grep initially
(eval-after-load "grep"
  '(grep-compute-defaults))

;; change vc-diff to use vc-ediff
(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'vc-ediff))
(setq ediff-split-window-function (quote split-window-horizontally))
(setq ediff-keep-variants nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)

(add-hook 'ediff-startup-hook 'ediff-swap-buffers)

(add-hook 'vc-dir-mode-hook
          (lambda () (local-set-key (kbd "K") #'vc-dir-kill-all-lines-at-mark)))
(add-hook 'vc-dir-mode-hook
          (lambda () (local-set-key (kbd "d") #'vc-ediff)))

(add-hook 'dirtree-mode-hook
		  (lambda () (local-set-key (kbd "<return>") #'tree-mode-toggle-expand)))

;; Startup variables
(setq shift-select-mode t)                  ; Allow for shift selection mode
(setq inhibit-startup_message t)            ; disable start up message
(setq inhibit-startup-echo-area-message t)
(setq inhibit-splash-screen t)              ; disable splash screen
(setq make-backup-files nil)                ; don't make backup files
(setq auto-save-default nil)                ; don't autosave
(setq ns-function-modifier 'hyper)          ; set Hyper to Mac's Fn key

(delete-selection-mode 1)					; Allows for deletion when typing over highlighted text
(fset 'yes-or-no-p 'y-or-n-p)               ; Use y or n instead of yes or no

(setq-default cursor-type 'bar)             ; Change cursor to bar
(setq-default indent-tabs-mode t)           ; always replace tabs with spaces
(setq-default tab-width 4)
(setq js-indent-level 4)
(setq dired-listing-switches "-alk")        ; dired less info

(show-paren-mode t)   ; Show paranthesis matching

;; Ido Support
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)                     ; For dired use C-j to quit at that path
(setq ido-enable-regexp t)
(setq ido-use-finename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".js" ".json" ".css" ".as" ".php" ".emacs" ".ini" ".el" ".ini" ".cfg" ".cnf"))

;; ;; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

;; ;; Global Mode Stuff
(setq global-linum-mode t) ; enable line numbers
(global-linum-mode 1) ; enable line numbers
(global-rainbow-delimiters-mode 1)
(scroll-bar-mode 1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(set-fringe-mode '(nil . 0))

;; ;; Tree file browser
(require 'tree-mode)
(require 'windata)
(require 'dirtree)
(setq dirtree-windata (quote (frame left 0.2 delete)))
(set-up-dir-tree)

;; Set start up dimesnions in characters
(maximize-frame)

;;------------------
;; My Key Shortcuts
;;------------------
(load-file (concat USERPATH "/keys.el"))
(load-file (concat USERPATH "/cacheproject.el"))
