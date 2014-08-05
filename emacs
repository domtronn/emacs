;;; Emacs --- my .emacs file setup change


;;; Commentary:

;; Games Dev Websites
;;  enginuity, nehe productions, opengl tutorial

;;; Code:
(load-file (concat USERPATH "/functions.el"))
(add-to-list 'load-path (concat USERPATH "/elisp"))

;;------------------
;; Load Files
;;------------------
(load-file (concat USERPATH "/elisp/buffer_move.el"))
(load-file (concat USERPATH "/elisp/framemove.el"))
(load-file (concat USERPATH "/elisp/move_line_region.el"))
(load-file (concat USERPATH "/elisp/highlight_current_line.el"))
(load-file (concat USERPATH "/elisp/js2-mode.el"))
(load-file (concat USERPATH "/elisp/actionscript-mode.el"))
(load-file (concat USERPATH "/elisp/noflet.el"))
(load-file (concat USERPATH "/elisp/shell-pop.el"))
(load-file (concat USERPATH "/elisp/popwin.el"))
(load-file (concat USERPATH "/elisp/linum-off.el"))
(load-file (concat USERPATH "/elisp/mon-css-color.el"))
(load-file (concat USERPATH "/elisp/etags-select.el"))
(load-file (concat USERPATH "/elisp/sticky-windows.el"))
(load-file (concat USERPATH "/elisp/repo-root.el"))

;;------------------
;; Requires
;;------------------
(if (require 'package)
		(progn (require 'package)
			 (add-to-list 'package-archives 
										'("marmalade" . "http://marmalade-repo.org/packages/"))
			 (add-to-list 'package-archives
										'("melpa" . "http://melpa.milkbox.net/packages/") t)
			 (package-initialize))
	(message "Package is not installed - Are you using Emacs v24 or later?"))

(require 'rainbow-delimiters)
(require 'paren)

(require 'multiple-cursors)   ; Amazing package to allow simultaneous multiline editiing

;(require 'tex)
;(TeX-global-PDF-mode t)

(require 'hideshowvis)
(autoload 'hideshowvis-enable "hideshowvis")
(autoload 'hideshowvis-minor-mode "hideshowvis" 'interactive)
;; (hideshowvis-symbols)

(autoload 'ibuffer "ibuffer" "List buffers." t)

(autoload 'css-color-mode "mon-css-color" "" t)
(css-color-global-mode)

(require 'autopair)
(autopair-global-mode)

(require 'auto-indent-mode)
(auto-indent-global-mode)

(require 'anzu)
(global-anzu-mode +1)

;; (require 'key-chord)
;; (key-chord-mode 1)

;; ;; Extra Packages
;; (require 'scala-mode2)  ;; https://github.com/hvesalai/scala-mode2
;; (require 'feature-mode) ;; https://github.com/michaelklishin/cucumber.el
;; (require 'groovy-mode)  ;; http://groovy.codehaus.org/Emacs+Groovy+Mode

(require 'popup)
(popwin-mode 1)
(setq popwin:close-popup-window-timer-interval 0.1)
(setq popwin:close-popup-window-timer nil)

(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'highlight)
(require 'fill-column-indicator)
(require 'button-lock)

(require 'rvm)
(rvm-use "ruby-2.1.2" "global")

(require 'filecache)

(require 'json)
(require 'json-snatcher)

(defun js-mode-bindings ()
	"Sets a hotkey for using the json-snatcher plugin"
	(when (string-match  "\\.json$" (buffer-name))
		(local-set-key (kbd "C-c C-c") 'jsons-print-path)))

(add-hook 'js-mode-hook 'js-mode-bindings)
(add-hook 'js2-mode-hook 'js-mode-bindings)

(eval-after-load 'js '(define-key js-mode-map (kbd "<s-down-mouse>") 'button-lock-mode))
(eval-after-load 'js '(define-key js-mode-map (kbd "s-B") 'update-javascript-dependency))
(eval-after-load 'js '(define-key js-mode-map (kbd "s-b") 'inject-javascript-dependency))
(eval-after-load 'js '(define-key js-mode-map (kbd "s-§") 'button-lock-mode))
(add-hook 'js-mode-hook 'js-hlt-nonused-dependencies)
(add-hook 'js-mode-hook #'(lambda () (add-hook 'after-save-hook 'js-hlt-nonused-dependencies)))
(add-hook 'js-mode-hook #'(lambda () (add-hook 'after-save-hook 'add-file-to-ext-lib-cache)))
(add-hook 'js-mode-hook #'(lambda () (add-hook 'after-save-hook 'add-file-to-project-cache)))
(add-hook 'js-mode-hook #'(lambda () 
			    (button-lock-set-button "\\.\\(\\w+\\)("
						    #'(lambda (event)
										(interactive "e")
										(save-excursion
											(mouse-set-point event)
											(etags-select-find (thing-at-point 'word))
											))
								:face 'function-link :mouse-face 'function-mouse-link 
								:face-policy 'append :grouping 1 :mouse-binding 'mouse-1)))

;(add-hook 'latex-mode 'flyspell-mode)

(add-hook 'etags-select-mode-hook #'(lambda () (message "Enabling Button Lock Mode")))
(add-hook 'etags-select-mode-hook #'(lambda () 
			    (button-lock-mode 1)
			    (button-lock-set-button "^\\([0-9]+\\).*"
						    #'(lambda (event)
										(interactive "e")
										(save-excursion
											(mouse-set-point event)
											(beginning-of-line)
											(etags-select-by-tag-number-without-prompt (thing-at-point 'symbol))))
								:face 'inherit :mouse-face 'mouse-over :face-policy 'append)))

(require 'rfringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode)

(require 'git-messenger)

(require 'flycheck-tip)
(require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode) ;; Enable flycheck globally
(add-hook 'javascript-mode-hook
         (lambda () (flycheck-mode t)))
(flycheck-tip-use-timer 'verbose)

;; (add-hook 'scala-mode-hook '(lambda () (find-tags-file-upwards)))
;; (add-hook 'groovy-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'js-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'java-mode-hook '(lambda () (find-tags-file-upwards)))

(require 'yasnippet)
(setq yas-snippet-dirs (concat USERPATH "/snippets"))
(yas/load-directory (concat USERPATH "/snippets"))

(require 'auto-complete-config)
(require 'auto-complete-etags)
;(require 'auto-complete-auctex)
(require 'ac-dabbrev)
(add-to-list 'ac-dictionary-directories (concat USERPATH "/elisp/ac-dict"))
(ac-config-default)

(ac-set-trigger-key "TAB")
(define-key ac-complete-mode-map [tab] 'ac-expand-common)
(define-key ac-completing-map "\e" 'ac-stop) ; use esc key to exit completion
(define-key ac-complete-mode-map [return] 'ac-complete)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-b") 'ac-previous)
(define-key ac-complete-mode-map (kbd "s-1") 'ac-complete-select-1)
(define-key ac-complete-mode-map (kbd "s-2") 'ac-complete-select-2)
(define-key ac-complete-mode-map (kbd "s-3") 'ac-complete-select-3)
(define-key ac-complete-mode-map (kbd "s-4") 'ac-complete-select-4)
(define-key ac-complete-mode-map (kbd "s-5") 'ac-complete-select-5)
(define-key ac-complete-mode-map (kbd "s-6") 'ac-complete-select-6)
(global-set-key "\C-f" 'ac-isearch)

(set-default 'ac-sources '(
									 ac-source-yasnippet
									 ac-source-etags
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

(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
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
(add-to-list 'auto-mode-alist '("\\.coffee" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.erb" . html-mode))

;; (add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
;; (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(setq js2-basic-offset 2)
(setq js2-enter-indents-newline t)
;;(setq js2-use-font-lock-faces t)

(setq framemove-hook-into-windmove t)
(setq truncate-lines t)
(setq org-agenda-include-diary t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'hideshowvis-enable)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'css-color-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'js-mode-hook '(lambda () (modify-syntax-entry ?_ "w"))) ; Add Underscore as part of word syntax
;; (add-hook 'js-mode-hook '(lambda () (add-hook 'write-contents-hooks 'format-code))) ; Run code formatting before save

;; Allow cmd clicking on functions depricated by button locks
;; (eval-after-load 'js
;; 	'(define-key js-mode-map (kbd "<s-mouse-1>")
;; 		 (lambda (event)
;;     (interactive "e")
;;     (let ((posn (elt event 1)))
;;       (with-selected-window (posn-window posn)
;;         (goto-char (posn-point posn))
;; 				(etags-select-find-tag-at-point))))))
	

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

;; Java Mode - Malabar Mode
;; (require 'cedet)
;; (require 'semantic)

;; (semantic-mode 1)
;; (require 'malabar-mode)
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))       

;; (add-to-list 'load-path (concat USERPATH "/elisp/jdee/lisp"))
;; (load "jde")

(add-to-list 'repository-root-matchers repository-root-matcher/svn)
(add-to-list 'repository-root-matchers repository-root-matcher/git)

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

(add-hook 'vc-dir-mode-hook (lambda () (local-set-key (kbd "K") #'vc-dir-kill-all-lines-at-mark)))
(add-hook 'vc-dir-mode-hook (lambda () (local-set-key (kbd "d") #'vc-ediff)))
(add-hook 'vc-dir-mode-hook (lambda () (local-set-key (kbd "q") #'kill-this-buffer)))
(add-hook 'vc-dir-mode-hook (lambda () (local-set-key (kbd "r") #'vc-revert)))
(add-hook 'vc-dir-mode-hook (lambda () (local-set-key (kbd "c") #'vc-resolve-conflicts)))
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(add-hook 'dirtree-mode-hook
		  (lambda () (local-set-key (kbd "<return>") #'tree-mode-toggle-expand)))

;; Startup variables
(setq shift-select-mode t)                  ; Allow for shift selection mode
(setq inhibit-startup_message t)            ; disable start up message
(setq inhibit-startup-echo-area-message t)
(setq inhibit-splash-screen t)              ; disable splash screen
(setq make-backup-files nil)                ; don't make backup files
(setq create-lockfiles nil)		              ; don't make lock files
(setq auto-save-default nil)                ; don't autosave
(setq visible-bell nil)                       ; Disbales beep and use visible bell 
(setq ns-function-modifier 'hyper)          ; set Hyper to Mac's Fn key

(delete-selection-mode 1)										; Allows for deletion when typing over highlighted text
(fset 'yes-or-no-p 'y-or-n-p)               ; Use y or n instead of yes or no

(setq-default cursor-type 'bar)             ; Change cursor to bar
(setq-default indent-tabs-mode t)           ; always replace tabs with spaces
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq dired-listing-switches "-alk")        ; dired less info

;; Get rid of stupid menu bar and Tool Bar.. 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq skeleton-pair t)
(setq skeleton-pair-on-word t)

(show-paren-mode t)   ; Show paranthesis matching

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
(set-fringe-mode '(nil . 0))

;; Set start up dimesnions in characters
(maximize-frame)

;; ;; Tree file browser
(require 'tree-mode)
(require 'windata)
(require 'dirtree)
(setq dirtree-windata (quote (frame left 0.2 delete)))

(define-derived-mode dirtree-mode tree-mode "Dir-Tree"
  "A mode to display tree of directory"
  (tree-widget-set-theme "ASCII"))

;; Set translucency
;;(set-frame-parameter (selected-frame) 'alpha '(85 50))
;;(add-to-list 'default-frame-alist '(alpha 85 50))

;;------------------
;; My Key Shortcuts
;;------------------
(load-file (concat USERPATH "/cacheproject.el"))
(load-file (concat USERPATH "/keys.el"))

;; Load Theme
(load-file (concat USERPATH "/emacs.packages/dgc-dark-theme.el"))

(server-start)
