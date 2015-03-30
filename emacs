;;; emacs --- My Emacs file

;; Copyright (C) 2014  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(toggle-frame-fullscreen)
(load-file (concat USERPATH "/functions.el"))
(add-to-list 'load-path (concat USERPATH "/elisp"))

;;------------------
;; Load Files
;;------------------
(load-file (concat USERPATH "/elisp/lorem-ipsum.el"))
(load-file (concat USERPATH "/elisp/drag-stuff.el"))      ;;; Leave this package for key binding overrides
(load-file (concat USERPATH "/elisp/linum-off.el"))
(load-file (concat USERPATH "/elisp/mon-css-color.el"))

;; Cool but needs some work
;; (load-file (concat USERPATH "/elisp/minimap.el"))

;;------------------
;; Requires
;;------------------
(if (require 'package)
		(progn (require 'package)
					 (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
					 (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
					 (package-initialize))
	(message "Package is not installed - Are you using Emacs v24 or later?"))

(require 'rainbow-delimiters)
(require 'paren)

(require 'multiple-cursors)   ; Amazing package to allow simultaneous multiline editiing

;(require 'tex)
;(TeX-global-PDF-mode t)

(require 'ibuffer-git)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(add-hook 'ibuffer-mode-hook (lambda () (local-set-key (kbd "G") #'ibuffer-vc-set-filter-groups-by-vc-root)))

(autoload 'css-color-mode "mon-css-color" "" t)
(css-color-global-mode)

(require 'smart-forward)
(require 'smart-newline)
(require 'smartparens)
(smartparens-global-mode)

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-with-modes sp--lisp-modes (sp-local-pair "(" nil :bind "s-("))

(require 'operate-on-number)

(add-to-list 'load-path (expand-file-name (concat USERPATH "/elisp/rails-minor-mode")))
(require 'rails)

;; (require 'key-chord)
;; (key-chord-mode 1)

;; ;; Extra Packages
;; (require 'scala-mode2)  ;; https://github.com/hvesalai/scala-mode2
;; (require 'feature-mode) ;; https://github.com/michaelklishin/cucumber.el
;; (require 'groovy-mode)  ;; http://groovy.codehaus.org/Emacs+Groovy+Mode

(require 'smex)

(require 'popup)
(require 'popwin)
(popwin-mode 1)
(setq popwin:close-popup-window-timer-interval 0.1)
(setq popwin:close-popup-window-timer nil)

(autoload 'dash-at-point "dash-at-point"
					"Search the word at point with Dash." t nil)

(global-prettify-symbols-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'highlight)
(require 'button-lock)
(require 'fill-column-indicator)
(setq fci-rule-column 160)

(drag-stuff-global-mode 1)

(require 'etags-select)

(require 'rfringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode)

(require 'peep-dired)
(require 'image+)
(require 'dired+)
(require 'dired-rainbow)
(require 'dired-filter)
(require 'dired-k)
(define-key dired-mode-map (kbd "K") 'dired-k)

(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "P") #'peep-dired)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "q") '(lambda () (interactive) (kill-all-dired-buffers)))))

(require 'git-timemachine)
(require 'git-messenger)

(require 'emms-setup)  ;; brew install mediainfo
(require 'emms-info-mediainfo)
(require 'emms-playing-time)
(require 'emms-mode-line-icon)
(require 'emms-browser)
(require 'emms-get-lyrics)
(emms-all)
(emms-default-players)
(add-to-list 'emms-info-functions #'emms-info-mediainfo)
(setq emms-source-file-default-directory "~/Music/")
(define-key emms-browser-mode-map (kbd "C-S-j")
	'(lambda () (interactive) (progn (emms-browser-add-tracks-and-play) (get-lyrics-and-display))))

(add-to-list 'load-path (concat USERPATH "/elisp/emacs-w3m/"))
(require 'w3m-load)   ;; brew install w3m

(require 'ack-and-a-half)

(require 'flycheck-tip)
(require 'flycheck)
(require 'flycheck-rust)
;; (add-hook 'after-init-hook #'global-flycheck-mode) ;; Enable flycheck globally
(add-hook 'javascript-mode-hook (lambda () (flycheck-mode t)))
(flycheck-tip-use-timer 'verbose)

(add-hook 'rust-mode-hook 'flycheck-mode)

(require 'rvm)
(rvm-use "ruby-2.1.2" "global")

(require 'filecache)

(require 'grunt)

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
(define-key global-map (kbd "s-r") 'vr/query-replace)

(define-key esc-map (kbd "C-r") 'vr/isearch-backward)
(define-key esc-map (kbd "C-s") 'vr/isearch-forward)

(require 'json)
(require 'json-snatcher)

(require 'js2-refactor)
(require 'js2-mode)
(require 'js-dependency-inject)

(defun js-mode-bindings ()
	"Set a hotkey for using the json-snatcher plugin."
	(when (string-match  "\\.json$" (buffer-name))
		(local-set-key (kbd "C-c C-c") 'jsons-print-path)))

(add-hook 'js-mode-hook 'js-mode-bindings)
(add-hook 'js2-mode-hook 'js-mode-bindings)

;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'web-mode-hook 'skewer-html-mode)

;; (eval-after-load 'skewer-mode '(define-key js2-mode-map (kbd "<s-return>") 'skewer-eval-defun))

(eval-after-load 'js '(define-key js2-mode-map (kbd "<s-down-mouse>") 'button-lock-mode))
(eval-after-load 'js '(define-key js2-mode-map (kbd "s-B") 'update-dependencies))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c s-B") 'sort-dependencies))
(eval-after-load 'js '(define-key js2-mode-map (kbd "s-b") 'inject-dependency-at-point))
(eval-after-load 'js '(define-key js2-mode-map (kbd "s-§") 'button-lock-mode))
(eval-after-load 'js '(define-key js2-mode-map (kbd "H-.") 'go-to-thing-at-point))

(add-hook 'js2-mode-hook 'js-hlt-nonused-dependencies)
(add-hook 'js2-mode-hook #'(lambda () (add-hook 'after-save-hook 'js-hlt-nonused-dependencies)))
(add-hook 'js2-mode-hook #'(lambda () 
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

;; (add-hook 'scala-mode-hook '(lambda () (find-tags-file-upwards)))
;; (add-hook 'groovy-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'js-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'ruby-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'java-mode-hook '(lambda () (find-tags-file-upwards)))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(require 'yasnippet)
(setq yas-snippet-dirs (concat USERPATH "/snippets"))
(yas-global-mode)

(add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1)))
(defadvice ansi-term (after advise-ansi-term-coding-system activate)
	"Set the coding system of ansi terminals."
	(set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(require 'auto-complete-config)
(require 'auto-complete-etags)
;(require 'auto-complete-auctex)
(require 'ac-dabbrev)
(add-to-list 'ac-dictionary-directories (concat USERPATH "/elisp/ac-dict"))
(ac-config-default)

(define-key ac-completing-map "\e" 'ac-stop) ; use esc key to exit completion
(define-key ac-complete-mode-map [tab] 'ac-expand-common)
(define-key ac-complete-mode-map [return] 'ac-complete)
(define-key ac-complete-mode-map (kbd "C-f") 'ac-isearch)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "s-1") 'ac-complete-select-1)
(define-key ac-complete-mode-map (kbd "s-2") 'ac-complete-select-2)
(define-key ac-complete-mode-map (kbd "s-3") 'ac-complete-select-3)
(define-key ac-complete-mode-map (kbd "s-4") 'ac-complete-select-4)
(define-key ac-complete-mode-map (kbd "s-5") 'ac-complete-select-5)
(define-key ac-complete-mode-map (kbd "s-6") 'ac-complete-select-6)

(set-default 'ac-sources '(
									 ac-source-yasnippet
									 ac-source-etags
									 ac-source-semantic
									 ac-source-dabbrev									 
									 ac-source-files-in-current-dir
									 ))

(require 'osx-dictionary)
(push "*osx-dictionary*" popwin:special-display-config)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse) ; Used for unique buffer names 
(setq uniquify-separator "/")              ; including parts of the path
(setq uniquify-after-kill-buffer-p t)      ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")   ; don't muck with special buffers

(add-to-list 'custom-theme-load-path (concat USERPATH "/emacs.packages/themes"))
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path
			'(
				"/usr/local/bin"
				"/usr/bin"
				"/bin"
				))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;;---------------
;; Mode Hooks
;;---------------
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.coffee" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.erb" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
;; (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(setq js2-enter-indents-newline t)

(setq framemove-hook-into-windmove t)
(setq truncate-lines nil)
(setq org-agenda-include-diary t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'css-color-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(add-hook 'js-mode-hook 'js2-mode)
(add-hook 'js2-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'js2-mode-hook '(lambda () (modify-syntax-entry ?_ "w"))) ; Add Underscore as part of word syntax
;; (add-hook 'js-mode-hook '(lambda () (add-hook 'write-contents-hooks 'format-code))) ; Run code formatting before save

;; Allow cmd clicking on functions deprecated by button locks
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
(require 'semantic)
(semantic-mode 1)
(require 'cedet)
(add-to-list 'load-path (concat USERPATH "/elisp/malabar-mode/src/main/lisp"))
(require 'malabar-mode)
(setq malabar-groovy-lib-dir (concat USERPATH "/elisp/malabar-mode/target"))
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(eval-after-load 'malabar-mode '(setq malabar-groovy-java-options nil))
(eval-after-load 'malabar-mode '(define-key malabar-mode-map (kbd "H-.") 'my-malabar-jump-to-thing))
(eval-after-load 'malabar-mode '(define-key malabar-mode-map (kbd "H->") 'malabar-find-implementations))
(eval-after-load 'malabar-mode '(define-key malabar-mode-map (kbd "H-<") 'malabar-start-find-parent))
(eval-after-load 'malabar-mode '(define-key malabar-mode-map (kbd "s-b") 'malabar-import-one-class))
(eval-after-load 'malabar-mode '(define-key malabar-mode-map (kbd "s-B") 'malabar-import-sort-imports))
(eval-after-load 'malabar-mode '(define-key malabar-mode-map (kbd "M-q") 'er/expand-region))
(add-hook 'malabar-mode-hook #'(lambda () (add-hook 'after-save-hook 'add-file-to-project-cache)))

(require 'repository-root)
(add-to-list 'repository-root-matchers repository-root-matcher/svn)
(add-to-list 'repository-root-matchers repository-root-matcher/git)

;; Load stuff to do with grep initially
(eval-after-load "grep" '(grep-compute-defaults))

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
(add-hook 'vc-dir-mode-hook (lambda () (local-set-key (kbd "P") #'magit-push)))
(add-hook 'vc-dir-mode-hook (lambda () (local-set-key (kbd "c") #'vc-resolve-conflicts)))
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(add-hook 'vc-annotate-mode-hook (lambda () (sticky-window-delete-other-windows)))

(add-hook 'magit-status-mode-hook (lambda () (sticky-window-delete-other-windows)))
(add-hook 'magit-branch-manager-mode-hook (lambda () (sticky-window-delete-other-windows)))

(add-hook 'dirtree-mode-hook (lambda () (local-set-key (kbd "<return>") #'tree-mode-toggle-expand)))

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
;; (setq dired-listing-switches "-alk")        ; dired less info

;; Get rid of stupid menu bar and Tool Bar.. 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(show-paren-mode t)   ; Show paranthesis matching

;; Ido Support
(require 'flx-ido)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)                     ; For dired use C-j to quit at that path
(setq ido-enable-regexp t)
(setq ido-create-new-buffer 'always)
(setq-default ido-file-extensions-order '(".js" ".java" ".json" ".css" ".as" ".php" ".xml" ".emacs" ".ini" ".el" ".ini" ".cfg" ".cnf"))

;; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

;; Global Mode Stuff
(global-linum-mode 1) ; enable line numbers
(set-fringe-mode '(2 . 0))

;; ;; Tree file browser
(require 'moe-theme)
(require 'dirtree)
(setq dirtree-windata (quote (frame left 0.2 delete)))

;;------------------
;; My Load Files
;;------------------
(load-file (concat USERPATH "/cacheproject.el"))
(load-file (concat USERPATH "/keys.el"))
(load-file (concat USERPATH "/advice.el"))

(load-file (concat USERPATH "/elisp/powerline.el"))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
	"Set the height of the minibuffer strings."
	(set (make-local-variable 'face-remapping-alist)
			 '((default :height 1.0))))

(server-start)

(provide 'emacs)
;;; emacs ends here
