;;; init.el --- My Emacs initialisation file.

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
(defconst base-path (file-name-directory load-file-name))
(add-to-list 'load-path (concat base-path "elisp"))

(setq custom-file (concat base-path "custom.el"))
(load-file (concat base-path "functions.el"))

;;------------------
;; Load Files
;;------------------

(require 'linum-off  "linum-off.elc")

(require 'drag-stuff "drag-stuff.elc")
(drag-stuff-global-mode 1)

(require 'mon-css-color "mon-css-color.elc")
(autoload 'css-color-mode "mon-css-color" "" t)
(css-color-global-mode)

(require 'package)
(setq-default package-user-dir (concat base-path "packages/elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
	
(require 'rainbow-delimiters)
(require 'paren)

(require 'multiple-cursors)

(require 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (local-set-key (kbd "G") #'ibuffer-vc-set-filter-groups-by-vc-root)))
(define-key ibuffer-mode-map (kbd "M-u") 'ibuffer-unmark-all)

(require 'smart-forward)
(require 'smart-newline)
(require 'smartparens)
(smartparens-global-mode)

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-with-modes sp--lisp-modes (sp-local-pair "(" nil :bind "s-("))

(require 'operate-on-number)

(require 'smex)
(smex-initialize)

(require 'popup)
(require 'popwin)
(popwin-mode 1)
(setq popwin:close-popup-window-timer-interval 0.1)
(setq popwin:close-popup-window-timer nil)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'etags-select)

(require 'rfringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode)

(require 'image+)
(require 'dired+)
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "q") 'kill-all-dired-buffers)))

(require 'git-timemachine)
(require 'git-messenger)

(require 'ack-and-a-half)

(require 'nameless)
(add-hook 'nameless-mode-hook (lambda () (local-set-key (kbd "C-c c") 'nameless-insert-name)))

(require 'window-layout)
(defvar cpp-layout
	(wlf:no-layout
	 '(| (:left-size-ratio 0.6) file
			 (- (:upper-size-ration 0.4) runner compilation))
	 '((:name file :buffer "file buffer")
		 (:name runner :buffer "*cpp-runner*")
		 (:name compilation :buffer "*compilation*"))))

(defvar beacon-layout
	(wlf:no-layout
	 '(| (:left-size-ratio 0.7) file runner)
	 '((:name file :buffer "file buffer")
     (:name runner :buffer "*gulp-watch*"))))

(require 'flycheck)
(global-flycheck-mode)

(require 'projectable (expand-file-name (concat base-path "elisp/projectable/projectable.el")))
(projectable-global-mode)

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
(define-key global-map (kbd "s-r") 'vr/query-replace)

(require 'helm-flx)
(helm-flx-mode)

(require 'json)
(require 'json-snatcher)

(require 'js2-mode)
(require 'js2-refactor)
(require 'js-dependency-injector "js-dependency-injector/js-dependency-injector.el")

(add-hook 'js2-mode-hook 'js-injector-minor-mode)

(global-prettify-symbols-mode)
(add-hook 'js2-mode-hook
					'(lambda ()
						 (push '("function" . ?ƒ) prettify-symbols-alist)
						 (push '("_" . ?λ) prettify-symbols-alist)
						 (push '("R" . ?Λ) prettify-symbols-alist)
						 (push '("R.__" . ?ρ) prettify-symbols-alist)
						 (push '("err" . ?ε) prettify-symbols-alist)
						 (push '("_.map" . ?↦) prettify-symbols-alist)
						 (push '("R.map" . ?↦) prettify-symbols-alist)
						 (push '("<=" . ?≤) prettify-symbols-alist)
						 (push '(">=" . ?≥) prettify-symbols-alist)))

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

(define-key c++-mode-map (kbd "M-q") 'er/expand-region)
(define-key c++-mode-map (kbd "C-c C-p") 'flycheck-previous-error)
(define-key c++-mode-map (kbd "C-c C-n") 'flycheck-next-error)
(add-hook 'c++-mode-hook
					(lambda () (unless (file-exists-p "makefile")
									(set (make-local-variable 'compile-command)
											 (let ((file (file-name-sans-extension buffer-file-name)))
												 (format "g++ %s -o %s" buffer-file-name file))))))

(add-hook 'scss-mode-hook
					(lambda () (set (make-local-variable 'compile-command)
										 (let ((file (file-name-sans-extension buffer-file-name)))
											 (format "sass '%s':%s.css" buffer-file-name file)))))

(eval-after-load 'js '(define-key js2-mode-map (kbd "s-B") 'update-dependencies))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c s-B") 'sort-dependencies))
(eval-after-load 'js '(define-key js2-mode-map (kbd "s-b") 'inject-dependency-at-point))
(eval-after-load 'js '(define-key js2-mode-map (kbd "M-s-∫") 'require-dependency-at-point))
(eval-after-load 'js '(define-key js2-mode-map (kbd "H-.") 'jump-to-thing-at-point))
(eval-after-load 'js '(define-key js2-mode-map (kbd "M-.") 'go-to-thing-at-point))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c C-n") 'js2-next-error))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-x c") 'grunt-exec))

;; JS2 Refactor things
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c C-m") 'context-coloring-mode))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c C-e") 'js2r-extract-var))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c C-i") 'js2r-inline-var))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c C-f") 'js2r-extract-function))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c C-r") 'js2r-rename-var))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c C-l") 'js2r-log-this))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-c .") 'js2-jump-to-definition))
(eval-after-load 'js '(define-key js2-mode-map (kbd "C-k") 'js2r-kill))

(add-hook 'js2-mode-hook '(lambda () (key-combo-common-load-default)))

(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'elisp-debug)
(define-key emacs-lisp-mode-map (kbd "H-. ") 'jump-to-find-function)

(add-hook 'js2-mode-hook '(lambda () (tern-mode t)))
(add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1)))
(eval-after-load 'tern
	'(progn
		 (require 'tern-auto-complete)
		 (tern-ac-setup)))

(require 'flyspell)
(add-hook 'flyspell-mode 'flyspell-popup-auto-correct-mode)

(add-hook 'latex-mode 'flyspell-mode)
(add-hook 'text-mode 'flyspell-mode)

(define-key flyspell-mode-map (kbd "M-/") 'flyspell-popup-correct)

(add-hook 'js-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'ruby-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'java-mode-hook '(lambda () (find-tags-file-upwards)))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(require 'key-combo)
(key-combo-mode 1)
(key-combo-load-default)

(require 'yasnippet)
(setq yas-snippet-dirs (concat base-path "/snippets"))
(yas-global-mode)

(defadvice ansi-term (after advise-ansi-term-coding-system activate)
	"Set the coding system of ansi terminals."
	(set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse) ; Used for unique buffer names
(setq uniquify-separator "/")              ; including parts of the path
(setq uniquify-after-kill-buffer-p t)      ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")   ; don't muck with special buffers

(add-to-list 'custom-theme-load-path (concat base-path "/packages/themes"))
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path '("/usr/local/bin" "/usr/bin" "/bin"))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml" . web-mode))
(add-to-list 'auto-mode-alist '("\\.partial" . web-mode))

(define-key web-mode-map (kbd "s-/") 'web-mode-comment-or-uncomment)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; (require 'auto-complete-auctex)
(require 'ac-dabbrev)
(require 'ac-math)

(define-key ac-completing-map "\e" 'ac-stop) ; use esc key to exit completion
(define-key ac-complete-mode-map [tab] 'ac-expand-common)
(define-key ac-complete-mode-map [return] 'ac-complete)
(define-key ac-complete-mode-map (kbd "C-f") 'ac-isearch)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)

(set-default 'ac-sources '(
									ac-source-yasnippet
									ac-source-semantic
									ac-source-dabbrev
									ac-source-files-in-current-dir))

(add-hook 'web-mode-hook 'ac-html-enable)
(add-to-list 'web-mode-ac-sources-alist
						 '("html" . (ac-source-html-attribute-value
												 ac-source-html-tag
												 ac-source-html-attribute)))

(add-to-list 'ac-modes 'latex-mode)
(add-hook 'LaTeX-mode-hook
					'(lambda () (setq ac-sources
											 (append '(ac-source-math-unicode
																 ac-source-math-latex
																 ac-source-latex-commands
																 )
															 ac-sources))))
(setq ac-math-unicode-in-math-p t)

(global-auto-complete-mode t)

(add-hook 'markdown-mode-hook 'ac-emoji-setup)
(add-hook 'git-commit-mode-hook
					'(lambda () (interactive)
						 (auto-complete-mode) (setq-local ac-sources '(ac-source-gh-issues))))
(add-hook 'ghi-comment-mode-hook
					'(lambda () (interactive)
						 (auto-complete-mode) (setq-local ac-sources '(ac-source-emoji ac-source-gh-issues))))

(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

;;---------------
;; Mode Hooks
;;---------------
(add-to-list 'auto-mode-alist '("\\.[s]json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.coffee" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.erb" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.spv?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))

(setq truncate-lines nil)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'css-color-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(require 'livedown (expand-file-name (concat base-path "elisp/emacs-livedown/livedown.el")))

(require 'hideshowvis)
(hideshowvis-symbols)
(add-hook 'prog-mode-hook 'hideshowvis-minor-mode)

(require 'grunt "~/code/grunt-el/grunt.el")

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

(add-hook 'vc-annotate-mode-hook 'sticky-window-delete-other-windows)

(add-hook 'magit-status-mode-hook 'sticky-window-delete-other-windows)
(add-hook 'magit-branch-manager-mode-hook 'sticky-window-delete-other-windows)

(require 'magit-gh-issues (expand-file-name (concat base-path "elisp/magit-gh-issues/magit-gh-issues.el")))
(require 'magit-gh-issues-emoji (expand-file-name (concat base-path "elisp/magit-gh-issues-emoji/magit-gh-issues-emoji.el")))

(add-hook 'magit-mode-hook 'magit-gh-issues-mode)
(add-hook 'magit-mode-hook 'image-minor-mode)

(eval-after-load 'magit '(define-key magit-mode-map (kbd "C-<tab>") 'projectable-find-file))

;; Startup variables
(setq shift-select-mode t)                  ; Allow for shift selection mode
(setq inhibit-splash-screen t)              ; disable splash screen
(setq make-backup-files nil)                ; don't make backup files
(setq create-lockfiles nil)									; don't make lock files
(setq auto-save-default nil)                ; don't autosave
(setq visible-bell nil)                     ; Disbales beep and use visible bell
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
;; (require 'ido-other-window (expand-file-name (concat base-path "elisp/ido-other-window.elc")))
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)                     ; For dired use C-j to quit at that path
(setq ido-enable-regexp t)
(setq ido-create-new-buffer 'always)

;; Global Mode Stuff
(global-linum-mode 1) ; enable line numbers
(set-fringe-mode '(2 . 0))

(require 'powerline "powerline.el")

;;------------------
;; My Load Files
;;------------------

(load-file (concat base-path "keys.el"))
(load-file (concat base-path "advice.el"))
(load-file (concat base-path "custom.el"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(server-start)

;; Local Variables:
;; eval: (flycheck-mode 0)
;; End:

(provide 'init)
;;; init.el ends here
