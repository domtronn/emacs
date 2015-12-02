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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst base-path (file-name-directory load-file-name))

(setq custom-file (concat base-path "init/custom.el"))

(require 'package)
(setq-default package-user-dir (concat base-path "packages/elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'benchmark-init)

(require 'use-package)

(use-package functions :load-path "init")

(use-package linum-off)

(use-package drag-stuff
  :config (drag-stuff-global-mode 1)
	:bind ("s-N" . drag-stuff-down)
				("s-P" . drag-stuff-up))

(use-package mon-css-color
	:load-path "elisp"
  :init (autoload 'css-color-mode "mon-css-color" "" t)
  :config (css-color-global-mode))

(use-package rainbow-delimiters)
(use-package paren)

(use-package multiple-cursors
	:bind ("s-n" . mc/mark-next-symbol-like-this)
				("s-p" . mc/mark-previous-symbol-like-this)
				("M-<mouse-1>" . mc/add-cursor-on-click))

(use-package expand-region
	:bind ("M-q" . er/expand-region))

(use-package ibuffer
  :config
  (bind-keys :map ibuffer-mode-map
             ("G" . ibuffer-vc-set-filter-groups-by-vc-root)
             ("M-u" . ibuffer-unmark-all)))

(use-package smart-forward
  :bind ("s-." . smart-forward)
        ("C-." . smart-forward)
        ("s-," . smart-backward)
        ("C-," . smart-backward))

(use-package smart-newline
  :bind ("RET" . smart-newline))

(use-package smartparens
  :config (smartparens-global-mode)
          (sp-local-pair '(minibuffer-inactive-mode lisp-mode emacs-lisp-mode text-mode) "'" nil :actions nil)
          (sp-with-modes sp--lisp-modes (sp-local-pair "(" nil :bind "s-(")))

(use-package operate-on-number
  :bind ("s-@" . operate-on-number-at-point))

(use-package smex
  :config (smex-initialize)
	:bind ("M-x" . smex)
				("M-X" . smex-major-mode-commands)
				("C-c M-x" . execute-extended-command))

(use-package popup)
(use-package popwin
  :config (popwin-mode 1)
          (setq popwin:close-popup-window-timer-interval 0.1)
          (setq popwin:close-popup-window-timer nil))

(use-package undo-tree
  :config (global-undo-tree-mode)
	:bind ("s-z" . undo-tree-undo)
				("s-Z" . undo-tree-redo)
				("s-y" . undo-tree-redo)
				("C-+" . undo-tree-redo))

(use-package etags-select)

(use-package git-gutter-fringe
  :init (use-package rfringe
          :config (set-fringe-mode '(2 . 0)))
  :config (global-git-gutter-mode))

(use-package erc
  :config
  (use-package tls))

(use-package image+)
(use-package dired+
  :config
  (bind-keys :map dired-mode-map
             ("q" . kill-all-dired-buffers)))

(use-package git-timemachine)
(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package ack-and-a-half)

(use-package nameless
  :defer t
  :config (bind-keys :map nameless-mode-map ("C-c c" . nameless-insert-name)))

(use-package window-layout
  :config
  (defvar trip-split-layout
    (wlf:no-layout
     '(| (:left-size-ratio 0.6) file
         (- (:upper-size-ration 0.4) runner compilation))
     '((:name file :buffer "file buffer")
       (:name runner :buffer "**")
       (:name compilation :buffer "*compilation*")))))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package projectable
  :load-path "elisp/projectable"
  :config
	(projectable-global-mode)
	(add-hook 'projectable-toggle-test-fallback-hook 'projectable-find-test)
	:bind
	([C-tab] . projectable-find-file)
	("C-S-<tab>" . projectable-find-file-other-window)
	("C-x p c" . projectable-change)
	("C-x C-b" . projectable-switch-buffer))

(use-package visual-regexp
  :bind ("C-c r" . vr/replace)
        ("C-c q" . vr/query-replace)
        ("C-c m" . vr/mc-mark)
        ("s-r" . vr/query-replace))

(use-package helm
   :bind ("s-V" . helm-show-kill-ring))
(use-package helm-swoop
                  :bind ("M-o" . helm-swoop)
     :init (use-package helm-flx
             :config (helm-flx-mode)))

(use-package json)
(use-package json-snatcher)

(global-prettify-symbols-mode)
(use-package js2-mode
  :config
  (use-package js2-refactor)
  (use-package js-dependency-injector
    :load-path "elisp/js-dependency-injector")
  (add-hook 'js2-mode-hook 'js-injector-minor-mode)
  (add-hook 'js2-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'js2-mode-hook '(lambda () (key-combo-common-load-default)))
  (add-hook 'js2-mode-hook '(lambda () (tern-mode t)))
  (add-hook 'js2-mode-hook
            '(lambda ()
               (push '("function" . ?ƒ) prettify-symbols-alist)
               (push '("R" . ?Λ) prettify-symbols-alist)
               (push '("R.__" . ?ρ) prettify-symbols-alist)
               (push '("_" . ?λ) prettify-symbols-alist)
               (push '("err" . ?ε) prettify-symbols-alist)
               (push '("_.map" . ?↦) prettify-symbols-alist)
               (push '("R.map" . ?↦) prettify-symbols-alist)
               (push '("<=" . ?≤) prettify-symbols-alist)
               (push '(">=" . ?≥) prettify-symbols-alist)))

  (bind-keys :map js2-mode-map
             ("H-." . jump-to-thing-at-point)
             ("C-c C-n" . js2-next-error)
             ("C-x c" . grunt-exec)

             ;; JS2 Refactor things
             ("C-c C-m" . context-coloring-mode)
             ("C-c C-e" . js2r-extract-var)
             ("C-c C-i" . js2r-inline-var)
             ("C-c C-f" . js2r-extract-function)
             ("C-c C-r" . js2r-rename-var)
             ("C-c C-l" . js2r-log-this)
             ("C-c ." . js2-jump-to-definition)
             ("C-k" . js2r-kill)))

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

(use-package cpp
  :mode ("\\.cpp" . c++-mode)
        ("\\.h" . c++-mode)
  :init
  (bind-keys :map c++-mode-map
             ("M-q" . er/expand-region)
             ("C-c C-p" . flycheck-previous-error)
             ("C-c C-n" . flycheck-next-error)))

(add-hook 'c++-mode-hook
					(lambda () (unless (file-exists-p "makefile")
									(set (make-local-variable 'compile-command)
											 (let ((file (file-name-sans-extension buffer-file-name)))
												 (format "g++ %s -o %s" buffer-file-name file))))))

(add-hook 'scss-mode-hook
					(lambda () (set (make-local-variable 'compile-command)
										 (let ((file (file-name-sans-extension buffer-file-name)))
											 (format "sass '%s':%s.css" buffer-file-name file)))))

(use-package lisp-mode
	:mode ("\\.el" . emacs-lisp-mode)
	:config
	(bind-keys :map emacs-lisp-mode-map
						 ("C-c C-l" . elisp-debug)
						 ("H-." . jump-to-find-function)))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(use-package context-coloring-mode :defer t)

(use-package ace-jump-mode
	:bind
	("C-c SPC" . ace-jump-char-mode)
	("C-c C-x SPC" . ace-jump-zap-to-char)
	("C-c C-SPC" . ace-jump-word-mode))

(use-package tern
  :config
  (add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1)))
  (use-package tern-auto-complete
    :config (tern-ac-setup)))

(use-package flyspell
  :config (bind-keys :map flyspell-mode-map
                     ("M-/" . flyspell-popup-correct)))
(add-hook 'flyspell-mode 'flyspell-popup-auto-correct-mode)

(add-hook 'latex-mode 'flyspell-mode)
(add-hook 'text-mode 'flyspell-mode)

(add-hook 'js-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'ruby-mode-hook '(lambda () (find-tags-file-upwards)))
(add-hook 'java-mode-hook '(lambda () (find-tags-file-upwards)))



(use-package key-combo
  :config (key-combo-mode 1)
          (key-combo-load-default))

(use-package yasnippet
  :defer t
  :config
	(setq yas-snippet-dirs (concat base-path "/snippets"))
	(add-hook 'after-init-hook 'yas-global-mode))

(use-package neotree
	:bind ([f1] . neotree-toggle)
	("<S-f1>" . neotree-find))

(use-package shell-pop
	:bind ("C-`" . shell-pop)
	:config
	(setq shell-pop-window-position "bottom")
	(setq shell-pop-window-size 40)
	(setq shell-pop-shell-type
				'("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse) ; Used for unique buffer names)
  (setq uniquify-separator "/")              ; including parts of the path
  (setq uniquify-after-kill-buffer-p t)      ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*"))  ; don't muck with special buffers

(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path '("/usr/local/bin" "/usr/bin" "/bin"))

(use-package web-mode
  :mode
  ("\\.phtml" . web-mode)
  ("\\.html" . web-mode)
  ("\\.spv" . web-mode)
  ("\\.tpl\\.php" . web-mode)
  ("\\.[agj]sp" . web-mode)
  ("\\.as[cp]x" . web-mode)
  ("\\.erb" . web-mode)
  ("\\.mustache" . web-mode)
  ("\\.hbs" . web-mode)
  ("\\.djhtml" . web-mode)
  ("\\.partial" . web-mode)
  :config (bind-keys :map web-mode-map
                     ("s-/" . web-mode-comment-or-uncomment))
  (add-to-list 'web-mode-ac-sources-alist
               '("html" . (ac-source-html-attribute-value
                           ac-source-html-tag
                           ac-source-html-attribute))))


(use-package auto-complete
  :init
  (use-package auto-complete-config)
  (use-package ac-dabbrev)
  (set-default 'ac-sources
               '(ac-source-yasnippet
                 ac-source-semantic
                 ac-source-dabbrev
                 ac-source-files-in-current-dir))
  :config
  (ac-config-default)
	(global-auto-complete-mode t)
  (add-to-list 'ac-modes 'latex-mode)
  (bind-keys :map ac-completing-map ("\e" . ac-stop))
  (bind-keys :map ac-complete-mode-map
             ([tab] . ac-expand-common)
             ([return] . ac-complete)
             ("C-f" . ac-isearch)
             ("C-n" . ac-next)
             ("C-p" . ac-previous))
	:bind
	([S-tab] . auto-complete))

(add-hook 'web-mode-hook 'ac-html-enable)

(add-hook 'LaTeX-mode-hook
					'(lambda () (setq ac-sources
											 (append '(ac-source-math-unicode
																 ac-source-math-latex
																 ac-source-latex-commands)
															 ac-sources))))

(add-hook 'markdown-mode-hook 'ac-emoji-setup)

(add-hook 'erc-mode-hook '(lambda () (ac-lambda 'ac-source-emoji)))
(add-hook 'git-commit-mode-hook '(lambda () (ac-lambda 'ac-source-gh-issues)))
(add-hook 'ghi-comment-mode-hook '(lambda () (ac-lambda 'ac-source-emoji 'ac-source-gh-issues)))

(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

;;---------------
;; Mode Hooks
;;---------------
(add-to-list 'auto-mode-alist '("\\.[s]json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.coffee" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.erb" . html-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))

(setq truncate-lines nil)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'css-color-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(use-package livedown
  :load-path "elisp/emacs-livedown")

(use-package hideshowvis
	:init (autoload 'hideshowvis-enable "hideshowvis" nil t)
  :config (hideshowvis-symbols)
	:bind ("s-_" . hs-show-all)
				("s--" . hs-show-block)
				("s-=" . hs-toggle-hiding)
				("s-+" . hs-hide-level))
(add-hook 'prog-mode-hook 'hideshowvis-minor-mode)

(use-package grunt
  :load-path "~/code/grunt-el"
  :bind ("C-M-g" . grunt-exec))

(use-package repository-root
  :config
  (add-to-list 'repository-root-matchers repository-root-matcher/svn)
  (add-to-list 'repository-root-matchers repository-root-matcher/git))

(use-package magit-gh-issues
	:load-path "elisp/magit-gh-issues"
	:init (add-hook 'magit-mode-hook 'magit-gh-issues-mode)
	:config (use-package magit-gh-issues-emoji
						:load-path "elisp/magit-gh-issues-emoji"))

(use-package magit
  :config (bind-keys :map magit-mode-map
                     ("C-c c" . magit-whitespace-cleanup)
                     ("C-<tab>" . projectable-find-file)))

(add-hook 'magit-mode-hook 'image-minor-mode)

;; Load stuff to do with grep initially
(eval-after-load "grep" '(grep-compute-defaults))

;; change vc-diff to use vc-ediff
(setq ediff-split-window-function (quote split-window-horizontally))
(setq ediff-keep-variants nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)

(add-hook 'ediff-startup-hook 'ediff-swap-buffers)

(add-hook 'vc-annotate-mode-hook 'sticky-window-delete-other-windows)
(add-hook 'magit-status-mode-hook 'sticky-window-delete-other-windows)
(add-hook 'magit-branch-manager-mode-hook 'sticky-window-delete-other-windows)

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
(setq-default tab-width 2)
(setq js-indent-level 2)

;; Get rid of stupid menu bar and Tool Bar..
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(show-paren-mode t)   ; Show paranthesis matching

;; Ido Support
(use-package ido
  :init
  (use-package flx-ido :config (flx-ido-mode 1))
  (use-package ido-ubiquitous :config (ido-ubiquitous-mode 1))
  (use-package ido-vertical-mode :config (ido-vertical-mode 1))
  :config
  (ido-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)                     ; For dired use C-j to quit at that path
  (setq ido-enable-regexp t)
  (setq ido-create-new-buffer 'always))

;; Global Mode Stuff
(global-linum-mode 1) ; enable line numbers

(use-package powerline :load-path "elisp")

;;------------------
;; My Load Files
;;------------------

(setq custom-file (concat base-path "init/custom.el"))
(add-to-list 'custom-theme-load-path (concat base-path "/packages/themes"))

(add-hook 'after-init-hook
					'(lambda ()
						 (load-file (concat base-path "init/keys.el"))
						 (load-file (concat base-path "init/custom.el"))
						 (load-file (concat base-path "init/advice.el"))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(server-start)

;; Local Variables:
;; eval: (flycheck-mode 0)
;; End:

(provide 'init)
;;; init.el ends here
