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
(package-initialize nil)

(require 'benchmark-init)
(add-hook
 'benchmark-init/tree-mode-hook
 '(lambda ()
    (local-set-key "i" '(lambda () (interactive) (find-file user-init-file)))
    (local-set-key "s" '(lambda () (interactive) (switch-to-buffer "*scratch*")))
    (local-set-key "p" 'projectable-change)
    (local-set-key "P" 'projectable-change-and-find-file)))

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
  :bind ("H-n" . mc/mark-next-symbol-like-this)
        ("s-n" . mc/skip-to-next-like-this)
        ("H-p" . mc/mark-previous-symbol-like-this)
        ("s-p" . mc/skip-to-previous-like-this)
        ("H-l" . mc/mark-all-symbols-like-this)
        ("M-<mouse-1>" . mc/add-cursor-on-click))

(use-package multi-line
  :bind ("C-c [" . multi-line-single-line)
        ("C-c ]" . multi-line))

(use-package expand-region
  :bind ("M-q" . er/expand-region))

(use-package ibuffer
  :defer t
  :config
  (use-package ibuf-ext
    :config (add-to-list 'ibuffer-never-show-predicates "^\\*"))
  (bind-keys :map ibuffer-mode-map
             ("G" . ibuffer-vc-set-filter-groups-by-vc-root)
             ("M-u" . ibuffer-unmark-all)))

(use-package smart-forward
  :bind ("s-." . forward-sexp)
        ("s-," . backward-sexp)
        ("C-." . smart-forward)
        ("C-," . smart-backward))

(use-package smart-newline
  :bind ("RET" . smart-newline))

(use-package smartparens
  :demand
  :config (smartparens-global-mode)
          (sp-local-pair
           '(minibuffer-inactive-mode lisp-mode emacs-lisp-mode slack-mode text-mode)
           "'" nil :actions nil)
          (sp-with-modes sp-lisp-modes (sp-local-pair "(" nil :bind "s-("))
  :bind ("C-)" . sp-slurp-hybrid-sexp)
        ("s-f" . sp-slurp-hybrid-sexp)
        ("s-b" . sp-forward-barf-sexp))

(use-package operate-on-number
  :bind ("s-@" . operate-on-number-at-point))

(use-package smex
  :config (smex-initialize)
  :bind ("M-x" . smex)
        ("M-X" . smex-major-mode-commands)
        ("C-c M-x" . execute-extended-command))

(use-package popup :defer t)
(use-package popwin
  :demand
  :config (popwin-mode 1)
          (setq popwin:close-popup-window-timer-interval 0.1)
          (setq popwin:close-popup-window-timer nil)
  :bind ("C-x m" . popwin:messages))

(use-package darkroom
  :config (setq darkroom-fringes-outside-margins nil)
          (setq darkroom-margins 0.0)
          (setq darkroom-text-scale-increase 1.0))

(use-package org-mode
  :mode ("\\.org" . org-mode)
  :init
  (add-hook 'org-mode-hook 'darkroom-mode)
  :config
  (org-beamer-mode)
  (bind-keys :map org-mode-map ("s-p" . fill-paragraph)))

(use-package doc-view
  :mode ("\\.pdf" . doc-view-mode)
  :init (add-hook 'doc-view-mode-hook 'darkroom-mode))

(use-package undo-tree
  :config (global-undo-tree-mode)
  :bind ("s-z" . undo-tree-undo)
        ("s-Z" . undo-tree-redo)
        ("s-y" . undo-tree-redo)
        ("C-+" . undo-tree-redo))

(use-package etags-select
  :bind ("H-." . etags-select-find-tag-at-point)
        ("H-?" . etags-select-find-tag))

(use-package git-gutter-fringe
  :if window-system
  :config (global-git-gutter-mode))

(use-package slack
  :commands (slack-start)
  :config
  (require 'slack-auth "~/.slack-auth.el")
  (setq slack-enable-emoji t) ;; if you want to enable emoji, default nil
  (setq slack-client-id     (plist-get slack-auth :client-id))
  (setq slack-client-secret (plist-get slack-auth :client-secret))
  (setq slack-token         (plist-get slack-auth :token))
  (setq slack-user-name "domtronn")
  (add-hook 'slack-mode-hook (lambda () (ac-emoji-setup)
                                           (ac-lambda 'ac-source-emoji)))
  :bind ("C-c C-s C-s" . slack-start)
        ("C-c C-s C-x" . slack-ws-close)
        ("C-c C-s C-i" . slack-im-select)
        ("C-c C-s C-c" . slack-channel-select)
        ("M-@" . slack-message-embed-mention))

(use-package image+ :after 'image-mode)
(use-package dired+
  :after 'dired
  :config
  (bind-keys :map dired-mode-map
             ("q" . kill-all-dired-buffers)))

(use-package git-timemachine :bind ("C-x v t" . git-timemachine))
(use-package git-messenger :bind ("C-x v p" . git-messenger:popup-message))

(use-package nameless
  :defer t
  :config (bind-keys :map nameless-mode-map ("C-c c" . nameless-insert-name)))

(use-package window-layout
  :config
  (defun wlf:trip-split-layout ()
    (interactive)
    (wlf:show (wlf:no-layout
     '(| (:left-size-ratio 0.6) file
         (- (:upper-size-ration 0.4) runner compilation))
     '((:name file :buffer "file buffer")
       (:name runner :buffer "*runner*")
       (:name compilation :buffer "*compilation*")))))

  :bind ("C-c C-w" . wlf:trip-split-layout))


(use-package flycheck-status-emoji :after flycheck)
(use-package flycheck
  :config (global-flycheck-mode)
  (bind-keys :map flycheck-mode-map
             ("C-c C-e" . flycheck-list-errors)
             ("C-c C-n" . flycheck-next-error)
             ("C-c C-p" . flycheck-previous-error))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :bind ("M-}" . flycheck-mode))

(use-package flyspell
  :init (setq flyspell-mode-map (make-sparse-keymap))
  (defun flyspell-toggle ()
    (interactive)
    (if flyspell-mode (flyspell-mode-off) (flyspell-mode)))
  (use-package flyspell-popup :defer t)
  :config (bind-keys :map flyspell-mode-map
                     ("s-]" . flyspell-goto-next-error)
                     ("M-/" . flyspell-popup-correct))
          (advice-add 'flyspell-mode-on :before 'flyspell-buffer)
  :bind ("M-{" . flyspell-toggle))

(use-package projectable
  :load-path "elisp/projectable"
  :config
  (projectable-global-mode)
  (add-hook 'projectable-toggle-test-fallback-hook 'projectable-find-test)
  :bind
  ([C-tab] . projectable-find-file)
  ("C-S-<tab>" . projectable-find-file-other-window)
  ("C-x p f c" . projectable-change-and-find-file)
  ("C-x p c" . projectable-change)
  ("C-x C-b" . projectable-switch-buffer)
  ("C-x C-p" . projectable-switch))

(use-package visual-regexp
  :bind ("C-c r" . vr/replace)
        ("C-c q" . vr/query-replace)
        ("C-c m" . vr/mc-mark)
        ("s-r" . vr/query-replace))

(use-package isearch
  :bind ("H-s" . isearch-forward-symbol-at-point)
        ("C-s" . isearch-forward-regexp)
        ("C-r" . isearch-backward-regexp)

  :init (bind-keys :map isearch-mode-map
                   ("C-;" . helm-swoop-from-isearch)
                   ("C-l" . helm-git-grep-from-isearch)))

(use-package helm :bind ("s-V" . helm-show-kill-ring)
                        ("M-y" . helm-show-kill-ring))
(use-package helm-ls-git
  :bind ("<H-tab>" . helm-ls-git-ls))
(use-package helm-git-grep
  :commands helm-git-grep-from-isearch
  :bind ([f2] . helm-git-grep)
  :config (bind-keys :map helm-git-grep-map
                     ("M-n" . helm-goto-next-file)
                     ("M-p" . helm-goto-precedent-file)))
(use-package helm-swoop
  :commands helm-swoop-from-isearch
  :bind ("M-o" . helm-swoop)
        ("M-O" . helm-swoop-same-face-at-point)
  :config (define-key helm-swoop-map (kbd "C-l") 'helm-git-grep-from-helm))

(global-prettify-symbols-mode)
(push '("->" . ?→) prettify-symbols-alist)
(push '("<-" . ?←) prettify-symbols-alist)
(push '("<=" . ?≤) prettify-symbols-alist)
(push '(">=" . ?≥) prettify-symbols-alist)

(use-package avy
  :bind
  ("s-g" . avy-goto-line)
  ("C-c a" . avy-goto-char)
  ("C-c SPC" . avy-goto-char)
  ("C-c C-a" . avy-goto-word-1)
  ("M-a" . avy-goto-word-1)
  :config
  (avy-setup-default))

(use-package avy-zap :after avy
  :bind
  ("M-z" . avy-zap-up-to-char))

(use-package wgrep
  :after 'helm-files
  :init (defun wgrep-end ()
          (interactive)
          (wgrep-finish-edit)
          (wgrep-save-all-buffers))
        (autoload 'wgrep-change-to-wgrep-mode "browse-url")
  :config (define-key wgrep-mode-map (kbd "C-c C-s") 'wgrep-end)
          (define-key helm-grep-mode-map (kbd "C-c C-p") 'wgrep-change-to-wgrep-mode))

(use-package tern
  :after 'js2-mode
  :config
  (bind-keys :map tern-mode-keymap
             ("M-." . jump-to-thing-at-point)
             ("M-," . pop-tag-mark))
  (use-package tern-auto-complete :config (tern-ac-setup)))

(use-package js2-refactor :after js2-mode)
(use-package js2r-extensions :after js2-mode :load-path "elisp")
(use-package js-dependency-injector
  :after js2-mode
  :load-path "elisp/js-dependency-injector")
(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config
  (add-hook 'js2-mode-hook 'js-injector-minor-mode)
  (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
  (add-hook 'js2-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'js2-mode-hook '(lambda () (key-combo-common-load-default)))
  (add-hook 'js2-mode-hook '(lambda () (tern-mode t)))
  (add-hook 'js2-mode-hook
            (lambda () (if (s-contains? "require.def" (buffer-substring (point-min) (point-max)))
                      (add-to-list 'ac-sources 'ac-source-requirejs-files)
                    (add-to-list 'ac-sources 'ac-source-project-files))))
  (add-hook 'js2-mode-hook
            '(lambda ()
               (push '("function" . ?ƒ) prettify-symbols-alist)
               (push '("var" . ?ν) prettify-symbols-alist)
               (push '("R" . ?Λ) prettify-symbols-alist)
               (push '("R.__" . ?ρ) prettify-symbols-alist)
               (push '("_" . ?λ) prettify-symbols-alist)
               (push '("err" . ?ε) prettify-symbols-alist)
               (push '("return" . ?⇐) prettify-symbols-alist)
               (push '("undefined" . ?∅) prettify-symbols-alist)
               (push '("error" . ?Ε) prettify-symbols-alist)
               (push '("_.map" . ?↦) prettify-symbols-alist)
               (push '("R.map" . ?↦) prettify-symbols-alist)
               (push '("_.compose" . ?∘) prettify-symbols-alist)
               (push '("R.compose" . ?∘) prettify-symbols-alist)
               ;; Key words
               (push '("for" . ?↻) prettify-symbols-alist)
               (push '("while" . ?∞) prettify-symbols-alist)
               (push '("module.exports" . ?⇧) prettify-symbols-alist)
               ;; Maths symbols
               (push '("<=" . ?≤) prettify-symbols-alist)
               (push '(">=" . ?≥) prettify-symbols-alist)
               (push '("!=" . ?≠) prettify-symbols-alist)
               (push '("!==" . ?≢) prettify-symbols-alist)
               (push '("===" . ?≡) prettify-symbols-alist)))

  (bind-keys :map js2-mode-map
             ("C-x c" . grunt-exec)

             ;; JS2 Refactor things
             ("C-c C-m" . context-coloring-mode)
             ("C-c m" . prettify-symbols-mode)
             ("s-P" . js2r-drag-stuff-up)
             ("s-N" . js2r-drag-stuff-down)
             ("C-c C-o" . js2r-order-vars-by-length)
             ("C-c C-s" . js2r-toggle-var-declaration)
             ("C-c C-v" . js2r-extract-var)
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
  :config
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

(use-package json-snatcher :after json)
(use-package json
  :mode ("\\.json" . json-mode)
  :config (add-hook 'json-mode-hook '(lambda () (projectable-stylise "4" t))))

(use-package markdown-mode
  :mode ("\\.md" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook 'ac-emoji-setup)
  (bind-keys* ("M-<left>" . backward-word)
              ("M-<right>" . forward-word))
  (bind-keys :map markdown-mode-map
             ("s-f" . next-link)
             ("s-b" . previous-link)))

(use-package browse-url
  :defer t
  :init (autoload 'browse-url-url-at-point "browse-url"))

(use-package link-hint
  :bind ("s-O" . link-hint-open-link)
        ("H-s-o" . link-hint-open-multiple-links))

(use-package markdown-toc
  :after markdown-mode
  :config (bind-keys :map markdown-mode-map
                     ("C-c C-t g" . markdown-toc-generate-toc)))

(use-package livedown
  :after markdown-mode
  :load-path "elisp/emacs-livedown"
  :config (bind-keys :map markdown-mode-map
                     ("C-c p" . livedown:preview)))

(use-package coffee-mode :mode ("\\.coffee" . coffee-mode))
(use-package scss-mode :mode ("\\.scss$" . scss-mode))
(use-package css-mode :mode ("\\.css$" . css-mode))

(use-package lisp-mode
  :mode ("\\.el" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            '(lambda () (ac-lambda 'ac-source-functions
                              'ac-source-variables
                              'ac-source-filepath
                              'ac-source-yasnippet
                              'ac-source-words-in-same-mode-buffers
                              'ac-source-words-in-same-mode-buffers)))
  :config
  (bind-keys :map emacs-lisp-mode-map
             ("C-c C-l" . elisp-debug)
             ("C-c RET" . context-coloring-mode)
             ("M-." . jump-to-find-function)
             ("M-," . pop-tag-mark)))

(use-package context-coloring-mode
  :defer t
  :config (advice-add 'load-theme :after
                      '(lambda (&rest args) (context-coloring-mode 0))))

(use-package key-combo
  :config (add-to-list 'key-combo-common-mode-hooks 'web-mode-hook)
          (key-combo-mode 1)
          (key-combo-load-default))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (concat base-path "/snippets"))
  (add-hook 'after-init-hook 'yas-global-mode))

(use-package neotree
  :bind ([f1] . neotree-toggle)
  ("<S-f1>" . neotree-find))

(use-package shell-pop
  :bind ("C-`" . shell-pop)
  :config
  (add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1)))
  (custom-set-variables
   '(shell-pop-autocd-to-working-dir nil)
   '(shell-pop-shell-type
     (quote
      ("ansi-term" "*ansi-term*"
       (lambda nil
         (ansi-term shell-pop-term-shell)))))
   '(shell-pop-term-shell "/bin/bash")
   '(shell-pop-window-position "bottom")
   '(shell-pop-window-size 40)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse) ; Used for unique buffer names)
  (setq uniquify-separator "/")              ; including parts of the path
  (setq uniquify-after-kill-buffer-p t)      ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*"))  ; don't muck with special buffers

(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path '("/usr/local/bin" "/usr/bin" "/bin"))

(use-package web-mode
  :load-path "~/.env/elisp/web-mode/"

  :mode
  ("\\.phtml$" . web-mode)
  ("\\.html$" . web-mode)
  ("\\.spv$" . web-mode)
  ("\\.erb$" . web-mode)
  ("\\.mustache$" . web-mode)
  ("\\.hbs$" . web-mode)
  ("\\.partial$" . web-mode)
  ("\\.jsx$" . web-mode)

  :config
  (bind-keys :map web-mode-map
             ("M-;" . semi-colon-end)
             ("s-/" . web-mode-comment-or-uncomment))
  (setq web-mode-ac-sources-alist
        '(("html" . (ac-source-html-tag
                     ac-source-words-in-same-mode-buffers
                     ac-source-html-attr))
          ("css" . (ac-source-css-selector
                    ac-source-css-id
                    ac-source-css-property))
          ("jsx" . (ac-source-yasnippet
                    ac-source-filepath
                    ac-source-words-in-same-mode-buffers
                    ac-source-tern-completion))))

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil)) ad-do-it)
      ad-do-it))

  (add-hook 'web-mode-hook
            (lambda () (when (equal web-mode-content-type "jsx") (tern-mode)))))

(use-package emmet-mode :after web-mode
  :config
  (bind-keys :map web-mode-map
             ("M-P" . key-combo-mode)
             ("C-o" . emmet-expand-yas))
  (bind-keys :map scss-mode-map
             ("M-P" . key-combo-mode)
             ("C-o" . emmet-expand-yas))
  ;; (advice-add 'emmet-expand-yas :before
  ;;             '(lambda () "Bemify the expression before expanding snippet when using the `|bem` filter"
  ;;                (bemify-emmet-string (emmet-expr-on-line))))
  )

(use-package auto-complete-config :after auto-complete)

;; Custom Auto Complete Sources
(use-package ac-projectable :load-path "~/.env/elisp" :after js2-mode)
(use-package ac-filepath :load-path "~/.env/elisp")
(use-package ac-css :load-path "~/.env/elisp" :after (web-mode scss-mode))

(use-package ac-emmet :after emmet-mode)
(use-package ac-html :after web-mode)

(use-package auto-complete
  :demand t
  :config
  (ac-config-default)
  (set-default 'ac-sources
               '(ac-source-yasnippet
                 ac-source-filepath
                 ac-source-words-in-same-mode-buffers))
  (global-auto-complete-mode t)

  (bind-keys :map ac-completing-map ("\e" . ac-stop))
  (bind-keys :map ac-complete-mode-map
             ([tab] . ac-expand-common)
             ([return] . ac-complete)
             ("C-s" . ac-isearch)
             ("C-n" . ac-next)
             ("C-p" . ac-previous))

  :bind ([S-tab] . auto-complete))

(add-hook 'scss-mode-hook
          '(lambda () (ac-lambda
                  'ac-source-yasnippet
                  'ac-source-css-property
                  'ac-source-css-id
                  'ac-source-css-selector
                  'ac-source-filepath
                  'ac-source-scss-colors)
             (emmet-mode)
             (ac-emmet-css-setup)))

(add-hook 'LaTeX-mode-hook
            '(lambda () (ac-lambda
                    'ac-source-math-unicode
                    'ac-source-math-latex
                    'ac-source-latex-commands)))

(add-hook 'LaTeX-mode-hook
          '(lambda () (local-set-key (kbd "C-x c") 'xelatex-make)))
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'darkroom-mode)

(add-hook 'git-commit-mode-hook '(lambda () (ac-lambda 'ac-source-gh-issues)))
(add-hook 'ghi-comment-mode-hook '(lambda () (ac-lambda 'ac-source-emoji 'ac-source-gh-issues)))

;;---------------
;; Mode Hooks
;;---------------
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.erb" . html-mode))

(setq truncate-lines nil)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'css-color-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)

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
  :defer t
  :config
  (add-to-list 'repository-root-matchers repository-root-matcher/svn)
  (add-to-list 'repository-root-matchers repository-root-matcher/git))

(use-package magit-gh-issues
  :disabled t
  :load-path "elisp/magit-gh-issues"
  :after 'magit
  :config (add-hook 'magit-mode-hook 'magit-gh-issues-mode)
          (use-package magit-gh-issues-emoji
            :load-path "elisp/magit-gh-issues-emoji"))

(use-package magit
  :defer t
  :config (bind-keys :map magit-mode-map
                     ("o" . magit-open-file-other-window)
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
(setq create-lockfiles nil)                 ; don't make lock files
(setq auto-save-default nil)                ; don't autosave
(setq visible-bell nil)                     ; Disbales beep and use visible bell
(setq ns-function-modifier 'hyper)          ; set Hyper to Mac's Fn key

;; Set mac modifiers to what I'm used to
(setq mac-function-modifier 'hyper)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(delete-selection-mode 1)                    ; Allows for deletion when typing over highlighted text
(fset 'yes-or-no-p 'y-or-n-p)               ; Use y or n instead of yes or no

(setq frame-title-format "Who's hacking %b?")
(setq-default cursor-type 'bar)             ; Change cursor to bar
(setq-default tab-width 2)
(setq js-indent-level 2)

;; Get rid of stupid menu bar and Tool Bar..
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(show-paren-mode t)   ; Show paranthesis matching

(use-package flx-ido :after ido :config (flx-ido-mode 1))
(use-package ido-ubiquitous :after ido :config (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode  :after ido
  :config (ido-vertical-mode 1)
  (add-hook 'ido-vertical-mode-hook
            '(lambda () (bind-keys :map ido-common-completion-map
                              ("C-f" . ido-next-match)
                              ("C-b" . ido-prev-match)))))
(use-package ido-describe-bindings :after ido
  :bind ("C-h b" . ido-describe-bindings))
(use-package ido-other-window :load-path "~/.env/elisp")

;; Ido Support
(use-package ido
  :demand
  :config
  (ido-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)                     ; For dired use C-j to quit at that path
  (setq ido-enable-regexp t)
  (setq ido-create-new-buffer 'always)

  (defalias 'ido-magic-forward-char 'ido-next-match)
  (defalias 'ido-magic-backward-char 'ido-prev-match)

  :bind
  ("C-x C-f" . ido-find-file)
  ("C-x f" . ido-find-file)
  ("C-x F" . ido-find-file-other-window)
  ("C-x B" . ido-switch-buffer-other-window)
  ("C-x b" . ido-switch-buffer))

;; Global Mode Stuff
(global-linum-mode 1) ; enable line numbers

(use-package yahoo-weather
  :defer t
  :init (setq yahoo-weather-location "Salford Quays")
  :config
  (defvar yahoo-run-id nil)
  (defun yahoo-weather-async-update-info ()
    (interactive)
    (async-start `(lambda ()
                    (require 'yahoo-weather (concat ,package-user-dir "/yahoo-weather-20160111.439/yahoo-weather.el"))
                    (yahoo-weather-update-info))
                 '(lambda (&rest args) (message "Yahoo weather updated [%s]" (format-time-string "%H:%M")))))
  (setq yahoo-run-id (run-at-time "1 sec" 900 'yahoo-weather-async-update-info)))

(use-package mode-icons
  :if window-system
  :load-path "elisp/mode-icons")

(add-hook 'after-init-hook 'update-powerline)
(use-package powerline
  :if window-system
  :load-path "elisp"
  :config
  (advice-add 'load-theme :after 'update-powerline))

(use-package boop :load-path "elisp/boop"
  :defer t
  :init
  (add-hook 'boop-update-hook
            '(lambda () (let ((local-config))
                     (deboop-group 'projectable)
                     (when (and (bound-and-true-p projectable-project-hash)
                                (gethash "boop" projectable-project-hash)
                                (boundp 'projectable-id))
                       (maphash
                        (lambda (key value)
                          (setq local-config (append local-config
                                        (list (list (intern key)
                                                    :script (intern (gethash "script" value))
                                                    :group 'projectable
                                                    :args (gethash "args" value)
                                                    :onselect `(lambda () (interactive) (browse-url (gethash "onclick" ,value))))))))
                        (gethash "boop" projectable-project-hash)))
                     (setq boop-config-alist (append boop-config-alist local-config))
                     (boop--sync-result-and-config))))

  :commands (boop-start))

(use-package window-numbering
  :init
  (add-hook
   'window-numbering-mode-hook
   '(lambda ()
      (let ((map (make-sparse-keymap)))
        (mapc
         (lambda (n) (define-key map (kbd (format "s-%s" n)) `(,(intern (format "select-window-%s" n)))))
         (number-sequence 1 9))
        (setq window-numbering-keymap map))))
  :config
  (window-numbering-mode)
  (window-numbering-clear-mode-line))

(use-package request
  :defer t
  :init
  (use-package json)
  :config
  (defun set-frame-title-yo-momma ()
    (interactive)
    (request
     "http://api.yomomma.info/"
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (let ((f (format-for-frame-title (cdr (assoc 'joke data)))))
                   (setq frame-title-format f))))))
  :bind ("<s-f8>" . set-frame-title-yo-momma))
(add-hook 'after-init-hook 'set-frame-title-yo-momma)

;;------------------
;; My Load Files
;;------------------

(setq custom-file (concat base-path "init/custom.el"))
(add-to-list 'custom-theme-load-path (concat base-path "/packages/themes"))

(use-package keys :load-path "init")
(load-file (concat base-path "init/custom.elc"))
(load-file (concat base-path "init/advice.elc"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(when window-system
  (load-theme 'aurora)
  (server-start)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(benchmark-init/show-durations-tree)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:

(provide 'init)
;;; init.el ends here
