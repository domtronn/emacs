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

;; When setting up from scratch, there are a couple of external
;; packages that you'll require, here is a list of things to install

;; npm install -g livedown
;; npm install -g n_
;; npm install -g ramda-repl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when load-file-name
  (defconst base-path (file-name-directory load-file-name)))

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
    (local-set-key "t" 'counsel-load-theme)
    (local-set-key "f" 'counsel-set-font)
    (local-set-key "a" 'org-agenda)
    (local-set-key "p" 'projectile-switch-project)))

(eval-when-compile (require 'use-package))
(use-package bind-key :ensure t)

(use-package functions :load-path "init")

;; !!! - For package updating comment this region

(use-package linum-off :ensure t)

(use-package mon-css-color
  :load-path "elisp"
  :init (autoload 'css-color-mode "mon-css-color" "" t)
  :config (css-color-global-mode))

(use-package rainbow-delimiters :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paren :ensure t)

(use-package multiple-cursors :ensure t
  :bind ("H-n" . mc/mark-next-like-this)
        ("s-n" . mc/skip-to-next-like-this)
        ("H-p" . mc/mark-previous-like-this)
        ("s-p" . mc/skip-to-previous-like-this)
        ("H-l" . mc/mark-all-symbols-like-this)
        ("M-<mouse-1>" . mc/add-cursor-on-click))

(use-package multi-line :ensure t
  :bind ("C-c [" . multi-line-single-line)
        ("C-c ]" . multi-line))

(use-package drag-stuff :ensure t
  :bind ("s-S-<up>" . drag-stuff-up)
        ("s-P" . drag-stuff-up)
        ("s-S-<down>" . drag-stuff-down)
        ("s-N" . drag-stuff-down))

(use-package expand-region :ensure t
  :bind ("M-q" . er/expand-region))

(use-package ibuf-ext :after ibuffer
  :config (add-to-list 'ibuffer-never-show-predicates "^\\*"))
(use-package ibuffer-vc :ensure t :after ibuffer
  :config
  (bind-keys :map ibuffer-mode-map
             ("G" . ibuffer-vc-set-filter-groups-by-vc-root)))
(use-package ibuffer :ensure t
  :defer t
  :bind ("s-p" . ibuffer)
  :config
  (bind-keys :map ibuffer-mode-map
             ("M-u" . ibuffer-unmark-all)))

(use-package smart-forward :ensure t
  :bind ("s-." . forward-sexp)
        ("s-," . backward-sexp)
        ("C-." . smart-forward)
        ("C-," . smart-backward))

(use-package smart-newline :ensure t
  :bind ("RET" . smart-newline))

(use-package smartparens-config :after smartparens)
(use-package smartparens :ensure t
  :demand
  :config
  (smartparens-global-mode)
  (sp-local-pair
   '(minibuffer-inactive-mode snippet-mode lisp-mode emacs-lisp-mode slack-mode text-mode)
           "'" nil :actions nil)
  (sp-with-modes sp-lisp-modes (sp-local-pair "(" nil :bind "s-("))
  :bind ("C-)" . sp-slurp-hybrid-sexp)
        ("<s-backspace>" . sp-splice-sexp)
        ("s-f" . sp-slurp-hybrid-sexp)
        ("s-b" . sp-forward-barf-sexp))

(use-package operate-on-number :ensure t
  :bind ("s-@" . operate-on-number-at-point))

(use-package popup :ensure t :defer t)
(use-package popwin :ensure t
  :demand
  :config (popwin-mode 1)
          (setq popwin:close-popup-window-timer-interval 0.1)
          (setq popwin:close-popup-window-timer nil)
          (defun popwin:flycheck-errors ()
            (interactive)
            (when (get-buffer "*Flycheck errors*") (popwin:popup-buffer "*Flycheck errors*")))
          (defun popwin:compilation ()
            (interactive)
            (when (get-buffer "*compilation*")
              (if (get-buffer-window "*compilation*")
                  (delete-window (get-buffer-window "*compilation*"))
                (popwin:popup-buffer "*compilation*" :noselect t :stick t :tail t))))
  :bind ("C-x m" . popwin:messages)
  ("C-x e" . popwin:flycheck-errors)
  ("C-x c" . popwin:compilation))

(use-package org :ensure t
  :defer t
  :mode ("\\.org"  . org-mode)
  :bind ("C-c c"   . org-capture)
        ("C-c a"   . org-agenda)
        ("C-c l"   . org-store-link)
        ("C-c e"   . org-export-dispatch)
        ("C-c C-l" . org-link)
        ("C-j"     . join-line)
  :init
  (setq diary-file "~/Dropbox/Documents/Org/diary"
        org-src-fontify-natively t
        org-agenda-files
        '("~/Dropbox/Documents/Org/meetings.org"
          "~/Dropbox/Documents/Org/tasks.org"
          "~/Dropbox/Documents/Org/birthdays.org")
        org-ellipsis "⤵"
        org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "DONE" "WAITING"))
        org-todo-keyword-faces
        '(("IN PROGRESS" . (:background "#f1c40f" :foreground "#2c3e50" :weight bold))
          ("DONE"        . (:foreground "#0fbf5c" :weight bold
                                        :box (:line-width 1 :color "#3dc748") ))
          ("WAITING"     . (:background "#e74c3c" :foreground "#2c3e50" :weight bold)))
        org-capture-templates
        `(("t" "Todo" entry (file+headline "~/Dropbox/Documents/Org/tasks.org" "Tasks")
           "* TODO %?\n %t")
          ("j" "Journal" entry (file+datetree "~/Dropbox/Documents/Org/journal.org")
           "** %^{Heading}  :LOG:\n%?")
          ("m" "Meeting" entry (file+headline "~/Dropbox/Documents/Org/meetings.org" "MEETINGS")
           "* %^{Title} %^g\n  SCHEDULED: %(cfw:capture-schedule-day)\n  %?\n"))
        org-agenda-todo-ignore-scheduled 'past
        org-scheduled-past-days 0
        org-deadline-warning-days 7
        org-agenda-tags-todo-honor-ignore-options t
        org-todo-keywords-for-agenda t
        org-agenda-skip-deadline-prewarning-if-schedule t
        org-agenda-skip-scheduled-if-done t
        org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((agenda "" ((org-agenda-sorting-strategy '(time-up deadline-up))))
            (alltodo "" ((org-agenda-sorting-strategy '(todo-state-up tag-down priority-down effort-down))))
            (todo "" ((org-agenda-files '("~/Dropbox/Documents/org/tickets.org"))
                      (org-agenda-sorting-strategy '(todo-state-up))))))))

  :config
  ;; Export Backends
  (use-package ox-twbs :ensure t)
  (use-package ox-reveal :ensure t)
  (use-package org-wc :ensure t)
  (bind-keys :map org-mode-map
             ("C-c C-x l"   . org-toggle-link-display)
             ("M-=" . org-wc-display)
             ("C-;" . org-search-view)
             ("C-j" . join-line)
             ("s-f" . org-next-link)
             ("s-o" . org-open-at-point)
             ("s-b" . org-previous-link))
  (bind-keys :map org-agenda-mode-map
             ("I" . org-start-ticket))
  (org-babel-do-load-languages
   'org-babel-load-languages '((sh . t) (ruby . t) (dot . t) (perl . t)))
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'abbrev-mode)
  (add-hook 'org-mode-hook '(lambda () (flycheck-mode 0)))
  (run-with-idle-timer 300 t 'wlf:agenda)
  (add-hook 'org-finalize-agenda-hook
            '(lambda () (org-color-tag "Birthdays:" "#27ae60")
                   (org-color-tag "Holidays:" "#3498db")
                   (org-color-tag "Reminders:" "#8e44ad"))))

(use-package calfw :ensure t :after org)
(use-package calfw-org
  :after calfw
  :bind ("C-c s" . cfw:open-org-calendar)
  :config
  (setq cfw:org-agenda-schedule-args '(:deadline :scheduled :sexp))
  (setq cfw:org-overwrite-default-keybinding t)
  (setq cfw:org-capture-template
        '("m" "calfw2org" entry (file nil)  "* %?\n %(cfw:org-capture-day)"))
  (setq calendar-week-start-day 1)
  (bind-keys :map cfw:calendar-mode-map
             ("g" . cfw:refresh-calendar-buffer)
             ("RET" . cfw:change-view-day)))

(use-package doc-view :ensure t
  :mode ("\\.pdf" . doc-view-mode))

(use-package undo-tree :ensure t
  :config (global-undo-tree-mode)
          (setq undo-tree-visualizer-diff t)
  :bind ("C-c C-u" . undo-tree-visualize)
        ("s-z" . undo-tree-undo)
        ("s-Z" . undo-tree-redo)
        ("s-y" . undo-tree-redo)
        ("C-+" . undo-tree-redo))

(use-package etags-select :ensure t :defer t
  :bind ("H-." . etags-select-find-tag-at-point)
        ("H->" . etags-select-find-tag))

(use-package github-browse-file :ensure t :commands (github-browse-file))
(use-package git-link :ensure t :commands (git-link git-link-homepage))
(use-package git-timemachine :ensure t :bind ("C-x v t" . git-timemachine))
(use-package git-gutter-fringe :ensure t
  :if window-system
  :config (global-git-gutter-mode)
  :bind ("C-x v p" . git-gutter:previous-hunk)
        ("C-x v n" . git-gutter:next-hunk))

(use-package image+ :ensure t :after 'image-mode)
(use-package dired+ :ensure t
  :after 'dired
  :config
  (setq insert-directory-program "gls")
  (add-hook 'dired-mode-hook '(lambda () (dired-hide-details-mode 0)))
  (add-hook 'dired-mode-hook '(lambda () (local-set-key (kbd "C-p") 'previous-line)))
  (defun dired-find-only-file ()
     (interactive)
     (when (file-directory-p (dired-get-file-for-visit))
       (dired-find-file)))
  (bind-keys :map dired-mode-map
             ("M-r" . wdired-change-to-wdired-mode)
             ("q" . kill-all-dired-buffers)))

(use-package dired-filter :ensure t
  :after 'dired
  :init (setq dired-filter-group-saved-groups
              '(("default"
                 ("JavaScript" (extension "js" "json"))
                 ("MarkUp" (extension "md" "org"))
                 ("Archives" (extension "zip" "rar" "gz" "bz2" "tar"))
                 ("Images" (extension "png" "gif" "jpg")))))
  :config (bind-keys :map dired-mode-map
                     ("//" . dired-filter-group-mode)
                     ("C-o" . project-find-file)))

(use-package dired-narrow :ensure t
  :after 'diredb
  :config (bind-keys :map dired-mode-map
                     ("/f" . dired-narrow)
                     ("/t" . dired-narrow-fuzzy)))

(use-package dired-quick-sort :ensure t
  :after 'dired
  :config
  (dired-quick-sort-setup))

(use-package windmove :ensure t
  :bind ("<M-s-right>" . windmove-right)
        ("<M-s-left>" . windmove-left)
        ("<M-s-up>" . windmove-up)
        ("<M-s-down>" . windmove-down))

(use-package zoom-window :ensure t
  :config (setq zoom-window-mode-line-color "#d35400")
  :bind ("C-x C-x" . zoom-window-zoom))

(use-package yaml-mode :ensure t :mode ("\\.yml$" . yaml-mode))
(use-package dockerfile-mode :ensure t
  :mode ("^Dockerfile$" . dockerfile-mode)
  :config (bind-keys :map dockerfile-mode-map
                     ("C-x c" . dockerfile-build-buffer)
                     ("C-x C-c" . dockerfile-build-no-cache-buffer)))

(use-package nameless :ensure t
  :defer t
  :config (bind-keys :map nameless-mode-map ("C-c C-c" . nameless-insert-name)))

(use-package hydra-window-layout
  :disabled t
  :load-path "elisp"
  :commands (wlf:agenda)
  :init (use-package window-layout :ensure t)
  :bind ("C-c w" . hydra-window-layout/body))

(use-package flycheck :ensure t
  :config (global-flycheck-mode)
  (setq flycheck-javascript-standard-executable "standard")
  (setq flycheck-javascript-eslint-executable "eslint")
  (setq flycheck-eslintrc ".eslintrc.json")
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (bind-keys :map flycheck-mode-map
             ("C-c C-e" . flycheck-list-errors)
             ("C-c C-n" . flycheck-next-error)
             ("C-c C-p" . flycheck-previous-error))
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-standard 'rjsx-mode)
  :bind ("M-}" . flycheck-mode))

(use-package eslint-reader
  :load-path "elisp/eslint-reader"
  :after js2-mode)

(use-package flyspell-popup :ensure t :defer t :after flyspell
  :config (bind-keys :map flyspell-mode-map ("±" . flyspell-popup-correct)))

(use-package flyspell :ensure t
  :init (defun flyspell-toggle ()
          (interactive)
          (if flyspell-mode (flyspell-mode-off) (flyspell-mode)))
  :config
  (setq ispell-dictionary "english")
  (dolist (hook '(text-mode-hook)) (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook)) (add-hook hook (lambda () (flyspell-mode -1))))
  (advice-add 'flyspell-mode-on :before 'flyspell-buffer)
  :bind ("M-{" . flyspell-toggle))

(use-package projectile :ensure t
  :commands (projectile-switch-project)
  :bind ("C-c p p" . projectile-switch-project)
  :config
  (recentf-mode)
  (projectile-mode)
  (require 'projectile-ignore (concat base-path ".projectile-ignore.el"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile" "gulpfile.js" "gruntfile.js" "Gulpfile.js" "Gruntfile.js" "package.json")
                projectile-project-root-files-bottom-up))
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                '("node_modules" "build" "tests" ".cache")))
  (setq projectile-globally-ignored-file-suffixes '(".min.js" ".tags" ".elc"))
  (setq projectile-tags-file-name ".tags")

  (add-hook 'projectile-after-switch-project-hook
            '(lambda () (setq tags-table-list `(,(concat (projectile-project-root) projectile-tags-file-name)))))

  (defun projectile-find-file-non-fuzzy ()
    (interactive)
    (let ((ivy-re-builders-alist '((t . ivy--regex)))) (projectile-find-file)))
  (defun projectile-find-file-basename ()
    (interactive)
    (projectile-completing-read
     "Find file: "
     (--map (cons (format "%s%s%s"
                          (file-name-nondirectory it)
                          (make-string (- 35 (min 30 (length (file-name-nondirectory it)))) ? )
                          (or (file-name-directory it) ".")) it)
            (projectile-current-project-files))
     :action `(lambda (file)
                (find-file (expand-file-name (cdr file) ,(projectile-project-root)))
                (run-hooks 'projectile-find-file-hook))))
  :bind
  ("M-o" . projectile-find-file-basename)
  ("C-o" . projectile-find-file)
  ("C-S-o" . projectile-find-file-non-fuzzy);
  ("C-c C-p" . projectile-ibuffer)
  ("C-c p o" . projectile-find-file-in-known-projects)
  ("C-c p a" . projectile-add-known-project)
  ("C-c p d" . projectile-find-dir)
  ("C-x C-b"   . projectile-switch-to-buffer)
  ("C-c p x x" . projectile-remove-known-project))

(use-package visual-regexp :ensure t
  :bind ("C-c r" . vr/replace)
        ("C-c q" . vr/query-replace)
        ("C-c m" . vr/mc-mark)
        ("s-r" . vr/query-replace)
  :config (setq vr/match-separator-string " → "))

(use-package cycle-quotes :ensure t
  :bind ("H-C" . cycle-quotes))

(use-package embrace :ensure t
  :config (use-package hydra-embrace :load-path "elisp/hydra")
  :bind
  ("H-SPC"   . hydra-embrace-or-native)
  ("H-S-SPC" . embrace-delete)
  ("H-c"     . embrace-change))

(use-package smex :ensure t :after counsel)
(use-package counsel :ensure t :after ivy
  :defer 5
  :config
  (setq counsel-find-file-at-point t)
  (defalias 'counsel-use-package 'counsel-load-library)
  (defun counsel-ag-project ()
      (interactive)
      (counsel-ag (thing-at-point 'symbol) (projectile-project-root)))
  (defun counsel-git-grep-from-isearch ()
    "Invoke `counsel-git-grep' from isearch."
    (interactive)
    (let ((input (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
        (isearch-exit)
        (counsel-git-grep nil input)))
    (global-set-key (kbd "<f9>")
                    (lambda () (interactive)
                      (call-interactively 'counsel-load-theme)
                      (call-interactively 'counsel-set-font)))

  :bind ([f2]      . counsel-git-grep)
        ("C-c f"   . counsel-ag-project)
        ("C-c v"   . counsel-git-grep)
        ("H-M-."   . counsel-imenu)
        ("M-x"     . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-h b"   . counsel-descbinds)
        ("C-h v"   . counsel-describe-variable)
        ("C-h f"   . counsel-describe-function)
        ("s-V"     . counsel-yank-pop)
        ("M-y"     . counsel-yank-pop))

(use-package ivy-hydra :ensure t :after ivy)
(use-package ivy-rich :ensure t :after ivy
  :config (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))
(use-package ivy :ensure t :after avy
  :config
  (ivy-mode)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-display-style 'plain)
  (advice-add 'counsel-git-grep
              :around '(lambda (f &rest args) (let ((ivy-re-builders-alist '((t . ivy--regex-plus)))) (apply f args))))
  (bind-keys :map ivy-minibuffer-map
             ("C-d" . ivy-backward-delete-char)
             ("C-S-j" . ivy-immediate-done))
  :bind ("C-c C-r" . ivy-resume)
        ("C-;"     . swiper)
        ("C-x b"     . ivy-switch-buffer))

(use-package isearch
  :bind ("H-s" . isearch-forward-symbol-at-point)
        ("C-s" . isearch-forward-regexp)
        ("C-r" . isearch-backward-regexp)
  :commands swiper-from-isearch
  :init (bind-keys :map isearch-mode-map
                   ("C-;" . swiper-from-isearch)
                   ("C-'" . avy-isearch)
                   ("C-l" . counsel-git-grep-from-isearch)))

(use-package avy-zap :ensure t
  :bind
  ("H-x" . avy-zap-to-char))
(use-package avy :ensure t
  :bind
  ("H-\\" . avy-goto-line)
  ("H-'" . avy-goto-word-1)
  ("H-\"" . avy-goto-char)
  :config
  (avy-setup-default)
  (bind-keys ("H-A" . (lambda () (interactive) (call-interactively 'avy-goto-word-1) (forward-word)))))

(use-package wgrep-ag :ensure t :after ag)
(use-package ag
  :ensure t
  :commands (ag-regexp ag-project-regexp)
  :bind ("C-c g" . ag-project-regexp))

(use-package rjsx-mode :ensure t
  :mode ("\\.jsx$" . rjsx-mode)
  :config
  (bind-keys :map rjsx-mode-map ("s-w" . js2-mode))
  (advice-add
   'key-combo-pre-command-function
   :around '(lambda (orig-f &rest args)
              (unless (and (member (js2-node-type (js2-node-at-point)) (list rjsx-JSX rjsx-JSX-ATTR rjsx-JSX-IDENT rjsx-JSX-MEMBER))
                           (member (this-command-keys) '("=" "-" "+")))
                (apply orig-f args)))))

(use-package js2-refactor :after js2-mode :ensure t)
(use-package js2r-extensions :after js2-mode :load-path "elisp")
(use-package js-injector
  :after js2-mode
  :load-path "elisp/js-dependency-injector"
  :config
  (setq js-injector-get-relative-func 'js-injector--get-projectile-files-alist))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(use-package js2-mode
  :mode "\\.js$"
  :config
  (setq js-switch-indent-offset 2)
  (setq js2-include-node-externs t)
  (setq js2-include-browser-externs t)
  (setq js2-basic-offset 2)
  (setq js2-enter-indents-newline t)
  (setq js2-highlight-level 3)
  (setq js2-pretty-multiline-declarations 'dynamic)

  (setq js2-jump-fallback-f '(lambda (thing &rest args) (counsel-ag thing (projectile-project-root))))

  (add-hook 'js2-mode-hook 'js-injector-minor-mode)
  (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
  (add-hook 'js2-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'js2-mode-hook '(lambda () (key-combo-common-load-default)))
  (add-hook 'js2-mode-hook
            '(lambda () (flycheck-select-checker (flycheck--guess-checker))))
  (bind-keys :map js2-mode-map
             ("C-c x" . send-to-repl)

             ;; JS2 Refactor things
             ("C-c m" . prettify-symbols-mode)
             ("s-P" . js2r-drag-stuff-up)
             ("s-N" . js2r-drag-stuff-down)
             ("C-c C-o" . js2r-order-vars-by-length)
             ("C-c C-s" . js2r-toggle-var-declaration)
             ("C-c C-v" . js2r-extract-var)
             ("C-c C-i" . js2r-inline-var)
             ("C-c C-f" . js2r-extract-function)
             ("C-c C-r" . js2r-rename-var)
             ("C-c ." . js2-jump-to-definition)
             ("C-k" . js2r-kill)
             ("s-w" . rjsx-mode)
             ("M-." . js2-jump-around)
             ("M-," . pop-tag-mark)
             ("<s-return>" . (lambda () (interactive) (dotimes (i 2) (smart-newline))))
             ("<s-S-return>" . (lambda () (interactive) (dotimes (i 4) (smart-newline))))
             ("<C-backspace>" . (lambda () (interactive) (smart-backward) (js2r-kill)))))

(use-package comint-mode
  :init
  (add-hook
   'comint-mode-hook
   (lambda ()
     (local-set-key (kbd "C-r") 'comint-history-isearch-backward)
     (local-set-key (kbd "<up>") 'comint-previous-input)
     (local-set-key (kbd "<down>") 'comint-next-input))))

(use-package cpp :ensure t
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

(defun js2-standard-fix ()
  (interactive)
  (when (buffer-file-name)
    (shell-command
     (format (if (flycheck-eslint-config-exists-p)
                 "eslint --fix %s" "standard --fix %s") (buffer-file-name)))))

;; (use-package scss-mode :ensure t :mode ("\\.scss" . sass-mode))
;; (add-hook 'scss-mode-hook
;;           (lambda () (when (buffer-file-name)
;;                   (set (make-local-variable 'compile-command)
;;                        (let ((file (file-name-sans-extension buffer-file-name)))
;;                          (format "sass '%s':%s.css" buffer-file-name file))))))

(use-package json :ensure json-mode
  :mode ("\\.json" . json-mode)
  :config
  (add-hook 'json-mode-hook
            '(lambda ()
               (setq-local js-indent-level 2)
               (local-set-key (kbd "<s-return>") '(lambda () (interactive) (dotimes (i 2) (smart-newline))))
               (local-set-key (kbd "<s-S-return>") '(lambda () (interactive) (dotimes (i 4) (smart-newline)))))))

(use-package goto-addr :ensure t :after markdown-mode)
(use-package browse-url :ensure t
  :defer t
  :init (autoload 'browse-url-url-at-point "browse-url"))

(use-package link-hint :ensure t
  :bind ("H-o" . link-hint-open-link)
        ("H-O" . link-hint-open-multiple-links))

(use-package markdown-toc :ensure t
  :after markdown-mode
  :config (bind-keys :map markdown-mode-map
                     ("C-c C-t g" . markdown-toc-generate-toc)))

(use-package markdown-mode :ensure t
  :mode ("\\.md" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (bind-keys* ("M-<left>" . backward-word)
              ("<M-S-left>" . backward-word)
              ("M-<right>" . forward-word)
              ("<M-S-right>" . forward-word))
  (bind-keys :map markdown-mode-map
             ("s-f" . next-link)
             ("s-b" . previous-link)))

(use-package livedown
  :after markdown-mode
  :load-path "elisp/emacs-livedown"
  :config (bind-keys :map markdown-mode-map
                     ("C-c C-c p" . livedown:preview)))

(use-package sudo-edit :ensure t
  :commands (sudo-edit-current-file))

(use-package scss-mode :ensure t
  :mode ("\\.scss$" . scss-mode)
  :config
  (bind-keys :map scss-mode-map
             ("<s-return>" . (lambda () (interactive) (dotimes (i 2) (smart-newline))))
             ("<s-S-return>" . (lambda () (interactive) (dotimes (i 4) (smart-newline))))))
(use-package css-mode :ensure t :mode ("\\.css$" . css-mode))

(use-package lisp-mode
  :mode ("\\.el" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            '(lambda () (ac-lambda 'ac-source-functions
                              'ac-source-variables
                              'ac-source-yasnippet
                              'ac-source-symbols
                              'ac-source-words-in-same-mode-buffers)))
  :config
  (bind-keys :map emacs-lisp-mode-map
             ("C-c n" . nameless-mode)
             ("C-c C-l" . elisp-debug)
             ("M-." . jump-to-find-function)))

(global-set-key (kbd "M-,") 'pop-tag-mark)

(use-package key-combo :ensure t
  :config (add-to-list 'key-combo-common-mode-hooks 'web-mode-hook)
          (key-combo-mode 1)
          (key-combo-load-default))

(add-hook 'after-init-hook 'yas-global-mode)
(use-package yasnippet :ensure t
  :commands (yas-global-mode yas-minor-mode)
  :config   (setq yas-snippet-dirs (concat base-path "/snippets")))

(use-package font-lock+ :ensure t :after all-the-icons)
(use-package all-the-icons
  :if window-system
  :load-path "elisp/all-the-icons")

(setq neo-theme (if window-system 'icons 'arrow))
(use-package neotree :ensure t
  :config
  (setq neo-show-updir-line nil
        neo-window-width 45)
  (add-hook 'neotree-mode-hook (lambda () (setq-local line-spacing 5)))
  (add-hook 'neotree-mode-hook (lambda () (setq-local tab-width 1)))

  (defun neotree-projectile ()
    (interactive )
    (let ((cw (get-buffer-window (current-buffer))))
     (if (neo-global--window-exists-p)
         (neotree-hide)
       (neotree-find (or (ignore-errors (projectile-project-root))
                         (and (buffer-file-name) (file-name-nondirectory (buffer-file-name)))
                         (getenv "HOME"))))
     (select-window cw)))

  (defun neotree-projectile-find ()
    (interactive)
    (let ((cw (get-buffer-window (current-buffer))))
      (neotree-find)
      (select-window cw)))

  :bind ([f1] . neotree-projectile)
        ("<S-f1>" . neotree-projectile-find)
        ("<M-f1>" . neotree-find))

(use-package compile :ensure t :defer t
  :config
  (add-hook 'compilation-mode-hook 'css-color-mode)
  (add-to-list 'compilation-error-regexp-alist '("at .*?\\(/.*?\\):\\(.*?\\):\\(.*?\\)$" 1 2 3))
  :bind ("C-x C-c" . compile))

(use-package ansi-color :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package shell-pop :ensure t
  :bind ("C-`" . shell-pop)
  :config
  (add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1)))
  (setq shell-pop-autocd-to-working-dir nil
        shell-pop-shell-type '("term" "*terminal*" (lambda () (ansi-term "/bin/bash" "*ansi-terminal*")))
        shell-pop-window-position "bottom"
        shell-pop-window-size 40))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package pug-mode :ensure t :mode ("\\.pug$" . pug-mode))
(use-package web-mode :ensure t
  :mode
  ("\\.html$" . web-mode)
  ("\\.scss$" . web-mode)

  :config
  (bind-keys :map web-mode-map
             ("M-;" . semi-colon-end)
             ("C-j" . join-line)
             ("s-=" . web-mode-fold-or-unfold)
             ("M-s M-m" . web-mode-tag-match)
             ("M-s M-e" . web-mode-tag-end)
             ("M-s M-a" . web-mode-tag-beginning)
             ("M-s M-n" . web-mode-tag-next)
             ("M-s M-p" . web-mode-tag-previous)
             ("M-a M-n" . web-mode-attribute-next)
             ("M-a M-p" . web-mode-attribute-previous)
             ("s-=" . hs-toggle-hiding)
             ("<M-S-return>" . web-mode-navigate)
             ("<backtab>" . web-mode-complete)
             ("<s-return>" . (lambda () (interactive) (dotimes (i 2) (smart-newline))))
             ("<s-S-return>" . (lambda () (interactive) (dotimes (i 4) (smart-newline))))
             ("s-/" . web-mode-comment-or-uncomment))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-auto-quote-style 1)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-ac-sources-alist
        '(("html" . (ac-source-html-tag
                     ac-source-words-in-same-mode-buffers
                     ac-source-html-attr))
          ("css" . (ac-source-css-selector
                    ac-source-css-id
                    ac-source-css-property)))))

;; Custom Auto Complete Sources
(use-package company :ensure t)
(use-package auto-complete-config :after auto-complete)
(use-package auto-complete :ensure t
  :config
  (ac-config-default)
  (setq ac-delay 0.2)
  (set-default 'ac-sources
               '(ac-source-yasnippet
                 ac-source-words-in-same-mode-buffers))
  (global-auto-complete-mode t)

  (bind-keys :map ac-completing-map ("\e" . ac-stop))
  (bind-keys :map ac-complete-mode-map
             ([tab] . ac-expand-common)
             ([return] . ac-complete)
             ("C-j" . ac-complete)
             ("C-s" . ac-isearch)
             ("C-n" . ac-next)
             ("C-p" . ac-previous))

  :bind
  ("<M-tab>" . auto-complete)
  ("§" . auto-complete))

(add-hook 'LaTeX-mode-hook '(lambda () (local-set-key (kbd "C-x c") 'xelatex-make)))
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'css-color-mode)

(use-package hideshowvis :ensure t
  :init (autoload 'hideshowvis-enable "hideshowvis" nil t)
  :config (hideshowvis-symbols)
  :bind ("s-_" . hs-show-all)
        ("s--" . hs-show-block)
        ("s-=" . hs-toggle-hiding)
        ("s-+" . hs-hide-level))
(add-hook 'prog-mode-hook 'hideshowvis-minor-mode)

(use-package grunt :ensure t :bind ("C-M-g" . grunt-exec))

(use-package magit :ensure t
  :defer t
  :config (bind-keys :map magit-mode-map
                     ("o" . magit-open-file-other-window)
                     ("C-c c" . magit-whitespace-cleanup)
                     ("C-c e" . magit-vc-ediff)))

(use-package atomic-chrome :ensure t :defer t
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-default-major-mode 'rjsx-mode))

(use-package yahoo-weather :ensure t
  :defer t
  :init (setq yahoo-weather-location "Salford Quays")
  :config
  (defvar yahoo-run-id nil)
  (defun yahoo-weather-async-update-info ()
    (interactive)
    (async-start `(lambda ()
                    (let* ((dir (car (directory-files ,package-user-dir t "yahoo-weather")))
                           (file (format "%s/yahoo-weather.el")))
                      (require 'yahoo-weather file)
                      (yahoo-weather-update-info)))
                 '(lambda (&rest args) (message "Yahoo weather updated [%s]" (format-time-string "%H:%M")))))
  (setq yahoo-run-id (run-at-time "1 sec" 900 'yahoo-weather-async-update-info)))

(use-package restart-emacs :ensure t :bind ("s-q" . restart-emacs))

(use-package fancy-battery :after spaceline :defer 10
  :config (fancy-battery-mode))

(use-package powerline
  :if window-system
  :config (setq-default powerline-default-separator 'nil))

(use-package spaceline-custom :after spaceline :load-path "init/spaceline-custom")
(use-package spaceline-colors :after spaceline-custom :load-path "init/spaceline-colors"
  :init (add-hook 'after-init-hook 'spaceline-update-faces)
  :config (advice-add 'load-theme :after 'spaceline-update-faces))

(use-package spaceline :after powerline :ensure t
  :config (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))

(use-package winum :disabled t :ensure t)
(use-package window-numbering :ensure t
  :init
  (add-hook
   'window-numbering-mode-hook
   '(lambda () (let ((map (make-sparse-keymap)))
            (dotimes (n 10)
              (define-key map (kbd (format "s-%s" n)) `(,(intern (format "select-window-%s" n)))))
            (setq window-numbering-keymap map))))
  :config
  (window-numbering-mode)
  (window-numbering-clear-mode-line))

;; change vc-diff to use vc-ediff
(setq ediff-split-window-function (quote split-window-horizontally))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)
(add-hook 'ediff-startup-hook 'ediff-swap-buffers)

;; !!! - Comment up to this location for updating

;; Startup variables
(setq shift-select-mode t)                  ; Allow for shift selection mode
(setq inhibit-splash-screen t)              ; disable splash screen
(setq make-backup-files nil)                ; don't make backup files
(setq create-lockfiles nil)                 ; don't make lock files
(setq auto-save-default nil)                ; don't autosave
;; Disable audible and visible bell in favor of flashing the mode line instead
(defun mode-line-visible-bell () "A friendlier vaisual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell nil)
(setq ring-bell-function 'mode-line-visible-bell)

;; Set Path
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path '("/usr/local/bin" "/usr/bin" "/bin"))

;; Set Mac modifiers keys
(setq mac-function-modifier 'hyper)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(delete-selection-mode 1)                   ; Allows for deletion when typing over highlighted text
(fset 'yes-or-no-p 'y-or-n-p)               ; Use y or n instead of yes or no

(blink-cursor-mode 0)
(setq-default cursor-type '(bar . 1))             ; Change cursor to bar
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

;; Get rid of stupid menu bar and Tool Bar..
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)
(show-paren-mode t)   ; Show paranthesis matching

;; Global Mode Stuff
(global-linum-mode 1) ; enable line numbers
(add-hook 'js2-mode-hook 'js2/load-prettify-symbols-alist)
(add-hook 'js2-mode-hook 'prettify-symbols-mode)
(use-package prettify-symbols-mode
  :bind ("C-c <C-return>" . prettify-symbols-mode)
  :config
  (global-prettify-symbols-mode)
  (setq prettify-symbols-unprettify-at-point t))

;;------------------
;; Themes
;;------------------
(use-package keys :load-path "init")
(load-file (concat base-path "init/custom.el"))
(load-file (concat base-path "init/advice.elc"))

;; Themed with Spaceline
(use-package gruvbox-theme :ensure t :defer t)
(use-package creamsody-theme :ensure t)
(use-package suscolors-theme :ensure t :defer t)
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package forest-blue-theme :ensure t :defer t)
(use-package liso-theme :ensure t :defer t)
(use-package peacock-theme :ensure t :defer t)

(use-package darkokai-theme :ensure t :defer t)
(use-package darktooth-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
(use-package niflheim-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package spacemacs-theme :ensure t :defer t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(load-theme 'creamsody t)
(when window-system
  (remove-mode-line-box)
  (server-start))

(benchmark-init/show-durations-tree)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:

(provide 'init)
;;; init.el ends here
