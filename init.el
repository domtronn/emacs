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
    (local-set-key "j" 'jenkins)
    (local-set-key "a" 'org-agenda)
    (local-set-key "di" 'docker-images)
    (local-set-key "dc" 'docker-containers)
    (local-set-key "p" 'projectile-switch-project)))

(eval-when-compile (require 'use-package))
(use-package bind-key)

(use-package functions :load-path "init")

;; !!! - For package updating comment this region

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
  :bind ("H-n" . mc/mark-next-like-this)
        ("s-n" . mc/skip-to-next-like-this)
        ("H-p" . mc/mark-previous-like-this)
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
           '(minibuffer-inactive-mode snippet-mode lisp-mode emacs-lisp-mode slack-mode text-mode)
           "'" nil :actions nil)
          (sp-with-modes sp-lisp-modes (sp-local-pair "(" nil :bind "s-("))
  :bind ("C-)" . sp-slurp-hybrid-sexp)
        ("s-f" . sp-slurp-hybrid-sexp)
        ("s-b" . sp-forward-barf-sexp))

(use-package operate-on-number
  :bind ("s-@" . operate-on-number-at-point))

(use-package command-log-mode
  :defer t
  :config (global-command-log-mode)
  :bind ("<f8>" . clm/toggle-command-log-buffer))

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

(use-package org
  :defer t
  :mode ("\\.org"  . org-mode)
  :bind ("C-c c"   . org-capture)
        ("C-c a"   . org-agenda)
        ("C-c l"   . org-store-link)
        ("C-c e"   . org-export-dispatch)
        ("C-c C-l" . org-link)
        ("C-j"     . join-line)
  :init
  (setq org-src-fontify-natively t
        org-agenda-files
        '("~/org/meetings.org" "~/org/tasks.org" "~/org/birthdays.org" "~/org/reminders.org")
        org-ellipsis "⤵"
        org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "DONE" "WAITING"))
        org-todo-keyword-faces
        '(("IN PROGRESS" . (:background "#f1c40f" :foreground "#2c3e50" :weight bold))
          ("WAITING"     . (:background "#e74c3c" :foreground "#2c3e50" :weight bold)))
        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %?\n %t")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "** %^{Heading}  :LOG:\n%?")
          ("m" "Meeting" entry (file+headline "~/org/meetings.org" "MEETINGS")
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
            (alltodo "" ((org-agenda-sorting-strategy '(todo-state-up))))))))

  :config
  ;; Export Backends
  (use-package ox-twbs)
  (use-package ox-reveal)
  (use-package ox-md)
  (use-package org-wc)
  (bind-keys :map org-mode-map
             ("C-c C-x C-l"   . org-toggle-link-display)
             ("M-=" . org-wc-display)
             ("C-j" . join-line)
             ("s-f" . org-next-link)
             ("s-o" . org-open-at-point)
             ("s-b" . org-previous-link))
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'abbrev-mode)
  (run-with-idle-timer 300 t 'wlf:agenda)
  (add-hook 'org-finalize-agenda-hook
            '(lambda () (org-color-tag "Birthdays:" "#27ae60")
                   (org-color-tag "Holidays:" "#3498db")
                   (org-color-tag "Reminders:" "#8e44ad"))))

(use-package calfw :after org)
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

(use-package doc-view
  :mode ("\\.pdf" . doc-view-mode)
  :init (add-hook 'doc-view-mode-hook 'darkroom-mode))

(use-package undo-tree
  :config (global-undo-tree-mode)
  :bind ("s-z" . undo-tree-undo)
        ("s-Z" . undo-tree-redo)
        ("s-y" . undo-tree-redo)
        ("C-+" . undo-tree-redo))

(use-package etags-select :after (lisp-mode)
  :bind ("H-." . etags-select-find-tag-at-point)
        ("H->" . etags-select-find-tag))

(use-package git-gutter-fringe
  :if window-system
  :config (global-git-gutter-mode))

(use-package image+ :after 'image-mode)
(use-package dired+
  :after 'dired
  :config
  (setq insert-directory-program "gls")
  (add-hook 'dired-mode-hook '(lambda () (dired-hide-details-mode 0)))
  (add-hook 'dired-mode-hook '(lambda () (local-set-key (kbd "C-p") 'previous-line)))
  (bind-keys :map dired-mode-map
             ("<right>" . dired-find-file)
             ("<left>" . dired-up-directory)
             ("q" . kill-all-dired-buffers)))

(use-package dired-filter
  :after 'dired
  :init (setq dired-filter-group-saved-groups
              '(("default"
                 ("JavaScript" (extension "js" "json"))
                 ("MarkUp" (extension "md" "org"))
                 ("Archives" (extension "zip" "rar" "gz" "bz2" "tar"))
                 ("Images" (extension "png" "gif" "jpg")))))
  :config (bind-keys :map dired-mode-map
                     ("//" . dired-filter-group-mode)))

(use-package dired-narrow
  :after 'dired
  :config (bind-keys :map dired-mode-map
                     ("/f" . dired-narrow)
                     ("/t" . dired-narrow-fuzzy)))

(use-package dired-subtree
  :after 'dired
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-cycle)
             ("x" . dired-subtree-remove)))

(use-package dired-quick-sort
  :after 'dired
  :config
  (dired-quick-sort-setup))

(use-package ranger
  :defer t
  :bind ("<s-f3>" . ranger))

(use-package git-timemachine :bind ("C-x v t" . git-timemachine))
(use-package git-messenger :bind ("C-x v p" . git-messenger:popup-message))

(use-package windmove
  :bind ("<M-s-right>" . windmove-right)
        ("<M-s-left>" . windmove-left)
        ("<M-s-up>" . windmove-up)
        ("<M-s-down>" . windmove-down))

(use-package zoom-window
  :config (setq zoom-window-mode-line-color "#d35400")
  :bind ("C-x C-z" . zoom-window-zoom))

(use-package tabulated-list
  :defer t
  :config
  (bind-keys :map tabulated-list-mode-map
             ("<S-tab>" . tablist-backward-column)))

(use-package dockerfile-mode
  :mode ("^Dockerfile$" . dockerfile-mode)
  :config (bind-keys :map dockerfile-mode-map
                     ("C-x c" . dockerfile-build-buffer)
                     ("C-x C-c" . dockerfile-build-no-cache-buffer)))
(use-package docker-tramp
  :after docker
  :config
  (defun docker-tramp-onto-entry ()
    "Tramp onto a docker instance"
    (interactive)
    (let ((tramp-string (format "/docker:root@%s:/" (tabulated-list-get-id))))
      (find-file tramp-string))))

(use-package docker
  :commands
  (docker-images docker-containers docker-volumes docker-networks docker-machines)
  :init
  (require 'docker-auth (concat base-path ".docker-auth.el"))
  (setenv "DOCKER_TLS_VERIFY" (plist-get docker-auth :tlsverify))
  (setenv "DOCKER_HOST" (plist-get docker-auth :host))
  (setenv "DOCKER_CERT_PATH" (plist-get docker-auth :certpath))
  (setenv "DOCKER_MACHINE_NAME" (plist-get docker-auth :machinename))
  (add-hook 'docker-containers-mode-hook
            (lambda () (bind-keys :map docker-containers-mode-map
                             ("T" . docker-tramp-onto-entry)
                             ("l" . docker-containers-logs-follow-selection)))))

(use-package jenkins
  :commands (jenkins)
  :init
  (require 'jenkins-auth (concat base-path ".jenkins-auth.el"))
  (setq jenkins-username (plist-get jenkins-auth :user))
  (setq jenkins-api-token (plist-get jenkins-auth :apitoken))
  (setq jenkins-url (plist-get jenkins-auth :url))
  :config
  (add-hook 'jenkins-mode-hook 'tablist-minor-mode)
  (add-hook 'jenkins-mode-hook '(lambda () (setq-local mode-line-format nil))))

(use-package nameless
  :defer t
  :config (bind-keys :map nameless-mode-map ("C-c c" . nameless-insert-name)))

(use-package hydra-window-layout
  :load-path "elisp"
  :commands (wlf:agenda wlf:startup)
  :init (use-package window-layout)
  :bind ("C-c w" . hydra-window-layout/body))

(use-package flycheck-status-emoji
  :load-path "eslip"
  :after flycheck)

(use-package flycheck
  :config (global-flycheck-mode)
  (setq flycheck-javascript-standard-executable "standard")
  (setq flycheck-javascript-eslint-executable "eslint")
  (bind-keys :map flycheck-mode-map
             ("C-c C-e" . flycheck-list-errors)
             ("C-c C-n" . flycheck-next-error)
             ("C-c C-p" . flycheck-previous-error))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :bind ("M-}" . flycheck-mode))

(use-package eslint-reader
  :load-path "elisp/eslint-reader"
  :after js2-mode)

(use-package flyspell
  :init (setq flyspell-mode-map (make-sparse-keymap))
  (defun flyspell-toggle ()
    (interactive)
    (if flyspell-mode (flyspell-mode-off) (flyspell-mode)))
  (use-package flyspell-popup :defer t)
  :config
  (bind-keys :map flyspell-mode-map
             ("s-]" . flyspell-goto-next-error)
             ("s-." . flyspell-goto-next-error)
             ("s-," . flyspell-popup-correct))
  (advice-add 'flyspell-mode-on :before 'flyspell-buffer)
  (setq ispell-dictionary "english")
  :bind ("M-{" . flyspell-toggle))

(use-package projectile
  :commands (projectile-switch-project)
  :bind ("C-c p p" . projectile-switch-project)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  :bind
  ("C-o" . projectile-find-file)
  ("C-c f" . projectile-ag)
  ("C-x C-b" . projectile-switch-to-buffer)
  ("C-c s-p" . projectile-ibuffer))

(use-package visual-regexp
  :bind ("C-c r" . vr/replace)
        ("C-c q" . vr/query-replace)
        ("C-c m" . vr/mc-mark)
        ("s-r" . vr/query-replace))

(use-package hydra-smart-copy
  :bind ("M-W" . hydra-smart-copy/body)
  :load-path "elisp/hydra")

(use-package cycle-quotes
  :bind ("H-C" . cycle-quotes))

(use-package embrace
  :config (use-package hydra-embrace :load-path "elisp/hydra")
  :bind
  ("H-SPC"   . hydra-embrace-or-native)
  ("H-S-SPC" . embrace-delete)
  ("H-x"     . embrace-delete)
  ("H-c"     . embrace-change))

(use-package counsel :after ivy
  :defer 5
  :config
  (defalias 'counsel-use-package 'counsel-load-library)
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
        ("<M-f2>"  . counsel-ag)
        ("C-c v"   . counsel-git-grep)
        ("<s-f1>"  . counsel-imenu)
        ("H-M-."   . counsel-imenu)
        ("<H-tab>" . counsel-git)
        ("M-o"     . counsel-git)
        ("M-x"     . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-h b"   . counsel-descbinds)
        ("C-h v"   . counsel-describe-variable)
        ("C-h f"   . counsel-describe-function)
        ("s-V"     . counsel-yank-pop)
        ("M-y"     . counsel-yank-pop))

(use-package ivy :after avy
  :config
  (ivy-mode)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-display-style nil)
  (ivy-set-actions
   t
   '(("y" kill-new "yank")
     ("p" other-window-everything "other window")))
  (bind-keys :map ivy-minibuffer-map
             ("C-d" . ivy-backward-delete-char)
             ("C-S-j" . ivy-immediate-done))
  :bind ("C-c C-r" . ivy-resume)
        ("C-;"     . swiper))

(use-package isearch
  :bind ("H-s" . isearch-forward-symbol-at-point)
        ("C-s" . isearch-forward-regexp)
        ("C-r" . isearch-backward-regexp)
  :commands swiper-from-isearch
  :init (bind-keys :map isearch-mode-map
                   ("C-;" . swiper-from-isearch)
                   ("C-'" . avy-isearch)
                   ("C-l" . counsel-git-grep-from-isearch)))

(global-prettify-symbols-mode)
(push '("->" . ?→) prettify-symbols-alist)
(push '("<-" . ?←) prettify-symbols-alist)
(push '("<=" . ?≤) prettify-symbols-alist)
(push '(">=" . ?≥) prettify-symbols-alist)

(use-package avy
  :bind
  ("H-\\" . avy-goto-line)
  ("H-\"" . avy-goto-char)
  ("H-'" . avy-goto-word-1)
  ("H-;" . avy-goto-word-0)
  :config
  (avy-setup-default)
  (bind-keys ("M-A" . (lambda () (interactive) (call-interactively 'avy-goto-word-1) (forward-word)))))

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
(use-package js-injector
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
            '(lambda () (flycheck-select-checker (flycheck--guess-checker))))
  (add-hook 'js2-mode-hook
            '(lambda ()
               (push '("function" . ?ƒ) prettify-symbols-alist)
               (push '("var" . ?ν) prettify-symbols-alist)
               (push '("const" . ?ς) prettify-symbols-alist)
               (push '("let" . ?γ) prettify-symbols-alist)
               (push '("=>" . ?→) prettify-symbols-alist)
               (push '("R" . ?Λ) prettify-symbols-alist)
               (push '("R.__" . ?ρ) prettify-symbols-alist)
               (push '("_" . ?λ) prettify-symbols-alist)
               (push '("err" . ?ε) prettify-symbols-alist)
               (push '("return" . ?⇐) prettify-symbols-alist)
               (push '("undefined" . ?∅) prettify-symbols-alist)
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
             ("C-c x" . send-to-repl)

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
             ("C-k" . js2r-kill)
             ("<C-backspace>" . (lambda () (interactive) (smart-backward) (js2r-kill)))))

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

(use-package comint-mode
  :init
  (add-hook
   'comint-mode-hook
   (lambda ()
     (local-set-key (kbd "C-r") 'comint-history-isearch-backward)
     (local-set-key (kbd "<up>") 'comint-previous-input)
     (local-set-key (kbd "<down>") 'comint-next-input))))

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
          (lambda () (when (buffer-file-name)
                  (set (make-local-variable 'compile-command)
                       (let ((file (file-name-sans-extension buffer-file-name)))
                         (format "sass '%s':%s.css" buffer-file-name file))))))

(add-hook 'js2-mode-hook
          (lambda () (set (make-local-variable 'compile-command)
                     (format "esformatter -i %s" buffer-file-name))))

(use-package json
  :mode ("\\.json" . json-mode)
  :config (add-hook 'json-mode-hook '(lambda () (setq-local js-indent-level 2))))

(use-package engine-mode
  :config (engine-mode t)
  (setq engine/browser-function browse-url-browser-function)
  (defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "G")
  (defengine google "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s" :keybinding "g")
  (defengine thesaurus "http://www.thesaurus.com/browse/%s?s=t" :keybinding "t")
  (defengine devdocs-ramda "http://devdocs.io/ramda/index#%s" :keybinding "R")
  (defengine devdocs "http://devdocs.io/#q=%s" :keybinding "d")
  (defengine eslint "http://eslint.org/docs/rules/%s"))

(use-package eww
  :defer t
  :bind ("<f10>" . eww)
        ("<s-f10>" . eww-list-bookmarks)
  :config (bind-keys :map eww-mode-map
                     ("j" . json-format)
                     ("c" . eww-copy-page-url)))

(use-package goto-addr :after markdown-mode)
(use-package markdown-mode
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

(use-package browse-url
  :defer t
  :init (autoload 'browse-url-url-at-point "browse-url"))

(use-package link-hint
  :bind ("H-o" . link-hint-open-link)
        ("H-O" . link-hint-open-multiple-links))

(use-package markdown-toc
  :after markdown-mode
  :config (bind-keys :map markdown-mode-map
                     ("C-c C-t g" . markdown-toc-generate-toc)))

(use-package livedown
  :after markdown-mode
  :load-path "elisp/emacs-livedown"
  :config (bind-keys :map markdown-mode-map
                     ("C-c C-c p" . livedown:preview)))

(use-package coffee-mode :mode ("\\.coffee" . coffee-mode))
(use-package scss-mode :mode ("\\.scss$" . scss-mode))
(use-package css-mode :mode ("\\.css$" . css-mode))

(use-package lisp-mode
  :mode ("\\.el" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            '(lambda () (ac-lambda 'ac-source-functions
                              'ac-source-symbols
                              'ac-source-variables
                              'ac-source-yasnippet
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

(add-hook 'after-init-hook 'yas-global-mode)
(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode)
  :config   (setq yas-snippet-dirs (concat base-path "/snippets")))

(use-package neotree
  :load-path "elisp/neotree"
  :config
  (setq neo-theme 'file-icons)
  (setq neo-show-updir-line nil)
  (add-hook 'neotree-mode-hook (lambda () (setq-local line-spacing 5)))
  (add-hook 'neotree-mode-hook (lambda () (setq-local mode-line-format nil)))
  (add-hook 'neotree-mode-hook (lambda () (setq-local tab-width 1)))
  (add-hook 'magit-mode-hook 'neotree-hide)
  (defun neotree-projectile ()
    (interactive )
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (neotree-find (or (ignore-errors (projectile-project-root))
                        (and (buffer-file-name) (file-name-nondirectory (buffer-file-name)))
                        (getenv "HOME")))))
  :bind ([f1] . neotree-projectile))

(use-package eshell
  :defer t
  :config
  (defun eshell/ansi () (ansi-term "/bin/bash"))
  (defun eshell/e (f) (find-file f))
  (defun eshell/ee (f) (find-file-other-window f)))

(use-package esh-mode
  :defer t
  :init (with-eval-after-load "esh-opt"
          (autoload 'epe-theme-dakrone "eshell-prompt-extras")
          (setq eshell-highlight-prompt nil
                eshell-prompt-function 'epe-theme-dakrone))
  :config (bind-keys :map eshell-mode-map
             ("C-r" . counsel-esh-history)))

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

(use-package pug-mode :mode ("\\.pug$" . pug-mode))

(use-package web-mode
  :mode
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
             ("<backtab>" . web-mode-complete)
             ("s-/" . web-mode-comment-or-uncomment)
             ("M-P" . key-combo-mode)
             ("C-o" . emmet-expand-yas))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-ac-sources-alist
        '(("html" . (ac-source-html-tag
                     ac-source-words-in-same-mode-buffers
                     ac-source-html-attr))
          ("css" . (ac-source-css-selector
                    ac-source-css-id
                    ac-source-css-property))
          ("jsx" . (ac-source-yasnippet
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
  (bind-keys :map scss-mode-map
             ("M-P" . key-combo-mode)
             ("C-o" . emmet-expand-yas))
  ;; (advice-add 'emmet-expand-yas :before
  ;;             '(lambda () "Bemify the expression before expanding snippet when using the `|bem` filter"
  ;;                (bemify-emmet-string (emmet-expr-on-line))))
  )

;; Custom Auto Complete Sources
(use-package auto-complete-config :after auto-complete)

(use-package ac-emmet :after emmet-mode)
(use-package ac-html
  :after web-mode
  :config
  (add-hook 'web-mode-hook 'ac/setup-html)
  (defun ac/setup-html ()
    (require 'ac-html-default-data-provider)
    (ac-html-enable-data-provider 'ac-html-default-data-provider)
    (setq ac-sources '(ac-source-html-tag
                       ac-source-html-attr
                       ac-source-html-attrv))
    (auto-complete-mode)))

(use-package company :after auto-complete)

(use-package auto-complete
  :config
  (ac-config-default)
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
                     ("C-c e" . magit-vc-ediff)
                     ("C-<tab>" . projectile-find-file)))


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

(use-package powerline
  :if window-system
  :load-path "elisp"
  :init   (add-hook 'after-init-hook 'update-powerline)
  :config (advice-add 'load-theme :after 'update-powerline))

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
  :init (use-package json)
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

;; !!! - Comment up to this location for updating

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

;; Global Mode Stuff
(global-linum-mode 1) ; enable line numbers

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
  (remove-mode-line-box)
  (server-start))

(unless window-system
  (load-theme 'spacemacs-dark))

(benchmark-init/show-durations-tree)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:

(provide 'init)
;;; init.el ends here
