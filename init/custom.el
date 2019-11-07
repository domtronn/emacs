;;------------------
;; Custom Variables
;;------------------

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-candidate-limit 80)
 '(ac-etags-requires 1)
 '(ac-expand-on-auto-complete t)
 '(ac-quick-help-delay 0.5)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#1c1f24" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF" "#DFDFDF"])
 '(ansi-term-color-vector
   [unspecified "#2e2e2e" "#bc8383" "#7f9f7f" "#d0bf8f" "#6ca0a3" "#dc8cc3" "#8cd0d3" "#b6b6b6"] t)
 '(backup-by-copying-when-mismatch nil)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-setup (quote one-frame))
 '(calendar-view-diary-initially-flag t)
 '(compilation-message-face (quote (quote default)))
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".svn-base")))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-in-non-selected-windows nil)
 '(cursor-type (quote (bar . 1)))
 '(custom-safe-themes
   (quote
    ("886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "030346c2470ddfdaca479610c56a9c2aa3e93d5de3a9696f335fd46417d8d3e4" "0990a0b1f0b473858c1ae6b73b8d9c3b804cc1251430f54dc080d82cc1e26e24" default)))
 '(dabbrev-check-all-buffers nil)
 '(dabbrev-eliminate-newlines nil)
 '(default-input-method "TeX")
 '(emmet-quote-style "\"")
 '(eval-sexp-fu-flash-mode t)
 '(fci-rule-character-color "#6c7175")
 '(fci-rule-color "#6c7175" t)
 '(flycheck-error-list-format
   [("Line" 4 flycheck-error-list-entry-< :right-align t)
    ("Col" 3 nil :right-align t)
    ("Level" 8 flycheck-error-list-entry-level-<)
    ("ID" 20 t)
    ("Message (Checker)" 0 t)] t)
 '(flycheck-eslintrc ".eslintrc.json" t)
 '(flycheck-highlighting-mode (quote lines))
 '(flycheck-idle-change-delay 2)
 '(fringe-mode (quote (0)) nil (fringe))
 '(git-gutter-fr:side (quote left-fringe))
 '(global-prettify-symbols-mode t)
 '(global-rainbow-delimiters-mode t)
 '(gradle-executable-path "/usr/local/bin/gradle")
 '(helm-reuse-last-window-split-state t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-current-line-globally t)
 '(highlight-symbol-colors
   (quote
    ("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80")))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors
   (if
       (eq
        (quote dark)
        (quote light))
       (quote
        (("#FFA726" . 0)
         ("#FFEE58" . 10)
         ("#FFF59D" . 30)
         ("#494949" . 60)
         ("#424242" . 80)))
     (quote
      (("#F8BBD0" . 0)
       ("#FF80AB" . 10)
       ("#9575CD" . 30)
       ("#494949" . 60)
       ("#424242" . 80)))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-sexp-background-color "#efebe9")
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(ibuffer-deletion-face (quote diredp-deletion-file-name))
 '(ibuffer-filter-group-name-face (quote font-lock-builtin-face))
 '(ibuffer-fontification-alist
   (quote
    ((10 buffer-read-only font-lock-constant-face)
     (15
      (and buffer-file-name
           (string-match ibuffer-compressed-file-name-regexp buffer-file-name))
      font-lock-doc-face)
     (20
      (string-match "^*"
                    (buffer-name))
      font-lock-keyword-face)
     (25
      (and
       (string-match "^ "
                     (buffer-name))
       (null buffer-file-name))
      italic)
     (30
      (memq major-mode ibuffer-help-buffer-modes)
      font-lock-comment-face)
     (35
      (eq major-mode
          (quote dired-mode))
      font-lock-function-name-face))))
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 60 60 :left :elide)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(ibuffer-git-column-length 24)
 '(ibuffer-marked-face (quote diredp-flag-mark))
 '(ibuffer-show-empty-filter-groups nil)
 '(ido-buffer-disable-smart-matches nil)
 '(ido-enable-dot-prefix t)
 '(ido-file-extensions-order
   (quote
    (".json" ".css" ".pl" ".sh" ".as" ".php" ".emacs" ".ini" ".js" ".el" ".ini" ".cfg" ".cnf" "")))
 '(ido-separator nil)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(initial-scratch-message
   ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬
;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤
;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴
")
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(jpop-completion-func (quote car))
 '(jpop-filter-regexps
   (quote
    ("~$" "\\.o$" "\\.exe$" "\\.a$" "/\\.svn" "\\.elc$" "\\.output$" "\\.$" "#$" "\\.class$" "\\.png$" "\\.svn*" "/node_modules/" "\\.gif$" "\\.gem$" "\\.pdf$" "\\.swp$" "\\.iml$" "\\.jar$" "\\/build\\/" "/\\.git" "\\/jsdoc\\/" "\\.min\\.js$" "\\.tags$" "\\.filecache" "\\.cache$" "\\/.git\\/" "\\/report\\/" "\\.gcov\\.html$" "\\.func.*\\.html$" "\\/tmp\\/")))
 '(jpop-project-directory "/Users/charld13/Projects")
 '(js-indent-level 2)
 '(js-injector-get-relative-func (quote js-injector--get-projectile-files-alist))
 '(js-injector-get-requirejs-func
   (quote
    (:files js-injector--get-projectile-relative-requirejs-alist :config js-injector--get-projectile-relative-requirejs-config)))
 '(js-injector-node-camelise (quote s-lower-camel-case))
 '(key-combo-common-default
   (quote
    (("," . ", ")
     ("=" " = " " == " " === ")
     ("=>" . " => ")
     ("=~" . " =~ ")
     ("=*" . " =* ")
     ("+" " + " "++")
     ("+=" . " += ")
     ("-" " - " "--")
     ("-=" . " -= ")
     ("->" . " -> ")
     (">" key-combo-execute-original " >> ")
     (">=" . " >= ")
     (">>=" . " >>= ")
     ("%" . " % ")
     ("%=" . " %= ")
     ("^" . " ^ ")
     ("^=" . " ^= ")
     ("!" . key-combo-execute-original)
     ("!=" . " != ")
     ("!==" . " !== ")
     ("!~" . " !~ ")
     ("~" . key-combo-execute-original)
     ("::" . " :: ")
     ("&" " & " " && ")
     ("&=" . " &= ")
     ("&&=" . " &&= ")
     ("*" . " * ")
     ("*=" . " *= ")
     ("**" . "**")
     ("**=" . " **=")
     ("<" key-combo-execute-original " << ")
     ("<=" . " <= ")
     ("<<=" . " <<= ")
     ("<-" . " <- ")
     ("<!" . "<!-- `!!' -->")
     ("|" " | " " || ")
     ("|=" . " |= ")
     ("||=" . " ||= ")
     ("/" key-combo-execute-original)
     ("/=" . " /= ")
     ("*/" . "*/")
     ("/*" . "/* `!!' */")
     ("/* RET" . "/*
`!!'
*/")
     ("{" key-combo-execute-original)
     ("{ RET" . "{
`!!'
}"))))
 '(line-spacing 5)
 '(magit-commit-arguments nil)
 '(magit-diff-use-overlays nil)
 '(magit-popup-show-common-commands nil)
 '(magit-push-always-verify nil)
 '(magit-revert-buffers t t)
 '(magit-visit-ref-create t)
 '(make-backup-files nil)
 '(markdown-marginalize-headers t)
 '(mc/always-run-for-all t)
 '(neo-smart-open t)
 '(neo-vc-integration (quote (face)))
 '(nlinum-format " %d ")
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(objed-cursor-color "#C16069")
 '(org-fontify-done-headline t t)
 '(org-fontify-quote-and-verse-blocks t t)
 '(org-fontify-whole-heading-line t t)
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (flycheck-title kubernetes eslint-fix poet-theme go-fill-struct company-emacs-eclim eclim emacs-eclim minimal-theme minimal phoenix-dark-pink-theme phoneix-dark-pink-theme go-eldoc company-go default-text-scale go-mode tao-themes auto-rename-tag company-restclient restclient pomidor graphql-mode highlight-symbol uuidgen flycheck highlight-indentation eyebrowse treemacs-projectile solaire-mode quickrun auto-highlight-symbol alchemist elixir-mode elixir twilight-jazz-theme import-js indent-guide eldoc-eval kaolin-themes clojure imenu-anywhere imenu-list doom-themes subatomic-theme docker-compose-mode decide flycheck-inline js-import rg treemacs kanji-mode helpful twilight-bright-theme nlinum-hl fancy-narrow origami spaceline benchmark-init company-emoji company-quickhelp git-gutter-fringe+ zerodark-theme zerodark memoize ibuffer-projectile anzu nord-theme rainbow-mode json-mode eslintd-fix flycheck-pos-tip flycheck-clojure ac-cider try flow-mode cider dracula gotham-theme dracula-theme toggle-quotes ample-zen-theme tangotango-theme kaolin-theme color-theme-sanityinc-tomorrow anti-zenburn-theme zenburn-theme danneskjold-theme prassee-theme which-key fancy-battery company-tern window-purpose purpose resize-window window-resize winum copy-as-format js-format ox-bullets rjsx-mode smooth-scrolling ox-md zoom-window yaml-mode yahoo-weather window-layout wgrep-ag wgrep-ack web-mode web-completion-data vline visual-regexp use-package undo-tree tao-theme suscolors-theme sudo-edit sticky-windows spacemacs-theme solarized-theme smex smartparens smart-newline smart-forward skewer-mode shell-pop scss-mode rust-mode restart-emacs request repository-root rainbow-delimiters pug-mode projectile pos-tip popwin php-mode ox-twbs ox-reveal org-wc org-bullets operate-on-number noflet niflheim-theme nginx-mode neotree names nameless multi-line multi monokai-theme math-symbol-lists material-theme markdown-toc magit-gh-pulls lorem-ipsum linum-off link-hint leuven-theme legalese kurecolor key-combo js2-refactor jenkins ivy-hydra inf-ruby image+ ibuffer-vc hideshowvis haml-mode gruvbox-theme grunt gntp github-issues github-browse-file git-timemachine git-messenger git-link git-gutter-fringe forest-blue-theme font-lock+ flyspell-popup flycheck-tip flycheck-rust flx exec-path-from-shell etags-select eshell-prompt-extras eshell-git-prompt engine-mode embrace eink-theme drag-stuff dockerfile-mode docker dired-quick-sort dired-narrow dired-filter dired+ darktooth-theme darkokai-theme cycle-quotes creamsody-theme counsel context-coloring composable command-log-mode coffee-mode chinese-word-at-point calfw buffer-move browse-url-dwim avy-zap aurora-theme auctex atomic-chrome atom-one-dark-theme ag ack-and-a-half ack ac-html ac-emoji ac-emmet ac-dabbrev)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(popwin:adjust-other-windows t)
 '(popwin:popup-window-height 15)
 '(popwin:special-display-config
   (quote
    (("*quickrun*" :regexp nil :width 0.4 :position right :dedicated t)
     ("*rg*" :regexp nil :height 0.4 :position bottom :noselect nil)
     ("*Flycheck errors*" :regexp nil :position bottom :stick t)
     ("*alchemist-eval-mode*" :position bottom :tail t)
     ("*git-gutter+-diff*" :regexp nil :position bottom)
     ("*compilation*" :regexp nil :height 30 :position bottom :noselect t :stick t :tail t)
     ("*s-" :regexp t :position bottom)
     ("*grunt" :regexp nil :position bottom :noselect t)
     ("*RE-Builder*" :height 2 :position bottom)
     ("*run-current-file output*" :regexp nil :position bottom :noselect t :dedicated t :tail t)
     ("*Ido Completions*" :position bottom :noselect t)
     ("*Completions*" :position bottom :noselect t))))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8")
 '(powerline-color-alist (quote ((powerline-color1 1) (powerline-color2 1))) t)
 '(powerline-color0 "#282828")
 '(powerline-color1 "#3C3C3C" t)
 '(powerline-color2 "#515151" t)
 '(powerline-foreground "#eeeeec")
 '(rainbow-delimiters-highlight-braces-p t)
 '(rainbow-delimiters-highlight-brackets-p t)
 '(safe-local-variable-values
   (quote
    ((flycheck-eslintrc . ".eslintrc.js")
     (flycheck-eslintrc . ".eslintrc")
     (eval add-hook
           (quote after-save-hook)
           (quote
            (lambda nil
              (byte-compile-file
               (buffer-file-name))))
           nil t))))
 '(scroll-bar-mode nil)
 '(scss-output-directory "../")
 '(shell-pop-autocd-to-working-dir nil)
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-window-position "bottom")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(smex-prompt-string "Why won't you just fucking ")
 '(sp-hybrid-kill-excessive-whitespace t)
 '(sp-sexp-suffix (quote ((js2-mode regexp "\"\""))))
 '(spaceline-all-the-icons-file-name-highlight "#fbece1")
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(tabbar-background-color "#353535")
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(treemacs-follow-mode t)
 '(treemacs-icon-closed-text #("📁 " 0 2 (face treemacs-directory-face)) t)
 '(treemacs-icon-open-text #("📂 " 0 2 (face treemacs-directory-face)) t)
 '(treemacs-icon-tag-leaf-text #("● " 0 2 (face treemacs-term-node-face)) t)
 '(treemacs-icon-tag-node-closed-text #("📀 " 0 2 (face treemacs-tags-face)) t)
 '(treemacs-icon-tag-node-open-text #("💿 " 0 2 (face treemacs-tags-face)) t)
 '(treemacs-icon-text #("📄 " 0 2 (face treemacs-file-face)) t)
 '(user-mail-address "dgc336@gmail.com")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#698b22")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#698b22")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#698b22"))))
 '(vc-annotate-very-old-color nil)
 '(vc-directory-exclusion-list
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "tvpjslib" "biscuit" ".tags")))
 '(vc-ignore-dir-regexp
   "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|biscuit\\|tvpjslib\\|node_modules\\|tags\\|filecache\\|afs\\|\\.\\.\\.\\)/\\)\\'|biscuit|tvpjslib|node_modules|.tags")
 '(vc-revert-show-diff nil)
 '(vc-svn-program "/usr/local/bin/svn")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))))
 '(xkcd-cache-dir "~/.xkcd/" t)
 '(xkcd-cache-latest "~/.xkcd/latest")
 '(yas-fallback-behavior (quote call-other-command)))

;;; Font Manipulation:
(defvar font-list
  '(
    ("APL385 Unicode" light)
    ("Andale Mono" extralight)
    ("Anka/Coder" extralight)
    ("Anonymous Pro" normal)
    ("Apercu" medium)
    ("Arial Monospaced MT" normal)
    ("Bedstead" normal)
    ("Cousine" light)
    ("Courier Prime" light)
    ("DroidSansMono Nerd Font Mono" normal)
    ("Fira Code" light)
    ("Fixedsys Excelsior" thin)
    ("Hack" normal)
    ("Hermit" normal)
    ("Iosevka Nerd Font Mono" light)
    ("IBM 3270" light)
    ("Input Mono Narrow" light)
    ("Maison Neue" medium)
    ("Monoid" light)
    ("NovaMono" medium)
    ("ProggySquareTTSZ" medium)
    ("Operator Mono" extralight)
    ("SF Mono" thin)
    ("Sometype Mono" thin)
    ("SkModernist" normal)
    ("Source Code Pro" extralight)
  ))

(set-fontset-font "fontset-default" 'unicode '("Apple Color Emoji"))
(defun set-font ()
  "Forward to `set-font-action'."
  (interactive)
  (ivy-read "Set font: "
            font-list
            :action #'set-font-action
            :caller 'set-font))

(defun set-editor-font ()
  "Forward to `set-editor-font-action'."
  (interactive)
  (ivy-read "Set editor font: "
            font-list
            :action #'set-editor-font-action
            :caller 'set-editor-font))

(defun set-font-action (font)
  "Set the main text edit font to FONT."
  (pcase-let ((`(,family ,weight) font))
    (set-face-attribute 'solaire-default-face nil :family family :weight weight)
    (set-face-attribute 'fixed-pitch nil :family family :weight weight)
    (run-at-time "0.2 sec" nil
                 `(lambda () (when (not (eq (face-attribute 'solaire-default-face :family ) ,family))
                          (set-face-attribute 'solaire-default-face nil :family ,family :weight (quote ,weight)))))))
  
(defun set-editor-font-action (font)
  "Set the editor font to FONT."
  (pcase-let ((`(,family ,weight) font))
    (set-face-attribute 'variable-pitch nil :family family :weight weight)
    (set-face-attribute 'default nil :family family :weight weight)))

(provide 'custom)
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold))))
 '(markdown-header-face-1 ((t (:inherit font-lock-function-name-face :weight bold :height 1.8))))
 '(markdown-header-face-2 ((t (:inherit font-lock-function-name-face :weight bold :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit font-lock-function-name-face :weight bold :height 1.2)))))
