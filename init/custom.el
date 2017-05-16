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
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
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
 '(cursor-type (quote (bar . 1)))
 '(custom-safe-themes
   (quote
    ("6c35ffc17f8288be4c7866deb7437e8af33cd09930e195738cdfef911ab77274" "a285e7bc4714013b6cd74ac4663b9fa5e8f734e989fcca42069c45ed702d4b72" "7f968c172d6ec46766773a8304c7570bdff45f1220d3700008a437d9529ca3e4" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "2997ecd20f07b99259bddba648555335ffb7a7d908d8d3e6660ecbec415f6b95" "6369644497a7dde1350dce08f404316c7cb19f2d1c1b937d412a8860c4eb02f5" "14099b48e45d17d7e007a9c33c690004861e4fb9bb14e9562a13a7b4ee3b683c" "6fa9baa12ed2813c3deb764cb896bb2e80ebbbbb786b56082ea4c4a23c2b6a69" "a3f85ee6e877f02e239d2a6633a5b8263b53113751aca549aa4c5f458829c95d" "66881e95c0eda61d34aa7f08ebacf03319d37fe202d68ecf6a1dbfd49d664bc3" "28315d38ed9f3ae4f84937ff390aec4739f86a78b8b55db15b8b81e4f8aae7ff" "5e46a1f5841476ba810a8331dbf2714ef5edb44ddf9b41aa0d681e1454107bf5" "60f9439182356ed1821f90ef1b1b1ca13234f81bfd25321a9cc80e6607a6a54f" "6df465c9d5df9d3f3aefb67e74ece2bf9e9e80e2037d22397e85d5bae190ab66" "36a116cdc52e42ca0a3496ed07e62025f8915243d7aee793b528a0673028c4bf" "d4c7190cd923cec95b3e1412eba44762fcaed44bab119e6657e0b924f740fda5" "1dbf2b600d24bc86c466d01f3c03f59b0d9159769daa7fcd4f2d8212e9fb708d" "ca2abff9d19f278aae4b694e745dde1eb1223cb559e36398a55178d42304a77a" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "5c6d40ef6e7bbe9e83dc0e32db794c7e9a6a0d9eb7d6a874aaf9744c053842b4" "c806912d706db590cde0483f0ec90db69bc5ce5da1d8846462d7c20aecb3dcda" "9f3dd1d7b46e99b94bb53506c44b651c811b3552100898842bdd22ce63ab8b55" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "d79ece4768dfc4bab488475b85c2a8748dcdc3690e11a922f6be5e526a20b485" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "99953b61ecd4c3e414a177934e888ce9ee12782bbaf2125ec2385d5fd732cbc2" "9baea9c4e3e3c6f38a861454fe566432e2d1c02bc1fa7cea9b9a11d7081f0819" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "b391fa6fc0967eb5980b203c27f5a98e9efc5296cddb92e232e17e592d8231e2" "98a619757483dc6614c266107ab6b19d315f93267e535ec89b7af3d62fb83cad" "dc54983ec5476b6187e592af57c093a42790f9d8071d9a0163ff4ff3fbea2189" "ff52e9e329c5a66eae3570e3f17288d0a9f96403ce1ac7cbca5a193ebc500936" "d9b0d5c7077ddad798e9749d0629e86b925c2e146641b24130edd8f82cd0cf5d" "e6d871e13fd608b78aaff9d7f63e81aab1c901439eb0f432d6bb505ff1dda3cb" "ac194ceaf2a741572f99274e22c96c953f27de11ca4944c05f4e3252937c12a0" "f110ecd815cb48e75e275db68d06e7439a6f82429d1cf51382520cfb5652fc44" "0107651d44bfacc9ee4d725fdba9a617273ef0569bffc8f07e7212d39d1bf6ec" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "419637b7a8c9cb43f273980f0c9879c0cbadace6b38efac0281e031772c84eb2" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "c46068478d29787a89c3930841e8de94d59cd6a573e4d479d71e78a3f41c6ae3" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "0db94e3baa6604fd1bf3871cf602f3ac400f65fce4c8adac07758af9259aefce" "ffc576509985b49dce4e34ae4cef858c81cd678cc920d43971c91bdffedfa7d7" "e5343a4b2c731f7f5ab9e25ccc69a20482614d2bb66b9246f86bfcfb0de8891a" "f23c2d947b426285828db5ec060784c2278ced8a96b9c5b1f40eed1d58428493" default)))
 '(dabbrev-check-all-buffers nil)
 '(dabbrev-eliminate-newlines nil)
 '(default-input-method "TeX")
 '(emmet-quote-style "\"")
 '(eval-sexp-fu-flash-mode t)
 '(fci-rule-color "#232A2F")
 '(flycheck-error-list-format
   [("Line" 4 flycheck-error-list-entry-< :right-align t)
    ("Col" 3 nil :right-align t)
    ("Level" 8 flycheck-error-list-entry-level-<)
    ("ID" 20 t)
    ("Message (Checker)" 0 t)] t)
 '(flycheck-eslintrc ".eslintrc.json" t)
 '(flycheck-highlighting-mode (quote lines))
 '(flycheck-idle-change-delay 2)
 '(git-gutter-fr:side (quote left-fringe))
 '(global-prettify-symbols-mode t)
 '(global-rainbow-delimiters-mode t)
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
 '(jpop-completion-func (quote car))
 '(jpop-filter-regexps
   (quote
    ("~$" "\\.o$" "\\.exe$" "\\.a$" "/\\.svn" "\\.elc$" "\\.output$" "\\.$" "#$" "\\.class$" "\\.png$" "\\.svn*" "/node_modules/" "\\.gif$" "\\.gem$" "\\.pdf$" "\\.swp$" "\\.iml$" "\\.jar$" "\\/build\\/" "/\\.git" "\\/jsdoc\\/" "\\.min\\.js$" "\\.tags$" "\\.filecache" "\\.cache$" "\\/.git\\/" "\\/report\\/" "\\.gcov\\.html$" "\\.func.*\\.html$" "\\/tmp\\/")))
 '(jpop-project-directory "/Users/charld13/Projects")
 '(js-indent-level 2 t)
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
 '(line-spacing 3)
 '(magit-commit-arguments nil)
 '(magit-diff-use-overlays nil)
 '(magit-popup-show-common-commands nil)
 '(magit-push-always-verify nil)
 '(magit-revert-buffers t t)
 '(magit-visit-ref-create t)
 '(make-backup-files nil)
 '(mc/always-run-for-all t)
 '(neo-smart-open t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (company-quickhelp git-gutter-fringe+ zerodark-theme zerodark memoize ibuffer-projectile anzu nord-theme rainbow-mode json-mode eslintd-fix flycheck-pos-tip flycheck-clojure ac-cider try flow-mode cider dracula gotham-theme dracula-theme toggle-quotes ample-zen-theme tangotango-theme kaolin-theme color-theme-sanityinc-tomorrow anti-zenburn-theme zenburn-theme danneskjold-theme prassee-theme which-key fancy-battery company-tern window-purpose purpose resize-window window-resize winum copy-as-format js-format spaceline ox-bullets rjsx-mode smooth-scrolling ox-md zoom-window yaml-mode yahoo-weather window-layout wgrep-ag wgrep-ack web-mode web-completion-data vline visual-regexp use-package undo-tree tao-theme suscolors-theme sudo-edit sticky-windows spacemacs-theme solarized-theme smex smartparens smart-newline smart-forward skewer-mode shell-pop scss-mode rust-mode restart-emacs request repository-root rainbow-delimiters pug-mode projectile pos-tip popwin php-mode ox-twbs ox-reveal org-wc org-bullets operate-on-number noflet niflheim-theme nginx-mode neotree names nameless multi-line multi monokai-theme math-symbol-lists material-theme markdown-toc magit-gh-pulls lorem-ipsum linum-off link-hint leuven-theme legalese kurecolor key-combo js2-refactor jenkins ivy-hydra inf-ruby image+ ibuffer-vc hideshowvis haml-mode gruvbox-theme grunt gntp github-issues github-browse-file git-timemachine git-messenger git-link git-gutter-fringe forest-blue-theme font-lock+ flyspell-popup flycheck-tip flycheck-rust flx exec-path-from-shell etags-select eshell-prompt-extras eshell-git-prompt engine-mode embrace eink-theme drag-stuff dockerfile-mode docker dired-quick-sort dired-narrow dired-filter dired+ darktooth-theme darkokai-theme cycle-quotes creamsody-theme counsel context-coloring composable command-log-mode coffee-mode chinese-word-at-point calfw buffer-move browse-url-dwim benchmark-init avy-zap aurora-theme auctex atomic-chrome atom-one-dark-theme ag ack-and-a-half ack ac-html ac-emoji ac-emmet ac-dabbrev)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(popwin:popup-window-height 15)
 '(popwin:special-display-config
   (quote
    (("*Flycheck errors*" :regexp nil :position bottom :stick t)
     ("*compilation*" :regexp nil :height 30 :position bottom :noselect t :tail t :stick t)
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
    ((flycheck-eslintrc . ".eslintrc")
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
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 40)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(smex-prompt-string "Why won't you just fucking ")
 '(sp-hybrid-kill-excessive-whitespace t)
 '(sp-sexp-suffix (quote ((js2-mode regexp "\"\""))))
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(tabbar-background-color "#353535")
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight extra-light :height 130 :width normal :foundry "nil" :family "Droid Sans Mono"))))
 '(ac-candidate-face ((t (:inherit company-tooltip-selection))))
 '(ac-selection-face ((t (:inherit company-tooltip-common-selection))))
 '(ac-yasnippet-candidate-face ((t (:inherit company-scrollbar-bg :foreground "#e74c3c"))))
 '(ac-yasnippet-selection-face ((t (:foreground "#c0392b"))))
 '(cfw:face-header ((t (:inherit \#F9690E :weight bold))))
 '(cfw:face-holiday ((t (:inherit (font-lock-doc-face cfw:face-day-title) :weight bold))))
 '(cfw:face-periods ((t (:inherit font-lock-warning-face :weight light))))
 '(cfw:face-select ((t (:background "#F27935" :foreground "#22313F"))))
 '(cfw:face-sunday ((t (:foreground "#6C7A89" :weight bold))))
 '(cfw:face-title ((t (:inherit font-lock-keyword-face :weight bold :height 2.0))))
 '(cfw:face-today ((t (:inherit font-lock-warning-face :weight bold))))
 '(cfw:face-today-title ((t (:background "#D24D57" :foreground "#22313F" :weight bold))))
 '(cfw:face-toolbar-button-off ((t (:inherit default :weight bold))))
 '(diredp-compressed-file-name ((t (:inherit dired-warning))))
 '(diredp-compressed-file-suffix ((t (:inherit dired-warning :weight bold))))
 '(diredp-date-time ((t (:inherit dired-mark :slant italic))))
 '(diredp-dir-heading ((t (:inherit dired-header))))
 '(diredp-dir-name ((t (:inherit dired-directory :background "#2C2C2C2C2C2C"))))
 '(diredp-dir-priv ((t (:inherit dired-directory :background "#2C2C2C2C2C2C"))))
 '(diredp-file-name ((t (:inherit default))))
 '(diredp-file-suffix ((t (:inherit dired-warning))))
 '(diredp-number ((t (:inherit dired-marked))))
 '(diredp-symlink ((t (:inherit dired-symlink))))
 '(eval-sexp-fu-flash ((t (:inherit spaceline-highlight-face :weight bold))))
 '(flycheck-error ((t (:underline "#74CBC4"))))
 '(flycheck-info ((t (:underline "#FF516D"))))
 '(flycheck-warning ((t (:underline "#FFEB95"))))
 '(hs-face ((t (:distant-foreground "black"))))
 '(js2-function-call ((t (:inherit font-lock-function-name-face :weight extra-light))))
 '(js2-non-used ((t (:inherit nil :foreground "#9c9c9c" :slant italic))) t)
 '(js2-object-property ((t (:inherit default :weight bold))))
 '(markdown-header-face ((t (:inherit default :weight bold))))
 '(markdown-header-face-1 ((t (:inherit default :height 1.8))))
 '(markdown-header-face-2 ((t (:inherit default :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit default :height 1.2))))
 '(neo-dir-link-face ((t (:inherit default :height 0.9))))
 '(neo-file-link-face ((t (:inherit default :height 0.9))))
 '(neo-vc-added-face ((t (:foreground "LightGreen" :height 0.9))))
 '(neo-vc-edited-face ((t (:foreground "#FFA15" :height 0.9))))
 '(neo-vc-up-to-date-face ((t (:foreground "LightGray" :height 0.9))))
 '(popup-menu-summary-face ((t (:inherit company-tooltip-selection))))
 '(popup-scroll-bar-background-face ((t (:inherit company-scrollbar-bg))))
 '(popup-scroll-bar-foreground-face ((t (:inherit company-scrollbar-fg))))
 '(popup-summary-face ((t (:inherit company-tooltip-selection))))
 '(popup-tip-face ((t (:inherit company-tooltip-selection :underline nil :slant normal :weight normal))))
 '(vertical-border ((((type tty)) (:inherit \#1b1d1e)))))

;;; Font Manipulation:
(defvar font-list '(("Menlo" normal)
                    ("Monaco" normal)
                    ("Courier New" normal)
                    ("Droid Sans Mono" normal)
                    ("Ubuntu Mono derivative Powerline" normal)
                    ("Source Code Pro" extralight)
                    ("Inconsolata" medium)
                    ("Apercu" medium)
                    ("Maison Neue" medium)
                    ("NotCourierSans" normal)
                    ("Hack" normal)
                    ("Anonymous Pro" normal)))

(defun set-font (&optional f)
  "Set the custom font F with completion."
  (interactive)
  (let* ((font (or f (assoc (completing-read "Font: " font-list nil nil) font-list)))
         (family (car font))
         (weight (cadr font)))
    (set-face-attribute 'default nil :family family :weight weight)
    (run-at-time "0.2 sec" nil
                 `(lambda () (when (not (eq (face-attribute 'default :family ) ,family))
                          (set-face-attribute 'default nil :family ,family :weight (quote ,weight)))))))

(provide 'custom)
;;; custom.el ends here
