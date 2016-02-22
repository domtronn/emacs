;;------------------
;; Custom Variables
;;------------------

;;; Code:

(defvar common-load-dir (file-name-directory load-file-name))
(defvar common-package-dir "../packages")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/.emacs.d/snippets/abbrev_defs")
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-candidate-limit 80)
 '(ac-comphist-file
   (format "%s/%s/ac-comphist.dat" common-load-dir common-package-dir))
 '(ac-etags-requires 1)
 '(ac-expand-on-auto-complete t)
 '(ac-quick-help-delay 0.5)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-save-list-file-prefix
   (format "%s/%s/auto-save-list/.saves-" common-load-dir common-package-dir))
 '(backup-by-copying-when-mismatch nil)
 '(compilation-message-face (quote (quote default)))
 '(custom-safe-themes
   (quote
    ("d9b0d5c7077ddad798e9749d0629e86b925c2e146641b24130edd8f82cd0cf5d" "e6d871e13fd608b78aaff9d7f63e81aab1c901439eb0f432d6bb505ff1dda3cb" "ac194ceaf2a741572f99274e22c96c953f27de11ca4944c05f4e3252937c12a0" "f110ecd815cb48e75e275db68d06e7439a6f82429d1cf51382520cfb5652fc44" "0107651d44bfacc9ee4d725fdba9a617273ef0569bffc8f07e7212d39d1bf6ec" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "419637b7a8c9cb43f273980f0c9879c0cbadace6b38efac0281e031772c84eb2" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "c46068478d29787a89c3930841e8de94d59cd6a573e4d479d71e78a3f41c6ae3" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "0db94e3baa6604fd1bf3871cf602f3ac400f65fce4c8adac07758af9259aefce" "ffc576509985b49dce4e34ae4cef858c81cd678cc920d43971c91bdffedfa7d7" "e5343a4b2c731f7f5ab9e25ccc69a20482614d2bb66b9246f86bfcfb0de8891a" "f23c2d947b426285828db5ec060784c2278ced8a96b9c5b1f40eed1d58428493" default)))
 '(dabbrev-check-all-buffers nil)
 '(dabbrev-eliminate-newlines nil)
 '(default-input-method "TeX")
 '(fci-rule-color "#232A2F")
 '(flycheck-highlighting-mode (quote lines))
 '(flycheck-idle-change-delay 2)
 '(git-gutter-fr:side (quote left-fringe))
 '(global-rainbow-delimiters-mode t)
 '(helm-reuse-last-window-split-state t)
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
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(ibuffer-default-sorting-mode (quote major-mode))
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
 '(ibuffer-show-empty-filter-groups nil)
 '(ido-buffer-disable-smart-matches nil)
 '(ido-enable-dot-prefix t)
 '(ido-file-extensions-order
   (quote
    (".json" ".css" ".pl" ".sh" ".as" ".php" ".emacs" ".ini" ".js" ".el" ".ini" ".cfg" ".cnf" "")))
 '(ido-separator nil)
 '(ido-use-filename-at-point (quote guess))
 '(jpop-completion-func (quote car))
 '(jpop-filter-regexps
   (quote
    ("~$" "\\.o$" "\\.exe$" "\\.a$" "/\\.svn" "\\.elc$" "\\.output$" "\\.$" "#$" "\\.class$" "\\.png$" "\\.svn*" "\\/node_modules\\/*" "\\.gif$" "\\.gem$" "\\.pdf$" "\\.swp$" "\\.iml$" "\\.jar$" "\\/build\\/" "/\\.git" "\\/jsdoc\\/" "\\.min\\.js$" "\\.tags$" "\\.filecache" "\\.cache$" "\\/.git\\/" "\\/report\\/" "\\.gcov\\.html$" "\\.func.*\\.html$" "\\/tmp\\/")))
 '(jpop-project-directory "/Users/charld13/Projects")
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-enter-indents-newline t t)
 '(js2-global-externs
   (quote
    ("log" "require" "spyOn" "jasmine" "expect" "beforeEach" "afterEach" "clearTimeout" "sinon" "module" "describe" "it" "define" "setInterval" "setTimeout" "bbc" "beforeAll")))
 '(jshint-configuration-path "~/.jshintrc")
 '(jshint-mode-jshintrc "~/.jshintrc")
 '(jshint-mode-mode "jslint")
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
 '(magit-popup-show-common-commands nil)
 '(magit-push-always-verify nil)
 '(magit-revert-buffers t)
 '(magit-visit-ref-create t)
 '(make-backup-files nil)
 '(neo-auto-indent-point nil)
 '(neo-persist-show nil)
 '(neo-smart-open t)
 '(neo-theme (quote ascii))
 '(neo-window-width 35)
 '(org-agenda-files nil)
 '(org-support-shift-select t)
 '(package-user-dir
   (expand-file-name
    (format "%s/%s/elpa" common-load-dir common-package-dir)))
 '(popwin:popup-window-height 20)
 '(popwin:special-display-config
   (quote
    (("*Flycheck errors*" :regexp nil :position bottom :stick t)
     ("*s-" :regexp t :position bottom)
     ("*grunt" :regexp nil :position bottom :noselect t)
     ("*RE-Builder*" :height 2 :position bottom)
     ("*run-current-file output*" :regexp nil :position bottom :noselect t :dedicated t :tail t)
     ("*Ido Completions*" :position bottom :noselect t)
     ("*Completions*" :position bottom :noselect t))))
 '(powerline-color-alist (quote ((powerline-color1 1) (powerline-color2 1))) t)
 '(powerline-color0 "#282828")
 '(powerline-color1 "#3C3C3C" t)
 '(powerline-color2 "#515151" t)
 '(powerline-foreground "#eeeeec")
 '(rainbow-delimiters-highlight-braces-p t)
 '(rainbow-delimiters-highlight-brackets-p t)
 '(safe-local-variable-values
   (quote
    ((eval add-hook
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
 '(smex-prompt-string "Why won't you just fucking ")
 '(sp-hybrid-kill-excessive-whitespace t)
 '(sp-sexp-suffix (quote ((js2-mode regexp "\"\""))))
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(tabbar-background-color "#353535")
 '(tags-revert-without-query t)
 '(vc-annotate-background nil)
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
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))))
 '(xkcd-cache-dir "~/.xkcd/")
 '(xkcd-cache-latest "~/.xkcd/latest")
 '(yas-fallback-behavior (quote call-other-command)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight extra-light :height 130 :width normal :foundry "nil" :family "Courier New"))))
 '(ac-candidate-face ((t (:background "#2B3B40" :foreground "#FFEB95"))))
 '(ac-candidate-mouse-face ((t nil)))
 '(ac-dabbrev-menu-face ((t (:background "#2B3B40" :foreground "#81a2be"))))
 '(ac-dabbrev-selection-face ((t (:background "#2B3B40" :foreground "#6699cc"))))
 '(ac-selection-face ((t (:background "#2B3B40" :foreground "goldenrod2" :weight normal))))
 '(ac-yasnippet-candidate-face ((t (:inherit ac-candidate-face :background "#2B3B40" :foreground "sandybrown"))))
 '(ac-yasnippet-selection-face ((t (:background "#2B3B40" :foreground "coral3"))))
 '(flycheck-error ((t (:underline "#74CBC4"))))
 '(flycheck-info ((t (:underline "#FF516D"))))
 '(flycheck-warning ((t (:underline "#FFEB95"))))
 '(helm-candidate-number ((t (:background "#fece28" :foreground "#3a3a3a"))))
 '(helm-header-line-left-margin ((t (:background "#fece28" :foreground "#3a3a3a"))))
 '(helm-selection ((t (:background "#F77669" :distant-foreground "#263038" :foreground "#263038"))))
 '(helm-source-header ((t (:background "#BE5046" :foreground "#263038" :height 1.3))))
 '(helm-swoop-target-line-block-face ((t (:background "#E5C07B" :foreground "#222222"))))
 '(helm-swoop-target-line-face ((t (:background "#E5C07B" :foreground "#222222"))))
 '(helm-swoop-target-word-face ((t (:background "#C678DD" :foreground "#282C34"))))
 '(hs-face ((t (:distant-foreground "black"))))
 '(js2-non-used ((t (:inherit nil :foreground "#9c9c9c" :slant italic))) t)
 '(popup-face ((t (:background "#2B3B40" :foreground "#FFEB95"))))
 '(popup-menu-selection-face ((t (:background "#335D7F" :foreground "#FFEB95"))))
 '(popup-scroll-bar-foreground-face ((t (:background "#232A2F"))))
 '(popup-tip-face ((t (:background "#2B3B40" :foreground "#FFEB95"))))
 '(vertical-border ((((type tty)) (:inherit \#1b1d1e)))))

;;; Font Manipulation:
(defvar font-list '(("Menlo" normal)
                    ("BPmono" normal)
                    ("Courier New" normal)
                    ("Ubuntu Mono" normal)
                    ("Source Code Pro" extralight)
                    ("NK57 Monospace" light)
                    ("Inconsolata" medium)
                    ("Hack" normal)
                    ("Cutive Mono" normal)
                    ("Anonymous Pro" normal)))

(defun set-font (&optional f)
  (interactive)
  (let* ((font (or f (assoc (completing-read "Font: " font-list nil nil) font-list)))
         (family (car  font))
         (weight (cadr  font)))
    (set-face-attribute 'default nil :family family :weight weight)
    (run-at-time "0.2 sec" nil
                 `(lambda () (when (not (eq (face-attribute 'default :family ) ,family))
                          (set-face-attribute 'default nil :family ,family :weight (quote ,weight)))))))

;;; Context Coloring:

(add-hook 'context-coloring-mode-hook 'context-coloring)
(add-hook 'context-coloring-mode-hook '(lambda () (prettify-symbols-mode 1) (context-coloring-colorize)))
(defun set-context-coloring (&optional _0 _1 _2 _3 _4 _5 _6 _7 _8)
	(let ((theme (car custom-enabled-themes)))
		(mapc (lambda (n)
						(custom-theme-set-faces
						 theme
						 `(,(intern (format "context-coloring-level-%s-face" n)) ((t :foreground ,(symbol-value (intern (format "_%s" n))))))))
					(number-sequence 0 8)) t))

(defun context-coloring (&optional override)
	"Load context colouring for different themes."
	(interactive)
	(let* ((theme (or override (car custom-enabled-themes))))
		(cond ((or (eq theme 'monokai) (eq theme 'wombat))
					 (set-context-coloring "#f8f8f2" "#66d9ef" "#a1efe4" "#a6e22e" "#e6db74" "#fd971f" "#f92672" "#fd5ff0" "#ae81ff"))
					((or (eq theme 'firecode) (eq theme 'ujelly))
					 (set-context-coloring "#f8f8f2" "#ae81ff" "#f63249" "#fd971f" "#e6db74" "#a6e22e" "#a1efe4" "#66d9ef"))
					((or (eq theme 'ample) (eq theme 'ample-flat) (eq theme 'arjen-grey))
					 (set-context-coloring "#bdbdb3" "#baba36" "#6aaf50" "#5180b3" "#ab75c3" "#cd7542" "#df9522" "#454545"))
					((or (eq theme 'atom-one-dark) (eq theme 'tsdh-dark) (eq theme 'atom-one-dark))
					 (set-context-coloring "#bdbdb3" "#BE5046" "#df9522" "#cd7542" "#ab75c3" "#5180b3" "#6aaf50" "#baba36" "#454545"))
					((or (eq theme 'aurora) (eq theme 'gotham) (eq theme 'misterioso))
					 (set-context-coloring "#f8f8f2" "#268bd2" "#2aa198" "#74CBC4" "#FFEB95" "#F9D330" "#cb4b16" "#dc322f" "#d33682"))
					((or (eq theme 'spacemacs-dark))
					 (set-context-coloring "#ffffff" "#89aaeb" "#c189eb" "#bf616a" "#dca432" "#ebcb8b" "#b4eb89" "#89ebca"))
					((or (eq theme 'zenburn) (eq theme 'darkmine))
					 (set-context-coloring "#dcdccc" "#93e0e3" "#bfebbf" "#f0dfaf" "#dfaf8f" "#cc9393" "#dc8cc3" "#94bff3" "#9fc59f"))
					((or (eq theme 'tango-dark))
					 (set-context-coloring "#c6a57b" "#346604" "#204a87" "#5c3566" "#a40000" "#b35000" "#c4a000" "#8ae234" "#8cc4ff"))
					((or (eq theme 'gruvbox))
					 (set-context-coloring "#fdf4c1" "#fb4934" "#fe8019" "#fabd2f" "#b8bb26" "#8ec07c" "#83a598" "#458588" "#b16286")))))

(provide 'custom)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
;;; custom.el ends here
