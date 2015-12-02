;;------------------
;; Custom Variables
;;------------------

;;; Code:

(defvar common-load-dir (file-name-directory load-file-name))
(defvar common-package-dir "packages")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/.emacs.d/snippets/abbrev_defs")
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-candidate-limit 30)
 '(ac-comphist-file
	 (format "%s/%s/ac-comphist.dat" common-load-dir common-package-dir))
 '(ac-etags-requires 1)
 '(ac-expand-on-auto-complete t)
 '(ack-and-a-half-arguments (quote (nil)))
 '(ack-and-a-half-executable "/usr/local/bin/ack")
 '(ansi-color-faces-vector
	 [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
	 ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-save-list-file-prefix
	 (format "%s/%s/auto-save-list/.saves-" common-load-dir common-package-dir))
 '(backup-by-copying-when-mismatch nil)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-setup (quote one-frame))
 '(calendar-view-diary-initially-flag t)
 '(compilation-message-face (quote (quote default)))
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes
	 (quote
		("0db94e3baa6604fd1bf3871cf602f3ac400f65fce4c8adac07758af9259aefce" "ffc576509985b49dce4e34ae4cef858c81cd678cc920d43971c91bdffedfa7d7" "e5343a4b2c731f7f5ab9e25ccc69a20482614d2bb66b9246f86bfcfb0de8891a" "f23c2d947b426285828db5ec060784c2278ced8a96b9c5b1f40eed1d58428493" default)))
 '(dabbrev-check-all-buffers nil)
 '(dabbrev-eliminate-newlines nil)
 '(default-input-method "TeX")
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(eimp-mogrify-program "mogrify")
 '(eshell-directory-name (format "%s/%s/eshell" common-load-dir common-package-dir))
 '(eyebrowse-mode t)
 '(file-cache-find-command "find -L")
 '(flycheck-highlighting-mode (quote lines))
 '(flycheck-jshintrc "/Users/charld13/.jshintrc")
 '(flymake-gui-warnings-enabled t)
 '(flymake-jslint-args
	 (quote
		("--bitwise" "--curly" "--indent" "--nomen" "--plusplus" "--vars" "--white")))
 '(flymake-log-level 3)
 '(git-gutter-fr:side (quote left-fringe))
 '(global-rainbow-delimiters-mode t)
 '(helm-reuse-last-window-split-state nil)
 '(helm-split-window-default-side (quote right))
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
 '(malabar-groovy-java-options
	 (quote
		("\"-Xms256m -Xmx512m -Djavax.net.ssl.keyStore=/Users/charld13/.subversion/dev.bbc.co.uk.p12 -Djavax.net.ssl.keyStorePassword=336MartIn!69 -Djavax.net.ssl.keyStoreType=PKCS12 -Djavax.net.ssl.trustStore=/Users/charld13/.m2/jssecacerts\"")))
 '(markdown-css-path mkdown-css-file-name)
 '(neo-auto-indent-point nil)
 '(neo-persist-show nil)
 '(neo-smart-open t)
 '(neo-theme (quote ascii))
 '(neo-window-width 35)
 '(org-agenda-files nil)
 '(org-support-shift-select t)
 '(package-user-dir (format "%s/%s/elpa" common-load-dir common-package-dir))
 '(popwin:popup-window-height 20)
 '(popwin:special-display-config
	 (quote
		(("*grunt" :regexp nil :position bottom :noselect t)
		 ("*Malabar Compilation*" :width 60 :position right :noselect t)
		 ("*RE-Builder*" :height 2 :position bottom)
		 ("*run-current-file output*" :regexp nil :position bottom :noselect t :dedicated t :tail t)
		 ("*Ido Completions*" :position bottom :noselect t)
		 ("*Help*")
		 ("Browsing by" :regexp t :position left)
		 ("*Completions*" :position bottom :noselect t)
		 ("*vc-log*" :position bottom)
		 ("*Occur*" :position bottom))) nil nil "op")
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(powerline-color-alist (quote ((powerline-color1 0.78) (powerline-color2 0.78))) t)
 '(powerline-color0 "#282828")
 '(powerline-color1 "#3C3C3C" t)
 '(powerline-color2 "#515151" t)
 '(powerline-foreground "#eeeeec")
 '(projectable-completion-func (quote car))
 '(projectable-filter-regexps
	 (quote
		("~$" "\\.o$" "\\.exe$" "\\.a$" "/\\.svn" "\\.elc$" "\\.output$" "\\.$" "#$" "\\.class$" "\\.png$" "\\.svn*" "\\/node_modules\\/*" "\\.gif$" "\\.gem$" "\\.pdf$" "\\.swp$" "\\.iml$" "\\.jar$" "\\/build\\/" "/\\.git" "\\/jsdoc\\/" "\\.min\\.js$" "\\.tags$" "\\.filecache" "\\.cache$" "\\/.git\\/" "report" "\\.gcov\\.html$" "\\.func.*\\.html$" "\\/tmp\\/")))
 '(rainbow-delimiters-highlight-braces-p t)
 '(rainbow-delimiters-highlight-brackets-p t)
 '(scroll-bar-mode nil)
 '(scss-output-directory "../")
 '(shell-pop-autocd-to-working-dir nil)
 '(shell-pop-shell-type
	 (quote
		("ansi-term" "*ansi-term*"
		 (lambda nil
			 (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 40)
 '(smex-prompt-string "Why won't you just fucking ")
 '(sp-hybrid-kill-excessive-whitespace t)
 '(sp-sexp-suffix (quote ((js2-mode regexp "\"\""))))
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(tabbar-background-color "#353535")
 '(tags-revert-without-query t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
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
 '(ac-candidate-face ((t (:background "khaki1" :foreground "grey14"))))
 '(ac-candidate-mouse-face ((t (:inherit nil :inverse-video nil))))
 '(ac-dabbrev-menu-face ((t (:background "#81a2be" :foreground "grey20"))))
 '(ac-dabbrev-selection-face ((t (:background "#6699cc" :foreground "grey20"))))
 '(ac-etags-candidate-face ((t (:background "#99cc99" :foreground "grey20"))))
 '(ac-etags-selection-face ((t (:background "#698b22" :foreground "grey20"))))
 '(ac-selection-face ((t (:background "goldenrod2" :foreground "grey20" :weight normal))))
 '(anzu-mode-line ((t (:foreground "#223311" :weight normal))))
 '(emms-browser-album-face ((t (:foreground "#f0c674" :weight extra-light :height 1.1 :family "DejaVu Sans Mono"))))
 '(emms-browser-artist-face ((t (:foreground "#cc6666" :height 1.3 :family "DejaVu Sans Mono Extralight"))))
 '(emms-browser-track-face ((t (:foreground "#698b22" :height 1.0 :family "DejaVu Sans Mono"))))
 '(emms-playlist-track-face ((t (:foreground "#698b22"))))
 '(hs-face ((t (:distant-foreground "black"))))
 '(js2-non-used ((t (:inherit nil :foreground "#9c9c9c" :slant italic))) t)
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
		(custom-theme-set-faces
		 theme
		 `(context-coloring-level-0-face ((t :foreground ,_0)))
		 `(context-coloring-level-1-face ((t :foreground ,_1)))
		 `(context-coloring-level-2-face ((t :foreground ,_2)))
		 `(context-coloring-level-3-face ((t :foreground ,_3)))
		 `(context-coloring-level-4-face ((t :foreground ,_4)))
		 `(context-coloring-level-5-face ((t :foreground ,_5)))
		 `(context-coloring-level-6-face ((t :foreground ,_6)))
		 `(context-coloring-level-7-face ((t :foreground ,_7)))
		 `(context-coloring-level-8-face ((t :foreground ,_8))))))
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
;;; custom.el ends here