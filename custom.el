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
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-candidate-limit 30)
 '(ac-comphist-file
	 (format "%s/%s/ac-comphist.dat" common-load-dir common-package-dir))
 '(ac-etags-requires 1)
 '(ac-expand-on-auto-complete t)
 '(ack-and-a-half-arguments (quote (nil)))
 '(ack-and-a-half-executable "/usr/local/bin/ack")
 '(abbrev-file-name "~/.emacs.d/snippets/abbrev_defs")
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
 '(completion-ignored-extensions
	 (quote
		(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".svn-base" ".gcov.html")))
 '(crosshairs-mode nil)
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes
	 (quote
		("693f5a81a3728c2548efb4118c81941933cf0f7b614f9f3133101395e5830152" "344de9645be9b518212355e951a17ca8cbe78c1dc43a552294a759c7862a22ce" "00f33c1c844d52ea581f6e6eefb00d44bfc0ce8c69da8d302cf6fdbc279267b5" "ce371b56cf0952d838db6dafd92aaa6e3aadd74199c06ed7440da9df5595c4ae" "dfd04932e786d429b61668df3365aa17b8b34748f34a9958188fd553877d050b" "19a7cf7bc84abba0cb364914c6818ccb7f4a8da799cc6082606190b3fff0739d" "dc261a3614777e08f8c8f70d7cf63215786eafeedae45fc7f062c614eabf584c" "e5343a4b2c731f7f5ab9e25ccc69a20482614d2bb66b9246f86bfcfb0de8891a" "65320d86c52e9019347ed725f2a7c07705be5acb38bc83b92064e2489f6c3edc" "685a7460fdc4b8c38796234d3a96b3aacbe4fba739fb33b5d6d149051ce74a58" "f5ef7ddecf161a2951048c204c2c6d9d5be08745b136dce583056ad4b234b861" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "c46068478d29787a89c3930841e8de94d59cd6a573e4d479d71e78a3f41c6ae3" "ffc576509985b49dce4e34ae4cef858c81cd678cc920d43971c91bdffedfa7d7" "54d1a67c63de597c83e937f443ab16e7b073e9df5d2ac1c0b5b9a3e19541b60f" "2a56fd74172e87e2bdb40c54a7cc1f4076daff786610197557ae55d4d86a0dff" "bed4d169698488b8b5b90f7dbdbaca2e7b9c4a18727adbb7b3ddcb4df0577ce0" "91faf348ce7c8aa9ec8e2b3885394263da98ace3defb23f07e0ba0a76d427d46" "3f78849e36a0a457ad71c1bda01001e3e197fe1837cb6eaa829eb37f0a4bdad5" "bdaab014ec6785f64b72efbea80808b762d8971247aacf2ffc6b76a39b9ed97c" "36affb6b6328d2bfa7a31b3183cd65d6dd1a8c0945382f94de729233b9737359" "d7088a7105aa09cc68e3d058f89917e07e0505e0f4ab522a6045ec8092d67c44" "8f4898805186ae0419125c5fd8cddcd3bd2946c9b1c8e96167f84f762c422f80" "de8fa309eed1effea412533ca5d68ed33770bdf570dcaa458ec21eab219821fd" "c006bc787154c31d5c75e93a54657b4421e0b1a62516644bd25d954239bc9933" "b5cb6ef3ffdf323f16b5f76fdfef9bf42672a1dc28187b761e4867d6f06e175a" "ca9d9cf1d550a6296db85048aebd07e982d3ce2e52727e431809fa579f5c8ebb" "e8a9dfa28c7c3ae126152210e3ccc3707eedae55bdc4b6d3e1bb3a85dfb4e670" "68cca1c0563918960cdd98f16164fb7c43ffc4c44ad6d97e10eda9cd684790bc" "0db94e3baa6604fd1bf3871cf602f3ac400f65fce4c8adac07758af9259aefce" "18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" "2212a7ebf9bb938b21c3918d8e068472fdffc6ec516a9ac30c016fac8e5601cb" "2b7dc2599258569933d8106c9778873d8806267bc702d1c58d31fb1f416f6eb4" "c35c0effa648fd320300f3d45696c640a92bdc7cf0429d002a96bda2b42ce966" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "b391fa6fc0967eb5980b203c27f5a98e9efc5296cddb92e232e17e592d8231e2" "e8825f26af32403c5ad8bc983f8610a4a4786eb55e3a363fa9acb48e0677fe7e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "823ea71cd79048ec98ba0bd131d969fa51ff595f8bdb25640b92e84653d72fb6" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "12722541c8998f056b761bf63a92216aaf4610e4eb1afe7991842a31fa28b6d8" "2bed8550c6f0a5ce635373176d5f0e079fb4fb5919005bfa743c71b5eed29d81" "81a4b3d3751940b01617381397f31168420252e50cc9600cc0fc168ff4819ced" "a776135e3d68ebb9c5033799a86290e2243e352f5b8fe6b3b96fbf80c65acd0c" "13590cc9554286c8e893463fd8e0dad7134d6b7db10060afbb6850db3e395f17" "4d8a17c8614aad8625b206341213ec1dbefa9b3eb35efb68b3ea286aa33e7bbb" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "fc01bb8da59411baec19563336db6afc0cd872931388a363aa2effee4a426e35" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "0516c9d3bf66bd7b3f31fb1432a15a1b4e106897797114313208ecfb53096df7" "9b5b8562882c5393daa590bef170ff0b200ce11684cb95674f3f1857456b5d05" "629d9ba6189ae9b42bb8d49ee57d23e6ea7cc697f1593c17ecf78bba38a65459" "2e11112c059abb3609d56ba4bd8d755a90888ab5bcbc679cd7082cc02e30ad3c" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "f1af57ed9c239a5db90a312de03741e703f712355417662c18e3f66787f94cbe" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "d60eb8ab8fc31c9058dd1b5b297549f19d5cf63778c677ea16649f0f1e527b3c" "36cbfa09a3733f2ce7bb193df4c2ae1cc5f542f914d47717bf705fbf3fb18ddc" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "990920bac6d35106d59ded4c9fafe979fb91dc78c86e77d742237bc7da90d758" "9e7e1bd71ca102fcfc2646520bb2f25203544e7cc464a30c1cbd1385c65898f4" "70945ec5fa00eef126b82c5efb7dce1f567fc772388c879b748e3552f38889fc" "e6d871e13fd608b78aaff9d7f63e81aab1c901439eb0f432d6bb505ff1dda3cb" "f110ecd815cb48e75e275db68d06e7439a6f82429d1cf51382520cfb5652fc44" "e1fbe2e10e5acc07e82ea4244be29f5fd47e3123d1856b77db527d496ca25e63" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "23be8bd26bdcc5bd3abc6800870869a978cbc1cde0ea065a4838169d4cc707b3" "f23c2d947b426285828db5ec060784c2278ced8a96b9c5b1f40eed1d58428493" "fa942713c74b5ad27893e72ed8dccf791c9d39e5e7336e52d76e7125bfa51d4c" "022a515bd470f04d54018e9d34ca383193f6b20487610e83bd34e3bf060dafb3" "51542917f2503fcd17cc57278acf6135c87f9487ae4e7d763688d773fa8b341e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "c4e6fe8f5728a5d5fd0e92538f68c3b4e8b218bcfb5e07d8afff8731cc5f3df0" "454dc6f3a1e9e062f34c0f988bcef5d898146edc5df4aa666bf5c30bed2ada2e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(dabbrev-check-all-buffers nil)
 '(dabbrev-eliminate-newlines nil)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(eimp-mogrify-program "mogrify")
 '(emms-cache-file
	 (format "%s/%s/emms/cache" common-load-dir common-package-dir))
 '(emms-mode-line-format " %s ")
 '(emms-mode-line-icon-color "#657b83")
 '(emms-mode-line-icon-image-cache
	 (\`
		(image :type xpm :ascent center :data
					 (\,
						(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #657b83 \",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\"};")))))
 '(emms-mode-line-pause-icon-image-cache
	 (\`
		(image :type xpm :ascent center :data
					 (\,
						(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    11   11        2            1\",
/* colors */
\". c #657b83 \",
\"# c None s None\",
/* pixels */
\"###########\",
\"#...###...#\",
\"#...###...#\",
\"#...###...#\",
\"#...###...#\",
\"#...###...#\",
\"#...###...#\",
\"#...###...#\",
\"#...###...#\",
\"#...###...#\",
\"###########\"};")))))
 '(emms-mode-line-play-icon-image-cache
	 (\`
		(image :type xpm :ascent center :data
					 (\,
						(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #657b83 \",
\"# c None s None\",
/* pixels */
\"##############\",
\"#.############\",
\"#...##########\",
\"#.....########\",
\"#.......######\",
\"#........#####\",
\"#......#######\",
\"#....#########\",
\"#...##########\",
\"#.############\",
\"##############\"};")))))
 '(eshell-directory-name (format "%s/%s/eshell" common-load-dir common-package-dir))
 '(evil-emacs-state-cursor (quote ("#E57373" bar)))
 '(evil-insert-state-cursor (quote ("#E57373" hbar)))
 '(evil-normal-state-cursor (quote ("#FFEE58" box)))
 '(evil-visual-state-cursor (quote ("#C5E1A5" box)))
 '(eyebrowse-mode t)
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#393939")
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
		("log" "require" "spyOn" "jasmine" "expect" "beforeEach" "afterEach" "clearTimeout" "sinon" "describe" "it" "define" "setInterval" "setTimeout" "bbc" "beforeAll")))
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
 '(minimap-always-recenter nil)
 '(minimap-dedicated-window t)
 '(minimap-hide-fringes t)
 '(minimap-recenter-type (quote middle))
 '(minimap-update-delay 0)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(neo-auto-indent-point nil)
 '(neo-persist-show nil)
 '(neo-smart-open t)
 '(neo-theme (quote ascii))
 '(neo-window-width 35)
 '(org-agenda-entry-text-exclude-regexps (quote ("^.*<.*>.*" ".*When.*")))
 '(org-agenda-files nil)
 '(org-calendar-insert-diary-entry-key (quote i))
 '(org-support-shift-select t)
 '(package-user-dir (format "%s/%s/elpa" common-load-dir common-package-dir))
 '(popwin:popup-window-height 20)
 '(popwin:special-display-config
	 (quote
		(("*grunt" :regexp nil :position bottom :noselect t)
		 ("*Malabar Compilation*" :width 60 :position right :noselect t)
		 ("*MINIMAP*" :regexp t :position right :noselect t :dedicated t)
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
 '(powerline-color0 "#212526")
 '(powerline-color1 "#2d2d2d" t)
 '(powerline-color2 "#2e3436" t)
 '(powerline-foreground "#eeeeec")
 '(projectable-completion-func (quote car))
 '(projectable-filter-regexps
	 (quote
		("~$" "\\.o$" "\\.exe$" "\\.a$" "/\\.svn" "\\.elc$" "\\.output$" "\\.$" "#$" "\\.class$" "\\.png$" "\\.svn*" "\\/node_modules\\/*" "\\.gif$" "\\.gem$" "\\.pdf$" "\\.swp$" "\\.iml$" "\\.jar$" "\\/build\\/" "/\\.git" "\\/jsdoc\\/" "\\.min\\.js$" "\\.tags$" "\\.filecache" "\\.cache$" "\\/.git\\/" "report" "\\.gcov\\.html$" "\\.func.*\\.html$" "\\/tmp\\/")))
 '(rainbow-delimiters-highlight-braces-p t)
 '(rainbow-delimiters-highlight-brackets-p t)
 '(scroll-bar-mode nil)
 '(scss-output-directory "../")
 '(semantic-mode t)
 '(semanticdb-default-save-directory
	 (format "%s/%s/semanticdb" common-load-dir common-package-dir))
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
 '(showcss/display-buffer-mode "scss-mode")
 '(sml/show-time t)
 '(sp-hybrid-kill-excessive-whitespace t)
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

(defun cycle-font ()
  (interactive)
  (let* ((next-font-index
					(% (+ 1 (-find-index (lambda (it) (equal (face-attribute 'default :family) (car it))) font-list)) (length font-list)))
				 (next-font (nth next-font-index font-list)))
		(set-font next-font)
		(message (format "Setting Font to: %s" (car next-font)))))

(provide 'custom)
;;; custom.el ends here
