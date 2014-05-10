;;------------------
;; Custom Variables
;;------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#c5c8c6" "#cc6666" "#698b22" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#2e2e2e"))
 '(calendar-mark-diary-entries-flag t)
 '(calendar-setup (quote one-frame))
 '(calendar-view-diary-initially-flag t)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".svn-base")))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(fci-rule-color "#282a2e")
 '(file-cache-filter-regexps (quote ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" ",v$" "\\.output$" "\\.$" "#$" "\\.class$" "\\/test.*\\.js$" "\\.png$" "\\.svn*" "\\.svn-base$" "\\/node_modules\\/" "\\/\\." "\\.gif$" "\\.gem$" "\\.pdf$" "\\.iml$" "\\.jar$" "\\/script-test[s]\\/")))
 '(file-cache-find-command "find -L")
 '(flycheck-highlighting-mode (quote lines))
 '(flycheck-jshintrc "/Users/charld13/.jshintrc")
 '(flymake-gui-warnings-enabled t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 30 30 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(ido-use-filename-at-point (quote guess))
 '(jshint-mode-jshintrc "~/.jshintrc")
 '(jshint-mode-mode "jslint")
 '(org-agenda-entry-text-exclude-regexps (quote ("^.*<.*>.*" ".*When.*")))
 '(org-agenda-files (quote ("/Users/charld13/Documents/Calendar.org")))
 '(org-calendar-insert-diary-entry-key (quote i))
 '(rainbow-delimiters-highlight-braces-p t)
 '(rainbow-delimiters-highlight-brackets-p t)
 '(shell-pop-autocd-to-working-dir nil)
 '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-height 40)
 '(shell-pop-window-position "bottom")
 '(sml/show-time t)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#cc6666") (40 . "#de935f") (60 . "#f0c674") (80 . "#698b22") (100 . "#8abeb7") (120 . "#81a2be") (140 . "#b294bb") (160 . "#cc6666") (180 . "#de935f") (200 . "#f0c674") (220 . "#698b22") (240 . "#8abeb7") (260 . "#81a2be") (280 . "#b294bb") (300 . "#cc6666") (320 . "#de935f") (340 . "#f0c674") (360 . "#698b22"))))
 '(vc-annotate-very-old-color nil)
 '(vc-svn-program "/usr/local/bin/svn"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hs-face ((t nil)))
 '(ido-first-match ((((class color) (min-colors 89)) (:foreground "#f99157"))))
 '(ido-subdir ((((class color) (min-colors 89)) (:foreground "#cc99cc"))))
 '(js2-error ((t (:background "#f2777a" :underline "#f2777a"))))
 '(js2-warning ((t (:underline (:color "#f99157" :style wave)))))
 '(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(mode-line ((t (:background "gray18" :box (:line-width 1 :color "burlywood1" :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#393939" :foreground "#999999" :box nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#f2777a"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#f99157"))))
 '(rainbow-delimiters-depth-3-face ((((class color) (min-colors 89)) (:foreground "#ffcc66"))))
 '(rainbow-delimiters-depth-4-face ((((class color) (min-colors 89)) (:foreground "#99cc99"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "OliveDrab4"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#6699cc"))))
 '(rainbow-delimiters-depth-7-face ((((class color) (min-colors 89)) (:foreground "#66cccc"))))
 '(rainbow-delimiters-depth-8-face ((((class color) (min-colors 89)) (:foreground "#ffcc66"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "DarkGoldenrod1"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#f2777a"))))
 '(show-paren-match ((t (:foreground "gray18" :background "gold1" :weight extra-bold)))))
