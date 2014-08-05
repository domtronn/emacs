(deftheme dgc-dark
  "Created 2014-05-07.")

(custom-theme-set-variables
 'dgc-dark
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".svn-base")))
 '(file-cache-filter-regexps (quote ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" ",v$" "\\.output$" "\\.$" "#$" "\\.class$" "\\/test.*\\.js$" "\\.png$" "\\.svn*" "\\.svn-base$")))
 '(ido-use-filename-at-point (quote guess))
 '(rainbow-delimiters-highlight-braces-p t)
 '(rainbow-delimiters-highlight-brackets-p t)
 '(shell-pop-autocd-to-working-dir t)
 '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-height 40)
 '(shell-pop-window-position "bottom")
 '(sml/show-time t)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(custom-safe-themes (quote ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(sml/time-format " %H:%M"))

(custom-theme-set-faces
 'dgc-dark
 '(flycheck-error ((t (:background "brown3" :foreground "khaki" :slant italic))))
 '(flycheck-warning ((t (:background "DarkGoldenrod1" :foreground "gray17" :slant italic))))
 '(hs-face ((t nil)))
 '(ido-first-match ((((class color) (min-colors 89)) (:foreground "#f99157"))))
 '(ido-subdir ((((class color) (min-colors 89)) (:foreground "#cc99cc"))))
 '(js2-warning ((((class color) (min-colors 89)) (:underline "#f99157"))))
 '(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
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

(set-face-foreground (quote font-lock-comment-face) "dimgray")
(set-face-foreground (quote font-lock-comment-delimiter-face) "gray25")

(provide-theme 'dgc-dark)
