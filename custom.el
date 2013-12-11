;;------------------
;; Custom Variables
;;------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-mark-diary-entries-flag t)
 '(calendar-setup (quote one-frame))
 '(calendar-view-diary-initially-flag t)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".svn-base")))
 '(file-cache-filter-regexps (quote ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" ",v$" "\\.output$" "\\.$" "#$" "\\.class$" "\\/test.*\\.js$" "\\.png$" "\\.svn*" "\\.svn-base$")))
 '(flycheck-highlighting-mode (quote lines))
 '(ibuffer-formats (quote ((mark modified read-only " " (name 30 30 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(ido-use-filename-at-point (quote guess))
 '(jshint-mode-jshintrc "~/.jshintrc")
 '(org-agenda-entry-text-exclude-regexps (quote ("^.*<.*>.*" ".*When.*")))
 '(org-agenda-files (quote ("/Users/charld13/Documents/Calendar.org")))
 '(org-calendar-insert-diary-entry-key (quote i))
 '(rainbow-delimiters-highlight-braces-p t)
 '(rainbow-delimiters-highlight-brackets-p t)
 '(shell-pop-autocd-to-working-dir t)
 '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-height 40)
 '(shell-pop-window-position "bottom")
 '(sml/show-time t)
 '(sml/time-format " %H:%M:%S")
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:background "brown3" :foreground "khaki" :slant italic))))
 '(flycheck-warning ((t (:background "DarkGoldenrod1" :foreground "gray17" :slant italic))))
 '(hs-face ((t nil)))
 '(ido-first-match ((t (:foreground "DodgerBlue3"))))
 '(ido-subdir ((t (:foreground "chocolate2"))))
 '(js2-warning ((t (:underline (:color "orange" :style wave)))))
 '(minibuffer-prompt ((t (:foreground "light green"))))
 '(mode-line ((t (:background "gray18" :foreground "whitesmoke" :box (:line-width 2 :color "gray18" :style released-button)))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "firebrick"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "chocolate"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "gold"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "olivedrab"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "lightseagreen"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "steelblue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "slateblue"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "darkorchid4"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#00000"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "magenta4"))))
 '(show-paren-match ((t (:foreground "gray18" :background "gold1" :weight extra-bold)))))
