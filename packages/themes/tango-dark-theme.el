(deftheme tango-dark
  "Created 2014-09-11.")

(custom-theme-set-variables
 'tango-dark
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-candidate-limit 15)
 '(ac-expand-on-auto-complete t)
 '(ack-and-a-half-arguments (quote ("--color-match=green" "--color-file=yellow" "-w")))
 '(ack-and-a-half-executable "/usr/local/bin/ack")
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(backup-by-copying-when-mismatch nil)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-setup (quote one-frame))
 '(calendar-view-diary-initially-flag t)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".svn-base")))
 '(dabbrev-check-all-buffers nil)
 '(dabbrev-eliminate-newlines nil)
 '(emms-cache-file "~/.emacs.d/emms/cache")
 '(emms-mode-line-format " %s ")
 '(fci-rule-color "#393939")
 '(file-cache-find-command "find -L")
 '(powerline-color2 "#2e3436")
 '(powerline-color1 "#2d2d2d")
 '(powerline-color0 "#212526")
 '(powerline-foreground "#eeeeec")
 '(flycheck-highlighting-mode (quote lines))
 '(flycheck-jshintrc "/Users/charld13/.jshintrc")
 '(git-gutter-fr:side (quote left-fringe)))

(custom-theme-set-faces
 'tango-dark
 '(ac-candidate-face ((t (:background "khaki1" :foreground "grey14"))))
 '(ac-candidate-mouse-face ((t (:inherit nil :inverse-video nil))))
 '(ac-dabbrev-menu-face ((t (:background "#81a2be" :foreground "grey20"))))
 '(ac-dabbrev-selection-face ((t (:background "#6699cc" :foreground "grey20"))))
 '(ac-etags-candidate-face ((t (:background "#99cc99" :foreground "grey20"))))
 '(ac-etags-selection-face ((t (:background "#698b22" :foreground "grey20"))))
 '(ac-selection-face ((t (:background "goldenrod2" :foreground "grey20" :weight extra-bold))))
 '(anzu-mode-line ((t (:foreground "#223311" :weight bold))))
 '(emms-browser-album-face ((t (:foreground "#f0c674" :weight extra-light :height 1.1 :family "DejaVu Sans Mono"))))
 '(emms-browser-artist-face ((t (:foreground "#cc6666" :height 1.3 :family "DejaVu Sans Mono Extralight"))))
 '(emms-browser-track-face ((t (:foreground "#698b22" :height 1.0 :family "DejaVu Sans Mono"))))
 '(emms-playlist-track-face ((t (:foreground "#698b22"))))
 '(etags-select-highlight-tag-face ((t (:background "cadetblue4" :foreground "#2d2d2d" :weight bold))))
 '(flycheck-fringe-error ((t (:background "brown3" :foreground "khaki" :inverse-video nil :box (:line-width 1 :color "#cc3333" :style released-button)))))
 '(flycheck-fringe-info ((t (:inherit success :background "#99cc99" :foreground "#353836" :box (:line-width 1 :color "#55cc55" :style pressed-button)))))
 '(flymake-errline ((t (:background "#Db4444" :foreground "#333333" :underline nil))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#e9b96e"))))
 '(function-link ((t (:underline t :slant italic))))
 '(function-mouse-link ((t (:inherit function-link :foreground "#6699cc"))))
 '(git-gutter-fr:modified ((t (:inherit git-gutter:modified :foreground "tan1" :weight bold))))
 '(highlight-current-line-face ((t (:background "#333335"))))
 '(hs-face ((t nil)))
 '(ido-first-match ((((class color) (min-colors 89)) (:foreground "#f99157"))))
 '(ido-subdir ((((class color) (min-colors 89)) (:foreground "#cc99cc"))))
 '(inherit ((t (:inherit nil))))
 '(isearch ((((class color) (min-colors 89)) (:foreground "#eeeeec" :background "#ce5c00"))))
 '(js2-error ((t (:underline "#f2777a"))))
 '(js2-non-used ((t (:inherit nil :foreground "#9c9c9c" :slant italic))))
 '(js2-non-used-var ((t (:inherit nil :foreground "#9c9c9c" :slant italic :underline "#cc3333"))))
 '(js2-warning ((t (:underline "brown3"))))
 '(lazy-highlight ((((class color) (min-colors 89)) (:background "#8f5902"))))
 '(link ((((class color) (min-colors 89)) (:underline t :foreground "#729fcf"))))
 '(magit-section-title ((t (:inherit header-line :foreground "#002200" :weight bold))))
 '(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "#b4fa70"))))
 '(minimap-active-region-background ((t (:background "gray24"))))
 '(mode-line ((t (:background "#ffcc66" :foreground "#1F1611" :box nil))))
 '(mode-line-inactive ((t (:background "#212526" :foreground "#ffcc66" :box nil))))
 '(mouse-over ((t (:background "gray30"))))
 '(popup-scroll-bar-background-face ((t (:background "#cc6666"))))
 '(popup-scroll-bar-foreground-face ((t (:background "#cc3333"))))
 '(popup-tip-face ((t (:background "khaki1" :foreground "grey14"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#f2777a"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#f99157"))))
 '(rainbow-delimiters-depth-3-face ((((class color) (min-colors 89)) (:foreground "#ffcc66"))))
 '(rainbow-delimiters-depth-4-face ((((class color) (min-colors 89)) (:foreground "#99cc99"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "OliveDrab4"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#6699cc"))))
 '(rainbow-delimiters-depth-7-face ((((class color) (min-colors 89)) (:foreground "#66cccc"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#8444d8"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "DarkGoldenrod1"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "#ffcc66" :foreground "dark red" :inverse-video nil :weight extra-bold))))
 '(show-paren-match ((t (:inverse-video t))))
 '(cursor ((((class color) (min-colors 89)) (:background "#fce94f"))))
 '(fringe ((((class color) (min-colors 89)) (:foreground "#93a1a1" :background "#2e3436"))))
 '(linum ((((class color) (min-colors 89)) (:foreground "#ffcc66" :background "#2e3436"))))
 '(highlight ((((class color) (min-colors 89)) (:foreground "#2e3436" :background "#edd400"))))
 '(region ((((class color) (min-colors 89)) (:background "#555753"))))
 '(secondary-selection ((((class color) (min-colors 89)) (:background "#204a87"))))
 '(trailing-whitespace ((((class color) (min-colors 89)) (:background "#a40000"))))
 '(compilation-mode-line-fail ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(compilation-mode-line-run ((((class color) (min-colors 89)) (:foreground "#ce5c00"))))
 '(compilation-mode-line-exit ((((class color) (min-colors 89)) (:foreground "#4e9a06"))))
 '(escape-glyph ((((class color) (min-colors 89)) (:foreground "#c4a000"))))
 '(error ((((class color) (min-colors 89)) (:foreground "#ff4b4b"))))
 '(warning ((((class color) (min-colors 89)) (:foreground "#fcaf3e"))))
 '(success ((((class color) (min-colors 89)) (:foreground "#8ae234"))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#ad7fa8"))))
 '(font-lock-comment-face ((((class color) (min-colors 89)) (:foreground "#73d216"))))
 '(font-lock-comment-delimiter-face ((((class color) (min-colors 89)) (:foreground "#4DAC00"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "#e6a8df"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "#fce94f"))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "#b4fa70"))))
 '(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "#8cc4ff"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#fcaf3e"))))
 '(link-visited ((((class color) (min-colors 89)) (:underline t :foreground "#3465a4"))))
 '(gnus-group-news-1 ((((class color) (min-colors 89)) (:foreground "#ad7fa8"))))
 '(gnus-group-news-1-low ((((class color) (min-colors 89)) (:foreground "#75507b"))))
 '(gnus-group-news-2 ((((class color) (min-colors 89)) (:foreground "#729fcf"))))
 '(gnus-group-news-2-low ((((class color) (min-colors 89)) (:foreground "#3465a4"))))
 '(gnus-group-news-3 ((((class color) (min-colors 89)) (:foreground "#8ae234"))))
 '(gnus-group-news-3-low ((((class color) (min-colors 89)) (:foreground "#73d216"))))
 '(gnus-group-news-4 ((((class color) (min-colors 89)) (:foreground "#e6a8df"))))
 '(gnus-group-news-4-low ((((class color) (min-colors 89)) (:foreground "#c17d11"))))
 '(gnus-group-news-5 ((((class color) (min-colors 89)) (:foreground "#fcaf3e"))))
 '(gnus-group-news-5-low ((((class color) (min-colors 89)) (:foreground "#f57900"))))
 '(gnus-group-news-low ((((class color) (min-colors 89)) (:foreground "#edd400"))))
 '(gnus-group-mail-1 ((((class color) (min-colors 89)) (:foreground "#ad7fa8"))))
 '(gnus-group-mail-1-low ((((class color) (min-colors 89)) (:foreground "#75507b"))))
 '(gnus-group-mail-2 ((((class color) (min-colors 89)) (:foreground "#729fcf"))))
 '(gnus-group-mail-2-low ((((class color) (min-colors 89)) (:foreground "#3465a4"))))
 '(gnus-group-mail-3 ((((class color) (min-colors 89)) (:foreground "#8ae234"))))
 '(gnus-group-mail-3-low ((((class color) (min-colors 89)) (:foreground "#73d216"))))
 '(gnus-group-mail-low ((((class color) (min-colors 89)) (:foreground "#edd400"))))
 '(gnus-header-content ((((class color) (min-colors 89)) (:weight normal :foreground "#c4a000"))))
 '(gnus-header-from ((((class color) (min-colors 89)) (:foreground "#edd400"))))
 '(gnus-header-subject ((((class color) (min-colors 89)) (:foreground "#8ae234"))))
 '(gnus-header-name ((((class color) (min-colors 89)) (:foreground "#729fcf"))))
 '(gnus-header-newsgroups ((((class color) (min-colors 89)) (:foreground "#c17d11"))))
 '(message-header-name ((((class color) (min-colors 89)) (:foreground "#729fcf"))))
 '(message-header-cc ((((class color) (min-colors 89)) (:foreground "#c4a000"))))
 '(message-header-other ((((class color) (min-colors 89)) (:foreground "#c17d11"))))
 '(message-header-subject ((((class color) (min-colors 89)) (:foreground "#8ae234"))))
 '(message-header-to ((((class color) (min-colors 89)) (:foreground "#edd400"))))
 '(message-cited-text ((((class color) (min-colors 89)) (:foreground "#8ae234"))))
 '(message-separator ((((class color) (min-colors 89)) (:foreground "#ad7fa8"))))
 '(smerge-refined-change ((((class color) (min-colors 89)) (:background "#204a87"))))
 '(ediff-current-diff-A ((((class color) (min-colors 89)) (:background "#555753"))))
 '(ediff-fine-diff-A ((((class color) (min-colors 89)) (:background "#204a87"))))
 '(ediff-even-diff-A ((((class color) (min-colors 89)) (:background "#41423f"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 89)) (:background "#41423f"))))
 '(ediff-current-diff-B ((((class color) (min-colors 89)) (:background "#555753"))))
 '(ediff-fine-diff-B ((((class color) (min-colors 89)) (:background "#8f5902"))))
 '(ediff-even-diff-B ((((class color) (min-colors 89)) (:background "#41423f"))))
 '(ediff-odd-diff-B ((((class color) (min-colors 89)) (:background "#41423f"))))
 '(flyspell-duplicate ((((class color) (min-colors 89)) (:underline "#fcaf3e"))))
 '(flyspell-incorrect ((((class color) (min-colors 89)) (:underline "#ef2929"))))
 '(semantic-decoration-on-includes ((((class color) (min-colors 89)) (:underline "#888a85"))))
 '(semantic-decoration-on-private-members-face ((((class color) (min-colors 89)) (:background "#5c3566"))))
 '(semantic-decoration-on-protected-members-face ((((class color) (min-colors 89)) (:background "#8f5902"))))
 '(semantic-decoration-on-unknown-includes ((((class color) (min-colors 89)) (:background "#a40000"))))
 '(semantic-decoration-on-unparsed-includes ((((class color) (min-colors 89)) (:background "#41423f"))))
 '(semantic-tag-boundary-face ((((class color) (min-colors 89)) (:overline "#729fcf"))))
 '(semantic-unmatched-syntax-face ((((class color) (min-colors 89)) (:underline "#ef2929"))))
 '(highlight-current-line-face ((t (:background "#3B4143"))))
 '(default ((((class color) (min-colors 4096)) (:foreground "#eeeeec" :background "#2e3436")) (((class color) (min-colors 256)) (:foreground "#eeeeec" :background "#222")) (((class color) (min-colors 89)) (:foreground "#eeeeec" :background "black")))))

(provide-theme 'tango-dark)
