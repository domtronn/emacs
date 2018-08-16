;;; aquafresh-theme.el --- A modification of the twilight bright theme
;;
;; Copyright (c) 2018 Dom Charlesworth
;;
;; Version: 0.1.0
;; Keywords: themes
;; URL: https://github.com/jimeh/twilight-jazz-theme.el
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Code:


(defun color-join (r g b)
  "Build a color from R G B. Inverse of `color-values'."
  (format "#%02x%02x%02x" (ash r -8) (ash g -8) (ash b -8)))

(defun color-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'color-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun aquafresh-apply-custom-theme (theme-name)
  "`let' bind all colors used in `aquafresh-themes' for THEME-NAME."
  (let* ((light (eq 'aquafresh-morning theme-name))
         (background (if light "#fbfbfb" "#151e25"))
         (solaire-background (if light "#ffffff" "#1d252c"))
         (foreground (if light "#6d838e" "#d3e6f8"))
         (alpha (if light 0.1 0.1))
         (blend `(lambda (col &optional a) (color-blend col ,background (or a ,alpha))))
         (cursor (if light "#151e25" "#f33859"))

         (gray-1 "#8394a7") (gray-1bg (funcall blend gray-1))
         (gray-2 "#9baeb7") (gray-2bg (funcall blend gray-2 0.05))

         (gray-3 (if light "#9bb3bb" "#384551"))
         (gray-3bg (if light background (funcall blend gray-3)))

         (gray-4 "#d2d0d3")
         (gray-5 "#efefef")

         (red-1 "#f33859") (red-1bg (funcall blend red-1))
         (red-2 "#ff6b6a") (red-2bg (funcall blend red-2))

         (green-1 "#1fe19b") (green-1bg (funcall blend green-1)) ;; #38fab4
         (green-2 "#2bf05e") (green-2bg (funcall blend green-2))

         (orange-1 "#ff9f43") (orange-1bg (funcall blend orange-1))
         (orange-2 "#ff5728") (orange-2bg (funcall blend orange-2))

         (yellow-1 "#fecb56") (yellow-1bg (funcall blend yellow-1))
         (yellow-2 "#e37b40") (yellow-2bg (funcall blend yellow-2))

         (cyan-1 "#4ad9da") (cyan-1bg (funcall blend cyan-1))
         (cyan-2 "#03bde3") (cyan-2bg (funcall blend cyan-2))
         (cyan-3 "#41b6bd") (cyan-3bg (funcall blend cyan-3))

         (blue-1 "#5072ff") (blue-1bg (funcall blend blue-1))
         (blue-2 "#7055e8") (blue-2bg (funcall blend blue-2))

         (magenta-1 "#f105b3") (magenta-1bg (funcall blend magenta-1))
         (magenta-2 "#ff9ff4") (magenta-2bg (funcall blend magenta-2)))

    (custom-theme-set-faces
     theme-name

     ;; Basics
     `(default ((t (:height 120 :background ,background :foreground ,foreground :weight extralight :foundry "nil" :family "Roboto Mono Light" ))))
     `(solaire-default-face ((t (:height 120 :background ,solaire-background :weight extralight :family "Operator Mono"))))
     `(cursor ((t (:background ,cursor))))
     `(region ((t (:background ,blue-1bg :foreground ,blue-1))))
     `(highlight ((t (:foreground ,cyan-2 :background ,cyan-2bg))))
     `(highlight-indentation-face ((t (:background ,green-2))))
     `(highlight-indentation-current-column-face ((t (:background ,magenta-1))))
     `(indent-guide-face ((t (:foreground ,background))))
     `(hl-line ((t (:bold t))))
     `(minibuffer-prompt ((t (:foreground ,green-2bg :background ,green-2 :italic t))))
     `(escape-glyph ((t (:foreground ,magenta-1 :background , magenta-1bg))))

     ;; Font-lock stuff
     `(font-lock-builtin-face ((t (:foreground ,yellow-1 :background ,yellow-1bg))))
     `(font-lock-constant-face ((t (:foreground ,magenta-1))))
     `(font-lock-comment-face ((t (:foreground ,gray-3 :background ,gray-3bg :italic t))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,gray-3 :background ,gray-3bg :italic t :bold t))))
     `(font-lock-doc-face ((t (:foreground ,gray-2 :background ,gray-2bg :italic t))))
     `(font-lock-doc-string-face ((t (:foreground ,gray-3 :background ,gray-3bg))))
     `(font-lock-function-name-face ((t (:foreground ,red-1 :background ,red-1bg))))
     `(font-lock-keyword-face ((t (:foreground ,orange-1 :background ,orange-1bg :italic t))))
     `(font-lock-negation-char-face ((t (:foreground ,yellow-1 :background ,yellow-1bg))))
     `(font-lock-preprocessor-face ((t (:foreground ,orange-1 :background ,orange-1bg))))
     `(font-lock-string-face ((t (:foreground ,green-1 :background ,green-1bg :italic t))))
     `(font-lock-type-face ((t (:foreground ,red-2 :background ,red-2bg :bold nil))))
     `(font-lock-variable-name-face ((t (:foreground ,cyan-1 :background ,cyan-1bg))))
     `(font-lock-warning-face ((t (:foreground ,red-1 :background ,red-1bg))))

     ;; UI related
     `(link ((t (:foreground ,cyan-1 :background ,cyan-1bg))))
     `(ivy-action ((t (:foreground ,background :background ,orange-1))))
     `(ivy-virtual ((t (:foreground ,background :background ,orange-1))))
     `(ivy-prompt-match ((t (:foreground ,cyan-1 :background ,cyan-1bg))))
     `(ivy-current-match ((t (:foreground ,background :background ,cyan-2 :italic t))))

     `(neo-dir-link-face ((t (:foreground ,foreground :underline t :bold t))))

     ;; All the icons
     `(all-the-icons-red ((t (:foreground ,red-1))))
     `(all-the-icons-red-alt ((t (:foreground ,red-1))))
     `(all-the-icons-lred ((t (:foreground ,red-2))))
     `(all-the-icons-dred ((t (:foreground ,red-2))))

     `(all-the-icons-green ((t (:foreground ,green-1))))
     `(all-the-icons-lgreen ((t (:foreground ,green-2))))
     `(all-the-icons-dgreen ((t (:foreground ,green-2))))

     `(all-the-icons-blue ((t (:foreground ,blue-1))))
     `(all-the-icons-blue-alt ((t (:foreground ,blue-1))))
     `(all-the-icons-lblue ((t (:foreground ,blue-2))))
     `(all-the-icons-dblue ((t (:foreground ,blue-2))))

     `(all-the-icons-cyan ((t (:foreground ,cyan-1))))
     `(all-the-icons-cyan-alt ((t (:foreground ,cyan-1))))
     `(all-the-icons-lcyan ((t (:foreground ,cyan-2))))
     `(all-the-icons-dcyan ((t (:foreground ,cyan-2))))

     `(all-the-icons-yellow ((t (:foreground ,yellow-1))))
     `(all-the-icons-lyellow ((t (:foreground ,yellow-2))))
     `(all-the-icons-dyellow ((t (:foreground ,yellow-2))))

     `(all-the-icons-orange ((t (:foreground ,orange-1))))
     `(all-the-icons-lorange ((t (:foreground ,orange-2))))
     `(all-the-icons-dorange ((t (:foreground ,orange-2))))

     `(all-the-icons-maroon ((t (:foreground ,cyan-1))))
     `(all-the-icons-lmaroon ((t (:foreground ,cyan-2))))
     `(all-the-icons-dmaroon ((t (:foreground ,cyan-2))))

     `(all-the-icons-silver ((t (:foreground ,gray-1))))
     `(all-the-icons-lsilver ((t (:foreground ,gray-2))))
     `(all-the-icons-dsilver ((t (:foreground ,gray-2))))

     `(all-the-icons-pink ((t (:foreground ,magenta-1))))
     `(all-the-icons-lpink ((t (:foreground ,magenta-2))))
     `(all-the-icons-dpink ((t (:foreground ,magenta-2))))

     `(all-the-icons-purple ((t (:foreground ,magenta-1))))
     `(all-the-icons-lpurple ((t (:foreground ,magenta-2))))
     `(all-the-icons-dpurple ((t (:foreground ,magenta-2))))

     ;; Spaceline
     (if light
         `(spaceline-highlight-face ((t (:background ,cyan-2 :foreground ,cyan-2bg))))
         `(spaceline-highlight-face ((t (:background ,red-1 :foreground ,background)))))
     `(powerline-active2 ((t (:background ,gray-3bg :overline ,gray-1bg))))
     (if light
         `(powerline-active1 ((t (:background ,background :foreground ,foreground))))
         `(powerline-active1 ((t (:background ,background :foreground ,foreground)))))

     (if light
         `(powerline-inactive1 ((t (:background ,background :foreground ,cyan-2))))
         `(powerline-inactive1 ((t (:background ,background :foreground ,gray-2)))))
     `(powerline-inactive2 ((t (:background ,background :italic t :foreground ,cyan-2 :overline ,gray-1bg))))

     (if light
         `(mode-line ((t (:foreground ,foreground :background ,gray-2bg :box ,gray-2))))
         `(mode-line ((t (:foreground ,foreground :background ,gray-2bg :box ,gray-1bg)))))
     `(mode-line-inactive ((t (:foreground ,cyan-2 :background ,background :box ,gray-1bg))))
     `(vertical-border ((t (:background ,background :foreground ,gray-4))))

     ;; Elixir
     `(elixir-attribute-face ((t (:foreground ,magenta-2 :background ,magenta-2bg))))
     `(elixir-atom-face ((t (:foreground ,blue-1 :background ,blue-1bg))))


     ;; Linum
     `(linum ((t (:foreground ,gray-3 :background ,solaire-background :height 100))))
     `(nlinum ((t (:height 80 :foreground ,red-1bg))))
     `(nlinum-current-line ((t (:foreground ,foreground :bold t :background ,solaire-background :height 125))))
     `(fringe ((t (:background ,gray-3bg))))

     ;; show-paren-mode
     `(show-paren-match ((t (:foreground ,cyan-2 :background ,cyan-2bg))))
     `(show-paren-mismatch ((t (:background ,red-1 :foreground ,red-1bg))))

     ;; auto highlight symbol mode
     `(ahs-plugin-defalt-face ((t (:foreground ,green-1 :background ,green-1bg))))
     `(ahs-face ((t (:foreground ,green-1 :background ,gray-1bg :italic t :underline t))))
     `(ahs-definition-face ((t (:foreground ,green-1bg :background ,green-1 :bold t))))

     ;; neotree
     `(neo-banner-face ((t (:foreground ,magenta-2))))
     `(neo-header-face ((t (:foreground ,magenta-2))))
     `(neo-vc-edited-face ((t (:foreground ,orange-1))))
     `(neo-vc-added-face ((t (:foreground ,green-1))))
     `(neo-root-dir-face ((t (:foreground ,cyan-1))))
     `(neo-dir-link-face ((t (:foreground ,red-1 :bold nil :underline nil))))

     ;; treemacs
     `(treemacs-root-face ((t (:foreground ,cyan-2 :background ,cyan-2bg :height 140))))
     `(treemacs-root-face)
     `(treemacs-directory-face ((t (:foreground ,gray-2 :background ,gray-2bg))))
     `(treemacs-fringe-indicator-face ((t (:foreground ,green-1 :background ,green-1))))
     `(treemacs-term-node-face ((t (:foreground ,red-2 :background ,red-2bg))))
     `(treemacs-tags-face ((t (:foreground ,red-2 :background ,red-2bg))))
     `(treemacs-on-failure-pulse-face ((t (:foreground ,background :background ,gray-1))))
     `(treemacs-git-modified-face ((t (:foreground ,cyan-1 :background ,cyan-1bg))))
     `(treemacs-git-modified-face ((t (:foreground ,cyan-1 :background ,cyan-1bg))))
     `(treemacs-git-ignored-face ((t (:foreground ,gray-4 :italic t))))

     ;; ido
     `(ido-only-match ((t (:foreground ,green-1 :background ,green-1bg))))
     `(ido-subdir ((t (:foreground ,magenta-1 :background ,magenta-1bg))))

     ;; whitespace-mode
     `(whitespace-empty ((t (:foreground ,yellow-1bg :background ,yellow-1))))
     `(whitespace-hspace ((t (:foreground ,gray-2))))
     `(whitespace-indentation ((t (:foreground ,gray-2))))
     `(whitespace-line ((t (:background ,gray-2))))
     `(whitespace-newline ((t (:foreground ,gray-2))))
     `(whitespace-space ((t (:foreground ,gray-2))))
     `(whitespace-space-after-tab ((t (:foreground ,gray-2))))
     `(whitespace-tab ((t (:foreground ,gray-2))))
     `(whitespace-trailing ((t (:foreground ,red-1bg :background ,red-1))))

     ;; flyspell-mode
     `(flyspell-incorrect ((t (:underline ,red-1))))
     `(flyspell-duplicate ((t (:underline ,red-1))))

     ;; magit
     `(magit-diff-added ((t (:foreground ,green-1 :background ,green-1bg))))
     `(magit-diff-their ((t (:foreground ,green-1 :background ,green-1bg))))
     `(magit-diff-added-highlight ((t (:foreground ,green-1 :background ,green-1bg :bold t))))
     `(magit-diff-deleted ((t (:foreground ,red-2 :background ,red-2bg))))
     `(magit-diff-removed ((t (:foreground ,red-1 :background ,red-1bg))))
     `(magit-diff-removed-highlight ((t (:foreground ,red-2 :background ,red-2bg :bold t))))
     `(magit-item-highlight ((t (:background ,background))))

     `(ahs-add-overlay-face)

     `(magit-tag ((t (:background ,orange-1bg :foreground ,orange-1))))

     `(magit-section-heading ((t (:foreground ,magenta-1 :background ,magenta-1bg :bold t))))
     `(magit-branch-current ((t (:foreground ,cyan-1 :background ,background :box 2))))
     `(magit-branch-remote ((t (:foreground ,green-1 :background ,green-1bg :italic t))))
     `(magit-branch-remote-head ((t (:foreground ,green-1 :background ,background :box 2))))
     `(magit-branch-local ((t (:foreground ,cyan-1 :background ,cyan-1bg))))

     ;; Visual regexp
     `(vr/match-0 ((t (:underline t :foreground ,cyan-1 :bold t))))
     `(vr/match-1 ((t (:underline t :foreground ,cyan-2 :bold t))))

     `(vr/group-0 ((t (:foreground ,green-1 :background ,green-1bg :box t :underline nil :bold nil))))
     `(vr/group-1 ((t (:foreground ,green-2 :background ,green-2bg :box t :underline nil :bold nil))))
     `(vr/group-2 ((t (:foreground ,yellow-1 :background ,yellow-1bg :box t :underline nil :bold nil))))

     `(vr/match-separator-face ((t (:foreground ,foreground :bold t))))

     ;; Auto highlight face


     ;; customisation
     `(custom-state ((t (:background ,green-2bg :foreground ,green-2 :bold t))))
     `(custom-button ((t (:background ,gray-1bg :foreground ,gray-1 :box (:line-width 1 :style released-button)))))
     `(custom-button-mouse ((t (:background ,gray-3bg :foreground ,gray-3 :box (:line-width 1 :style released-button)))))
     `(custom-button-pressed ((t (:background ,gray-1bg :foreground ,gray-1 :box (:line-width 1 :style pressed-button)))))

     `(custom-changed ((t (:background ,cyan-2 :foreground ,cyan-2bg))))
     `(custom-link ((t (:background ,blue-1 :foreground ,blue-1bg))))
     `(custom-variable-tag ((t (:foreground ,magenta-1 :background ,magenta-1bg))))
     `(widget-field ((t (:background ,gray-3bg :box 1))))

     `(link ((t (:background ,blue-1bg :foreground ,blue-1))))

     ;; Dired stuff
     `(dired-directory ((t (:background ,orange-1bg :foreground ,orange-1))))
     `(dired-dir-name ((t (:background ,orange-1bg :foreground ,orange-1))))
     `(diredp-dir-priv ((t (:background ,orange-1 :foreground ,orange-1bg))))
     `(diredp-no-priv ((t (:background ,gray-3bg :foreground ,foreground))))
     `(diredp-read-priv ((t (:background ,green-2 :foreground ,foreground))))
     `(diredp-write-priv ((t (:background ,cyan-1 :foreground ,foreground))))
     `(diredp-exec-priv ((t (:background ,magenta-1 :foreground ,background))))
     `(diredp-dir-priv ((t (:foreground ,background :background ,orange-2))))

     `(diredp-dir-heading ((t (:background ,blue-1bg :foreground ,blue-1))))
     `(diredp-dir-name ((t (:foreground ,blue-1bg :background ,blue-1))))
     `(diredp-ignored-file-name ((t (:foreground ,green-2 :background ,green-2bg))))
     `(diredp-file-name ((t (:foreground ,magenta-2 :background ,magenta-2bg))))
     `(diredp-file-suffix ((t (:foreground ,magenta-2 :background ,background :bold t))))
     `(diredp-symlink ((t (:foreground ,magenta-2bg :background ,foreground))))

     `(diredp-date-time ((t (:foreground ,red-2 :weight bold :background ,red-2bg :italic t))))
     `(diredp-number ((t (:foreground ,orange-1 :background ,orange-1bg))))

     `(diredp-flag-mark ((t (:foreground ,green-1bg :background ,green-1))))
     `(diredp-flag-mark-line ((t (:foreground ,green-1 :background ,green-1bg))))

     `(magit-diff-hunk-heading-highlight ((t (:foreground ,magenta-1bg :background ,magenta-1))))
     ;; Ediff
     `(ediff-current-diff-B ((t (:background ,red-2bg :foreground ,red-2 :box 1))))
     `(ediff-current-diff-A ((t (:background ,green-2bg :foreground ,green-2 :box 1))))
     `(ediff-current-diff-C ((t (:background ,yellow-1 :foreground ,yellow-1bg))))

     `(ediff-fine-diff-A ((t (:background ,red-2 :foreground ,red-2bg))))
     `(ediff-fine-diff-B ((t (:background ,green-2 :foreground ,green-2bg))))

     `(ediff-even-diff-A ((t (:background ,gray-3bg :foreground ,gray-3))))
     `(ediff-even-diff-B ((t (:background ,gray-3bg :foreground ,gray-3))))
     `(ediff-odd-diff-A ((t (:background ,gray-1bg :foreground ,gray-1))))
     `(ediff-odd-diff-B ((t (:background ,gray-1bg :foreground ,gray-1))))

     ;; hydra
     `(hydra-face-cyan ((t (:foreground ,cyan-2 :background ,cyan-2bg :bold t))))
     `(hydra-face-pink ((t (:foreground ,magenta-1 :background ,magenta-1bg :bold t))))
     `(hydra-face-red ((t (:foreground ,red-1 :background ,red-1bg :bold t))))
     `(hydra-face-teal ((t (:foreground ,orange-1 :background ,orange-1bg :bold t))))

     ;; searching
     `(isearch ((t (:foreground ,magenta-1bg :background ,magenta-1))))
     `(isearch-fail ((t (:foreground ,red-2bg :background ,red-2))))
     `(lazy-highlight ((t (:foreground ,magenta-1 :background ,magenta-1bg))))
     `(anzu-mode-line ((t (:foreground ,cyan-1bg :background ,cyan-1))))

     `(rg-filename-face ((t (:foreground ,green-1 :background ,green-1bg))))
     `(rg-file-tag-face ((t (:foreground ,green-1bg :background ,green-1))))

     `(rg-match-face ((t (:foreground ,magenta-1 :background ,magenta-1bg))))
     `(rg-info-face ((t (:foreground ,magenta-1bg :background ,magenta-1))))

     `(wgrep-face ((t (:foreground ,blue-2 :background ,blue-2bg))))
     `(wgrep-done-face ((t (:foreground ,blue-2bg :background ,blue-2))))

     `(compilation-line-number ((t (:foreground ,cyan-2 :background ,cyan-2bg))))
     `(compilation-column-number ((t (:foreground ,cyan-1 :background ,cyan-1bg))))

     ;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((t (:bold t :foreground ,red-1))))
     `(rainbow-delimiters-depth-2-face ((t (:bold t :foreground ,cyan-1))))
     `(rainbow-delimiters-depth-3-face ((t (:bold t :foreground ,green-1))))
     `(rainbow-delimiters-depth-4-face ((t (:bold t :foreground ,red-1))))
     `(rainbow-delimiters-depth-5-face ((t (:bold t :foreground ,cyan-1))))
     `(rainbow-delimiters-depth-6-face ((t (:bold t :foreground ,green-1))))
     `(rainbow-delimiters-depth-7-face ((t (:bold t :foreground ,red-1))))
     `(rainbow-delimiters-depth-8-face ((t (:bold t :foreground ,cyan-1))))
     `(rainbow-delimiters-depth-9-face ((t (:bold t :foreground ,green-1))))

     ;; Avy
     `(avy-lead-face ((t (:background ,magenta-2 :foreground ,magenta-2bg))))
     `(avy-lead-face-0 ((t (:background ,green-1 :foreground ,green-1bg))))
     `(avy-lead-face-1 ((t (:background ,orange-1 :foreground ,orange-1bg))))

     ;; Javascript
     `(rjsx-attr ((t (:background ,cyan-1bg :bold nil :foreground ,cyan-1 :italic t))))
     `(rjsx-tag ((t (:background ,magenta-1bg :foreground ,magenta-1 ))))
     `(rjsx-text ((t (:background ,solaire-background :foreground ,foreground :italic t :inherit nil))))
     `(rjsx-tag-bracket-face ((t (:background ,magenta-1bg :bold t :foreground ,magenta-1 ))))

     `(show-paren-match ((t (:foreground ,background :background ,cyan-1))))
     `(js2-function-param ((t (:foreground ,blue-2 :background ,blue-2bg))))
     `(js2-function-call ((t (:foreground ,foreground))))
     `(js2-non-used ((t (:foreground ,red-1 :underline t :italic t))))

     `(js2-object-property ((t (:bold nil :italic t :foreground ,gray-1 :background ,gray-1bg :weight light ))))
     `(js2-object-property-access ((t (:bold nil :foreground ,gray-3 :background ,gray-3bg ))))

     ;; Clojure
     `(clojure-keyword-face ((t (:foreground ,magenta-1 :bold nil :background ,magenta-1bg))))
     `(cider-error-highlight-face ((t (:foreground ,red-1bg :background ,red-1))))
     `(cider-result-overlay-face ((t (:foreground ,green-1 :box 2 :bold t :background ,green-1bg))))

     ;; Eldoc
     `(eldoc-highlight-function-argument ((t (:italic t :bold t))))

     ;; MC
     `(mc/region-face ((t (:foreground ,blue-2 :background ,blue-2bg))))

     ;; Errors
     `(flycheck-error ((t (:underline (:color ,red-2) :background ,red-2bg :foreground ,red-2))))
     `(flycheck-warning ((t (:underline (:color ,yellow-1)))))

     ;; Git gutter
     `(git-gutter-fr+-added ((t (:foreground ,green-1 :background ,green-1))))
     `(git-gutter-fr+-deleted ((t (:foreground ,red-1 :background ,red-1))))
     `(git-gutter-fr+-modified ((t (:foreground ,orange-1 :background ,orange-1))))

     `(success ((t (:foreground ,green-1))))
     `(error ((t (:foreground ,red-1))))
     `(warning ((t (:foreground ,orange-1))))

     ;; highlight-indentation-mode
     `(highlight-indentation-face ((t (:background ,gray-1bg))))
     `(highlight-indentation-current-column-face ((t (:background ,gray-5))))

     ;; ECB
     `(ecb-default-general-face ((t (:foreground ,foreground :background ,gray-1bg))))
     `(ecb-default-highlight-face ((t (:foreground ,magenta-1 :background ,magenta-1bg))))
     `(ecb-method-face ((t (:foreground ,red-1 :background ,red-1bg))))
     `(ecb-tag-header-face ((t (:background ,cyan-2bg))))

     ;; org-mode
     `(org-date ((t (:foreground ,magenta-1 :background ,magenta-1bg))))
     `(org-done ((t (:foreground ,green-1 :background ,green-1bg))))
     `(org-hide ((t (:foreground ,gray-2 :background ,gray-1bg))))
     `(org-link ((t (:foreground ,cyan-1 :background ,cyan-1bg))))
     `(org-todo ((t (:foreground ,red-1 :background ,red-1bg))))

     ;; Company
     `(company-tooltip ((t (:foreground ,red-2 :background ,red-2bg))))
     `(company-tooltip-annotation ((t (:foreground ,orange-1))))
     `(company-tooltip-common ((t (:foreground ,red-2bg :background ,red-2 ))))
     `(company-tooltip-mouse ((t :background ,green-1 :foreground ,green-1bg)))
     `(company-tooltip-search ((t :background ,magenta-1 :foreground ,magenta-1bg)))
     `(company-tooltip-search-selection ((t :background ,green-1 :foreground ,green-1bg)))

     ;; Git commit
     `(git-commit-summary ((t :background ,green-1bg :foreground ,green-1)))
     `(git-commit-overlong-summary ((t :background ,red-1 :foreground ,red-1bg)))

     `(company-scrollbar-fg ((t :background ,green-1)))
     `(company-scrollbar-bg ((t :background ,green-1bg)))

     `(company-tooltip-selection ((t :background ,green-1bg :foreground ,green-1)))

     `(company-preview ((t (:foreground ,cyan-2 :background ,cyan-2bg))))
     `(company-preview-common ((t (:foreground ,orange-1 :background ,orange-1bg))))

     ;; Terminal
     `(term-color-black ((t :foreground ,foreground)))
     `(term-color-cyan ((t :foreground ,blue-2 )))
     `(term-color-cyan ((t :foreground ,cyan-1 )))
     `(term-color-green ((t (:foreground ,green-1))))
     `(term-color-magenta ((t :foreground ,magenta-2)))
     `(term-color-red ((t :foreground ,red-1 )))
     `(term-color-white ((t :foreground ,gray-1)))
     `(term-color-yellow ((t (:foreground ,yellow-1)))))

    (custom-theme-set-variables
     theme-name
     ;; ;; Fill Column Indicator mode
     (if light
         `(spaceline-all-the-icons-file-name-highlight ,foreground)
       `(spaceline-all-the-icons-file-name-highlight ,red-1))
     `(treemacs-icon-open-text ,(propertize "📂 " 'face 'treemacs-directory-face))
     `(treemacs-icon-closed-text ,(propertize "📁 " 'face 'treemacs-directory-face))
     `(treemacs-icon-tag-leaf-text ,(propertize "● " 'face 'treemacs-term-node-face))
     `(treemacs-icon-tag-node-open-text ,(propertize "💿 " 'face 'treemacs-tags-face))
     `(treemacs-icon-tag-node-closed-text ,(propertize "📀 " 'face 'treemacs-tags-face))
     `(treemacs-icon-text ,(propertize "📄 " 'face 'treemacs-file-face))
     `(fci-rule-color ,gray-2)
     `(fci-rule-character-color ,gray-2))))

(defun aquafresh-apply-themes ()
  "Apply both themes for `aquafresh-morning' & `aquafresh-midnight'."
  (interactive)
  (aquafresh-apply-custom-theme 'aquafresh-midnight)
  (aquafresh-apply-custom-theme 'aquafresh-morning))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'aquafresh-theme)

;;; aquafresh-theme.el ends here
