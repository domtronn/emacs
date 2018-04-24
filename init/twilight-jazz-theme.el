;;; twilight-jazz-theme.el --- A modification of the twilight bright theme
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

(deftheme twilight-jazz
  "A soothing dark-on-light theme.")

(let ((background "#FFFFFF")
      (foreground "#505050")
      (selection "#c7e1f2")
      (hl-line "#f5f5f5")
      (cursor "#b4b4b4")

      (gray-1 "#888487") (gray-1bg "#f3f3f3")
      (gray-2 "#d9d9d9")
      (gray-3 "#cac2c0") (gray-3bg "#f9f9f8")
      (gray-4 "#d2d0d3")
      (gray-5 "#efefef")
      (red-1 "#f33859") (red-1bg "#ffecef")
      (red-2 "#ed5276") (red-2bg "#fee6eb")
      (brown-1 "#e37b40") (brown-1bg "#fcece3")
      (orange-1 "#f6a502") (orange-1bg "#fef6ea")
      (yellow-1 "#f1ca4d") (yellow-1bg "#fefaee")
      (cream-1 "#f7eac8") (cream-1bg "#fefdfa")
      (green-1 "#1fe19b") (green-1bg "#eefff8") ;; #38fab4
      (green-2 "#b1eb04") (green-2bg "#f7fdeb")
      (blue-1 "#4ad9da") (blue-1bg "#effbfb")
      (blue-2 "#35b1bf") (blue-2bg "#ebf6f7")
      (purple-3 "#ff85cb") (purple-3bg "#fff3f9")
      (purple-1 "#f105b3") (purple-1bg "#ffe9f8")
      (purple-2 "#7055e8") (purple-2bg "#f0eefd")
      )

  (custom-theme-set-faces
   'twilight-jazz

   ;; Basics
   `(default ((t (:background ,background :foreground ,foreground))))
   `(cursor ((t (:background ,cursor))))
   `(region ((t (:background ,blue-2bg :foreground ,blue-2))))
   `(highlight ((t (:foreground ,blue-2 :background ,blue-2bg))))
   `(hl-line ((t (:background ,hl-line))))
   `(minibuffer-prompt ((t (:foreground ,yellow-1bg :background ,yellow-1 :italic t))))
   `(escape-glyph ((t (:foreground ,purple-1 :background , purple-1bg))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((t (:foreground ,yellow-1 :background ,yellow-1bg))))
   `(font-lock-constant-face ((t (:foreground ,purple-1 :bold t))))
   `(font-lock-comment-face ((t (:foreground ,gray-3 :background ,gray-3bg :italic t))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,gray-3 :background ,gray-3bg :italic t :bold t))))
   `(font-lock-doc-face ((t (:foreground ,gray-1 :background ,gray-1bg))))
   `(font-lock-doc-string-face ((t (:foreground ,gray-1 :background ,gray-1bg))))
   `(font-lock-function-name-face ((t (:foreground ,red-1 :background ,red-1bg))))
   `(font-lock-keyword-face ((t (:foreground ,orange-1 :background ,orange-1bg))))
   `(font-lock-negation-char-face ((t (:foreground ,yellow-1 :background ,yellow-1bg))))
   `(font-lock-preprocessor-face ((t (:foreground ,orange-1 :background ,orange-1bg))))
   `(font-lock-string-face ((t (:foreground ,green-1 :background ,green-1bg))))
   `(font-lock-type-face ((t (:foreground ,red-2 :background ,red-2bg :bold nil))))
   `(font-lock-variable-name-face ((t (:foreground ,blue-1 :background ,blue-1bg))))
   `(font-lock-warning-face ((t (:foreground ,red-1 :background ,red-1bg))))

   ;; UI related
   `(link ((t (:foreground ,blue-1 :background ,blue-1bg))))
   `(ivy-action ((t (:foreground ,background :background ,orange-1))))
   `(ivy-virtual ((t (:foreground ,background :background ,orange-1))))
   `(ivy-prompt-match ((t (:foreground ,blue-1 :background ,blue-1bg))))
   `(ivy-current-match ((t (:foreground ,background :background ,red-1 :italic t))))

   `(mode-line ((t (:foreground ,blue-2 :background ,blue-2bg))))
   `(mode-line-inactive ((t (:foreground ,gray-1 :background ,gray-3bg))))
   `(vertical-border ((t (:background ,background :foreground ,gray-4))))

   ;; Linum
   `(linum ((t (:foreground ,gray-3 :background ,gray-3bg :bold t))))
   `(nlinum-current-line ((t (:foreground ,gray-3bg :background ,gray-3 :bold t :height 0.8))))
   `(fringe ((t (:background ,gray-3bg))))
   
   ;; show-paren-mode
   `(show-paren-match ((t (:foreground ,blue-2 :background ,blue-2bg))))
   `(show-paren-mismatch ((t (:background ,red-1 :foreground ,red-1bg))))

   ;; ido
   `(ido-only-match ((t (:foreground ,green-1 :background ,green-1bg))))
   `(ido-subdir ((t (:foreground ,purple-1 :background ,purple-1bg))))

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
   `(magit-diff-add ((t (:foreground ,green-1 :background ,green-1bg))))
   `(magit-diff-added-highlight ((t (:foreground ,green-1 :background ,green-1bg :bold t))))
   `(magit-diff-del ((t (:foreground ,red-2 :background ,red-2bg))))
   `(magit-diff-removed ((t (:foreground ,red-1 :background ,red-1bg))))
   `(magit-diff-removed-highlight ((t (:foreground ,red-2 :background ,red-2bg :bold t))))
   `(magit-item-highlight ((t (:background ,gray-1bg))))

   `(magit-tag ((t (:background ,orange-1bg :foreground ,orange-1))))
   
   `(magit-section-heading ((t (:foreground ,purple-1 :background ,purple-1bg :bold t))))
   `(magit-branch-current ((t (:foreground ,blue-1 :background ,background :box 2))))
   `(magit-branch-remote ((t (:foreground ,green-1 :background ,green-1bg :italic t))))
   `(magit-branch-remote-head ((t (:foreground ,green-1 :background ,background :box 2))))
   `(magit-branch-local ((t (:foreground ,blue-1 :background ,blue-1bg))))
   
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,purple-1bg :background ,purple-1))))

   ;; searching
   `(isearch ((t (:foreground ,purple-1bg :background ,purple-1))))
   `(isearch-fail ((t (:foreground ,red-2bg :background ,red-2))))
   `(lazy-highlight ((t (:foreground ,purple-1 :background ,purple-1bg))))
   `(anzu-mode-line ((t (:foreground ,blue-1bg :background ,blue-1))))
   
   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:bold t :foreground ,red-1))))
   `(rainbow-delimiters-depth-2-face ((t (:bold t :foreground ,blue-1))))
   `(rainbow-delimiters-depth-3-face ((t (:bold t :foreground ,green-1))))
   `(rainbow-delimiters-depth-4-face ((t (:bold t :foreground ,red-1))))
   `(rainbow-delimiters-depth-5-face ((t (:bold t :foreground ,blue-1))))
   `(rainbow-delimiters-depth-6-face ((t (:bold t :foreground ,green-1))))
   `(rainbow-delimiters-depth-7-face ((t (:bold t :foreground ,red-1))))
   `(rainbow-delimiters-depth-8-face ((t (:bold t :foreground ,blue-1))))
   `(rainbow-delimiters-depth-9-face ((t (:bold t :foreground ,green-1))))
   
   ;; Javascript
   `(rjsx-attr ((t (:background ,blue-1bg :bold nil :foreground ,blue-1 ))))
   `(rjsx-tag ((t (:background ,purple-1bg :foreground ,purple-1 ))))
   `(rjsx-tag-bracket-face ((t (:background ,purple-1bg :bold t :foreground ,purple-1 ))))

   `(show-paren-match ((t (:foreground ,background :background ,blue-1))))
   `(js2-function-param ((t (:foreground ,purple-2 :background ,purple-2bg))))
   
   `(js2-object-property ((t (:bold nil :italic t ))))
   `(js2-object-property-access ((t (:bold nil :foreground ,gray-3 :background ,gray-3bg ))))

   ;; Clojure
   `(clojure-keyword-face ((t (:foreground ,purple-1 :bold t))))
   
   ;; Errors
   `(flycheck-error ((t (:underline (:color ,red-2)))))
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
   `(ecb-default-highlight-face ((t (:foreground ,purple-1 :background ,purple-1bg))))
   `(ecb-method-face ((t (:foreground ,red-1 :background ,red-1bg))))
   `(ecb-tag-header-face ((t (:background ,blue-2bg))))

   ;; org-mode
   `(org-date ((t (:foreground ,purple-1 :background ,purple-1bg))))
   `(org-done ((t (:foreground ,green-1 :background ,green-1bg))))
   `(org-hide ((t (:foreground ,gray-2 :background ,gray-1bg))))
   `(org-link ((t (:foreground ,blue-1 :background ,blue-1bg))))
   `(org-todo ((t (:foreground ,red-1 :background ,red-1bg))))
   
   ;; Company
   `(company-tooltip ((t (:foreground ,red-2 :background ,red-2bg))))
   `(company-tooltip-annotation ((t (:foreground ,orange-1))))
   `(company-tooltip-common ((t (:foreground ,red-2bg :background ,red-2 ))))
   `(company-tooltip-mouse ((t :background ,green-1 :foreground ,green-1bg)))
   `(company-tooltip-search ((t :background ,purple-1 :foreground ,purple-1bg)))
   `(company-tooltip-search-selection ((t :background ,green-1 :foreground ,green-1bg)))
   
   
   `(company-scrollbar-fg ((t :background ,green-1)))
   `(company-scrollbar-bg ((t :background ,green-1bg)))
   
   `(company-tooltip-selection ((t :background ,green-1bg :foreground ,green-1)))
   
   `(company-preview ((t (:foreground ,blue-2 :background ,blue-2bg))))
   `(company-preview-common ((t (:foreground ,orange-1 :background ,orange-1bg))))

   
   
   ;; Terminal
   `(term-color-black ((t :foreground ,foreground)))
   `(term-color-blue ((t :foreground ,purple-2 )))
   `(term-color-cyan ((t :foreground ,blue-1 )))
   `(term-color-green ((t (:foreground ,green-1))))
   `(term-color-magenta ((t :foreground ,purple-1)))
   `(term-color-red ((t :foreground ,red-1 )))
   `(term-color-white ((t :foreground ,gray-1)))
   `(term-color-yellow ((t (:foreground ,yellow-1))))
   )

  (custom-theme-set-variables
   'twilight-jazz

   ;; ;; Fill Column Indicator mode
   `(fci-rule-color ,gray-2)
   `(fci-rule-character-color ,gray-2)
   
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'twilight-jazz)

;;; twilight-jazz-theme.el ends here
