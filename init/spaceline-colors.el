;;; spaceline-colors.el --- Color theming for custom ATI spaceline

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(defun spaceline--set-face (face alist)
  "Set FACE to be the foreground & background defined in ALIST."
  (let-alist alist (set-face-attribute face nil :foreground .foreground :background .background)))

(defvar spaceline--colors-alist
  '((gruvbox
    ((active (highlight (background . "#ff8700") (foreground . "#1d2021"))
             (default   (background . "#3c3836") (foreground . "#f4e8ba"))
             (other     (background . "#1d2021") (foreground . "#928374"))
             (middle    (background . "#282828")))
     (inactive (default (background . "#1d2021") (foreground . "#a89984"))
               (other   (background . "#1d2021") (foreground . "#a89984"))
               (middle  (background . "#282828")))))
    (gruvbox-dark-soft
    ((active (highlight (background . "#ff8700") (foreground . "#1d2021"))
             (default   (background . "#3c3836") (foreground . "#f4e8ba"))
             (other     (background . "#1d2021") (foreground . "#928374"))
             (middle    (background . "#282828")))
     (inactive (default (background . "#1d2021") (foreground . "#a89984"))
               (other   (background . "#1d2021") (foreground . "#a89984"))
               (middle  (background . "#282828")))))
    (gruvbox-dark-hard
    ((active (highlight (background . "#b8bb26") (foreground . "#1d2021"))
             (default   (background . "#3c3836") (foreground . "#f4e8ba"))
             (other     (background . "#282828") (foreground . "#928374"))
             (middle    (background . "#1d2021")))
     (inactive (default (background . "#1d2021") (foreground . "#a89984"))
               (other   (background . "#1d2021") (foreground . "#a89984"))
               (middle  (background . "#282828")))))
    (subatomic
     ((active (highlight (background . "#f9b529") (foreground . "#1d2021")))))

    (creamsody
    ((active (highlight (background . "#83A598") (foreground . "#1d2021"))
             (default   (background . "#DFE5C5") (foreground . "#1d2021"))
             (other     (background . "#282C32") (foreground . "#FDF4C1"))
             (middle    (background . "#282C32")))
     (inactive (default (background . "#1d2021") (foreground . "#a89984"))
               (other   (background . "#1d2021") (foreground . "#a89984"))
               (middle  (background . "#282C32")))
     (buffer-highlight . "#1d2021")))

    (kaolin-galaxy
     ((buffer-highlight . "#967bb6")))

    (atom-one-dark
     ((active (highlight (background . "#98C379") (foreground . "#1d2021"))
              (default   (background . "#3E4451") (foreground . "#AAAAAA"))
              (other     (background . "#282C32") (foreground . "#AAAAAA"))
              (middle    (background . "#282C32")))
      (inactive (default (background . "#1d2021") (foreground . "#666D7A"))
                (other   (background . "#1d2021") (foreground . "#666D7A"))
                (middle  (background . "#282C32")))))

    (doom-spacegrey
     ((active (highlight (background . "#5699AF") (foreground . "#1d2021"))
              (default   (background . "#343D46") (foreground . "#5699AF"))
              (other     (background . "#282C32") (foreground . "#c0c5ce"))
              (middle    (background . "#282C32")))
      (inactive (default (background . "#282C32") (foreground . "#73797e"))
                (other   (background . "#282C32") (foreground . "#73797e"))
                (middle  (background . "#282C32")))
      (buffer-highlight . "#1d2021")))

   (forest-blue
    ((active (highlight (background . "#fc5a7b") (foreground . "#1d2021"))
             (default   (background . "#f42f56") (foreground . "#232a2f"))
             (other     (background . "#203439") (foreground . "#bfb8b1"))
             (middle    (background . "#203439")))
     (inactive (default (background . "#253c41") (foreground . "#506064"))
               (other   (background . "#253c41") (foreground . "#506064"))
               (middle  (background . "#203439")))))

   (darktooth
    ((active (highlight (background . "#8EC07C") (foreground . "#282828"))
             (default   (background . "#506E59") (foreground . "#8EC07C"))
             (other     (background . "#32302F") (foreground . "#FDF4C1"))
             (middle    (background . "#282828")))
     (inactive (default (background . "#506E59") (foreground . "#87AF87"))
               (other   (background . "#506E59") (foreground . "#87AF87"))
               (middle  (background . "#282828")))))

   (peacock
    ((active (highlight (background . "#FF5D38") (foreground . "#262626"))
             (default   (background . "#3e3c38") (foreground . "#E0E4CC"))
             (middle    (background . "#2B2A27")))
     (inactive (default (background . "#3e3c38") (foreground . "#524e48"))
               (other   (background . "#3e3c38") (foreground . "#524e48"))
               (middle  (background . "#3e3c38")))))

   (tangotango
    ((active (highlight (background . "#edd400") (foreground . "#262626"))
             (other     (background . "#252b2b") (foreground . "#bbbbbc"))
             (default   (background . "#222222") (foreground . "#eeeeec"))
             (middle    (background . "#252b2b")))
     (inactive (default (background . "#2e3434") (foreground . "#eeeeec"))
               (other   (background . "#2e3434") (foreground . "#eeeeec"))
               (middle  (background . "#2e3434")))))

   (zenburn
    ((active (other     (background . "#494949") (foreground . "#DCDCCC"))
             (default   (background . "#383838") (foreground . "#DCDCCC"))
             (highlight (background . "#AFD8AF") (foreground . "#383838")))
     (inactive (default (background . "#383838") (foreground . "#1b1b1b"))
               (middle  (background . "#383838")))))

   (dracula
    ((active (other     (background . "#373844") (foreground . "#DCDCCC"))
             (default   (background . "#44475a") (foreground . "#f8f8f2"))
             (highlight (background . "#50fa7b") (foreground . "#373844"))
             (middle    (background . "#282a36")))
     (inactive (default (background . "#373844") (foreground . "#565761"))
               (middle  (background . "#373844")))))

   (spacemacs-light
    ((active (highlight (background . "#9380b2") (foreground . "#edf2e9"))
             (other     (background . "#e3dedd") (foreground . "#655370"))
             (middle    (background . "#efeae9")))
     (inactive (default (background . "#efeae9") (foreground . "#a094a2"))
               (other   (background . "#efeae9") (foreground . "#a094a2"))
               (middle  (background . "#efeae9")))))

   (kaolin
    ((highlight . "#91b9c7")
     (active (highlight (background . "#353535") (foreground . "#91b9c7"))
             (default   (background . "#282828") (foreground . "#9a9f9b"))
             (other     (background . "#282828") (foreground . "#9a9f9b"))
             (middle    (background . "#282828")))
     (inactive (default (background . "#202020") (foreground . "#30555a"))
               (other   (background . "#202020") (foreground . "#30555a"))
               (middle  (background . "#202020")))))

   (twilight-bright
    ((active
      (highlight (background . "#f1f4f8") (foreground . "#6b82a7"))
      (default   (background . "#f1f4f8") (foreground . "#6b82a7"))
      (other     (background . "#f1f4f8") (foreground . "#6b82a7"))
      (middle    (background . "#f1f4f8")))

     (inactive
      (default (background . "#f7f7f7") (foreground . "#b4b4b4"))
      (other   (background . "#f7f7f7") (foreground . "#b4b4b4"))
      (middle  (background . "#f7f7f7") (foreground . "#b4b4b4")))
     (highlight . "#6b82a7")))

   (twilight-jazz
    ((active
      ;; (highlight (background . "#e3f4ff") (foreground . "#417598"))
      ;; (default   (background . "#e3f4ff") (foreground . "#417598"))
      ;; (other     (background . "#e3f4ff") (foreground . "#417598"))
      ;; (middle    (background . "#e3f4ff"))
      ;; (highlight (background . "#f1f4f8") (foreground . "#6b82a7"))
      ;; (default   (background . "#f1f4f8") (foreground . "#6b82a7"))
      ;; (other     (background . "#f1f4f8") (foreground . "#6b82a7"))
      ;; (middle    (background . "#f1f4f8")))
      (highlight (background . "#f33859") (foreground . "#ffffff"))
      (default   (background . "#4ad9da") (foreground . "#effbfb"))
      (other     (background . "#ccf4f4") (foreground . "#4ad9da"))
      (middle    (background . "#f5f6f4")))
     (inactive
      (default (background . "#38fab4") (foreground . "#fff"))
      (other   (background . "#38fab4") (foreground . "#fff"))
      (middle  (background . "#eefff8") (foreground . "#b4b4b4")))
     (highlight . "#ffffff")))

   (aquafresh-midnight
    ((active
      (highlight (foreground . "#f33859") (background . "#563A46"))
      (default   (foreground . "#4ad9da") (background . "#354850"))
      (other     (foreground . "#f1ca4d") (background . "#676145"))
      (middle    (background . "#354850")))
     (inactive
      (default (background . "#31494A") (foreground . "#38fab4"))
      (other   (background . "#31494A") (foreground . "#38fab4"))
      (middle  (background . "#354850") (foreground . "#b4b4b4")))
     (highlight . "#4ad9da")))

   (nord
    ((active (highlight (background . "#E5E9F0") (foreground . "#2E3440")))
     (highlight . "#88C0D0")))

   (tao-yang
    ((active (highlight (background . "#252525") (foreground . "#E8E8E8"))
             (default   (background . "#252525") (foreground . "#E8E8E8"))
             (middle    (background . "#252525") (foreground . "#E8E8E8"))
             (other     (background . "#252525") (foreground . "#E8E8E8")))
     (inactive (default (background . "#DADADA") (foreground . "#616161"))
               (other   (background . "#DADADA") (foreground . "#616161"))
               (middle  (background . "#DADADA")))
     (highlight . "#E8E8E8")))
   ))

(defun spaceline-update-faces (&rest args)
  "Update the faces for the current theme from `custom-enabled-themes'.
ARGS is needed to allow for this function to be used as advice"
  (let* ((theme-alist (cadr (assoc (car custom-enabled-themes) spaceline--colors-alist))))
    (when theme-alist
      (let-alist theme-alist
        (when .active.highlight (spaceline--set-face 'spaceline-highlight-face  .active.highlight))
        (when .active.middle    (spaceline--set-face 'powerline-active2         .active.middle))
        (when .active.other     (spaceline--set-face 'mode-line                 .active.other))
        (when .active.default   (spaceline--set-face 'powerline-active1         .active.default))

        (when .inactive.default (spaceline--set-face 'powerline-inactive1       .inactive.default))
        (when .inactive.other   (spaceline--set-face 'mode-line-inactive        .inactive.other))
        (when .inactive.middle  (spaceline--set-face 'powerline-inactive2       .inactive.middle))

        (setq spaceline-all-the-icons-file-name-highlight (if .highlight .highlight nil))))))

(provide 'spaceline-colors)
;;; spaceline-colors.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
