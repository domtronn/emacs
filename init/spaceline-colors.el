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


(defvar theme-spaceline-color-alist
  '((whiteboard     ((active . ((highlight . "#bbbbbb") "#d7d7d7" "#2a2a2a")) (secondary . ("#bbbbbb" "#d7d7d7" "#2a2a2a"))))
    (atom-one-dark  ((active . ("#3E4451" "#5C6370" "#ABB2BF")) (secondary . ("#3E4451" "#5C6370" "#ABB2BF"))))
    (darktooth      ((active . ("#222222" "#222222" "#FDF3C3")) (secondary . ("#403935" "#403935" "#988975"))))
    (niflheim       ((active . ("#222222" "#2a2a2a" "#bababa")) (secondary . ("#222222" "#2a2a2a" "#bababa"))))
    (aurora         ((active . ("#455a64" "#2B3B40" "#CDD3D3")) (secondary . ("#232A2F" "#232A2F" "#556D79"))))
    (forest-blue    ((active . ("#0e5994" "#203439" "#d3cbc4")) (secondary . ("#203439" "#203439" "#203439"))))
    (eink           ((active . ("#DDDDD8" "#DDDDD8" "#383838")) (secondary . ("#DDDDD8" "#DDDDD8" "#DDDDD8"))))
    (ujelly         ((active . ("#000000" "#000000" "#ffffff")) (secondary . ("#000000" "#000000" "#ffffff"))))
    (spacemacs-dark ((active . ("#6c3163" "#292B2E" "#b2b2b2")) (secondary . ("#292B2E" "#292B2E" "#292B2E"))))
    (solarized-dark ((active . ("#657b83" "#073642" "#073642")) (secondary . ("#002b36" "#002b36" "#586e75"))))
    (gruvbox        ((active . ("#3c3836" "#282828" "#f4e8ba")) (secondary . ("#504945" "#282828" "#a89984"))))
    (material       ((active . ("#1c1f26" "#1c1f26" "#ffffff")) (secondary . ("#1c1f26" "#1c1f26" "#a7adba"))))
    (monokai        ((active . ("#363731" "#272822" "#E5DDB7")) (secondary . ("#272822" "#272822" "#75715E"))))
    (darkokai       ((active . ("#ab7eff" "#242728" "#3D4345")) (secondary . ("#242728" "#242728" "#5D6365"))))
    (suscolors      ((active . ("#5faf5f" "#262626" "#262626")) (secondary . ("#262626" "#262626" "#949494"))))
    (creamsody      ((active . ("#1D2021" "#282C32" "#EBDBB2")) (secondary . ("#504945" "#282C32" "#A89984"))))
    (wombat         ((active . ("#444444" "#343434" "#CCC9C0")) (secondary . ("#444444" "#343434" "#99968b"))))))

(defvar spaceline--theme-colors-alist
  '((gruvbox        ((active   . ((highlight . ((background . "#83a598") (foreground . "#1d2021")))
                                  (default   . ((background . "#3c3836") (foreground . "#f4e8ba")))
                                  (other     . ((background . "#1d2021") (foreground . "#928374")))
                                  (middle    . ((background . "#282828")))))

                     (inactive . ((default   . ((background . "#1d2021") (foreground . "#a89984")))
                                  (other     . ((background . "#1d2021") (foreground . "#a89984")))
                                  (middle    . ((background . "#282828")))))))

    (creamsody      ((active   . ((highlight . ((background . "#529F96") (foreground . "#1d2021")))
                                  (default   . ((background . "#DFE5C5") (foreground . "#1d2021")))
                                  (other     . ((background . "#282C32") (foreground . "#FDF4C1")))
                                  (middle    . ((background . "#282C32")))))

                     (inactive . ((default   . ((background . "#1d2021") (foreground . "#a89984")))
                                  (other     . ((background . "#1d2021") (foreground . "#a89984")))
                                  (middle    . ((background . "#282C32")))))))
    ))

(defun spaceline--set-face (face alist)
  "Set FACE to be the foreground & background defined in ALIST."
  (let-alist alist (set-face-attribute face nil :foreground .foreground :background .background)))

(defun spaceline-update-faces (&rest args)
  "Update the faces for the current theme from `custom-enabled-themes'.
ARGS is needed to allow for this function to be used as advice"
  (let ((theme-alist (cadr (assoc (car custom-enabled-themes) spaceline--theme-colors-alist))))
    (when theme-alist
      (let-alist theme-alist
        (spaceline--set-face 'spaceline-highlight-face  .active.highlight)
        (spaceline--set-face 'powerline-active2         .active.middle)
        (spaceline--set-face 'mode-line                 .active.other)
        (spaceline--set-face 'powerline-active1         .active.default)

        (spaceline--set-face 'powerline-inactive1       .inactive.default)
        (spaceline--set-face 'mode-line-inactive        .inactive.other)
        (spaceline--set-face 'powerline-inactive2       .inactive.middle)))))

(provide 'spaceline-colors)
;;; spaceline-colors.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
