;;; mode-icons.el --- XPM Icons for the mode line

(require 'dash)

(defun mode-icon-color-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'mode-icon-color-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))

(defun mode-icon-color-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun mode-icon-emacs-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char *elisp_png[] = {
/* columns rows colors chars-per-pixel */
\"22 22 3 1 \",
\"+ c %s\",
\". c %s\",
\"  c %s\",
\"                      \",
\"                      \",
\"         ....         \",
\"       .........      \",
\"     ............     \",
\"    .........++...    \",
\"    ...++++++++...    \",
\"   .....++.........   \",
\"  .......++........   \",
\"  ........++.......   \",
\"  .....++++++.......  \",
\"  ...++++++.........  \",
\"  ...++++...........  \",
\"   ...++...........   \",
\"   ....+++.........   \",
\"    .....++++++...    \",
\"    .......++++...    \",
\"     ............     \",
\"       .........      \",
\"         .....        \",
\"                      \",
\"                      \"};"
					 c2
					 c1
					 ;; (mode-icon-color-blend
					 ;; 	(color-values c1)
					 ;; 	(color-values c2) 0.4)
					 c2)
	 'xpm t :height 2 :ascent 'center))

(defun mode-icon-markdown-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char *markdown_png[] = {
/* columns rows colors chars-per-pixel */
\"22 14 3 1 \",
\"  c %s\",
\". c %s\",
\"@ c %s\",
/* pixels */
\"@@@@@@@@@@@@@@@@@@@@@@\",
\".                    .\",
\"                      \",
\"   @@@   @@@   @@@    \",
\"   @@@@ @@@@   @@@    \",
\"   @@@@@@@@@   @@@    \",
\"   @@@@@@@@@ @@@@@@@  \",
\"   @@@@@@@@@ @@@@@@@  \",
\"   @@@@@@@@@  @@@@@   \",
\"   @@@ @ @@@   @@@    \",
\"   @@@   @@@    @     \",
\"                      \",
\".                    .\",
\"@@@@@@@@@@@@@@@@@@@@@@\" }; "
					 c1
					 (mode-icon-color-blend
						(color-values c1)
						(color-values c2) 0.4)
					 c2)
	 'xpm t :height 2 :ascent 'center))

(defun mode-icon-org-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char *org_mode_png[] = {
/* columns rows colors chars-per-pixel */
\"22 22 3 1 \",
\". c %s\",
\"o c %s\",
\"  c %s\",
\"                      \",
\"                     .\",
\"                    . \",
\"                  ..  \",
\"      oooo      ...   \",
\"     oooooo.oooo..    \",
\"    ooooo....oo.      \",
\"   ooooo....  ..      \",
\"   ooooo....  ..      \",
\"  ooooooo.........    \",
\"  oooooooo..........  \",
\"  ooooo.o...........  \",
\"  oooooo...     ...   \",
\"   oo.ooo..           \",
\"    o......           \",
\"     o......          \",
\"       .....          \",
\"         ...          \",
\"          ..          \",
\"           .          \",
\"                      \",
\"                      \"}; "
					 c1
					 (mode-icon-color-blend
						(color-values c1)
						(color-values c2) 0.4)
					 c2)
	 'xpm t :height 2 :ascent 'center))


(defvar mode-icon-alist 
  '(
    (c-mode        devicon-icon "\xe639")
    (c++-mode      devicon-icon "\xE635")
    (c-sharp-mode  devicon-icon "\xe637")
    (coffee-mode   devicon-icon "\xe66a")
    (css-mode      devicon-icon "\xe679")
    (docker-mode   devicon-icon "\xe61f")
    (erlang-mode   devicon-icon "\xe617")
    (web-mode      devicon-icon "\xe7f7")
    (html-mode     devicon-icon "\xe7f7")
    (magit-mode    devicon-icon "\xe7a8")
    
    (java-mode     devicon-icon "\xe842")
    (ruby-mode     devicon-icon "\xebca")
    (mysql-mode    devicon-icon "\xeb61")
    (php-mode      devicon-icon "\xeb71")
    (scss-mode     devicon-icon "\xebcb")

    (js-mode         js-icon)
    (js2-mode        js-icon)
    (javascript-mode js-icon)

    (emacs-lisp-mode  mode-icon-xpm "emacs")
    (markdown-mode    mode-icon-xpm "markdown")
    (org-mode         mode-icon-xpm "org")
    
    (haskell-mode  mfizz-icon  "\xf129")
    (clojure-mode  mfizz-icon  "\xf10b")
    (elixir-mode   mfizz-icon  "\xf116")
    (nginx-mode    mfizz-icon  "\xf143")
    (perl-mode     mfizz-icon  "\xf14b")
    (play-mode     mfizz-icon  "\xf151")
    (python-mode   mfizz-icon  "\xf156")
    (scala-mode    mfizz-icon  "\xf15e")
    (bash-mode     mfizz-icon  "\xf160")
    (shell-mode    mfizz-icon  "\xf162")
    (spring-mode   mfizz-icon  "\xf165")))

(defun mode-icon-insert-all ()
  (--map (apply (cadr it) (cddr it)) mode-icon-alist))

(defun mode-icon-for-mode (mode)
  (let ((mode-icon (assoc mode mode-icon-alist)))
    (when mode-icon
      (propertize (apply (cadr mode-icon) (cddr mode-icon))
                  'face `(:background ,(powerline-c1) :foreground ,(powerline-fg))
                  'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
                  'local-map (let ((map (make-sparse-keymap)))
                               (define-key map [mode-line down-mouse-1]
                                 `(menu-item ,(purecopy "Menu Bar") ignore
                                             :filter (lambda (_) (mouse-menu-major-mode-map))))
                               (define-key map [mode-line mouse-2] 'describe-mode)
                               (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                               map)))))

(defun mode-icon-xpm (mode)
  (propertize " " 'display
    (funcall (intern (format "mode-icon-%s-xpm" mode)) (powerline-fg) (powerline-c1))
    'face `(:background ,(powerline-c1))
    'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
    'local-map (let ((map (make-sparse-keymap)))
                 (define-key map [mode-line down-mouse-1]
                   `(menu-item ,(purecopy "Menu Bar") ignore
                               :filter (lambda (_) (mouse-menu-major-mode-map))))
                 (define-key map [mode-line mouse-2] 'describe-mode)
                 (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                 map)))

(defun mode-icon ()
  (mode-icon-for-mode major-mode))

(defun js-icon ()
  "Get the appropriate JavaScript Icon"
  (let* ((file-name (file-name-nondirectory (or (buffer-file-name) "")))
         (gulp? (s-contains? "gulpfile" file-name t))
         (grunt? (s-contains? "gruntfile" file-name t))
         (node? (when (buffer-file-name)
                  (locate-dominating-file (buffer-file-name) ".node-version"))))
    (cond
     (gulp? (devicon-icon "\xe7ec"))
     (grunt? (mfizz-icon  "\xf127"))
     ;; (node? (devicon-icon "\xeb6a"))
     (node? (devicon-icon "\xe845"))
     (t (devicon-icon     "\xe845")))))

(defun mfizz-icon (icon) (propertize icon 'face '(:family "mfizz" :weight extra-light) 'display '(height 1.0)))
(defun devicon-icon (icon) (propertize icon 'face '(:family "devicon" :weight extra-light) 'display '(height 1.0)))

(provide 'mode-icons)

;;; mode-icons.el ends here
