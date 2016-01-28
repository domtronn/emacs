;;; mode-icons.el --- XPM Icons for the mode line

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

(defun mode-icon-coffeescript-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char * coffeescript[] = {
/* columns rows colors chars-per-pixel */
\"22 22 4 1 \",
\"  c %s\",
\". c %s\",
\"X c %s\",
\"o c %s\",
\"oooooooooooooooooooooo\",
\"oooooooooooooooooooooo\",
\"oooooooooooooXoooooooo\",
\"oooooo.XooX  ...Xooooo\",
\"oooooo.XX  XXXo. ooooo\",
\"o. ooooXXXooX..Xoooo  \",
\"oo.  .XXXoooXoXXX.. .o\",
\"oXXoXX... . ......oXoX\",
\"o.  .XXooooooooooX.  .\",
\"oo        .          o\",
\"oX                   o\",
\"o                   .o\",
\"  o                 oo\",
\" XoX               Xoo\",
\"  oo              .ooo\",
\". XoX             oooo\",
\"o  Xo.           .oooo\",
\"ooXXXo           ooooo\",
\"ooooooX         .ooooo\",
\"oooooooXX..X ..ooooooo\",
\"oooooooooooooooooooooo\",
\"oooooooooooooooooooooo\"
};"
					 c1
					 (mode-icon-color-blend (color-values c1) (color-values c2) 0.66)
					 (mode-icon-color-blend (color-values c1) (color-values c2) 0.33)
					 c2)
	 'xpm t :height 2 :ascent 'center))
(defun mode-icon-javascript-xpm (c1 c2)
	(create-image
	 (format "/* XPM */
static char *js[] = {
/* columns rows colors chars-per-pixel */
\"22 22 3 1 \",
\". c %s\",
\"X c %s\",
\"  c %s\",
\"                      \",
\"                      \",
\"                      \",
\"   XXXXXXXXXXXXXXXXXXX\",
\"   XXXXXXXXXXXXXXXXXXX\",
\"   XXXXXXXXXXXXXXXXXXX\",
\"   XXXXXXX..XXX....XXX\",
\"   XXXXXXX..XX......XX\",
\"   XXXXXXX..X...XX..XX\",
\"   XXXXXXX..X...XXXXXX\",
\"   XXXXXXX..XX....XXXX\",
\"   XXXXXXX..XXX....XXX\",
\"   XXXXXXX..XXXXX...XX\",
\"   XXXXXXX..X..XXX..XX\",
\"   XXX..X...X...X...XX\",
\"   XXX.....XXX.....XXX\",
\"   XXXX...XXXXX...XXXX\",
\"   XXXXXXXXXXXXXXXXXXX\",
\"   XXXXXXXXXXXXXXXXXXX\",
\"                      \",
\"                      \",
\"                      \"};"
					 c2 c1 c2)
	 'xpm t :height 2 :ascent 'center))
(defun mode-icon-sass-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char *sass_png[] = {
/* columns rows colors chars-per-pixel */
\"22 17 3 1 \",
\". c %s\",
\"x c %s\",
\"  c %s\",
/* pixels */
\"     x.....x          \",
\"    x........x        \",
\"   ....     ..        \",
\"  ...       ..        \",
\" ..        x..        \",
\"...       x..         \",
\"..   .xx....          \",
\"..   ...... .  ..     \",
\"...      .  ..x..     \",
\" ...   ... ......  x  \",
\"  ... .........x.....x\",
\"   x........x.....  x.\",
\"  ...............    .\",
\" ..  ..........x      \",
\"x.. .. x.x  x         \",
\"...x..                \",
\".....                 \"};"
					 c1
					 (mode-icon-color-blend
						(color-values c1)
						(color-values c2) 0.4)
					 c2)
	 'xpm t :height 2 :ascent 'center))
(defun mode-icon-binbash-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char *binbash_png[] = {
/* columns rows colors chars-per-pixel */
\"22 22 3 1 \",
\"X c %s\",
\"  c %s\",
\"+ c %s\",
/* pixels */
\"++++++++++++++++++++++\",
\"++++++++++++++++++++++\",
\"++++++++++++++++++++++\",
\"++++++++++++++++++++++\",
\"++                  ++\",
\"++                  ++\",
\"++    XX  XX    XX  ++\",
\"++    XX  XX    XX  ++\",
\"++  XXXXXXXXXX  XX  ++\",
\"++  XXXXXXXXXX  XX  ++\",
\"++    XX  XX    XX  ++\",
\"++    XX  XX    XX  ++\",
\"++  XXXXXXXXXX      ++\",
\"++  XXXXXXXXXX      ++\",
\"++    XX  XX    XX  ++\",
\"++    XX  XX    XX  ++\",
\"++                  ++\",
\"++                  ++\",
\"++++++++++++++++++++++\",
\"++++++++++++++++++++++\",
\"++++++++++++++++++++++\",
\"++++++++++++++++++++++\"}; "
					 c2 c1 c2)
	 'xpm t :height 2 :ascent 'center))
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
(defun mode-icon-cpp-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char *c___png[] = {
/* columns rows colors chars-per-pixel */
\"22 22 3 1 \",
\"o c %s\",
\"  c %s\",
\". c %s\",
\"......................\",
\"........ooooo.........\",
\".......oooooooo.......\",
\".....oooooooooooo.....\",
\"...oooooooooooooooo...\",
\"..oooooo......oooooo..\",
\".oooooo........ooooo .\",
\".ooooo..........oo   .\",
\".oooo....ooooo..     .\",
\".oooo...oooooo       .\",
\".oooo...ooo   ..  .. .\",
\".oooo...o    .........\",
\".oooo...      ..  .. .\",
\".oooo...             .\",
\".oooo...     ....    .\",
\".ooo  ....  ....     .\",
\".o     ........      .\",
\"..      ......      ..\",
\"...               ....\",
\".....           ......\",
\".......        .......\",
\"......................\",
\"......................\"};"
					 c1
					 (mode-icon-color-blend
						(color-values c1)
						(color-values c2) 0.4)
					 c2)
	 'xpm t :height 2 :ascent 'center))

(defun mode-icon-c-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char *c_png[] = {
/* columns rows colors chars-per-pixel */
\"22 24 4 1 \",
\". c %s\",
\"X c %s\",
\"O c %s\",
\"  c %s\",
\"                      \",
\"                      \",
\"         OOOO         \",
\"       OOOOOOOO       \",
\"      OOOOOOOOOOO     \",
\"    OOOOOOOOOOOOOO    \",
\"  OOOOOOO    OOOOOOO  \",
\" XXOOOO        OOOOXO \",
\" XXXOO          OXXX  \",
\" XXXO    OOOO  OXXXXO \",
\" XXXX   OOOOOOOXXXXO  \",
\" XXXX  XXOOOXXXXXOO   \",
\" XXXX  XXX..XXXXXO    \",
\" XXXX  X......XXXXXO  \",
\" XXXX   OO..oO OXXXXO \",
\" XXXX     OO    OXXXO \",
\" .....         ...... \",
\"  ......      ......  \",
\"   .......  .......   \",
\"     ............     \",
\"      .........       \",
\"       .......        \",
\"         ...          \",
\"                      \" }; "
					 c1
					 (mode-icon-color-blend
						(color-values c1)
						(color-values c2) 0.8)
					 (mode-icon-color-blend
						(color-values c1)
						(color-values c2) 0.6)
					 c2)
	 'xpm t :height 2 :ascent 'center))

(defun mode-icon-python-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char *python_png[] = {
/* columns rows colors chars-per-pixel */
\"22 22 3 1 \",
\"o c %s\",
\"+ c %s\",
\"  c %s\",
/* pixels */
\"                      \",
\"                      \",
\"       ++++++         \", 
\"      +++++++++       \", 
\"      +  ++++++       \", 
\"      +  ++++++       \", 
\"      +++++++++       \", 
\"           ++++ oo    \", 
\"  +++++++++++++ ooo   \", 
\" +++++++++++++  oooo  \", 
\" ++++++        ooooo  \", 
\" +++++  oooooooooooo  \", 
\" ++++ oooooooooooooo  \", 
\"  +++ ooooooooooooo   \", 
\"   ++ ooooooooooo     \", 
\"      oooo            \", 
\"      ooooooooo       \", 
\"      ooooo  oo       \", 
\"      ooooo  o        \", 
\"       oooooo         \",
\"                      \",
\"                      \"}"
					 c1
					 (mode-icon-color-blend
						(color-values c1)
						(color-values c2) 0.6)
					 c2)
	 'xpm t :height 2 :ascent 'center))

(defun mode-icon-ruby-xpm (c1 c2)
  (create-image
	 (format "/* XPM */
static char *ruby2_png[] = {
/* columns rows colors chars-per-pixel */
\"22 22 3 1 \",
\"_ c %s\",
\"+ c %s\",
\"  c %s\",
/* pixels */
\"                      \",
\"                      \",
\"                      \",
\"            +++++++   \",
\"           +______+   \",
\"          +++____++   \",
\"         +++++__+++   \",
\"        +++++++++++   \",
\"       +______+++++   \",
\"      ++_____+_++++   \",
\"     +++____+__++++   \",
\"    ++++___+____+++   \",
\"   +++++__+_____+++   \",
\"  +_++++_+_______++   \",
\"  +__++++________++   \",
\"  +____++++_______+   \",
\"  +___+++++++_____+   \",
\"  +__+++++++++++___   \",
\"  ++++++++++++++++_   \",
\"                      \",
\"                      \",
\"                      \"};"
					 (mode-icon-color-blend
						(color-values c1);; 
						(color-values c2) 0.5)
					 c1
					 c2)
	 'xpm t :height 2 :ascent 'center))

(defcustom mode-icons
  '((emacs-lisp-mode "emacs")
    (js2-mode "javascript")
		(scss-mode "sass"))
  "Icons for major and minor modes to map to function id names."

  :type '(repeat
          (list (string :tag "Regular Expression")
                (choice
                 (string :tag "Icon Name")
                 (const :tag "Suppress" nil))
                (choice
                 (const :tag "text" nil)
                 (const :tag "png" png)
                 (const :tag "gif" gif)
                 (const :tag "jpeg" jpeg)
                 (const :tag "xbm" xbm)
                 (const :tag "xpm" xpm))))
  :group 'mode-icons)

(provide 'mode-icons)

;;; mode-icons.el ends here
