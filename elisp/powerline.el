;;; powerline.el --- fancy statusline

;; Name: Emacs Powerline
;; Author: Nicolas Rougier and Chen Yuan
;; Version: 1.2
;; Keywords: statusline
;; Repository: https://github.com/yuanotes/powerline (not maintained)
;; Alternative: https://github.com/milkypostman/powerline by Donald Curtis

;;; Commentary:

;; This package simply provides a minor mode for fancifying the status line.

;;; Changelog:

;; v1.0 - Nicolas Rougier posted to wiki (http://www.emacswiki.org/emacs/PowerLine)
;; v1.1 - Guard clause around the powerline output, so that if
;;        powerline tries to output something unexpected, it won't
;;        just fail and flail-barf.  (JonathanArkell)
;; v1.2 - Fixed the Guard Clause to not just sit there and message like mad
;;        When a list is encountered, it is interpreted as a mode line. Fixes
;;        problems with shell mode and nXhtml mode.

;;; Code:

(defvar powerline-color1 "#383838")
(defvar powerline-color2 "#666666")
(defvar powerline-fg "#bababa")
(defvar powerline-color-alist '(("#383838" 1) ("#666666" 1)))

(defvar theme-powerline-color-alist
  '((whiteboard ("#bbbbbb" "#d7d7d7" "#2a2a2a"))
    (atom-one-dark ("#3E4451" "#5C6370" "#ABB2BF"))
    (gotham ("#10272D" "#081E26" "#357C91"))
    (ujelly ("#000000" "#000000" "#ffffff"))
    (moe-light ("#CCCCB7" "#EDEDD3" "#3F3F38"))
    (aurora ("#455a64" "#2B3B40" "#CDD3D3"))
    (misterioso ("#455a64" "#2B3B40" "#CDD3D3"))
    (jazz ("#151515" "#101010" "#7F6A4F"))))

(defun update-powerline (&rest args)
  "Update the extra powerline colours based on a mapping to theme."
  (let* ((theme (car custom-enabled-themes))
         (alist (cadr (assoc theme theme-powerline-color-alist))))
    (if alist
        (setq powerline-color1 (car alist)
              powerline-color2 (cadr alist)
              powerline-fg (caddr alist)
              powerline-color-alist alist)
      (setq powerline-fg "white"))))

(defun arrow-left-xpm
  (color1 color2)
  "Return an XPM left arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"12 22 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\"..........  \",
\"........... \",
\"........... \",
\"..........  \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow-right-xpm
  (color1 color2)
  "Return an XPM right arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"12 22 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"  ..........\",
\" ...........\",
\" ...........\",
\"  ..........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-right-xpm
  (color1 color2)
  "Return an XPM right curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"         ...\",
\"         ...\",
\"       .....\",
\"       .....\",
\"       .....\",
\"      ......\",
\"      ......\",
\"      ......\",
\"      ......\",
\"      ......\",
\"      ......\",
\"       .....\",
\"       .....\",
\"       .....\",
\"         ...\",
\"         ...\",
\"           .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-left-xpm
  (color1 color2)
  "Return an XPM left curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"...         \",
\"...         \",
\".....       \",
\".....       \",
\".....       \",
\"......      \",
\"......      \",
\"......      \",
\"......      \",
\"......      \",
\"......      \",
\".....       \",
\".....       \",
\".....       \",
\"...         \",
\"...         \",
\".           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun make-xpm
  (name color1 color2 data)
  "Return an XPM image for lol data"
  (create-image
   (concat
    (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
            (downcase (replace-regexp-in-string " " "_" name))
            (length (car data))
            (length data)
            (if color1 color1 "None")
            (if color2 color2 "None"))
    (let ((len  (length data))
          (idx  0))
      (apply 'concat
             (mapcar #'(lambda (dl)
                        (setq idx (+ idx 1))
                        (concat
                         "\""
                         (concat
                          (mapcar #'(lambda (d)
                                     (if (eq d 0)
                                         (string-to-char " ")
                                       (string-to-char ".")))
                                  dl))
                         (if (eq idx len)
                             "\"};"
                           "\",\n")))
                     data))))
   'xpm t :ascent 'center))

(defun half-xpm
  (color1 color2)
  (make-xpm "half" color1 color2
            (make-list 18
                       (append (make-list 6 0)
                               (make-list 6 1)))))

(defun percent-xpm
  (pmax pmin we ws width color1 color2)
  (let* ((fs   (if (eq pmin ws)
                   0
                 (round (* 17 (/ (float ws) (float pmax))))))
         (fe   (if (eq pmax we)
                   17
                 (round (* 17 (/ (float we) (float pmax))))))
         (o    nil)
         (i    0))
    (while (< i 18)
      (setq o (cons
               (if (and (<= fs i)
                        (<= i fe))
                   (append (list 0) (make-list width 1) (list 0))
                 (append (list 0) (make-list width 0) (list 0)))
               o))
      (setq i (+ i 1)))
    (make-xpm "percent" color1 color2 (reverse o))))


;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of the given function."
  (let ((table-sym (gensym))
  (val-sym (gensym))
  (args-sym (gensym)))
    (set table-sym (make-hash-table :test 'equal))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let ((,val-sym (gethash ,args-sym ,table-sym)))
   (if ,val-sym
       ,val-sym
     (puthash ,args-sym (apply ,func ,args-sym) ,table-sym))))))

(memoize 'arrow-left-xpm)
(memoize 'arrow-right-xpm)
(memoize 'curve-left-xpm)
(memoize 'curve-right-xpm)
(memoize 'half-xpm)
(memoize 'percent-xpm)

(defvar powerline-minor-modes nil)
(defvar powerline-arrow-shape 'arrow)
(defun powerline-make-face
  (bg &optional fg)
  (if bg
      (let ((cface (intern (concat "powerline-"
                                   bg
                                   "-"
                                   (if fg
                                       (format "%s" fg)
                                     "white")))))
        (make-face cface)2
        (if fg
            (if (eq fg 0)
                (set-face-attribute cface nil
                                    :background bg
                                    :box nil)
              (set-face-attribute cface nil
                                  :foreground fg
                                  :background bg
                                  :box nil))
          (set-face-attribute cface nil
                            :foreground powerline-fg
                            :background bg
                            :box nil))
        cface)
    nil))

(defun powerline-make-left
  (string color1 &optional color2 localmap)
  (let ((plface (powerline-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface))
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize " " 'display
                     (arrow-left-xpm color1 color2)
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq powerline-arrow-shape 'arrow)
                                            (force-mode-line-update)))) ""))))

(defun powerline-make-right
  (string color2 &optional color1 localmap)
  (let ((plface (powerline-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
         (propertize " " 'display
                     (arrow-right-xpm color1 color2)
                   'local-map (make-mode-line-mouse-map
                               'mouse-1 (lambda () (interactive)
                                          (setq powerline-arrow-shape 'arrow)
                                          (force-mode-line-update))))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface)))))

(defun powerline-make-fill (color)
  ;; justify right by filling with spaces to right fringe, 20 should be calculated
  (let ((plface (powerline-make-face color))
        (amount (- (window-total-width)
                   (+ 34
                      (if (eq (get-buffer-window) powerline-current-window)
                          (length (-powerline-get-weather " %(weather) "))
                        0)
                      (if (and (fboundp 'boop-format-results)
                               (eq (get-buffer-window) powerline-current-window))
                          (+ 1 (length  (boop-format-results))) 0)
                      (length (-powerline-get-temp))))))
    (propertize " " 'display `((space :align-to ,amount)) 'face plface)))

(defun powerline-make-text
  (string color &optional fg localmap)
  (let ((plface (powerline-make-face color)))
    (if string
        (if localmap
            (propertize string 'face plface 'mouse-face plface 'local-map localmap)
          (propertize string 'face plface))
      "")))

(defun powerline-make (side string color1 &optional color2 localmap)
  (cond ((and (eq side 'right) color2) (powerline-make-right  string color1 color2 localmap))
        ((and (eq side 'left) color2)  (powerline-make-left   string color1 color2 localmap))
        ((eq side 'left)               (powerline-make-left   string color1 color1 localmap))
        ((eq side 'right)              (powerline-make-right  string color1 color1 localmap))
        ((eq side 'donttouch)          (powerline-make-right  string color1 color1 localmap))
        (t                             (powerline-make-text   string color1 localmap))))

(defmacro defpowerline (name string)
  "Macro to create a powerline chunk."
  `(defun ,(intern (concat "powerline-" (symbol-name name)))
       (side color1 &optional color2)
     (powerline-make side
                     (let ((result ,string))
                       (cond ((listp result)
                              (format-mode-line result))
                             ((not (or (stringp result)
                                       (null result)))
                              (progn
                                " ERR"))
                             (t
                              result)))
                     color1 color2)))



(defun powerline-mouse (click-group click-type string)
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                    nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

(defpowerline arrow       "")

(defvar powerline-buffer-size-suffix t)
(defpowerline buffer-size (propertize
                            (if powerline-buffer-size-suffix
                                "%I"
                              "%i")
                            'local-map (make-mode-line-mouse-map
                                        'mouse-1 (lambda () (interactive)
                                                   (setq powerline-buffer-size-suffix
                                                         (not powerline-buffer-size-suffix))
                                                   (redraw-modeline)))))
(defpowerline rmw         "%*")
(defpowerline major-mode  (propertize (if (stringp mode-name) mode-name (format "%s" (buffer-mode (current-buffer))))
                                      'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
                                      'local-map (let ((map (make-sparse-keymap)))
                                                   (define-key map [mode-line down-mouse-1]
                                                     `(menu-item ,(purecopy "Menu Bar") ignore
                                                                 :filter (lambda (_) (mouse-menu-major-mode-map))))
                                                   (define-key map [mode-line mouse-2] 'describe-mode)
                                                   (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                                                   map)))
(defpowerline process      mode-line-process)
(defpowerline minor-modes (let ((mms (split-string (format-mode-line minor-mode-alist))))
                            (apply 'concat
                                   (mapcar #'(lambda (mm)
                                              (propertize (if (string= (car mms)
                                                                       mm)
                                                              mm
                                                            (concat " " mm))
                                                          'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                                                          'local-map (let ((map (make-sparse-keymap)))
                                                                       (define-key map [mode-line down-mouse-1]   (powerline-mouse 'minor 'menu mm))
                                                                       (define-key map [mode-line mouse-2]        (powerline-mouse 'minor 'help mm))
                                                                       (define-key map [mode-line down-mouse-3]   (powerline-mouse 'minor 'menu mm))
                                                                       (define-key map [header-line down-mouse-3] (powerline-mouse 'minor 'menu mm))
                                                                       map)))
                                           mms))))
(defpowerline row         "%4l")
(defpowerline column      "%3c")
(defpowerline percent     "%6p")
(defpowerline narrow      (let (real-point-min real-point-max)
                            (save-excursion
                              (save-restriction
                                (widen)
                                (setq real-point-min (point-min) real-point-max (point-max))))
                            (when (or (/= real-point-min (point-min))
                                      (/= real-point-max (point-max)))
                              (propertize "Narrow"
                                          'help-echo "mouse-1: Remove narrowing from the current buffer"
                                          'local-map (make-mode-line-mouse-map
                                                      'mouse-1 'mode-line-widen)))))
(defpowerline status      "%s")
(defpowerline global      global-mode-string)
(defpowerline emacsclient mode-line-client)
(defpowerline project-id (if (and (boundp 'projectable-id)
                                  (not (eql nil projectable-id)))
                             (concat (upcase projectable-id) " ∘")
                           (format "×")))

;; (defpowerline vc          (when (and (buffer-file-name (current-buffer))
;;                                      vc-mode)
;;                            (symbol-name (vc-mode-line (buffer-file-name (current-buffer) )))))
(defpowerline vc vc-mode)
(defpowerline time (format-time-string "%H:%M"))

(defpowerline flycheck-status (propertize (format "%s" (cadr (flycheck-status-emoji-mode-line-text)))
                                          'display '(height 0.7)))

(defun -powerline-get-temp ()
  (let ((temp (-powerline-get-weather "%(temperature)")))
    (unless (string= "" temp) (format "%s°C" (round (string-to-number temp))))))

(defun -powerline-get-weather (format)
  (if (boundp 'yahoo-weather-info)
      (downcase (yahoo-weather-info-format yahoo-weather-info format))
    ""))

(defpowerline weather (-powerline-get-weather " %(weather) "))
(defpowerline temperature (-powerline-get-temp))

(defpowerline eb-indicator (eyebrowse-mode-line-indicator))

(defpowerline buffer-id (propertize (car (propertized-buffer-identification "%12b"))
                                      'face (powerline-make-face color1)))

(defpowerline window-number (format "%c" (+ 10121 (window-numbering-get-number))))

(defpowerline percent-xpm (propertize "  "
                                      'display
                                      (let (pmax
                                            pmin
                                            (ws (window-start))
                                            (we (window-end)))
                                        (save-restriction
                                          (widen)
                                          (setq pmax (point-max))
                                          (setq pmin (point-min)))
                                        (percent-xpm pmax pmin we ws 15 color1 color2))))

(defvar powerline-current-window nil)
(defun update-current-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq powerline-current-window (selected-window))))
(add-function :before pre-redisplay-function 'update-current-window)

(defun powerline-boop ()
  (when (fboundp 'boop-format-results)
    (let ((s (boop-format-results)))
      (add-face-text-property 0 (length s) `(:background ,powerline-color2) nil s) s)))

(defun -count-notifications (pattern notification-char)
  (when (boundp 'slack-ims)
    (let ((result
          (-reduce '+ (-map 'string-to-number (-non-nil
                                               (-map (lambda (it)
                                                       (with-temp-buffer
                                                         (insert (format "%s" it))
                                                         (goto-char (point-min))
                                                         (when (search-forward-regexp pattern (point-max) t)
                                                           (match-string 1)))) slack-ims))))))
     (when (> result 0) notification-char))))

(defpowerline new-im-notifications (-count-notifications "[0-9]+ \\([0-9]+\\) (.*?)" "✩"))
(defpowerline new-channel-notifications (-count-notifications "(.*?) \\([0-9]+\\) [0-9]+ nil" "✧"))

(defun powerline-mode-xpm ()
  (propertize " " 'display (mode-icon-ruby-xpm powerline-fg powerline-color1)
              'face `(:background ,powerline-color1)))

(setq-default mode-line-format
              (list "%e"
                    '(:eval (concat
                             (powerline-rmw    'left  nil  )
                             (powerline-project-id     'left  nil  )
                             (powerline-window-number  'left  nil  )

                             ;; (powerline-new-im-notifications  'left  nil  )

                             (powerline-buffer-id      'left  nil   powerline-color1  )
                             
                             (powerline-make-text " " powerline-color1)
                             ;; (powerline-mode-xpm)
                             (powerline-major-mode       'left  powerline-color1  )
                             (powerline-process        'text        powerline-color1  )
                             (powerline-flycheck-status  'left  powerline-color1  )
                             (powerline-narrow         'left        powerline-color1  powerline-color2 )


                             (if (eq (get-buffer-window) powerline-current-window)
                                 (concat
                                  (powerline-vc       'center      powerline-color2  )
                                  (powerline-make-fill             powerline-color2  )
                                  (powerline-weather  'text        powerline-color2  )
                                  (powerline-boop))
                               (powerline-make-fill                powerline-color2  ))

                             (powerline-row            'right       powerline-color1  powerline-color2 )
                             (powerline-make-text      ":"          powerline-color1  )
                             (powerline-column         'right       powerline-color1  )
                             (powerline-time           'right  nil  powerline-color1  )
                             (powerline-temperature    'right  nil  )
                             (powerline-make-text      "-"   nil  )
                             (powerline-buffer-size    'left   nil  )))))

(provide 'powerline)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:
;;; powerline.el ends here
