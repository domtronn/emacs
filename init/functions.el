;;; functions.el --- List of my own functions

;; Copyright (C) 2014  Dominic Charlesworth <dgc336@gmail.com>

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
;;; common functions used in Emacs

;;; Code:

;; --------------------------------------------
;; Functions used in compiling latex & xelatex
;; --------------------------------------------
(defun latex-make ()
  (interactive)
  (setq buffer-save-without-query t)
  (if (buffer-modified-p) (save-buffer))
  (let ((f1 (current-frame-configuration))
        (retcode (shell-command (concat "rubber --pdf " (buffer-file-name)))))
    (message "Return code ❯ %s" retcode)
    (if (= retcode 0) (find-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))))

(defun xelatex-make ()
  (interactive)
  (if (string-equal major-mode "latex-mode")
      (progn
        (setq buffer-save-without-query t)
        (if (buffer-modified-p) (save-buffer))
        (let ((f1 (current-frame-configuration))
              (retcode (shell-command (concat "xelatex -interaction=nonstopmode " (buffer-file-name)))))
          (message "Return code ❯ %s" retcode)
          (if (get-buffer (concat (file-name-sans-extension (buffer-name)) ".pdf"))
              (kill-buffer (concat (file-name-sans-extension (buffer-name)) ".pdf")))
          (find-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
          (delete-other-windows)))))

(defun jump-to-find-function ()
  "Go to the function definition for elisp"
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (find-function (intern (thing-at-point 'symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS2 Navigation Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar js2-jump-fallback-f nil)
(defun js2-jump-around ()
  "Jump to variable definition or try to find its module"
  (interactive)
  (let* ((p (point))
         (found? (and (ignore-errors (js2-jump-to-definition))
                      (not (eq p (point))))))
    (unless found?
      (when (js2--looking-at "let\\|var\\|const")
        (beginning-of-line)
        (search-forward "= ")
        (setq found? (ignore-errors (js2-jump-to-definition))))
      (unless found? (goto-char p)))
    (unless found?
      (when (js2--looking-at "require\(.*?\)")
        (beginning-of-line)
        (search-forward "require")
        (setq found? (ignore-errors (js2-jump-to-require)))))
    (unless found?
      (when (js2--looking-at "import.*?from")
        (setq found? (ignore-errors (js2-jump-to-import)))))
    (unless found?
      (if (not js2-jump-fallback-f)
          (error "Could not find module to jump to...")
        (ring-insert find-tag-marker-ring (point-marker))
        (funcall js2-jump-fallback-f (thing-at-point 'symbol))))))

(defun js2-jump-to-require ()
  "Jump to a require module"
  (let* ((node (js2r--closest 'js2-call-node-p))
         (args (first (js2-call-node-args node)))
         (name (js2-string-node-value args)))
    (js2--find-file name)))

(defun js2-jump-to-import ()
  "Jump to an import module"
  (let* ((node (js2r--closest 'js2-import-node-p))
         (name (js2-import-node-module-id node)))
    (js2--find-file name)))
    
(defun js2--find-file (name)
  (let* ((module-dir (file-name-directory name))
         (module-name (concat (file-name-base name) "\\."))
         (modules (ignore-errors
                    (or
                     (directory-files module-dir t module-name)
                     (directory-files name t "index")))))
    (unless modules
      (error "Could not find module '%s'" name))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-file (first modules))))

(defun js2--looking-at (s)
  (save-excursion
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (not (eq nil (string-match s line))))))

(defun js2/load-prettify-symbols-alist ()
  (push '("function" . ?𝒇) prettify-symbols-alist)
  (push '("=>" . ?⭢) prettify-symbols-alist)
  (push '("return" . ?⇐) prettify-symbols-alist)
  (push '("undefined" . ?∅) prettify-symbols-alist)
  ;; Maths symbols
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("!=" . ?≠) prettify-symbols-alist)
  (push '("!==" . ?≢) prettify-symbols-alist)
  (push '("===" . ?≡) prettify-symbols-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening Files in other applications ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-in (ed &optional file)
  "Opens the current file in ED or specified FILE."
  (shell-command (format "%s %s" ed (or file buffer-file-name))))

(defun panic ()
  "Alias to run `open-current-file`."
  (interactive)
  (open-current-file))

(defun open-urls-in-file ()
  (interactive)
  (let (result)
    (save-excursion
      (with-current-buffer (buffer-name)
        (goto-char (point-min))
        (while (search-forward-regexp goto-address-url-regexp nil t)
          (setq result (append (list (match-string 0)) result)))))
    (browse-url
     (completing-read "Open URL: "
                      (-uniq (--map (s-chop-suffix "\)" it) result))))))

(defun open-current-file ()
  "Open the current file in different things."
  (interactive)
  (let ((type (completing-read
               "Open current file in editor ❯ " '("Sublime Text" "Atom" "Chrome" "Finder" "Multiple URLs" "URL") nil nil)))
    (cond ((string-equal type "Sublime Text") (open-in "subl"))
          ((string-equal type "Atom") (shell-command (format "atom %s" (projectile-project-root))))
          ((string-equal type "Chrome") (browse-url (buffer-file-name)))
          ((string-equal type "Multiple URLs")
           (call-interactively 'link-hint-open-multiple-links))
          ((string-equal type "URL")
           (if (browse-url-url-at-point) (browse-url-at-point) (link-hint-open-link)))
          ((string-equal type "Finder") (open-in "open" (file-name-directory (buffer-file-name)))))))

(defun isearch-yank-from-start ()
  "Move to beginning of word before yanking word in `isearch-mode`."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'isearch-yank-from-start
                             'isearch-yank-word-or-char
                             isearch-mode-map))

(add-hook 'isearch-mode-hook (lambda ()
  "Activate my customized Isearch word yank command."
  (substitute-key-definition 'isearch-yank-word-or-char
                             'isearch-yank-from-start
                             isearch-mode-map)))

(defun quick-term (name)
  (interactive "sEnter terminal name ❯ ")
  (ansi-term "/bin/bash" name))


;; ============================================================================
(defun dgc-comment ()
  "comment or uncomment highlighted region or line"
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-buffer-regexps (r)
  "Kill all buffers matching regexp R."
  (interactive "sEnter Regexp ❯ ")
  (let ((killable-buffers (-filter '(lambda (b) (string-match r (buffer-name b))) (buffer-list))))
    (when (yes-or-no-p (format "Kill %d buffers matching \"%s\"? " (length killable-buffers) r))
      (mapc #'kill-buffer killable-buffers))))

;; ============================================================================

(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b "*Register to be set up to hold `my-ediff-bwin-config'configuration.")
(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e "*Register to be used to hold `my-ediff-awin-config' window configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
                  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
                  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (if (get-buffer "*vc-dir*")
      (progn (switch-to-buffer "*vc-dir*")
             (delete-other-windows))
    (when my-ediff-bwin-config
      (set-window-configuration my-ediff-bwin-config)
      (kill-ediff-revisions))))

(defun kill-ediff-revisions ()
  "Function tor emove the revision buffers after quit"
  (let ((bufs (--filter (and
                         (not (string-equal (buffer-name) (buffer-name it)))
                         (s-starts-with? (buffer-name) (buffer-name it))) (buffer-list))))
    (-map 'kill-buffer bufs)))

(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))


;; ============================================================================
(defun random-hex ()
  "Return a string in the form of #FFFFFF. Choose the number for
   #xffffff randomly using Emacs Lisp's builtin function (random)."
  ;; seed our random number generator: current datetime plus Emacs's
  ;; process ID
  (interactive)
  (random t)
  (message "%s" (format "#%06x" (random #xffffff))))

(defun ahahah ()
  "You know what it displays..."
  (interactive)
  (let ((clr (random-hex)))
    (message "%s" (propertize "Ah ah ah, you didn't say the magic word!"
                              'face `(:foreground ,clr)))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let  ((i 1)
           (numWindows (count-windows)))
      (while  (< i numWindows)
        (let* (
               (w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i numWindows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1  b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(defun semi-colon-end ()
  "Function to insert a semi colon at the end of the line from anywhere."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (end-of-line)
    (insert ";")))

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters."
  (interactive)
  (save-excursion
    (progn
      (re-search-forward "[ \t\r\n]+" nil t)
      (replace-match "" nil nil))))

(defun upcase-case-next-letter ()
  "Upcase the next letter, then move forward one character."
  (interactive)
  (upcase-region (point) (+ 1 (point)))
  (forward-char))

(defun downcase-case-next-letter ()
  "Downcase the next letter, then move forward one character."
  (interactive)
  (downcase-region (point) (+ 1 (point)))
  (forward-char))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(defun duplicate-line-and-replace-regexp ()
  (interactive)
  (duplicate-line)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1)
  (call-interactively 'vr/query-replace))

(defun font-scale (op &optional amount)
  "Apply function operator OP (+/-) to scale face attribute height by AMOUNT."
  (let* ((height (face-attribute 'default :height))
         (mode-line-height (face-attribute 'mode-line :height)))
    (set-face-attribute 'default nil :height (funcall op height (or amount 10)))))

(defun elisp-debug (var)
  (interactive "sDebug variable ❯ ")
  (insert (concat "(message \"" (capitalize var) ": %s\" " var ")")))

(defun smart-delete-pair (&rest args)
  "Smartly delete character or pair based on the next character."
  (interactive)
  (save-excursion
    (let ((next-char (buffer-substring (point) (+ (point) 1)))
          (prev-char (buffer-substring (point) (- (point) 1))))
     (cond ((string-match "[\{\(\[\"']" next-char) (delete-pair))
           ((string-match "[\{\(\[\"']" prev-char) (backward-char 1) (delete-pair))
           ((string-match "[\]\}\)]" next-char) (smart-backward) (delete-pair))
           ((string-match "[\]\}\)]" prev-char) (smart-backward) (delete-pair))))))

(defun magit-open-file-other-window ()
  (interactive)
  (let ((current-section (magit-current-section)))
    (when (eq 'file (magit-section-type current-section))
      (let* ((file-name (expand-file-name (magit-section-value current-section)))
             (file-buffer (or (find-buffer-visiting file-name) (create-file-buffer file-name))))
        (when (not (file-directory-p file-name))
          (display-buffer file-buffer))))))

(defun magit-whitespace-cleanup ()
  (interactive)
  (let ((current-section (magit-current-section)))
    (when (eq 'file (magit-section-type current-section))
      (let* ((magit-file (magit-section-value current-section))
             (file-name (expand-file-name magit-file))
             (file-buffer (or (get-file-buffer file-name) (create-file-buffer file-name))))
        (with-current-buffer file-buffer
          (goto-char (point-min))
          (whitespace-cleanup)
          (save-buffer file-buffer))
        (when (-contains? (magit-staged-files) magit-file)
          (magit-stage-file magit-file))
        (magit-refresh)))))

(defun magit-vc-ediff ()
  (interactive)
  (let ((current-section (magit-current-section)))
    (when (eq 'file (magit-section-type current-section))
      (let* ((file-name (expand-file-name (magit-section-value current-section)))
             (file-buffer (find-file file-name)))
        (with-current-buffer file-buffer (call-interactively 'vc-ediff))))))

(defun you-can-never-leave (&optional full)
  (interactive)
  (let ((playing
         (string-equal "playing\n"
          (shell-command-to-string "osascript -e \"if app \\\"iTunes\\\" is running then tell app \\\"iTunes\\\" to get player state\""))))
    (when (not playing)
      (if full
          (shell-command "osascript -e \"tell application \\\"Terminal\\\" to do script \\\"mplayer ~/.emacs.d/elisp/music/hotel-california.m4a -ss 04:14.3 && exit 0\\\"\"")
        (shell-command "osascript -e \"tell application \\\"Terminal\\\" to do script \\\"mplayer ~/.emacs.d/elisp/music/youcanneverleave.wav && exit 0\\\"\""))))
  (restart-emacs))

(defun gallery-music ()
  (interactive)
  (shell-command "osascript -e \"tell application \\\"Terminal\\\" to do script \\\"mplayer ~/.emacs.d/elisp/music/gallery.m4a && exit 0\\\"\""))

(defun dired-mark-duplicate-dirs ()
  "Mark all directories which have a common part."
  (interactive)
  (when (not (eq 'dired-mode (with-current-buffer (buffer-name) major-mode)))
    (error "Not in dired-mode"))
  (let* ((cwd (dired-current-directory))
         (cmd (format "ls %s | grep -Eo [a-z0-9\-]+[a-z] | sort | uniq -d | xargs echo -n" cwd))
         (dirs (split-string (shell-command-to-string cmd) " ")))
    (mapc (lambda (dir) (dired-mark-files-regexp dir)) dirs)))

(defun ac-lambda (&rest sources)
  "Sets up autocomplete mode and local SOURCES"
  (interactive)
  (setq-local ac-sources sources))

(defun -move-link (f)
  (funcall f "http[s]\\{0,1\\}://[a-z0-9#%\./_-]+"))

(defun previous-link ()
  (interactive)
  (-move-link 'search-backward-regexp))

(defun next-link ()
  (interactive)
  (-move-link 'search-forward-regexp))

(defun json-comma? ()
  "Whether or not to use a comma at the end of a json snippet"
  (with-current-buffer (buffer-name)
    (save-excursion
      (let* ((restore (point))
             (quote (and (goto-char restore) (search-forward "\"" nil t)))
             (brace (and (goto-char restore) (search-forward "}" nil t))))
        (when (and quote brace) (< quote brace))))))

;; (defun bemify-emmet-string (expr)
;;   "Pre process an emmet string to be bemified."
;; 	(let* ((split (split-string (car expr) "|"))
;; 				 (filter (cadr split))
;; 				 (emmet-s (car split)))
;; 		(when (equal filter "bem")
;; 			(let ((bemified
;; 						 (with-temp-buffer
;; 							 (insert emmet-s)
;; 							 (goto-char (point-min))
;; 							 (while (re-search-forward "\\.\\([a-zA-Z]+[a-zA-Z0-9]*\\)" (point-max) t)
;; 								 (let ((base-class (match-string 1))
;; 											 (restore-point (point)))
;; 									 (while (re-search-forward "\\.[a-z]*?\\([_-]\\{2\\}\\)" (point-max) t)
;; 										 (replace-match (format ".%s%s" base-class (match-string 1))))
;; 									 (goto-char restore-point)))
;; 							 (buffer-string))))
;; 				(when (not (equal emmet-s bemified))
;; 					(with-current-buffer (current-buffer)
;; 						(goto-char (cadr expr))
;; 						(delete-region (cadr expr) (caddr expr))
;; 						(insert bemified)))))))

(defun dir-depth (dir)
  "Gives depth of directory DIR"
  (when dir (length (split-string (expand-file-name dir) "/"))))

(defun flycheck--guess-checker ()
  "Guess the JS lintrc file in the current project"
  (if (not (buffer-file-name))
      'javascript-standard
    (let* ((jshintrc-loc (locate-dominating-file (buffer-file-name) flycheck-jshintrc))
           (jshintrc-depth (dir-depth jshintrc-loc))
           (eslintrc-loc (locate-dominating-file (buffer-file-name) flycheck-eslintrc))
           (eslintrc-depth (dir-depth eslintrc-loc)))
      (cond
       ((and (not eslintrc-loc) (not jshintrc-loc)) 'javascript-standard)
       ((and jshintrc-loc (not eslintrc-loc)) 'javascript-jshint)
       ((and eslintrc-loc (not jshintrc-loc)) 'javascript-eslint)
       ((> jshintrc-depth eslintrc-depth) 'javascript-jshint)
       (t 'javascript-eslint)))))

(defun other-window-everything (thing)
  (cond
   ((get-buffer thing) (switch-to-buffer-other-window thing))
   (t (find-file-other-window thing))))

(defmacro defrepl (name executable)
  `(prog1
       (defun ,(intern (format "%s-repl" name)) ()
         ,(format "Open a `%s` repl" name)
         (interactive)
         (ansi-color-for-comint-mode-on)
         (add-to-list
          'comint-preoutput-filter-functions
          (lambda (output)
            (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))
         (pop-to-buffer (make-comint ,(format "%s-repl" name) ,executable)))))

(defrepl "ramda" "ramda-repl")
(defrepl "node" "node")
(defrepl "lodash" "n_")

(defmacro defewwmenu (name query)
  `(defun ,(intern (format "eww-%s-imenu" name)) ()
     (interactive)
     (unless (get-buffer-window "*eww*")
       (if (get-buffer "*eww*")
           (popwin:popup-buffer "*eww*" :height 30 :noselect t :stick nil)
         (error "Could not find current *eww* buffer")))
     (with-current-buffer (get-buffer "*eww*")
       (let ((p (point)) (results (list )))
         (goto-char (point-min))
         (while (re-search-forward ,query (point-max) t)
           (unless (assoc (match-string 1) results)
             (setq results (append results
                                   (list (cons
                                          (match-string-no-properties 1)
                                          (match-string-no-properties 0)))))))
         (goto-char p)
         (ivy-read "Jump to ❯ " results
                   :action
                   (lambda (match)
                     (with-current-buffer (get-buffer "*eww*")
                       (goto-char (point-min))
                       (set-window-point
                        (get-buffer-window "*eww*")
                        (search-forward match)))
                     (let ((cb (current-buffer)))
                       (select-window (get-buffer-window "*eww*"))
                       (recenter-top-bottom 1)
                       (select-window (get-buffer-window cb))))
                   :update-fn (lambda () (setq ivy-calling t) (ivy-call))
                   :unwind (lambda () (setq ivy-calling nil)))))))

(defewwmenu "ramda" "\\(.*?\\) Added in")
(defewwmenu "lodash" "_\.\\([a-z]+?\\)(.*?)")

(global-set-key (kbd "C-c / i") 'eww-imenu)
(defun eww-imenu ()
  (interactive)
  (with-current-buffer (get-buffer "*eww*")
    (cond
     ((string-match "docs/ramda" eww-current-url)
      (eww-ramda-imenu))
     ((string-match "docs/lodash" eww-current-url)
      (eww-lodash-imenu))
     (t (error "Could not find a matching imenu")))))

(defun send-to-repl ()
  (interactive)
  (let ((buf (car (--filter (and (string-match "repl" (buffer-name it)) (get-buffer-window it))
                            (buffer-list)))))
    (when (not buf) (error "Could not find repl to send to"))
    (let ((proc (get-buffer-process buf))
          (content (if (region-active-p)
                       (buffer-substring (region-beginning) (region-end))
                     (buffer-string))))
      (comint-send-string proc (format "%s\n" content)))))

;; Docker helper functions

(defun docker-containers--remove-trailing-whitespace (proc string)
  "Remove trailing whitespace from PROC on STRING."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (goto-char (process-mark proc))
        (insert string)
        (delete-trailing-whitespace)
        (set-marker (process-mark proc) (point))))))

(defun docker-containers-logs-follow-all ()
  "Follow all currently running docker containers"
  (interactive)
  (--map (docker-containers-logs-follow (car it))
         (docker-containers-entries)))

(defun docker-containers-logs-follow (image)
  "Create a process buffer to follow an image"
  (let* ((cmd (format "docker logs -f --tail=50 %s" image))
         (bufname (format "*docker-logs:%s*" image))
         (buf (get-buffer bufname))
         (proc (get-buffer-process buf)))

    (when (and buf proc)
      (set-process-query-on-exit-flag proc nil)
      (kill-buffer buf))

    (prog1 (setq buf (get-buffer-create bufname))
      (with-current-buffer bufname
        (compilation-mode)))

    (let ((proc (start-process-shell-command bufname buf cmd)))
      (set-process-filter proc #'docker-containers--remove-trailing-whitespace))
    (get-buffer bufname)))

(defun docker-containers-logs-follow-selection ()
  "Log the currently selected docker container"
  (interactive)
  (let ((cb (current-buffer)))
    (switch-to-buffer-other-window
     (docker-containers-logs-follow (tabulated-list-get-id)))
    (select-window (get-buffer-window cb))))

(defun org-color-tag (tag col)
    (while (re-search-forward tag nil t)
      (add-text-properties (match-beginning 0) (point-at-eol)
                           `(face (:foreground ,col)))))

(defun cfw:capture-schedule-day ()
  (let* ((date (s-chop-prefix "<" (s-chop-suffix ">" (cfw:org-capture-day))))
         (h (format-time-string "%H"))
         (M (string-to-number (format-time-string "%M")))
         (m (format "%02d" (- M (mod M 30))))
         (H (1+ (string-to-number h))))
    (message "%s" (format "<%s %s:%s-%s:%s>" date h m H m ))))

 (defun unfill-paragraph (&optional region)
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max))
            ;; This would override `fill-column' if it's an integer.
            (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

(global-set-key (kbd "<s-f12>") 'dgc/save-or-restore-window-config)
(defvar dgc/window-config nil "A variable to store current window config")
(defun dgc/save-or-restore-window-config ()
  (interactive)
  (if dgc/window-config
      (progn (message "♺ Restoring window layout")
           (set-window-configuration dgc/window-config)
           (setq dgc/window-config nil))
    (message "✓ Saving window layout")
    (setq dgc/window-config (current-window-configuration))))

(global-set-key (kbd "M-K") 'kill-assignment)
(defun kill-assignment ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((start (+ 3 (search-backward " = " (line-beginning-position) t))))
      (kill-region start (line-end-position))
      (goto-char start))))

(global-set-key (kbd "S-SPC") 'wrap-space-or-space)
(defun wrap-space-or-space ()
  (interactive)
  (if mark-active
      (let ((p (point))
            (rb (region-beginning))
            (re (region-end)))
        (goto-char re) (insert " ")
        (goto-char rb) (insert " ")
        (goto-char p))
    (insert " ")))

(defun my/projectile-test-suffix (project-type)
  "Find default test files suffix based on PROJECT-TYPE."
  (cond
   ((member project-type '(emacs-cask)) "-test")
   ((member project-type '(grunt generic)) "Spec")
   ((member project-type '(gulp)) "-spec")))

(advice-add 'projectile-toggle-between-implementation-and-test :around
            '(lambda (f &rest args)
               (if (or (eq major-mode 'scss-mode)
                       (eq major-mode 'css-mode))
                   (projectile-other-resolution)
                 (apply f args))))
(defun projectile-other-resolution ()
  (interactive)
  (let ((res (and (string-match "/\\([0-9]+\\)/" (buffer-file-name))
                  (match-string 1 (buffer-file-name)))))
    (find-file
     (format "%s/%s"
             (projectile-project-root)
             (car (--reject (string-match res it)
                            (--filter (string-match (file-name-base (buffer-file-name)) it)
                                      (projectile-current-project-files))))))))

(global-set-key (kbd "<S-f10>") 'eww-edit-url)
(defun eww-edit-url ()
  (interactive)
  (eww (read-string "Enter URL or keywords ❯ "
                    (when (eq major-mode 'eww-mode)
                      (eww-copy-page-url)))))

(defcustom livedown-port 1337
  "Port on which livedown server will run."
  :type 'integer
  :group 'livedown)

(defcustom livedown-open t
  "Open browser automatically."
  :type 'boolean
  :group 'livedown)

(defun livedown-preview ()
  "Preview the current file in livedown."
  (interactive)

  (call-process-shell-command
   (format "livedown stop --port %s &"
           livedown-port))

  (start-process-shell-command
   (format "emacs-livedown")
   (format "emacs-livedown-buffer")
   (format "livedown start %s --port %s %s "
           buffer-file-name
           livedown-port
           (if livedown-open "--open" "")))
  (print (format "%s rendered @ %s" buffer-file-name livedown-port) (get-buffer "emacs-livedown-buffer")))

(defun org-start-ticket ()
  (interactive)
  (call-interactively 'link-hint-copy-link)
  (let ((link (car kill-ring)))
    (goto-char (point-min))
    (search-forward link)
    (org-agenda-todo 2)
    (browse-url (car (split-string link "]")))))

(provide 'functions)
;;; functions.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:
