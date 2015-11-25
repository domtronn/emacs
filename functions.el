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

(defun use-tabs ()
  (interactive)
  (setq-default indent-tabs-mode t))

(defun use-spaces ()
  (interactive)
  (setq-default indent-tabs-mode nil))

;; --------------------------------------------
;; Functions used in compiling latex & xelatex
;; --------------------------------------------
(defun latex-make ()
  (interactive)
  (setq buffer-save-without-query t)
  (if (buffer-modified-p) (save-buffer))
  (let ((f1 (current-frame-configuration))
        (retcode (shell-command (concat "rubber --pdf " (buffer-file-name)))))
    (message "Return code : %s" retcode)
    (if (= retcode 0) (find-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))))

(defun xelatex-make ()
  (interactive)
  (if (string-equal (buffer-mode (buffer-name)) "latex-mode")
      (progn
        (setq buffer-save-without-query t)
        (if (buffer-modified-p) (save-buffer))
        (let ((f1 (current-frame-configuration))
              (retcode (shell-command (concat "xelatex " (buffer-file-name)))))
          (message "Return code : %s" retcode)
          (if (= retcode 0)
              (progn
                (if (buffer-exists (concat (file-name-sans-extension (buffer-name)) ".pdf"))
                    (kill-buffer (concat (file-name-sans-extension (buffer-name)) ".pdf")))
                (find-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
                (delete-other-windows)))))
    nil))

(defun get-last-spy ()
  (interactive)
  (save-excursion
    (let ((start (+ (search-backward-regexp "spyOn(" 6)))
          (end (- (search-forward-regexp ")") 1)))
      (if (string-match "spyOn\(\\\(.*\\\),\\s-*['\"]\\\(.*?\\\)['\"]" (buffer-substring start end))
          (format "%s.%s" (match-string 1 (buffer-substring start end)) (match-string 2 (buffer-substring start end)))))))

(defun go-to-class ()
  "Find and go to the class at point"
  (interactive)
  (let ((class-name (thing-at-point 'symbol)))
    (ring-insert find-tag-marker-ring (point-marker))
    (save-excursion
      (beginning-of-buffer)
      (let ((start (search-forward-regexp "function\\s-*("))
            (end (- (search-forward ")") 1)))
        (goto-char start)
        (if (search-forward class-name end t)
            (mapcar #'(lambda (lib-alist)
                         (let* ((id (car lib-alist))
                                (file-alist (cdr lib-alist))
                                (record (assoc (concat (downcase class-name) ".js") file-alist)))
                           (if (and record (= (length record) 2))
                               (let ((found-file (cadr record)))
                                 (message "Found %s" found-file)
                                 (find-file found-file))
                             (message "Couldn't find a cached file for %s..." class-name))))
                     projectable-project-alist)
          nil)))))

(defun go-to-require ()
  "Tries to load the require path assoscaited with a variable for node includes"
  (interactive)
  (save-excursion
    (ring-insert find-tag-marker-ring (point-marker))
    (let ((require-pos (search-forward "require" (line-end-position) t)))
      (when require-pos
        (if (search-forward-regexp "(['\"]\\(.*?\\)['\"])" (line-end-position) t)
            (mapcar #'(lambda (lib-alist)
                        (let* ((id (car lib-alist))
                               (file-alist (cdr lib-alist))
                               (file (file-name-base (match-string 1)))
                               (record (assoc (concat file ".js") file-alist)))
                          (if (and record (= (length record) 2))
                              (let ((found-file (cadr record)))
                                (message "Found %s" found-file)
                                (find-file found-file))
                            (message "Couldn't find a cached file for %s..." file))))
                    projectable-project-alist)
          nil)))))

(defun etags-select-find-tag-at-point-fallback ()
  "If there is a thing at point call find thing at point, else find thing"
  (if (thing-at-point 'symbol)
      (etags-select-find-tag-at-point)
    (etags-select-find-tag)))

(defun go-to-thing-at-point ()
  "Go to the thing at point assuming if it's not a class or function it's a variable."
  (interactive)
  (let ((thing (thing-at-point 'symbol))
        (thing-point (point)))
    (if (eq nil (go-to-require))
        (if (eq nil (go-to-class))
            (if (eq nil (search-backward-regexp
                         (format "var\\s-*%s" thing) (point-min) t))
                (if (eq nil (search-backward-regexp
                             (format "function\\s-*(.*%s" thing) (point-min) t))
                    (progn
                      (search-backward ".")
                      (let ((thing-clazz (thing-at-point 'symbol)))
                        (goto-char thing-point)
                        (etags-select-find-tag-at-point)))
                  (progn
                    (search-forward thing)
                    (backward-word))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening Files in other applications ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-in-and-activate-intellj ()
  "Opens the current file in intellij for n00b5!"
  (interactive)
  (shell-command
   (format "/Applications/IntelliJ\\ IDEA\\ 14\\ CE.app/Contents/MacOS/idea %s; osascript -e \"tell application \\\"IntelliJ Idea 14 CE\\\" to activate\""
           (buffer-file-name))))

(defun open-in (ed &optional file)
  "Opens the current file in ED or specified FILE."
  (shell-command (format "%s %s" ed (or file buffer-file-name))))

(defun panic ()
  "Alias to run `open-current-file`."
  (interactive)
  (open-current-file))

(defun open-current-file ()
  "Open the current file in different things."
	(interactive)
  (let ((type (completing-read
               "Open current file in editor: " '("IntelliJ IDEA" "Sublime Text" "Atom" "Chrome" "Finder") nil nil)))
    (cond ((string-equal type "Sublime Text") (open-in "subl"))
          ((string-equal type "Atom") (open-in "atom"))
          ((string-equal type "IntelliJ IDEA") (open-in-and-activate-intellj))
          ((string-equal type "Chrome") (browse-url (buffer-file-name)))
          ((string-equal type "Finder") (open-in "open" (file-name-directory (buffer-file-name)))))))

(defun buffer-mode (buf)
  "Return the major mode associated with BUF."
  (with-current-buffer buf
    major-mode))

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

(defun my-vc-dir ()
  "Call `vc-dir` on the appropriate project."
  (interactive)
  (let ((repo-root (repository-root)))
    (if (repository-root-match repository-root-matcher/git repo-root repo-root)
        (magit-status)
        (vc-dir repo-root))))

(defun quick-term (name)
  (interactive "sEnter terminal name : ")
  (ansi-term "/bin/bash" name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom utilities for using multiple cursors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mc/malt ()
  "Shortcut for `mc/mark-all-symbols-like-this`."
  (interactive)
  (mc/mark-all-symbols-like-this))

(defun malt ()
  "Call `mc/mark-all-like-this` on word at point."
  (interactive)
  (er/mark-symbol)
  (mc/mark-all-symbols-like-this))

(defun mc/dalt ()
  "Shortcut for `mc/mark-all-symbols-like-this-in-defun`."
  (interactive)
  (mc/mark-all-symbols-like-this-in-defun))

(defun dalt ()
  "Call `mc/mark-all-like-this-in-defun` on word at point."
  (interactive)
  (er/mark-symbol)
  (mc/mark-all-symbols-like-this-in-defun))

(defun buffer-exists (bufname) (not (eq nil (get-buffer bufname))))

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
  (interactive "sEnter Regexp: ")
	(let ((killable-buffers (-filter '(lambda (b) (string-match r (buffer-name b))) (buffer-list))))
		(when (yes-or-no-p (format "Kill %d buffers matching \"%s\"? " (length killable-buffers) r))
			(mapc #'kill-buffer killable-buffers))))

;; ============================================================================

(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

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
  (if (buffer-exists "*vc-dir*")
      (progn (switch-to-buffer "*vc-dir*")
             (delete-other-windows))
    (when my-ediff-bwin-config (set-window-configuration my-ediff-bwin-config))))

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
(defun copy-file-name ()
  "Returns a random colour"
  (interactive)
  (message (concat "Copied " (buffer-file-name)))
  (kill-new (buffer-file-name)))

(defun copy-file-dir ()
  "Copies the files directory to the kill region"
  (interactive)
  (message (concat "Copied " (file-name-directory (buffer-file-name))))
  (kill-new (file-name-directory (buffer-file-name))))

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
  (random)
  (setq clr (nth (random (length (defined-colors))) (defined-colors)))
  (message "%s" (propertize "Ah ah ah, you didn't say the magic word!"
                            'face `(:foreground ,clr))))

(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (let ((a-tramp-file-p (string-match-p ":\\'" a))
                      (b-tramp-file-p (string-match-p ":\\'" b)))
                  (cond
                   ((and a-tramp-file-p b-tramp-file-p)
                    (string< a b))
                   (a-tramp-file-p nil)
                   (b-tramp-file-p t)
                   (t (time-less-p
                       (sixth (file-attributes (concat ido-current-directory b)))
                       (sixth (file-attributes (concat ido-current-directory a))))))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (setq i 1)
    (setq numWindows (count-windows))
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
        (setq i (1+ i)))))))

(defun find-top-level-dir (dir find-dir)
  (interactive)
  (let* ((parent (mapconcat 'identity (butlast (split-string dir "\\/")) "/")))
    (if (file-exists-p (concat parent "/" find-dir))
        (progn
          (setq ret nil)
          (find-top-level-dir parent find-dir))
      (setq ret dir))) ret)

(defun find-file-upwards (file-to-find &optional starting-path)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r (if starting-path
                     starting-path
                   default-directory))))

(defun clear-tags-table ()
  "Resets lists of tags files and deletes associated tags buffers"
  (interactive)
  ;; clear tags-table-list and buffers
  (dolist (f tags-table-list)
    (let ((b (get-file-buffer f)))
      (when b
        (kill-buffer b))))
  (setq tags-table-list nil)
  ;; clear tags-file-name and buffer
  (when tags-file-name
    (let ((b (get-file-buffer tags-file-name)))
      (when b
        (kill-buffer b)))
    (setq tags-file-name nil)))

(defun find-tags-file-upwards ()
  "Get and set the tags file"
  (interactive)
  (if (eq tags-table-list nil)
      (let ((my-tags-file (file-truename (find-file-upwards ".tags"))))
        (when my-tags-file
          (clear-tags-table)
          (message "Loading tags file: %s" my-tags-file)
          (visit-tags-table my-tags-file)))))

(defun replace-regexp-in-buffer (arg1 arg2)
  "Go to beginning of buffer and replace ARG1 with ARG2."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward arg1 nil t)
      (replace-match arg2))))

(defun replace-string-in-buffer (arg1 arg2)
  "Go to beginning of buffer and replace ARG1 with ARG2."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward arg1 nil t)
      (replace-match arg2))))

(defun semi-colon-end ()
  "Function to insert a semi colon at the end of the line from anywhere."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (end-of-line)
    (insert (format "%s" ";"))))

(defun swap-regions (beg1 end1 beg2 end2)
  "Swap region between BEG1 and END1 with region BEG2 and END2.

For the first region, mark the first region and set mark at
point.  The second region only needs to be marked normally.
Again, set the mark at the beginning and end of the first region,
then mark the second region with mark and point.

The order of the two regions in the buffer doesn't matter.
Either one can precede the other.  However, the regions can not
be swapped if they overlap.

All arguments can either be a number for a position in the buffer
or a marker."
  (interactive
   (if (< (length mark-ring) 2)
       (error "Not enough in mark-ring to swap a region")
     (let ((region (list (region-beginning) (region-end)))
     (marks (sort (list (marker-position (car mark-ring))
            (marker-position (cadr mark-ring)))
      '<)))
       (if (< (car region) (car marks))
     (append region marks)
   (append marks region)))))
  (if (or (and (< beg2 beg1) (< beg1 end2))
    (and (< beg1 beg2) (< beg2 end1)))
      (error "Unable to swap overlapping regions")
      (save-excursion
  (insert
   (prog1 (delete-and-extract-region beg2 end2)
     (goto-char beg2)
     (insert
      (delete-and-extract-region beg1 end1))
     (goto-char beg1))))))

(defun wrap-space-or-space ()
  (interactive)
  (if (use-region-p)
      (insert-pair 1 " " " " )
    (insert " ")))

(defun setup-cpp-mode ()
  (interactive)
  (let ((cb (get-buffer (buffer-name))))
    (get-buffer-create "*compilation*")
    (unless (buffer-exists "*cpp-runner*")
          (quick-term "cpp-runner"))
    (switch-to-buffer cb)
    (wlf:show cpp-layout)))

(defun setup-beacon-mode ()
  (interactive)
  (let ((cb (get-buffer (buffer-name))))
    (unless (buffer-exists "*gulp-watch*")
          (quick-term "gulp-watch"))
    (switch-to-buffer cb)
    (wlf:show beacon-layout)))

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters."
  (interactive)
  (save-excursion
    (progn
      (re-search-forward "[ \t\r\n]+" nil t)
      (replace-match "" nil nil))))


(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(global-set-key "\M-u" 'upcase-case-next-letter)
(defun upcase-case-next-letter ()
  "Toggle the case of the next letter, then move forward one character."
  (interactive)
  (upcase-region (point) (+ 1 (point)))
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
  (next-line 1)
  (yank))

(defun leet-mode ()
  "Turn off auto complete and major mode."
  (interactive)
  (auto-complete-mode 0)
  (smartparens-mode 0)
  (key-combo-mode 0)
  (flycheck-mode 0)
  (font-lock-mode 0))

(defun font-scale (op &optional amount)
  "Apply function operator OP (+/-) to scale face attribute height by AMOUNT."
  (let ((height (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (funcall op height (or amount 10)))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(defun elisp-debug (var)
  (interactive "sDebug variable: ")
	(insert (concat "(message \"" (capitalize var) ": %s\" " var ")")))

(defun magit-whitespace-cleanup ()
  (interactive)
  (let ((current-section (magit-current-section)))
    (when (eq 'file (magit-section-type current-section))
      (let* ((file-name (expand-file-name (magit-section-value current-section)))
             (file-buffer (or (get-file-buffer file-name) (create-file-buffer file-name))))
        (with-current-buffer file-buffer
          (goto-char (point-min))
          (whitespace-cleanup)
          (save-buffer file-buffer)))
      (magit-refresh))))

(defun you-can-never-leave ()
  (interactive)
  (shell-command "osascript -e \"tell application \\\"Terminal\\\" to do script \\\"mplayer ~/.emacs.d/elisp/youcanneverleave.wav && exit 0\\\"\"")
  (restart-emacs))

(provide 'functions)
;;; functions.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:
