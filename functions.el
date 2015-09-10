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

(defun insert-random-return ()
  (interactive) (insert (random-return)))

(defun random-return ()
  (setq words '("pow" "shazam" "foo" "bar" "wollop" "bam" "smash"
                "boom" "vrooom" "splat" "kapow" "krunch" "jabberwocky"
                "hooey" "mumbo" "jumbo" "borogrove" "bandersnatch"
                "snicker" "snack" "zorp" "fam" "tootle" "toot" "root"
                "muggle" "hokey" "cokey" "jiggery-pokery" "gazoo"
                "fiddlededee" "fiddle" "dedee" "blether" "doo" "ga"
                "claptrap" "fadoodle" "fa" "ranny" "zoo" "fa" "foo"
                "slip" "slop" "skittles" "tara" "taradiddle" "foodadoo"
                "thinga" "majiga" "thingamajiga" "ma" "jiga" "twaddle"
                ))
  (random t)
  (nth (random (length words)) words))

(defun js-hlt-nonused-dependencies ()
  "Will highlght the parts of the function include that are not used in the class"
  (interactive)
  (if (or (eq (buffer-mode (buffer-name)) 'js-mode) (eq (buffer-mode (buffer-name)) 'js2-mode) )
      (save-excursion
        (beginning-of-buffer)
        (let ((start) (end))
          (if (search-forward-regexp "function\\s-*(" (point-max) t)
              (progn (setq start (point))
                     (search-forward-regexp ")" (point-max) t)
                     (setq end (- (point) 1))
                     (mapc #'(lambda (arg)
                               (unhighlight-regexp (format "[^a-zA-Z]%s" arg))
                               (if (< (count-words-region start (point-max) arg) 2)
                                   (progn
                                     (message "%s is not used" arg)
                                     (highlight-regexp (format "[^a-zA-Z]%s" arg) 'js2-non-used)
                                     )))
                           (split-string (buffer-substring start end) ",\\s-*\n*\\s-*" t))))))))

(defun js-hlt-nonused-vars ()
  (interactive)
  (if (eq (buffer-mode (buffer-name)) 'js-mode)
      (save-excursion
        (beginning-of-buffer)
        (while (and (< (point) (point-max))
                    (search-forward-regexp "var\\s-*\\\w+" (point-max) t))
          (let ((temp-point (point))
                (arg (thing-at-point 'symbol)))
            (unhighlight-regexp (format "var\\s-*%s" arg))
            (if (< (count-words-region temp-point (point-max) arg) 1)
                (progn (message "%s is not used" arg)
                       (highlight-regexp (format "var\\s-*%s" arg) 'js2-non-used-var)))
            (goto-char temp-point))))))

(defun my-malabar-jump-to-thing ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (malabar-jump-to-thing (point)))

(defun malabar-start-find-parent ()
  "Tries to find the most appropriate parents of a java class"
  (interactive)
  (malabar-find-parent '("implements" "extends")))

(defun malabar-find-parent (types)
  "Takes a list of types and recurses through finding the classes"
  (if types
      (save-excursion
        (beginning-of-buffer)
        (search-forward-regexp (concat (car types) "\\s-*\\([a-z]+\\)") nil t)
        (if (match-string 1)
            (malabar-jump-to-thing (- (point) 1))
          (malabar-find-parent (cdr types))))
  (message "Could not find parent to go to" )))

(defun malabar-find-implementations ()
  "Finds implementations of a java interface"
  (interactive )
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "interface\\s-*\\([a-z]+\\)" nil t)
    (let ((match-string (match-string 1))
          (match-type (replace-regexp-in-string ".*\\\.\\(\\\w+\\)$" "\\1" (buffer-name))))
      (if match-string
          (progn (ring-insert find-tag-marker-ring (point-marker))
                 (pop-ack-and-a-half (concat "implements " match-string) match-type (repository-root) (current-buffer) t "--output=\" \""))
        (message "Class is not an interface")))))

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
                               (let ((found-file (concat (car (cdr record)) (car record))))
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
                              (let ((found-file (concat (car (cdr record)) (car record))))
                                (message "Found %s" found-file)
                                (find-file found-file))
                            (message "Couldn't find a cached file for %s..." file))))
                    projectable-project-alist)
          nil)))))

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
   (format "/Applications/IntelliJ\\ IDEA\\ 14\\ CE.app/Contents/MacOS/idea %s; osascript -e \"tell application \\\"IntelliJ Idea 14 CE\\\" to activate\"" (buffer-file-name))))

(defun open-in-and-activate-sublime ()
  "Opens the current file in Sublime!"
  (interactive)
  (shell-command
   (format "subl %s" (buffer-file-name))))

(defun open-current-file ()
  "Open the current file in different things."
  (interactive)
  (let ((type (ido-completing-read
               "Run which VC status manager: " '("IntelliJ IDEA" "Sublime Text") nil nil)))
    (cond ((string-equal type "Sublime Text")
           (open-in-and-activate-sublime))
          ((string-equal type "IntelliJ IDEA")
           (open-in-and-activate-intellj))
          ((string-equal type "Finder")
           (open-in-finder)))))

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

(defun alt-run-current-file ()
  (interactive)
  (let* ((fSuffix (file-name-extension (buffer-file-name))))
    (cond ((string-equal fSuffix "js")
           (grunt-spec))
          ((string-equal fSuffix "java")
           (set-up-test-watch 30 (format " -Dtest=%s"
                                         (file-name-sans-extension (file-name-base (buffer-file-name))))))
          ((string-equal (buffer-mode (buffer-name)) "latex-mode")
           (xelatex-make)))))

(defun run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is Emacs Lisp, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffixMap
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node")             ; node.js
            ("sh" . "bash")
            ("rs" . "rustc")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("cpp" . "make")
            ("tex" . "rubber --pdf")
            )
          )
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         (cmdStr (cond ((string-equal (buffer-mode (buffer-name)) "c++-mode")
                        (concat progName " \""  (file-name-sans-extension fName) "\"; " (file-name-sans-extension fName)))
                       ((string-equal (buffer-mode (buffer-name)) "rust-mode")
                        (concat progName " "  fName "; " (file-name-sans-extension fName) "; rm " (file-name-sans-extension fName)))
                       (t (concat progName " \"" fName "\""))
                       )))

    (message cmdStr)

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer) ) )

    (cond ((string-equal fSuffix "el") ; special case for emacs lisp
           (load (file-name-sans-extension fName)))
          ((string-equal fSuffix "tex")
           (find-file (concat (file-name-sans-extension fName) ".pdf")))
          ((string-equal fSuffix "js")
           (grunt-exec))
          ((string-equal fSuffix "java")
           (malabar-run-junit-test))
          (t (if progName
                 (progn
                   (message "Running…")
                   (shell-command cmdStr "*run-current-file output*" )
                   )
               (message "No recognized program file suffix for this file.")
               )))))

(defun pop-ack-and-a-half (search-string match-dir current-buf &optional pop args)
  (save-excursion
    (message search-string)
    (ack-and-a-half search-string t match-dir)
    (if pop
        (progn (sticky-window-delete-other-windows)
               (switch-to-buffer current-buf)
               (popwin:popup-buffer "*Ack-and-a-half*")
               (if (not (print truncate-lines))
                   (toggle-truncate-lines)))
      (progn (rotate-windows))
    )))

(defun quick-term (name)
  (interactive "sEnter terminal name : ")
  (ansi-term "/bin/bash" name))

(defun visit-ansi-term (name)
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((proc-buffer-name (concat "*" name "*")))
    (let ((is-term (string= "term-mode" major-mode))
          (is-running (term-check-proc proc-buffer-name))
          (term-cmd "/bin/bash")
          (anon-term (get-buffer proc-buffer-name)))
      (if is-term
          (if is-running
              (if (string= name (buffer-name))
                  (call-interactively 'rename-buffer)
                (if anon-term
                    (switch-to-buffer name)
                  (ansi-term term-cmd name)))
            (kill-buffer (buffer-name))
            (ansi-term term-cmd name))
        (if anon-term
            (if (term-check-proc proc-buffer-name)
                (switch-to-buffer proc-buffer-name)
              (kill-buffer proc-buffer-name)
              (ansi-term term-cmd name))
          (ansi-term term-cmd name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom utilities for using multiple cursors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mark-word-at-point ()
  "Mark the current word at point."
  (interactive)
  (progn
    (mark-word)
    (backward-word)))

(defun mc/malt ()
  "Shortcut for `mc/mark-all-symbols-like-this`."
  (interactive)
  (mc/mark-all-symbols-like-this))

(defun malt ()
  "Call `mc/mark-all-like-this` on word at point."
  (interactive)
  (progn
    (mark-word-at-point)
    (mc/mark-all-symbols-like-this)))

(defun mc/dalt ()
  "Shortcut for `mc/mark-all-symbols-like-this-in-defun`."
  (interactive)
  (mc/mark-all-symbols-like-this-in-defun))

(defun dalt ()
  "Call `mc/mark-all-like-this-in-defun` on word at point."
  (interactive)
  (progn
    (mark-word-at-point)
    (mc/mark-all-symbols-like-this-in-defun)))

(defun buffer-exists (bufname) (not (eq nil (get-buffer bufname))))

;; ============================================================================
(defun dgc-scroll-up-in-place (x)
  "Scroll (up) X lines in the file maintaining cursor position in window."
  (interactive)
  (scroll-up x)
  (next-line x))

;; ============================================================================
(defun dgc-scroll-down-in-place (x)
  "Scroll down X lines in the file maintaining cursor position in window."
  (interactive)
  (scroll-down x)
  (previous-line x))

;; ============================================================================
(defun dgc-comment ()
  "comment or uncomment highlighted region or line"
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun close-all-buffers ()
  "Kills all open buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

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

(defun vc-dir-kill-all-lines-at-mark ()
  "remove all files with certain status"
  (interactive)
  (backward-word)
  (let ((marked-word (thing-at-point 'word)))
    (if (eq (buffer-mode (buffer-name)) "vc-dir-mode")
        (message marked-word)
      (save-excursion
        (beginning-of-buffer)
        (while (search-forward marked-word) (vc-dir-kill-line))))))

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

(defun random-hex ()
  "Return a string in the form of #FFFFFF. Choose the number for
   #xffffff randomly using Emacs Lisp's builtin function (random)."
  ;; seed our random number generator: current datetime plus Emacs's
  ;; process ID
  (interactive)
  (random t)
  (message "%s" (format "#%06x" (random #xffffff))))

;; ============================================================================
(defun ahahah ()
  "You know what it displays..."
  (interactive)
  (random t)
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

(defun format-member-vars (var-list &optional UNDERSCORES)
  (interactive)
  (format "\n\t\t%s\n"
          (mapconcat
           #'(lambda (arg)
               (if UNDERSCORES (format "\t\tvar _%s;" arg) (format "\t\tvar %s;" arg)))
           (split-string var-list ",\\s-*" t) "\n")))

(defun format-member-vars-single-line (var-list &optional UNDERSCORES)
  (interactive)
  (format "\n\t\tvar %s;\n"
          (mapconcat
           #'(lambda (arg)
               (if UNDERSCORES (format "_%s" arg) (format "%s" arg)))
           (split-string var-list ",\\s-*" t) ", ")))

(defun format-init-members (var-list &optional UNDERSCORES)
  (interactive)
  (mapconcat
   #'(lambda (arg)
       (if UNDERSCORES (format "_%s = %s;" arg arg) (format "this.%s = %s;" arg arg)))
   (split-string var-list ",\\s-*" t) "\n\t\t\t\t"))

(defun format-getters (var-list &optional UNDERSCORES)
  (interactive)
  (mapconcat
   #'(lambda (var)
       (format ",\n\n%s" (inject-getter var UNDERSCORES)))
   (split-string var-list ",\\s-*" t) ""))

(defun inject-getter (var-name &optional UNDERSCORES)
  (interactive)
  (if (not (boundp 'UNDERSCORES))
      (setq UNDERSCORES nil))
  (with-temp-buffer
    (js-mode)
    (insert "reqgetter")
    (yas-expand)
    (insert (upcase-initials var-name))
    (yas-next-field)
    (insert (if UNDERSCORES (concat "_" var-name ) var-name))
    (format "%s" (buffer-substring (point-min) (point-max)))))

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
          (ac-etags-clear-cache)
          (message "Loading tags file: %s" my-tags-file)
          (visit-tags-table my-tags-file)))
    (progn (ac-etags-setup)
           (ac-etags-ac-setup))))

(defun jds-find-tags-file ()
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (progn
    (defun find-tags-file-r (path)
      "find the tags file from the parent directories"
      (let* ((parent (file-name-directory path))
             (possible-tags-file (concat parent ".tags")))
        (cond
         ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
         ((string= "/TAGS" possible-tags-file) (error "no tags file found"))
         (t (find-tags-file-r (directory-file-name parent))))))

    (if (buffer-file-name)
        (catch 'found-it
          (find-tags-file-r (buffer-file-name)))
      (error "buffer is not visiting a file"))))

(defun jds-set-tags-file-path ()
  "calls `jds-find-tags-file' to recursively search up the directory tree to find
a file named 'TAGS'. If found, set 'tags-table-list' with that path as an argument
otherwise raises an error."
  (interactive)
  (setq tags-table-list (cons (jds-find-tags-file) tags-table-list)))

(defun auto-type-string (string)
  (interactive)
  (mapc #'(lambda (letter) (execute-kbd-macro letter)) (split-string string ""))
  (execute-kbd-macro [return]))

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

(defun generate-project-file (project-name &optional project-path src-path test-path test-ext test-cmd)
  (let ((project-dir "~/Documents/Projects/")
        (template-file "~/Documents/Projects/.projectTemplate")
        (result ""))
    (if (file-exists-p (concat project-dir project-name ".json"))
        (delete-file (concat project-dir project-name ".json")))
    (with-temp-buffer
      (insert-file-contents template-file)
      (setq result (replace-regexp-in-string "%PROJECT_NAME%" project-name (buffer-string) t t))
      (setq result (replace-regexp-in-string "%PROJECT_PATH%" project-path result t t))
      (setq result (replace-regexp-in-string "%SRC%" src-path result t t))
      (setq result (replace-regexp-in-string "%TEST%" test-path result t t))
      (setq result (replace-regexp-in-string "%EXT%" test-ext result t t))
      (setq result (replace-regexp-in-string "%TEST_CMD%" test-cmd result t t)))
    (with-temp-file (concat project-dir project-name ".json")
      (insert result)) t))

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

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters."
  (interactive)
  (save-excursion
    (progn
      (re-search-forward "[ \t\r\n]+" nil t)
      (replace-match "" nil nil))))

(global-set-key "\M-u" 'upcase-case-next-letter)

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

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

(provide 'functions)
;;; functions.el ends here
