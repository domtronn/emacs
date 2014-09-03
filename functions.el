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

(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth))
  (if (not (frame-parameter nil 'fullscreen))
      (progn (tool-bar-mode 1)
             (tool-bar-mode -1)
             (set-frame-height (selected-frame) 69))))

(defun f1-toggle-fullscreen ()
  (interactive)
  (toggle-fullscreen))

(defun f2-remove-tool-bar ()
  "Remove the toolbar"
  (interactive)
  (tool-bar-mode 1)
  (tool-bar-mode -1))

(defun f3-true-fullscreen ()
  "Makes true fullscreen"
  (interactive)
  (let* ((res-alist '((800 . 53) (1050 . 70))))
    (set-frame-height (selected-frame) (cdr (assoc (x-display-pixel-height) res-alist)))))

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

(defun grunt-spec ()
  "Run grunt"
  (interactive)
  (if (buffer-exists "*grunt*")
      (kill-buffer "*grunt*"))
  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
         (result (call-process-shell-command
                  (concat "grunt spec --config=" (find-file-upwards "gruntfile.js") " --filter=" (buffer-name)) nil grunt-buffer t)))))

(defun inject-javascript-dependency ()
  (interactive)
  (let ((class-name (thing-at-point 'word)))
    (save-excursion
      (beginning-of-buffer)
      (let ((aStart (search-forward-regexp "\\s-*\\[\n\\s-*"))
            (aEnd (- (search-forward-regexp "\\s-*\\]") 1))
            (bStart (search-forward-regexp "function\\s-*("))
            (bEnd (- (search-forward ")") 1)))
        (let ((pos (position class-name
                             (split-string (buffer-substring bStart bEnd) ",\\s-*\n*\\s-*" t)
                             :test #'string-equal)))
          (beginning-of-buffer)
          (let ((my-point
                 (search-forward (nth (- pos 1) (split-string (buffer-substring aStart aEnd) ",\\s-*\n\\s-*" t)))))
            (message "%s" pos)
            (message "%s" (length (split-string (buffer-substring aStart aEnd) ",\\s-*\n*\\s-*" t)))
            (if (eq pos (length (split-string (buffer-substring aStart aEnd) ",\\s-*\n*\\s-*" t)))
                (progn (message "Setting q backwards") (setq my-point (+ 1(search-backward "\"")))))
            (progn (goto-char my-point)
                   (insert (format ",\n%s" (inject-dependency class-name))))))))))

(defun update-javascript-dependency ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((bStart (search-forward-regexp "function\\s-*("))
          (bEnd (- (search-forward ")") 1)))
      (beginning-of-buffer)
      (replace-regexp "\\[\\s-*\n\\(['\\\"a-z/, \n\t\r]*\\)\\s-\\]"
                      (format "\[\n%s\n\t\]" (inject-dependency (buffer-substring bStart bEnd)))))))

(defun get-last-spy ()
  (interactive)
  (save-excursion
    (let ((start (+ (search-backward-regexp "spyOn(" 6)))
          (end (- (search-forward-regexp ")") 1)))
      (if (string-match "spyOn\(\\\(.*\\\),\\s-*['\"]\\\(.*?\\\)['\"]" (buffer-substring start end))
          (format "%s.%s" (match-string 1 (buffer-substring start end)) (match-string 2 (buffer-substring start end)))))))

(defun inject-dependency (dep-list)
  (interactive)
  (mapconcat
   #'(lambda (arg)
       (let ((res-require-path nil))
         (maphash #'(lambda (id assoc-list)
                      (let ((record (assoc (concat (downcase arg) ".js") assoc-list)))
                        (if (eq res-require-path nil)
                            (setq res-require-path
                                  (if (= (length record) 2)
                                      (concat (replace-regexp-in-string ".*script" id (cadr record))
                                              (file-name-sans-extension (car record)))
                                    nil)))))
                  external-cache-hash)
         (format "\t\t\"%s\""
                 (if (eq res-require-path nil)
                     (concat "???/" (downcase arg))
                   res-require-path))))
   (split-string dep-list ",\\s-*\n*\\s-*" t) ",\n"))

(defun insert-random-return ()
  (interactive)
  (let ((word (random-return)))
    (insert word)))

(defun random-return ()
  (interactive)
  (setq words '("pow" "shazam" "foo" "bar" "wollop" "bam" "smash"
                "boom" "vrooom" "splat" "kapow" "krunch" "jabberwocky"
                "hooey" "mumbo" "jumbo" "borogrove" "bandersnatch"
                "mimsy" "snicker" "snack" "vorpal" "wabe" "zorp" "fam"
                "horcrux" "muggle" "shiwu" "hokey" "cokey"))
  (random t)
  (message (nth (random (length words)) words)))

(defun browse-sandbox ()
  (interactive)
  (browse-url "http://pal.sandbox.dev.bbc.co.uk/sprtiptvjs/?brand=chrome&model=20_0&config=beta"))

(defun js-hlt-nonused-dependencies ()
  "Will highlght the parts of the function include that are not used in the class"
  (interactive)
  (if (eq (buffer-mode (buffer-name)) 'js-mode)
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

(defun collapse-all-its ()
  (interactive)
	(save-excursion
		(beginning-of-buffer)
		(while (and (< (point) (point-max))
								(search-forward-regexp "[a-z]+Each.*{" (point-max) t))
			(hs-hide-block))
		(beginning-of-buffer)
		(while (and (< (point) (point-max))
								(search-forward-regexp "it(\".+\"" (point-max) t))
			(end-of-line)
			(hs-hide-block))))

(defun collapse-all-functions ()
  (interactive)
	(save-excursion
		(beginning-of-buffer)
		(while (and (< (point) (point-max))
								(search-forward-regexp "[a-zA-Z]+: function (" (point-max) t))
			(end-of-line)
			(hs-hide-block))))

(defun count-words-buffer (arg)
  (interactive "r")
  (save-excursion
    (let (wordCount)
      (setq wordCount 0)
      (beginning-of-buffer)
      (while (and (< (point) (point-max))
                  (search-forward arg (point-max) t))
        (setq wordCount (1+ wordCount)))
      wordCount)))

(defun count-words-region (posStart posEnd arg)
  (interactive "r")
  (save-excursion
    (let (wordCount)
      (setq wordCount 0)
      (goto-char posStart)
      (while (and (< (point) posEnd)
                  (search-forward arg posEnd t))
        (if (string-equal arg (thing-at-point 'symbol))
            (setq wordCount (1+ wordCount))))
      wordCount)))

(defun my-malabar-jump-to-thing ()
  (interactive)
	(ring-insert find-tag-marker-ring (point-marker))
  (malabar-jump-to-thing (point)))

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
            (maphash #'(lambda (id file-alist)
                         (let ((record (assoc (concat (downcase class-name) ".js") file-alist)))
                           (if (and record (= (length record) 2))
                               (let ((found-file (concat (car (cdr record)) (car record))))
                                 (message "Found %s" found-file)
                                 (find-file found-file))
                             (message "Couldn't find a cached file for %s..." class-name))))
                     external-cache-hash)
          nil)))))

(defun go-to-thing-at-point ()
  "Go to the thing at point assuming if it's not a class or function it's a variable"
  (interactive)
  (let ((thing (thing-at-point 'symbol))
        (thing-point (point)))
    (if (eq nil (go-to-class))
        (if (eq nil (search-backward-regexp
                     (format "var\\s-*%s" thing) (point-min) t))
            (if (eq nil (search-backward-regexp
                         (format "function\\s-*(.*%s" thing) (point-min) t))
                (progn
                  (search-backward ".")
                  (let ((thing-clazz (thing-at-point 'symbol)))
                    (goto-char thing-point)
                    (etags-select-find-tag-at-point)
                    (if (string-equal "*etags-select*" (buffer-name))
                        (progn
                          (search-forward-regexp (format  "%s.js" (downcase thing-clazz)))
                          (next-line)
                          (etags-select-goto-tag)))))
              (progn
                (search-forward thing)
                (backward-word)))))))


(defun close-and-pop-buffer (oldbuffer buffer)
  (switch-to-buffer oldbuffer)
  (popwin:popup-buffer buffer))

(defun occur-at-point ()
  "Run occur on a thing."
  (interactive)
  (occur (thing-at-point 'symbol))
  (toggle-read-only))

(defun open-in-and-activate-intellj ()
  "Opens the current file in intellij for n00b5!"
  (interactive)
  (shell-command
   (format "idea %s; osascript -e \"tell application \\\"IntelliJ Idea 13\\\" to activate\"" (buffer-file-name))))

(defun buffer-mode (buffer-or-string)
  "Return the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defun my-isearch-yank-word-or-char-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'my-isearch-yank-word-or-char-from-beginning
                             'isearch-yank-word-or-char
                             isearch-mode-map))

(add-hook 'isearch-mode-hook
          (lambda ()
            "Activate my customized Isearch word yank command."
            (substitute-key-definition 'isearch-yank-word-or-char
                                       'my-isearch-yank-word-or-char-from-beginning
                                       isearch-mode-map)))

(defun my-vc-dir ()
  "calls vc-dir on the appropriate project"
  (interactive)
  (vc-dir (repository-root)))

(defun alt-run-current-file ()
  (interactive)
  (let* ((fSuffix (file-name-extension (buffer-file-name))))
    (cond ((string-equal fSuffix "js")
           (grunt-spec))
					((string-equal fSuffix "java")
           (set-up-test-watch 30))
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
                       (t (concat progName " \"" fName "\""))
                       )))

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

(defun set-up-ack-results ()
  "Opens a pop up for rgrep results"
  (interactive )
  (save-excursion
    (progn
      (let ((match-string (thing-at-point 'symbol))
            (match-type (concat "*" (replace-regexp-in-string ".*\\(\\\.\\\w+\\)$" "\\1" (buffer-name))))
            (match-dir (repository-root))
            (current-buf (current-buffer)))
        (ack-and-a-half match-string t match-dir)
        (sticky-window-delete-other-windows)
        (switch-to-buffer current-buf)
        (popwin:popup-buffer "*Ack-and-a-half*")
        (if (not (print truncate-lines))
            (toggle-truncate-lines))))))

(defun set-up-ack-results-with-prompt (search-string match-type)
  "Opens a pop up for rgrep results with a search string"
  (interactive "sEnter search string : \nsEnter file type : ")
  (save-excursion
    (progn
      (let ((match-dir (repository-root))
            (current-buf (current-buffer))
            (old-ack-args ack-and-a-half-arguments))
        (setq ack-and-a-half-arguments (list "--color-match=green" "--color-match=yellow" (concat "--" match-type)))
        (ack-and-a-half search-string t match-dir)
        (setq ack-and-a-half-arguments old-ack-args)
        (sticky-window-delete-other-windows)
        (switch-to-buffer current-buf)
        (popwin:popup-buffer "*Ack-and-a-half*")
        (if (not (print truncate-lines))
            (toggle-truncate-lines))))))

(defun create-tags (dir-name)
  "Create tags file."
  (let* ((cmd (format "find %s -type f -follow | grep -E \"\\\.groovy$|\\\.rb$|\\\.scala$|\\\.js$|\\\.java$\" | grep -vE \"\\\.min\\\.js$|\\\\/node_modules\\\\/|\\\\/build\\\\/|\\\\/bdd-api\\\\/|\\\\/test\\\\/|\\\\/script-tests\\\\/|\\\\/docs\\\\/\" | xargs ctags -f %s/.tags -e" dir-name dir-name)))
    (shell-command cmd)))

(defun create-tags-for-project ()
  "Creates tags files in the base of each project module in PROJECTPATH"
  (interactive)
  (mapc
   'create-tags
   (split-string (shell-command-to-string (concat "cat " PROJECTPATH)) "\n" t)))

(defun go-to-emms-playlist ()
  (interactive)
	(if (not (eq nil (buffer-exists " *EMMS Playlist*")))
			(progn 
				(if (not (eq nil (string-match "Browsing by:" (buffer-name (current-buffer)))))
						(delete-window))
				(if (string-equal (buffer-name (current-buffer)) " *EMMS Playlist*")
						(delete-window)
					(if (buffer-exists " *EMMS Playlist*")
							(progn
								(delete-other-windows)
								(split-window-horizontally)
								(enlarge-window-horizontally 50)
								(switch-to-buffer " *EMMS Playlist*")
								(rotate-windows)
								(other-window 1)
								(set-window-dedicated-p (get-buffer-window) t)
								(setq window-size-fixed t)))))))

(defun get-lyrics-and-display ()
  (interactive)
  (let ((prev-buffer-name (buffer-name (current-buffer))))
    (if (or (string-match "Browsing by:" (buffer-name (current-buffer)))
            (string-equal (buffer-name (current-buffer)) " *EMMS Playlist*"))
        (progn
          (delete-window)
          (setq prev-buffer-name (buffer-name (current-buffer)))))
    (other-window 1)
    (if (not (eq nil (string-match "Lyrics:" (buffer-name (current-buffer)))))
        (progn
          (other-window 1)
          (delete-other-windows))
			(if emms-player-playing-p
					(progn
						(delete-other-windows)
						(emms-get-lyrics-current-song)
            (if (string-match "Lyrics" (buffer-name (current-buffer)))
              (progn
                (switch-to-buffer prev-buffer-name)
                (split-window-horizontally)
                (enlarge-window-horizontally 50)
                (other-window 1)
                (emms-get-lyrics-current-song)
                (set-window-dedicated-p (get-buffer-window) t)
                (setq window-size-fixed t)
                (other-window 1))
              (progn
                (message "EMMS could not find lyrics")
                (switch-to-buffer prev-buffer-name))))
        (message "EMMS is currently not playing.")))))

(defun go-to-emms-browser ()
  (interactive)
  (let ((prev-buffer-name (buffer-name (current-buffer))))
    (if (not (eq nil (buffer-exists " *EMMS Playlist*")))
        (progn
          (if (string-equal (buffer-name (current-buffer)) " *EMMS Playlist*")
              (delete-window))
          (if (not (eq nil (string-match "Browsing by:" (buffer-name (current-buffer)))))
              (delete-window)
            (progn
              (if (buffer-exists "Browsing by: artist")
                  (switch-to-buffer "Browsing by: artist"))
              (if (buffer-exists "Browsing by: album")
                  (switch-to-buffer "Browsing by: album"))
              (if (buffer-exists "Browsing by: genre")
                  (switch-to-buffer "Browsing by: genre"))
              (if (or (string-equal prev-buffer-name (buffer-name (current-buffer)))
                      (string-equal "*Messages*" (buffer-name (current-buffer))))
                  (message "EMMS is currently not configured")
                (progn (switch-to-buffer prev-buffer-name)
                       (delete-other-windows)
                       (split-window-horizontally)
                       (enlarge-window-horizontally 50)
                       (other-window 1)
                       (if (buffer-exists "Browsing by: artist")
                           (switch-to-buffer "Browsing by: artist"))
                       (if (buffer-exists "Browsing by: album")
                           (switch-to-buffer "Browsing by: album"))
                       (if (buffer-exists "Browsing by: genre")
                           (switch-to-buffer "Browsing by: genre"))
                       (set-window-dedicated-p (get-buffer-window) t)
                       (setq window-size-fixed t)))))))))

(defun set-up-test-watch (&optional user-size)
  "Sets the display ready for grunt watching"
  (interactive)
  (save-excursion
    (let ((size 50))
      (progn
				(if user-size
						(setq size (- size user-size)))
        (delete-other-windows)
        (split-window-horizontally)
        (enlarge-window-horizontally size)
        (other-window 1)
        (visit-ansi-term "ansi-term")
        (if (not (eq nil project-test-cmd))
            (auto-type-string project-test-cmd)
          )
        (set-window-dedicated-p (get-buffer-window) t)
        (setq window-size-fixed t)
        (other-window 1)))))

(defun set-up-term-and-run (name size cmd)
	"Sets up an ansi terminal named name and then runs cmd"
	  (save-excursion
    (let (running (buffer-exists name))
      (progn
        (delete-other-windows)
        (split-window-horizontally)
        (enlarge-window-horizontally (- 50 size))
        (other-window 1)
        (visit-ansi-term name)
				(auto-type-string cmd)
        (set-window-dedicated-p (get-buffer-window) t)
        (setq window-size-fixed t)
        (other-window 1)))))
  

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

(defun mark-word-at-point ()
  "Marks the current word at point"
  (interactive)
  (progn
    (mark-word)
    (backward-word)))

(defun mc/malt ()
  (interactive)
  (mc/mark-all-like-this))

(defun malt ()
  "Uses mc/mark-all-like-this at point"
  (interactive)
  (progn
    (mark-word-at-point)
    (mc/mark-all-symbols-like-this)))

(defun mc/dalt ()
  (interactive)
  (mc/mark-all-like-this-in-defun))

(defun dalt ()
  "Uses mc/mark-all-like-this-in-defun at point"
  (interactive)
  (progn
    (mark-word-at-point)
    (mc/mark-all-like-this-in-defun)))

;; ============================================================================
(defun domtronn-timestamp ()
  "Insert history entry"
  (interactive)
  (insert (format-time-string "%d-%b-%Y")))

;; ============================================================================
(defun domtronn-sign ()
  "Insert my name and data"
  (interactive)
  (insert "-- Dom Charlesworth (dgc336@gmail.com)\n   ")
  (domtronn-timestamp))

(defun buffer-exists (bufname) (not (eq nil (get-buffer bufname))))

(defun go-to-calendar ()
  "Goes to the *grep* buffer and runs the rgrep command"
  (interactive)
  (progn
    (if (not (eq (buffer-name (current-buffer)) (buffer-name (get-buffer "*Calendar*"))))
        (switch-to-buffer-other-frame (get-buffer "*Calendar*")))
    (calendar)))

(defun go-to-agenda ()
  "Goes to the *grep* buffer and runs the rgrep command"
  (interactive)
  (if (buffer-exists "*Org Agenda*")
      (switch-to-buffer (get-buffer "*Org Agenda*"))
    (org-agenda-list)))

;; ============================================================================
(defun domtronn-sign-professional ()
  "Insert my name and data"
  (interactive)
  (insert "-- Dominic Charlesworth (Dominic.Charlesworth@bbc.co.uk)\n   ")
  (domtronn-timestamp))

;; ============================================================================
(defun dgc-scroll-up-in-place (x)
  "Scroll (up) one line in the file maintaining cursor position in window"
  (interactive)
  (scroll-up x)
  (next-line x))

;; ============================================================================
(defun dgc-scroll-down-in-place (x)
  "Scroll down one line in the file maintaining cursor position in window"
  (interactive)
  (scroll-down x)
  (previous-line x))

;; ============================================================================
(defun dgc-forward-word ()
  "Move one word forward. Leave the pointer at start of word"
  (interactive)
  (forward-char 1)
  (backward-word 1)
  (forward-word 2)
  (backward-word 1)
  (backward-char 1)
  (cond ((looking-at "_") (forward-char 1) (dgc-forward-word))
        (t (forward-char 1))))

;; ============================================================================
(defun dgc-forward-word-2 ()
  "Move one word forward. Leave the pointer at end of word"
  (interactive)
  (forward-word 1))

;; ============================================================================
(defun dgc-backward-word ()
  "Move one word backward. Leave the pointer at start of word"
  (interactive)
  (backward-word 1)
  (backward-char 1)
  (cond ((looking-at "_") (dgc-backward-word))
        (t (forward-char 1))))

;; ============================================================================
(defun dgc-backward-word-2 ()
  "Move one word backward. Leave the pointer at end of word"
  (interactive)
  (backward-word 2)
  (forward-word 1))

;; ============================================================================
(defun dgc-indent-buffer ()
  "indents whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

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
  (kill-new (buffer-file-name)))

(defun random-hex ()
  "Return a string in the form of #FFFFFF. Choose the number for
   #xffffff randomly using Emacs Lisp's builtin function (random)."
  ;; seed our random number generator: current datetime plus Emacs's
  ;; process ID
  (interactive)
  (random t)
  (message "%s" (format "#%06x" (random #xffffff)))
  )

;; ============================================================================
(defun ahahah ()
  "You know what it displays..."
  (interactive)
  (random t)
  (setq clr (nth (random (length (defined-colors))) (defined-colors)))
  (message "%s" (propertize "Ah ah ah, you didn't say the magic word!"
                            'face
                            `(:foreground ,clr))))

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
      (setq ret dir))
    )
  ret)

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

(defun add-file-to-project-cache ()
  (interactive)
  (if (and (not (assoc-string (file-name-nondirectory (buffer-file-name)) file-cache-alist))
           (find-file-upwards ".tags")
           (eq (buffer-mode (buffer-name)) 'js-mode)
           (not (string-match "Spec" (buffer-name))))
      (progn
        (message "[filecache] Adding %s to the file cache..." (buffer-file-name))
        (push (list (file-name-nondirectory (buffer-name)) (file-name-directory (buffer-file-name))) file-cache-alist)))
  t)

(defun add-file-to-ext-lib-cache ()
  (interactive)
  (let ((lib-cache (gethash project-id external-cache-hash))
        (temp-file-cache-alist file-cache-alist))
    (if (and (boundp 'project-id)
             (not (eq project-id nil))
             (not (assoc-string (file-name-nondirectory (buffer-file-name)) (gethash project-id external-cache-hash)))
             (find-file-upwards ".filecache")
             (eq (buffer-mode (buffer-name)) 'js-mode))
        (progn
          (message "[filecache] Adding %s to the external library cache..." (buffer-file-name))
          (push
           (list (file-name-nondirectory (buffer-file-name))
                 (file-name-directory (buffer-file-name)))
           (gethash project-id external-cache-hash))
          (setq file-cache-alist (gethash project-id external-cache-hash))
          (file-cache-save-cache-to-file (find-file-upwards ".filecache"))
          (setq file-cache-alist temp-file-cache-alist))))
  t)



(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (labels
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
    (find-file-r default-directory)))

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

(defun open-test ()
  "replace name "
  (interactive)
  (let ((file-ext (file-name-extension (buffer-file-name)))
				(src-path (concat "/" project-src-path "/"))
				(test-path (concat "/" project-test-path "/")))
    (if (string-match project-test-path (file-truename ( buffer-file-name )))
        (find-file (replace-regexp-in-string test-path src-path
                                             (replace-regexp-in-string (concat project-test-ext "\\\." file-ext) (concat "\." file-ext) (buffer-file-name))))
      (find-file
       (replace-regexp-in-string src-path test-path
                                 (replace-regexp-in-string (concat "\\\." file-ext) (concat project-test-ext "\." file-ext) (buffer-file-name)))))))

(defun grunt ()
  "Run grunt"
  (if (buffer-exists "*grunt*")
      (kill-buffer "*grunt*"))
  (interactive)
  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
         (result (call-process-shell-command (concat "grunt test --no-color --config=" (find-file-upwards "gruntfile.js") " --filter=" (buffer-name)) nil grunt-buffer t))
         (output (with-current-buffer grunt-buffer (buffer-string))))
    (cond ((zerop result)
           (message "Grunt completed without errors"))
          (t
           (message nil)
           (delete-windows-on "*grunt*")
           (popwin:popup-buffer "*grunt*")))))

(defun grunt-this-test-file ()
  "Run grunt"
  (interactive)
  (if (buffer-exists "*grunt*")
      (kill-buffer "*grunt*"))
  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
         (result (call-process-shell-command
                  (concat "grunt test --no-color --config=" (find-file-upwards "gruntfile.js") " --filter=" (buffer-name)) nil grunt-buffer t))
         (output (with-current-buffer grunt-buffer (buffer-string))))
    (cond ((zerop result)
           (message "Grunt completed without errors"))
          (t
           (message nil)
           (delete-windows-on "*grunt*")
           (popwin:popup-buffer "*grunt*")))))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(defun format-code ()
  "Prints cool message if in javascript mode"
  (interactive)
  (if (eq (buffer-mode (buffer-name)) 'js-mode)
      (jshint-code))
  nil)

(defun jshint-code ()
  "Reformats the code to go with jshint."
  (interactive)
  (save-excursion
    (replace-regexp-in-buffer "\\(\\\w+\\)\\\s+(" "\\1(") ; Add space between function and parantheses
    (replace-regexp-in-buffer "function(" "function (") ; Add space between function and parantheses
    (replace-regexp-in-buffer "switch(" "switch (") ; Add space between switch and parantheses
    (replace-regexp-in-buffer "if(" "if (") ; Add space between if and parantheses
    (replace-regexp-in-buffer "for(" "for (") ; Add space between for and parantheses
    (replace-regexp-in-buffer "){" ") {") ; Add space between close paren and open brace
    (replace-regexp-in-buffer "{}" "{ }") ; Add space between open-close braces
    (replace-regexp-in-buffer "'\\(.*?\\)'" "\"\\1\"") ; Replace single quotes with double quotes
    (replace-regexp-in-buffer "\\(\\\w+\\) : function" "\\1: function") ; Remove space before colon of function
    (replace-regexp-in-buffer "\(\\\s+\\(.*\\)\)" "\(\\1\)") ; Remove whitespace around arguments
    (replace-regexp-in-buffer "\\(\\\w+\\)\\\s+\)" "\\1\)")
    ;; (replace-regexp-in-buffer "\(\\(\\\w.*\\)\)" "\( \\1 \)") ; Add space around method arguments
    (replace-regexp-in-buffer "\\(\\\w+\\),\\\s+\\(\\\w+\\)" "\\1, \\2") ; Remove whitespace between csv
    (replace-regexp-in-buffer "\\(\\\w+\\),\\(\\\w+\\)" "\\1, \\2") ; Replace word,word with word, word
    (replace-regexp-in-buffer "[ \t]*$" "") ; Remove trailing whitespace
    (replace-regexp-in-buffer "\\(^[ \t]*\n[ \t]*$\\)+" "") ; Remove double whitespace
    (replace-regexp-in-buffer "{.*\n\\(\n\\)+\\(.*return.*\n\\)\\(\n\\)+\\(.*}\\)" "{\n\\2\\4") ; Have get methods in single lines
    (replace-regexp-in-buffer "\\\s+;" ";") ; Replace whitespace before semi-colon
    (replace-regexp-in-buffer "}\\\s+," "},") ; Replace whitespace between end brace and comma
    (replace-regexp-in-buffer "\\\s+$" "") ; Remove trailing whitespace
    (replace-regexp-in-buffer "    " "  ") ; Reformat for the use of tabs over spaces
    ))

(defun js2r--extract-method ()
  (interactive)
  (js2-mode)
  (call-interactively (js2r-extract-method))
  (js-mode))

(defun auto-type-string (string)
  (interactive)
  (mapc #'(lambda (letter) (execute-kbd-macro letter)) (split-string string ""))
  (execute-kbd-macro [return])
  nil)

(defun load-custom-theme (arg)
  (interactive (list (read-file-name "Enter path to Project file: " (concat USERPATH "/emacs.packages/themes/"))))
  (load-file arg))

(defun replace-regexp-in-buffer (arg1 arg2)
  "Goes to beginning of buffer for each replace-regexp"
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp arg1 arg2)))

(defun replace-string-in-buffer (arg1 arg2)
  "Goes to beginning of buffer for each replace-regexp"
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp arg1 arg2)))

(defun add-quotation ()
  (interactive)
  (if (use-region-p)
			(save-excursion
				(insert "\"")
				(goto-char (region-end))
				(insert "\""))))

(defun add-brace ()
	(interactive)
  (if (use-region-p)
			(save-excursion
				(insert "{")
				(goto-char (region-end))
				(insert "}"))))

(defun add-bracket ()
	(interactive)
  (if (use-region-p)
			(save-excursion
				(insert "[")
				(goto-char (region-end))
				(insert "]"))))

(defun create-project (type)
  (interactive (list (ido-completing-read "Project Type : " '("Java" "JavaScript" "Ruby"))))
	(let ((project-path (read-directory-name "Project Location : " "~/code/"))
				(project-name (read-string "Project Name : " (concat "my-"(downcase type)"-app")))
				(temp-buffer-name (concat (downcase type) "-project-setup"))
				(create t))
		(message "%s %s" project-path project-name)
		(if (file-exists-p (concat project-path project-name))
				(if (yes-or-no-p (concat "Project " project-path project-name " already exists. Would you like to replace it?"))
						(shell-command (concat "rm -rf " project-path project-name))
					(setq create nil)))
		(cond ((string-equal type "Java")
					 (if create
							 (progn
								 (message "Building project...")
								 (set-up-term-and-run
									15 temp-buffer-name
									(concat
									 "cd " project-path "; clear; mvnc archetype:generate -DgroupId=com."
									 (replace-regexp-in-string "-" "" (downcase project-name)) " -DartifactId="
									 project-name " -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false | "
									 "grep -v ^Downloading; sed -i '' -e 's/3\.8\.1/4\.8\.1/g' " project-path project-name "/pom.xml"
									 "; emacsclient -e \"(progn (kill-buffer \\\"*" temp-buffer-name "*\\\") (message \\\""project-name" has been created successfully\\\"))\""))
								 (generate-project-file project-name project-path "src/main/java" "src/test/java" "Test" "mvnc test --quiet")
								 )))
					((string-equal type "Ruby")
					 (message "I'm sorry, but ruby project creation has not been implemented yet! It'll just use rails though"))
					((string-equal type "JavaScript")
					 (if create
							 (progn
								 (message "Building project...")
								 (set-up-term-and-run
									15 temp-buffer-name
									(concat "cd " project-path
													"; mkdir " project-name "; cd " project-name
													"; yo requirejs-jasmine-karma:" project-name
													"; emacsclient -e \"(progn (kill-buffer \\\"*" temp-buffer-name "*\\\") (message \\\""project-name" has been created successfully\\\"))\""))
								 (other-window 1)
								 (auto-type-string "")
								 (auto-type-string "")
								 (generate-project-file project-name project-path "app" "test" "Spec" "grunt test")))))))

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

(defun convert-css (from to)
  (interactive "nConvert from resolution : \nnTo resolution : ")
	(let ((factor (/ (float to) (float from))))
		(save-excursion
			(beginning-of-buffer)
			(while (and (< (point) (point-max))
									(search-forward-regexp "\\([0-9]+\\)px" nil t))
				(replace-match (concat (number-to-string (round (* (string-to-number (match-string 1)) factor))) "px") t nil)
				))))

(defun for-each-marked-file-run-convert-css (from to)
  (interactive)
  (mapc
	 #'(lambda (file) (progn
											(find-file file)
											(convert-css from to)
											(save-buffer)
											(kill-this-buffer))) (dired-get-marked-files)))

;; ----------------------------------------------------------------------------
;; MACROS
;; ----------------------------------------------------------------------------
																				;
; Clears the double blank lines from a file leaving only single lines
(fset 'clear-all-double-lines
      [?\M-> ?\C-u ?0 ?\M-x ?c ?l ?e ?a ?r ?- ?d ?o ?u ?b ?l ?e ?- ?l ?i ?n ?e return ?\C-x ?\C-o ?\C-x ?\C-o ?\C-x ?\C-o])

(fset 'clear-double-line
      "\C-r^\C-q\C-j$\C-x\C-o")

; Hide all functions in a file using hs minor mode
(fset 'hide-prev-function
      [?\C-r ?: ?. ?* ?f ?u ?n ?c ?t ?i ?o ?n ?\C-e ?\s-- ?\C-a])

(fset 'press-return
      [?c ?d ? ?$ ?T ?E ?S ?T ?S return])

(fset 'hide-all-functions
      [?\M-> ?\C-u ?0 ?\M-x ?h ?i ?d ?e ?- ?p ?r ?e ?v ?- ?f ?u ?n ?c ?t ?i ?o ?n return ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e ?\C-e])

(provide 'functions)
;;; functions.el ends here
