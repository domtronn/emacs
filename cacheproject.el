;; Load a project into the file cache to find files simply
(setq file-cache-filter-regexps (quote ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" ",v$" "\\.output$" "\\.$" "#$" "\\.class$" "\\/test.*\\.js$" "\\.png$" "\\.svn*" "\\.svn-base$" "\\/node_modules\\/" "\\/\\." "\\.gif$" "\\.gem$" "\\.pdf$" "\\.iml$" "\\.jar$" "\\/script-test[s]\\/tests" "\\/node_modules\\/" "\\/jsdoc\\/" "\\.min\\.js$")))

(defun project-clear ()
	"Clears the cache of projects"
	(interactive)
	(file-cache-clear-cache))

(defun project-refresh ()
	(interactive)
	(progn
		(project-clear)
		(mapc
		 'file-cache-add-directory-recursively
		 (split-string (shell-command-to-string (concat "cat " PROJECTPATH)) "\n" t))
		(create-tags-for-project)))

(defun project-change (arg)
  "Changes the project path and reloads the new cache"
  (interactive (list (read-file-name "Enter path to Project file: " "~/Documents/Projects/")))
  (setq PROJECTPATH arg)
	(project-refresh)
	(message "New project directory is %s." arg))

(defun project-set (arg)
	(interactive (list (read-file-name "Enter path to Project file: " "~/Documents/Projects/")))
  (setq PROJECTPATH arg)
  (message PROJECTPATH)
)

(unless (boundp 'PROJECTPATH)
	(call-interactively 'project-set))

; Cache environment files to find them easily!
; These are defined in ./projects.csv
(if (string-equal "0\n" 				  
	  (shell-command-to-string (format "if [ -d %s ]; then echo 1; else echo 0; fi" PROJECTPATH)))
	(progn 
	  (mapc
	   'file-cache-add-directory-recursively
	   (split-string (shell-command-to-string (concat "cat " PROJECTPATH)) "\n" t))
	  (create-tags-for-project))
  (progn
	(message (concat PROJECTPATH " is not a project file"))
	(file-cache-add-directory USERPATH )
	(file-cache-add-directory (concat USERPATH "/elisp"))))

;; (mapc
;;  'test
;;  (split-string (shell-command-to-string (concat "cat " PROJECTPATH)) "\n" t))

;; ========================================
;;  Caching Functions
;; ========================================


(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))

(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
	 (lambda ()
	   (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))
