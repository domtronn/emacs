;; Load a project into the file cache to find files simply

(defun project-change (arg)
  "Changes the project path and reloads the new cache"
  (interactive (list (read-file-name "Enter path to Project file: ")))
  (setq PROJECTPATH arg)
	(message "New project directory is %s. Run project-refresh to see changes." arg))

(unless (boundp 'PROJECTPATH)
	(call-interactively 'project-change))

; Cache environment files to find them easily!
; These are defined in ./projects.csv
(mapc
 'file-cache-add-directory-recursively
 (split-string (shell-command-to-string (concat "cat " PROJECTPATH)) "\n" t))


;; ========================================
;;  Caching Functions
;; ========================================

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
		 (split-string (shell-command-to-string (concat "cat " PROJECTPATH)) "\n" t))))

(defun project-add (arg)
	"Appends the path to this project to your project file"
	(interactive (list (read-directory-name "Enter project path: ")))
	(shell-command (concat "echo \"" arg "\" >> " PROJECTPATH))
	(project-refresh))


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
