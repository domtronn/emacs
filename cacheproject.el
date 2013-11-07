;; Load a project into the file cache to find files simply


(setq PROJECTPATH (concat (getenv "HOME") "/Projects.csv"))

; Cache environment files to find them easily!
; These are defined in ./projects.csv
(mapc
 'file-cache-add-directory-using-find 
 (split-string (shell-command-to-string (concat "cat " PROJECTPATH)) "\n" t))


;; ========================================
;;  Caching Functions
;; ========================================

(defun project-change (arg)
  "Changes the project path and reloads the new cache"
  (interactive (list (read-directory-name "Enter project path: ")))
  (setq project-path arg)
  (message "New project directory is %s. Loading cache..." arg)
  (file-cache-clear-cache)
  (file-cache-add-directory-using-find arg))

(defun project-add (arg)
	"Adds a project directory to the cache for easy file finding"
	(interactive (list (read-directory-name "Enter project path: ")))
	(message "Adding %s to the cache..." arg)
	(file-cache-add-directory-recursively arg ))

(defun project-clear ()
	"Clears the cache of projects"
	(file-cache-clear-cache))

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
