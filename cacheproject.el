
;;; cacheproject.el --- 

;; Copyright (C) 2014  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: files, convenience

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

;;; Code:

(defvar external-lib-alist nil)
(defvar project-src-path nil)
(defvar project-test-path nil)
(defvar project-test-cmd nil)
(defvar project-test-ext nil)
(defvar project-id nil)
(setq-default project-id "EMACS")
(setq-default file-cache-filter-regexps (quote ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" "\\.output$" "\\.$" "#$" "\\.class$" "\\/test.*\\.js$"
																								"\\.png$" "\\.svn*" "\\/node_modules\\/*" "\\.gif$" "\\.gem$" "\\.pdf$" "\\.swp$" "\\.iml$" "\\.jar$"
																								"\\/build\\/" "Spec\\.js$" "\\/script-tests\\/specs" "\\/jsdoc\\/" "\\.min\\.js$" "\\.tags$" "\\.filecache"
																								"\\.cache$" "\\/.git\\/" "report" "\\.gcov\\.html$" "\\.func.*\\.html$")))

(defun project-clear ()
	"Clears the cache of projects"
	(interactive)
	(setq tags-table-list nil)
	(file-cache-clear-cache))

(defun project-refresh ()
	"Parses a json project file for modules of a project, whether or not to cache them
	 And external dependencies (mainly for javascript). If the supplied file is not a 
	 file but a directory, it just adds this directory to the file cache"
	(interactive)
	(progn
		(project-clear)
		(if (string-equal "0\n"
					(shell-command-to-string (format "if [ -d %s ]; then echo 1; else echo 0; fi" PROJECTPATH)))
				;; If it's a project file do this
				(progn 
					(let ((json-object-type 'hash-table)
								(json-contents (shell-command-to-string (concat "cat " PROJECTPATH))))
						(setq project-id (gethash "projectId" (json-read-from-string json-contents)))
						
						;; Set up the indentation settings i.e. tabs vs spaces
						(use-tabs)
						(if (gethash "tabs" (json-read-from-string json-contents))
								(when (eq :json-false (gethash "tabs" (json-read-from-string json-contents)))
                    (message "Using spaces for project files")
										(use-spaces)))
						(when (gethash "indent" (json-read-from-string json-contents))
              (let ((indent-level (gethash "indent" (json-read-from-string json-contents))))
                (message "Setting indent level to %s" indent-level)
                (setq-default js-indent-level indent-level)
                (setq-default js2-basic-offset indent-level)
                (setq-default css-indent-offset indent-level)
                (setq-default web-mode-markup-indent-offset indent-level)
                (setq-default c-basic-offset indent-level)
                ))
						
						;; Set up variables used in opening and running tests
						(let ((testing (gethash "testing" (json-read-from-string json-contents))))
							(setq project-src-path (gethash "srcPath" testing))
							(setq project-test-path (gethash "testPath" testing))
							(setq project-test-cmd (gethash "testCmd" testing))
							(setq project-test-ext (gethash "testExt" testing)))

						(set-external-lib-alist-using-python PROJECTPATH)
						(setq file-cache-alist (cdr (assoc project-id external-lib-alist)))
						
						;; Create tags files for all of the project files
						(let ((project (gethash "project" (json-read-from-string json-contents)))
									(libs (gethash "libs" (json-read-from-string json-contents))))

							(setq tags-tag-list nil)
							(mapc #'create-tags-and-append project)
							(mapc #'create-tags-and-append libs)))
					
					(setq tags-table-list (reverse tags-table-list))
					(ac-etags-setup) 
					(ac-etags-ac-setup))
			;; Else Load as a directory
			(progn 
				(message (concat PROJECTPATH " is not a project file - Interpreting as Directory"))
				(setq project-id (upcase (file-name-base PROJECTPATH)))
				(setq project-test-cmd "!!")
				(file-cache-add-directory-recursively PROJECTPATH)))))

(defun create-tags-and-append (hash)
	(when (not (eq :json-false (gethash "create-tags" hash)))
		(create-tags (gethash "dir" hash))
    (when (file-exists-p (concat (gethash "dir" hash) "/.tags"))
      (setq tags-table-list
            (append tags-table-list (list (file-truename (concat (gethash "dir" hash) "/.tags"))))))))

(defun project-change (arg)
  "Changes the project path and reloads the new cache"
  (interactive (list (ido-read-file-name "Enter path to Project file: " "~/Documents/Projects/")))
  (setq PROJECTPATH arg)
	(setq external-lib-alist (make-hash-table :test 'equal))
	(project-refresh)
	(message "New project directory is %s." arg))

(defun file-cache-add-directory-using-python (dir)
	(let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string))
		(setq file-cache-alist
					(append file-cache-alist
									(json-read-from-string
									 (shell-command-to-string
										(concat
										 "/usr/bin/python "
										 (concat USERPATH "/create-file-alist.py ")
										 (concat "\"" (expand-file-name dir) "\" ")
										 (concat "\"" (mapconcat 'identity file-cache-filter-regexps ",") "\""))
										)))) t))

(defun set-external-lib-alist-using-python (dir)
	(let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string))
		(setq external-lib-alist
				  (json-read-from-string
					 (shell-command-to-string
						(concat
						 "/usr/bin/python "
						 (concat USERPATH "/create-file-alist.py ")
						 (expand-file-name dir)
						 (concat " \"" (mapconcat 'identity file-cache-filter-regexps ",") "\""))
						))))
	t)

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


(provide 'cacheproject)
;;; cacheproject.el ends here
