
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

(defvar external-cache-hash nil)
(defvar project-src-path nil)
(defvar project-test-path nil)
(defvar project-test-cmd nil)
(defvar project-test-ext nil)
(defvar project-id nil)
(setq external-cache-hash (make-hash-table :test 'equal))
(setq-default project-id "EMACS")
(setq-default file-cache-filter-regexps (quote ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" "\\.output$" "\\.$" "#$" "\\.class$" "\\/test.*\\.js$" "\\.png$" "\\.svn*" "\\.svn-base$" "\\/node_modules\\/*" "\\.gif$" "\\.gem$" "\\.pdf$" "\\.swp$" "\\.iml$" "\\.jar$" "\\/script-tests\\/tests" "Spec\\.js$" "\\/script-tests\\/specs" "\\/jsdoc\\/" "\\.min\\.js$" "\\.tags$" "\\.filecache" "\\/testconfig\\/" "\\/.git\\/" "report")))

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
				;; If it's a file project file do this
				(progn 
					(let ((json-object-type 'hash-table)
								(json-contents (shell-command-to-string (concat "cat " PROJECTPATH))))
						(setq project-id (gethash "projectId" (json-read-from-string json-contents)))
						(use-tabs)
						(if (gethash "tabs" (json-read-from-string json-contents))
								(if (eq :json-false (gethash "tabs" (json-read-from-string json-contents)))
										(use-spaces)))
						(if (gethash "indent" (json-read-from-string json-contents))
								(setq js-indent-level (gethash "indent" (json-read-from-string json-contents))))
						(let ((testing (gethash "testing" (json-read-from-string json-contents))))
							(setq project-src-path (gethash "srcPath" testing))
							(setq project-test-path (gethash "testPath" testing))
							(setq project-test-cmd (gethash "testCmd" testing))
							(setq project-test-ext (gethash "testExt" testing)))
            (mapc
						 #'(lambda (hash)
								 (progn
									 (if (not (eq (gethash "cache" hash) :json-false))
											 (progn 
												 (if (not (file-exists-p (concat (gethash "dir" hash) "/.filecache")))
														 (let ((temp-file-cache-alist file-cache-alist))																		
															 (setq file-cache-alist nil)
															 (file-cache-add-directory-using-python (gethash "dir" hash))
															 (file-cache-save-cache-to-file (concat (gethash "dir" hash) "/.filecache"))
															 (setq file-cache-alist temp-file-cache-alist))
													 nil)
												 (file-cache-add-cache-from-file (concat (gethash "dir" hash) "/.filecache")))
										 (progn 
											 (if (file-exists-p (concat (gethash "dir" hash) "/.filecache"))
													 (delete-file (concat (gethash "dir" hash) "/.filecache")))
											 (file-cache-add-directory-using-python (gethash "dir" hash)))
										 )
									 (create-tags (gethash "dir" hash))))
						 (gethash "project" (json-read-from-string json-contents)))
						;; This block handles the libs section which is used for javascript dependency injection
						(mapc 
						 #'(lambda (hash) 
								 (progn 
									 (let ((temp-file-cache-alist file-cache-alist))
										 (setq file-cache-alist nil)
										 (file-cache-add-directory-using-python (gethash "dir" hash))
										 ;; (message "[filecache] Adding Cache recursively for External Dependency %s..." (gethash "dir" hash))
										 (puthash (gethash "id" hash) file-cache-alist external-cache-hash)
										 (setq file-cache-alist temp-file-cache-alist)))
								 (create-tags (gethash "dir" hash))
								 (setq tags-table-list (cons 
																				(file-truename (concat (gethash "dir" hash) "/.tags"))
																				tags-table-list )))
						 (gethash "libs" (json-read-from-string json-contents))))
					(setq tags-table-list (reverse tags-table-list))
					(ac-etags-setup) 
					(ac-etags-ac-setup))
			;; Else Load as a directory
			(progn 
				(message (concat PROJECTPATH " is not a project file - Interpreting as Directory"))
				(setq project-id (upcase (file-name-base PROJECTPATH)))
				(setq project-test-cmd "!!")
				(file-cache-add-directory-using-python PROJECTPATH)))))

(defun project-change (arg)
  "Changes the project path and reloads the new cache"
  (interactive (list (read-file-name "Enter path to Project file: " "~/Documents/Projects/")))
  (setq PROJECTPATH arg)
	(setq external-cache-hash (make-hash-table :test 'equal))
	(project-refresh)
	(message "New project directory is %s." arg))

(defun file-cache-add-directory-using-python (dir)
	(let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string))
		(setq file-cache-alist
					(json-read-from-string
					 (shell-command-to-string
						(concat
						 "/usr/bin/python "
						 (concat USERPATH "/create-file-alist.py ")
						 (concat "\"" (expand-file-name dir) "\" ")
						 (concat "\"" (mapconcat 'identity file-cache-filter-regexps ",") "\""))
						)))
		t))

(defun file-cache-save-cache-to-file (file)
  "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
  (interactive "FFile: ")
  (with-temp-file (expand-file-name file)
    (prin1 file-cache-alist (current-buffer))))

(defun file-cache-add-cache-from-file (file)
  "Clear `file-cache-alist' and read cache from FILE.
The file cache can be saved to a file using
`file-cache-save-cache-to-file'."
  (interactive "fFile: ")
	(message "%s" (concat "Looking for cache in " file))
  (let ((buf (find-file-noselect file)))
    (setq file-cache-alist (append (read buf) file-cache-alist))
    (kill-buffer buf)))

(unless (boundp 'PROJECTPATH)
	(call-interactively 'project-change))

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
