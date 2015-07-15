;;; js-dependency-inject.el --- 

;; Copyright (C) 2014  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: convenience, abbrev, tools

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

;; This package allows you to perform the equivalent of dependency
;; injection for javascript files

;;; Code:
(defun sort-dependencies ()
	"Sorts the dependency require paths alphabetically.
This reorders the class list to align with the reordered
require paths."
  (interactive)
  (save-excursion
		(let ((require-class-alist '())
					(class-name-list (get-class-name-list))
					(require-path-list (get-require-path-list)))
			(mapc #'(lambda (require-path)
								(push (list (format "%s" require-path) (pop class-name-list)) require-class-alist))
						require-path-list)
			(setq require-class-alist
						(sort require-class-alist #'(lambda (a b) (string< (car a) (car b)))))
			
			(setq require-path-list
						(mapcar #'(lambda (x) (car x)) require-class-alist))
			(setq class-name-list
						(mapcar #'(lambda (x) (cadr x)) require-class-alist))
			
			(inject-dependency require-path-list class-name-list t))))

(defun require-dependency-at-point ()
  "Inject the dependency at point.
This function will take the word under point and look for it in the
dependncy list. If it exists, it will add a require path as the variable argument"
  (interactive)
  (save-excursion
    (let* ((popup-point (point))
					 (class-symbol (or (thing-at-point 'word)
                             (save-excursion (backward-word) (thing-at-point 'word))))
           (class-name (concat class-symbol ".js"))
           (result (assoc class-name (get-dependency-relative-alist)))
           (qc (get-quote-char)))
      (if result
          (let ((require-path (show-popup-with-options result popup-point (concat qc "%s" qc))))
            (end-of-line)
            (insert (format " = require(%s);" require-path)))
        (message "%s does not exist in any dependencies" class-symbol)
          )
      )))

(defun inject-dependency-at-point ()
	"Inject the dependency at point.
This function will take the word under point and look for it in the
dependncy list. If it exists, it will append it to the function list
and add the require path, if it is already used it will update the
current dependency. If it does not exist, do nothing and print to the
minibuffer."
  (interactive)
	(save-excursion 
		(let* ((popup-point (point))
					 (class-symbol (or (thing-at-point 'word) (save-excursion (backward-word) (thing-at-point 'word))))
					 (class-name (concat (downcase class-symbol) ".js"))
					 (result (assoc class-name (get-dependency-alist)))
					 (qc (get-quote-char)))
			(if result
					(let ((require-path (show-popup-with-options result popup-point (concat qc "%s"qc ))))
						(inject-dependency (list require-path) (list class-symbol)))
				(message "%s does not exist in any dependencies" class-symbol)))))

(defun update-dependencies ()
	"Update all of the classes in the function block.
This function constructs a list of require paths based on the class
names present in the function block. These are delegated to the
inject-dependency function which will replace the view with the new
arguments."
  (interactive)
  (save-excursion
		(let* ((popup-point (point))
					 (class-name-list (get-class-name-list))
					 (require-path-list (list))
					 (dependency-alist (get-dependency-alist)))

			(mapcar
			 #'(lambda (class-symbol)
					 (let* ((class-name (concat (downcase class-symbol) ".js"))
									(result (assoc class-name dependency-alist))
									(qc (get-quote-char)))
						 (setq require-path-list
									 (append require-path-list
													 (list (if result
																		 (show-popup-with-options result popup-point (concat qc "%s" qc))
																	 (format "\"???/%s\"" (downcase class-symbol))))))))
			 class-name-list)
			(inject-dependency require-path-list class-name-list t)
			(sort-dependencies))))

(defun inject-dependency (require-paths class-names &optional replace)
	"Inject or replace a list of REQUIRE-PATHS and CLASS-NAMES into a
JavaScript file. It uses the CLASS-NAMES as the keys of the REQUIRE-PATHS.
The REPLACE flag will replace all require paths and class names with these."
		(let ((require-path-list nil)
					(class-name-list nil)
					(require-path-region (get-require-path-region))
					(class-name-region (get-class-name-region)))
			(when (not replace)
				(setq require-path-list (get-require-path-list))
				(setq class-name-list (get-class-name-list)))
			(mapc
			 #'(lambda (require-path)
					 (let* ((index (position require-path require-paths :test #'string-equal))
									(class-name (nth index class-names)))

						 (if (not (member class-name class-name-list))
								 (setq class-name-list (append class-name-list (list class-name))))

						 (setq index (position class-name class-name-list :test #'string-equal))
						 (if (or replace (eq index (length require-path-list)))
								 (setq require-path-list (append require-path-list (list require-path)))
							 (setf (nth index require-path-list) require-path))
						 ))
			 require-paths)
			
			(replace-region (car class-name-region) (cadr class-name-region)
											(format-text-in-rectangle (mapconcat 'identity class-name-list ", ") 150))
			(replace-region (car require-path-region) (cadr require-path-region)
											(format "%s\n" (mapconcat 'identity require-path-list ",\n")))
			(indent-require-block)))

(defun get-dependency-alist ()
	"Construct the dependency alist from the external-lib-alist.
It assossciates each file name to a list of locations of that file."
		(let ((dependency-alist (list)))
			(mapcar
			 #'(lambda (project-assoc)
					 (mapcar
						#'(lambda (elt)
								;; Filter results by /script/ regexp
								(let* ((filtered-results (filter-list (lambda (x) (string-match "/script/" x)) (cdr elt))))
									;; If we have filtered results, append them
									(when filtered-results
										(let ((modified-results
													 (mapcar #'(lambda (x)
																			 (concat (replace-regexp-in-string ".*script" (car project-assoc) x)
																							 (replace-regexp-in-string ".js" "" (car elt)))) filtered-results)))
											
											;; Create alist element of file to folder
											(let ((appended-results (append (list (car elt)) modified-results)))

												;; If entry already exists - remove and redefine appended-results
												(when (not (eq nil (assoc (car elt) dependency-alist)))
													(setq appended-results (append (assoc (car elt) dependency-alist) modified-results))
													(setq dependency-alist (delq (assoc (car elt) dependency-alist) dependency-alist)))

												(push appended-results dependency-alist))))
									))
						(cdr project-assoc)))
			 external-lib-alist) dependency-alist))

(defun get-dependency-relative-alist ()
  "Constructs the dependency alist from external-lib-alist.
It assosciates each file name to a list of relative file paths"
  (let ((dependency-alist (list)))
    (mapcar
     #'(lambda (project-assoc)
         (mapcar
          #'(lambda (elt)
              (let* ((cwd (file-name-directory (buffer-file-name)))
                     (modified-results
                      (mapcar #'(lambda (x)
                                  (let ((relative-name (file-relative-name (concat x (car elt)) cwd)))
                                    (if (string-match "^[a-zA-Z]" relative-name)
                                        (concat "./" relative-name)
                                      relative-name)))
                              (cdr elt))))
                (let ((appended-results (append (list (car elt)) modified-results)))

                  ;; If entry already exists - remove and redefine appended-results
                  (when (not (eq nil (assoc (car elt) dependency-alist)))
                    (setq appended-results (append (assoc (car elt) dependency-alist) modified-results))
                    (setq dependency-alist (delq (assoc (car elt) dependency-alist) dependency-alist)))
                  
                  (push appended-results dependency-alist)))
              ) (cdr project-assoc))
         ) external-lib-alist) dependency-alist))

(defun get-require-path-list ()
	(let ((a (car (get-require-path-region)))
				(b (cadr (get-require-path-region))))
		(mapcar #'chomp (split-string (buffer-substring a b) ",\\s-*\n\\s-*"))))

(defun get-class-name-list  ()
	(let ((a (car (get-class-name-region)))
				(b (cadr (get-class-name-region))))
		(if (not (eq a b))
				(mapcar #'chomp (split-string (buffer-substring a b) ",\\s-*"))
			nil)))

(defun get-require-path-region ()
  (get-region "\\s-*\\[\n\\s-*" "\\s-*\\]"))

(defun get-class-name-region ()
  (get-region "function\s-*\(" "\)"))

(defun get-region (regex-a regex-b)
		(beginning-of-buffer)
		(search-forward-regexp "require\\|define")
		(let ((start (search-forward-regexp regex-a))
					(end (- (search-forward-regexp regex-b) 1)))
			(list start end)))

(defun get-quote-char ()
  (if (> (count-matches "\"" (point-min) (point-max))
				 (count-matches "'" (point-min) (point-max)))
			"\"" "'"))

(defun filter-list (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun show-popup-with-options (options popup-point f)
	(format f (if (= 1 (length (cdr options)))
								(cadr options)
							(popup-menu* (cdr result) :point popup-point))))

(defun indent-require-block ()
  (interactive)
  (save-excursion
		(beginning-of-buffer)
		(indent-region (search-forward-regexp "function\\s-*(.*\n") (cadr (get-class-name-region)))
		(indent-region (car (get-require-path-region)) (cadr (get-require-path-region)))
		(goto-char (cadr (get-require-path-region))) (indent-according-to-mode)))

(defun replace-region (region-start region-end replacement)
	(goto-char region-start)
	(delete-region region-start region-end)
	(insert replacement))

(defun format-text-in-rectangle (text width)
	(with-temp-buffer
		(insert text)
		(goto-char (+ (point-min) width))
		(while (< (point) (point-max))
			(backward-word)
			(newline)
			(goto-char (+ (point) width)))
		(format "%s" (buffer-substring (point-min) (point-max)))))

(provide 'js-dependency-inject)
;;; js-dependency-inject.el ends here
