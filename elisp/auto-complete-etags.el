;;; auto-complete-etags.el ---

;; Copyright 2009 Yen-Chin,Lee
;;           2010, 2011 whitypig <whitypig@gmail.com>
;;

;; Author: Yen-Chin,Lee
;;         whtiypig <whitypig@gmail.com>
;; Keywords: auto-complete-mode etags
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; TODO: Put searched signatures into cache.
;; BUG: Source file which has searching tag is somehow modified.
;; BUG: After showing document, extra new lines are inserted in the current buffer.
;; BUG: Unnecessary marks are set.
;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-etags)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'auto-complete)
(require 'etags)

(defgroup auto-complete-etags nil
  "Auto completion with ETAGS"
  :group 'auto-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate"
  :group 'auto-complete-etags)

(defface ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate."
  :group 'auto-complete-etags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables and Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ac-etags-candidates-limit 40
  "The number of candidates to popup.
nil means there is no limit about it.")

(defvar ac-etags-use-document nil
  "Set this to t when you want to see sunction signatures.")

(defvar ac-etags-tags-current-completion-table nil
  "Current etags completion table for tags.")

(defvar ac-etags-current-tags-file-name tags-file-name
  "The name of the currently-chosen tags file name.

`tags-file-name' is defined in etasg.el")

(defvar ac-etags-current-tags-table-list tags-table-list
  "The name of the currently-chosen tags table.

`tags-table-list is defined in etags.el'")

(defvar ac-etags-document-functions
  '((c-mode . ac-etags-get-c-mode-document)
    (c++-mode . ac-etags-get-c++-mode-document)))

(defconst ac-etags-document-not-found-message "No documentation found.")

(defvar ac-etags-prefix-functions
  '((c++-mode . ac-etags-prefix-c++-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ac-etags-init ()
  "Initialization function for ac-etags."
  (unless (and
           ;; tags-file-name and tags-table-list are defined in `etags.el'
           (equal tags-table-list
                  ac-etags-current-tags-table-list)
           (equal tags-file-name
                  ac-etags-current-tags-file-name)
           ;; We assume that tags-file-name and each file in
           ;; tags-table-list have their own existing buffers.
           (or (null tags-file-name)
               (verify-visited-file-modtime (get-file-buffer tags-file-name)))
           (or (null tags-table-list)
               (not (member nil
                       (mapcar #'verify-visited-file-modtime
                               (mapcar #'get-file-buffer tags-table-list))))))
    ;; When tags-file-name or list has changed, we create a new completion table.
    (let ((tags-completion-table nil))
      (setq ac-etags-tags-current-completion-table (tags-completion-table))
      (setq ac-etags-current-tags-file-name tags-file-name)
      (setq ac-etags-current-tags-table-list tags-table-list))))

(defun ac-etags-candidate ()
  ;; These two variables are defined in `etags.el'
  (when (or tags-file-name tags-table-list)
    ;; If at least one tags table is selected, initialize completion table.
    (ac-etags-init)
    (let* ((candidates (sort (all-completions ac-target ac-etags-tags-current-completion-table)
                             #'string<))
           (len (length candidates)))
      (when (and (numberp ac-etags-candidates-limit)
                 (< ac-etags-candidates-limit len))
        (nbutlast candidates (- len ac-etags-candidates-limit)))
      (append (ac-etags-same-mode-candidate)
              (ac-etags-buffer-dictionary-candidate)
              candidates))))

(defun ac-etags-same-mode-candidate ()
  (ac-word-candidates
   (lambda (buffer)
     (derived-mode-p (buffer-local-value 'major-mode buffer)))))

(defun ac-etags-buffer-dictionary-candidate ()
  (ac-buffer-dictionary))

(defun ac-etags-prefix ()
  (or (and (assoc major-mode ac-etags-prefix-functions)
           (funcall (cdr (assoc major-mode ac-etags-prefix-functions))))
      (ac-prefix-symbol)))

;; @param item The name to be searched for in tagfile.
;; @param tag-file The absolute pathname of tag-file to be visited.
(defun ac-etags-get-item-location-in-tags (item tag-file)
  "Return a list of lists, each list consisting of information
with which we try to find definitions of ITEM. car of each
element is an abosolute pathname and cdr is line-number."
  (let ((b (find-file-noselect tag-file))
        (locs nil) (filename nil) (linenum nil))
    (unless b
      (error "ac-etags: Cannot find file: %s" tag-file))
    (unless (and (stringp tag-file) (file-name-absolute-p tag-file))
      (error "ac-etags: The name of tag file is not absolute"))
    (with-current-buffer b
      (goto-char (point-min))
      (while (re-search-forward (concat "" item "\\([0-9]+\\),[0-9]+$") nil t)
        (setq linenum (string-to-number (match-string 1)))
        ;; Search for the filename containing this item
        (save-excursion
          (if (re-search-backward "^\\([^[:cntrl:]]+\\),[0-9]+$" nil t)
              (setq filename (match-string 1))
            (error "ac-etags: Cannot find the source file for tag \"%s\"" item)))
        (unless (file-name-absolute-p filename)
          (setq filename (expand-file-name filename (file-name-directory tag-file)))
          ;; Work around on a cygwin and windows machine.
          (when (and (eq system-type 'windows-nt)
                     (string-match "\\([[:alpha:]]:/\\).*/cygdrive/[[:alpha:]]/\\(.*\\)"
                                   filename))
            (setq filename (concat (match-string 1 filename) (match-string 2 filename)))))
        (if (and filename linenum)
            (add-to-list 'locs (list filename linenum)))))
    (nreverse locs)))

;; @todo What to do when multiple tags match item.
(defun ac-etags-search-for-documentation (item)
  "Search for and return the documentation about ITEM."
  (let* ((ret ac-etags-document-not-found-message) (case-fold-search nil)
         (loc nil) (locs nil) (ll nil) (mode major-mode) (docs nil))
    (when tags-table-list
      (dolist (tagfile tags-table-list)
        (setq ll (ac-etags-get-item-location-in-tags item tagfile))
        ;; Check to see if this file is to be opened with the related mode to MODE.
        (dolist (l ll)
          (when (and l (ac-etags-is-target-mode-p (car l) mode))
            (add-to-list 'locs l))))
      ;; locs => ((f1 l1) (f2 l2))
      ;; We try to find doc only when filename is an absolute pathname.
      (dolist (l locs)
        (when (and l (stringp (car l)) (file-name-absolute-p (car l)))
          (push (ac-etags-get-document-by-mode item l mode) docs)))
      ;; Format docs
      (when docs
        (delete-dups docs)
        (when (and (> (length docs) 1) (member ac-etags-document-not-found-message docs))
          (setq docs (delete ac-etags-document-not-found-message docs)))
        (setq ret (apply #'concat (mapcar (lambda (x) (concat x "\n")) docs)))
        ;; Remove a trailing carriage-return, newline, and space if any.
        (setq ret (replace-regexp-in-string "[\r\n ]+$" "" ret))))
    ret))

(defun ac-etags-is-target-mode-p (filename buffer-mode-name)
  (let ((mode (assoc-default filename auto-mode-alist 'string-match)))
    (cond
     ((eq buffer-mode-name 'c++-mode)
      (or (eq buffer-mode-name mode ) (string= "h" (file-name-extension filename))))
     (t (eq buffer-mode-name mode)))))

(defun ac-etags-get-document-by-mode (item location mode)
  (let ((f (cdr (assoc mode ac-etags-document-functions))))
    (if f (funcall f item (car location) (cadr location))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For c-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ac-etags-get-c-mode-document (item filename linenum)
  "Return documentation about ITEM defined in file FILENAME on
line number LINENUM."
  (let ((doc ac-etags-document-not-found-message) (beg nil))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (goto-char (point-min))
      (forward-line (1- linenum))
      (let ((line (thing-at-point 'line)))
        (when (string-match item line)
          ;; We are concerned with only fucntion-like structures.
          (when (string-match (concat item "(") line)
            (cond
             ((string-match (concat "^" item) line)
              (or (re-search-backward "\\([};/]\\|^$\\)" nil t) (goto-char (point-min)))
              (goto-char (1+ (point)))
              (setq beg (point)))
             (t
              (beginning-of-line)
              (setq beg (point))))
            (skip-chars-forward "^{;\\\\/")
            (setq doc (buffer-substring-no-properties beg (point))))
          (setq doc (replace-regexp-in-string ";" "" doc))
          (setq doc (replace-regexp-in-string "[ \n\t]+" " " doc))
          (setq doc (replace-regexp-in-string "\\(^ \\| $\\)" "" doc)))))
    doc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @todo Need to be refactored.
(defun ac-etags-get-c++-mode-document (item filename linenum)
  (let ((doc ac-etags-document-not-found-message) (beg nil))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (goto-char (point-min))
      (forward-line (1- linenum))
      (let ((line (thing-at-point 'line)))
        ;; Strip a class name if any.
        (let ((i nil))
          (when (setq i (string-match "::" item))
            (setq item (substring-no-properties item (+ 2 i)))))
        (when (string-match item line)
          ;; We are concerned with only fucntion-like structures.
          (when (string-match (concat item "(") line)
            (cond
             ((string-match (concat "^" item) line)
              (or (re-search-backward "\\([};/]\\|^$\\)" nil t) (goto-char (point-min)))
              (goto-char (1+ (point)))
              (setq beg (point)))
             (t
              (beginning-of-line)
              (setq beg (point))))
            (skip-chars-forward "^{;\\\\/")
            (setq doc (buffer-substring-no-properties beg (point)))
            (setq doc (replace-regexp-in-string ";" "" doc))
            (setq doc (replace-regexp-in-string "[ \r\n\t]+" " " doc))
            (setq doc (replace-regexp-in-string "\\(^ \\| $\\)" "" doc))))))
    doc))

(defun ac-etags-prefix-c++-mode ()
  (let ((c (char-before))
        (bol (save-excursion (beginning-of-line) (point))))
    (cond
     ((and (characterp c) (char-equal c ?:))
      ;; Has just entered `::' ?
      (when (and (char-before (1- (point)))
                 (char-equal (char-before (1- (point))) ?:))
        (save-excursion
          (ac-etags-skip-backward-delim)
          (if (and (= (point) bol)
                   (ac-etags-double-colon-p (point)))
              (+ 2 (point))
            (point)))))
     ;; There is `::' on the currently-editing line,
     ;; and has just entered a character other than `:'.
     ((save-excursion
        (re-search-backward "::"
                            (save-excursion
                              (ac-etags-skip-backward-delim)
                              (point))
                            t))
      (save-excursion
        (ac-etags-skip-backward-delim)
        (if (ac-etags-double-colon-p (point))
            (+ 2 (point))
          (point))))
     (t nil))))

(defun ac-etags-skip-backward-delim ()
  (let ((bol (save-excursion (beginning-of-line) (point)))
        (cont t))
    (while (and cont (search-backward "::" bol t))
      (when (and (char-before) (string-match "[[:alpha:]]" (string (char-before))))
        ;; skip a namespace
        (skip-chars-backward "^* \t;()<>" bol)
        (setq cont nil)))))

(defun ac-etags-double-colon-p (pos)
  "Return t if characters at position POS and POS+1 are colons."
  (let ((c1 (char-after pos))
        (c2 (char-after (1+ pos))))
    (and (characterp c1)
         (characterp c2)
         (char-equal c1 ?:)
         (char-equal c2 ?:))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++-mode ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ac-etags-document (item)
  "Return documentation corresponding to ITEM. If no
documentation is found, return nil."
  (when ac-etags-use-document
    (let ((sig (ac-etags-search-for-documentation (substring-no-properties item))))
      (if (stringp sig)
        sig
        nil))))

;; Define ac-source-etags
(ac-define-source etags
  '((candidates . ac-etags-candidate)
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (document . ac-etags-document)
    (requires . 2)
    (prefix . ac-etags-prefix)))

(provide 'auto-complete-etags)
;;; auto-complete-etags.el ends here
