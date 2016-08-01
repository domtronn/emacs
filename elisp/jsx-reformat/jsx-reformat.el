;;; jsx-reformat.el --- Function to refromat JSX code

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Version: 1.0.1
;; Package-Requires: ((web-mode "20160306.1222"))
;; URL: https://github.com/domtronn/eslint-reader.el
;; Keywords: convenient, lisp

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

;; Reformat a block of JSX html to match common practices for JSX and React.

;; This package prioritises putting attributes on new lines, putting
;; the content of elements on new lines, e.g.

;; <div><span className='class' tag='tag'>Span Contents</span></div>

;; Would become

;; <div>
;;   <span
;;     className='class'
;;     tag='tag'>
;;     Span Contents
;;   </span>
;; </div>

;;; Code:

(require 's)
(require 'web-mode)
(require 'expand-region)
(require 'delsel)

(defun jsx-reformat--tag ()
  (let ((start-point (point))
        (start-line (count-lines (point-min) (1+ (point))))
        (match-line (and
                     (web-mode-tag-match-position)
                     (count-lines (point-min) (web-mode-tag-match-position))))
        (self-closing (eq nil (web-mode-tag-match-position))))
    (when self-closing
      (web-mode-tag-end)
      (when (looking-at "\s+<[a-z]")
        (newline)
        (web-mode-tag-previous)
        (web-mode-tag-end)))
    (when (and (not match-line) (not self-closing))
      (unless (looking-back "^\s*") (newline)))
    (when (and match-line (eq start-line match-line))
      (web-mode-tag-match)
      (newline)
      (web-mode-tag-match)
      (web-mode-tag-end)
      (newline)
      (goto-char start-point))))

(defun jsx-reformat--tags ()
  (while (web-mode-element-next) (jsx-reformat--tag)))

(defun jsx-reformat--count-attributes ()
  (let ((count 0)
        (text (buffer-substring (point) (web-mode-tag-end-position))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (web-mode-attribute-next) (setq count (1+ count))))
    count))

(defun jsx-reformat--element-attributes ()
  (let ((count (jsx-reformat--count-attributes))
        (start (web-mode-tag-end-position))
        (spread (count-lines (point) (web-mode-tag-end-position))))
    (when (> count 1)
      (while (and (web-mode-attribute-next)
                  (< (point) start))
        (unless (looking-back "^\s+") (newline)))
      (web-mode-attribute-previous)
      (goto-char (1- (web-mode-tag-end-position)))
      (when (and (looking-at "/>")
                 (not (looking-back "^\s+")))
        (newline)
        (forward-char 2))
      )
    (when (and (eq count 1)
               (> spread 1))
      (dotimes (i (1- spread)) (forward-line 1) (join-line))
      (web-mode-tag-end-position))
    ))

(defun jsx-reformat--attributes ()
  "Search forward and format all attributes."
  (unless (looking-at "<") (web-mode-element-next))
  (jsx-reformat--element-attributes)
  (while (web-mode-element-next) (jsx-reformat--element-attributes)))

(defun jsx-reformat--object-literals ()
  "Search forward and place object literals on new lines."
  (while (re-search-forward "{{\\(.*?\\)}}" (point-max) t)
    (replace-match (format "{{\n%s\n}}" (match-string 1)))))

(defun jsx-reformat ()
  (interactive)
  (let ((restore-point (point))
        (started-region (region-active-p)))
    (unless started-region
      (unless (web-mode-jsx-is-html) (web-mode-element-previous))
      (unless (web-mode-jsx-is-html) (error "Not in JSX Html"))
      (er/mark-inside-pairs))
    (let*
        ((start (region-beginning))
         (end (region-end))
         (text (buffer-substring start end)) result)
      (goto-char start)
      (with-temp-buffer
        (insert (concat " "
                        (replace-regexp-in-string
                         " +" " "
                         (replace-regexp-in-string "\n" " " text))))
        ;; (insert (concat " " text))
        (goto-char (point-min))
        (save-excursion (jsx-reformat--tags))
        (save-excursion (jsx-reformat--attributes))
        (save-excursion (jsx-reformat--object-literals))
        (setq result (s-chomp (s-trim (buffer-string))))
        )
      (delete-region start end)
      (unless started-region (er/mark-inside-pairs))
      (delete-active-region)
      (insert (if started-region result (format "\n%s\n" result)))
      (unless started-region
        (setq end (1+ (point)))
        (setq start (scan-lists end -1 0)))
      (goto-char start)
      (indent-region start end)
      (whitespace-cleanup-region start end)
      (goto-char restore-point)
      )))

(provide 'jsx-reformat)

;;; jsx-reformat.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (nameless-mode 1)
;; End:
