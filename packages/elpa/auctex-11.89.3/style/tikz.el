;;; tikz.el --- AUCTeX style for `tikz.sty'

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Matthew Leach <matthew@mattleach.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2016-22-03
;; Keywords: tex tikz

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds some support for `tikz.sty'

;;; Code:

(defun TeX-TikZ-get-opt-arg-string (arg &optional open close)
  "Return a string for optional arguments.
If ARG is nil or \"\", return \"\".  Otherwise return \"OPEN ARG
CLOSE\". If OPEN or CLOSE are nil, set them to `LaTeX-optop' and
`LaTeX-optcl' respectively."
  (unless (or open close)
    (setq open LaTeX-optop)
    (setq close LaTeX-optcl))
  (if (and arg (> (length arg) 0))
      (concat open arg close)
    ""))

(defun TeX-TikZ-arg-rect-point (_ignored)
  "Prompt the user for a point on the Cartesian plane.
Ask the user for an X and Y coordinate, and return the string
\"(X,Y)\"."
  (let ((x (TeX-read-string (TeX-argument-prompt nil nil "X-coordinate")))
        (y (TeX-read-string (TeX-argument-prompt nil nil "Y-coordinate"))))
   (concat " (" x ", " y") ")))

(defun TeX-TikZ-arg-polar-point (_ignored)
  "Prompt the user for a point on the polar plane.
Ask the user for r and theta values, and return the string
\"(THETA:R)\"."
  (let ((r (TeX-read-string (TeX-argument-prompt nil nil "R")))
        (theta (TeX-read-string (TeX-argument-prompt nil nil "Theta"))))
   (concat " (" theta ":" r ") ")))

(defun TeX-TikZ-arg-options (optional)
  "Prompt the user for options to a TikZ macro.
If OPTIONAL is non-nil, always return `LaTeX-optop' and
`LaTeX-optcl', even if the user doesn't provide any input."
  (let ((options (TeX-read-string (TeX-argument-prompt optional nil "Options" ))))
    (if optional
        (TeX-TikZ-get-opt-arg-string options)
      (concat LaTeX-optop options LaTeX-optcl))))

(defun TeX-TikZ-arg-name (optional)
  "Prompt the user for a TikZ name.
If OPTIONAL is non-nil, always return \"()\", even if the user
doesn't provide any input."
  (let ((name (TeX-read-string (TeX-argument-prompt optional nil "Name" ))))
    (if optional
        (TeX-TikZ-get-opt-arg-string name "(" ")")
      (concat "(" name ")"))))

(defun TeX-TikZ-arg-label (optional)
  "Prompt the user for TikZ label.
If OPTIONAL is non-nil always return `TeX-grop' and `TeX-grcl',
even if the user doesn't provide any input."
  (let ((label (TeX-read-string (TeX-argument-prompt optional nil "Label" ))))
    (if optional
        (TeX-TikZ-get-opt-arg-string label TeX-grop TeX-grcl)
      (concat TeX-grop label TeX-grcl))))

(defun TeX-TikZ-arg-node (_ignored)
  "Prompt the user for the deatils of a node.
Ask the user for the name and text for a node and return the
string \"node[OPTIONS](NAME){TEXT}\"."
  (let ((options (TeX-TikZ-arg-options t))
        (name (TeX-TikZ-arg-name t))
        (label (TeX-TikZ-arg-label nil)))
    (concat "node" options name label " ")))

(defun TeX-TikZ-get-arg-type (types &optional prompt)
  "Prompt the user for an argument type.
TYPES is a list of possible types that the user can specify.  If
PROMPT is non-nil use that prompt instead."
  (let ((completion-ignore-case t)
        (prompt (if prompt
                    prompt
                  "Next argument type (RET to finish): ")))
    (completing-read prompt types nil t)))

(defun TeX-TikZ-single-macro-arg (function-alist &optional prompt)
  "Prompt the user for a single argument to compose a TikZ macro.
FUNCTION-ALIST is a mapping of argument-types to functions.  The
user is prompted for the argument type, the chosen function is
then called and the value returned.  PROMPT is used as the prompt
for the argument type."
  (let* ((argument-types (mapcar 'car function-alist))
         (argument-type (TeX-TikZ-get-arg-type argument-types prompt)))
    (funcall
     (cadr (assoc argument-type function-alist))
     argument-type)))


(defun TeX-TikZ-macro-arg (function-alist)
  "Prompt the user for arguments to compose a TikZ macro.
FUNCTION-ALIST is a mapping of argument-types to functions.  The
user is repeatedly prompted for the next argument-type; they can
choose form the cars in FUNCTION-ALIST and the appropriate
function is then called.  If the user enters \"\", then the macro
is finished."
  (let* ((options (TeX-TikZ-arg-options t))
         ;; For the iterative version, we need to add "" to the
         ;; function-alist, allowing the user to end the macro.
         (function-alist-iterative `(,@function-alist ("" identity)))
         (string-to-insert (TeX-TikZ-single-macro-arg function-alist-iterative)))

    ;; Insert the macro options.
    (insert options " ")

    ;; Iteratively prompt the user for TikZ's arguments until "" is
    ;; returned.
    (while (not (string= string-to-insert ""))
      (insert string-to-insert)
      (setq string-to-insert
            (TeX-TikZ-single-macro-arg function-alist-iterative)))

    ;; Finish the macro.
    (insert ";")))

(defcustom TeX-TikZ-point-name-regexp
  "(\\([A-Za-z0-9]+\\))"
  "A regexp that matches TikZ names."
  :type 'regexp
  :group 'auctex-tikz)

(defun TeX-TikZ-find-named-points ()
  "Find TiKZ named points in current enviroment.
Begin by finding the span of the current TikZ enviroment and then
searching within that span to find all named-points and return
them as a list of strings, dropping the '()'."
  (let* ((env-end (save-excursion
                    (LaTeX-find-matching-end)
                     (point)))
         (matches ()))
    ;; TODO: Handle cases where we are in a nested environment, \scope
    ;; for example.
    (save-excursion
      (LaTeX-find-matching-begin)
      (save-match-data
        (while (re-search-forward TeX-TikZ-point-name-regexp env-end t)
          (add-to-list 'matches (match-string 1)))))
    matches))

(defun TeX-TikZ-arg-named-point (_ignored)
  "Prompt the user for the name of a previous named-point."
  (let ((point-name (completing-read "Point name: "
                                     (TeX-TikZ-find-named-points))))
    (concat " (" point-name ") ")))

(defconst TeX-TikZ-point-function-map
  '(("Rect Point" TeX-TikZ-arg-rect-point)
    ("Polar Point" TeX-TikZ-arg-polar-point)
    ("Named Point" TeX-TikZ-arg-named-point))
  "An alist of point specification types and their functions." )

(defconst TeX-TikZ-path-connector-function-map
  (let ((connectors '("--" "|-" "-|")))
    (apply 'append (mapcar
                     (lambda (connector)
                       `((,connector identity)
                         (,(concat connector " +") identity)
                         (,(concat connector " ++") identity)))
                     connectors)))
  "An alist of path connectors.
A set of base connectors along with variants that have \" +\" and
\" ++\" appended to them, mapping to the identity function.")

(defconst TeX-TikZ-draw-arg-function-map
  `(,@TeX-TikZ-point-function-map
    ,@TeX-TikZ-path-connector-function-map
    ("Node" TeX-TikZ-arg-node))
  "An alist of argument names and functoins for TikZ's \draw.")

(defun TeX-TikZ-draw-arg (_ignored)
  "Prompt the user for the arguments to a TikZ draw macro."
  (TeX-TikZ-macro-arg TeX-TikZ-draw-arg-function-map))

(defun TeX-TikZ-coordinate-arg (_ignored)
  "Prompt the user for the arguments to a TikZ coordinate macro."
  (let ((options (TeX-TikZ-arg-options t))
        (name (TeX-TikZ-arg-name nil))
        (point (TeX-TikZ-single-macro-arg TeX-TikZ-point-function-map
                                          "Coordinate point type: ")))
    (insert options " " name " at" point ";")))

(defun TeX-TikZ-node-arg (_ignored)
  "Prompt the user for the arguments to a TikZ node macro."
  (let ((options (TeX-TikZ-arg-options t))
        (name (TeX-TikZ-arg-name nil))
        (point (TeX-TikZ-single-macro-arg TeX-TikZ-point-function-map
                                          "Node point type: "))
        (label (TeX-TikZ-arg-label nil)))
    (insert options " " name  " at" point label ";")))

(TeX-add-style-hook
 "tikz"
 (lambda ()
   (TeX-add-symbols
    '("draw" (TeX-TikZ-draw-arg))
    '("coordinate" (TeX-TikZ-coordinate-arg))
    '("node" (TeX-TikZ-node-arg)))
   (LaTeX-add-environments
    '("tikzpicture"))))

;;; tikz.el ends here
