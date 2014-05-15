;;; Compiled snippets and support files for `text-mode'
;;; .yas-setup.el support file if any:
;;;
(defun yas-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))

(defun ca-all-asscs (asslist query)
  "returns a list of all corresponding values (like rassoc)"
  (cond
   ((null asslist) nil)
   (t
    (if (equal (cdr (car asslist)) query)
        (cons (car (car asslist)) (ca-all-asscs (cdr asslist) query))
      (ca-all-asscs (cdr asslist) query)))))
;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
										 '(("!" "#!/usr/bin/env `(let ((found (ca-all-asscs interpreter-mode-alist major-mode))) (if found (yas/choose-value found) \"\"))`$0" "bang" nil nil nil nil nil nil)
											 ("bash" "#!/bin/bash\n$0" "bash" nil nil nil nil nil nil)
											 ("code" ".. code:: ${1:python}" "code" nil nil nil nil nil nil)
											 ("email" "`(replace-regexp-in-string \"@\" \"@NOSPAM.\" user-mail-address)`" "(user's email)" nil nil nil nil nil nil)
											 ("fi" "`(yas-with-comment \"FIXME: \")`" "fixme" nil nil nil nil nil nil)
											 ("var" "`(ca-with-comment \"-*- ${1:var}: ${2:value} -*-\")`" "var" nil nil nil nil nil nil)
											 ("mode" "`(yas-with-comment \"-*- mode: ${1:mode} -*-\")`" "mode" nil nil nil nil nil nil)
											 ("time" "`(current-time-string)`" "(current time)" nil nil nil nil nil nil)
											 ("t" "`(yas-with-comment \"TODO: \")`" "todo" nil nil nil nil nil nil)
											 ("x" "`(yas-with-comment \"XXX: \")`" "xxx" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu May 15 15:39:54 2014
