;;; Compiled snippets and support files for `perl-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'perl-mode
										 '(("is" "use ${1:$$(if yas-modified-p (mapconcat 'identity (split-string yas-text \" \") \"::\"))}$0;" "is" nil nil nil nil nil nil)
											 ("sub" "sub $1 {\n		$0\n}" "Sub" nil nil nil nil nil nil)
											 ("use" "use ${1:$$(if yas-modified-p (mapconcat 'identity (split-string yas-text \" \") \"::\"))}$0;" "use" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri May 30 09:30:39 2014
