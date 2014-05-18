;;; Compiled snippets and support files for `sh-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
										 '(("args" "if [ $# -lt ${1:2} ]\n   then $0\nfi" "args" nil nil nil nil nil nil)
											 ("!" "#!/bin/bash\n$0" "bang" nil nil nil nil nil nil)
											 ("for" "for ${1:var} in ${2:stuff}; do\n    $0\ndone" "for loop" nil nil nil nil nil nil)
											 ("f" "function ${1:name} {\n         $0\n}" "function" nil nil nil nil nil nil)
											 ("if" "if ${1:[ -f file]}\n   then ${2:do}\nfi\n$0" "if" nil nil nil nil nil nil)
											 ("ife" "if ${1:cond}\nthen ${2:stuff}\nelse ${3:other}\nfi\n$0" "ife" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sun May 18 23:11:13 2014
