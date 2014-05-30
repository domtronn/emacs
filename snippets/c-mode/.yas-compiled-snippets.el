;;; Compiled snippets and support files for `c-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c-mode
										 '(("ass" "#include <assert.h>\n$0" "assert" nil nil nil nil nil nil)
											 ("compile" "// -*- compile-command: \"${1:gcc -Wall -o ${2:dest} ${3:file}}\" -*-" "compile" nil nil nil nil nil nil)
											 ("d" "#define $0" "define" nil nil nil nil nil nil)
											 ("fopen" "FILE *${fp} = fopen(${\"file\"}, \"${r}\");" "FILE *fp = fopen(..., ...);" nil nil nil nil nil nil)
											 ("malloc" "malloc(sizeof($1)${2: * ${3:3}});\n$0" "malloc" nil nil nil nil nil nil)
											 ("packed" "__attribute__((__packed__))$0" "packed" nil nil nil nil nil nil)
											 ("pr" "printf(\"${1:format string}\"${2: ,a0,a1});" "printf" nil nil nil nil nil nil)
											 ("io" "#include <stdio.h>" "stdio" nil nil nil nil nil nil)
											 ("std" "#include <stdlib.h>\n" "stdlib" nil nil nil nil nil nil)
											 ("str" "#include <string.h>" "string" nil nil nil nil nil nil)
											 ("case" "switch (${1:ch}) {\n       case ${2:const}:\n       ${3:a = b};\n       break;\n       ${4:default:\n       ${5:action}}\n}\n" "switch" nil nil nil nil nil nil)
											 ("union" "typedef union {\n        $0\n} ${1:name};" "union" nil nil nil nil nil nil)
											 ("uni" "#include <unistd.h>" "unistd" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri May 30 09:30:39 2014
