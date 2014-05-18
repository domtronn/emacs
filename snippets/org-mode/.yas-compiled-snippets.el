;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
										 '(("blog" "#+STARTUP: showall indent\n#+STARTUP: hidestars\n#+BEGIN_HTML\n---\nlayout: default\ntitle: ${1:title}\nexcerpt: ${2:excerpt}\n---\n$0" "blog" nil nil nil nil nil nil)
											 ("code" "#+begin_${1:lang} ${2:options}\n$0\n#+end_$1" "code" nil nil nil nil nil nil)
											 ("dot" "#+begin_src dot :file ${1:file} :cmdline -T${2:pdf} :exports none :results silent\n            $0\n#+end_src\n\n[[file:$1]]" "dot" nil nil nil nil nil nil)
											 ("elisp" "#+begin_src emacs-lisp :tangle yes\n$0\n#+end_src" "elisp" nil nil nil nil nil nil)
											 ("emb" "src_${1:lang}${2:[${3:where}]}{${4:code}}" "embedded" nil nil nil nil nil nil)
											 ("entry" "#+begin_html\n---\nlayout: ${1:default}\ntitle: ${2:title}\n---\n#+end_html\n$0" "entry" nil nil nil nil nil nil)
											 ("fig" "#+CAPTION: ${1:caption}\n#+ATTR_LaTeX: ${2:scale=0.75}\n#+LABEL: fig:${3:label}\n" "figure" nil nil nil nil nil nil)
											 ("img" "<img src=\"$1\"\n alt=\"$2\" align=\"${3:left}\"\n title=\"${4:image title}\"\n class=\"img\"\n</img>\n$0" "img" nil nil nil nil nil nil)
											 ("latex" "#+BEGIN_LaTeX\n$0\n#+END_LaTeX" "latex" nil nil nil nil nil nil)
											 ("matrix" "\\left \\(\n\\begin{array}{${1:ccc}}\n${2:v1 & v2} \\\\\n$0\n\\end{array}\n\\right \\)" "matrix" nil nil nil nil nil nil)
											 ("src" "#+begin_src ${1:language}\n$0\n#+end_src\n" "src" nil nil nil nil nil nil)
											 ("name" "#+srcname: $0" "srcname" nil nil nil nil nil nil)
											 ("verse" "#+begin_verse\n        $0\n#+end_verse" "verse" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sun May 18 23:11:13 2014
