;;; Compiled snippets and support files for `java-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'java-mode
										 '(("apr_assert" "if (Globals.useAssertions) {\n   ${1:assert ..};\n}\n" "apr_assert" nil nil nil nil nil nil)
											 ("cls" "class ${1:Class} {\n      $0\n}" "cls" nil nil nil nil nil nil)
											 ("/*" "/**\n * ${1:documentation}\n */" "doc" nil nil nil nil nil nil)
											 ("eq" "public boolean equals(${1:Class} other) {\n       $0\n}" "equals" nil nil nil nil nil nil)
											 ("file" "public class ${1:`(file-name-base\n                    (or (buffer-file-name)\n                        (buffer-name)))`} {\n  $0\n}\n" "file_class" nil nil nil nil nil nil)
											 ("for" "for (${1:int i = 0}; ${2:i < N}; ${3:i++}) {\n    $0\n}" "for" nil nil nil nil nil nil)
											 ("fori" "for (${1:Object el} : ${2:iterator}) {\n    $0\n}\n" "fori" nil nil nil nil nil nil)
											 ("if" "if (${1:condition) {\n   $0\n}" "if" nil nil nil nil nil nil)
											 ("ife" "if (${1:cond}) {\n    $2\n}\nelse {\n     $3\n}" "ife" nil nil nil nil nil nil)
											 ("imp" "import ${1:System.};\n$0" "import" nil nil nil nil nil nil)
											 ("iterator" "public Iterator<${1:type}> iterator() {\n       $0\n}\n" "iterator" nil nil nil nil nil nil)
											 ("doc" "/**\n * $0\n *\n */" "javadoc" nil nil nil nil nil nil)
											 ("main" "public static void main(String[] args) {\n       $0\n}" "main" nil nil nil nil nil nil)
											 ("method" "${1:public }${2:void} ${3:name} (${4:args}) {\n           $0\n}" "method" nil nil nil nil nil nil)
											 ("new" "${1:Type} ${2:obj} = new ${3:Constr}(${4:args});\n$0" "new" nil nil nil nil nil nil)
											 ("param" "@param ${1:paramater} $0" "param" nil nil nil nil nil nil)
											 ("printf" "System.out.printf(\"$0%n\");" "printf" nil nil nil nil nil nil)
											 ("pr" "System.out.println(\"${1:text}\");\n$0" "println" nil nil nil nil nil nil)
											 ("ret" "@return ${1:description}" "return" nil nil nil nil nil nil)
											 ("test" "@Test\npublic void ${1:Case}() {\n       $0\n}" "test" nil nil nil nil nil nil)
											 ("tc" "import junit.framework.*;\nimport junit.textui.*;\n\npublic class Test${1:Class} extends TestCase {\n       protected void setUp() {\n                 $0\n       }\n}" "testClass" nil nil nil nil nil nil)
											 ("." "this.$0" "this" nil nil nil nil nil nil)
											 ("toStr" "public String toString() {\n       $0\n}" "toString" nil nil nil nil nil nil)
											 ("try" "try {\n    $0\n}\ncatch (${1:Throwable e}) {\n      ${2:System.out.println(\"Error \" + e.getMessage());\n      e.printStackTrace();}\n}\n" "try" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sun May 18 23:11:13 2014
