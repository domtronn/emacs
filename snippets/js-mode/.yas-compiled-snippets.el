;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
										 '(("al" "alert($0);" "alert" nil nil nil nil nil nil)
											 ("class" "var ${1:name} = new Class({\n  initialize: function($2) {\n    $0\n  }\n});" "Class" nil nil nil nil nil nil)
											 ("com" "/*\n * $0\n */" "comment (/* ... */)" nil nil nil nil nil nil)
											 ("def" "${1:name}: function ($2) {\n  $0\n}," "function" nil nil nil nil nil nil)
											 ("each" "${1:collection}.each(function($2) {\n  $0\n});" "each" nil nil nil nil nil nil)
											 ("el" "else {\n  $0\n}" "else" nil nil nil nil nil nil)
											 ("ev.add" "addEvent('${1:event}', function($2) {\n  $0\n});" "addEvent" nil nil nil nil nil nil)
											 ("ev.fire" "fireEvent('$0')" "fireEvent" nil nil nil nil nil nil)
											 ("f" "function($1) {\n  $0\n}" "anonymous function" nil nil nil nil nil nil)
											 ("for" "for(var ${1:i} = ${2:0}; $1 < ${3:collection}.length; $1++) {\n  $0\n}" "for" nil nil nil nil nil nil)
											 ("if" "if (${1:condition}) {\n  $0\n}" "if" nil nil nil nil nil nil)
											 ("init" "initialize: function($1) {\n  $0\n}" "Constructor" nil nil nil nil nil nil)
											 ("log" "console.log($0);" "console.log" nil nil nil nil nil nil)
											 ("mem" "${1:$(concat \"this._\" yas-text)} = ${1:variable};\n$0" "member variable" nil nil nil nil nil nil)
											 ("req.def" "require.def(\"`(replace-regexp-in-string \".*script/\" \"/sprtiptvjs/\" (file-name-sans-extension (buffer-file-name)))`\",\n	[\n		\n	],\n\n	function ($1) {\n		\"use strict\";	\n\n		return ({\n\n			init: function () {\n				\n			}\n\n		});\n\n	}\n);\n" "require def" nil nil
												((yas/indent-line 'fixed))
												nil nil nil)
											 ("req.html" "new Request.HTML({\n  onSuccess: function(responseTree, responseElements, responseHTML, responseJavaScript) {\n    $0\n  }\n}).${1:get}(${2:url});" "html" nil nil nil nil nil nil)
											 ("req.json" "new Request.JSON({\n  onSuccess: function(responseJSON, responseText) {\n    $0\n  }\n}).${1:send}(${2:url});" "json" nil nil nil nil nil nil)
											 ("reqdef" "`(setq use-underscores nil)``(setq available-exts nil)`require.def(\"`(replace-regexp-in-string \".*script/\" \"sprtiptvjs/\" (file-name-sans-extension (buffer-file-name)))`\",\n	[\n${1:$(inject-dependency yas-text)}\n	],\n\n	function (${1:$$(setq available-exts yas-text)}) {\n		\"use strict\";\n${3:$(when yas-modified-p (format-member-vars-single-line yas-text use-underscores))}\n		return ${2:$$(concat (yas-choose-value (split-string available-exts \",\\\\s-*\")) \".extend\")}({\n\n			init: function (${3:Variables}) {\n				${3:$(when yas-modified-p (format-init-members yas-text use-underscores))}\n			}${3:$(when yas-modified-p (format-getters yas-text use-underscores))}\n\n		});\n\n	}\n);" "require def" nil nil
												((yas/indent-line 'fixed))
												nil nil nil)
											 ("reqgetter" "			get${1:$$(upcase-initials yas-text)}: function () {\n				return $2;\n			}" "Define Getter" nil nil
												((yas/indent-line 'fixed))
												nil nil nil)
											 ("spy" "var $1${2:$(upcase-initials yas-text)}Spy = this.sandbox.spy(${1:Class}, \"${2:Method}\");\n$0" "Spy" nil nil nil nil nil nil)
											 ("stub" "this.sandbox.stub(${1:class}.prototype);" "Stub" nil nil nil nil nil nil)
											 ("stubret" "this.sandbox.stub(${1:class}, \"${2:method}\").returns(${3:value});\n$0" "Stub.ret" nil nil nil nil nil nil)
											 ("stubvar" "var stub${1:$(upcase-initials yas-text)} = this.sandbox.stub(${1:class}.prototype);\n$0" "Stub var" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sun May 18 23:11:13 2014
