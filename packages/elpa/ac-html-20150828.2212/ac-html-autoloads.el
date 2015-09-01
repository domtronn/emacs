;;; ac-html-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ac-haml" "ac-haml.el" (21989 21268 0 0))
;;; Generated autoloads from ac-haml.el

(ac-html-define-ac-source "haml" :tag-prefix "%\\(.*\\)" :attr-prefix ":\\(.*\\)" :attrv-prefix ac-haml-attrv-prefix :current-tag-func ac-haml-current-tag :current-attr-func ac-haml-current-attr)

;;;***

;;;### (autoloads nil "ac-html" "ac-html.el" (21989 21268 0 0))
;;; Generated autoloads from ac-html.el

(ac-html-define-ac-source "html" :tag-prefix ac-html-tag-prefix :attr-prefix ac-html-attr-prefix :attrv-prefix ac-html-value-prefix :current-tag-func ac-html-current-tag :current-attr-func ac-html-current-attr)

;;;***

;;;### (autoloads nil "ac-html-core" "ac-html-core.el" (21989 21268
;;;;;;  0 0))
;;; Generated autoloads from ac-html-core.el

(autoload 'ac-html-define-data-provider "ac-html-core" "\
Define ac-html data provider with this macro.
This macro is buggy and cannot be used now.

\(fn PROVIDER &rest PAIRS)" nil t)

(put 'ac-html-define-data-provider 'lisp-indent-function '1)

(autoload 'ac-html-enable-data-provider "ac-html-core" "\
Enable data provider PROVIDER.

\(fn PROVIDER)" nil nil)

(autoload 'ac-html-define-ac-source "ac-html-core" "\
Define ac-html lang with this macro.

\(fn LANG &rest PAIRS)" nil t)

(put 'ac-html-define-ac-source 'lisp-indent-function '1)

;;;***

;;;### (autoloads nil "ac-html-default-data-provider" "ac-html-default-data-provider.el"
;;;;;;  (21989 21268 0 0))
;;; Generated autoloads from ac-html-default-data-provider.el

(ac-html-define-data-provider 'ac-html-default-data-provider :tag-func 'ac-html-default-tags :attr-func 'ac-html-default-attrs :attrv-func 'ac-html-default-attrvs :tag-doc-func 'ac-html-default-tag-doc :attr-doc-func 'ac-html-default-attr-doc :attrv-doc-func 'ac-html-default-attrv-doc)

;;;***

;;;### (autoloads nil "ac-jade" "ac-jade.el" (21989 21268 0 0))
;;; Generated autoloads from ac-jade.el

(ac-html-define-ac-source "jade" :tag-prefix "^[	 ]*\\(.*\\)" :attr-prefix "\\(?:,\\|(\\)[ ]*\\(.*\\)" :attrv-prefix ac-jade-attrv-prefix :current-tag-func ac-jade-current-tag :current-attr-func ac-jade-current-attr)

;;;***

;;;### (autoloads nil "ac-slim" "ac-slim.el" (21989 21268 0 0))
;;; Generated autoloads from ac-slim.el

(ac-html-define-ac-source "slim" :tag-prefix ac-slim-tag-prefix :attr-prefix ac-slim-attr-prefix :attrv-prefix ac-slim-attrv-prefix :current-tag-func ac-slim-current-tag :current-attr-func ac-slim-current-attr)

;;;***

;;;### (autoloads nil nil ("ac-html-pkg.el" "web-completion-data.el")
;;;;;;  (21989 21268 283522 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ac-html-autoloads.el ends here
