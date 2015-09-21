;;; annotate-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "annotate" "annotate.el" (22011 50256 0 0))
;;; Generated autoloads from annotate.el

(let ((loads (get 'annotate 'custom-loads))) (if (member '"annotate" loads) nil (put 'annotate 'custom-loads (cons '"annotate" loads))))

(autoload 'annotate-mode "annotate" "\
Toggle Annotate mode.

\(fn &optional ARG)" t nil)

(defvar annotate-file "~/.annotations" "\
File where annotations are stored.")

(custom-autoload 'annotate-file "annotate" t)

(defface annotate-highlight '((t (:underline "coral"))) "\
Face for annotation highlights." :group (quote annotate))

(defface annotate-annotation '((t (:background "coral" :foreground "black"))) "\
Face for annotations." :group (quote annotate))

(defvar annotate-annotation-column 85 "\
Where annotations appear.")

(custom-autoload 'annotate-annotation-column "annotate" t)

(defvar annotate-diff-export-context 2 "\
How many lines of context to include in diff export.")

(custom-autoload 'annotate-diff-export-context "annotate" t)

(defvar annotate-use-messages t "\
Whether status messages may appear in the minibuffer.")

(custom-autoload 'annotate-use-messages "annotate" t)

(autoload 'annotate-annotate "annotate" "\
Create, modify, or delete annotation.

\(fn)" t nil)

(autoload 'annotate-save-annotations "annotate" "\
Save all annotations to disk.

\(fn)" t nil)

(autoload 'annotate-export-annotations "annotate" "\
Export all annotations as a unified diff file.
An example might look like this:

--- /home/bastibe/Projects/annotate.el/annotate.el	2015-06-19 15:13:36.718796738 +0200
+++ /home/bastibe/Projects/annotate.el/annotate.el	2015-06-19 15:13:36.718796738 +0200
@@ -73,5 +73,5 @@
 ;;;###autoload
 (defface annotate-highlight
-  '((t (:underline \"coral\")))
+  '((t (:underline \"coral\")))
#        ~~~~~~~~~~~~~~~~~~
#        this doesn't work in cli
   \"Face for annotation highlights.\"
   :group 'annotate)

This diff does not contain any changes, but highlights the
annotation, and can be conveniently viewed in diff-mode.

\(fn)" t nil)

(autoload 'annotate-load-annotations "annotate" "\
Load all annotations from disk.

\(fn)" t nil)

(autoload 'annotate-clear-annotations "annotate" "\
Clear all current annotations.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; annotate-autoloads.el ends here
