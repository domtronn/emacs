;;; grunt-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "grunt" "grunt.el" (21953 47010 0 0))
;;; Generated autoloads from grunt.el

(autoload 'grunt-exec "grunt" "\
Run tasks from gruntfile.  Calling with PFX will clear the cache of tasks.

You can also manually enter in any valid task at the prompt, even
if it's not suggested.  It will get/create one buffer per task
per project, as needed.

When invoked with a prefix argument, we'll clear the tasks cache
for you.  Note that if `grunt-show-all-tasks' is nil, the
cache (and the prefix argument functionality of this function) is
immaterial.

\(fn &optional PFX)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; grunt-autoloads.el ends here
