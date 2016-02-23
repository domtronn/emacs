;;; git-commit-insert-issue-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "git-commit-insert-issue" "git-commit-insert-issue.el"
;;;;;;  (22220 16302 0 0))
;;; Generated autoloads from git-commit-insert-issue.el

(autoload 'git-commit-insert-issue-get-issues "git-commit-insert-issue" "\
Get all the issues from the current project.
   Return a list.

\(fn &optional USERNAME PROJECT-NAME)" nil nil)

(autoload 'git-commit-insert-issue-mode "git-commit-insert-issue" "\
See the issues when typing 'Fixes #' in a commit message.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; git-commit-insert-issue-autoloads.el ends here
