;;; sticky-windows-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "sticky-windows" "sticky-windows.el" (21646
;;;;;;  42513 0 0))
;;; Generated autoloads from sticky-windows.el

(autoload 'sticky-window-keep-window-visible "sticky-windows" "\
Insure the buffer associated with the current window stays visible.
This is handy for ERC buffers where you would like to see the
conversation while you work in other windows within the frame. 
This is intended to be used with `sticky-window-delete-window'.
A prefix arg reverses this operation.

\(fn)" t nil)

(autoload 'sticky-window-delete-window "sticky-windows" "\
This is intended to be a replacement for `delete-window', but
that avoids deleting windows that have been marked as dedicated
with `sticky-window-keep-window-visible'.

\(fn)" t nil)

(autoload 'sticky-window-delete-other-windows "sticky-windows" "\
Delete all other windows that are not marked to be visible with `sticky-window-keep-window-visible'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; sticky-windows-autoloads.el ends here
