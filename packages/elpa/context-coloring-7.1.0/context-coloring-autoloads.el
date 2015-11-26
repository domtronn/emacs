;;; context-coloring-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "context-coloring" "context-coloring.el" (22102
;;;;;;  22798 0 0))
;;; Generated autoloads from context-coloring.el

(autoload 'context-coloring-mode "context-coloring" "\
Toggle contextual code coloring.
With a prefix argument ARG, enable Context Coloring mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Context Coloring mode is a buffer-local minor mode.  When
enabled, code is colored by scope.  Scopes are colored
hierarchically.  Variables referenced from nested scopes retain
the color of their defining scopes.  Certain syntax, like
comments and strings, is still colored with `font-lock'.

The entire buffer is colored initially.  Changes to the buffer
trigger recoloring.

Define your own colors by customizing faces like
`context-coloring-level-N-face', where N is a number starting
from 0.  If no face is found on a custom theme nor the `user'
theme, the defaults are used.

New language / major mode support can be added with
`context-coloring-define-dispatch', which see.

Feature inspired by Douglas Crockford.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; context-coloring-autoloads.el ends here
