;;; operate-on-number-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (operate-on-number-at-point apply-operation-to-number-at-point
;;;;;;  find-number-at-point) "operate-on-number" "operate-on-number.el"
;;;;;;  (21483 26893 0 0))
;;; Generated autoloads from operate-on-number.el

(autoload 'find-number-at-point "operate-on-number" "\
Search the current line till EOL for a number.
If a pure number is found, move point to the beginning of the
number and return the value.  Raise an error otherwise.

\(fn)" t nil)

(autoload 'apply-operation-to-number-at-point "operate-on-number" "\
Apply an operation specified by KEY on a number at point.

If called interactively, use the last key input as KEY.

If the operation requires an additional operand, it is taken from
one of the following sources in the order named:

1. Prefix argument if given

2. Value read from keyboard if READ-ARGS is non-nil or the :read
   property is non-nil

3. Default argument predefined in `operate-on-number-at-point-alist'

\(fn &optional KEY READ-ARGS)" t nil)

(autoload 'operate-on-number-at-point "operate-on-number" "\
Operate on number at point.

The kind of operation to perform is specified by the following
key typed.

An optional number ARG becomes a counter operand to the number at
point for the operation if applicable.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("operate-on-number-pkg.el") (21483 26893
;;;;;;  471510 0))

;;;***

(provide 'operate-on-number-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; operate-on-number-autoloads.el ends here
