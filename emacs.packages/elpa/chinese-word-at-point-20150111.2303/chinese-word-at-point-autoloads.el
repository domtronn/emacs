;;; chinese-word-at-point-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "chinese-word-at-point" "chinese-word-at-point.el"
;;;;;;  (21712 57141 0 0))
;;; Generated autoloads from chinese-word-at-point.el

(autoload 'chinese-word-at-point "chinese-word-at-point" "\
Return the (most likely) Chinese word at point, or nil if none is found.

\(fn)" nil nil)

(autoload 'chinese-or-other-word-at-point "chinese-word-at-point" "\
Return the Chinese or other language word at point, or nil if none is found.

Here's \"other\" denotes any language words that Emacs can understand,
i.e. (thing-at-point 'word) can get proper word.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; chinese-word-at-point-autoloads.el ends here
