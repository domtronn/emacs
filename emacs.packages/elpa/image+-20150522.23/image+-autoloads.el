;;; image+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "image+" "image+.el" (21860 7959 0 0))
;;; Generated autoloads from image+.el

(autoload 'imagex-sticky-mode "image+" "\
To manipulate Image at point.

\(fn &optional ARG)" t nil)

(defvar imagex-global-sticky-mode nil "\
Non-nil if Imagex-Global-Sticky mode is enabled.
See the command `imagex-global-sticky-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `imagex-global-sticky-mode'.")

(custom-autoload 'imagex-global-sticky-mode "image+" nil)

(autoload 'imagex-global-sticky-mode "image+" "\
Toggle Imagex-Sticky mode in all buffers.
With prefix ARG, enable Imagex-Global-Sticky mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Imagex-Sticky mode is enabled in all buffers where
`imagex-sticky-mode-maybe' would do it.
See `imagex-sticky-mode' for more information on Imagex-Sticky mode.

\(fn &optional ARG)" t nil)

(defvar imagex-auto-adjust-mode nil "\
Non-nil if Imagex-Auto-Adjust mode is enabled.
See the command `imagex-auto-adjust-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `imagex-auto-adjust-mode'.")

(custom-autoload 'imagex-auto-adjust-mode "image+" nil)

(autoload 'imagex-auto-adjust-mode "image+" "\
Adjust image to current frame automatically in `image-mode'.

Type \\[imagex-sticky-restore-original] to restore the original image.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; image+-autoloads.el ends here
