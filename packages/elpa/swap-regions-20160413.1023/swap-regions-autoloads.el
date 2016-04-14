;;; swap-regions-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "swap-regions" "swap-regions.el" (22287 20620
;;;;;;  0 0))
;;; Generated autoloads from swap-regions.el

(defvar swap-regions-mode nil "\
Non-nil if Swap-Regions mode is enabled.
See the command `swap-regions-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `swap-regions-mode'.")

(custom-autoload 'swap-regions-mode "swap-regions" nil)

(autoload 'swap-regions-mode "swap-regions" "\
Toggle Swap-Regions mode on or off.
With a prefix argument ARG, enable Swap-Regions mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{swap-regions-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'swap-regions "swap-regions" "\
Swap the current region and the previous region.

Prefixed with one \\[universal-argument], replace the current
region with the previous region.

Prefixed with two \\[universal-argument]'s, replace the previous
region with the current region.

\(fn BEG END &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; swap-regions-autoloads.el ends here
