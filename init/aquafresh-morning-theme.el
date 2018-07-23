;;; aquafresh-morning-theme.el --- A modification of the twilight bright theme
;;
;; Copyright (c) 2018 Dom Charlesworth
;;
;; Version: 0.1.0
;; Keywords: themes
;; URL: https://github.com/jimeh/twilight-jazz-theme.el
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Code:

(require 'aquafresh-theme "./aquafresh-theme")

(deftheme aquafresh-morning "A soothing dark-on-light theme.")
(aquafresh-apply-custom-theme 'aquafresh-morning)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'aquafresh-morning)

;;; aquafresh-morning-theme.el ends here
