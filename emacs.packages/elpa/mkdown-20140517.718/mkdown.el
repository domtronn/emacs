;;; mkdown.el --- Pretty Markdown previews based on mkdown.com

;; Author: Andrew Tulloch
;; URL: https://github.com/ajtulloch/mkdown.el
;; Version: 0.1
;; Created: 17-05-2014
;; Keywords: markdown
;; Package-Requires: ((markdown-mode "2.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A clean stylesheet for Markdown files in Emacs.
;; See mkdown.com for the web equivalent.  CSS taken from mkdown.com
;; under the MIT License.

;;; License:

;; The MIT License (MIT)

;; Copyright (c) 2014 Andrew Tulloch

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.



;;; Code:

(require 'markdown-mode)

;;;###autoload
(defconst mkdown-css-file-name
  (expand-file-name
   "mkdown.css"
   (file-name-directory (or load-file-name buffer-file-name)))
  "The absolute path of the mkdown.css file.")

(provide 'mkdown)

;;; mkdown.el ends here
