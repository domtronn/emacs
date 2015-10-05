;;; keys.el --- My key bindings

;; Copyright (C) 2014  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; My Keys
;; note: C-h k is the command used to discover key-presses
(global-set-key (kbd "M-£") '(lambda () (interactive) (insert "#")))
(global-set-key [C-return] 'dabbrev-expand)
(global-set-key [S-tab] 'auto-complete)
(global-set-key (kbd "M-\\") 'kill-whitespace)
(global-set-key [M-tab] 'dabbrev-expand)

(global-set-key (kbd "SPC") 'wrap-space-or-space)

(global-set-key (kbd "s-V") 'helm-show-kill-ring)

(global-set-key (kbd "s-.") 'smart-forward)
(global-set-key (kbd "C-.") 'smart-forward)

(global-set-key (kbd "s-,") 'smart-backward)
(global-set-key (kbd "C-,") 'smart-backward)

(global-set-key (kbd "RET") 'smart-newline)
;; (global-set-key (kbd "M-\"") 'add-quotation)

;; Ace jump keys
(global-set-key (kbd "C-c SPC") 'ace-jump-char-mode)
(global-set-key (kbd "C-c C-x SPC") 'ace-jump-zap-to-char)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-word-mode)

(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-c w") 'esw/select-window)

;; Closing Files
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-W")
  '(lambda () (interactive) (kill-buffer (buffer-name))))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; Alignment
(global-set-key (kbd "C-x a a") 'align-regexp)
(global-set-key (kbd "C-x a :")
                '(lambda (b e) (interactive "r") (align-regexp b e (rx (group (zero-or-more (syntax whitespace))) ":") 1 1)))
(global-set-key (kbd "C-x a =")
                '(lambda (b e) (interactive "r") (align-regexp b e (rx (group (zero-or-more (syntax whitespace))) "=") 1 1)))
(global-set-key (kbd "C-x a .")
                '(lambda (b e) (interactive "r") (align-regexp b e (rx (group (zero-or-more (syntax whitespace))) ".") -1 0 nil)))

;; Undo and Redo
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-y") 'undo-tree-redo)
(global-set-key (kbd "C-+") 'undo-tree-redo)

(global-set-key (kbd "s-_") 'hs-show-all)
(global-set-key (kbd "s--") 'hs-show-block)
(global-set-key (kbd "s-=") 'hs-toggle-hiding)
(global-set-key (kbd "s-+") 'hs-hide-level)

(global-set-key (kbd "M-;") 'semi-colon-end)

(global-set-key (kbd "H-f") 'ac-isearch)

(global-set-key [C-right] 'forward-word)
(global-set-key [C-left] 'backward-word)
(global-set-key [M-right] 'forward-word)
(global-set-key [M-left] 'backward-word)

(global-set-key [C-tab] 'projectable-find-file)
(global-set-key (kbd "C-S-<tab>") 'projectable-find-file-other-window)
(global-set-key (kbd "C-S-x C-S-f") 'file-cache-ido-find-file)

(global-set-key (kbd "C-x f") 'ido-find-file)

;; change to use regexp searching rather than normal isearch
(global-set-key (kbd "H-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-set-key (kbd "M-?") 'ispell-word)

;; Allow for join lines backwards
(global-set-key (kbd "C-j") 'join-line)
(global-set-key (kbd "M-s-¬") 'jshint-code)

(global-set-key (kbd "s-m") 'mark-paragraph)
(global-set-key (kbd "s-:") 'eval-region)

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; Bring up shell terminal
(global-set-key (kbd "C-`") 'shell-pop)

;; Tag searching
(global-set-key (kbd "H-.") 'etags-select-find-tag-at-point);;etags-select-find-tag-at-point)
(global-set-key (kbd "H-?") 'etags-select-find-tag)
(global-set-key (kbd "H-,") 'pop-tag-mark)
(global-set-key (kbd "H-<") 'pop-to-mark-command)

;; move and resize windows
(global-set-key [M-s-left] 'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up] 'windmove-up)
(global-set-key [M-s-down] 'windmove-down)

(global-set-key (kbd "H--") '(lambda () (interactive) (font-scale '- 10)))
(global-set-key (kbd "H-=") '(lambda () (interactive) (font-scale '+ 10)))

;; Comment regions
(global-set-key (kbd "s-/") 'dgc-comment)

(global-set-key [f1] 'neotree-toggle)
(global-set-key (kbd "<S-f1>") '(lambda () (interactive) (call-interactively 'neotree-find)))
(global-set-key [f2] 'ag-regexp-project-at-point)
(global-set-key (kbd "<M-f2>") 'ag-regexp)

(global-set-key [f3] '(lambda () (interactive) (dired (if (buffer-file-name) (file-name-directory (buffer-file-name)) USERPATH))))
(global-set-key [f4] 'run-current-file)
(global-set-key (kbd "<S-f4>") 'alt-run-current-file)

(global-set-key [f5] 'vc-ediff)
(global-set-key (kbd "<M-f5>") 'vc-next-action)
(global-set-key [f6] 'my-vc-dir)
(global-set-key (kbd "<M-f6>") '(lambda () (interactive) (progn (if (buffer-exists "*vc-dir*") (kill-buffer "*vc-dir*")) (my-vc-dir))))

(global-set-key [f7] 'occur-at-point)

(global-set-key [f8] 'emms-smart-browse)
(global-set-key (kbd "<S-f8>") 'go-to-emms-browser)
(global-set-key (kbd "<s-f8>") 'go-to-emms-playlist)
(global-set-key (kbd "<S-s-f8>") 'get-lyrics-and-display)
(global-set-key (kbd "<M-f9>") 'emms-next)
(global-set-key (kbd "<M-f8>") 'emms-pause)
(global-set-key (kbd "<M-f7>") 'emms-previous)

(global-set-key (kbd "<M-S-f1>") 'toggle-frame-fullscreen)

(global-set-key [f11] 'browse-sandbox)
(global-set-key [f10] 'xkcd)

(global-set-key [M-d] 'kill-word)
(global-set-key [(control backspace)] 'backward-kill-word)

(global-set-key (kbd "C-c P") 'emms-pause)

(global-set-key (kbd "s-@") 'operate-on-number-at-point)

;; Buffer Movemenet

(global-set-key (kbd "s-[") 'backward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "s-]") 'forward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "s-N") 'drag-stuff-down)
(global-set-key [s-down] 'drag-stuff-down)
(global-set-key (kbd "s-P") 'drag-stuff-up)
(global-set-key [s-up] 'drag-stuff-up)

(global-set-key [M-up] '(lambda () (interactive) (previous-line 5)))
(global-set-key [M-down] '(lambda () (interactive) (next-line 5)))
(global-set-key [C-up] '(lambda () (interactive) (previous-line 5)))
(global-set-key [C-down] '(lambda () (interactive) (next-line 5)))

(global-set-key [S-M-up] '(lambda () (interactive) (previous-line 15)))
(global-set-key [S-M-down] '(lambda () (interactive) (next-line 15)))
(global-set-key (kbd "M-P") '(lambda () (interactive) (previous-line 15)))
(global-set-key (kbd "M-N") '(lambda () (interactive) (next-line 15)))
(global-set-key [C-S-M-up] '(lambda () (interactive) (progn(previous-line 15) (recenter))))
(global-set-key [C-S-M-down] '(lambda () (interactive) (progn (next-line 15) (recenter))))

(global-set-key (kbd "C-z") 'ahahah)

(global-set-key (kbd "C-x t") 'open-test)
(global-set-key (kbd "s-t") 'open-test)

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-k") 'kill-line)

(global-set-key (kbd "C-x C-x") 'sticky-window-delete-other-windows)
(global-set-key (kbd "C-S-x C-x") 'delete-other-windows)

(global-set-key (kbd "H-l") (kbd "C-l"))

;; Duplicate line
(global-set-key (kbd "s-d") 'duplicate-line)
(global-set-key (kbd "M-W") "\C-a\C- \C-n\M-w\C-p\C-e")

(global-set-key (kbd "C-S-w") 'mark-word)

;; Amazing mode to mark all instances in a definition
(global-set-key (kbd "H-s-r") 'mark-word-at-point)
(global-set-key (kbd "M-q") 'er/expand-region)
(global-set-key (kbd "H-q") 'er/expand-region)

(global-set-key (kbd "s-R") 'mc/mark-all-like-this)
(global-set-key (kbd "s-n") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "s-p") 'mc/mark-previous-symbol-like-this)

(global-set-key (kbd "H-t") 'grunt)
(global-set-key (kbd "S-s-t") 'grunt)
(global-set-key (kbd "C-M-g") 'grunt-exec)

;; Use C-i as my personal prefix command
(global-set-key (kbd "H-a") 'beginning-of-line)
(global-set-key (kbd "H-e") 'end-of-line)

(global-set-key (kbd "C-x C-b") 'projectable-switch-buffer)
(global-set-key (kbd "C-S-x C-b") 'ido-switch-buffer-other-window)

(global-set-key (kbd "C-)") 'sp-slurp-hybrid-sexp)
(global-set-key (kbd "s-f") 'sp-forward-slurp-sexp)
(global-set-key (kbd "s-b") 'sp-forward-barf-sexp)

(global-set-key (kbd "C-=") 'insert-random-return)

(global-set-key "\C-c\C-d" 'osx-dictionary-search-pointer)
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

(global-set-key (kbd "<s-return>") 'eval-defun)

(global-set-key "\M-}" 'flycheck-mode)
(global-set-key "\M-{" 'flyspell-mode)
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)

(global-set-key "\M-]" 'next-error)
(global-set-key "\M-[" 'previous-error)

(global-set-key (kbd "s-o") 'open-current-file)

(provide 'keys)
;;; keys.el ends here
