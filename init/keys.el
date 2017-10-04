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
(global-set-key (kbd "M-*") '(lambda () (interactive) (insert "•")))
(global-set-key (kbd "M-<kp-decimal>") 'dabbrev-expand)
(global-set-key (kbd "M-S-SPC") 'kill-whitespace)

(global-set-key (kbd "<M-return>") '(lambda () (interactive) (end-of-line) (smart-newline)))

(global-set-key (kbd "<s-mouse-1>") 'goto-address-at-point)

;; Closing Files
(global-set-key (kbd "s-W") 'kill-current-buffer)

(global-set-key (kbd "s-<") 'fill-paragraph)
(global-set-key (kbd "s->") 'unfill-paragraph)

(global-set-key (kbd "H-`") 'other-window)

(global-set-key (kbd "s-h") 'goto-line)

(global-set-key (kbd "s-p") 'backward-list)
(global-set-key (kbd "s-n") 'forward-list)

;; Alignment
(global-set-key (kbd "C-x a a") 'align-regexp)
(global-set-key (kbd "C-x a (")
                '(lambda (b e) (interactive "r") (align-regexp b e (rx (group (zero-or-more (syntax whitespace))) "(") 1 1)))
(global-set-key (kbd "C-x a :")
                '(lambda (b e) (interactive "r") (align-regexp b e (rx (group (zero-or-more (syntax whitespace))) ":") 1 1)))
(global-set-key (kbd "C-x a =")
                '(lambda (b e) (interactive "r") (align-regexp b e (rx (group (zero-or-more (syntax whitespace))) "=") 1 1)))
(global-set-key (kbd "C-x a .")
                '(lambda (b e) (interactive "r") (align-regexp b e (rx (group (zero-or-more (syntax whitespace))) ".") -1 0 nil)))

;; Undo and Redo
(global-set-key (kbd "M-;") 'semi-colon-end)
(global-set-key (kbd "M-?") 'ispell-word)

(global-set-key "\M-u" 'upcase-case-next-letter)
(global-set-key "\M-l" 'downcase-case-next-letter)

(global-set-key "\M-U" 'upcase-word)
(global-set-key "\M-L" 'downcase-word)

;; Allow for join lines backwards
(global-set-key (kbd "C-j") 'join-line)

(global-set-key (kbd "s-m") 'mark-paragraph)
(global-set-key (kbd "s-:") 'eval-region)

(global-unset-key (kbd "M-<down-mouse-1>"))

;; Tag searching
(global-set-key (kbd "H-,") 'pop-tag-mark)
(global-set-key (kbd "H-<") 'pop-to-mark-command)
(global-set-key (kbd "H-s-,") '(lambda () (interactive) (ring-insert find-tag-marker-ring (point-marker))))

;; move and resize windows
(global-set-key (kbd "H-+") '(lambda () (interactive) (text-scale-adjust 1)))
(global-set-key (kbd "H-_") '(lambda () (interactive) (text-scale-adjust -1)))
(global-set-key (kbd "H--") '(lambda () (interactive) (global-text-scale-adjust -1)))
(global-set-key (kbd "H-=") '(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "<H-backspace>") '(lambda () (interactive) (global-text-scale-set 0)))

(global-set-key (kbd "s-]") 'exchange-point-and-mark)

;; Comment regions
(global-set-key (kbd "s-/") 'dgc-comment)

;; F Functions
(global-set-key (kbd "H-£") '(lambda () (interactive) (dired (when (buffer-file-name) (file-name-directory (buffer-file-name))))))
(global-set-key (kbd "H-M-(")
                '(lambda () (interactive)
                   (if (or (not (frame-parameter (selected-frame) 'alpha))
                           (eq 100 (frame-parameter (selected-frame) 'alpha)))
                       (set-frame-parameter (selected-frame) 'alpha '60)
                     (set-frame-parameter (selected-frame) 'alpha '100))))
(global-set-key (kbd "H-)") '(lambda () (interactive) (find-file user-init-file)))

(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "C-S-d") 'backward-delete-char)
(global-set-key (kbd "H-d") 'smart-delete-pair)
(global-set-key [(control backspace)] 'backward-kill-word)

;; Buffer Movemenet
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-c C-,") 'whitespace-mode)
(global-set-key (kbd "C-c C-.") 'whitespace-cleanup)

(global-set-key (kbd "M-P") '(lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-N") '(lambda () (interactive) (next-line 5)))

(global-set-key (kbd "C-z") 'ahahah)

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-k") 'kill-line)

;; Duplicate line
(global-set-key (kbd "s-d") 'duplicate-line)
(global-set-key (kbd "s-D")
                '(lambda () (interactive)
                   (call-interactively 'duplicate-line-and-replace-regexp)))

(global-set-key (kbd "<s-return>") 'eval-defun)
(global-set-key (kbd "<s-S-return>") 'eval-buffer)

;; Regexp snippets
(global-set-key (kbd "C-c C-v i") '(lambda () (interactive) (insert "\\(.*?\\)")))

(global-set-key "\M-]" 'next-error)
(global-set-key "\M-[" 'previous-error)

(global-set-key (kbd "s-k") '(lambda () (interactive) (kill-buffer (buffer-name))))
(global-set-key (kbd "s-K") 'kill-buffer-regexps)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-s") 'save-buffer)

(global-set-key (kbd "s-a") 'mark-whole-buffer)

(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)

(global-set-key (kbd "s-o") 'open-current-file)
(global-set-key (kbd "s-O") 'open-urls-in-file)
(global-set-key (kbd "s-0") '(lambda () (interactive) (text-scale-set 0.2)))

(provide 'keys)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
;;; keys.el ends here
