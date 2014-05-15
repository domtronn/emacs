;; My Keys
;; note: C-h k is the command used to discover keypresses
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(global-set-key [C-return] 'dabbrev-expand)
(global-set-key [S-tab] 'ac-expand)

(global-set-key (kbd "s-_") 'hide-all-functions)
(global-set-key (kbd "s--") 'hs-hide-block)
(global-set-key (kbd "s-=") 'hs-show-block)
(global-set-key (kbd "s-+") 'hs-show-all)

(global-set-key [s-right] 'end-of-line)           ;; Cmd Right goes to eol
(global-set-key [s-left] 'beginning-of-line)      ;; Cmd Left goes to bol

(global-set-key [C-right] 'dgc-forward-word-2)
(global-set-key [C-left] 'dgc-backward-word)

;; Navigate parantheses
(global-set-key [M-right] 'forward-list)           ;; Alt Right goes forward delimiter
(global-set-key [M-left] 'backward-list)           ;; Alt left goes to 

(global-set-key [C-tab] 'file-cache-ido-find-file)
(global-set-key (kbd "C-x f") 'ido-find-file)

;; change to use regexp searching rather than normal isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-?") 'query-replace-regexp)

;; Allow for join lines backwards
(global-set-key (kbd "C-j") 'join-line)

(global-set-key (kbd "M-s-¬") 'jshint-code)

(global-set-key (kbd "s-m") 'mark-paragraph)
(global-set-key (kbd "s-:") 'eval-region)

;; Bring up shell terminal
(global-set-key (kbd "C-~") 'shell-pop)

;; Tag searching
(global-set-key (kbd "H-.") 'etags-select-find-tag-at-point)
(global-set-key (kbd "H-?") 'etags-select-find-tag)
(global-set-key (kbd "H-,") 'pop-tag-mark)

;; move and resize windows
(global-set-key [M-s-left] 'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up] 'windmove-up)
(global-set-key [M-s-down] 'windmove-down)

(global-set-key (kbd "H--") 'shrink-window-horizontally)
(global-set-key (kbd "H-_") 'shrink-window)
(global-set-key (kbd "H-=") 'enlarge-window-horizontally)
(global-set-key (kbd "H-+") 'enlarge-window)

;; Comment regions
(global-set-key (kbd "s-/") 'dgc-comment)
(global-set-key (kbd "C-c C-c") 'dgc-comment)

(global-set-key [f2] 'set-up-rgrep-results)
(global-set-key (kbd "<M-f2>") 'set-up-rgrep-results-with-prompt)

(global-set-key [f5] 'vc-ediff)
(global-set-key [f4] 'vc-next-action)
(global-set-key [f6] 'my-vc-dir)
(global-set-key (kbd "<M-f6>") '(lambda () (interactive) (progn (if (buffer-exists "*vc-dir*") (kill-buffer "*vc-dir*")) (my-vc-dir))))

(global-set-key [f8] 'run-current-file)

;; Buffer Movemenet
(global-set-key [S-wheel-down] '(lambda () (interactive) (dgc-scroll-up-in-place 1)))
(global-set-key [S-wheel-up] '(lambda () (interactive) (dgc-scroll-down-in-place 1)))

;; Up and Down multiple lines at once
(global-set-key [C-up] '(lambda () (interactive) (previous-line 5)))
(global-set-key [C-down] '(lambda () (interactive) (next-line 5)))

(global-set-key [S-M-up] '(lambda () (interactive) (previous-line 15)))
(global-set-key [S-M-down] '(lambda () (interactive) (next-line 15)))

(global-set-key (kbd "C-z") 'ahahah)

;; Function needs some work
;; (global-set-key (kbd "C-x t") 'open-test)
;; (global-set-key (kbd "s-t") 'open-test)


;; Copy and Kill
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-k") 'kill-line)

(global-set-key [s-down] 'move-line-region-down)
(global-set-key [s-up] 'move-line-region-up)

(global-set-key [(control shift backspace)] 'kill-word)
(global-set-key [(control backspace)] 'backward-kill-word)

(global-set-key (kbd "s-d") "\C-a\C- \C-n\M-w\C-y\C-p") ;; Duplicate line


;; Window Navigation
(global-set-key (kbd "C-x C-x") 'delete-other-windows)

(global-set-key (kbd "C-,") 'rotate-windows)
(global-set-key (kbd "H-l") (kbd "C-l"))

(global-set-key (kbd "C-S-w") 'mark-word)

;; Amazing mode to mark all instances in a definiton
(global-set-key (kbd "C->") 'mc/mark-all-like-this-in-defun)
(global-set-key (kbd "C-<") 'my-mark-all-like-this)
(global-set-key (kbd "s-e") 'my-mark-all-like-this)

(global-set-key (kbd "H-t") 'grunt)
(global-set-key (kbd "S-s-t") 'grunt)
(global-set-key (kbd "s-r") 'grunt-this-test-file)

;; Use C-i as my personal prefix command
(global-set-key (kbd "H-a") 'beginning-of-line)
(global-set-key (kbd "H-e") 'end-of-line)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'goto-line)

(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

(global-set-key (kbd "S-<return>") 'yas/expand)

(global-set-key "\M-}" 'flycheck-mode)
(global-set-key "\M-]" 'flycheck-next-error)
(global-set-key "\M-[" 'flycheck-previous-error)

;; It's Key Chord Time!
(key-chord-define-global "IO" 'open-in-and-activate-intellj)
(key-chord-define-global "??" 'set-up-rgrep-results-with-prompt)
(key-chord-define-global "?/" 'occur-at-point)

(provide 'keys)
;;; keys.el ends here
