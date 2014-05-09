;; My Keys
;; note: C-h k is the command used to discover keypresses
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(global-set-key [C-return] 'dabbrev-expand)
(global-set-key [S-tab] 'ac-expand)

(global-set-key (kbd "s-_") 'hide-all-functions)
(global-set-key (kbd "s--") 'hs-hide-block)
(global-set-key (kbd "s-=") 'hs-show-block)
(global-set-key (kbd "s-+") 'hs-show-all)

(global-set-key [C-right] 'dgc-forward-word-2)
(global-set-key [C-left] 'dgc-backward-word)
(global-set-key [M-right] 'dgc-forward-word-2)
(global-set-key [M-left] 'dgc-backward-word)

;; ;; Skeleton pair parenthesis 
;; ;; This is now depreciated by the use of the autopair global mode
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe) 
;; (global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "'") 'skeleton-pair-insert-maybe)

;; Navigate parantheses
(global-set-key (kbd "s-.") 'forward-list)
(global-set-key (kbd "s-,") 'backward-list)

(global-set-key [C-tab] 'file-cache-ido-find-file)
(global-set-key (kbd "C-S-x C-S-f") 'file-cache-ido-find-file)

;; change to use regexp searching rather than normal isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-?") 'query-replace-regexp)

;; Allow for join lines backwards
(global-set-key (kbd "C-j") 'join-line)

;; Bring up shell terminal
(global-set-key (kbd "C-~") 'shell-pop)
(global-set-key (kbd "C-`") 'shell-pop)

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

(global-set-key [f1] 'dgc-copy-line)
(global-set-key [f2] 'yank)

(global-set-key [M-d] 'kill-word)
(global-set-key [(control backspace)] 'backward-kill-word)

;; Buffer Movemenet
(global-set-key [S-wheel-down] '(lambda () (interactive) (dgc-scroll-up-in-place 1)))
(global-set-key [S-wheel-up] '(lambda () (interactive) (dgc-scroll-down-in-place 1)))

(global-set-key [C-down] 'move-line-region-down)
(global-set-key [C-up] 'move-line-region-up)

(global-set-key [M-up] '(lambda () (interactive) (previous-line 5)))
(global-set-key [M-down] '(lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") '(lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-n") '(lambda () (interactive) (next-line 5)))

(global-set-key [S-M-up] '(lambda () (interactive) (previous-line 15)))
(global-set-key [S-M-down] '(lambda () (interactive) (next-line 15)))
(global-set-key [C-S-M-up] '(lambda () (interactive) (progn(previous-line 15) (recenter))))
(global-set-key [C-S-M-down] '(lambda () (interactive) (progn (next-line 15) (recenter))))

(global-set-key (kbd "C-z") 'ahahah)

(global-set-key (kbd "C-x t") 'open-test)
(global-set-key (kbd "s-t") 'open-test)

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-k") 'kill-line)

(global-set-key (kbd "C-x C-x") 'delete-other-windows)

(global-set-key (kbd "C-,") 'rotate-windows)
(global-set-key (kbd "H-l") (kbd "C-l"))

;; Duplicate line
(global-set-key (kbd "s-d") "\C-a\C- \C-n\M-w\C-y")

(global-set-key (kbd "C-S-w") 'mark-word)

;; Amazing mode to mark all instances in a definiton
(global-set-key (kbd "C->") 'mc/mark-all-like-this-in-defun)
(global-set-key (kbd "C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "H-t") 'grunt)
(global-set-key (kbd "s-r") 'grunt-this-test-file)

;; Use C-i as my personal prefix command
(define-prefix-command 'dgc-map)
(global-set-key (kbd "H-x") 'dgc-map)

(global-set-key (kbd "H-a") 'beginning-of-line)
(global-set-key (kbd "H-e") 'end-of-line)

(global-set-key (kbd "C-§") 'dgc-log)
(global-set-key (kbd "C-±") 'dgc-level-log)
(global-set-key (kbd "H-/") 'go-to-grep-and-rgrep)
(global-set-key (kbd "H-\\") 'go-to-agenda)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x C-c") 'goto-line)

(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

(define-key dgc-map (kbd "H-b") 'anything-buffers-list)

(define-key dgc-map (kbd "s") 'domtronn-sign) 
(define-key dgc-map (kbd "H-s") 'domtronn-sign-professional)
(define-key dgc-map (kbd "t") 'domtronn-timestamp)
