(require 'dash)

(defvar wlf:previous-layout nil "Layout to restore to when switching to agenda")

;;;###autoload
(defun wlf:startup ()
  (let ((benchmark-buf (get-buffer-create "*Benchmark Init Results Tree*"))
        (agenda-buf (get-buffer-create "*Org Agenda*")))
    (with-current-buffer agenda-buf (ignore-errors (org-agenda nil "n")))
    (with-current-buffer benchmark-buf (ignore-errors (benchmark-init/show-durations-tree)))
    (wlf:show (wlf:no-layout
      '(| (:left-size-ratio 0.46)
          benchmark
          agenda)
      '((:name benchmark :buffer benchmark-buf)
        (:name agenda :buffer agenda-buf))))
    (select-window-1)))

;;;###autoload
(defun wlf:set-restore-layout-binding ()
  (interactive)
  (when wlf:previous-layout
    (set-window-configuration wlf:previous-layout)
    (setq wlf:previous-layout nil)))

(defun wlf:agenda ()
  (let ((calendar-buf (get-buffer-create "*cfw-calendar*"))
        (agenda-buf (get-buffer-create "*Org Agenda*")))
    (unless (or (get-buffer-window agenda-buf)
                (get-buffer-window calendar-buf))
      (setq wlf:previous-layout (current-window-configuration)))
    (when (not (get-buffer-window agenda-buf))
      (with-current-buffer agenda-buf
        (org-agenda nil "n")))
    (when (not (get-buffer-window calendar-buf))
      (with-current-buffer calendar-buf
        (cfw:open-org-calendar)
        (cfw:change-view-two-weeks)))
    (wlf:show (wlf:no-layout
      '(| (:left-size-ratio 0.46)
          agenda
          calendar)
      '((:name calendar :buffer calendar-buf)
        (:name agenda :buffer agenda-buf))))
    (with-current-buffer calendar-buf
      (cfw:change-view-two-weeks)
      (cfw:refresh-calendar-buffer nil)
      (local-set-key (kbd "R") 'wlf:set-restore-layout-binding))
    (with-current-buffer agenda-buf
      (local-set-key (kbd "R") 'wlf:set-restore-layout-binding))
    (select-window-1)))

(defun wlf:triple-split-layout ()
  (wlf:show (wlf:no-layout
    '(| (:left-size-ratio 0.6) file
        (- (:upper-size-ratio 0.4) runner compilation))
    '((:name file :buffer "file buffer")
      (:name runner :buffer "*runner*")
      (:name compilation :buffer "*compilation*")))))

(defun wlf:javascript-repls ()
  (let ((current (current-buffer))
        (repl-ramda (ramda-repl))
        (repl-lodash (lodash-repl))
        (repl-node (node-repl)))
    (wlf:show (wlf:no-layout
               '(| (:left-size-ratio 0.6) js
                   (- (:upper-size-ratio 0.4) ramda
                      (- (:upper-size-ratio 0.5) lodash node)))
               '((:name js :buffer current)
                 (:name ramda :buffer repl-ramda)
                 (:name lodash :buffer repl-lodash)
                 (:name node :buffer repl-node))))
    (select-window (get-buffer-window current))))

(defun wlf:get-docker-file-buffer ()
  "Get the first Dockerfile buffer or current buffer"
  (or (cdr (assoc
            "Dockerfile"
            (--map (cons (file-name-base (buffer-file-name it)) it)
                   (--filter (buffer-file-name it) (buffer-list)))))
      (current-buffer)))

(defun wlf:get-docker-logs-buffer ()
  "Get the first docker logs buffer or current buffer"
  (or (get-buffer
       (car (--filter (string-match "*docker-logs:" it)
                      (-map 'buffer-name (buffer-list)))))
      (get-buffer-create "*docker-result*")))

(defun wlf:docker ()
  (ignore-errors (docker-containers))
  (ignore-errors (docker-containers-logs-follow-all))
  (ignore-errors (docker-images))
  (let ((dockerfile-buf (wlf:get-docker-file-buffer))
        (build-buf (get-buffer-create "*docker-build-output*"))
        (container-buf (get-buffer-create "*docker-containers*"))
        (image-buf (get-buffer-create "*docker-images*"))
        (log-buf (wlf:get-docker-logs-buffer)))
    (wlf:show (wlf:no-layout
      '(- (:upper-size-ratio 0.7)
          (| (:left-size-ratio 0.6)
             file
             (- (:upper-size-ration 0.5)
                log
                build))
          (- (:upper-size-ratio 0.5)
             containers
             images))
      '((:name file :buffer dockerfile-buf)
        (:name log :buffer log-buf)
        (:name build :buffer build-buf)
        (:name containers :buffer container-buf)
        (:name images :buffer image-buf))))
    (select-window-1)))

(defun wlf:devdocs-url (docset)
  (let ((ip (replace-regexp-in-string "\n$" ""
             (shell-command-to-string "docker-machine ip default"))))
    (format "http://%s/docs/%s" ip docset)))

(defun wlf:lodash ()
  (let ((current (current-buffer))
        (repl-lodash (lodash-repl))
        (docs-lodash (with-current-buffer (get-buffer-create "*eww*")
                      (eww (wlf:devdocs-url "lodash"))))
        (term (or (get-buffer "*lodash-term*")
                  (quick-term "lodash-term"))))
    (wlf:show (wlf:no-layout
               '(| (:left-size-ratio 0.56)
                   (- (:upper-size-ratio 0.8) js term)
                   (- (:upper-size-ratio 0.6) repl docs))
               '((:name js :buffer current)
                 (:name repl :buffer repl-lodash)
                 (:name docs :buffer docs-lodash)
                 (:name term :buffer term))))
    (select-window (get-buffer-window docs-lodash))))

(defun wlf:ramda ()
  (let ((current (current-buffer))
        (repl-ramda (ramda-repl))
        (docs-ramda (with-current-buffer (get-buffer-create "*eww*")
                      (eww (wlf:devdocs-url "ramda"))))
        (term (or (get-buffer "*ramda-term*")
                  (quick-term "ramda-term"))))
    (wlf:show (wlf:no-layout
               '(| (:left-size-ratio 0.56)
                   (- (:upper-size-ratio 0.8) js term)
                   (- (:upper-size-ratio 0.6) repl docs))
               '((:name js :buffer current)
                 (:name repl :buffer repl-ramda)
                 (:name docs :buffer docs-ramda)
                 (:name term :buffer term))))
    (select-window (get-buffer-window docs-ramda))))

(defun wlf:ert-layout ()
  (let* ((test-buffer (format "%s-test.el" (file-name-base (buffer-file-name)))))
    (wlf:show (wlf:no-layout
               '(| (:left-size-ratio 0.6)
                   (- (:upper-size-ratio 0.5) file test)
                   (- (:upper-size-ratio 0.7) ert messages))
               '((:name file :buffer "file buffer")
                 (:name test :buffer test-buffer)
                 (:name messages :buffer "*Messages*")
                 (:name ert :buffer "*ert*"))))))

(defun wlf:codepen-layout ()
  (let ((scss-buf (get-buffer-create "codepen.scss"))
        (html-buf (get-buffer-create "codepen.html"))
        (js-buf (get-buffer-create "codepen.js")))
    (wlf:show (wlf:no-layout
               '(| (:left-size-ratio 0.3) html
                   (| (:left-size-ratio 0.5) scss js))
               '((:name html :buffer html-buf)
                 (:name scss :buffer scss-buf)
                 (:name js :buffer js-buf))))
    (with-current-buffer js-buf (js2-mode))
    (with-current-buffer html-buf (web-mode))
    (with-current-buffer scss-buf (scss-mode))))

(defun wlf:system-layout ()
  (ignore-errors (docker-containers))
  (ignore-errors (docker-images))
  (ignore-errors (jenkins))
  (let ((containers-buf (get-buffer-create "*docker-containers*"))
        (images-buf (get-buffer-create "*docker-images*"))
        (jenkins-buf (get-buffer-create "*jenkins-status*")))
    (wlf:show (wlf:no-layout
               '(- (:upper-size-ratio 0.7) jenkins
                   (- (:upper-size-ratio 0.5) containers images))
               '((:name jenkins :buffer jenkins-buf)
                 (:name containers :buffer containers-buf)
                 (:name images :buffer images-buf))))))

(defun wlf:select-window ()
  "Prompt use to select a window"
  (interactive)
  (let* ((windows-n (length (window-list)))
         (window-nums (number-sequence 49 (+ 48 windows-n))))
    (select-window-by-number (- (read-char-choice "Window: " window-nums) 48))))

(defhydra hydra-window-layout (:color pink :hint nil)
  "
 ^Layouts^           ^|^ ^Sizing^  ^|^ Splitting
-------------------^+^---------^+^----------------
 _S_/_s_: System         |  ^ ^ _k_ ^ ^  | _z_: Kill
 _D_/_d_: Docker         |  _h_ ^+^ _l_  | _x_: Remove
 _T_/_t_: Triple Split   |  ^ ^ _j_ ^ ^  | _-_: Horizontal
 _C_/_c_: CodePen        |         | _|_: Vertical
 _E_/_e_: Ert Runner
 _W_/_w_: JavaScript
 _R_/_r_: Ramda
 ___/_-_: Lodash
 _A_/_a_: Agenda

"
  ("s" (wlf:system-layout))
  ("S" (wlf:system-layout) :exit t)

  ("t" (wlf:triple-split-layout))
  ("T" (wlf:triple-split-layout) :exit t)

  ("c" (wlf:codepen-layout))
  ("C" (wlf:codepen-layout) :exit t)

  ("e" (wlf:ert-layout))
  ("E" (wlf:ert-layout) :exit t)

  ("w" (wlf:javascript-repls))
  ("W" (wlf:javascript-repls) :exit t)

  ("r" (wlf:ramda))
  ("R" (wlf:ramda) :exit t)

  ;; ("s" (wlf:startup))
  ("a" (wlf:agenda))
  ("A" (wlf:agenda) :exit t)

  ("-" (wlf:lodash) :exit t)
  ("_" (wlf:lodash) :exit t)

  ("d" (wlf:docker))
  ("D" (wlf:docker) :exit t)

  ("/" (wlf:select-window) "select")
  ("k" (enlarge-window 1))
  ("K" (enlarge-window 10))

  ("j" (shrink-window 1))
  ("J" (shrink-window 10))

  ("l" (enlarge-window-horizontally 1))
  ("L" (enlarge-window-horizontally 10))

  ("h" (shrink-window-horizontally 1))
  ("H" (shrink-window-horizontally 10))

  ("x" (delete-window))
  ("z" (lambda () (kill-buffer) (delete-window)))
  ("|" (split-window-horizontally))
  ("-" (split-window-vertically))
  ("q" nil "quit"))

(provide 'hydra-window-layout)
