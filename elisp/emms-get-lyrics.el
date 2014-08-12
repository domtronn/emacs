;;; emms-get-lyrics.el --- Get the lyrics of the song emms is currently playing

;; Copyright (C) 2007 Jay Belanger
;; Copyright (C) 2010 Marcos Talau (talau@users.sourceforge.net)

;; emms-get-lyrics.el is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License as 
;; published by the Free Software Foundation; either version 2, or 
;; (at your option) any later version.

;; emms-get-lyrics.el is distributed in the hope that it will be 
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:

;; The function 'emms-get-lyrics-current-song' tries to get the lyrics
;; to the song that emms is currently playing.  It currently requires
;; w3m to get the lyrics.  It copies the lyrics to a file ending in
;; .lyrics; if the variable `emms-get-lyrics-use-files' is nil, it
;; will just display the lyrics in a buffer without saving them to a
;; file.  If the variable `emms-get-lyrics-dir' is non-nil, then the
;; lyrics will be put in this directory with the file
;; ARTIST-TITLE.lyrics; otherwise it will be put in the same directory
;; as the song file, in a file with the same name as the song file
;; except the extension will by ".lyrics".

;;; Code:

(defvar emms-get-lyrics-version "2.0")

;;; User Customization

(defvar emms-get-lyrics-use-files t)
(defvar emms-get-lyrics-dir nil)
(defvar emms-get-lyrics-match-lyric-notfound "This page needs content.")
(defvar emms-get-lyrics-match-ad-top "Ringtone to your Cell phone")
(defvar emms-get-lyrics-match-ad-bottom "Ringtone to your Cell phone")
(defvar emms-get-lyrics-debug nil)

(defun emms-get-lyrics-url (artist title)
  "Return the URL of lyric."
  (let ((url (concat
   "http://www.lyricwiki.org/index.php?title="
   (replace-regexp-in-string 
    " " "_"
    (concat
     artist
     ":"
     title))
   "&printable=yes")))
    (if emms-get-lyrics-debug
	(message "emms-get-lyrics-url %s" url)
	)
    url))

(defun emms-get-lyrics-w3m (url buffer)
  "Get the lyric in the Internet and put it in the BUFFER."
  (if emms-get-lyrics-debug
      (message "calling: w3m -dump %s" url))
  (call-process "w3m" nil buffer nil "-dump" url))

(defun emms-get-lyrics-redirect-check (url buffer)
  "If need to redirect to another site, do this!"
  (goto-char (point-min))
  (cond ((search-forward "#REDIRECT" nil t)
	 (if emms-get-lyrics-debug
	     (message "Redirecting to %s" url))
	 (delete-region (point-min) (point-max))
	 (emms-get-lyrics-w3m url buffer))))

(defun emms-get-lyrics-write-lyric (bname title artist)
  "When Lyric is not found, ask the user if he wants to write it."
  (switch-to-buffer bname)
  (delete-region (point-min) (point-max))
  (insert ";; This lyric is not found at LyricWiki.org\n")
  (insert ";; You can write the lyric or try to find it\n")
  (insert ";; in the Internet.\n")
  (insert ";; Once you writted or finded, please, post it in LyricWiki.org\n\n")
  (insert title " (" artist ")\n\n"))

(defun emms-get-lyrics (artist title fn &optional file)
  "Core function. Get lyrics, put it in the buffer and format it."
  (let ((bname (concat "Lyrics: " title " (" artist ")")))
    (cond ((get-buffer bname)
           (switch-to-buffer bname))
          ((and file (file-exists-p file))
           (find-file file)
           (rename-buffer bname))
          (t
           (let ((buffer (if file
                             (find-file-noselect file)
                           (get-buffer-create bname))))
             (set-buffer buffer)
             (funcall fn (emms-get-lyrics-url artist title) buffer)
             (goto-char (point-min))
             (if (not (search-forward 
                        emms-get-lyrics-match-lyric-notfound nil t))
		 ; where lyrics from?
                 (let ((frominsert
                        (save-excursion
                          (if (re-search-forward "^Retrieved from")
			    (buffer-substring-no-properties
                               (+ (line-beginning-position) 16)
                               (- (line-end-position) 1))
                            "From LyricWiki"))))
		   (emms-get-lyrics-redirect-check frominsert buffer)
		   (goto-char (point-min))
                   (insert "(from " frominsert ")\n\n")
                   (insert title " (" artist ")\n")
		   ; delete ads, and save disk space ;D
		   (let ((trash-point (point)))
		     (search-forward emms-get-lyrics-match-ad-top)
		     (delete-region trash-point (point)))
		   (search-forward emms-get-lyrics-match-ad-bottom)
		   (move-beginning-of-line nil)
		   (kill-line)
		   ; delete bottom
                   (goto-char (point-max))   (move-beginning-of-line nil)
                   (if (or
                        (search-backward "External links" nil t)
                        (search-backward "Retrieved from" nil t))
                       (delete-region (point) (point-max)))
                   (when file 
                     (rename-buffer bname)
                     (save-buffer))
		   (goto-char (point-min))
		   (switch-to-buffer buffer)
		   (goto-char (point-min)))
	       (restore-buffer-modified-p nil)
	       (if (y-or-n-p (concat "Sorry, unable to find lyrics for " artist " (" file "). Do you want help writing this lyrics now? (say yes :-) ) "))
		   (emms-get-lyrics-write-lyric bname title artist)
		 (kill-buffer))
               ))))))
  
(defun emms-get-lyrics-current-song ()
  "From current song extract info and find the Lyric."
  (interactive)
  (let* ((track (emms-playlist-current-selected-track))
         (artist (cdr (assoc 'info-artist track)))
         (title (cdr (assoc 'info-title track))))
    (cond ((not emms-player-playing-p)
	   (message "Nothing playing right now"))
	  ((not (executable-find "w3m"))
	   (message "You need the program w3m"))
	  ((not (executable-find emms-info-mp3info-program-name))
	   (message "You need the program %s (If you install it, reopen your Emacs.)" emms-info-mp3info-program-name))
	  ((not artist)
	   (message "No artist name in current song"))
	  ((not title)
	   (message "No title in current song"))
	  (t (emms-get-lyrics artist title 'emms-get-lyrics-w3m
                         (if emms-get-lyrics-use-files
                             (if emms-get-lyrics-dir
                                 (concat
                                  emms-get-lyrics-dir
                                  "/"
                                  (replace-regexp-in-string
                                   " " "_"
                                   (concat
                                    artist
                                    "-"
                                    title
                                    ".lyrics")))
                               (concat
                                (file-name-sans-extension (cdr (assoc 'name track)))
                                ".lyrics"))))))))

(provide 'emms-get-lyrics)
;;; emms-get-lyrics.el ends here
