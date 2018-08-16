(require 'f)
(require 's)
(require 'json)
(require 'projectile)
(require 'company)

(defconst company-jsimport--static-modules
  '("child_process" "path" "fs" "crypto" "dns" "events" "http" "https" "os" "util"))

(--map (list it "node") company-jsimport--static-modules)

(defun company-jsimport--create-dependency-candidate (candidate)
  "Create `company' candidate from CANDIDATE."
  (let* ((name (format "%s" (car candidate)))
         (version (format "%s" (cdr candidate)))
         (import-name (s-lower-camel-case (-last-item (s-split "/" name)))))
    (propertize name 'version version 'import import-name 'type 'dependency 'completion name)))

(defun company-jsimport--create-module-candidate (candidate)
  "Create `company' candidate from CANDIDATE."
  (let* ((name (file-name-nondirectory candidate))
         (path (concat (projectile-project-root) candidate))
         (relative (f-relative path (file-name-directory buffer-file-name)))
         (relative-path (if (s-starts-with? "." relative) relative (format "./%s" relative)))
         (completion (if (or
                          (string-suffix-p ".js" relative-path)
                          (string-suffix-p ".jsx" relative-path))
                         (file-name-sans-extension relative-path)
                       relative-path))
         (location (file-name-directory (f-relative path (projectile-project-root)))))
    (propertize name 'path path 'type 'module 'completion completion 'location location)))


(defun company-jsimport--get-dependencies (prefix)
  "Read closest package.json dependencies and filter by PREFIX."
  (unless buffer-file-name (error "Not in a project file"))
  (let* ((package (locate-dominating-file buffer-file-name "package.json"))
         (json (json-read-from-string (f-read-text (format "%s/package.json" package))))
         (node-v (s-trim (shell-command-to-string "node --version")))
         (node-deps (--map (list it node-v) company-jsimport--static-modules)))
    (--filter (s-contains-p prefix it)
              (-map 'company-jsimport--create-dependency-candidate
                    (append (cdr (assoc 'dependencies json))
                            (cdr (assoc 'dev-dependencies json)) node-deps)))))

(defun company-jsimport--get-modules (prefix)
  "Read project files for importable modules."
  (unless buffer-file-name (error "Not in a project file"))
  (let* ((files (projectile-current-project-files)))
    (-map 'company-jsimport--create-module-candidate
          (--filter (and (s-contains-p prefix (file-name-nondirectory it))
                         (-contains? '("jsx" "js" "scss" "json" "css") (file-name-extension it)) )
                    files))))

(defun company-jsimport--meta (candidate)
  (cl-case (get-text-property 0 'type candidate)
    (depdendency
     (format "Import version %s of %s as %s"
             (get-text-property 0 'version candidate)
             candidate
             (get-text-property 0 'import candidate)))))

(defun company-jsimport--annotation (candidate)
  (cl-case (get-text-property 0 'type candidate)
    (dependency (format " (%s)" (get-text-property 0 'version candidate)))
    (module (when-let ((location (get-text-property 0 'location candidate)))
              (format " ./%s" (substring (file-name-directory location) 0 -1))))))

(defun company-jsimport--candidates (prefix)
  "Load the candidates for PREFIX."
  (append (company-jsimport--get-dependencies prefix)
          (company-jsimport--get-modules prefix)))

(defun company-jsimport--post-completion (completion)
  (let ((real-completion (get-text-property 0 'completion completion)))
    (save-excursion
      (delete-char (- (length completion)))
      (insert real-completion)
      (unless (looking-at "['\"]") (insert "'"))
      (backward-char (1+ (length real-completion)))
      (unless (looking-back "['\"]") (insert "'")))))

(defun company-jsimport-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-jsimport-backend))
    (prefix (and (-contains? '(js2-mode rjsx-mode js-mode javascript-mode) major-mode)
                 (or (looking-back "^import .+ from ['\"]?.*" (line-beginning-position))
                     (looking-back "^import ['\"]?.*" (line-beginning-position)))
                 (company-grab-symbol)))
    (candidates (company-jsimport--candidates arg))
    (annotation (company-jsimport--annotation arg))
    (meta (company-jsimport--meta arg))
    (sorted t)
    (post-completion (company-jsimport--post-completion arg))))

(provide 'company-jsimport)
