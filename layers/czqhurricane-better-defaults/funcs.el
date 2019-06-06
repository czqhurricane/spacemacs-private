(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

;; {{
;; @see: http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun czqhurricane/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
;; }}

(defun czqhurricane/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun czqhurricane/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun czqhurricane/occur-dwin ()
  "Call 'occur` with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (deactivate-mark)
  (call-interactively 'occur))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(defun dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: "
                                current-prefix-arg files)
      files)))
  (let (list-switch)
    (start-process
     cmd nil shell-file-name
     shell-command-switch
     (format
      "nohup 1>/dev/null 2>/dev/null %s \"%s\""
      (if (and (> (length file-list) 1)
               (setq list-switch
                     (cadr (assoc cmd dired-filelist-cmd))))
          (format "%s %s" cmd list-switch)
        cmd)
      (mapconcat #'expand-file-name file-list "\" \"")))))

(defun dired-open-terminal ()
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let* ((current-dir (dired-current-directory))
         (buffer (if (get-buffer "*zshell*")
                     (switch-to-buffer "*zshell*")
                   (ansi-term "/bin/zsh" "zshell")))
         (proc (get-buffer-process buffer)))
    (term-send-string
     proc
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

(defun dired-copy-file-here (file)
  "This command copies the file oldname to newname.
An error is signaled if oldname is not a regular file.
If newname names a directory, it copies oldname into that directory,
preserving its final name component."
  (interactive "fCopy file: ")
  (copy-file file default-directory))

(defun my-dired-find-file ()
  "Open buffer in another window"
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (if (car (file-attributes filename))
        (dired-find-alternate-file)
      (dired-find-file-other-window))))

(defun czqhurricane/dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

(defun czqhurricane/dired-up-directory ()
  "Goto up directory and resue buffer."
  (interactive)
  (find-alternate-file ".."))

(defun czqhurricane/insert-space-after-point ()
  (interactive)
  (save-excursion (insert " ")))

(defmacro dakra-define-up/downcase-dwim (case)
  (let ((func (intern (concat "dakra-" case "-dwim")))
        (doc (format "Like `%s-dwim' but %s from beginning when no region is active." case case))
        (case-region (intern (concat case "-region")))
        (case-word (intern (concat case "-word"))))
    `(defun ,func (arg)
       ,doc
       (interactive "*p")
       (save-excursion
         (if (use-region-p)
             (,case-region (region-beginning) (region-end))
           (beginning-of-thing 'symbol)
           (,case-word arg))))))

(dakra-define-up/downcase-dwim "upcase")
(dakra-define-up/downcase-dwim "downcase")
(dakra-define-up/downcase-dwim "capitalize")

(defun select-english-input-source ()
  (interactive)
  (let* ((cmd (format "osascript %s" (getenv "SELECTENGLISHINPUTSOURCE"))))
    (eshell-command cmd)))

(defun evil-keyboard-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (and evil-mode (evil-force-normal-state))
  (progn
    (select-english-input-source)
    (keyboard-quit)))

(defun czqhurricane//dired-store-link (orig-fun &rest args)
  (if (or (derived-mode-p 'dired-mode) (derived-mode-p 'org-mode))
    (cond
      ((derived-mode-p 'dired-mode)
        (let ((file (dired-get-filename nil t)))
          (setf file (if file
                         (dired-make-relative (expand-file-name file) (file-name-directory (directory-file-name (file-name-directory default-directory))))
                       default-directory))
            (let ((link (concat "file:" file))
                  (desc  file))
              (if (string-match ".*?\\.\\(?:png\\|jpg\\)\\(.*\\)$" (file-name-nondirectory file))
                  (push (list link nil) org-stored-links)
                (push (list link desc) org-stored-links))
              (message "Stored: %s" (or desc link))
              (car org-stored-links))))
    ((derived-mode-p 'org-mode)
      (when (org-at-target-p)
        (let ((target nil))
        (setf target (string-trim (match-string 0) "<<" ">>"))
        (let ((link target)
              (desc (concat "See " target)))
          (push (list link desc) org-stored-links)
          (message "Stored: %s" (or link desc))
          (car org-stored-links))))))
      (apply orig-fun args)))
(advice-add 'org-store-link :around #'czqhurricane//dired-store-link)

(defadvice find-file (after insert-header-to-org-buffer
                            activate compile)
  "When a new `org' buffer is created, then insert a header to it."
  (when (buffer-file-name)
    (if (equal "org" (file-name-extension buffer-file-name))
      (progn
        (save-excursion
        (goto-char (point-min))
        (when (not (re-search-forward "# -\\*- eval: (setq org-download-image-dir (concat default-directory \\\"/screenshotImg\\\")); -\\*-" nil t))
          (insert "# -*- eval: (setq org-download-image-dir (concat default-directory \"/screenshotImg\")); -*-\n"))
        (save-buffer))))))
