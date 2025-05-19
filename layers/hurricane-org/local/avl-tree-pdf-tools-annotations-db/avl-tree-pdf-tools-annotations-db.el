(defcustom pdf-tools-annotations-db-directory dotspacemacs-directory
  "Directory where will store its database."  )

(defvar pdf-tools-annotations-db nil)

(defvar pdf-tools-annotations-db-entries nil
  "Entries hash table, part of `pdf-tools-annotations-db'.")

(defvar pdf-tools-annotations-db-index nil
  "Collection of all entries sorted by date, part of `pdf-tools-annotations-db'.")

(defvar pdf-tools-annotations-db-version
  ;; If records are avaiable (Emacs 26), use the newer database format
  (if (functionp 'record)
      4
    "0.0.3")
  "The database version this version expects to use.")

(defvar pdf-tools-annotations-ref-archive nil
  "Index of archived/packed content.")

(defvar pdf-tools-annotations-ref-cache nil
  "Temporary storage of the full archive content.")

(cl-defstruct (pdf-tools-annotations-entry (:constructor pdf-tools-annotations-entry--create))
  id page edges type content modified full-filepath outlines)

(cl-defstruct (pdf-tools-annotations-ref (:constructor pdf-tools-annotations-ref--create))
  id)

(cl-defun pdf-tools-annotations-ref-archive-filename (&optional (suffix ""))
  "Return the base filename of the archive files."
  (concat (expand-file-name "data/archive" pdf-tools-annotations-db-directory) suffix))

(defun pdf-tools-annotations-entry--merge (a b)
  "Merge B into A, preserving A's tags. Return true if an actual
update occurred, not counting content."
  (setf (pdf-tools-annotations-entry-content a) (pdf-tools-annotations-entry-content b))
  (not
   (zerop
    (cl-loop for i from 1 below (1- (length a))
             for part-a = (aref a i)
             for part-b = (aref b i)
             count (not (equal part-a part-b))
             do (setf (aref a i) part-b)))))

(defun pdf-tools-annotations-ref--file (ref)
  "Determine the storage filename for REF."
  (let* ((id (pdf-tools-annotations-ref-id ref))
         (root (expand-file-name "data" pdf-tools-annotations-db-directory))
         (subdir (expand-file-name (substring id 0 2) root)))
    (expand-file-name id subdir)))

(defun pdf-tools-annotations-ref-archive-load ()
  "Load the archived ref index."
  (let ((archive-index (pdf-tools-annotations-ref-archive-filename ".index")))
    (if (file-exists-p archive-index)
        (with-temp-buffer
          (insert-file-contents archive-index)
          (setf pdf-tools-annotations-ref-archive (read (current-buffer))))
      (setf pdf-tools-annotations-ref-archive :empty))))

(defun pdf-tools-annotations-ref-archive-ensure ()
  "Ensure that the archive index is loaded."
  (when (null pdf-tools-annotations-ref-archive) (pdf-tools-annotations-ref-archive-load)))

(defun pdf-tools-annotations-ref-archive-load ()
  "Load the archived ref index."
  (let ((archive-index (pdf-tools-annotations-ref-archive-filename ".index")))
    (if (file-exists-p archive-index)
        (with-temp-buffer
          (insert-file-contents archive-index)
          (setf pdf-tools-annotations-ref-archive (read (current-buffer))))
      (setf pdf-tools-annotations-ref-archive :empty))))

(defun pdf-tools-annotations-ref-archive-ensure ()
  "Ensure that the archive index is loaded."
  (when (null pdf-tools-annotations-ref-archive) (pdf-tools-annotations-ref-archive-load)))

(defun pdf-tools-annotations-ref-exists-p (ref)
  "Return true if REF can be dereferenced."
  (pdf-tools-annotations-ref-archive-ensure)
  (or (and (hash-table-p pdf-tools-annotations-ref-archive)
           (not (null (gethash (pdf-tools-annotations-ref-id ref) pdf-tools-annotations-ref-archive))))
      (file-exists-p (pdf-tools-annotations-ref--file ref))))

(defun pdf-tools-annotations-deref (ref)
  "Fetch the content behind the reference, or nil if non-existent."
  (pdf-tools-annotations-ref-archive-ensure)
  (if (not (pdf-tools-annotations-ref-p ref))
      ref
    (let ((index (and (hash-table-p pdf-tools-annotations-ref-archive)
                      (gethash (pdf-tools-annotations-ref-id ref) pdf-tools-annotations-ref-archive)))
          (archive-file (pdf-tools-annotations-ref-archive-filename ".gz"))
          (coding-system-for-read 'utf-8))
      (if (and index (file-exists-p archive-file))
          (progn
            (when (null pdf-tools-annotations-ref-cache)
              (with-temp-buffer
                (insert-file-contents archive-file)
                (setf pdf-tools-annotations-ref-cache (buffer-string)))
              ;; Clear cache on next turn.
              (run-at-time 0 nil (lambda () (setf pdf-tools-annotations-ref-cache nil))))
            (substring pdf-tools-annotations-ref-cache (car index) (cdr index)))
        (let ((file (pdf-tools-annotations-ref--file ref)))
          (when (file-exists-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (buffer-string))))))))

(defun pdf-tools-annotations-ref (content)
  "Create a reference to CONTENT, to be persistently stored."
  (if (pdf-tools-annotations-ref-p content)
      content
    (let* ((id (secure-hash 'sha1 (encode-coding-string content 'utf-8 t)))
           (ref (pdf-tools-annotations-ref--create :id id))
           (file (pdf-tools-annotations-ref--file ref)))
      (prog1 ref
        (unless (pdf-tools-annotations-ref-exists-p ref)
          (mkdir (file-name-directory file) t)
          (let ((coding-system-for-write 'utf-8)
                ;; Content data loss is a tolerable risk.
                ;; Fsync will occur soon on index write anyway.
                (write-region-inhibit-fsync t))
            (with-temp-file file
              (insert content))))))))

(defun pdf-tools-annotations-deref-entry (entry)
  "Move ENTRY's content to filesystem storage. Return the entry."
  (let ((content (pdf-tools-annotations-entry-content entry)))
    (prog1 entry
      (when (stringp content)
        (setf (pdf-tools-annotations-entry-content entry) (pdf-tools-annotations-ref content))))))

(defun pdf-tools-annotations-db--get-entry (id)
  "Get the entry for ID."
  (pdf-tools-annotations-db--ensure)
  (gethash id pdf-tools-annotations-db-entries))

;; 要处理 entry-a 或 entry-b 为 nil 的情况。
;; 当entry-a 或 entry-b任一一个为nil时，不做edge case 处理，pdf-tools-annotations-entry-modified 函数会抛出错误，
;; 导致与avl-tree--cmpfun 相关的 avl-tree-delete avl-tree-enter 调用时出错。
(defun pdf-tools-annotations-db--compare (a b)
  "Return true if entry A is newer than entry B.
Handles cases where either entry is nil."
  (let* ((entry-a (pdf-tools-annotations-db--get-entry a))
         (entry-b (pdf-tools-annotations-db--get-entry b)))
    (cond
     ;; Both entries are nil: compare strings as fallback
     ((and (null entry-a) (null entry-b))
      (string< (prin1-to-string b) (prin1-to-string a)))

     ;; If only one is nil, treat the nil entry as "newer"
     ((null entry-a) nil)  ; A is nil, so B is considered "newer"
     ((null entry-b) t)    ; B is nil, so A is considered "newer"

     ;; Normal case: compare dates and times
     (t
      (let ((date-a (nth 0 (pdf-tools-annotations-entry-modified entry-a)))
            (date-b (nth 0 (pdf-tools-annotations-entry-modified entry-b)))
            (time-a (nth 1 (pdf-tools-annotations-entry-modified entry-a)))
            (time-b (nth 1 (pdf-tools-annotations-entry-modified entry-b))))
        (if (= date-a date-b)
            (if (= time-a time-b)
                (string< (prin1-to-string b) (prin1-to-string a))
              (> time-a time-b))
          (> date-a date-b)))))))

(defun pdf-tools-annotations-db--set-update-time ()
  "Update the database last-update time."
  (setf pdf-tools-annotations-db (plist-put pdf-tools-annotations-db :last-update (float-time))))

(defun pdf-tools-annotations-db--dummy ()
  "Create an empty dummy database for Emacs 25 and earlier."
  (list :version "0.0.3"
        ;; :feeds #s(hash-table size 65
        ;;                      test equal
        ;;                      rehash-size 1.5
        ;;                      rehash-threshold 0.8
        ;;                      data ())
        :entries #s(hash-table size 65
                               test equal
                               rehash-size 1.5
                               rehash-threshold 0.8
                               data ())
        :index [cl-struct-avl-tree- [nil nil nil 0] pdf-tools-annotations-db--compare]))

(defun pdf-tools-annotations-db--upgrade (db)
  "Upgrade the database from a previous format."
  (if (not (vectorp (plist-get db :index)))
      db  ; Database is already in record format
    (let* ((new-db (pdf-tools-annotations-db--empty))
           ;; Dynamically bind for other functions
           (pdf-tools-annotations-db-entries (plist-get new-db :entries))
           (pdf-tools-annotations-db-index (plist-get new-db :index)))
      ;; Fix up entries
      (cl-loop with table = (plist-get new-db :entries)
               with index = (plist-get new-db :index)
               for entry hash-values of (plist-get db :entries)
               for id = (aref entry 1)
               for content = (aref entry 5)
               for fixed = (pdf-tools-annotations-entry--create
                            :id id
                            :page (aref entry 2)
                            :edges (aref entry 3)
                            :type (aref entry 4)
                            :content (aref content 1)
                            :modified (aref entry 6)
                            )
               do (setf (gethash id table) fixed)
               do (avl-tree-enter index id))
      (plist-put new-db :last-update (plist-get db :last-update)))))

(defun pdf-tools-annotations-db--empty ()
  "Create an empty database object."
  `(:version ,pdf-tools-annotations-db-version
             ;; :feeds ,(make-hash-table :test 'equal)
             :entries ,(make-hash-table :test 'equal)
             ;; Compiler may warn about this (bug#15327):
             :index ,(avl-tree-create #'pdf-tools-annotations-db--compare)))

(defun pdf-tools-annotations-db--load ()
  "Load the database index from the filesystem."
  (let ((index (expand-file-name "index" pdf-tools-annotations-db-directory))
        (enable-local-variables nil)) ; don't set local variables from index!
    (if (not (file-exists-p index))
        (setf pdf-tools-annotations-db (pdf-tools-annotations-db--empty))
      ;; Override the default value for major-mode. There is no
      ;; preventing find-file-noselect from starting the default major
      ;; mode while also having it handle buffer conversion. Some
      ;; major modes crash Emacs when enabled in large buffers (e.g.
      ;; org-mode). This includes the Elfeed index, so we must not let
      ;; this happen.
      (cl-letf (((default-value 'major-mode) 'fundamental-mode))
        (with-current-buffer (find-file-noselect index :nowarn)
          (goto-char (point-min))
          (if (eql pdf-tools-annotations-db-version 4)
              ;; May need to skip over dummy database
              (let ((db-1 (read (current-buffer)))
                    (db-2 (ignore-errors (read (current-buffer)))))
                (setf pdf-tools-annotations-db (or db-2 db-1)))
            ;; Just load first database
            (setf pdf-tools-annotations-db (read (current-buffer))))
          (kill-buffer))))
    ;; Perform an upgrade if necessary and possible
    (unless (equal (plist-get pdf-tools-annotations-db :version) pdf-tools-annotations-db-version)
      (ignore-errors
        (copy-file index (concat index ".backup")))
      (message "Upgrading pdf-tools annotations index for Emacs 26 ...")
      (setf pdf-tools-annotations-db (pdf-tools-annotations-db--upgrade pdf-tools-annotations-db))
      (message "Pdf-tools annotations index upgrade complete."))
    (setf
     pdf-tools-annotations-db-entries (plist-get pdf-tools-annotations-db :entries)
     pdf-tools-annotations-db-index (plist-get pdf-tools-annotations-db :index)
     ;; Internal function use required for security!
     (avl-tree--cmpfun pdf-tools-annotations-db-index) #'pdf-tools-annotations-db--compare)))

(defun pdf-tools-annotations-db--ensure ()
  "Ensure that the database has been loaded."
  (when (null pdf-tools-annotations-db)
    (pdf-tools-annotations-db--load)))

(defun pdf-tools-get-hierarchy-of-outline-reversely (page outlines)
  (let ((next-outline nil)
        (current-outline nil)
        (outline-captured nil)
        (depth-reference nil)
        (result '()))
    (dolist (item (nreverse outlines))
      (let-alist item
        (when (and (eq .type 'goto-dest) (> .page 0))
          (if (> .page page)
              (setq next-outline .title)
            (progn
              (unless outline-captured
                (setq outline-captured t)
                (setq depth-reference (1- .depth))
                (setq current-outline .title)
                (push current-outline result)
                )
              (if (<= .depth depth-reference)
                  (progn
                    (setq depth-reference (1- .depth))
                    (push .title result)))
              )))))
    (nreverse result)))

(defun pdf-tools-annotations--entry-create (full-filepath entry-data)
  (pdf-tools-annotations-entry--create
   :id  (cdr (assq 'id entry-data))
   :page (cdr (assq 'page entry-data))
   :edges (cdr (assq 'edges entry-data))
   :type (cdr (assq 'type entry-data))
   :content (cdr (assq 'contents entry-data))
   :modified (cdr (assq 'modified entry-data))
   :full-filepath full-filepath
   :outlines (mapconcat #'identity (pdf-tools-get-hierarchy-of-outline-reversely (cdr (assq 'page entry-data)) (pdf-info-outline)) " < ")
   ))

(defun pdf-tools-annotations-db--add (full-filepath entries)
  "Add ENTRIES to the database."
  (pdf-tools-annotations-db--ensure)
  (cl-loop for entry in entries
           for id = (format "%s#%s" full-filepath (pdf-tools-annotations-entry-id entry))
           for original = (gethash id pdf-tools-annotations-db-entries)
           for new-date = (nth 0 (pdf-tools-annotations-entry-modified entry))
           for new-time = (nth 1 (pdf-tools-annotations-entry-modified entry))
           for original-date = (and original (nth 0 (pdf-tools-annotations-entry-modified original)))
           for original-time = (and original (nth 1 (pdf-tools-annotations-entry-modified original)))
           ;; do (pdf-tools-annotations-deref-entry entry)
           do (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add id: %s]" id)
           do (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add entry: %s]" entry)
           do (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add original: %s]" original)
           do (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add new-date: %s]" new-date)
           do (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add original-date: %s]" original-date)
           do (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add new-time: %s]" new-time)
           do (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add original-time: %s]" original-time)
           when original count
           (if (and original-date
                    original-time
                    (= new-date original-date)
                    (= new-time original-time))
               (progn
                 (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add when if: %s]" 1)
                 (condition-case err
                     (pdf-tools-annotations-entry--merge original entry)
                   (error
                    (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add 捕捉到错误: %S]" err))))
             (progn
               (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add when if: %s]" 0)
               (condition-case err
                   (if (avl-tree-member pdf-tools-annotations-db-index id)
                       (avl-tree-delete pdf-tools-annotations-db-index id)
                     (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add id not found in the tree: %s]" id))
                 (error
                  (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add 捕捉到错误: %S]" err))))
             (prog1
                 (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add when if: %s]" "prog1")
               (condition-case err
                   (progn
                     (pdf-tools-annotations-entry--merge original entry)
                     (avl-tree-enter pdf-tools-annotations-db-index id))
                 (error
                  (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add 捕捉到错误: %S]" err)))
               ))
           into change-count

           else count
           (progn
             (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add else: %s]" 1)
             (condition-case err
                 (setf (gethash id pdf-tools-annotations-db-entries) entry)
               (error
                (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add 捕捉到错误: %S]" err))))
           into change-count

           and do
           (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add and do: %s]" 1)
           (condition-case err
               (progn
                 (avl-tree-enter pdf-tools-annotations-db-index id))
             (error
              (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add 捕捉到错误: %S]" err)))

           finally
           (unless (zerop change-count)
             (elfeed-tube-log 'debug "[pdf-tools-annotations-db--add finally: %s]" 1)
             )
           )
  :success)

(defun pdf-tools-annotations-db--save ()
  "Write the database index to the filesystem."
  (pdf-tools-annotations-db--ensure)
  (setf pdf-tools-annotations-db (plist-put pdf-tools-annotations-db :version pdf-tools-annotations-db-version))
  (mkdir pdf-tools-annotations-db-directory t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file (expand-file-name "index" pdf-tools-annotations-db-directory)
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle nil))
        (princ (format ";;; Elfeed Database Index (version %s)\n\n"
                       pdf-tools-annotations-db-version))
        (when (eql pdf-tools-annotations-db-version 4)
          ;; Put empty dummy index in front
          (princ ";; Dummy index for backwards compatablity:\n")
          (prin1 (pdf-tools-annotations-db--dummy))
          (princ "\n\n;; Real index:\n"))
        (prin1 pdf-tools-annotations-db)
        :success))))

(defun pdf-tools-annotations-db--delete (id)
  (pdf-tools-annotations-db--ensure)
  (avl-tree-delete pdf-tools-annotations-db-index id)
  (remhash id pdf-tools-annotations-db-entries))

(defun not-nil (x)
  (not (null x)))

(defun pdf-tools-annotations-db--get-all-entries ()
  (pdf-tools-annotations-db--ensure)
  (let ((entries))
    (avl-tree-mapc
     (lambda (id)
       (push (pdf-tools-annotations-db--get-entry id) entries))
     pdf-tools-annotations-db-index)
    (seq-filter 'not-nil (nreverse entries))))

(defun get-all-annotations-from-pdf (&optional file)
  "Return all annotations saved in FILE, as a concatenation of
their contents."
  (let* ((annots (with-current-buffer (or (and file (find-file))
                                          (current-buffer))
                   (pdf-view-mode)
                   (ignore-errors (pdf-annot-getannots nil nil nil)))))
    (if (listp annots)
        annots
      ;; 如果返回的不是列表，返回一个空列表
      (message "No annotations found or unexpected return value from pdf-annot-getannots.")
      nil)))

(defun pdf-tools-filte-empty-annotations (annotations)
  "Filter annotations to remove those with 'content' as nil or empty string."
  (seq-filter (lambda (annotation)
                (let ((content (cdr (assq 'contents annotation))))
                  (and content (not (string-empty-p content)))))
              annotations))

(defun pdf-tools-get-pdf-full-filepath ()
  (if pdf-annot-list-document-buffer
      (with-current-buffer (get-buffer pdf-annot-list-document-buffer)
        (buffer-file-name))
    (buffer-file-name)))

(defun pdf-tools-get-available-annotations ()
  (pdf-tools-filte-empty-annotations (get-all-annotations-from-pdf)))

(defun pdf-tools-annotations-add (full-filepath)
  (->>
   (get-all-annotations-from-pdf)
   (pdf-tools-filte-empty-annotations)
   (mapcar (lambda (data) (pdf-tools-annotations--entry-create full-filepath data)))
   (funcall (apply-partially #'pdf-tools-annotations-db--add full-filepath)))
  (pdf-tools-annotations-db--save))

(add-hook 'kill-buffer-hook #'(lambda () (when (and (not (eq major-mode 'eaf-mode)) (string-suffix-p "pdf" (buffer-name)) (> (length (pdf-tools-get-available-annotations)) 0)) (pdf-tools-annotations-add (buffer-file-name)))))
(add-hook 'quit-window-hook #'(lambda () (when (and (not (eq major-mode 'eaf-mode)) (string-suffix-p "pdf" (buffer-name)) (> (length (pdf-tools-get-available-annotations)) 0)) (pdf-tools-annotations-add (buffer-file-name)))))

(defun pdf-tools-annotations-delete (origfunc id &optional file-or-buffer)
  (funcall origfunc id file-or-buffer)
  (ignore-errors
    (pdf-tools-annotations-db--delete (format "%s#%s" (pdf-tools-get-pdf-full-filepath) id))
    (pdf-tools-annotations-db--save)))

(advice-add #'pdf-info-delannot :around #'pdf-tools-annotations-delete)

(defun pdf-tools-collect-annotations-for-ivy (annotsinfo &optional arg)
  (mapcar
   (lambda (x)
     (let* ((page (pdf-tools-annotations-entry-page x))
            (edges (pdf-tools-annotations-entry-edges x))
            (content (replace-regexp-in-string "\n" " " (pdf-tools-annotations-entry-content x)))
            (full-filepath (pdf-tools-annotations-entry-full-filepath x))
            (outlines (pdf-tools-annotations-entry-outlines x)))
       (list
        (format "%s%s" (propertize content 'face '((:foreground "SkyBlue2"))) (propertize (concat " < " outlines " < " (file-name-nondirectory full-filepath)) 'face '(shadow italic)))
        'pdf-tools
        page
        edges
        content
        full-filepath
        outlines)))
   annotsinfo))
