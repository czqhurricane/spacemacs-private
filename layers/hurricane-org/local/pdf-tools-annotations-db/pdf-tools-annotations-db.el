;;; pdf-tools-annotations-db.el --- PDF annotations database API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2023

;; Author: Your Name
;; URL: https://github.com/your-repo
;; Keywords: pdf, annotations
;; Package-Requires: ((emacsql "20240906.1342") (pdf-tools "20240429.407"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This module provides a SQLite database backend for PDF annotations.
;;
;;; Code:
(require 'pdf-tools)
(require 'emacsql)

;;; Options
(defcustom pdf-tools-annotations-db-location (locate-user-emacs-file "pdf-annotations.db")
  "The path to file where the PDF annotations database is stored."
  :type 'string
  :group 'pdf-tools)

(defcustom pdf-tools-annotations-db-update-on-save t
  "If t, update the PDF annotations database upon saving the file.
Disable this if your PDF files are large and updating the database is slow."
  :type 'boolean
  :group 'pdf-tools)

;;; Variables
(defconst pdf-tools-annotations-db-version 1)

(defvar pdf-tools-annotations-db--connection (make-hash-table :test #'equal)
  "Database connection to PDF annotations database.")

;;; Core Functions
(defun pdf-tools-enabled-p ()
  "检查当前缓冲区是否启用了 pdf-tools。
如果当前缓冲区是 PDF 文件并且 pdf-tools 已加载，则返回 t，否则返回 nil。"
  (and (eq major-mode 'pdf-view-mode)
       (fboundp 'pdf-info-open)
       ;; (boundp 'pdf-view-mode)
       (buffer-file-name)))

(defun pdf-tools-annotations-db--get-connection ()
  "Return the database connection, if any."
  (gethash "pdf-annotations-db" pdf-tools-annotations-db--connection))

(defun pdf-tools-annotations-db ()
  "Entrypoint to the PDF annotations sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (pdf-tools-annotations-db--get-connection)
               (emacsql-live-p (pdf-tools-annotations-db--get-connection)))
    (let ((init-db (not (file-exists-p pdf-tools-annotations-db-location))))
      (make-directory (file-name-directory pdf-tools-annotations-db-location) t)
      (let ((conn (emacsql-sqlite-open pdf-tools-annotations-db-location)))
        (puthash "pdf-annotations-db" conn pdf-tools-annotations-db--connection)
        (when init-db
          (pdf-tools-annotations-db--init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (pdf-tools-annotations-db--upgrade-maybe conn version)))
          (cond
           ((> version pdf-tools-annotations-db-version)
            (emacsql-close conn)
            (user-error
             "The PDF annotations database was created with a newer version.  %s"
             "You need to update the package"))
           ((< version pdf-tools-annotations-db-version)
            (emacsql-close conn)
            (error "BUG: The PDF annotations database scheme changed %s"
                   "and there is no upgrade path")))))))
  (pdf-tools-annotations-db--get-connection))

;;; Entrypoint: (pdf-tools-annotations-db-query)
(define-error 'emacsql-constraint "SQL constraint violation")

(defun pdf-tools-annotations-db-query (sql &rest args)
  "Run SQL query on PDF annotations database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (apply #'emacsql (pdf-tools-annotations-db) sql args))

(defun pdf-tools-annotations-db-query! (handler sql &rest args)
  "Run SQL query on PDF annotations database with ARGS.
SQL can be either the emacsql vector representation, or a string.
The query is expected to be able to fail, in this situation, run HANDLER."
  (condition-case err
      (pdf-tools-annotations-db-query sql args)
    (emacsql-constraint
     (funcall handler err))))

;;; Schemata
(defconst pdf-tools-annotations-db--table-schemata
  '((sync_times
     [(file :unique :primary-key)
      (last_sync_time :not-null)])

    (files
     [(file :unique :primary-key)
      (title :not-null)
      ])

    (annotations
     ([(id :not-null :primary-key)
       (file :not-null)
       (page :not-null)
       (edges :not-null)
       (type :not-null)
       (color :not-null)
       contents
       subject
       (created :not-null)
       (modified :not-null)
       outlines]
      (:foreign-key [file] :references files [file] :on-delete :cascade)))))

(defconst pdf-tools-annotations-db--table-indices
  '((annotations-file-page annotations [file page])))

(defun pdf-tools-annotations-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table ,schema) pdf-tools-annotations-db--table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (pcase-dolist (`(,index-name ,table ,columns) pdf-tools-annotations-db--table-indices)
      (emacsql db [:create-index $i1 :on $i2 $S3] index-name table columns))
    (emacsql db (format "PRAGMA user_version = %s" pdf-tools-annotations-db-version))))

(defun pdf-tools-annotations-db--upgrade-maybe (db version)
  "Upgrades the database schema for DB, if VERSION is old."
  (emacsql-with-transaction db
    'ignore
    (if (< version pdf-tools-annotations-db-version)
        (progn
          (message (format "Upgrading the PDF annotations database from version %d to version %d"
                           version pdf-tools-annotations-db-version))
          (pdf-tools-annotations-db-sync t))))
  version)

(defun pdf-tools-annotations-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the PDF annotations database."
  (unless db
    (setq db (pdf-tools-annotations-db--get-connection)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun pdf-tools-annotations-db--close-all ()
  "Closes all database connections made by PDF annotations."
  (when (pdf-tools-annotations-db--get-connection)
    (pdf-tools-annotations-db--close (pdf-tools-annotations-db--get-connection))))

;;; Database API
;;;; Clearing
(defun pdf-tools-annotations-db-clear-all ()
  "Clears all entries in the PDF annotations cache."
  (interactive)
  (when (file-exists-p pdf-tools-annotations-db-location)
    (dolist (table (mapcar #'car pdf-tools-annotations-db--table-schemata))
      (pdf-tools-annotations-db-query `[:delete :from ,table]))))

(defun pdf-tools-annotations-db-clear-file (&optional file)
  "Remove any related annotations to the FILE.
If FILE is nil, clear the current buffer."
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (pdf-tools-annotations-db-query [:delete :from files
                                           :where (= file $s1)]
                                  file))

;;;; File operations
(defun pdf-tools-annotations-db--file-hash (file)
  "Compute the hash of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha1 (current-buffer))))

(defun pdf-tools-annotations-db-insert-file (&optional hash)
  "Update the files table for the current buffer.
If HASH is non-nil, use that as the file's hash without recalculating it."
  (let* ((file (buffer-file-name))
         (title (file-name-nondirectory file))
         ;; (attr (file-attributes file))
         ;; (atime (file-attribute-access-time attr))
         ;; (mtime (file-attribute-modification-time attr))
         ;; (hash (or hash (pdf-tools-annotations-db--file-hash file)))
         )
    (pdf-tools-annotations-db-query
     [:insert :into files
              :values $v1]
     (list (vector file title)))))

;;;; Annotation operations
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

(defun pdf-tools-annotations-db-insert-annotation (annotation)
  "将 ANNOTATION 插入到数据库中。
如果注释的 content 为空，则跳过插入。"
  (when (alist-get 'contents annotation)
    (let* ((file (buffer-file-name))
           (original-id (alist-get 'id annotation))
           ;; 使用文件完整路径和原始ID组合成新的唯一ID
           ;; 确保 original-id 是字符串
           (id (concat file "#" (if (stringp original-id)
                                    original-id
                                  (format "%s" original-id))))
           (page (alist-get 'page annotation))
           (edges (alist-get 'edges annotation))
           (type (alist-get 'type annotation))
           (color (alist-get 'color annotation))
           (contents (alist-get 'contents annotation))
           (subject (alist-get 'subject annotation))
           (created (or (alist-get 'created annotation)
                        (butlast (current-time) 2)))
           (modified (or (alist-get 'modified annotation)
                         (butlast (current-time) 2)))
           (outlines (mapconcat #'identity (pdf-tools-get-hierarchy-of-outline-reversely page (pdf-info-outline)) " < ")))
      (pdf-tools-annotations-db-query
       [:insert :into annotations
                :values $v1]
       (vector id file page edges type color contents subject created modified outlines)))))

(defun pdf-tools-annotations-db-update-annotation (annotation)
  "Update ANNOTATION in the database."
  (let* ((file (buffer-file-name))
         (original-id (alist-get 'id annotation))
         ;; 使用文件完整路径和原始ID组合成新的唯一ID
         ;; 确保 original-id 是字符串
         (id (concat file "#" (if (stringp original-id)
                                  original-id
                                (format "%s" original-id))))
         (modified (alist-get 'modified annotation))
         (contents (alist-get 'contents annotation))
         (subject (alist-get 'subject annotation))
         (color (alist-get 'color annotation))
         (outlines (alist-get 'outlines annotation)))
    (pdf-tools-annotations-db-query
     [:update annotations
              :set [[(= contents $s1)]
                    [(= subject $s2)]
                    [(= color $s3)]
                    [(= modified $s4)]
                    [(= outlines $s5)]]
              :where (= id $s6)]
     contents subject color modified outlines id)))

(defun pdf-tools-get-pdf-full-filepath ()
  (if pdf-annot-list-document-buffer
      (with-current-buffer (get-buffer pdf-annot-list-document-buffer)
        (buffer-file-name))
    (buffer-file-name)))

(defun pdf-tools-annotations-db-delete-annotation (id &optional file-or-buffer)
  "Delete annotation with ID from the database."
  (let* ((file (or file-or-buffer (pdf-tools-get-pdf-full-filepath)))
         ;; 使用文件完整路径和原始ID组合成新的唯一ID
         ;; 确保 original-id 是字符串
         (full-id (concat file "#" (if (stringp id)
                                       id
                                     (format "%s" id)))))
    (pdf-tools-annotations-db-query
     [:delete :from annotations
              :where (= id $s1)]
     full-id)))

(defun pdf-tools-annotations-db-get-annotations (&optional file page)
  "获取注释。
如果提供了 FILE，则获取该文件的注释。
如果同时提供了 PAGE，则获取该文件特定页面的注释。
如果没有提供任何参数，则获取所有注释。"
  (cond
   ((and file page)
    (pdf-tools-annotations-db-query
     [:select * :from annotations
              :where (and (= file $s1) (= page $s2))]
     file page))
   (file
    (pdf-tools-annotations-db-query
     [:select * :from annotations
              :where (= file $s1)]
     file))
   (t
    (pdf-tools-annotations-db-query
     [:select * :from annotations]))))

(defun pdf-tools-annotations-db-get-last-sync-time (file)
  "获取 FILE 的最后同步时间。"
  (let ((result (pdf-tools-annotations-db-query
                 [:select last_sync_time :from sync_times
                          :where (= file $s1)]
                 file)))
    (when result
      (car (car result)))))

(defun pdf-tools-annotations-db-update-sync-time (file time)
  "更新 FILE 的同步时间为 TIME。"
  (pdf-tools-annotations-db-query
   [:insert-or-replace :into sync_times
                       :values $v1]
   (vector file time)))

(defun pdf-tools-annotations-db-sync (&optional force)
  "将当前 PDF 文档中的注释同步到数据库。
如果 FORCE 非空，则先清除数据库。"
  (interactive)
  (when (or pdf-tools-annotations-db-update-on-save
            (pdf-tools-enabled-p))
    (let* ((file (buffer-file-name))
           (last-sync-time (pdf-tools-annotations-db-get-last-sync-time file))
           (current-time (butlast (current-time) 2)))
      (when force
        (pdf-tools-annotations-db-clear-file file))
      ;; (pdf-tools-annotations-db-insert-file)
      (->>
       (get-all-annotations-from-pdf)
       (pdf-tools-filte-empty-annotations)
       (mapcar (lambda (annot) (when (or force (not last-sync-time) (time-less-p last-sync-time (or (alist-get 'modified annot) (butlast (current-time) 2))))
                            (pdf-tools-annotations-db-insert-annotation annot)))))
      ;; 更新同步时间
      (pdf-tools-annotations-db-update-sync-time file current-time)
      (format "PDF annotations sync: %s" (file-name-nondirectory file)))))

;;;; Hooks and auto-updating
(defun pdf-tools-annotations-db-update ()
  "Update PDF annotations database for the current buffer."
  (when (or pdf-tools-annotations-db-update-on-save
            (pdf-tools-enabled-p))
    (pdf-tools-annotations-db-sync)))

(defun pdf-tools-get-available-annotations ()
  (pdf-tools-filte-empty-annotations  (get-all-annotations-from-pdf)))

(provide 'pdf-tools-annotations-db)
;;; pdf-tools-annotations-db.el ends here
