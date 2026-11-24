;;; emacs-reader-access.el --- Display book access labels -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Share access evidence between the catalog and both search views.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'emacs-reader-filter)
(declare-function emacs-reader--book-field "emacs-reader" (book key))

(defvar emacs-reader--catalog-access (make-hash-table :test #'equal)
  "Access labels from chapter catalogs opened during this session.")

(defun emacs-reader--result-title (book)
  "Display BOOK's access label alongside its original title."
  (let ((label (or (gethash (cons (alist-get 'origin book) (alist-get 'bookUrl book))
                            emacs-reader--catalog-access)
                   (alist-get 'accessLabel book)))
        (name (emacs-reader--book-field book 'name)))
    (if label (format "[%s] %s" label name) name)))

(defun emacs-reader--remember-access (book chapters)
  "Update search views for BOOK when CHAPTERS carry explicit pricing information."
  (when (equal (alist-get 'readerType book) "legado")
    (let ((paid 0) (free 0) (key (cons (alist-get 'origin book) (alist-get 'bookUrl book))))
      (seq-doseq (ch chapters)
        (pcase (plist-get (alist-get 'native ch) :access)
          ('paid (cl-incf paid)) ('free (cl-incf free))))
      (when (> (+ paid free) 0)
        (let ((label (cond ((and (> paid 0) (> free 0)) "部分收费")
                           ((= paid (length chapters)) "收费")
                           ((> paid 0) "含收费章节")
                           ((= free (length chapters)) "免费"))))
          (unless (equal label (gethash key emacs-reader--catalog-access))
            (if label (puthash key label emacs-reader--catalog-access)
              (remhash key emacs-reader--catalog-access))
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (when (and (derived-mode-p 'emacs-reader-search-mode)
                           emacs-reader-filter-refresh-function)
                  (funcall emacs-reader-filter-refresh-function))))))))))

(provide 'emacs-reader-access)
;;; emacs-reader-access.el ends here
