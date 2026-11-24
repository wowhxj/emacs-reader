;;; emacs-reader-delete-test.el --- Shelf deletion tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise the shelf command against a temporary library.

;;; Code:
(require 'ert)
(require 'emacs-reader)

(defmacro emacs-reader-delete-test--with-library (&rest body)
  "Run BODY in a fresh local shelf buffer with deletion confirmed."
  `(let ((emacs-reader-directory (make-temp-file "reader-delete-" t))
         (emacs-reader-backend 'local))
     (unwind-protect
         (with-temp-buffer
           (emacs-reader-shelf-mode)
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             ,@body))
       (delete-directory emacs-reader-directory t))))

(defun emacs-reader-delete-test--select (book)
  "Save BOOK in the test library and move point to its shelf entry."
  (emacs-reader-local--write (emacs-reader-local--shelf-path book) book)
  (emacs-reader-shelf-refresh)
  (goto-char (text-property-any (point-min) (point-max) 'emacs-reader-book
                                (cl-find book emacs-reader--shelf-books :test #'equal))))

(ert-deftest emacs-reader-delete-imported-book ()
  (emacs-reader-delete-test--with-library
   (let ((original (expand-file-name "original.txt" emacs-reader-directory)))
     (with-temp-file original (insert "第一章 开篇\n第一行"))
     (let* ((book (emacs-reader-import-file original))
            (copy (expand-file-name (alist-get 'bookUrl book) emacs-reader-directory))
            (record (emacs-reader-local--shelf-path book)))
       (emacs-reader-delete-test--select book)
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (prompt)
                    (should (string-match-p (regexp-quote copy) prompt))
                    nil)))
         (call-interactively #'emacs-reader-shelf-delete))
       (should (file-exists-p copy))
       (should (file-exists-p record))
       (call-interactively #'emacs-reader-shelf-delete)
       (should (file-readable-p original))
       (should-not (file-exists-p copy))
       (should-not (file-exists-p record))
       (should-not emacs-reader--shelf-books)))))

(ert-deftest emacs-reader-delete-shared-copy-and-retry ()
  (emacs-reader-delete-test--with-library
   (let* ((url (concat "books/" (make-string 64 ?a) ".epub"))
          (copy (expand-file-name url emacs-reader-directory))
          (first `((readerType . "file") (origin . "A") (name . "A") (bookUrl . ,url)))
          (last `((readerType . "file") (origin . "B") (name . "B") (bookUrl . ,url)))
          (record (emacs-reader-local--shelf-path last))
          (real-delete (symbol-function 'delete-file)))
     (make-directory (file-name-directory copy))
     (with-temp-file copy (insert "test"))
     (emacs-reader-delete-test--select first)
     (cl-letf (((symbol-function 'yes-or-no-p)
                (lambda (prompt)
                  (should (string-match-p (regexp-quote copy) prompt))
                  (emacs-reader-local--write record last)
                  t)))
       (emacs-reader-shelf-delete))
     (should (file-exists-p copy))
     (emacs-reader-delete-test--select first)
     (cl-letf (((symbol-function 'yes-or-no-p)
                (lambda (prompt) (should (string-match-p "保留.*共享副本" prompt)))))
       (emacs-reader-shelf-delete))
     (should (file-exists-p copy))
     (should (equal (list last) emacs-reader--shelf-books))
     (emacs-reader-delete-test--select last)
     (dolist (fail-at (list copy record))
       (cl-letf (((symbol-function 'delete-file)
                  (lambda (path &rest args)
                    (if (equal path fail-at) (signal 'file-error '("simulated"))
                      (apply real-delete path args)))))
         (should-error (emacs-reader-shelf-delete) :type 'file-error))
       (should (file-exists-p record))
       (should (eq (file-exists-p copy) (equal fail-at copy))))
     (emacs-reader-shelf-delete)
     (should-not (file-exists-p record)))))

(ert-deftest emacs-reader-delete-legado-and-reader3 ()
  (emacs-reader-delete-test--with-library
   (let ((book '((readerType . "legado") (origin . "source") (name . "N")
                 (bookUrl . "https://example.org/book"))))
     (emacs-reader-delete-test--select book)
     (let ((emacs-reader-backend 'reader3))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (ert-fail "prompt"))))
         (should-error (emacs-reader-shelf-delete) :type 'user-error)))
     (should (file-exists-p (emacs-reader-local--shelf-path book)))
     (cl-letf (((symbol-function 'yes-or-no-p)
                (lambda (prompt) (should (equal prompt "移除《N》的书架记录? ")))))
       (emacs-reader-shelf-delete))
     (should-not emacs-reader--shelf-books)
     (should-error (emacs-reader-shelf-delete) :type 'user-error))))

(ert-deftest emacs-reader-delete-refuses-unsafe-paths ()
  (emacs-reader-delete-test--with-library
   (let* ((original (expand-file-name "original.txt" emacs-reader-directory))
          (url (concat "books/" (make-string 64 ?a) ".txt"))
          (copy (expand-file-name url emacs-reader-directory))
          (books (directory-file-name (file-name-directory copy))))
     (with-temp-file original (insert "original"))
     (dolist (bad-url (list original "books/../original.txt" "books/original.txt"))
       (emacs-reader-delete-test--select
        `((readerType . "file") (origin . "file") (name . "N") (bookUrl . ,bad-url)))
       (should-error (emacs-reader-shelf-delete) :type 'user-error))
     (emacs-reader-delete-test--select
      `((readerType . "file") (origin . "file") (name . "N") (bookUrl . ,url)))
     (make-symbolic-link emacs-reader-directory books)
     (should-error (emacs-reader-shelf-delete) :type 'user-error)
     (delete-file books)
     (make-directory books)
     (make-symbolic-link original copy)
     (should-error (emacs-reader-shelf-delete) :type 'user-error)
     (should (file-symlink-p copy))
     (should (file-exists-p original)))))

(provide 'emacs-reader-delete-test)
;;; emacs-reader-delete-test.el ends here
