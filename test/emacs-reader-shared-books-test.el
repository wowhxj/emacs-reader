;;; emacs-reader-shared-books-test.el --- Shared books tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-
;;; Commentary:
;; Exercise configured storage, compatibility, and worker configuration.
;;; Code:
(require 'ert)
(require 'emacs-reader-local)

(ert-deftest emacs-reader-shared-books-paths-and-ownership ()
  (let* ((emacs-reader-directory (make-temp-file "reader-library-" t))
         (shared (make-temp-file "reader-shared-" t))
         (emacs-reader-books-directory shared)
         (source (expand-file-name "original.txt" shared)))
    (unwind-protect
        (progn
          (with-temp-file source (insert "共享书籍内容"))
          (let* ((book (emacs-reader-import-file source))
                 (record (emacs-reader-local--shelf-path book)))
            (should (equal (alist-get 'bookUrl book) "shared-books/original.txt"))
            (should (equal (emacs-reader-local--resolve-file (alist-get 'bookUrl book)) source))
            (should-not (emacs-reader-local--shelf-copy record))
            (let ((emacs-reader-local--sync-preview t) chapters failure)
              (emacs-reader-local--fetch-file
               book nil (lambda (value error) (setq chapters value failure error)))
              (should-not failure)
              (should chapters))
            (setf (alist-get 'readerOwned book) t)
            (emacs-reader-local--write record book)
            (should-not (emacs-reader-local--shelf-copy record))
            (should (equal (emacs-reader-local--resolve-file "books/old.txt")
                           (expand-file-name "books/old.txt" emacs-reader-directory)))
            (should-error (emacs-reader-local--resolve-file "shared-books/../escape"))))
      (delete-directory emacs-reader-directory t)
      (delete-directory shared t))))

(ert-deftest emacs-reader-shared-books-default-compatibility ()
  (let* ((emacs-reader-directory (make-temp-file "reader-library-" t))
         (emacs-reader-books-directory nil)
         (source (make-temp-file "reader-input-" nil ".txt" "内容不同")))
    (unwind-protect
        (let* ((book (emacs-reader-import-file source))
               (record (emacs-reader-local--shelf-path book)))
          (should (string-prefix-p "books/" (alist-get 'bookUrl book)))
          (setf (alist-get 'durChapterIndex book) 3)
          (emacs-reader-local--write record book)
          (let ((emacs-reader-books-directory "books"))
            (should (= 3 (alist-get 'durChapterIndex (emacs-reader-import-file source)))))
          (should (eq 'delete (car (emacs-reader-local--shelf-copy record)))))
      (delete-file source)
      (delete-directory emacs-reader-directory t))))

(ert-deftest emacs-reader-shared-books-worker-config ()
  (let* ((emacs-reader-directory (make-temp-file "reader-library-" t))
         (emacs-reader-books-directory "shared")
         (source (make-temp-file "reader-input-" nil ".txt" "异步书籍内容"))
         result failure done)
    (unwind-protect
        (progn
          (emacs-reader-import-file-async source nil nil nil
                                          (lambda (book error)
                                            (setq result book failure error done t)))
          (setq emacs-reader-books-directory "changed")
          (let ((deadline (+ (float-time) 30)))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should done)
          (should-not failure)
          (should (string-prefix-p "shared-books/" (alist-get 'bookUrl result)))
          (should (file-exists-p (expand-file-name
                                  (substring (alist-get 'bookUrl result) 13)
                                  (expand-file-name "shared" emacs-reader-directory)))))
      (delete-file source)
      (delete-directory emacs-reader-directory t))))

(provide 'emacs-reader-shared-books-test)
;;; emacs-reader-shared-books-test.el ends here
