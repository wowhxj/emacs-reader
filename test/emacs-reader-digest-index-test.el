;;; emacs-reader-digest-index-test.el --- Digest index tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-reader-local)

(defmacro reader-digest-test--library (&rest body)
  "Run BODY with a disposable library."
  (declare (indent 0))
  `(let* ((root (make-temp-file "reader-digest-test-" t))
          (emacs-reader-directory (expand-file-name "library" root))
          (emacs-reader-books-directory nil))
     (unwind-protect (progn ,@body) (delete-directory root t))))

(defun reader-digest-test--write (path contents)
  "Write CONTENTS to PATH."
  (with-temp-file path (insert contents)))

(ert-deftest reader-digest-index-reuses-and-invalidates ()
  (reader-digest-test--library
    (let* ((path (expand-file-name "source.txt" root))
           (original (symbol-function 'emacs-reader-local--digest))
           (calls 0))
      (reader-digest-test--write path "abc")
      (cl-letf (((symbol-function 'emacs-reader-local--digest)
                 (lambda (p) (cl-incf calls) (funcall original p))))
        (let ((first (emacs-reader-local--cached-digest path)))
          (should (equal first (emacs-reader-local--cached-digest path)))
          (should (= calls 1))
          (let* ((attrs (file-attributes path))
                 (mtime (file-attribute-modification-time attrs))
                 (ctime (file-attribute-status-change-time attrs)))
            (sleep-for 0.01)
            (reader-digest-test--write path "xyz")
            (set-file-times path mtime)
            (should-not (equal ctime
                               (file-attribute-status-change-time
                                (file-attributes path)))))
          (should-not (equal first (emacs-reader-local--cached-digest path)))
          (should (= calls 2))
          (let ((emacs-reader-directory (expand-file-name "other" root)))
            (emacs-reader-local--cached-digest path)
            (should (= calls 3))))))))

(ert-deftest reader-digest-index-corrupt-and-unwritable ()
  (reader-digest-test--library
    (let ((path (expand-file-name "source.txt" root)))
      (reader-digest-test--write path "abc")
      (let ((digest (emacs-reader-local--cached-digest path))
            (index (emacs-reader-local--record "digest-index" path)))
        (reader-digest-test--write index "broken json")
        (should (equal digest (emacs-reader-local--cached-digest path)))
        (reader-digest-test--write index "123")
        (cl-letf (((symbol-function 'emacs-reader-local--write)
                   (lambda (&rest _) (error "read-only"))))
          (should (equal digest (emacs-reader-local--cached-digest path))))))))

(ert-deftest reader-digest-index-changing-file-is-not-cached ()
  (reader-digest-test--library
    (let* ((path (expand-file-name "source.txt" root))
           (original (symbol-function 'emacs-reader-local--digest)))
      (reader-digest-test--write path "abc")
      (cl-letf (((symbol-function 'emacs-reader-local--digest)
                 (lambda (p) (prog1 (funcall original p)
                               (reader-digest-test--write p "changed")))))
        (emacs-reader-local--cached-digest path))
      (should-not (file-exists-p (emacs-reader-local--record "digest-index" path))))))

(ert-deftest reader-digest-index-symlink-is-not-cached ()
  (reader-digest-test--library
    (let ((path (expand-file-name "source.txt" root))
          (link (expand-file-name "link.txt" root)))
      (reader-digest-test--write path "abc")
      (make-symbolic-link path link)
      (should (equal (emacs-reader-local--digest path)
                     (emacs-reader-local--cached-digest link)))
      (should-not (file-exists-p (emacs-reader-local--record "digest-index" link))))))

(ert-deftest reader-digest-index-shelf-record-reused ()
  (reader-digest-test--library
    (let* ((books (expand-file-name "books" emacs-reader-directory))
           (path (expand-file-name "book.txt" books))
           (book '((readerType . "file") (bookUrl . "books/book.txt")
                   (origin . "file") (durChapterIndex . 9)))
           (original (symbol-function 'emacs-reader-local--digest))
           (calls 0))
      (make-directory books t)
      (reader-digest-test--write path "abc")
      (emacs-reader-local--add book t)
      (cl-letf (((symbol-function 'emacs-reader-local--digest)
                 (lambda (p) (cl-incf calls) (funcall original p))))
        (let ((digest (funcall original path)))
          (should (equal book (emacs-reader-local--find-digest digest)))
          (should (equal book (emacs-reader-local--find-digest digest)))
          (should (= calls 1)))))))

(ert-deftest reader-digest-index-import-reuses-source-and-shelf ()
  (reader-digest-test--library
    (let* ((path (expand-file-name "source.txt" root))
           (original (symbol-function 'emacs-reader-local--digest))
           (calls 0))
      (reader-digest-test--write path "abc")
      (cl-letf (((symbol-function 'emacs-reader-files-supported-p) (lambda (_) t))
                ((symbol-function 'emacs-reader-files--inspect)
                 (lambda (_) (list (list :title "Example") '(chapter))))
                ((symbol-function 'emacs-reader-local--digest)
                 (lambda (p) (cl-incf calls) (funcall original p))))
        (let ((book (emacs-reader-local--prepare-import path nil nil nil emacs-reader-directory)))
          (emacs-reader-local--add book t)
          (should (= calls 2))
          (should (equal book (emacs-reader-local--prepare-import
                               path nil nil nil emacs-reader-directory)))
          (should (= calls 2)))))))

(ert-deftest reader-digest-index-reused-by-new-process ()
  (reader-digest-test--library
    (let* ((path (expand-file-name "source.txt" root))
           (source (locate-library "emacs-reader-local.el"))
           (program (expand-file-name invocation-name invocation-directory)))
      (reader-digest-test--write path "abc")
      (let ((digest (emacs-reader-local--cached-digest path)))
        (should (= 0
                   (call-process
                    program nil nil nil "-Q" "--batch" "-l" source
                    "--eval"
                    (prin1-to-string
                     `(let ((emacs-reader-directory ,emacs-reader-directory))
                        (cl-letf (((symbol-function 'emacs-reader-local--digest)
                                   (lambda (&rest _) (error "cache miss"))))
                          (unless (equal ,digest (emacs-reader-local--cached-digest ,path))
                            (error "wrong digest"))))))))))))

(provide 'emacs-reader-digest-index-test)
;;; emacs-reader-digest-index-test.el ends here
