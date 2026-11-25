;;; emacs-reader-relocation-test.el --- Relocation tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'emacs-reader-local)
(require 'emacs-reader-files)

(ert-deftest emacs-reader-relocation-hash-and-identity ()
  (let* ((root (make-temp-file "reader-relocation-" t))
         (emacs-reader-directory (expand-file-name "library" root))
         (emacs-reader-books-directory (expand-file-name "new" root))
         (emacs-reader-local--relocations (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (make-directory emacs-reader-books-directory t)
          (let* ((digest (secure-hash 'sha256 "hello"))
                 (url (concat "books/" digest ".txt"))
                 (path (expand-file-name (concat digest ".txt") emacs-reader-books-directory)))
            (with-temp-file path (insert "hello"))
            (let* ((result (emacs-reader-local--relocation-scan
                            (list url) emacs-reader-directory emacs-reader-books-directory))
                   (entry (car (plist-get result :matches))))
              (should (equal (car entry) url))
              (puthash (emacs-reader-local--relocation-key url)
                       (cons (nth 1 entry) (nth 2 entry)) emacs-reader-local--relocations)
              (should (equal path (emacs-reader-local--resolve-file url)))
              (with-temp-file path (insert "wrong"))
              (should-not (equal path (emacs-reader-local--resolve-file url)))
              (should-not (plist-get (emacs-reader-local--relocation-scan
                                      (list url) emacs-reader-directory emacs-reader-books-directory)
                                     :matches))))
          (delete-directory emacs-reader-directory t))
      (delete-directory root t))))

(ert-deftest emacs-reader-relocation-ambiguity ()
  (let* ((root (make-temp-file "reader-relocation-" t))
         (library (expand-file-name "library" root))
         (books (expand-file-name "new" root))
         (digest (secure-hash 'sha256 "hello"))
         (name (concat digest ".txt")))
    (unwind-protect
        (progn
          (dolist (sub '("a" "b"))
            (make-directory (expand-file-name sub books) t)
            (with-temp-file (expand-file-name (concat sub "/" name) books) (insert "hello")))
          (let ((result (emacs-reader-local--relocation-scan
                         (list (concat "books/" name)) library books)))
            (should (= 1 (plist-get result :ambiguous)))
            (should-not (plist-get result :matches))))
      (delete-directory root t))))

(ert-deftest emacs-reader-relocation-import-reuses-record ()
  (let* ((root (make-temp-file "reader-relocation-" t))
         (emacs-reader-directory (expand-file-name "library" root))
         (emacs-reader-books-directory (expand-file-name "new" root))
         (emacs-reader-local--relocations (make-hash-table :test 'equal))
         (content "a single chapter")
         (digest (secure-hash 'sha256 content))
         (url (concat "books/" digest ".txt"))
         (path (expand-file-name (concat digest ".txt") emacs-reader-books-directory))
         (record `((readerType . "file") (bookUrl . ,url) (origin . "file")
                   (name . "Existing") (durChapterIndex . 7))))
    (unwind-protect
        (progn
          (make-directory emacs-reader-books-directory t)
          (make-directory (expand-file-name "shelf" emacs-reader-directory) t)
          (make-directory (expand-file-name "digest-index" emacs-reader-directory) t)
          (with-temp-file path (insert content))
          (emacs-reader-local--write (emacs-reader-local--shelf-path record) record)
          (should (equal (alist-get 'bookUrl (emacs-reader-local--find-digest digest)) url))
          (should (= 7 (alist-get 'durChapterIndex
                                  (emacs-reader-local--read (emacs-reader-local--shelf-path record))))))
      (delete-directory root t))))

(provide 'emacs-reader-relocation-test)
;;; emacs-reader-relocation-test.el ends here
