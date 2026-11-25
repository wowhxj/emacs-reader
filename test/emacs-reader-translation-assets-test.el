;;; emacs-reader-translation-assets-test.el --- Bilingual asset tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Test file identity, literal Org content and exclusive publication.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-reader-translation-assets)

(defmacro with-emacs-reader-asset-test-library (&rest body)
  "Run BODY in a disposable reader library."
  (declare (indent 0) (debug t))
  `(let* ((library (make-temp-file "reader-asset-test-" t))
          (emacs-reader-directory library)
          (emacs-reader-translation-directory nil)
          (book '((name . "旅行札记") (author . "") (originName . "OPDS")
                  (readerType . "file") (bookUrl . "/tmp/preview-1.epub")))
          (chapter '((index . 0) (title . "第一章") (native . (:url "OEBPS/c1.xhtml"))))
          (sources '("* Heading\n#+END_EXAMPLE\n: indent\n\n" "\t : second" ""))
          (texts '("** 译文\n#+TITLE: Test\n: 译\n" "第二段" ""))
          (file (emacs-reader-translation-asset-path book chapter 'en-zh sources)))
     (unwind-protect (progn ,@body) (delete-directory library t))))

(ert-deftest emacs-reader-asset-roundtrip-edits ()
  (with-emacs-reader-asset-test-library
   (should-not (file-exists-p (emacs-reader-translation-asset-root)))
   (should-not (emacs-reader-translation-asset-read file sources))
   (should (equal texts
                  (emacs-reader-translation-asset-write file book chapter 'en-zh sources texts)))
   (with-temp-buffer
     (insert-file-contents file)
     (search-forward "第二段")
     (replace-match "校订后的长段落。\n: * 校订\n: #+TITLE: 校订")
     (write-region nil nil file nil 'silent))
   (let ((edited (list (car texts) "校订后的长段落。\n* 校订\n#+TITLE: 校订" "")))
     (should (equal edited (emacs-reader-translation-asset-read file sources)))
     (should (equal edited
                    (emacs-reader-translation-asset-write
                     file book chapter 'en-zh sources texts))))))

(ert-deftest emacs-reader-asset-invalid-retained ()
  (with-emacs-reader-asset-test-library
   (emacs-reader-translation-asset-write file book chapter 'en-zh sources texts)
   (should-error (emacs-reader-translation-asset-read file (reverse sources)))
   (dolist (broken '("changed source" "empty translation" "truncation"))
     (with-temp-buffer
       (insert-file-contents file)
       (pcase broken
         ("changed source" (search-forward "* Heading") (replace-match "wrong"))
         ("empty translation" (search-forward ": 第二段") (replace-match ": "))
         ("truncation" (erase-buffer)))
       (write-region nil nil file nil 'silent)
       (let ((before (buffer-string)))
         (should-error (emacs-reader-translation-asset-read file sources))
         (should-error
          (emacs-reader-translation-asset-write file book chapter 'en-zh sources texts))
         (should (equal before (with-temp-buffer (insert-file-contents file) (buffer-string))))))
     ;; Restore the fixture for the next independent corruption case.
     (delete-file file)
     (emacs-reader-translation-asset-write file book chapter 'en-zh sources texts))))

(ert-deftest emacs-reader-asset-exclusive-publication ()
  (with-emacs-reader-asset-test-library
   (should-error (emacs-reader-translation-asset-write file book chapter 'en-zh sources '("one")))
   (should-not (file-exists-p file))
   (let ((publish (symbol-function 'add-name-to-file)) (winning '("人工一" "人工二" "")))
     (cl-letf (((symbol-function 'add-name-to-file)
                (lambda (&rest args)
                  (cl-letf (((symbol-function 'add-name-to-file) publish))
                    (emacs-reader-translation-asset-write file book chapter 'en-zh sources winning)
                    (apply publish args)))))
       (should (equal winning
                      (emacs-reader-translation-asset-write
                       file book chapter 'en-zh sources texts))))
     (should (equal winning (emacs-reader-translation-asset-read file sources)))
     (should-not (directory-files (file-name-directory file) nil "\\`\\.translation-")))))

(ert-deftest emacs-reader-asset-portable-version-identity ()
  (with-emacs-reader-asset-test-library
   (let ((shelf (copy-tree book)) (old-root (emacs-reader-translation-asset-root))
         (moved (expand-file-name "moved" library)))
     (setf (alist-get 'bookUrl shelf) "books/abc123.epub")
     (should (equal file (emacs-reader-translation-asset-path shelf chapter 'en-zh sources)))
     (emacs-reader-translation-asset-write file book chapter 'en-zh sources texts)
     (let ((emacs-reader-directory moved))
       (make-directory moved)
       (copy-directory old-root (emacs-reader-translation-asset-root) nil t t)
       (should (equal texts (emacs-reader-translation-asset-read
                             (emacs-reader-translation-asset-path shelf chapter 'en-zh sources)
                             sources)))))
   (dolist (alt (list (emacs-reader-translation-asset-path book chapter 'zh-en sources)
                      (emacs-reader-translation-asset-path book chapter 'en-zh (reverse sources))
                      (emacs-reader-translation-asset-path book '((index . 1)) 'en-zh sources)
                      (emacs-reader-translation-asset-path
                       book (cons '(anchor . "other") chapter) 'en-zh sources)))
     (should-not (equal file alt)))
   (let* ((preview (cons '(bookUrl . "/tmp/preview-1.txt") book))
          (shelf (cons '(bookUrl . "books/abc123.txt") book))
          (txt '((index . 0) (native . (:url "txt:1:100")))))
     (should (equal (emacs-reader-translation-asset-path
                     preview (cons '(title . "preview-1") txt) 'en-zh sources)
                    (emacs-reader-translation-asset-path
                     shelf (cons '(title . "abc123") txt) 'en-zh sources))))))

(ert-deftest emacs-reader-asset-path-and-metadata-safety ()
  (with-emacs-reader-asset-test-library
   (should-not (file-exists-p file))
   (let* ((emacs-reader-translation-directory (expand-file-name "custom" library))
          (url "https://u:secret@invalid.example/book?token=secret")
          (book `((name . ,url) (bookUrl . ,url) (origin . ,url)))
          (chapter `((title . "../../unsafe") (url . ,url) (index . 1)))
          (file (emacs-reader-translation-asset-path book chapter 'en-zh sources))
          (dir (emacs-reader-translation-asset-book-directory book)))
     (should (string-prefix-p (file-name-as-directory emacs-reader-translation-directory) file))
     (should-not (string-match-p "secret\\|\\.\\./" file))
     (make-directory emacs-reader-translation-directory)
     (make-symbolic-link library (directory-file-name dir))
     (should-error
      (emacs-reader-translation-asset-write file book chapter 'en-zh sources texts))
     (delete-file (directory-file-name dir))
     (emacs-reader-translation-asset-write file book chapter 'en-zh sources texts)
     (with-temp-buffer
       (insert-file-contents file)
       (should-not (search-forward "secret" nil t)))
     (make-symbolic-link (expand-file-name "absent" library) (concat file ".link"))
     (should-error (emacs-reader-translation-asset-read (concat file ".link") sources)))))

(provide 'emacs-reader-translation-assets-test)
;;; emacs-reader-translation-assets-test.el ends here
