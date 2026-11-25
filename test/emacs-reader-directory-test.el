;;; emacs-reader-directory-test.el --- Directory import tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Validate recursive imports, progress preservation and ownership.

;;; Code:
(require 'ert)
(require 'emacs-reader)

(ert-deftest emacs-reader-directory-import-background-and-deduplicate ()
  (let* ((library (make-temp-file "reader-lib-" t))
         (source (make-temp-file "reader-source-" t))
         (emacs-reader-directory library) (emacs-reader-backend 'local)
         (books (expand-file-name "books/nested" library))
         (original (expand-file-name "原本.TXT" books)) result)
    (unwind-protect
        (progn
          (make-directory books t)
          (make-directory (expand-file-name "sub" source))
          (with-temp-file original (insert "第一章 一\n内容一\n第二章 二\n内容二"))
          (let* ((book (emacs-reader-import-file original))
                 (record (emacs-reader-local--shelf-path book)))
            (setf (alist-get 'durChapterIndex book) 1)
            (emacs-reader-local--write record book)
            (should (equal (alist-get 'bookUrl book) "books/nested/原本.TXT"))
            (should (eq :false (alist-get 'readerOwned book)))
            (should-not (emacs-reader-local--shelf-copy record))
            (copy-file original (expand-file-name "sub/duplicate.txt" source))
            (with-temp-file (expand-file-name "sub/new.txt" source) (insert "不同内容"))
            (with-temp-file (expand-file-name "bad.epub" source) (insert "bad"))
            (make-symbolic-link source (expand-file-name "sub/loop" source))
            (emacs-reader-import-directory source (lambda (value) (setq result value)))
            (should-not result)
            (let ((deadline (+ (float-time) 30)))
              (while (and (not result) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should result)
            (should (= 1 (plist-get result :added)))
            (should (= 1 (plist-get result :duplicates)))
            (should (= 1 (length (plist-get result :errors))))
            (should (= 2 (length (emacs-reader-local--records "shelf"))))
            (should (= 1 (alist-get 'durChapterIndex (emacs-reader-local--read record))))
            (should (file-exists-p original))))
      (delete-directory library t)
      (delete-directory source t))))

(ert-deftest emacs-reader-directory-import-reuses-legacy-copy ()
  (let* ((emacs-reader-directory (make-temp-file "reader-legacy-" t))
         (source (make-temp-file "reader-source-" nil ".txt" "独特的内容"))
         (books (expand-file-name "books" emacs-reader-directory))
         (digest (emacs-reader-local--digest source))
         (legacy (expand-file-name (concat digest ".txt") books)))
    (unwind-protect
        (progn
          (make-directory books t)
          (copy-file source legacy)
          (let ((book (emacs-reader-import-file source)))
            (should (equal (alist-get 'bookUrl book) (concat "books/" digest ".txt")))
            (should (eq t (alist-get 'readerOwned book)))
            (should (eq 'delete (car (emacs-reader-local--shelf-copy
                                      (emacs-reader-local--shelf-path book)))))))
      (delete-file source)
      (delete-directory emacs-reader-directory t))))

(ert-deftest emacs-reader-directory-duplicate-skips-inspection ()
  (let* ((emacs-reader-directory (make-temp-file "reader-dedup-" t))
         (source (make-temp-file "reader-source-" nil ".txt" "章节\n内容")))
    (unwind-protect
        (let* ((book (emacs-reader-import-file source))
               (record (emacs-reader-local--shelf-path book)))
          (setf (alist-get 'durChapterIndex book) 7)
          (emacs-reader-local--write record book)
          (cl-letf (((symbol-function 'emacs-reader-files--inspect)
                     (lambda (&rest _) (error "重复书籍不应重新解析"))))
            (should (= 7 (alist-get 'durChapterIndex (emacs-reader-import-file source)))))
          (should (= 1 (length (emacs-reader-local--records "shelf")))))
      (delete-file source)
      (delete-directory emacs-reader-directory t))))

(ert-deftest emacs-reader-directory-import-key ()
  (should (eq (lookup-key emacs-reader-shelf-mode-map (kbd "I"))
              'emacs-reader-import-directory)))

(provide 'emacs-reader-directory-test)
;;; emacs-reader-directory-test.el ends here
