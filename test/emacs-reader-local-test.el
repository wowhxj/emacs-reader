;;; emacs-reader-local-test.el --- Local reader integration tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise the default shelf and reading entry points with no Reader3 transport.

;;; Code:
(require 'ert)
(require 'emacs-reader)

(ert-deftest emacs-reader-local-import-read-resume ()
  (let* ((emacs-reader-directory (make-temp-file "reader-library-" t))
         (emacs-reader-backend 'local)
         (path (expand-file-name "故事.txt" emacs-reader-directory))
         readers)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'emacs-reader--reader3-request)
                     (lambda (&rest _) (ert-fail "Unexpected NAS request")))
                    ((symbol-function 'emacs-reader--display-buffer) #'switch-to-buffer))
            (with-temp-file path (insert "第一章 开始\n第一段。\n第二章 结尾\n第二段。"))
            (emacs-reader-import-file path)
            (emacs-reader-import-file path)
            (delete-file path)
            (emacs-reader-shelf)
            (should (= 1 (length emacs-reader--shelf-books)))
            (call-interactively #'emacs-reader-open-book)
            (push (current-buffer) readers)
            (should (string-match-p "第一段" (buffer-string)))
            (emacs-reader-next-chapter)
            (should (string-match-p "第二段" (buffer-string)))
            (emacs-reader-shelf)
            (should (= 1 (alist-get 'durChapterIndex (car emacs-reader--shelf-books))))
            (call-interactively #'emacs-reader-open-book)
            (push (current-buffer) readers)
            (should (string-match-p "第二段" (buffer-string)))
            (should (= emacs-reader--current-chapter-index 1))))
      (mapc #'kill-buffer readers)
      (when-let* ((buf (get-buffer "*Reader Shelf*"))) (kill-buffer buf))
      (delete-directory emacs-reader-directory t))))

(ert-deftest emacs-reader-local-source-registry ()
  (let* ((emacs-reader-directory (make-temp-file "reader-sources-" t))
         (file (expand-file-name "import.json" emacs-reader-directory)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "[{\"bookSourceUrl\":\"https://source.example\",\"bookSourceName\":\"测试\","
                    "\"ruleSearch\":{\"bookList\":\"$.books\"}}]"))
          (emacs-reader-import-legado file)
          (emacs-reader-import-legado file)
          (emacs-reader-add-opds "目录" "https://catalog.example/opds")
          (should (= 2 (length (emacs-reader-local--records "sources"))))
          (let ((source (emacs-reader-local--source '((origin . "https://source.example")))))
            (should (equal (plist-get source :ruleSearch) '(:bookList "$.books")))))
      (delete-directory emacs-reader-directory t))))

(ert-deftest emacs-reader-local-legado-search-preview-shelf ()
  (let* ((emacs-reader-directory (make-temp-file "reader-legado-flow-" t))
         (emacs-reader-backend 'local)
         (file (expand-file-name "source.json" emacs-reader-directory))
         (pages '(("https://fixture.test/find?q=book" . "[{\"title\":\"Book\",\"url\":\"/book\"}]")
                  ("https://fixture.test/book" . "[{\"title\":\"First\",\"url\":\"/one\"}]")
                  ("https://fixture.test/one" . "{\"text\":\"Chapter content.\"}")))
         reader)
    (unwind-protect
        (save-window-excursion
          (with-temp-file file
            (insert "{\"bookSourceUrl\":\"https://fixture.test\",\"bookSourceName\":\"Fixture\","
                    "\"searchUrl\":\"/find?q={{key}}\",\"ruleSearch\":{\"bookList\":\"$[*]\","
                    "\"bookUrl\":\"url\",\"name\":\"title\"},\"ruleToc\":{\"chapterList\":\"$[*]\","
                    "\"chapterName\":\"title\",\"chapterUrl\":\"url\"},"
                    "\"ruleContent\":{\"content\":\"text\"}}"))
          (emacs-reader-import-legado file)
          (cl-letf (((symbol-function 'emacs-reader--reader3-request)
                     (lambda (&rest _) (ert-fail "Unexpected NAS request")))
                    ((symbol-function 'emacs-reader--display-buffer) #'switch-to-buffer)
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'completing-read)
                     (lambda (_ candidates &rest _) (caar (last candidates))))
                    ((symbol-function 'url-retrieve)
                     (lambda (url callback args &rest _)
                       (let ((body (cdr (assoc url pages))))
                         (should body)
                         (with-current-buffer (generate-new-buffer " *reader-http*")
                           (set-buffer-multibyte nil)
                           (setq-local url-http-response-status 200)
                           (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n" body)
                           (apply callback nil args))))))
            (emacs-reader-search "book")
            (should (= 1 (length emacs-reader--search-books)))
            (emacs-reader-search-preview)
            (setq reader (current-buffer))
            (should (string-match-p "Chapter content" (buffer-string)))
            (should-not (emacs-reader-local--records "shelf"))
            (emacs-reader-add-to-shelf)
            (emacs-reader-shelf)
            (should (equal "Book" (alist-get 'name (car emacs-reader--shelf-books))))
            (should (= 0 (alist-get 'durChapterIndex (car emacs-reader--shelf-books))))))
      (dolist (buffer (list reader (get-buffer "*Reader Search*") (get-buffer "*Reader Shelf*")))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory emacs-reader-directory t))))

(provide 'emacs-reader-local-test)
;;; emacs-reader-local-test.el ends here
