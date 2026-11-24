;;; emacs-reader-opds-test.el --- Catalog tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Deterministic catalog and acquisition tests.  No network required.

;;; Code:

(require 'ert)
(require 'emacs-reader-opds)

(defun emacs-reader-opds-test--parse (body)
  "Parse fixture BODY relative to a nested catalog."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (emacs-reader-opds--parse "https://a.invalid/catalog/root")))

(ert-deftest emacs-reader-opds-atom-navigation-and-acquisition ()
  (let ((items (emacs-reader-opds-test--parse
                (concat "<a:feed xmlns:a='http://www.w3.org/2005/Atom' xml:base='../'>"
                        "<a:link rel='search' href='search.xml'/>"
                        "<a:link rel='next' href='?page=2'/>"
                        "<a:entry><a:title>下一页</a:title>"
                        "<a:link type='application/atom+xml' href='nav'/></a:entry>"
                        "<a:entry xml:base='books/'><a:title>红楼梦</a:title>"
                        "<a:author><a:name>曹雪芹</a:name></a:author>"
                        "<a:link rel='http://opds-spec.org/acquisition/open-access'"
                        " href='download?id=5' type='application/epub+zip'/>"
                        "<a:link rel='http://opds-spec.org/acquisition/buy' href='bad.epub'/>"
                        "<a:link rel='http://opds-spec.org/acquisition' href='file:///tmp/b.epub'/>"
                        "</a:entry></a:feed>"))))
    (should (equal (mapcar #'emacs-reader-opds--item-kind items) '(search feed feed download)))
    (should (equal (emacs-reader-opds--item-url (nth 1 items)) "https://a.invalid/?page=2"))
    (should (equal (emacs-reader-opds--item-title (nth 2 items)) "下一页"))
    (should (equal (emacs-reader-opds--item-author (nth 3 items)) "曹雪芹"))
    (should (equal (emacs-reader-opds--item-url (nth 3 items))
                   "https://a.invalid/books/download?id=5"))))

(ert-deftest emacs-reader-opds-json-groups-navigation-and-pagination ()
  (let ((items (emacs-reader-opds-test--parse
                (concat "{\"links\":[{\"rel\":\"next\",\"href\":\"next\"},"
                        "{\"rel\":[\"search\"],\"href\":\"search{?query}\"}],"
                        "\"navigation\":[{\"title\":\"Books\",\"href\":\"books\"}],"
                        "\"groups\":[{\"publications\":[{\"metadata\":{\"title\":\"Book\","
                        "\"author\":[{\"name\":\"One\"},\"Two\"]},\"links\":["
                        "{\"rel\":\"http://opds-spec.org/acquisition\",\"href\":\"a.txt\"},"
                        "{\"rel\":\"http://opds-spec.org/acquisition\","
                        "\"href\":\"a.pdf\"}]}]}]}"))))
    (should (equal (mapcar #'emacs-reader-opds--item-kind items) '(feed search feed download)))
    (should (equal (emacs-reader-opds--item-label (nth 2 items)) "Books"))
    (should (equal (emacs-reader-opds--item-author (nth 3 items)) "One, Two"))))

(ert-deftest emacs-reader-opds-search-templates ()
  (dolist (url '("https://a.invalid/?q={searchTerms}" "https://a.invalid/{?q,count}"))
    (should (equal (emacs-reader-opds--expand-search url "红 /&")
                   "https://a.invalid/?q=%E7%BA%A2%20%2F%26")))
  (should-error (emacs-reader-opds--expand-search "https://a.invalid/{required}" "abc")))

(ert-deftest emacs-reader-opds-opensearch-description ()
  (let ((description "https://a.invalid/catalog/search.xml") result)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "红 A"))
              ((symbol-function 'emacs-reader-opds--get)
               (lambda (url read-body done &optional _binary)
                 (should (equal url description))
                 (with-temp-buffer
                   (insert "<OpenSearchDescription>"
                           "<Url type='text/html' template='/ignore?q={searchTerms}'/>"
                           "<Url type='application/atom+xml' template='results?q={searchTerms}'/>"
                           "</OpenSearchDescription>")
                   (goto-char (point-min))
                   (funcall done (funcall read-body url)))))
              ((symbol-function 'emacs-reader-opds--feed)
               (lambda (url heading) (setq result (list url heading)))))
      (emacs-reader-opds--search description)
      (should (equal result '("https://a.invalid/catalog/results?q=%E7%BA%A2%20A" "红 A"))))))

(ert-deftest emacs-reader-opds-public-archive-files ()
  (let* ((item (emacs-reader-opds--item :url "archive-id" :title "Work"))
         (files (emacs-reader-opds--files
                 item '((files . (((name . "A B.epub")) ((name . "A_djvu.txt"))
                                  ((name . "A_lcp.epub")) ((name . "A.pdf"))
                                  ((name . "private.txt") (private . "true"))))))))
    (should (equal (mapcar #'emacs-reader-opds--item-label files) '("A B.epub" "A_djvu.txt")))
    (should (equal (emacs-reader-opds--item-url (car files))
                   "https://archive.org/download/archive-id/A%20B.epub"))))

(ert-deftest emacs-reader-opds-select-and-import-through-http ()
  (let* ((start "https://a.invalid/catalog")
         (book "https://a.invalid/book")
         (data (encode-coding-string "第一章\n正文\n" 'utf-8))
         path imported response)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _)
                 (setq response (generate-new-buffer " *catalog HTTP*"))
                 (with-current-buffer response
                   (set-buffer-multibyte nil)
                   (setq-local url-http-response-status 200)
                   (insert "HTTP/1.1 200 OK\r\n\r\n")
                   (insert (if (equal url book) data
                             (encode-coding-string
                              (concat "<feed><entry><title>中文</title><author><name>A</name>"
                                      "</author><link rel='http://opds-spec.org/acquisition'"
                                      " href='/book' type='text/plain'/></entry></feed>") 'utf-8)))
                   (funcall callback nil))))
              ((symbol-function 'emacs-reader-import-file)
               (lambda (file title author origin)
                 (setq path file imported (list title author origin))
                 (should (equal (file-name-extension file) "txt"))
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally file)
                   (should (equal (buffer-string) data))))))
      (save-window-excursion
        (emacs-reader-opds-browse "My catalog" start)
        (unwind-protect
            (progn
              (button-activate (next-button (point-min)))
              (should (equal imported '("中文" "A" "My catalog")))
              (should-not (file-exists-p path))
              (should-not (buffer-live-p response)))
          (kill-buffer (current-buffer)))))))

(provide 'emacs-reader-opds-test)
;;; emacs-reader-opds-test.el ends here
