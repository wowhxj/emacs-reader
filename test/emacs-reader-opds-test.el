;;; emacs-reader-opds-test.el --- Catalog tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Deterministic catalog and acquisition tests.  No network required.

;;; Code:

(require 'ert)
(require 'emacs-reader)
(require 'emacs-reader-opds)

(defun emacs-reader-opds-test--parse (body)
  "Parse fixture BODY relative to a nested catalog."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (emacs-reader-opds--parse "https://a.invalid/catalog/root")))

(ert-deftest emacs-reader-opds-alternate-html-or-atom-detail ()
  (let* ((item (car (emacs-reader-opds-test--parse
                     "<entry><title>Book</title><link href='detail' type='text/html'/></entry>")))
         (url (emacs-reader-opds--item-url item)) body shown web)
    (cl-letf (((symbol-function 'emacs-reader-opds--get)
               (lambda (target parse done)
                 (should (equal target url))
                 (with-temp-buffer (insert body) (goto-char (point-min))
                                   (funcall done (funcall parse url)))))
              ((symbol-function 'emacs-reader-opds--show) (lambda (_ items) (setq shown items)))
              ((symbol-function 'browse-url) (lambda (target) (setq web target))))
      (setq body (concat "<?xml version='1.0'?><feed><entry><title>Book</title>"
                         "<link rel='http://opds-spec.org/acquisition' href='b.epub'"
                         " type='application/epub+zip'/></entry></feed>"))
      (emacs-reader-opds--activate item)
      (should (eq 'download (emacs-reader-opds--item-kind (car shown))))
      (setq body "<!doctype html><html><title>Book</title></html>")
      (emacs-reader-opds--activate item)
      (should (equal web url)))))

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
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "红 A"))
                ((symbol-function 'emacs-reader-opds--get)
                 (lambda (url read-body done &optional _binary)
                   (push url result)
                   (funcall done
                            (with-temp-buffer
                              (insert (if (equal url description)
                                          (concat "<OpenSearchDescription><Url type='text/html'"
                                                  " template='/ignore?q={searchTerms}'/>"
                                                  "<Url type='application/atom+xml'"
                                                  " template='results?q={searchTerms}'/>"
                                                  "</OpenSearchDescription>")
                                        "<feed/>"))
                              (goto-char (point-min)) (funcall read-body url))))))
        (emacs-reader-opds--search description)
        (should (equal result (list "https://a.invalid/catalog/results?q=%E7%BA%A2%20A"
                                    description)))
        (should (equal (caar emacs-reader-opds--history) "红 A"))))))

(ert-deftest emacs-reader-json-catalog-direct-downloads-and-pagination ()
  (let* ((source '((type . "json") (name . "Example")
                   (url . "https://a.invalid/api/search?q={query}&page={page}")
                   (definition . ((rows . "result.books") (label . "name")
                                  (href . "{url}") (total . "count") (pageSize . 3)))))
         (doc '((count . 4) (result . ((books . (((name . "A") (url . "../a.epub"))
                                                 ((name . "B") (url . "https://b.invalid/b.txt"))
                                                 ((name . "C") (url . "file:///tmp/c.txt"))))))))
         requests next)
    (with-temp-buffer
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _ fn &rest args)
                   (when (= delay emacs-reader-catalog--page-delay)
                     (setq next (lambda () (apply fn args))))
                   (timer-create)))
                ((symbol-function 'emacs-reader-opds--start) #'ignore)
                ((symbol-function 'emacs-reader-opds--get)
                 (lambda (url parse done)
                   (push url requests)
                   (cl-letf (((symbol-function 'emacs-reader-opds--json) (lambda () doc)))
                     (funcall done (funcall parse url))))))
        (emacs-reader-json-catalog-search source "红 &")
        (let ((items (cdar emacs-reader-opds--history)))
          (should (equal (mapcar #'emacs-reader-opds--item-kind items)
                         '(download download)))
          (should (equal (mapcar #'emacs-reader-opds--item-url (seq-take items 2))
                         '("https://a.invalid/a.epub" "https://b.invalid/b.txt")))
          (setq doc '((count . 4) (result . ((books . (((name . "D") (url . "/d.txt"))))))))
          (goto-char (button-start (next-button (point-min))))
          (let ((point-before (point)))
            (funcall next)
            (should (= point-before (point))))
          (should (= 1 (length emacs-reader-opds--history)))
          (should (= 3 (length (cdar emacs-reader-opds--history))))
          (should (equal requests '("https://a.invalid/api/search?q=%E7%BA%A2%20%26&page=2"
                                    "https://a.invalid/api/search?q=%E7%BA%A2%20%26&page=1"))))))))

(defconst emacs-reader-opds-test--source
  (expand-file-name "../examples/open-library.json"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest emacs-reader-json-catalog-public-editions-and-files ()
  (let* ((src (emacs-reader-local--read emacs-reader-opds-test--source))
         (rule (alist-get 'definition src))
         (editions (alist-get 'next rule))
         (doc '((files . (((name . "A B.epub")) ((name . "A_djvu.txt")) ((name . "A_lcp.epub"))
                          ((name . "A.pdf")) ((name . "private.txt") (private . "true"))))
                (metadata . ((identifier . "archive-id")))))
         (files (emacs-reader-json--items (alist-get 'next editions) doc "https://a.invalid/"
                                          "Work" "Writer")))
    (let* ((batch (copy-tree (alist-get 'batch rule)))
           (row '((title . "Work") (key . "/works/W") (ebook_access . "public")
                  (ia . ("first" "second" "first" "bad*"))))
           (data `((docs . (,row)))))
      (should (equal (mapcar (lambda (group) (alist-get 'batch group))
                             (emacs-reader-json--batch-rows batch row)) '("first\" OR \"second")))
      (setf (alist-get 'size batch) 1)
      (let* ((small-rule (cons (cons 'batch batch) rule))
             (items (emacs-reader-json--items small-rule data "https://a.invalid/" nil nil)))
        (should (= (length items) 1))
        (with-temp-buffer
          (emacs-reader-opds--activate (car items))
          (let ((actions (cdar emacs-reader-opds--history)))
            (should (equal (mapcar #'emacs-reader-opds--item-kind actions)
                           '(record json-page json-page)))
            (should (equal (emacs-reader-opds--item-url (car actions)) "https://a.invalid/works/W"))
            (should (string-search "first" (emacs-reader-opds--item-url (nth 1 actions))))
            (should (string-search "second" (emacs-reader-opds--item-url (nth 2 actions))))))))
    (should (equal (mapcar #'emacs-reader-opds--item-label files) '("A B.epub" "A_djvu.txt")))
    (should (equal (emacs-reader-opds--item-author (car files)) "Writer"))
    (should (equal (emacs-reader-opds--item-url (car files))
                   "https://archive.org/download/archive-id/A%20B.epub"))))

(ert-deftest emacs-reader-opds-shows-acquisition-status-on-buttons ()
  (let* ((rule (alist-get 'definition (emacs-reader-local--read emacs-reader-opds-test--source)))
         (docs '(((title . "无电子版") (key . "/1") (ebook_access . "no_ebook"))
                 ((title . "可借阅") (key . "/2") (ebook_access . "borrowable") (ia . ("id")))
                 ((title . "公开版") (key . "/3") (ebook_access . "public") (ia . ("id")))))
         (items (emacs-reader-json--items rule `((docs . ,docs)) "https://example.org/" nil nil)))
    (with-temp-buffer
      (emacs-reader-opds--show
       "结果" (append items (list (emacs-reader-opds--item :kind 'download :label "book.epub")
                                  (emacs-reader-opds--item :kind 'feed :label "下一页"))))
      (dolist (label '("【仅书目信息】 无电子版" "【仅书目信息】 可借阅" "【可查版本】 公开版"
                       "【可下载】 book.epub" "下一页"))
        (let ((button (next-button (point))))
          (should (equal label (button-label button)))
          (goto-char (button-end button)))))))

(ert-deftest emacs-reader-opds-select-preview-and-add-through-http ()
  (let* ((start "https://a.invalid/catalog")
         (book "https://a.invalid/book")
         (data (encode-coding-string "第一章\n正文\n" 'utf-8))
         path imported preview response confirm fail-preview)
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
              ((symbol-function 'y-or-n-p) (lambda (_) confirm))
              ((symbol-function 'emacs-reader-preview-file)
               (lambda (file title author origin temporary)
                 (setq path file preview (list title author origin temporary))
                 (when fail-preview (error "Preview failed"))))
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
              (goto-char (button-start (next-button (point-min))))
              (call-interactively (key-binding (kbd "RET")))
              (should (equal preview '("中文" "A" "My catalog" t)))
              (should-not imported)
              (should (file-exists-p path))
              (delete-file path) ; Simulate closing the successful preview.
              (setq path nil)
              (call-interactively (key-binding "a"))
              (should-not path)
              (setq confirm t)
              (call-interactively (key-binding "a"))
              (should (equal imported '("中文" "A" "My catalog")))
              (should-not (file-exists-p path))
              (should-not (buffer-live-p response))
              (setq fail-preview t path nil)
              (call-interactively (key-binding (kbd "RET")))
              (should (string-search "Preview failed" (buffer-string)))
              (should path)
              (should-not (file-exists-p path)))
          (kill-buffer (current-buffer)))))))

(ert-deftest emacs-reader-opds-shelf-search-selects-registered-catalog ()
  (let ((emacs-reader-backend 'local)
        (url "https://catalog.invalid/opds/") result)
    (with-temp-buffer
      (cl-letf (((symbol-function 'emacs-reader-local--records)
                 (lambda (_) `(((type . "opds") (name . "文渊阁") (url . ,url)))))
                ((symbol-function 'completing-read)
                 (lambda (_ choices &rest _args)
                   (or (car (assoc-string (concat "文渊阁 [opds] <" url ">") choices))
                       (ert-fail "Registered OPDS catalog missing from shelf search"))))
                ((symbol-function 'emacs-reader-opds--start)
                 (lambda (name) (setq emacs-reader-opds--name name)))
                ((symbol-function 'emacs-reader-opds--get)
                 (lambda (target parser done)
                   (push target result)
                   (funcall done (with-temp-buffer
                                   (insert (if (equal url target)
                                               "<feed><link rel='search' href='?q={searchTerms}'/>
</feed>" "<feed/>"))
                                   (goto-char (point-min)) (funcall parser target))))))
        (emacs-reader-search "红 A")
        (should (equal result (list (concat url "?q=%E7%BA%A2%20A") url)))
        (should (equal (mapcar #'car emacs-reader-opds--history) '("红 A" "文渊阁")))))))

(ert-deftest emacs-reader-opds-empty-reply-can-retry-search ()
  (with-temp-buffer
    (emacs-reader-opds-mode)
    (setq emacs-reader-opds--name "文渊阁")
    (let (requests)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url callback &rest _)
                   (push url requests)
                   (with-current-buffer (generate-new-buffer " *mock OPDS*")
                     (setq-local url-http-response-status nil)
                     (when (cdr requests)
                       (setq url-http-response-status 200)
                       (insert "HTTP/1.1 200 OK\r\n\r\n<feed/>"))
                     (funcall callback nil)))))
        (emacs-reader-opds--feed "https://example.invalid/opds/" "西游记")
        (should (string-match-p "未收到 HTTP 状态码（收到 0 字节）" (buffer-string)))
        (button-activate (next-button (point-min)))
        (should (equal requests (make-list 2 "https://example.invalid/opds/")))
        (should (equal (caar emacs-reader-opds--history) "西游记"))))))

(provide 'emacs-reader-opds-test)
;;; emacs-reader-opds-test.el ends here
