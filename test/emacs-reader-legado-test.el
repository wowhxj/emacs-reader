;;; emacs-reader-legado-test.el --- Offline Legado tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Exercise the public callbacks with real HTTP response buffers and
;; local fixture bodies.  No network or Reader service is needed.

;;; Code:

(require 'ert)
(require 'emacs-reader-legado)

(ert-deftest emacs-reader-legado-easou-catalog-links ()
  (let* ((rule (concat "@js:'https://book.easou.com/ta/show.m?esid=fixture"
                       "&gid=@get:{gid}&nid={{$.nid}}&st={{$.sort}}"
                       "&gst=0&cu=&cName={{$.chapterName}}"
                       "&autoBuy=1'+',{\"webView\":true}'"))
         (src (list :bookSourceUrl "http://api.ieasou.com/" :ruleToc
                    (list :chapterList "$..chapters[*]" :chapterName "$.chapterName"
                          :chapterUrl rule)))
         (url "https://book.eayue.com/ta/listAjax.m?nid=123&gid=id-01")
         (doc (emacs-reader-legado--parse
               (concat "{\"chapters\":[{\"chapterName\":\"A&B\",\"nid\":1,\"sort\":1,\"charge\":0},"
                       "{\"chapterName\":\"续\",\"nid\":1,\"sort\":2,\"charge\":1}]}")))
         (chapters (emacs-reader-legado--catalog src doc url))
         (link (plist-get (car chapters) :url)))
    (should (= 2 (length chapters)))
    (should (string-search "gid=id-01&nid=1&st=1" link))
    (should (string-search "autoBuy=0&cName=A%26B" link))
    (should-not (string-search "esid" link))
    (should-not (emacs-reader-legado--easou-gid src url (concat rule ";unexpected()")))
    (cl-letf (((symbol-function 'emacs-reader-legado--fetch)
               (lambda (&rest _) (ert-fail "收费章节不应触发请求"))))
      (emacs-reader-legado-content
       src (cadr chapters) (lambda (text err)
                             (should-not text)
                             (should (string-search "免费" err)))))))

(ert-deftest emacs-reader-legado-boolean-chapter-filter ()
  (let ((doc (emacs-reader-legado--parse
              (concat "{\"rows\":[{\"id\":0,\"v\":true},{\"id\":1,\"v\":false},"
                      "{\"id\":2,\"v\":null},{\"id\":3},{\"id\":4,\"v\":\"false\"}]}"))))
    (should (equal '("1") (emacs-reader-legado--values doc "rows[?(@.v == false)].id")))
    (should (equal '("0") (emacs-reader-legado--values doc "rows[?(@.v==true)].id")))))

(defun emacs-reader-legado-test--with-pages (pages body &optional check-request)
  "Serve PAGES in order while running BODY and optional CHECK-REQUEST."
  (let ((pending pages))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url handler args &rest _)
                 (pcase-let ((`(,expected ,response) (pop pending)))
                   (should (equal url expected))
                   (when check-request (funcall check-request))
                   (with-current-buffer (generate-new-buffer " *legado fixture*")
                     (set-buffer-multibyte nil)
                     (setq-local url-http-response-status 200)
                     (insert (encode-coding-string
                              (concat "HTTP/1.1 200 OK\r\n"
                                      "Content-Type: text/plain; charset=UTF-8\r\n\r\n" response)
                              'utf-8))
                     (apply handler nil args))))))
      (funcall body)
      (should-not pending))))

(ert-deftest emacs-reader-legado-html-reading ()
  (let ((source '(:bookSourceUrl "https://fixture.test" :bookSourceName "书源"
                                 :searchUrl "/search?q={{key}}&p={{page}}"
                                 :ruleSearch (:bookList "@css:ul.results > li" :name "a@text"
                                                        :author "class.writer.0@text"
                                                        :bookUrl "a@href")
                                 :ruleBookInfo (:name "meta[property=og:title]@content"
                                                      :tocUrl "#toc@href")
                                 :ruleToc (:chapterList "id.chapters@tag.a" :chapterName "text"
                                                        :chapterUrl "href")
                                 :ruleContent (:content "#body@html")))
        books info chapters text)
    (emacs-reader-legado-test--with-pages
     '(("https://fixture.test/search?q=%E4%B9%A6%20%26&p=2"
        "<ul class='results'><li><a href='/a/book'>书名</a><b class='writer'>作者</b></li></ul>")
       ("https://fixture.test/a/book"
        "<meta property='og:title' content='全名'><a id='toc' href='../toc/index'>目录</a>")
       ("https://fixture.test/toc/index" "<div id='chapters'><a href='1'>第一章</a></div>")
       ("https://fixture.test/toc/1" "<div id='body'><p>第一行<br>第二行</p><p>结尾&amp;。</p></div>"))
     (lambda ()
       (emacs-reader-legado-search source "书 &" (lambda (v e) (should-not e) (setq books v)) 2)
       (should (equal books '((:bookUrl "https://fixture.test/a/book" :name "书名" :author "作者"
                                        :origin "https://fixture.test" :originName "书源"))))
       (emacs-reader-legado-book-info source (car books)
                                      (lambda (v e) (should-not e) (setq info v)))
       (should (equal (plist-get info :name) "全名"))
       (should (equal (plist-get info :tocUrl) "https://fixture.test/toc/index"))
       (emacs-reader-legado-chapters source info (lambda (v e) (should-not e) (setq chapters v)))
       (should (equal chapters '((:title "第一章" :url "https://fixture.test/toc/1"))))
       (emacs-reader-legado-content source (car chapters)
                                    (lambda (v e) (should-not e) (setq text v)))
       (should (equal text "第一行\n第二行\n结尾&。"))))))

(ert-deftest emacs-reader-legado-json-reading ()
  (let ((source '(:bookSourceUrl "https://json.test" :bookSourceName "JSON"
                                 :searchUrl "/search"
                                 :ruleSearch (:bookList "$.books[*]" :bookUrl "$.url" :name "title")
                                 :ruleBookInfo (:init "$.data" :tocUrl "toc")
                                 :ruleToc (:chapterList "@json:$.chapters" :chapterName "$.name"
                                                        :chapterUrl "url")
                                 :ruleContent (:content "$.pages[0].lines[*]")))
        book chapter)
    (emacs-reader-legado-test--with-pages
     '(("https://json.test/search" "{\"books\":[{\"title\":\"书\",\"url\":\"/b/1\"}]}")
       ("https://json.test/b/1" "{\"data\":{\"toc\":\"index\"}}")
       ("https://json.test/b/index" "{\"chapters\":[{\"name\":\"一\",\"url\":\"../c\"}]}")
       ("https://json.test/c" "{\"pages\":[{\"lines\":[\"甲\",\"乙\"]}]}"))
     (lambda ()
       (emacs-reader-legado-search source "" (lambda (v e) (should-not e) (setq book (car v))))
       (emacs-reader-legado-chapters source book
                                     (lambda (v e) (should-not e) (setq chapter (car v))))
       (should (equal chapter '(:title "一" :url "https://json.test/c")))
       (emacs-reader-legado-content source chapter
                                    (lambda (v e) (should-not e) (should (equal v "甲\n乙"))))
       (emacs-reader-legado-search source "" (lambda (v e) (should-not v) (should-not e)) 2)))))

(ert-deftest emacs-reader-legado-same-page-catalog ()
  (emacs-reader-legado-test--with-pages
   '(("https://fixture.test/book" "<a href='one'>一</a>"))
   (lambda ()
     (emacs-reader-legado-chapters
      '(:ruleToc (:chapterList "a" :chapterName "text"
                               :chapterUrl "href"))
      '(:bookUrl "https://fixture.test/book")
      (lambda (v e) (should-not e)
        (should (equal v
                       '((:title "一" :url "https://fixture.test/one")))))))))

(ert-deftest emacs-reader-legado-unsupported-rules ()
  (dolist (case '(("<p>x</p>" "@js:result") ("<p>x</p>" "p:first@text")
                  ("{}" "$..*") ("[]" "$[?(@.a)]")))
    (emacs-reader-legado-test--with-pages
     (list (list "https://fixture.test/ch" (car case)))
     (lambda ()
       (emacs-reader-legado-content
        (list :ruleContent (list :content (cadr case))) '(:url "https://fixture.test/ch")
        (lambda (v e) (should-not v) (should (string-match-p "不支持" e)))))))
  (emacs-reader-legado-test--with-pages
   nil
   (lambda ()
     (emacs-reader-legado-search '(:searchUrl "<js>bad()</js>") ""
                                 (lambda (v e) (should-not v) (should (string-match-p "不支持" e)))))))

(ert-deftest emacs-reader-legado-post-request ()
  (let ((source (list :bookSourceUrl "https://test.invalid"
                      :header "{\"Accept\":\"text/plain\",\"X-Static\":\"one\"}"
                      :searchUrl (concat
                                  "/s, {\"method\":\"POST\",\"body\":\"q={{key}}&p={{page}}\","
                                  "\"headers\":{\"accept\":\"application/json\"}}")
                      :ruleSearch '(:bookList "$.books"))))
    (emacs-reader-legado-test--with-pages
     '(("https://test.invalid/s" "{\"books\":[]}"))
     (lambda () (emacs-reader-legado-search
                 source "书+" (lambda (v e) (should-not v) (should-not e)) 2))
     (lambda ()
       (should (equal url-request-method "POST"))
       (should (equal url-request-data "q=%E4%B9%A6%2B&p=2"))
       (should (equal (cdr (assoc-string "Accept" url-request-extra-headers t)) "application/json"))
       (should (equal (cdr (assoc-string "X-Static" url-request-extra-headers t)) "one"))))))

(ert-deftest emacs-reader-legado-pagination ()
  (let ((source '(:ruleToc (:chapterList "a.ch" :chapterName "text" :chapterUrl "href"
                                         :nextTocUrl ".next@href")
                           :ruleContent (:content "text" :nextContentUrl "next"))))
    (emacs-reader-legado-test--with-pages
     '(("https://test.invalid/i" "<a class=ch href='1'>一</a><a class=next href='ii'>下页</a>")
       ("https://test.invalid/ii" "<a class=ch href='2'>二</a>")
       ("https://test.invalid/1" "{\"text\":\"上\",\"next\":\"1b\"}")
       ("https://test.invalid/1b" "{\"text\":\"下\"}"))
     (lambda ()
       (emacs-reader-legado-chapters
        source '(:bookUrl "https://test.invalid/i")
        (lambda (chapters message)
          (should-not message)
          (should (equal (mapcar (lambda (c) (plist-get c :title)) chapters) '("一" "二")))
          (emacs-reader-legado-content source (car chapters)
                                       (lambda (v e)
                                         (should-not e)
                                         (should (equal v "上\n下"))))))))))

(ert-deftest emacs-reader-legado-pagination-loop ()
  (emacs-reader-legado-test--with-pages
   '(("https://test.invalid/c" "{\"text\":\"上\",\"next\":\"b\"}")
     ("https://test.invalid/b" "{\"text\":\"下\",\"next\":\"c\"}"))
   (lambda ()
     (emacs-reader-legado-content
      '(:ruleContent (:content "text" :nextContentUrl "next")) '(:url "https://test.invalid/c")
      (lambda (v e) (should-not v) (should (string-match-p "分页链接循环" e)))))))

(ert-deftest emacs-reader-legado-credential-scope ()
  (let ((source '(:bookSourceUrl "https://fixture.test" :header (:Cookie "session=fake")
                                 :ruleContent (:content "text"))))
    (emacs-reader-legado-test--with-pages
     nil (lambda ()
           (dolist (address '("https://other.test/c" "http://fixture.test/c"
                              "https://fixture.test:444/c"))
             (emacs-reader-legado-content
              source (list :url address)
              (lambda (v e) (should-not v) (should (string-match-p "同源" e)))))
           (emacs-reader-legado-search
            '(:bookSourceUrl "https://fixture.test"
                             :searchUrl
                             "https://b.test/s,{\"header\":{\"Authorization\":\"x\"}}")
            "" (lambda (v e) (should-not v) (should (string-match-p "同源" e))))))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url handler &rest _)
                 (should (zerop url-max-redirections))
                 (should (equal (cdr (assoc-string "Cookie" url-request-extra-headers))
                                "session=fake"))
                 (with-current-buffer (generate-new-buffer " *legado redirect*")
                   (setq-local url-http-response-status 302)
                   (funcall handler '(:error (error http-redirect-limit)))))))
      (emacs-reader-legado-content
       source '(:url "https://fixture.test:443/c")
       (lambda (v e) (should-not v) (should (string-match-p "禁止自动重定向" e)))))))

(ert-deftest emacs-reader-legado-response-distinguishes-errors ()
  (dolist (example '((403 nil "denied" "HTTP 403") (nil nil "" "0 字节")
                     (nil (:error (error connection-failed)) "" "connection-failed")
                     (200 nil "HTTP/1.1 200 OK\r\n\r\n{}oops" "JSON")
                     (200 nil "HTTP/1.1 200 OK\r\n\r\n" "空正文")))
    (pcase-let ((`(,code ,status ,body ,message) example))
      (with-temp-buffer
        (setq-local url-http-response-status code)
        (insert body)
        (let ((failure (should-error (emacs-reader-legado--response status))))
          (should (string-search message (error-message-string failure))))))))

(ert-deftest emacs-reader-legado-chapter-reading-path ()
  (let ((source '(:bookSourceUrl "https://books.test/" :searchUrl "/s"
                                 :ruleSearch (:bookList "li" :name "a@text" :bookUrl "a@href")
                                 :ruleBookInfo (:kind "b@text&&\ni@text##类型："
                                                      :intro "@js:unsupported()")
                                 :ruleToc (:chapterList "dd" :chapterName "a@text"
                                                        :chapterUrl "a@href")
                                 :ruleContent (:content "#content@html" :nextContentUrl
                                                        "text.下一页@href" :replaceRegex "##广告")))
        book info chapters lines)
    (emacs-reader-legado-test--with-pages
     '(("https://books.test/s" "<li>书名</li><li><a href='/b'>书</a></li>")
       ("https://books.test/b" "<b>类型：连载</b><i>仙侠</i>")
       ("https://books.test/b" "<dd>卷首</dd><dd><a href='/c'>第一章</a></dd>")
       ("https://books.test/c" "<div id='content'>广告<br>　甲</div><a href='d'>下一页</a>")
       ("https://books.test/d" "<div id='content'>&nbsp;乙\r\n丙</div><a href='e'>下一章</a>"))
     (lambda ()
       (emacs-reader-legado-search source "书" (lambda (v e) (should-not e) (setq book (car v))))
       (emacs-reader-legado-book-info source book (lambda (v e) (should-not e) (setq info v)))
       (should (equal (plist-get info :kind) "连载\n仙侠"))
       (emacs-reader-legado-chapters source info (lambda (v e) (should-not e) (setq chapters v)))
       (should (equal (length chapters) 1))
       (emacs-reader-legado-content source (car chapters)
                                    (lambda (v e) (should-not e) (setq lines v)))
       (should (equal lines "　甲\n乙\n丙"))))))

(ert-deftest emacs-reader-legado-chapter-empty-rows ()
  (emacs-reader-legado-test--with-pages
   '(("https://books.test/s" "<li>书名</li>"))
   (lambda ()
     (emacs-reader-legado-search
      '(:bookSourceUrl "https://books.test/" :searchUrl "/s"
                       :ruleSearch (:bookList "li" :name "a@text" :bookUrl "a@href"))
      "书" (lambda (books err) (should-not books) (should (string-search "bookList" err)))))))

(provide 'emacs-reader-legado-test)
;;; emacs-reader-legado-test.el ends here
