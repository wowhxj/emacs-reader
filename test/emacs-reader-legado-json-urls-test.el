;;; emacs-reader-legado-json-urls-test.el --- JSON search field fixtures -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Cover declarative URL extraction from imported JSON book sources.

;;; Code:

(require 'ert)
(require 'emacs-reader-legado)

(ert-deftest emacs-reader-legado-json-search-fields ()
  (let* ((doc (emacs-reader-legado--parse "[{\"id\":42,\"name\":\"三体\",\"nid\":\"a/b&书\"}]"))
         (row (cons 'json (car (emacs-reader-legado--values doc "$.[*]" t)))))
    (should (equal (emacs-reader-legado--search-field row '(:name "$.name@put:{gid:id}") :name)
                   "三体"))
    (should (equal (emacs-reader-legado--values row "https://book.example/info/{{$.id}}?lang=cn")
                   '("https://book.example/info/42?lang=cn")))
    (should (equal (emacs-reader-legado--values
                    row "https://book.example/list?gid={{$.id}}&nid={{$.nid}}&p=1")
                   '("https://book.example/list?gid=42&nid=a%2Fb%26%E4%B9%A6&p=1")))
    (should (equal (emacs-reader-legado--values row "$.id@js:'https://book.example/book/'+result")
                   '("https://book.example/book/42")))
    (should-not (emacs-reader-legado--values row "/book/{{$.missing}}"))
    (dolist (rule '("https://{{$.nid}}/book" "//host/book/{{$.nid}}" "/book/{{f()}}"
                    "$.id@js:'https://book.example/book/'+result+f()"))
      (should-error (emacs-reader-legado--values row rule)))
    (should-error (emacs-reader-legado--search-field row '(:name "$.name@put:{gid:f()}") :name))))

(ert-deftest emacs-reader-legado-json-url-requires-scalar ()
  (dolist (json '("null" "[]" "[7]" "{}" "\"..\""))
    (should-not (emacs-reader-legado--values
                 (emacs-reader-legado--parse (concat "{\"id\":" json "}")) "/book/{{$.id}}"))))

(ert-deftest emacs-reader-legado-json-list-response-type ()
  (let ((rule "$.voicelist"))
    (should-not (emacs-reader-legado--values (emacs-reader-legado--parse "{}") rule t))
    (should-not (emacs-reader-legado--values
                 (emacs-reader-legado--parse "{\"voicelist\":[]}") rule t))
    (should (string-match-p "HTML.*JSON"
                            (error-message-string
                             (should-error (emacs-reader-legado--values
                                            (emacs-reader-legado--parse "<p>入口</p>") rule t)))))))

(ert-deftest emacs-reader-legado-json-yueyou-nested-list ()
  (let* ((doc (emacs-reader-legado--parse
               (concat "{\"data\":[{\"list\":[{\"wapBookId\":\"a/b\"}]},"
                       "{\"nested\":{\"list\":[{\"wapBookId\":2}]}}]}")))
         (rows (emacs-reader-legado--values doc "$..list[*]" t))
         (rule "http://sma.yueyouxs.com/b/{$.wapBookId}.html"))
    (should (equal (mapcar (lambda (row) (car (emacs-reader-legado--values (cons 'json row) rule)))
                           rows)
                   '("http://sma.yueyouxs.com/b/a%2Fb.html" "http://sma.yueyouxs.com/b/2.html")))
    (should-not (emacs-reader-legado--values doc "$..unknown[*]" t))
    (should-error (emacs-reader-legado--values
                   doc "https://books.example/detail,{'method':'POST','body':'{{$.id}}'}"))))

(provide 'emacs-reader-legado-json-urls-test)
;;; emacs-reader-legado-json-urls-test.el ends here
