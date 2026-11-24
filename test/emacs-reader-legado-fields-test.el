;;; emacs-reader-legado-fields-test.el --- Legado field fixtures -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Exercise field rules used by imported book sources.

;;; Code:

(require 'ert)
(require 'emacs-reader-legado)

(ert-deftest emacs-reader-legado-fields-yueyou ()
  (let* ((doc (emacs-reader-legado--parse
               (concat "<div class='v-list-item' onclick=\"jump('/book/23', '', '')\">"
                       "<b class='v-author'> 作 者 </b></div>")))
         (row (cons 'html (car (emacs-reader-legado--values doc ".v-list-item" t))))
         (rule "@onclick@js:result.match(/\\('(.*?)', '', ''\\)/)[1]"))
    (should (equal (emacs-reader-legado--values row rule) '("/book/23")))
    (should (equal (emacs-reader-legado--values row ".v-author@text##\\s") '("作者")))
    (should-not (emacs-reader-legado--values doc rule))
    (should-error (emacs-reader-legado--values row (concat rule ";f()")))
    (should (equal (emacs-reader-legado--values
                    row "@onclick@js:result.match(/(book)\\/(\\d+)/)[2]") '("23")))
    (dolist (expr '("result.match(/x/i)[0]" "result.match(/x/)[1]" "result.match(/(?=x)/)[0]"
                    "result.match(/x/)[0] || 'url'" "result.match(f())[0]"))
      (should-error (emacs-reader-legado--values row (concat "@onclick@js:" expr))))))

(ert-deftest emacs-reader-legado-fields-index-and-rewrite ()
  (let ((doc (emacs-reader-legado--parse
              (concat "<table><tr><td>表头</td></tr><tr class='odd'><td><a>书一-后缀</a></td></tr>"
                      "<tr class='odd'><td><a>书二_后缀</a></td></tr></table>"))))
    (should (equal (emacs-reader-legado--values doc "tr!0@a.0@text##\\-.*|\\_.*|\\s.*")
                   '("书一" "书二")))
    (should (equal (emacs-reader-legado--values doc ".odd.1@a@text##(书.)_.*##《$1》") '("《书二》")))
    (should-not (emacs-reader-legado--values doc "a.9@text"))
    (should (equal (emacs-reader-legado--values doc "a.-1@text") '("书二_后缀")))))

(ert-deftest emacs-reader-legado-fields-json-url ()
  (let ((rule "/novels/api/book/{{$.book_id}}"))
    (dolist (pair '(("123" . "123") ("\"a/b?c\"" . "a%2Fb%3Fc")))
      (should (equal (emacs-reader-legado--values
                      (emacs-reader-legado--parse (concat "{\"book_id\":" (car pair) "}")) rule)
                     (list (concat "/novels/api/book/" (cdr pair))))))
    (should-not (emacs-reader-legado--values (emacs-reader-legado--parse "{}") rule))
    (should-error (emacs-reader-legado--values (emacs-reader-legado--parse "{}") "/x/{{f()}}"))))

(ert-deftest emacs-reader-legado-fields-hide-executable-rules ()
  (dolist (rule '("<js>private-value\nsecond line</js>"
                  "https://api.test/?token={{private-value}}"))
    (let ((message (error-message-string (should-error (emacs-reader-legado--unsupported rule)))))
      (should (string-search "不支持" message))
      (should-not (string-search "private-value" message))
      (should-not (string-search "\n" message)))))

(ert-deftest emacs-reader-legado-fields-escaped-wide-parenthesis ()
  (should (equal (emacs-reader-legado--values (emacs-reader-legado--parse "<h1>书名（连载）</h1>")
                                              "h1@text##\\（.*|\\(.*|免费阅读") '("书名"))))

(provide 'emacs-reader-legado-fields-test)
;;; emacs-reader-legado-fields-test.el ends here
