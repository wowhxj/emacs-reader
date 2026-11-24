;;; emacs-reader-legado-static-html-test.el --- Static HTML search tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Exercise static search with wire responses and the normal callback.

;;; Code:

(require 'ert)
(require 'emacs-reader-legado-static-html)

(defun emacs-reader-legado-static-html-test--source (site)
  "Return a fixture SOURCE for SITE."
  (if (eq site 'yidu)
      (list :bookSourceUrl "https://www.121du.net/"
            :searchUrl emacs-reader-legado-static-html--yidu-url
            :ruleSearch '(:bookList "<js>fixture</js>id.sitebox@dl" :name "h3@a@text"
                                    :bookUrl "h3@a@href"))
    (list :bookSourceUrl "https://www.juzixs.vip/"
          :searchUrl emacs-reader-legado-static-html--wenxue-url
          :ruleSearch '(:bookList "class.bookbox" :name "tag.h4@tag.a@a@text##免费阅读"
                                  :bookUrl
                                  "tag.h4@tag.a@href@js:'https://www.juzixs.vip'+result"))))

(defun emacs-reader-legado-static-html-test--run (site body &optional status)
  "Run search for SITE with BODY and HTTP STATUS and record the callback."
  (let ((emacs-reader-legado-static-html--preamble-sha256 (secure-hash 'sha256 "<js>fixture"))
        requests books failure)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url cb &rest _)
                 (push (list url url-request-method url-request-extra-headers) requests)
                 (with-current-buffer (generate-new-buffer " *reader-http*")
                   (setq-local url-http-response-status (or status 200))
                   (insert "Content-Type: text/html; charset=utf-8\r\n\r\n"
                           (encode-coding-string body 'utf-8))
                   (funcall cb nil)))))
      (emacs-reader-legado-search (emacs-reader-legado-static-html-test--source site) "书 &+#,{}"
                                  (lambda (rows err) (setq books rows failure err))))
    (list books failure requests)))

(ert-deftest emacs-reader-static-html-yidu-list-and-self-referer ()
  (pcase-let* ((html "<div id='sitebox'><dl><h3><a href='/book/1'>书</a></h3></dl></div>")
               (`(,rows ,failure ((,url ,method ,headers)))
                (emacs-reader-legado-static-html-test--run 'yidu html)))
    (should-not failure)
    (should (equal url "https://www.121du.net/search/?wd=%E4%B9%A6%20%26%2B%23%2C%7B%7D"))
    (should (equal method "GET"))
    (should (equal headers (list (cons "Referer" url))))
    (should (equal (plist-get (car rows) :bookUrl) "https://www.121du.net/book/1"))))

(ert-deftest emacs-reader-static-html-wenxue-static-page ()
  (pcase-let* ((html "<div class='bookbox'><h4><a href='/b/1'>书免费阅读</a></h4></div>")
               (`(,rows ,failure ((,url ,method ,_)))
                (emacs-reader-legado-static-html-test--run 'wenxue html)))
    (should-not failure)
    (should (equal method "GET"))
    (should (equal url (concat "https://m.juzixs.vip/search.php?keyword="
                               "%E4%B9%A6%20%26%2B%23%2C%7B%7D&submit=")))
    (should (equal (plist-get (car rows) :name) "书"))
    (should (equal (plist-get (car rows) :bookUrl) "https://www.juzixs.vip/b/1"))))

(ert-deftest emacs-reader-static-html-requires-evidence ()
  (dolist (site '(yidu wenxue))
    (dolist (spec '(("<script>document.cookie='test'</script>" 200 "验证")
                    ("<title>安全验证</title>" 200 "验证")
                    ("<div>请启用 JavaScript</div>" 200 "JavaScript")
                    ("<p>forbidden</p>" 403 "403")))
      (pcase-let ((`(,rows ,err ,requests)
                   (emacs-reader-legado-static-html-test--run site (car spec) (cadr spec))))
        (should-not rows)
        (should (string-search (nth 2 spec) err))
        (should (= 1 (length requests)))))))

(ert-deftest emacs-reader-static-html-unknown-script ()
  (should-error (emacs-reader-legado-static-html--rules
                 'yidu '(:bookList "<js>other()</js>id.sitebox@dl"))))

(provide 'emacs-reader-legado-static-html-test)
;;; emacs-reader-legado-static-html-test.el ends here
