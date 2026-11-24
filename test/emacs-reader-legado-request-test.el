;;; emacs-reader-legado-request-test.el --- Request fixtures -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Test the request emitted by public search and the options data grammar.

;;; Code:

(require 'ert)
(require 'emacs-reader-legado)

(ert-deftest emacs-reader-legado-request-post-charset ()
  (dolist (case `((,(concat "/modules/article/search.php,"
                            "{'charset':'gbk','body':'searchkey={{key}}','method':'POST'}")
                   "/modules/article/search.php" "searchkey=%CA%E9%2B" gbk)
                  ("/search.html,{'method':'POST','body':'s={{key}}'}"
                   "/search.html" "s=%E4%B9%A6%2B" nil)))
    (pcase-let ((`(,target ,path ,body ,coding) case)
                (base "https://fixture.invalid")
                (called nil))
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url handler &rest _)
                   (should (equal url (concat base path)))
                   (should (equal url-request-method "POST"))
                   (should (equal url-request-data body))
                   (with-current-buffer (generate-new-buffer " *request fixture*")
                     (set-buffer-multibyte nil)
                     (setq-local url-http-response-status 200)
                     (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
                             (encode-coding-string "[{\"name\":\"书\",\"url\":\"/b\"}]"
                                                   (or coding 'utf-8)))
                     (funcall handler nil)))))
        (emacs-reader-legado-search
         (list :searchUrl target :bookSourceUrl base
               :ruleSearch '(:bookList "$[*]" :name "name" :bookUrl "url"))
         "书+"
         (lambda (books message)
           (should-not message)
           (should (equal "书" (plist-get (car books) :name)))
           (setq called t)))
        (should called)))))

(ert-deftest emacs-reader-legado-request-quoted-data ()
  (let ((object (emacs-reader-legado--request-object
                 (concat "{'body':{'list':[\"don't\",'a\\'b\"c',false,null],"
                         "'esc':'x\\\\y\\n\\u4e66'},\"header\":{\"Accept\":'application/json'}}"))))
    (should (equal (plist-get (plist-get object :body) :list) ["don't" "a'b\"c" :false :null]))
    (should (equal (plist-get (plist-get object :body) :esc) "x\\y\n书"))
    (should (equal (emacs-reader-legado--headers (plist-get object :header))
                   '(("Accept" . "application/json")))))
  (should (equal (emacs-reader-legado--headers "{'X-Test':\"a'b\\\"c\"}") '(("X-Test" . "a'b\"c"))))
  (dolist (bad '("{'body':run()}" "{'a':1} trailing" "{'x':'bad\\q'}"))
    (should-error (emacs-reader-legado--request-object bad)))
  (let ((err (should-error (emacs-reader-legado--headers "{'Cookie':'secret', bad}"))))
    (should-not (string-search "secret" (error-message-string err)))))

(ert-deftest emacs-reader-legado-request-script-before-options ()
  (dolist (target '("  @js:search(key, {x:1})" "<js>search(key, {x:1})</js>"))
    (let (message)
      (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "unexpected fetch"))))
        (dolist (page '(1 2))
          (setq message nil)
          (emacs-reader-legado-search (list :searchUrl target) "书" (lambda (_ e) (setq message e))
                                      page)
          (should (string-match-p "不支持.*WebView" message)))))))

(provide 'emacs-reader-legado-request-test)
;;; emacs-reader-legado-request-test.el ends here
