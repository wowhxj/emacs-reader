;;; emacs-reader-legado-direct-test.el --- Redirected search results -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Exercise exact matches that redirect the first search page to book details.

;;; Code:

(require 'ert)
(require 'emacs-reader-legado)

(ert-deftest emacs-reader-legado-redirected-search ()
  (let ((source '(:bookSourceUrl "https://search.test" :bookSourceName "测试源"
                                 :searchUrl "/search?p={{page}}" :ruleSearch (:bookList ".bookbox")
                                 :ruleBookInfo (:init ".item" :name "tag.img.0@alt"
                                                      :author "p a@text")))
        (detail "https://books.test/3904/"))
    (dolist (case `((,detail 1 "赤心巡天" t)
                    (,detail 1 "其他书" nil)
                    (,detail 2 "赤心巡天" nil)
                    (nil 1 "赤心巡天" nil)))
      (pcase-let ((`(,final ,page ,query ,matches) case)
                  (calls 0))
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (address handler &rest _)
                     (with-current-buffer (generate-new-buffer " *direct book*")
                       (set-buffer-multibyte nil)
                       (setq-local url-http-response-status 200
                                   url-current-object (url-generic-parse-url (or final address)))
                       (insert "HTTP/1.1 200 OK\r\n\r\n"
                               (encode-coding-string
                                "<div class='item'><img alt='赤心巡天'><p><a>情何以甚</a></p></div>"
                                'utf-8))
                       (funcall handler nil)))))
          (emacs-reader-legado-search
           source query (lambda (rows error)
                          (cl-incf calls)
                          (should-not error)
                          (if matches
                              (should (equal rows `((:bookUrl ,detail :name "赤心巡天"
                                                              :author "情何以甚"
                                                              :origin "https://search.test"
                                                              :originName "测试源"))))
                            (should-not rows))) page)
          (should (= calls 1)))))))

(provide 'emacs-reader-legado-direct-test)
;;; emacs-reader-legado-direct-test.el ends here
