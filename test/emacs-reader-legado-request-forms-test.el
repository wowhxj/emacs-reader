;;; emacs-reader-legado-request-forms-test.el --- Static request fixtures -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise the complete request sent to the transport without network access.

;;; Code:
(require 'ert)
(require 'emacs-reader-legado)

(defconst emacs-reader-legado-test--keyword-post
  (concat "@js:\noption={\"method\":\"POST\",\"body\":{\"keyword\":key}}\n"
          "\"http://example.org/\"+\"search,\"+JSON.stringify(option)"))

(ert-deftest emacs-reader-legado-request-forms-keyword-post ()
  (let ((query "三体 \"&\\") captured)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url &rest _)
                 (setq captured (list url url-request-method
                                      (cdr (assoc "Content-Type" url-request-extra-headers))
                                      (json-parse-string
                                       (decode-coding-string url-request-data 'utf-8)
                                       :object-type 'plist))))))
      (emacs-reader-legado--fetch nil emacs-reader-legado-test--keyword-post "http://example.org/"
                                  #'ignore (list query 1))
      (should (equal captured (list "http://example.org/search" "POST" "application/json"
                                    (list :keyword query)))))))

(ert-deftest emacs-reader-legado-request-forms-static-header ()
  (let (captured)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url &rest _) (setq captured url-request-extra-headers))))
      (emacs-reader-legado--fetch nil "/search,{\"headers\":\"{os:'pc'}\"}" "https://example.org/"
                                  #'ignore)
      (should (equal captured '(("os" . "pc"))))))
  (dolist (header '("{os:get()}" "{os:'pc',extra:1}" "{os:'pc'};run()" "{OS:'pc'}"))
    (should-error (emacs-reader-legado--headers header))))

(ert-deftest emacs-reader-legado-request-forms-reject-computed-options ()
  (dolist (text (list (concat emacs-reader-legado-test--keyword-post ";run()")
                      (string-replace ":key" ":getKey()" emacs-reader-legado-test--keyword-post)
                      (string-replace "+\"search," "+path+\"search,"
                                      emacs-reader-legado-test--keyword-post)
                      (string-replace "POST" "GET" emacs-reader-legado-test--keyword-post)))
    (should-error (emacs-reader-legado--request-parts text '("三体" 1))))
  (should-error (emacs-reader-legado--request-parts emacs-reader-legado-test--keyword-post)))

(ert-deftest emacs-reader-legado-request-forms-keyword-post-single-page ()
  (let ((source (list :searchUrl emacs-reader-legado-test--keyword-post)) called)
    (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "Unexpected request"))))
      (emacs-reader-legado-search source "三体"
                                  (lambda (books err) (setq called (list books err))) 2)
      (should (equal called '(nil nil))))))

(provide 'emacs-reader-legado-request-forms-test)
;;; emacs-reader-legado-request-forms-test.el ends here
