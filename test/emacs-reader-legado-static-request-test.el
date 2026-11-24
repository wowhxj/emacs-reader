;;; emacs-reader-legado-static-request-test.el --- Static request fixtures -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Exercise static requests through search and the HTTP wire serializer.

;;; Code:

(require 'ert)
(require 'emacs-reader-legado)

(ert-deftest emacs-reader-legado-static-request-json-wire ()
  (dolist (body '("{\"nested\":{\"q\":{{key}},\"p\":{{page}}}}"
                  "'{\n  \"nested\": {\"q\":\"{{key}}\",\"p\":{{page}}}\n}'"))
    (let ((query "书\"+\\\n") wire failure)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url &rest _)
                   (with-temp-buffer
                     (setq-local url-http-data url-request-data
                                 url-http-method url-request-method
                                 url-http-target-url (url-generic-parse-url url)
                                 url-http-extra-headers url-request-extra-headers
                                 url-http-extensions-header nil
                                 url-http-referer nil
                                 url-http-proxy nil)
                     (setq wire (url-http-create-request))))))
        (emacs-reader-legado-search
         (list :searchUrl (concat "/find,{'webView':false,'method':'POST','body':" body "}")
               :header "{\"X-Test\"=\"测试\"}" :bookSourceUrl "https://example.invalid")
         query (lambda (_ message) (setq failure message)) 2))
      (should-not failure)
      (should wire)
      (should (= (string-bytes wire) (length wire)))
      (should (string-search (encode-coding-string "X-Test: 测试\r\n" 'utf-8) wire))
      (should (string-search "Content-Type: application/json\r\n" wire))
      (let* ((data (substring wire (+ 4 (string-search "\r\n\r\n" wire))))
             (object (json-parse-string (decode-coding-string data 'utf-8) :object-type 'plist)))
        (should (equal (plist-get object :nested) (list :q query :p 2)))))))

(ert-deftest emacs-reader-legado-static-request-ua-wrapper ()
  (let ((wrapper (concat "<js>\n(()=>{\nvar ua = \"fixture\";\n"
                         "var hdr = {\"User-Agent\": ua};\n"
                         "return JSON.stringify(hdr);\n})()\n</js>")))
    (should (equal (emacs-reader-legado--headers wrapper) '(("User-Agent" . "fixture"))))
    (dolist (bad (list (string-replace "return" "run(); return" wrapper)
                       (string-replace "stringify(hdr)" "stringify(other)" wrapper)
                       (string-replace "fixture" "{{unknown}}" wrapper)
                       (string-replace "fixture" "@JS:run()" wrapper)
                       (string-replace "fixture" "x\ny" wrapper)))
      (should-error (emacs-reader-legado--headers bad)))))

(ert-deftest emacs-reader-legado-static-request-webview-and-origin ()
  (dolist (case '(("{'webView':true}" nil nil) ("{'webView':'true'}" nil nil)
                  ("{'webView':null}" nil nil) ("{'webView':'false'}" nil t)
                  ("{'webView':false}" "{\"Cookie\"=\"fixture\"}" nil)))
    (let (sent failure)
      (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (setq sent t))))
        (emacs-reader-legado-search
         (list :searchUrl (concat "https://other.invalid/s," (car case))
               :bookSourceUrl "https://example.invalid" :header (cadr case))
         "x" (lambda (_ message) (setq failure message))))
      (should (eq (nth 2 case) sent))
      (should (eq (null sent) (stringp failure))))))

(ert-deftest emacs-reader-legado-static-request-zero-based-page ()
  (let (body)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _) (setq body url-request-data))))
      (emacs-reader-legado-search
       '(:searchUrl "/s,{'method':'POST','body':'q={{key}}&p={{page-1}}'}"
                    :bookSourceUrl "https://example.invalid")
       "书+" #'ignore 2))
    (should (equal body "q=%E4%B9%A6%2B&p=1"))))

(provide 'emacs-reader-legado-static-request-test)
;;; emacs-reader-legado-static-request-test.el ends here
