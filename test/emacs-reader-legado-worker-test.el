;;; emacs-reader-legado-worker-test.el --- Large reading responses -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Drive real parser subprocesses with offline HTTP fixtures.

;;; Code:

(require 'ert)
(require 'emacs-reader-legado)

(defun emacs-reader-legado-worker-test--read (body start)
  "Serve one response BODY, run START and measure the calling event loop."
  (let ((raw (encode-coding-string body 'utf-8)) (calls 0) (ticks 0)
        (last (float-time)) (max-gap 0.0) (deadline (+ (float-time) 30)) result failure)
    (let ((timer (run-at-time 0.01 0.01 (lambda ()
                                          (let ((now (float-time)))
                                            (setq max-gap (max max-gap (- now last)) last now)
                                            (cl-incf ticks))))))
      (unwind-protect
          (cl-letf (((symbol-function 'url-retrieve)
                     (lambda (_url handler &rest _)
                       (with-current-buffer (generate-new-buffer " *Legado fixture*")
                         (set-buffer-multibyte nil)
                         (setq-local url-http-response-status 200)
                         (insert "HTTP/1.1 200 OK\r\n\r\n" raw)
                         (funcall handler nil)))))
            (funcall start (lambda (value err) (setq result value failure err) (cl-incf calls)))
            (should (zerop calls))
            (while (and (zerop calls) (< (float-time) deadline)) (accept-process-output nil 0.01))
            (should (= calls 1))
            (should (> ticks 0))
            (message "Legado %d bytes: %d heartbeats, max interval %.1f ms"
                     (length raw) ticks (* 1000 (max max-gap (- (float-time) last)))))
        (cancel-timer timer)))
    (cons result failure)))

(ert-deftest emacs-reader-legado-worker-large-same-page-catalog ()
  (let* ((body (concat "<main>" (mapconcat (lambda (i) (format "<a href='/ch/%s'>第%s回</a>" i i))
                                           (number-sequence 1 5000) "") "</main>"))
         (result (emacs-reader-legado-worker-test--read
                  body (lambda (cb) (emacs-reader-legado-chapters
                                     '(:ruleToc (:chapterList "a" :chapterName "text"
                                                              :chapterUrl "href"))
                                     '(:bookUrl "https://fixture.invalid/book") cb))))
         (chapters (car result)))
    (should-not (cdr result))
    (should (= 5000 (length chapters)))
    (should (equal (car chapters) '(:title "第1回" :url "https://fixture.invalid/ch/1")))
    (should (equal (car (last chapters))
                   '(:title "第5000回" :url "https://fixture.invalid/ch/5000")))))

(ert-deftest emacs-reader-legado-worker-large-content-and-error ()
  (let* ((text (make-string 120000 ?甲))
         (start (lambda (cb) (emacs-reader-legado-content
                              '(:ruleContent (:content "$.body" :replaceRegex "##广告"))
                              '(:url "https://fixture.invalid/ch") cb)))
         (value (emacs-reader-legado-worker-test--read
                 (json-serialize (list :body (concat " 广告" text "\n\n\n 乙 "))) start))
         (bad (emacs-reader-legado-worker-test--read (concat "[" text) start)))
    (should (equal value (list (concat text "\n\n乙"))))
    (should-not (car bad))
    (should (string-search "JSON" (cdr bad)))))

(provide 'emacs-reader-legado-worker-test)
;;; emacs-reader-legado-worker-test.el ends here
