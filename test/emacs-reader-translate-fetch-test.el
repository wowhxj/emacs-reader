;;; emacs-reader-translate-fetch-test.el --- Background chapter reads -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise plain, local EPUB and Reader3 resource paths with offline fixtures.

;;; Code:
(require 'ert)
(require 'emacs-reader-translate-fetch)
(require 'emacs-reader-translate-view)

(ert-deftest emacs-reader-translate-fetch-local-matches-reading-buffer ()
  (dolist (fixture '(("file" "book.txt" "one\nline!\ntwo。")
                     ("legado" "https://example.org" "one\nline!\ntwo。")
                     ("file" "book.epub" "one\nline!\n\ntwo。")))
    (pcase-let ((`(,type ,url ,body) fixture))
      (with-temp-buffer
        (emacs-reader-reading-mode)
        (setq emacs-reader--current-book `((readerType . ,type) (bookUrl . ,url))
              emacs-reader--preview-p t
              emacs-reader--current-chapter-index 0
              emacs-reader--chapters [((title . "One")) ((title . "Two") (native . (:url "2")))])
        (let ((inhibit-read-only t)) (insert "current text"))
        (let ((reader (current-buffer)) (window (selected-window)) paragraphs)
          (cl-letf (((symbol-function 'emacs-reader-local--fetch)
                     (lambda (_book native done)
                       (should (equal native '(:url "2"))) (funcall done body nil)))
                    ((symbol-function 'emacs-reader--tts-stop) (lambda () (ert-fail "TTS stopped")))
                    ((symbol-function 'emacs-reader-local--progress)
                     (lambda (&rest _) (ert-fail "Progress changed"))))
            (with-temp-buffer
              (let ((origin (current-buffer)))
                (emacs-reader-translate-fetch
                 reader 1 (lambda (texts err) (should-not err) (setq paragraphs texts)))
                (should (eq origin (current-buffer)))))
            (should (eq window (selected-window)))
            (should (equal (buffer-string) "current text"))
            (should (eq emacs-reader--current-chapter-index 0))
            (should-not emacs-reader--requested-chapter-index)
            (emacs-reader--display-chapter
             reader emacs-reader--chapter-request-id 1 body
             (and (equal url "book.epub") 'paragraphs))
            (should (= (length paragraphs) 2))
            (should (equal paragraphs (emacs-reader-translate-view-paragraphs)))))))))

(ert-deftest emacs-reader-translate-fetch-reader3-catalog-and-asset ()
  (skip-unless (libxml-available-p))
  (with-temp-buffer
    (emacs-reader-reading-mode)
    (setq emacs-reader--current-book '((bookUrl . "book")))
    (let ((emacs-reader-server-url "https://example.org") (emacs-reader--session-cookie "abc")
          (html "<html><body><p>Hello<br>there</p><p>World</p></body></html>") calls paragraphs)
      (cl-letf (((symbol-function 'emacs-reader--reader3-request)
                 (lambda (name method data callback no-relogin)
                   (should no-relogin)
                   (push name calls)
                   (should (equal method "POST"))
                   (pcase name
                     ("getChapterList"
                      (funcall callback
                               '(:ok ((isSuccess . t) (data . (((index . 0) (title . "A"))))))))
                     ("getBookContent"
                      (should (equal data '((url . "book") (index . 0) (cache . 1))))
                      (funcall callback '(:ok ((isSuccess . t) (data . "/epub/book/x.html"))))))))
                ((symbol-function 'emacs-reader--url-transport)
                 (lambda (req callback)
                   (should (equal (plist-get req :url) "https://example.org/epub/book/x.html"))
                   (should (equal (cdr (assoc "Cookie" (plist-get req :headers)))
                                  "reader.session=abc"))
                   (should (equal (plist-get req :method) "GET"))
                   (funcall callback (list :ok html)))))
        (emacs-reader-translate-fetch (current-buffer) 0
                                      (lambda (value err) (should-not err) (setq paragraphs value)))
        (should (equal calls '("getBookContent" "getChapterList")))
        (should-not emacs-reader--chapters)
        (should-not emacs-reader--current-chapter-index)
        (emacs-reader--display-chapter (current-buffer) emacs-reader--chapter-request-id 0 html t)
        (should (equal paragraphs (emacs-reader-translate-view-paragraphs)))
        (should (= 2 (length paragraphs)))))))

(ert-deftest emacs-reader-translate-fetch-rejects-locked-and-automatic-purchase ()
  (with-temp-buffer
    (setq emacs-reader--current-book '((bookUrl . "book")))
    (dolist (case '((((native . (:access paid))) "锁章")
                    (((native . (:easouLocked t))) "锁章")
                    (((isVip . t)) "锁章")
                    (((title . "🔒 Paid")) "锁章")
                    (((url . "https://x/a?autoBuy=1")) "自动购买")
                    (((chapterUrl . "https://x/a?x=y&autoBuy=1&z=0")) "自动购买")
                    (((native . (:url "https://x/a?autoBuy=%31"))) "自动购买")))
      (setq emacs-reader--chapters (vector (car case)))
      (let (reason)
        (cl-letf (((symbol-function 'emacs-reader--reader3-request)
                   (lambda (&rest _) (ert-fail "Unexpected request")))
                  ((symbol-function 'emacs-reader-local--fetch)
                   (lambda (&rest _) (ert-fail "Unexpected local request"))))
          (emacs-reader-translate-fetch (current-buffer) 0
                                        (lambda (body err) (should-not body) (setq reason err))))
        (should (string-search (cadr case) reason))))))

(ert-deftest emacs-reader-translate-fetch-distinguishes-notices-and-empty ()
  (with-temp-buffer
    (setq emacs-reader--current-book '((bookUrl . "book") (accessLabel . "可能含收费章节"))
          emacs-reader--chapters [((title . "第二章") (url . "https://x/?autoBuy=0"))])
    (dolist (case '(("　\n  " nil "没有正文") ("本页没有可显示的文字。" nil "没有正文")
                    ("当前版本过低，已停止使用。" nil "服务提示")
                    ("他读到“当前版本过低，已停止使用”。" ("他读到“当前版本过低，已停止使用”。") nil)))
      (let ((emacs-reader--session-cookie "fixture") result reason)
        (cl-letf (((symbol-function 'emacs-reader--reader3-request)
                   (lambda (_name _method _data done _no-relogin)
                     (funcall done `(:ok ((isSuccess . t) (data . ,(car case))))))))
          (emacs-reader-translate-fetch (current-buffer) 0
                                        (lambda (body err) (setq result body reason err))))
        (should (equal result (cadr case)))
        (if (nth 2 case) (should (string-search (nth 2 case) reason)) (should-not reason))))))

(ert-deftest emacs-reader-translate-fetch-cancels-before-resource-fetch ()
  (with-temp-buffer
    (setq emacs-reader--current-book '((bookUrl . "book")) emacs-reader--chapters [((title . "A"))])
    (let ((emacs-reader--session-cookie "fixture") response)
      (cl-letf (((symbol-function 'emacs-reader--reader3-request)
                 (lambda (_name _method _data done _no-relogin) (setq response done)))
                ((symbol-function 'emacs-reader--url-transport)
                 (lambda (&rest _) (ert-fail "Late resource request"))))
        (funcall (emacs-reader-translate-fetch (current-buffer) 0
                                               (lambda (&rest _) (ert-fail "Late callback"))))
        (funcall response '(:ok ((isSuccess . t) (data . "/epub/content.xhtml"))))))))

(ert-deftest emacs-reader-translate-fetch-never-prompts-for-login ()
  (with-temp-buffer
    (setq emacs-reader--current-book '((bookUrl . "book")))
    (let ((emacs-reader--session-cookie nil) reason)
      (cl-letf (((symbol-function 'emacs-reader--ensure-session)
                 (lambda (&rest _) (ert-fail "Unexpected login"))))
        (emacs-reader-translate-fetch (current-buffer) 0
                                      (lambda (body err) (should-not body) (setq reason err))))
      (should (stringp reason)))))

(provide 'emacs-reader-translate-fetch-test)
;;; emacs-reader-translate-fetch-test.el ends here
