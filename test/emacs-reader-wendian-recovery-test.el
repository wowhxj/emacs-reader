;;; emacs-reader-wendian-recovery-test.el --- Wendian recovery tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Exercise manual recovery through the reader using an isolated page cache.

;;; Code:

(require 'ert)
(require 'emacs-reader)
(require 'emacs-reader-wendian-read)
(require 'emacs-reader-wendian-index)

(defconst emacs-reader-wendian-recovery-test--url
  "https://wendian.dicomp.net/ancient/detail.php?id=2")

(defun emacs-reader-wendian-recovery-test--setup ()
  "Set up a Wendian preview in the current buffer."
  (emacs-reader-reading-mode)
  (setq emacs-reader--current-book
        `((readerType . "legado") (origin . "https://wendian.dicomp.net")
          (bookUrl . ,emacs-reader-wendian-recovery-test--url) (name . "书"))
        emacs-reader--preview-p t
        emacs-reader--preview-start-pending t))

(ert-deftest emacs-reader-wendian-recovery-import-in-place ()
  (let* ((emacs-reader-directory (make-temp-file "reader-recovery" t))
         (html (expand-file-name "book.html" emacs-reader-directory))
         (emacs-reader-wendian--pages nil)
         (url emacs-reader-wendian-recovery-test--url)
         requested opened)
    (unwind-protect
        (with-temp-buffer
          (emacs-reader-wendian-recovery-test--setup)
          (cl-letf (((symbol-function 'emacs-reader-local--source)
                     (lambda (_) (list :bookSourceUrl "https://wendian.dicomp.net"
                                       :searchUrl emacs-reader-wendian--index-url)))
                    ((symbol-function 'emacs-reader-legado--fetch)
                     (lambda (_source target _base callback &rest _)
                       (push target requested)
                       (funcall callback nil target "需要人工验证" target)))
                    ((symbol-function 'browse-url-default-browser)
                     (lambda (target &rest _) (setq opened target)))
                    ((symbol-function 'read-file-name) (lambda (&rest _) html))
                    ((symbol-function 'emacs-reader-local--add) (lambda (&rest _) (ert-fail "入架")))
                    ((symbol-function 'emacs-reader-local--progress)
                     (lambda (&rest _) (ert-fail "写入进度"))))
            (emacs-reader--load-catalog)
            (should emacs-reader--recovery-mode)
            (dolist (action emacs-reader--recovery-actions)
              (should (eq (key-binding (kbd (car action))) (nth 2 action))))
            (dolist (label (cdr header-line-format))
              (should (commandp (lookup-key (get-text-property 0 'local-map label)
                                            [header-line mouse-1]))))
            (call-interactively (key-binding "v"))
            (should (equal opened url))
            (with-temp-file html (insert "<title>Verification</title>"))
            (should-error (call-interactively (key-binding "i")))
            (should emacs-reader--recovery-mode)
            (with-temp-file html
              (insert "<main><h1>书</h1><article><section id='ch-1'>本章正文</section>"
                      "</article></main>"))
            (call-interactively (key-binding "i"))
            (should (equal requested (list url)))
            (should (string-search "本章正文" (buffer-string)))
            (should (= emacs-reader--current-chapter-index 0))
            (should-not emacs-reader--recovery-mode)))
      (delete-directory emacs-reader-directory t))))

(ert-deftest emacs-reader-wendian-recovery-failed-chapter-target ()
  (with-temp-buffer
    (emacs-reader-wendian-recovery-test--setup)
    (let* ((target (concat emacs-reader-wendian-recovery-test--url "#ch-2"))
           callbacks imported)
      (setq emacs-reader--preview-start-pending nil
            emacs-reader--chapters
            (vector '((title . "第一章") (native :url "first"))
                    `((title . "第二章") (native :url ,target))))
      (cl-letf (((symbol-function 'emacs-reader-local--fetch)
                 (lambda (_book _chapter callback) (push callback callbacks)))
                ((symbol-function 'read-file-name) (lambda (&rest _) "book.html"))
                ((symbol-function 'emacs-reader-wendian-import-page)
                 (lambda (url file) (setq imported (list url file)))))
        (emacs-reader--load-chapter 0)
        (funcall (car callbacks) "第一章正文" nil)
        (let ((original (buffer-string)))
          (emacs-reader--load-chapter 1)
          (emacs-reader--load-chapter 1)
          (funcall (cadr callbacks) nil "过期")
          (should-not emacs-reader--recovery)
          (funcall (car callbacks) nil "验证")
          (should (equal original (buffer-string)))
          (should (= emacs-reader--current-chapter-index 0))
          (should (equal target (plist-get emacs-reader--recovery :url)))
          (call-interactively (key-binding "g"))
          (funcall (car callbacks) nil "再次失败")
          (call-interactively (key-binding "i"))
          (should (equal imported (list target "book.html")))
          (funcall (car callbacks) "第二章正文" nil)
          (should (string-search "第二章正文" (buffer-string)))
          (should (= emacs-reader--current-chapter-index 1))
          (should-not emacs-reader--recovery-mode))))))

(ert-deftest emacs-reader-wendian-recovery-stale-catalog-and-other-sources ()
  (with-temp-buffer
    (emacs-reader-wendian-recovery-test--setup)
    (let (callbacks)
      (cl-letf (((symbol-function 'emacs-reader--request-catalog)
                 (lambda (_book callback) (push callback callbacks))))
        (emacs-reader--load-catalog)
        (emacs-reader-wendian-recovery-test--setup)
        (emacs-reader--load-catalog)
        (funcall (cadr callbacks) nil)
        (should-not emacs-reader--recovery)
        (should emacs-reader--catalog-loading)
        (setq emacs-reader--current-book (copy-tree emacs-reader--current-book))
        (setf (alist-get 'origin emacs-reader--current-book) "https://example.org")
        (funcall (car callbacks) nil)
        (should-not emacs-reader--recovery-mode)))))

(ert-deftest emacs-reader-wendian-recovery-file-picker-ignores-obsolete-target ()
  (with-temp-buffer
    (emacs-reader-wendian-recovery-test--setup)
    (emacs-reader--offer-recovery emacs-reader-wendian-recovery-test--url)
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (&rest _) (emacs-reader--clear-recovery) "old.html"))
              ((symbol-function 'emacs-reader-wendian-import-page)
               (lambda (&rest _) (ert-fail "导入过期请求"))))
      (should-error (emacs-reader-wendian-continue-html) :type 'user-error))))

(provide 'emacs-reader-wendian-recovery-test)
;;; emacs-reader-wendian-recovery-test.el ends here
