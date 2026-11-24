;;; emacs-reader-wendian-index-test.el --- Wendian index tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; The fixture uses LightRead's six-field rows, including absent authors and variant refs.

;;; Code:

(require 'ert)
(require 'emacs-reader-wendian-index)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defconst emacs-reader-wendian-test--data
  '(:version 1 :site "https://wendian.dicomp.net/" :sections ["ancient" "tcm" "classics" "mengxue"]
             :categories ["古籍" "中医" "四书" "蒙学"] :authors ["王應麟" "孔子"] :t2s "經经論论語语應应傳传統统"
             :books [["三字經（傳統版）" 0 3 3 "三字經|傳統版" 1615]
                     ["論語Ａ編" 1 0 0 2 30] ["疏論語" -1 0 0 3 40]
                     ["論語" 1 2 2 "论语" 22000] ["论语" -1 0 0 7 22001]
                     ["本草" 0 1 1 1 99000]]))

(defconst emacs-reader-wendian-test--source
  (list :bookSourceUrl emacs-reader-wendian--site :bookSourceName "问典甲"
        :searchUrl emacs-reader-wendian--index-url))

(defun emacs-reader-wendian-test--index ()
  "Prepare the test index."
  (with-temp-buffer
    (insert (json-encode emacs-reader-wendian-test--data))
    (goto-char (point-min))
    (emacs-reader-wendian--prepare)))

(ert-deftest emacs-reader-wendian-index-queries ()
  (let ((index (emacs-reader-wendian-test--index)) (emacs-reader-wendian--page-size 2))
    (cl-flet ((search-page (q &optional p)
                (emacs-reader-wendian--results emacs-reader-wendian-test--source index q (or p 1))))
      (should (equal (mapcar (lambda (r) (plist-get r :name)) (search-page "《论语》"))
                     '("論語" "论语")))
      (should (equal (mapcar (lambda (r) (plist-get r :name)) (search-page "《论语》" 2))
                     '("論語Ａ編" "疏論語")))
      (should-not (search-page "《论语》" 3))
      (should-not (search-page "　，　"))
      (should-not (search-page "本草 孔子"))
      (should (equal (plist-get (car (search-page "论语a 孔子")) :bookUrl)
                     "https://wendian.dicomp.net/ancient/detail.php?id=2"))
      (should (equal (plist-get (car (search-page "王应麟 本草")) :bookUrl)
                     "https://wendian.dicomp.net/tcm/detail.php?id=1"))
      (should (equal (plist-get (car (search-page "论语")) :bookUrl)
                     "https://wendian.dicomp.net/classics/book.php?book=%E8%AE%BA%E8%AF%AD"))
      (let ((row (car (search-page "王应麟，三字经"))))
        (should (equal (plist-get row :bookUrl)
                       (concat "https://wendian.dicomp.net/mengxue/book.php?book="
                               "%E4%B8%89%E5%AD%97%E7%B6%93&var=%E5%82%B3%E7%B5%B1%E7%89%88")))
        (should (equal (mapcar (lambda (k) (plist-get row k))
                               '(:author :kind :wordCount :originName))
                       '("王應麟" "蒙学" "1615" "问典甲")))))))

(defun emacs-reader-wendian-test--respond (completion body)
  "Invoke simulated HTTP COMPLETION with BODY as UTF-8 bytes."
  (with-current-buffer (generate-new-buffer " *wendian response*")
    (set-buffer-multibyte nil)
    (insert "HTTP/1.1 200 OK\r\n\r\n")
    (let ((url-http-end-of-headers (point-marker)) (url-http-response-status 200))
      (insert (encode-coding-string body 'utf-8))
      (apply (car completion) nil (cdr completion)))))

(ert-deftest emacs-reader-wendian-index-concurrent-cache-and-retry ()
  (let* ((emacs-reader-directory (make-temp-file "reader-wendian-" t))
         (file (expand-file-name "wendian/index.json" emacs-reader-directory))
         (emacs-reader-wendian--indexes (make-hash-table :test #'equal))
         (emacs-reader-wendian--pending (make-hash-table :test #'equal))
         (other (plist-put (copy-sequence emacs-reader-wendian-test--source) :bookSourceName "乙"))
         starts completion timers first second)
    (unwind-protect
        (cl-letf (((symbol-function 'cancel-timer) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (_ _ f &rest args) (push (cons f args) timers)))
                  ((symbol-function 'url-retrieve)
                   (lambda (url f args &rest _) (push url starts) (setq completion (cons f args)))))
          (emacs-reader-wendian-search other "论语" (lambda (rows err) (setq first (list rows err))))
          (should-not first)
          (apply (caar timers) (cdar timers))
          (emacs-reader-wendian-test--respond completion "{}")
          (should (stringp (cadr first)))
          (should-not (file-exists-p file))
          (setq first nil)
          (emacs-reader-wendian-search other "本草" (lambda (rows err) (setq first (list rows err))))
          (apply (caar timers) (cdar timers))
          ;; The stalled transport has not called back.  Fire its timeout and
          ;; ensure a late success cannot overwrite the cache or reply twice.
          (apply (caar timers) (cdar timers))
          (should (string-match-p "超时" (cadr first)))
          (emacs-reader-wendian-test--respond completion
                                              (json-encode emacs-reader-wendian-test--data))
          (should-not (file-exists-p file))
          (should (string-match-p "超时" (cadr first)))
          (setq first nil timers nil starts nil)
          (emacs-reader-wendian-search emacs-reader-wendian-test--source "论语"
                                       (lambda (rows err) (setq first (list (car rows) err))))
          (emacs-reader-wendian-search other "本草"
                                       (lambda (rows err) (setq second (list (car rows) err))))
          (should (= (length timers) 1))
          (apply (caar timers) (cdar timers))
          (should-not (or first second))
          (should (equal starts (list emacs-reader-wendian--index-url)))
          (emacs-reader-wendian-test--respond completion
                                              (json-encode emacs-reader-wendian-test--data))
          (should (equal (plist-get (car first) :name) "論語"))
          (should (equal (plist-get (car second) :name) "本草"))
          (should (equal (plist-get (car first) :originName) "问典甲"))
          (should (equal (plist-get (car second) :originName) "乙"))
          (should-not (or (cadr first) (cadr second)))
          (should (file-regular-p file))
          (clrhash emacs-reader-wendian--indexes)
          (setq starts nil second nil)
          (emacs-reader-wendian-search other "本草"
                                       (lambda (rows err) (setq second (list (car rows) err))))
          (apply (caar timers) (cdar timers))
          (should-not starts)
          (should (equal (plist-get (car second) :author) "王應麟")))
      (delete-directory emacs-reader-directory t))))

(ert-deftest emacs-reader-wendian-index-untrusted-source ()
  (let (result)
    (emacs-reader-wendian-search '(:searchUrl "https://example.org/wendian-index.json") "书"
                                 (lambda (rows err) (setq result (list rows err))))
    (should-not (car result))
    (should (stringp (cadr result)))))

(provide 'emacs-reader-wendian-index-test)
;;; emacs-reader-wendian-index-test.el ends here
