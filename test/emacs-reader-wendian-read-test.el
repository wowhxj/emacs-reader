;;; emacs-reader-wendian-read-test.el --- Wendian reading tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Exercise the public book, catalog and content callbacks with Wendian HTML.

;;; Code:

(require 'ert)
(require 'emacs-reader-wendian-read)

(defmacro emacs-reader-wendian-read-test--pages (pages &rest body)
  "Serve successive (URL HTML) PAGES while running BODY."
  (declare (indent 1))
  `(let ((pending ,pages) (emacs-reader-wendian--pages nil)
         (emacs-reader-directory (expand-file-name (make-temp-name "wd-test")
                                                   temporary-file-directory)))
     (cl-letf (((symbol-function 'emacs-reader-legado--fetch)
                (lambda (_source url _base callback &optional _search)
                  (pcase-let ((`(,expected ,html) (pop pending)))
                    (should (equal expected url))
                    (funcall callback (emacs-reader-legado--parse html) url nil url)))))
       ,@body
       (should-not pending))))

(defun emacs-reader-wendian-read-test--call (fn item)
  "Call FN with ITEM and return its synchronous fixture result."
  (let ((result 'no-response))
    (funcall fn nil item (lambda (v e) (should-not e) (setq result v)))
    (should-not (eq result 'no-response))
    result))

(ert-deftest emacs-reader-wendian-ancient-chapter-cache ()
  (let* ((url "https://wendian.dicomp.net/ancient/detail.php?id=19014")
         (html (concat "<main><nav>字典工具</nav><h1>《西游记》<span>西遊記</span></h1>"
                       "<article><section id='ch-0'><h2>第一回</h2><p><a class='poem-char'"
                       " href='/dict/'>混</a><a>沌</a>未分<br>天地乱。</p></section>"
                       "<section id='ch-1'><h2>第二回</h2><p>二&amp;三。</p></section>"
                       "</article><a>下一章</a></main>")))
    (emacs-reader-wendian-read-test--pages
     (list (list url html))
     (let* ((info (emacs-reader-wendian-read-test--call
                   #'emacs-reader-wendian-book-info (list :bookUrl url :author "吴承恩")))
            (rows (emacs-reader-wendian-read-test--call #'emacs-reader-wendian-chapters info)))
       (should (equal info (list :bookUrl url :author "吴承恩" :name "西游记" :tocUrl url)))
       (should (equal rows (list (list :title "第一回" :url (concat url "#ch-0"))
                                 (list :title "第二回" :url (concat url "#ch-1")))))
       (should (equal (emacs-reader-wendian-read-test--call #'emacs-reader-wendian-content
                                                            (car rows))
                      "混沌未分\n天地乱。"))
       (should (equal (emacs-reader-wendian-read-test--call #'emacs-reader-wendian-content
                                                            (cadr rows))
                      "二&三。"))
       (emacs-reader-wendian-content nil (list :url (concat url "#ch-9"))
                                     (lambda (v e) (should-not v) (should e)))))))

(ert-deftest emacs-reader-wendian-medical-unsectioned ()
  (let ((url "https://wendian.dicomp.net/tcm/detail.php?id=4"))
    (emacs-reader-wendian-read-test--pages
     (list (list url "<main><h1>医书</h1><article><p>属性：风寒</p><p>治法。</p></article></main>"))
     (let ((rows (emacs-reader-wendian-read-test--call #'emacs-reader-wendian-chapters
                                                       (list :bookUrl url))))
       (should (equal rows (list (list :title "医书" :url url))))
       (should (equal (emacs-reader-wendian-read-test--call #'emacs-reader-wendian-content
                                                            (car rows))
                      "风寒\n治法。"))))))

(ert-deftest emacs-reader-wendian-linked-chapters ()
  (dolist (section '("mengxue" "classics"))
    (let* ((base (concat "https://wendian.dicomp.net/" section "/"))
           (book (concat base "book.php?book=book")) (detail (concat base "detail.php?id=2")))
      (emacs-reader-wendian-read-test--pages
       (list (list book (concat "<main><h1>书</h1><a href='detail.php?id=1'>开始阅读</a>"
                                "<div id='toc'><a href='detail.php?id=1'>上篇</a>"
                                "<a href='detail.php?id=2'><span class='truncate'>"
                                "<span>2.</span>下篇"
                                "</span><span>1.2K</span></a></div>"
                                "<a href='detail.php?id=8'>其他作品</a></main>"))
             (list detail "<main><article><a>天</a><a>地</a>玄黃\n宇宙洪荒</article></main>"))
       (let ((rows (emacs-reader-wendian-read-test--call #'emacs-reader-wendian-chapters
                                                         (list :bookUrl book))))
         (should (equal rows (list (list :title "上篇" :url (concat base "detail.php?id=1"))
                                   (list :title "下篇" :url detail))))
         (should (equal (emacs-reader-wendian-read-test--call #'emacs-reader-wendian-content
                                                              (cadr rows)) "天地玄黃\n宇宙洪荒")))))))

(ert-deftest emacs-reader-wendian-verification-and-retry ()
  (let ((url "https://wendian.dicomp.net/ancient/detail.php?id=9"))
    (emacs-reader-wendian-read-test--pages
     (list (list url "<html><title>Security Verification</title></html>")
           (list url "<main><h1>书</h1><article>正文</article></main>"))
     (emacs-reader-wendian-content nil (list :url url)
                                   (lambda (v e) (should-not v) (should (string-search url e))
                                     (should (string-search "emacs-reader-wendian-import-html" e))))
     (should (equal (emacs-reader-wendian-read-test--call #'emacs-reader-wendian-content
                                                          (list :url url)) "正文")))))

(ert-deftest emacs-reader-wendian-import-persists ()
  (let* ((emacs-reader-directory (make-temp-file "wendian-library" t))
         (html (expand-file-name "chapter.html" emacs-reader-directory))
         (url "https://wendian.dicomp.net/ancient/detail.php?id=1")
         (emacs-reader-wendian--pages nil))
    (unwind-protect
        (progn
          (with-temp-file html
            (insert "<link rel='canonical' href='" url "'><main><h1>书</h1>"
                    "<article><section id='ch-2'><h2>标题</h2>本文</section></article></main>"))
          (should-error (emacs-reader-wendian-import-page (concat url "2") html))
          (should (= 1 (emacs-reader-wendian-import-page (concat url "#ch-2") html)))
          (setq emacs-reader-wendian--pages nil)
          (cl-letf (((symbol-function 'emacs-reader-legado--fetch)
                     (lambda (&rest _) (ert-fail "网络请求"))))
            (should (equal (emacs-reader-wendian-read-test--call #'emacs-reader-wendian-content
                                                                 (list :url (concat url "#ch-2")))
                           "本文"))))
      (delete-directory emacs-reader-directory t))))

(provide 'emacs-reader-wendian-read-test)
;;; emacs-reader-wendian-read-test.el ends here
