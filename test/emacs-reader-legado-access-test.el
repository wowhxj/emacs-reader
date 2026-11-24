;;; emacs-reader-legado-access-test.el --- Search and chapter access tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Use the Easou and Motie search response shapes without accessing either server.

;;; Code:

(require 'ert)
(require 'emacs-reader-legado)

(defun emacs-reader-legado-access-test--search (source url json)
  "Run SOURCE search with JSON from URL, allowing exactly one fetch."
  (let ((calls 0) books)
    (cl-letf (((symbol-function 'emacs-reader-legado--fetch)
               (lambda (_source _target _base callback &optional _search)
                 (cl-incf calls)
                 (funcall callback (emacs-reader-legado--parse json) url nil url))))
      (emacs-reader-legado-search source "三国演义"
                                  (lambda (results err) (should-not err) (setq books results))))
    (should (= calls 1))
    books))

(ert-deftest emacs-reader-legado-access-easou-search ()
  (let* ((url "https://book.eayue.com/ta/searchMoreBoosAjax.m")
         (source (list :bookSourceUrl "http://api.ieasou.com/" :searchUrl url
                       :ruleSearch '(:bookList "$.[*]" :name "$.novelName@put:{gid:enid}"
                                               :author "$.author@put:{nid:nid}" :bookUrl "/{$.nid}")
                       :ruleToc
                       '(:isVip "$.charge@js:result.replace(/0!/, '').replace(/1!/, '🔒')")))
         (books (emacs-reader-legado-access-test--search
                 source url (concat "[{\"novelName\":\"三国演义\",\"author\":\"罗贯中\",\"nid\":5,"
                                    "\"chapterCount\":174,\"isCharge\":1},"
                                    "{\"novelName\":\"B\",\"nid\":6,\"isCharge\":0},"
                                    "{\"novelName\":\"C\",\"nid\":7,\"isCharge\":null},"
                                    "{\"novelName\":\"D\",\"nid\":8,\"isCharge\":2}]"))))
    (should (equal (plist-get (car books) :author) "罗贯中"))
    (should (equal (mapcar (lambda (b) (plist-get b :accessLabel)) books)
                   '("含收费章节" "免费" "可能含收费章节" "可能含收费章节")))))

(ert-deftest emacs-reader-legado-access-motie-search ()
  (let* ((url "http://app2.motie.com/search?word=abc")
         (source (list :bookSourceUrl "http://app2.motie.com" :bookSourceName "磨铁中文💰"
                       :searchUrl url
                       :ruleSearch '(:bookList "data.bookList" :name "name"
                                               :bookUrl "/pc/book/{$.id}/catalog")
                       :ruleToc (list :chapterName emacs-reader-legado--free-title-rule)))
         (body (concat "{\"data\":{\"bookList\":[{\"name\":\"A\",\"id\":2,\"free\":null},"
                       "{\"name\":\"B\",\"id\":3,\"free\":true},"
                       "{\"name\":\"C\",\"id\":4,\"free\":false}]}}")))
    (should (equal (mapcar (lambda (b) (plist-get b :accessLabel))
                           (emacs-reader-legado-access-test--search source url body))
                   '("可能含收费章节" "免费" "含收费章节")))
    (plist-put source :ruleToc nil)
    (dolist (book (emacs-reader-legado-access-test--search source url body))
      (should-not (plist-member book :accessLabel)))))

(ert-deftest emacs-reader-legado-access-generic-vip-rule ()
  (let ((row (emacs-reader-legado--parse "{\"isCharge\":1}"))
        (url "https://example.org/search"))
    (should (equal (emacs-reader-legado--search-access row '(:ruleToc (:isVip "$.vip")) url)
                   '(:accessLabel "可能含收费章节")))
    (should-not (emacs-reader-legado--search-access row '(:ruleToc (:isVip " ")) url))))

(ert-deftest emacs-reader-legado-access-chapter-values ()
  (let* ((src (list :ruleToc (list :chapterList "rows"
                                   :chapterName emacs-reader-legado--free-title-rule
                                   :chapterUrl "/{$.id}")))
         (doc (emacs-reader-legado--parse
               (concat "{\"rows\":[{\"name\":\"A\",\"id\":1,\"free\":true},"
                       "{\"name\":\"B\",\"id\":2,\"free\":false},"
                       "{\"name\":\"C\",\"id\":3,\"free\":null}]}")))
         (chapters (emacs-reader-legado--catalog src doc "https://example.org")))
    (should (equal (mapcar (lambda (c) (plist-get c :access)) chapters) '(free paid nil)))
    (should-not (plist-member (nth 2 chapters) :access)))
  (dolist (case '((0 free) (1 paid) ("1" paid) ("0" free) (2 nil) (nil nil)))
    (let ((row (emacs-reader-legado--parse "{\"nid\":12,\"sort\":1}")))
      (when (car case) (puthash "charge" (car case) (cdr row)))
      (let ((chapter (emacs-reader-legado--easou-chapter row "第一章" "12")))
        (should (eq (plist-get chapter :access) (cadr case)))
        (unless (cadr case) (should-not (plist-member chapter :access)))))))

(provide 'emacs-reader-legado-access-test)
;;; emacs-reader-legado-access-test.el ends here
