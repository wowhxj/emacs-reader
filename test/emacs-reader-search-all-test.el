;;; emacs-reader-search-all-test.el --- Cross-source search tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Deterministic provider callbacks exercise out-of-order and late replies.

;;; Code:
(require 'ert)
(require 'emacs-reader-search-all)

(ert-deftest emacs-reader-all-mixed-sources-and-actions ()
  (let* ((legado '((type . "legado") (url . "https://example.test") (name . "小说")
                   (definition . ((bookSourceUrl . "https://example.test")))))
         (opds '((type . "opds") (name . "分类目录")))
         (json '((type . "json") (name . "JSON")))
         (entry (emacs-reader-opds--item :kind 'download :label "作品" :url "https://a.test/b.epub"))
         (book '(:name "小说作品" :origin "https://example.test" :bookUrl "https://example.test/b"))
         requests opened added)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'emacs-reader--display-buffer) #'switch-to-buffer)
                    ((symbol-function 'emacs-reader-legado-search)
                     (lambda (_ q cb page) (should (equal q "书")) (push (cons page cb) requests)))
                    ((symbol-function 'emacs-reader-catalog-results)
                     (lambda (src _ cb) (push (cons (alist-get 'type src) cb) requests)))
                    ((symbol-function 'emacs-reader-catalog-open-item)
                     (lambda (src item) (setq opened (list src item))))
                    ((symbol-function 'emacs-reader-add-to-shelf)
                     (lambda (item) (setq added item))))
            (emacs-reader-search-all "书" (list legado opds json))
            (funcall (cdr (assoc "opds" requests)) nil "HTTP 502")
            (funcall (cdr (assoc "json" requests)) (list entry) nil)
            (funcall (cdr (assoc 1 requests)) (list book) nil)
            (should (= 3 (length tabulated-list-entries)))
            (goto-char (point-min))
            (call-interactively (key-binding (kbd "RET")))
            (should (equal opened (list json entry)))
            (forward-line 1)
            (call-interactively (key-binding "a"))
            (should (equal (alist-get 'name added) "小说作品"))
            (emacs-reader-all-retry)
            (funcall (cdr (assoc "opds" requests)) nil nil)
            (should (= 2 (length tabulated-list-entries)))
            (emacs-reader-all-more)
            (funcall (cdr (assoc 2 requests)) (list book) nil)
            (should-error (emacs-reader-all-more) :type 'user-error)))
      (when-let* ((buffer (get-buffer "*Reader Search All*"))) (kill-buffer buffer)))))

(ert-deftest emacs-reader-all-bounds-requests-and-ignores-stale-replies ()
  (let (callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'emacs-reader--display-buffer) #'switch-to-buffer)
                    ((symbol-function 'emacs-reader-catalog-results)
                     (lambda (_src _query cb) (push cb callbacks))))
            (let ((source '((type . "opds") (name . "目录"))))
              (emacs-reader-search-all "旧" (make-list 6 source))
              (should (= 4 (length callbacks)))
              (funcall (car callbacks) nil nil)
              (should (= 5 (length callbacks)))
              (let ((late (car callbacks)))
                (emacs-reader-search-all "新" (list source))
                (funcall late nil "过期")
                (should (= 1 (length emacs-reader-all--running)))
                (should-not tabulated-list-entries)
                (funcall (car callbacks) nil nil)
                (should-not emacs-reader-all--running)))))
      (when-let* ((buffer (get-buffer "*Reader Search All*"))) (kill-buffer buffer)))))

(provide 'emacs-reader-search-all-test)
;;; emacs-reader-search-all-test.el ends here
