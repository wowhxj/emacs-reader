;;; emacs-reader-filter-test.el --- Test live keyword narrowing -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise literal matching across columns and cancellation in a real minibuffer.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'emacs-reader-filter)

(ert-deftest emacs-reader-filter-literal-columns ()
  (with-temp-buffer
    (let* ((a '(1 ["Book [1]" ("HTTP" action ignore) "403"]))
           (b '(2 ["Book 1" "HTTP" "403"]))
           (rows (list a b)))
      (setq emacs-reader-filter-query "403 [1] http")
      (should (equal (emacs-reader-filter-rows rows) (list a)))
      (setq emacs-reader-filter-query "  ")
      (should (eq rows (emacs-reader-filter-rows rows))))))

(ert-deftest emacs-reader-filter-minibuffer-live-and-cancel ()
  (with-temp-buffer
    (let (seen)
      (setq emacs-reader-filter-refresh-function (lambda () (push emacs-reader-filter-query seen)))
      (ert-simulate-keys (kbd "a b RET") (emacs-reader-filter-edit))
      (should (member "a" seen))
      (should (equal emacs-reader-filter-query "ab"))
      (condition-case nil
          (ert-simulate-keys (kbd "c C-g") (emacs-reader-filter-edit))
        (quit nil))
      (should (member "abc" seen))
      (should (equal emacs-reader-filter-query "ab"))
      (emacs-reader-filter-clear)
      (should (equal emacs-reader-filter-query "")))))

(provide 'emacs-reader-filter-test)
;;; emacs-reader-filter-test.el ends here
