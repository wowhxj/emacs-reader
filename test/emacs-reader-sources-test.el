;;; emacs-reader-sources-test.el --- Source manager tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise the manager against a temporary registry.

;;; Code:

(require 'ert)
(require 'emacs-reader-sources)

(defmacro emacs-reader-sources-test--with-registry (&rest body)
  "Run BODY with an empty registry and isolated manager buffer."
  (declare (indent 0) (debug t))
  `(let ((emacs-reader-directory (make-temp-file "reader-source-test-" t))
         (emacs-reader-sources--buffer (generate-new-buffer-name "*Sources test*")))
     (unwind-protect
         (save-window-excursion
           (cl-letf (((symbol-function 'emacs-reader--display-buffer) #'switch-to-buffer))
             ,@body))
       (when (get-buffer emacs-reader-sources--buffer)
         (kill-buffer emacs-reader-sources--buffer))
       (delete-directory emacs-reader-directory t))))

(defun emacs-reader-sources-test--legado (url)
  "Return a Legado record with representative rules for URL."
  `((type . "legado") (name . "旧名") (url . ,url)
    (definition . ((bookSourceName . "旧名") (bookSourceUrl . ,url)
                   (ruleContent . ((content . "正文"))) (enabled . :false)))))

(ert-deftest emacs-reader-sources-test-legado-edit ()
  (emacs-reader-sources-test--with-registry
    (let* ((old "https://one.test") (new "https://two.test")
           (src (emacs-reader-sources-test--legado old)))
      (emacs-reader-sources--save src)
      (emacs-reader-sources)
      (should (derived-mode-p 'tabulated-list-mode))
      (dolist (key '("a" "e" "d" "g" "q" "n" "p" "?"))
        (should (commandp (key-binding (kbd key)))))
      (emacs-reader-sources-edit)
      (unwind-protect
          (progn
            (should (equal (key-binding (kbd "C-c C-c")) #'emacs-reader-sources-edit-save))
            (dolist (change `((,old . ,new) ("旧名" . "新名") ("正文" . "新规则")))
              (goto-char (point-min))
              (search-forward (car change))
              (replace-match (cdr change) t t))
            (cl-letf (((symbol-function 'yes-or-no-p) #'ignore))
              (emacs-reader-sources-edit-save))
            (should (derived-mode-p 'emacs-reader-sources-edit-mode))
            (should (equal (emacs-reader-local--records "sources") (list src)))
            (cl-letf (((symbol-function 'yes-or-no-p) #'always))
              (emacs-reader-sources-edit-save))
            (let* ((row (emacs-reader-sources--selected)) (def (alist-get 'definition row)))
              (should (equal (alist-get 'aliases row) (vector old)))
              (should (equal (alist-get 'name row) "新名"))
              (should (equal (alist-get 'url row) new))
              (should (equal (alist-get 'content (alist-get 'ruleContent def)) "新规则"))
              (should (eq (alist-get 'enabled def) :false))
              (should-not (file-exists-p (emacs-reader-sources--path src)))))
        (when (derived-mode-p 'emacs-reader-sources-edit-mode) (kill-buffer))))))

(ert-deftest emacs-reader-sources-test-conflicts-and-rollback ()
  (emacs-reader-sources-test--with-registry
    (let* ((src (emacs-reader-sources-test--legado "https://current.test"))
           (other (emacs-reader-sources-test--legado "https://taken.test"))
           (old-path (emacs-reader-sources--path src))
           (delete-fn (symbol-function 'delete-file)))
      (setf (alist-get 'aliases other) ["https://former.test"])
      (emacs-reader-sources--save src)
      (emacs-reader-sources--save other)
      (dolist (url '("https://taken.test" "https://former.test"))
        (should-error (emacs-reader-sources--save (emacs-reader-sources-test--legado url) src)
                      :type 'user-error))
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (path &rest args)
                   (if (equal path old-path) (signal 'file-error '("模拟删除失败"))
                     (apply delete-fn path args)))))
        (should-error (emacs-reader-sources--save
                       (emacs-reader-sources-test--legado "https://new.test") src)
                      :type 'file-error))
      (let ((records (emacs-reader-local--records "sources")))
        (should (= (length records) 2))
        (should (member src records))
        (should (member other records))))))

(ert-deftest emacs-reader-sources-test-preset-lifecycle ()
  (emacs-reader-sources-test--with-registry
    (let ((copy (expand-file-name "books/copy.txt" emacs-reader-directory)))
      (make-directory (file-name-directory copy) t)
      (write-region "书籍" nil copy)
      (emacs-reader-sources)
      (should-not (emacs-reader-local--records "sources"))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Open Library")))
        (emacs-reader-sources-add-preset))
      (let* ((src (emacs-reader-sources--selected)) (new (copy-tree src)))
        (should (equal (alist-get 'type src) "json"))
        (let ((invalid (copy-tree src)))
          (setf (alist-get 'label (alist-get 'definition invalid)) "")
          (should-error (emacs-reader-sources--save invalid src) :type 'user-error))
        (setf (alist-get 'name new) "我的目录"
              (alist-get 'url new) "https://custom.test/?q={query}"
              (alist-get 'rows (alist-get 'definition new)) "results")
        (emacs-reader-sources-edit)
        (erase-buffer)
        (insert "{")
        (should-error (emacs-reader-sources-edit-save) :type 'json-error)
        (call-interactively (key-binding (kbd "C-c C-k")))
        (should (equal (emacs-reader-local--records "sources") (list src)))
        (emacs-reader-sources-edit)
        (erase-buffer)
        (insert (decode-coding-string (json-serialize new) 'utf-8))
        (cl-letf (((symbol-function 'yes-or-no-p) #'always))
          (call-interactively (key-binding (kbd "C-c C-c"))))
        (should (equal (emacs-reader-local--records "sources") (list new)))
        (cl-letf (((symbol-function 'yes-or-no-p) #'ignore)) (emacs-reader-sources-delete))
        (should (equal (emacs-reader-sources--selected) new))
        (cl-letf (((symbol-function 'yes-or-no-p) #'always)) (emacs-reader-sources-delete))
        (should-not (emacs-reader-local--records "sources"))
        (should (file-exists-p copy)))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "文渊阁")))
        (emacs-reader-sources-add-preset))
      (should (equal (alist-get 'type (emacs-reader-sources--selected)) "opds")))))

(ert-deftest emacs-reader-sources-register-bibliographic-catalog ()
  (emacs-reader-sources-test--with-registry
    (let ((src '((type . "json") (name . "书目") (url . "https://example.test/?q={query}")
                 (definition . ((rows . "docs") (label . "title") (recordHref . "{key}"))))))
      (emacs-reader-sources--save src)
      (should (equal (emacs-reader-local--records "sources") (list src))))))

(provide 'emacs-reader-sources-test)

;; Local Variables:
;; elisp-lint-indent-specs: ((emacs-reader-sources-test--with-registry . 0))
;; End:
;;; emacs-reader-sources-test.el ends here
