;;; emacs-reader-sources-export-test.el --- Source ZIP tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise the public export command against a temporary local registry.

;;; Code:

(require 'ert)
(require 'emacs-reader-sources-export)
(require 'emacs-reader-sources)

(defmacro with-emacs-reader-sources-export-test-registry (&rest body)
  "Run BODY with a private registry and an existing output directory."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "reader-export-" t))
          (emacs-reader-directory (expand-file-name "library" root))
          (output (expand-file-name "中文 备份" root)))
     (unwind-protect
         (progn (make-directory output) ,@body)
       (delete-directory root t))))

(ert-deftest emacs-reader-sources-export-test-round-trip ()
  (with-emacs-reader-sources-export-test-registry
   (let ((sources '(((type . "opds") (name . "同名/📖") (url . "https://a.test"))
                    ((type . "opds") (name . "同名/📖") (url . "https://b.test"))
                    ((type . "json") (name . "搜书") (url . "https://c.test/?q={query}")
                     (definition . ((rows . "data") (label . "title") (href . "url"))))
                    ((type . "legado") (name . "书库") (url . "https://l.test")
                     (aliases . ["https://old.test"]) (extra . :null)
                     (definition . ((bookSourceName . "书库") (bookSourceUrl . "https://l.test")
                                    (enabled . :false) (header . ((Cookie . "私密示例")))))))))
     (mapc #'emacs-reader-sources--save sources)
     (let* ((emacs-reader-sources--records nil)
            (paths (emacs-reader-sources-export "all" output))
            (expected-stamp (substring (car paths) -21 -4)))
       (should-not emacs-reader-sources--records)
       (should (= 3 (length paths)))
       (should (string-match-p "\\`[0-9]\\{17\\}\\'" expected-stamp))
       (dolist (zip paths)
         (let* ((name (file-name-nondirectory zip))
                (kind (nth 3 (split-string name "-")))
                (extracted (expand-file-name kind root))
                (emacs-reader-directory (expand-file-name "restored" extracted)))
           (should (equal name (format "emacs-reader-booksource-%s-backup-%s.zip"
                                       kind expected-stamp)))
           (should (= #o600 (file-modes zip)))
           (should (equal 0 (call-process "python3" nil nil nil "-I" "-m" "zipfile"
                                          "-e" zip extracted)))
           (let ((entries (directory-files (expand-file-name "records" extracted) t "\\.json\\'"))
                 (originals (seq-filter (lambda (r) (equal kind (alist-get 'type r))) sources)))
             (should (= (length originals) (length entries)))
             (dolist (entry entries)
               (let ((record (emacs-reader-local--read entry)))
                 (should (member record originals))
                 (pcase kind
                   ("json" (emacs-reader-sources-import-json entry))
                   ("opds" (emacs-reader-sources-add-opds
                            (alist-get 'name record) (alist-get 'url record)))
                   ("legado"
                    (let ((definitions (expand-file-name "legado.json" extracted)))
                      (should (equal (emacs-reader-local--read definitions)
                                     (vector (alist-get 'definition record))))
                      (emacs-reader-import-legado definitions))))
                 (should (file-exists-p (emacs-reader-sources--path record))))))))))))

(ert-deftest emacs-reader-sources-export-test-selection-and-collision ()
  (with-emacs-reader-sources-export-test-registry
   (should-error (emacs-reader-sources-export "all" output) :type 'user-error)
   (emacs-reader-sources--save '((type . "opds") (name . "目录") (url . "https://a.test")))
   (should-error (emacs-reader-sources-export "json" output) :type 'user-error)
   (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "20250701010203123"))
             ((symbol-function 'completing-read) (lambda (&rest _) "opds"))
             ((symbol-function 'read-directory-name) (lambda (&rest _) output)))
     (let* ((paths (call-interactively #'emacs-reader-sources-export))
            (path (car paths)) (attrs (file-attributes path)))
       (should (= 1 (length paths)))
       (should-error (emacs-reader-sources-export "all" output) :type 'user-error)
       (should (equal attrs (file-attributes path)))
       (should (equal (directory-files output t directory-files-no-dot-files-regexp) paths))))))

(ert-deftest emacs-reader-sources-export-test-concurrent-collision-rollback ()
  (with-emacs-reader-sources-export-test-registry
   (emacs-reader-sources--save '((type . "opds") (name . "目录") (url . "https://a.test")))
   (emacs-reader-sources--save '((type . "json") (name . "搜索") (url . "https://j.test/{query}")
                                 (definition . ((rows . "r") (label . "l") (href . "u")))))
   (let ((publish (symbol-function 'add-name-to-file)) collision)
     (cl-letf (((symbol-function 'add-name-to-file)
                (lambda (src dest)
                  (when (string-match-p "-json-backup-" dest)
                    (setq collision dest)
                    (write-region "existing" nil dest nil 'silent))
                  (funcall publish src dest))))
       (should-error (emacs-reader-sources-export "all" output) :type 'file-already-exists))
     (should (equal (directory-files output t directory-files-no-dot-files-regexp)
                    (list collision)))
     (with-temp-buffer
       (insert-file-contents collision)
       (should (equal "existing" (buffer-string)))))))

(ert-deftest emacs-reader-sources-export-test-exclusive-copy ()
  (with-emacs-reader-sources-export-test-registry
   (emacs-reader-sources--save '((type . "opds") (name . "目录") (url . "https://a.test")))
   (cl-letf (((symbol-function 'add-name-to-file)
              (lambda (&rest _) (signal 'file-error '("Hard links unsupported")))))
     (let ((archive (car (emacs-reader-sources-export "opds" output))))
       (should (equal 0 (call-process "python3" nil nil nil "-I" "-m" "zipfile" "-t" archive)))
       (should (= #o600 (file-modes archive)))
       (should-error (emacs-reader-sources-export--publish archive archive "python3")
                     :type 'file-already-exists)
       (let ((emacs-reader-sources-export--exclusive-copy
              (string-replace "shutil.copyfileobj(source, target)"
                              "target.write(b'x'); raise OSError('disk full')"
                              emacs-reader-sources-export--exclusive-copy))
             (partial (expand-file-name "partial.zip" output)))
         (should-error (emacs-reader-sources-export--publish archive partial "python3")
                       :type 'file-error)
         (should-not (file-exists-p partial)))
       (should (equal (directory-files output t directory-files-no-dot-files-regexp)
                      (list archive)))))))

(provide 'emacs-reader-sources-export-test)
;;; emacs-reader-sources-export-test.el ends here
