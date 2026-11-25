;;; emacs-reader-image-pdf-test.el --- Scanned PDF integration tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'emacs-reader)
(require 'emacs-reader-files)

(ert-deftest emacs-reader-image-pdf-open-uses-emureader ()
  (let* ((emacs-reader-directory (make-temp-file "reader-image-test-" t))
         (path (expand-file-name "scan.pdf" emacs-reader-directory))
         (book '((readerType . "file") (readerImagePdf . t) (bookUrl . "scan.pdf")
                 (name . "Scan")))
         opened ensured)
    (unwind-protect
        (progn
          (write-region "PDF" nil path nil 'silent)
          (cl-letf (((symbol-function 'emureader-ensure-module) (lambda () (setq ensured t)))
                    ((symbol-function 'emureader-dyn--load-doc) (lambda (&rest _) t))
                    ((symbol-function 'emureader-open-doc) (lambda (file) (setq opened file)))
                    ((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                    ((symbol-value 'save-place-mode) nil))
            (with-temp-buffer
              (use-local-map (make-sparse-keymap))
              (emacs-reader-open-book book)
              (should (eq (key-binding (kbd "q")) #'emacs-reader-quit)))
            (should ensured)
            (should (equal opened path))))
      (delete-directory emacs-reader-directory t))))

(ert-deftest emacs-reader-image-pdf-unavailable-module ()
  (let* ((emacs-reader-directory (make-temp-file "reader-image-test-" t))
         (path (expand-file-name "scan.pdf" emacs-reader-directory)))
    (unwind-protect
        (progn
          (write-region "PDF" nil path nil 'silent)
          (cl-letf (((symbol-function 'emureader-ensure-module) #'ignore)
                    ((symbol-function 'emureader-open-doc) #'ignore))
            (let ((had (fboundp 'emureader-dyn--load-doc))
                  (original (and (fboundp 'emureader-dyn--load-doc)
                                 (symbol-function 'emureader-dyn--load-doc))))
              (unwind-protect
                  (progn (fmakunbound 'emureader-dyn--load-doc)
                         (should-error (emacs-reader-open-book
                                        '((readerType . "file") (readerImagePdf . t)
                                          (bookUrl . "scan.pdf") (name . "Scan")))
                                       :type 'user-error))
                (when had (fset 'emureader-dyn--load-doc original))))))
      (delete-directory emacs-reader-directory t))))

(ert-deftest emacs-reader-image-pdf-real-async-import ()
  (let ((source (getenv "READER_SCAN_PDF"))
        (bad (getenv "READER_BAD_PDF")))
    (unless (and source bad) (ert-skip "Set READER_SCAN_PDF and READER_BAD_PDF"))
    (let ((emacs-reader-directory (make-temp-file "reader-image-library-" t))
          (emacs-reader-books-directory nil)
          result failure done process)
      (unwind-protect
          (progn
            (setq process (emacs-reader-import-file-async
                           source nil nil nil
                           (lambda (book error) (setq result book failure error done t))))
            (should (processp process))
            (should-not done)
            (let ((deadline (+ (float-time) 120)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 1)))
            (should done)
            (should-not failure)
            (should (eq (alist-get 'readerImagePdf result) t))
            (should (= 448 (alist-get 'totalChapterNum result)))
            (should (eq (alist-get 'readerImagePdf
                                   (emacs-reader-local--read
                                    (emacs-reader-local--shelf-path result))) t))
            (setq done nil failure nil)
            (emacs-reader-import-file-async
             bad nil nil nil (lambda (_book error) (setq failure error done t)))
            (let ((deadline (+ (float-time) 120)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 1)))
            (should done)
            (should failure))
        (delete-directory emacs-reader-directory t)))))

(provide 'emacs-reader-image-pdf-test)
;;; emacs-reader-image-pdf-test.el ends here
