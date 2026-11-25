;;; emacs-reader-file-worker-test.el --- File worker protocol tests -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'emacs-reader-file-worker)

(ert-deftest emacs-reader-file-worker-test-large-result ()
  "A large multibyte response is delivered completely, once."
  (let* ((text (make-string 1000000 ?中))
         (path (make-temp-file "reader-worker-large-" nil ".txt" text))
         (calls 0) value failure)
    (unwind-protect
        (progn
          (emacs-reader-file-worker-start
           'content (list path (list :title "large" :url (format "txt:1:%d" (1+ (length text)))))
           (lambda (result error) (cl-incf calls) (setq value result failure error)))
          (let ((deadline (+ (float-time) 30)))
            (while (and (zerop calls) (< (float-time) deadline))
              (accept-process-output nil 0.1)))
          (should (= calls 1))
          (should-not failure)
          (should (equal text value)))
      (delete-file path))))

(ert-deftest emacs-reader-file-worker-test-response-validation ()
  "Protocol failures are distinguishable from errors returned by the worker."
  (dolist (input '("" "(\"unfinished" "(nil nil) extra" "(nil)"))
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (let ((response (emacs-reader-file-worker--read-response)))
        (should-not (car response))
        (should (string-prefix-p "Book worker response error:" (cadr response))))))
  (with-temp-buffer
    (insert "(nil \"End of file during parsing\")\n")
    (goto-char (point-min))
    (should (equal '(nil "End of file during parsing")
                   (emacs-reader-file-worker--read-response)))))

(ert-deftest emacs-reader-file-worker-test-library-source-selection ()
  "Choose source only when it exists and is newer than the located bytecode."
  (let* ((dir (make-temp-file "reader-worker-library-" t))
         (source (expand-file-name "worker.el" dir))
         (bytecode (concat source "c")))
    (unwind-protect
        (progn
          (write-region "" nil bytecode nil 'silent)
          (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) bytecode)))
            (should (equal bytecode (emacs-reader-file-worker--library)))
            (write-region "" nil source nil 'silent)
            (set-file-times source (seconds-to-time 100))
            (set-file-times bytecode (seconds-to-time 200))
            (should (equal bytecode (emacs-reader-file-worker--library)))
            (set-file-times source (seconds-to-time 300))
            (should (equal source (emacs-reader-file-worker--library)))))
      (delete-directory dir t))))

(provide 'emacs-reader-file-worker-test)
;;; emacs-reader-file-worker-test.el ends here
