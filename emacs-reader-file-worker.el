;;; emacs-reader-file-worker.el --- Background local book operations -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Run the existing file parser in a clean batch Emacs.  Only the parent
;; updates the shelf record and refreshes interactive buffers.

;;; Code:
(require 'emacs-reader-files)

(declare-function emacs-reader-local--prepare-import "emacs-reader-local")

(defun emacs-reader-file-worker--main ()
  "Read one request from the command line and print its result."
  (let ((coding-system-for-write 'utf-8-unix)
        (print-length nil) (print-level nil) (print-circle nil))
    (prin1
     (condition-case err
         (pcase-let* ((`(,paths ,op . ,args) (read (pop command-line-args-left)))
                      (exec-path paths))
           (list (pcase op
                   ('titles
                    (cl-loop for path in (directory-files (car args) t "\\.epub\\'")
                             for meta = (ignore-errors (emacs-reader-files-metadata path))
                             for title = (plist-get meta :title)
                             collect (cons (format "%s 〔%s〕" (or title "EPUB")
                                                   (file-name-nondirectory path)) path)))
                   ('inspect (apply #'emacs-reader-files--inspect args))
                   ('chapters (apply #'emacs-reader-files-chapters args))
                   ('content (apply #'emacs-reader-files-content args))
                   ('import (require 'emacs-reader-local)
                            (apply #'emacs-reader-local--prepare-import args))
                   (_ (error "Unknown book operation"))) nil))
       (error (list nil (error-message-string err)))))))

(defun emacs-reader-file-worker-start (op args callback)
  "Run OP with ARGS and deliver (RESULT ERROR) to CALLBACK exactly once."
  (let* ((default-directory temporary-file-directory)
         (library (locate-library "emacs-reader-file-worker"))
         (program (expand-file-name invocation-name invocation-directory))
         (print-length nil) (print-level nil)
         (command (list program "-Q" "--batch" "--eval" "(setq load-prefer-newer t)"
                        "-L" (file-name-directory library)
                        "-l" library "-f" "emacs-reader-file-worker--main"
                        (prin1-to-string (cons exec-path (cons op args)))))
         (output (generate-new-buffer " *Reader file worker*"))
         (errors (generate-new-buffer " *Reader file errors*")))
    (condition-case err
        (make-process
         :name "reader-file-worker" :buffer output :stderr errors :command command
         :connection-type 'pipe :coding 'utf-8-unix :noquery t
         :sentinel
         (lambda (process event)
           (when (and (buffer-live-p output) (memq (process-status process) '(exit signal)))
             (let ((result
                    (unwind-protect
                        (condition-case failure
                            (with-current-buffer output
                              (unless (zerop (process-exit-status process))
                                (error "Book worker %s: %s" (string-trim event)
                                       (with-current-buffer errors (string-trim (buffer-string)))))
                              (goto-char (point-min))
                              (read (current-buffer)))
                          (error (list nil (error-message-string failure))))
                      (kill-buffer output)
                      (kill-buffer errors))))
               (apply callback result)))))
      (error (kill-buffer output) (kill-buffer errors) (signal (car err) (cdr err))))))

(provide 'emacs-reader-file-worker)
;;; emacs-reader-file-worker.el ends here
