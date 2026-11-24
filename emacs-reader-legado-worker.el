;;; emacs-reader-legado-worker.el --- Local reading rule worker -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Parse large catalogs and chapters in a clean Emacs subprocess.  Network
;; requests, redirect checks and callbacks stay in the caller's Emacs.

;;; Code:

(require 'cl-lib)

(defun emacs-reader-legado-worker--main ()
  "Read a private job file and print one value/error response to stdout."
  (let ((file (pop command-line-args-left))
        (coding-system-for-write 'utf-8-unix)
        (print-length nil) (print-level nil) (print-circle nil) result)
    (condition-case err
        (pcase-let ((`(,fn . ,args)
                     (with-temp-buffer
                       (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file))
                       (read (current-buffer)))))
          (unless (memq fn '(emacs-reader-legado--project emacs-reader-legado--finish-content))
            (error "未知的书源解析操作"))
          (setq result (list (apply fn args) nil)))
      (error (setq result (list nil (error-message-string err)))))
    (prin1 result)))

(defun emacs-reader-legado-worker-run (function args callback)
  "Apply reading FUNCTION to ARGS locally, delivering (value error) to CALLBACK."
  (let* ((default-directory invocation-directory)
         (buffer (generate-new-buffer " *Legado parser*")) file failure)
    (condition-case err
        (progn
          (setq file (make-temp-file "emacs-reader-legado-"))
          (with-temp-buffer
            (let ((print-length nil) (print-level nil) (print-circle nil)
                  (coding-system-for-write 'utf-8-unix)
                  (write-region-annotate-functions nil))
              (prin1 (cons function args) (current-buffer))
              (write-region (point-min) (point-max) file nil 'silent)))
          (make-process
           :name "Legado parser" :buffer buffer :noquery t :connection-type 'pipe
           :coding 'utf-8-unix :stderr (get-buffer-create " *Legado parser stderr*")
           :command (list (expand-file-name invocation-name invocation-directory) "-Q" "--batch"
                          "-l" (symbol-file 'emacs-reader-legado--project 'defun)
                          "-l" (symbol-file 'emacs-reader-legado-worker--main 'defun)
                          "-f" "emacs-reader-legado-worker--main" file)
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (let (result)
                 (unwind-protect
                     (setq result (condition-case nil
                                      (and (zerop (process-exit-status process))
                                           (with-current-buffer buffer
                                             (goto-char (point-min)) (read (current-buffer))))
                                    (error nil)))
                   (kill-buffer buffer)
                   (delete-file file))
                 (apply callback (or result '(nil "书源解析进程意外退出"))))))))
      (error (setq failure (error-message-string err))))
    (when failure
      (kill-buffer buffer)
      (when file (delete-file file))
      (funcall callback nil failure))))

(provide 'emacs-reader-legado-worker)
;;; emacs-reader-legado-worker.el ends here
