;;; emacs-reader-translation-engine-test.el --- Offline translation tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise the public subscription API with deterministic simulated gptel replies.

;;; Code:

(require 'ert)
(require 'emacs-reader-translation-engine)

(defconst emacs-reader-translation-test--book '((bookUrl . "book:1") (name . "Book")))
(defconst emacs-reader-translation-test--chapter '((index . 1) (title . "章")))

(defvar emacs-reader-translation-test--requests)
(defvar emacs-reader-translation-test--aborted)
(defvar emacs-reader-translation-test--results)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-context)
(defvar gptel-use-tools)

(defmacro emacs-reader-translation-test--with-mock (&rest body)
  "Run BODY against a private reader library and mocked gptel transport."
  (declare (indent 0) (debug t))
  `(let ((emacs-reader-directory (make-temp-file "reader-translation-" t))
         (emacs-reader-translation--jobs (make-hash-table :test #'equal))
         (gptel-model 'test-model) (gptel-backend 'test-backend)
         emacs-reader-translation-test--requests emacs-reader-translation-test--aborted
         emacs-reader-translation-test--results
         (original-require (symbol-function 'require)))
     (unwind-protect
         (cl-letf (((symbol-function 'require)
                    (lambda (feature &rest args)
                      (if (eq feature 'gptel) feature (apply original-require feature args))))
                   ((symbol-function 'gptel-abort)
                    (lambda (buffer) (push buffer emacs-reader-translation-test--aborted)))
                   ((symbol-function 'gptel-request)
                    (lambda (prompt &rest args)
                      (push (list prompt args (buffer-string) gptel-backend gptel-model
                                  gptel-use-context gptel-use-tools)
                            emacs-reader-translation-test--requests))))
           ,@body)
       (delete-directory emacs-reader-directory t))))

(defun emacs-reader-translation-test--request (paragraphs &optional direction callback)
  "Subscribe to PARAGRAPHS in DIRECTION with optional CALLBACK."
  (emacs-reader-translation-request
   emacs-reader-translation-test--book emacs-reader-translation-test--chapter
   (or direction 'en-zh) paragraphs
   (or callback (lambda (value error)
                  (push (list value error) emacs-reader-translation-test--results)))))

(defmacro emacs-reader-translation-test--wait (form)
  "Wait briefly for timers until FORM holds."
  `(let ((deadline (+ 2 (float-time))))
     (while (and (not ,form) (< (float-time) deadline)) (accept-process-output nil 0.01))
     (should ,form)))

(defun emacs-reader-translation-test--reply (response)
  "Pass RESPONSE to the latest simulated request."
  (funcall (plist-get (cadar emacs-reader-translation-test--requests) :callback)
           response '(:status "mock server")))

(ert-deftest emacs-reader-translation-engine-shares-work-and-isolates-context ()
  (emacs-reader-translation-test--with-mock
   (with-temp-buffer
     (insert "private unrelated buffer contents")
     (let* ((paragraphs '("quote \"A\"" "" "non-ASCII Ω"))
            (cancel (emacs-reader-translation-test--request paragraphs))
            (second (emacs-reader-translation-test--request paragraphs)))
       (should-not emacs-reader-translation-test--requests)
       (emacs-reader-translation-test--wait emacs-reader-translation-test--requests)
       (funcall cancel)
       (should-not emacs-reader-translation-test--aborted)
       (pcase-let ((`(,prompt ,args ,contents ,backend ,model ,context ,tools)
                    (car emacs-reader-translation-test--requests)))
         ;; gptel serializes the prompt again inside its HTTP request body.
         (let* ((body (json-serialize (list :content prompt)))
                (request (json-parse-string body :object-type 'alist)))
           (should (equal prompt (alist-get 'content request))))
         (should (equal "" contents))
         (should (equal '(test-backend test-model nil nil) (list backend model context tools)))
         (should-not (plist-get args :transforms))
         (should (equal (mapcar (lambda (item) (alist-get 'text item))
                                (json-parse-string prompt :object-type 'alist))
                        '("quote \"A\"" "non-ASCII Ω"))))
       (emacs-reader-translation-test--reply '(reasoning . "thinking"))
       (emacs-reader-translation-test--reply
        "[{\"id\":1,\"text\":\"乙\"},{\"id\":0,\"text\":\"甲\"}]")
       (emacs-reader-translation-test--wait emacs-reader-translation-test--results)
       (should (equal '((("甲" "" "乙") nil)) emacs-reader-translation-test--results))
       (funcall second)
       ;; Edit the durable asset, then simulate a fresh Emacs session.
       (let ((path (emacs-reader-translation-asset-path
                    emacs-reader-translation-test--book emacs-reader-translation-test--chapter
                    'en-zh paragraphs)))
         (with-temp-buffer
           (insert-file-contents path)
           (goto-char (point-min))
           (search-forward ": 甲")
           (replace-match ": 校订稿" t t)
           (let ((coding-system-for-write 'utf-8-unix)) (write-region nil nil path))))
       (let ((emacs-reader-translation--jobs (make-hash-table :test #'equal)))
         (emacs-reader-translation-test--request paragraphs)
         (emacs-reader-translation-test--wait
          (= 2 (length emacs-reader-translation-test--results)))
         (should (equal (car emacs-reader-translation-test--results) '(("校订稿" "" "乙") nil)))
         (should (= 1 (length emacs-reader-translation-test--requests))))))))

(ert-deftest emacs-reader-translation-engine-resumes-after-invalid-reply ()
  (emacs-reader-translation-test--with-mock
   (let ((emacs-reader-translation--batch-size 4) (paragraphs '("abcdefgh" "ij")))
     (emacs-reader-translation-test--request paragraphs 'zh-en)
     (emacs-reader-translation-test--wait emacs-reader-translation-test--requests)
     (emacs-reader-translation-test--reply "[{\"id\":0,\"text\":\"one\"}]")
     (emacs-reader-translation-test--wait (= 2 (length emacs-reader-translation-test--requests)))
     (emacs-reader-translation-test--reply "[{\"id\":7,\"text\":\"wrong\"}]")
     (should (stringp (cadar emacs-reader-translation-test--results)))
     (emacs-reader-translation-test--request paragraphs 'zh-en)
     (emacs-reader-translation-test--wait (= 3 (length emacs-reader-translation-test--requests)))
     (should (equal (caar emacs-reader-translation-test--requests)
                    "[{\"id\":1,\"text\":\"efgh\"}]"))
     (emacs-reader-translation-test--reply "[{\"id\":1,\"text\":\"two\"}]")
     (emacs-reader-translation-test--wait (= 4 (length emacs-reader-translation-test--requests)))
     (emacs-reader-translation-test--reply "[{\"id\":2,\"text\":\"three\"}]")
     (emacs-reader-translation-test--wait
      (= 2 (length emacs-reader-translation-test--results)))
     (should (equal (car emacs-reader-translation-test--results) '(("one two" "three") nil))))))

(ert-deftest emacs-reader-translation-engine-cancel-discards-late-reply ()
  (emacs-reader-translation-test--with-mock
   (let ((cancel (emacs-reader-translation-test--request '("a"))))
     (emacs-reader-translation-test--wait emacs-reader-translation-test--requests)
     (funcall cancel)
     (funcall cancel)
     (should (= 1 (length emacs-reader-translation-test--aborted)))
     (emacs-reader-translation-test--request '("a"))
     (emacs-reader-translation-test--reply "[{\"id\":0,\"text\":\"stale\"}]")
     (emacs-reader-translation-test--wait (= 2 (length emacs-reader-translation-test--requests)))
     (emacs-reader-translation-test--reply nil)
     (should (equal '((nil "gptel: mock server")) emacs-reader-translation-test--results)))))

(ert-deftest emacs-reader-translation-engine-delivers-first-batch-before-completion ()
  (emacs-reader-translation-test--with-mock
   (let ((lines '("a" "b" "c" "d" "e")) received replay result)
     (emacs-reader-translation-request
      emacs-reader-translation-test--book emacs-reader-translation-test--chapter 'en-zh lines
      (lambda (texts err) (should-not err) (setq result texts))
      (lambda (start texts) (push (cons start texts) received)))
     (emacs-reader-translation-test--wait emacs-reader-translation-test--requests)
     (let ((prompt (caar emacs-reader-translation-test--requests)))
       (should (= 4 (length (json-parse-string prompt))))
       (emacs-reader-translation-test--reply prompt))
     (should (equal received '((0 "a" "b" "c" "d"))))
     (should-not result)
     (emacs-reader-translation-request
      emacs-reader-translation-test--book emacs-reader-translation-test--chapter 'en-zh lines
      #'ignore (lambda (start texts) (push (cons start texts) replay)))
     (should (equal replay received))
     (emacs-reader-translation-test--wait (= 2 (length emacs-reader-translation-test--requests)))
     (emacs-reader-translation-test--reply (caar emacs-reader-translation-test--requests))
     (emacs-reader-translation-test--wait result)
     (should (equal (car received) '(4 "e")))
     (should (equal result lines)))))

(ert-deftest emacs-reader-translation-engine-preserves-invalid-asset ()
  (emacs-reader-translation-test--with-mock
   (let ((path (emacs-reader-translation-asset-path
                emacs-reader-translation-test--book emacs-reader-translation-test--chapter
                'en-zh '("source"))))
     (make-directory (file-name-directory path) t)
     (with-temp-file path (insert "user-edited malformed asset"))
     (emacs-reader-translation-test--request '("source"))
     (emacs-reader-translation-test--wait emacs-reader-translation-test--results)
     (should (stringp (cadar emacs-reader-translation-test--results)))
     (should-not emacs-reader-translation-test--requests)
     (with-temp-buffer (insert-file-contents path)
                       (should (equal (buffer-string) "user-edited malformed asset"))))))

(ert-deftest emacs-reader-translation-engine-rejects-unmatched-items ()
  (let ((batch '((0 0 "a") (1 1 "b"))))
    (dolist (invalid '("[]" "[{},{}]" "[{\"id\":0,\"text\":\"x\"},{\"id\":0,\"text\":\"y\"}]"
                       "[{\"id\":0,\"text\":\"x\"},{\"id\":1,\"text\":null}]"))
      (should-error (emacs-reader-translation--ordered
                     (json-parse-string invalid :object-type 'alist) batch)))))

(provide 'emacs-reader-translation-engine-test)
;;; emacs-reader-translation-engine-test.el ends here
