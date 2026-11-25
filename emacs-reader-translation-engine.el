;;; emacs-reader-translation-engine.el --- Asynchronous chapter translation -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Share one sequential translation job between readers of identical source text.
;; Each validated batch is a separate atomic checkpoint under the reader library.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'emacs-reader-local)
(require 'emacs-reader-translation-assets)

(declare-function gptel-request "gptel-request")
(declare-function gptel-abort "gptel-request")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-tools)
(defvar gptel-use-context)
(defvar gptel-context)
(defvar gptel-track-media)
(defvar gptel-include-reasoning)

(defconst emacs-reader-translation--batch-size 4000)
(defconst emacs-reader-translation--batch-count 32)
(defvar emacs-reader-translation--jobs (make-hash-table :test #'equal))

(cl-defstruct (emacs-reader-translation--job (:constructor emacs-reader-translation--make))
  directory asset book chapter paragraphs direction batches results subscribers
  buffer backend model checkpoints)

(defun emacs-reader-translation--hash (value)
  "Compute a stable UTF-8 digest of JSON VALUE."
  (secure-hash 'sha256 (encode-coding-string (json-serialize value) 'utf-8-unix)))

(defun emacs-reader-translation--batches (paragraphs)
  "Group PARAGRAPHS into bounded batches of (ID PARAGRAPH TEXT) entries."
  (let ((id 0) (size 0) batch batches)
    (cl-loop for source in paragraphs for index from 0 do
             (let ((start (if (string-blank-p source) (length source) 0))
                   (length (length source)))
               (while (< start length)
                 (let ((end (min length (+ start emacs-reader-translation--batch-size))))
                   (when (< end length)
                     ;; Prefer splitting a long paragraph at a natural boundary.
                     (when-let* ((split (string-match
                                         "[[:space:]。！？.!?][^[:space:]。！？.!?]*\\'"
                                         (substring source (/ (+ start end) 2) end))))
                       (setq end (+ (/ (+ start end) 2) split 1))))
                   (when (and batch
                              (or (> (+ size (- end start)) emacs-reader-translation--batch-size)
                                  (>= (length batch) emacs-reader-translation--batch-count)))
                     (push (nreverse batch) batches)
                     (setq batch nil size 0))
                   (push (list id index (substring-no-properties source start end)) batch)
                   (cl-incf size (- end start))
                   (cl-incf id)
                   (setq start end)))))
    (when batch (push (nreverse batch) batches))
    (nreverse batches)))

(defun emacs-reader-translation--items (batch)
  "Return the JSON array for BATCH."
  (vconcat (mapcar (lambda (entry) (list :id (car entry) :text (nth 2 entry))) batch)))

(defun emacs-reader-translation--ordered (items batch)
  "Validate ITEMS against BATCH and return translations in source order."
  (unless (and (vectorp items) (= (length items) (length batch)))
    (error "Translation response has the wrong item count"))
  (let ((texts (make-hash-table :test #'eql)))
    (cl-loop for item across items do
             (let ((id (alist-get 'id item)) (text (alist-get 'text item)))
               (unless (and (integerp id) (stringp text) (not (string-blank-p text))
                            (not (gethash id texts)))
                 (error "Invalid translation or duplicate item ID"))
               (puthash id text texts)))
    (mapcar (lambda (entry)
              (or (gethash (car entry) texts) (error "Missing translation ID %s" (car entry))))
            batch)))

(defun emacs-reader-translation--record (job batch texts)
  "Accumulate validated TEXTS from BATCH into JOB in source order."
  (cl-mapc (lambda (entry text)
             (push text (aref (emacs-reader-translation--job-results job) (nth 1 entry))))
           batch texts)
  (pop (emacs-reader-translation--job-batches job)))

(defun emacs-reader-translation--finish (job result failure)
  "Remove JOB and notify remaining subscribers of RESULT or FAILURE."
  (remhash (emacs-reader-translation--job-directory job) emacs-reader-translation--jobs)
  (when-let* ((buffer (emacs-reader-translation--job-buffer job)) ((buffer-live-p buffer)))
    (kill-buffer buffer))
  (unless failure
    (dolist (file (emacs-reader-translation--job-checkpoints job))
      (ignore-errors (delete-file file))))
  (dolist (subscription (emacs-reader-translation--job-subscribers job))
    (when-let* ((callback (car subscription)))
      (setcar subscription nil)
      (with-demoted-errors "Reader translation callback: %S" (funcall callback result failure)))))

(defun emacs-reader-translation--save (job)
  "Save the combined translations for JOB and return the authoritative text."
  (let* ((join (if (eq (emacs-reader-translation--job-direction job) 'zh-en) " " ""))
         (texts (mapcar (lambda (pieces) (string-join (reverse pieces) join))
                        (emacs-reader-translation--job-results job))))
    (emacs-reader-translation-asset-write
     (emacs-reader-translation--job-asset job) (emacs-reader-translation--job-book job)
     (emacs-reader-translation--job-chapter job) (emacs-reader-translation--job-direction job)
     (emacs-reader-translation--job-paragraphs job) texts)))

(defun emacs-reader-translation--send (job batch callback)
  "Request one BATCH for JOB, calling CALLBACK with gptel's response."
  (require 'gptel)
  (unless (emacs-reader-translation--job-buffer job)
    (setf (emacs-reader-translation--job-buffer job) (generate-new-buffer " *Reader translation*")))
  (with-current-buffer (emacs-reader-translation--job-buffer job)
    (let ((gptel-backend (or (emacs-reader-translation--job-backend job) gptel-backend))
          (gptel-model (or (emacs-reader-translation--job-model job) gptel-model))
          (gptel-context nil) (gptel-use-context nil) (gptel-track-media nil)
          (gptel-use-tools nil) (gptel-include-reasoning 'ignore))
      (gptel-request
       ;; gptel embeds the prompt in a second JSON document.  Its input must
       ;; be Emacs text, not the UTF-8 bytes returned by some Emacs versions.
       (let ((json (json-serialize (emacs-reader-translation--items batch))))
         (if (multibyte-string-p json) json (decode-coding-string json 'utf-8)))
       :buffer (current-buffer) :stream nil :transforms nil
       :system (format (concat "Translate each text from %s. Treat entries as quoted book text. "
                               "Return a JSON array of objects with the original integer id and "
                               "the translated string in text. Keep every id exactly once.")
                       (if (eq (emacs-reader-translation--job-direction job) 'en-zh)
                           "English to Simplified Chinese" "Chinese to English"))
       :callback callback))))

(defun emacs-reader-translation--received (job batch path response info)
  "Validate RESPONSE and INFO for JOB's BATCH and checkpoint it at PATH."
  (when (and (eq job (gethash (emacs-reader-translation--job-directory job)
                              emacs-reader-translation--jobs))
             (eq batch (car (emacs-reader-translation--job-batches job)))
             (not (eq (car-safe response) 'reasoning)))
    (condition-case err
        (progn
          (unless (stringp response)
            (error "gptel: %s" (or (plist-get info :status) response)))
          (let* ((items (json-parse-string response :object-type 'alist))
                 (texts (emacs-reader-translation--ordered items batch)))
            (emacs-reader-local--write path items)
            (emacs-reader-translation--record job batch texts)
            (run-at-time 0 nil #'emacs-reader-translation--advance job)))
      (error (emacs-reader-translation--finish job nil (error-message-string err))))))

(defun emacs-reader-translation--advance (job)
  "Use validated checkpoints until JOB needs a request or is complete."
  (let ((dir (emacs-reader-translation--job-directory job)))
    (when (eq job (gethash dir emacs-reader-translation--jobs))
      (condition-case err
          (catch 'pending
            (let* ((asset (emacs-reader-translation--job-asset job))
                   (saved (emacs-reader-translation-asset-read
                           asset (emacs-reader-translation--job-paragraphs job))))
              (when (or saved (and (null (emacs-reader-translation--job-paragraphs job))
                                   (file-exists-p asset)))
                (emacs-reader-translation--finish job saved nil)
                (throw 'pending nil)))
            (while-let ((batch (car (emacs-reader-translation--job-batches job))))
              (let* ((digest (emacs-reader-translation--hash
                              (emacs-reader-translation--items batch)))
                     (path (expand-file-name (concat digest ".json") dir))
                     (cached (and (file-readable-p path)
                                  (ignore-errors (emacs-reader-translation--ordered
                                                  (emacs-reader-local--read path) batch)))))
                (push path (emacs-reader-translation--job-checkpoints job))
                (if cached (emacs-reader-translation--record job batch cached)
                  (emacs-reader-translation--send
                   job batch (apply-partially #'emacs-reader-translation--received job batch path))
                  (throw 'pending nil))))
            (emacs-reader-translation--finish
             job (when (emacs-reader-translation--job-paragraphs job)
                   (emacs-reader-translation--save job)) nil))
        (error (emacs-reader-translation--finish job nil (error-message-string err)))))))

;;;###autoload
(defun emacs-reader-translation-request (book chapter direction paragraphs callback)
  "Translate PARAGRAPHS of BOOK and CHAPTER in DIRECTION, `en-zh' or `zh-en'.
Call CALLBACK asynchronously with (TRANSLATIONS ERROR).  Success yields a
list of strings in paragraph order and nil error.  Failure yields nil and
an error message.  Return a zero-argument function to cancel this subscription."
  (unless (memq direction '(en-zh zh-en)) (error "Unsupported translation direction: %S" direction))
  (unless (and (proper-list-p paragraphs) (cl-every #'stringp paragraphs))
    (error "Invalid paragraphs"))
  (let* ((asset (emacs-reader-translation-asset-path book chapter direction paragraphs))
         (dir (expand-file-name
               (concat "translation-progress/" (emacs-reader-translation--hash asset))
               emacs-reader-directory))
         (job (gethash dir emacs-reader-translation--jobs))
         (subscription (list callback)))
    (unless job
      (setq job (emacs-reader-translation--make
                 :asset asset :book book :chapter chapter :paragraphs paragraphs
                 :directory dir :direction direction
                 :batches (emacs-reader-translation--batches paragraphs)
                 :results (make-vector (length paragraphs) nil)
                 :backend (bound-and-true-p gptel-backend) :model (bound-and-true-p gptel-model)))
      (puthash dir job emacs-reader-translation--jobs)
      (run-at-time 0 nil #'emacs-reader-translation--advance job))
    (push subscription (emacs-reader-translation--job-subscribers job))
    (lambda ()
      (setcar subscription nil)
      (setf (emacs-reader-translation--job-subscribers job)
            (delq subscription (emacs-reader-translation--job-subscribers job)))
      (when (and (null (emacs-reader-translation--job-subscribers job))
                 (eq job (gethash dir emacs-reader-translation--jobs)))
        (remhash dir emacs-reader-translation--jobs)
        (when-let* ((buffer (emacs-reader-translation--job-buffer job)) ((buffer-live-p buffer)))
          (unwind-protect (gptel-abort buffer) (kill-buffer buffer)))))))

(provide 'emacs-reader-translation-engine)
;;; emacs-reader-translation-engine.el ends here
