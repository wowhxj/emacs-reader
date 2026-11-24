;;; emacs-reader-wendian-index.el --- Search the Wendian book index -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Search the compact public LightRead index.  A validated copy on disk is
;; sufficient for subsequent Emacs sessions, including sessions without a network.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ucs-normalize)
(require 'url-http)
(require 'emacs-reader-local)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defconst emacs-reader-wendian--index-url
  "https://raw.githubusercontent.com/yzfly/LightRead/main/src/data/wendian-index.json")
(defconst emacs-reader-wendian--site "https://wendian.dicomp.net/")
(defconst emacs-reader-wendian--sections '("classics" "mengxue" "ancient" "tcm"))
(defconst emacs-reader-wendian--page-size 40)
(defvar emacs-reader-wendian--indexes (make-hash-table :test #'equal))
(defvar emacs-reader-wendian--pending (make-hash-table :test #'equal))

(cl-defstruct (emacs-reader-wendian--work (:conc-name emacs-reader-wendian--w-))
  title author category rank ref chars key base author-key)

(defun emacs-reader-wendian--fold (text table)
  "Normalize TEXT using NFKC, case folding, and the index's TABLE."
  (concat (cl-map 'vector (lambda (ch) (or (aref table ch) ch))
                  (downcase (ucs-normalize-NFKC-string text)))))

(defun emacs-reader-wendian--compact (text)
  "Return TEXT with only letters and digits retained."
  (replace-regexp-in-string "[^[:alnum:]]+" "" text))

(defun emacs-reader-wendian--prepare ()
  "Read a UTF-8 index from point and prepare search keys."
  (let* ((raw (json-parse-buffer :object-type 'plist))
         (sections (plist-get raw :sections)) (authors (plist-get raw :authors))
         (categories (plist-get raw :categories)) (books (plist-get raw :books))
         (pairs (plist-get raw :t2s)) (table (make-char-table nil)) works)
    (unless (and (equal (plist-get raw :version) 1)
                 (equal (plist-get raw :site) emacs-reader-wendian--site)
                 (vectorp books) (> (length books) 0) (vectorp sections)
                 (vectorp authors) (vectorp categories)
                 (stringp pairs) (zerop (% (length pairs) 2)))
      (error "无效的问典书目索引"))
    (cl-loop for i from 0 below (length pairs) by 2
             do (aset table (aref pairs i) (aref pairs (1+ i))))
    (cl-loop for row across books do
             (unless (and (vectorp row) (= (length row) 6))
               (error "无效的问典书目行"))
             (cl-destructuring-bind (title ai ci si ref chars) (append row nil)
               (let* ((rank (cl-position (aref sections si) emacs-reader-wendian--sections
                                         :test #'equal))
                      (author (if (eql ai -1) "" (aref authors ai)))
                      (folded (emacs-reader-wendian--fold title table))
                      (key (emacs-reader-wendian--compact folded)))
                 (unless (and rank (natnump chars) (not (string-empty-p title))
                              (if (< rank 2) (and (stringp ref) (not (string-empty-p ref)))
                                (natnump ref)))
                   (error "无效的问典书目记录 %s" title))
                 (push (make-emacs-reader-wendian--work
                        :title title :author author :category (aref categories ci)
                        :rank rank :ref ref :chars chars :key key
                        :base (if (string-match "[[(][^][()]*[])]\\'" folded)
                                  (emacs-reader-wendian--compact
                                   (substring folded 0 (match-beginning 0))) key)
                        :author-key (emacs-reader-wendian--compact
                                     (emacs-reader-wendian--fold author table))) works))))
    (cons table (nreverse works))))

(defun emacs-reader-wendian--score (work terms key)
  "Rate WORK for query TERMS and compact KEY."
  (let ((title (emacs-reader-wendian--w-key work))
        (author (emacs-reader-wendian--w-author-key work)))
    (cond ((or (equal title key) (equal (emacs-reader-wendian--w-base work) key)) 40)
          ((string-prefix-p key title) 30)
          ((or (string-search key title)
               (cl-every (lambda (term) (string-search term title)) terms)) 20)
          ((and (cl-some (lambda (term) (string-search term title)) terms)
                (cl-every (lambda (term) (or (string-search term title)
                                             (string-search term author))) terms)) 15)
          ((cl-every (lambda (term) (string-search term author)) terms) 10))))

(defun emacs-reader-wendian--row (source work)
  "Build a Legado result for SOURCE from WORK."
  (let* ((rank (emacs-reader-wendian--w-rank work))
         (section (nth rank emacs-reader-wendian--sections))
         (ref (emacs-reader-wendian--w-ref work))
         (path (if (>= rank 2) (format "detail.php?id=%d" ref)
                 (let ((parts (split-string ref "|")))
                   (concat "book.php?book=" (url-hexify-string (car parts))
                           (when (cadr parts)
                             (concat "&var=" (url-hexify-string (cadr parts)))))))))
    (list :name (emacs-reader-wendian--w-title work) :author (emacs-reader-wendian--w-author work)
          :kind (emacs-reader-wendian--w-category work)
          :wordCount (number-to-string (emacs-reader-wendian--w-chars work))
          :bookUrl (concat emacs-reader-wendian--site section "/" path)
          :origin (plist-get source :bookSourceUrl)
          :originName (plist-get source :bookSourceName))))

(defun emacs-reader-wendian--results (source index query page)
  "Search INDEX for QUERY on PAGE, returning Legado rows for SOURCE."
  (let* ((terms (split-string (emacs-reader-wendian--fold query (car index)) "[^[:alnum:]]+" t))
         (key (apply #'concat terms)) hits)
    (when terms
      (dolist (work (cdr index))
        (when-let* ((score (emacs-reader-wendian--score work terms key)))
          (push (cons (vector (- score) (emacs-reader-wendian--w-rank work)
                              (length (emacs-reader-wendian--w-key work))
                              (- (emacs-reader-wendian--w-chars work))) work) hits)))
      (setq hits (cl-stable-sort
                  (nreverse hits)
                  (lambda (a b)
                    (cl-loop for x across a for y across b unless (= x y) return (< x y)))
                  :key #'car))
      (cl-loop for hit in (nthcdr (* (1- page) emacs-reader-wendian--page-size) hits)
               repeat emacs-reader-wendian--page-size
               collect (emacs-reader-wendian--row source (cdr hit))))))

(defun emacs-reader-wendian--finish (file index problem)
  "Deliver INDEX or PROBLEM to each request waiting on FILE."
  (let ((waiting (nreverse (gethash file emacs-reader-wendian--pending))))
    (remhash file emacs-reader-wendian--pending)
    (when index (puthash file index emacs-reader-wendian--indexes))
    (dolist (request waiting)
      (condition-case err (funcall request index problem)
        (error (message "问典搜索回调失败：%s" (error-message-string err)))))))

(defun emacs-reader-wendian--receive (status file)
  "Validate the response with STATUS, then atomically cache its body in FILE."
  (let ((buffer (current-buffer)) temporary index problem)
    (unwind-protect
        (condition-case err
            (progn
              (when (or (plist-get status :error) (not (eql url-http-response-status 200)))
                (error "问典索引下载失败（HTTP %s，%s）" url-http-response-status status))
              (goto-char url-http-end-of-headers)
              (skip-chars-forward "\r\n")
              (let* ((start (point)) (parsed (emacs-reader-wendian--prepare))
                     (coding-system-for-write 'no-conversion))
                (make-directory (file-name-directory file) t)
                (setq temporary (make-temp-file (concat file ".")))
                (write-region start (point-max) temporary nil 'silent)
                (rename-file temporary file t)
                (setq temporary nil index parsed)))
          (error (setq problem (error-message-string err))))
      (when (and temporary (file-exists-p temporary)) (delete-file temporary))
      (kill-buffer buffer))
    (emacs-reader-wendian--finish file index problem)))

(defun emacs-reader-wendian--load (url file)
  "Load FILE from disk, or fetch the trusted index at URL asynchronously."
  (let ((cached (and (file-readable-p file)
                     (ignore-errors
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally file)
                         (emacs-reader-wendian--prepare))))))
    (if cached (emacs-reader-wendian--finish file cached nil)
      (condition-case err
          (let* ((url-request-method "GET") (url-request-data nil)
                 (url-request-extra-headers nil) (url-max-redirections 0)
                 finished buffer
                 (timeout
                  (run-at-time 45 nil
                               (lambda ()
                                 (unless finished
                                   (setq finished t)
                                   (when (buffer-live-p buffer) (kill-buffer buffer))
                                   (emacs-reader-wendian--finish file nil "问典索引下载超时"))))))
            (condition-case err
                (setq buffer
                      (url-retrieve url
                                    (lambda (status)
                                      (if finished (kill-buffer (current-buffer))
                                        (setq finished t)
                                        (cancel-timer timeout)
                                        (emacs-reader-wendian--receive status file))) nil t t))
              (error (cancel-timer timeout) (signal (car err) (cdr err)))))
        (error (emacs-reader-wendian--finish file nil (error-message-string err)))))))

;;;###autoload
(defun emacs-reader-wendian-search (source query callback &optional page)
  "Search raw Legado SOURCE for QUERY on one-based PAGE (default 1).
Call CALLBACK with a list of Legado plists and nil, or nil and an error.
Concurrent first searches share one download and each retains its own query."
  (let* ((url (plist-get source :searchUrl))
         (file (expand-file-name "wendian/index.json" emacs-reader-directory))
         (page (or page 1))
         (deliver (lambda (index problem)
                    (let (rows)
                      (when index
                        (condition-case err
                            (setq rows (emacs-reader-wendian--results source index query page))
                          (error (setq problem (error-message-string err)))))
                      (funcall callback rows problem)))))
    (cond ((not (equal url emacs-reader-wendian--index-url))
           (funcall callback nil "问典书源的 searchUrl 不是 LightRead 公开书目索引"))
          ((not (and (integerp page) (> page 0))) (funcall callback nil "无效的页码"))
          ((gethash file emacs-reader-wendian--indexes)
           (funcall deliver (gethash file emacs-reader-wendian--indexes) nil))
          (t (let ((waiting (gethash file emacs-reader-wendian--pending)))
               (puthash file (cons deliver waiting) emacs-reader-wendian--pending)
               (unless waiting (run-at-time 0 nil #'emacs-reader-wendian--load url file)))))))

(provide 'emacs-reader-wendian-index)
;;; emacs-reader-wendian-index.el ends here
