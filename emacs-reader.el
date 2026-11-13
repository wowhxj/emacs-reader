;;; emacs-reader.el --- Read books from a Reader server -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/wowhxj/emacs-reader
;; Keywords: multimedia, reading

;;; Commentary:

;; A personal client for hectorqin/reader.  The current implementation
;; provides Reader3 login (remembered via Customize and auth-source), a
;; recent-first shelf, and read-only text-chapter navigation.  Progress
;; synchronization remains intentionally unimplemented.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'hl-line)
(require 'json)
(require 'shr)
(require 'tabulated-list)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-parse)

(defvar url-http-response-status nil)
(defvar url-http-content-type nil)
(defvar url-http-content-length nil)

(defgroup emacs-reader nil
  "Read books from a Reader server."
  :group 'applications)

(defcustom emacs-reader-server-url nil
  "Base URL of the Reader server, without credentials or query parameters."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'emacs-reader)

(defcustom emacs-reader-username nil
  "Username used to look up the Reader password in `auth-source'."
  :type '(choice (const :tag "Prompt or configure later" nil) string)
  :group 'emacs-reader)

(defcustom emacs-reader-allow-insecure-http t
  "Whether to allow plain HTTP for a self-hosted Reader server.

Defaults to non-nil.  Plain HTTP exposes credentials and session tokens
to network observers."
  :type 'boolean
  :group 'emacs-reader
  :safe #'booleanp)

(defcustom emacs-reader-max-response-bytes (* 4 1024 1024)
  "Maximum response size accepted by the JSON API transport."
  :type 'integer
  :group 'emacs-reader
  :safe #'integerp)

(defcustom emacs-reader-text-width 80
  "Width, in columns, of the centered text column in reading buffers.
A CJK character occupies two columns."
  :type 'integer
  :group 'emacs-reader
  :safe #'integerp)

(defcustom emacs-reader-tts-voice "晓晓"
  "Name of the Reader server's HttpTTS configuration to use."
  :type 'string :group 'emacs-reader)

(defcustom emacs-reader-tts-rate 1.0
  "Reader speech rate, normally between 0.5 and 2.0."
  :type 'number :group 'emacs-reader)

(defface emacs-reader-tts-paragraph '((t :inherit highlight))
  "Face of the paragraph currently being read aloud."
  :group 'emacs-reader)

(defvar-local emacs-reader--tts-active nil)
(defvar-local emacs-reader--tts-generation 0)
(defvar-local emacs-reader--tts-process nil)
(defvar-local emacs-reader--tts-file nil)
(defvar-local emacs-reader--tts-overlay nil)
(defvar-local emacs-reader--tts-request-buffer nil)

(defface emacs-reader-shelf-title '((t :inherit bold :height 1.15))
  "Face for book titles on the Reader shelf."
  :group 'emacs-reader)

(defface emacs-reader-shelf-meta '((t :inherit shadow))
  "Face for secondary book details on the Reader shelf."
  :group 'emacs-reader)

(defface emacs-reader-shelf-progress '((t :inherit success))
  "Face for the filled part of a shelf progress bar."
  :group 'emacs-reader)

(defconst emacs-reader--api-prefix "/api/v1/"
  "Versioned Reader API prefix.")

(defconst emacs-reader--reader3-prefix "/reader3/"
  "Legacy Reader3 API prefix used by the deployed web client.")

(defvar emacs-reader--token nil
  "In-memory Reader access token; never persisted by this package.")

(defvar emacs-reader--session-cookie nil
  "In-memory Reader3 session-cookie value; never persisted by this package.")

(defvar-local emacs-reader--transport-error nil
  "Non-nil when the HTTP response exceeded its configured bound.")

(defvar-local emacs-reader--shelf-books nil
  "Books currently displayed in the Reader shelf buffer.")

(defvar-local emacs-reader--current-book nil
  "Book record associated with a Reader reading buffer.")

(defvar-local emacs-reader--current-chapter-index nil
  "Zero-based chapter index currently displayed.")

(defvar-local emacs-reader--requested-chapter-index nil
  "Zero-based chapter index most recently requested.")

(defvar-local emacs-reader--chapter-request-id 0
  "Monotonic identifier used to ignore stale chapter responses.")

(defvar-local emacs-reader--chapters nil
  "Vector of chapter records indexed by their zero-based chapter index.")

(defvar-local emacs-reader--catalog-loading nil
  "Non-nil while a chapter catalog is being fetched.")

(defvar-local emacs-reader--catalog-show-pending nil
  "Non-nil when the catalog should be shown after loading.")

(defvar-local emacs-reader--toc-reader-buffer nil
  "Reading buffer to which this table of contents belongs.")

(defun emacs-reader--parsed-server-url ()
  "Return the validated parsed `emacs-reader-server-url'."
  (unless (and (stringp emacs-reader-server-url)
               (not (string-empty-p emacs-reader-server-url)))
    (user-error "Set `emacs-reader-server-url' first"))
  (when (string-match-p "[?#]" emacs-reader-server-url)
    (user-error "Reader server URL must not include a query or fragment"))
  (let* ((url (url-generic-parse-url emacs-reader-server-url))
         (scheme (url-type url)))
    (unless (and (url-host url)
                 (member scheme '("https" "http")))
      (user-error "Reader server URL must use HTTP or HTTPS and include a host"))
    (when (or (url-user url) (url-password url))
      (user-error "Do not put credentials in `emacs-reader-server-url'"))
    (when (and (string= scheme "http")
               (not emacs-reader-allow-insecure-http))
      (user-error "Plain HTTP is disabled; explicitly opt in only for a trusted self-hosted server"))
    (let ((path (url-filename url)))
      (when (or (string-match-p "\\\\" path)
                (string-match-p "\\(?:^\\|/\\)\\.\\.?\\(?:/\\|$\\)" path)
                (string-match-p "\\(?:%2e\\|%2f\\|%5c\\)" path))
        (user-error "Reader server URL contains an unsafe path")))
    url))

(defun emacs-reader--api-url (path)
  "Return the validated URL for API PATH.
PATH must be below a supported Reader API prefix."
  (unless (and (stringp path)
               (or (string-prefix-p emacs-reader--api-prefix path)
                   (string-prefix-p emacs-reader--reader3-prefix path))
               (not (string-match-p "[?#]" path))
               (not (string-match-p "\\(?:^\\|/\\)\\.\\.?\\(?:/\\|$\\)" path))
               (not (string-match-p "\\(?:%2e\\|%2f\\|%5c\\)" path)))
    (user-error "Invalid Reader API path"))
  (let* ((url (emacs-reader--parsed-server-url))
         (base-path (string-remove-suffix "/" (or (url-filename url) "/"))))
    (setf (url-filename url) (concat base-path path)
          (url-target url) nil)
    (url-recreate-url url)))

(defun emacs-reader--auth-spec ()
  "Return the auth-source :host/:port/:user spec for the configured account."
  (let ((url (emacs-reader--parsed-server-url)))
    (list :host (url-host url)
          :port (number-to-string (url-port url))
          :user emacs-reader-username)))

(defun emacs-reader--save-password (password)
  "Offer to store PASSWORD in auth-source unless it is already stored there."
  (let ((stored (emacs-reader--auth-source-password)))
    (unwind-protect
        (cond
         ((equal stored password))
         (stored
          (message "auth-source holds a different Reader password; update it manually"))
         (t
          (let ((save (plist-get (car (apply #'auth-source-search
                                             :create t :max 1 :secret password
                                             (emacs-reader--auth-spec)))
                                 :save-function)))
            (when (functionp save)
              (funcall save)
              (apply #'auth-source-forget+ (emacs-reader--auth-spec))))))
      (when stored (clear-string stored)))))

(defun emacs-reader--auth-source-password ()
  "Return a copy of the configured password, or nil if none is available.
The caller must clear the returned string after use."
  (unless (and (stringp emacs-reader-username)
               (not (string-empty-p emacs-reader-username)))
    (user-error "Set `emacs-reader-username' to look up its password"))
  (let* ((entries (apply #'auth-source-search
                         :require '(:secret) :max 1
                         (emacs-reader--auth-spec)))
         (secret (plist-get (car entries) :secret))
         (password (if (functionp secret) (funcall secret) secret)))
    (and (stringp password) (copy-sequence password))))

(defun emacs-reader--json-response (status)
  "Normalize a JSON response and STATUS without retaining its body."
  (emacs-reader--http-response status))

(defun emacs-reader--http-response (status &optional html-p)
  "Normalize the current URL response and STATUS.
When HTML-P is non-nil, accept HTML instead of JSON and return decoded text."
  (let ((response-status url-http-response-status)
        (content-type url-http-content-type)
        (content-length url-http-content-length))
    (cond
     (emacs-reader--transport-error
      (list :error 'response-too-large))
     ((or (plist-get status :redirect)
          (let ((error-data (plist-get status :error)))
            (and (consp error-data)
                 (eq (cadr error-data) 'http-redirect-limit))))
      (list :error 'redirect :status response-status))
     ((plist-get status :error)
      (list :error 'transport))
     ((not (and (integerp response-status)
                (<= 200 response-status)
                (< response-status 300)))
      (list :error (cond ((= (or response-status 0) 401) 'authentication)
                         ((= (or response-status 0) 429) 'rate-limit)
                         ((and response-status (>= response-status 500)) 'server)
                         ((and response-status (>= response-status 300)) 'http)
                         (t 'transport))
            :status response-status))
     ((memq response-status '(204 205))
      (list :ok nil :status response-status))
     ((and content-length (> content-length emacs-reader-max-response-bytes))
      (list :error 'response-too-large :status response-status))
     ((not (and (stringp content-type)
                (string-match-p
                 (if html-p
                     "\\`\\(?:text/html\\|application/xhtml\\+xml\\)\\(?:[;[:space:]]\\|\\'\\)"
                   "\\`application/\\(?:[[:alnum:].+-]+\\+\\)?json\\(?:[;[:space:]]\\|\\'\\)")
                 (downcase content-type))))
      (list :error 'unexpected-content-type :status response-status))
     (t
      (let ((separator (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward "\r?\n\r?\n" nil t)
                           (point))))
            json)
        (if (not separator)
            (list :error 'invalid-response :status response-status)
          (let ((body (buffer-substring-no-properties separator (point-max))))
            (cond
             ((> (string-bytes body) emacs-reader-max-response-bytes)
              (list :error 'response-too-large :status response-status))
             (html-p
              (list :ok (decode-coding-string body 'utf-8) :status response-status))
             (t
              (condition-case nil
                  (setq json
                        (json-parse-string
                         (decode-coding-string body 'utf-8)
                         :object-type 'alist :array-type 'list
                         :null-object nil :false-object :false))
                (json-parse-error
                 (setq json :invalid-json)))
              (if (eq json :invalid-json)
                  (list :error 'invalid-json :status response-status)
                (list :ok json :status response-status)))))))))))

(defun emacs-reader--response-session-cookie ()
  "Return the Reader3 session-cookie value from the current response, if any.
The result is (t . VALUE), allowing an empty VALUE to clear the session."
  (let ((case-fold-search t)
        (found nil)
        value
        (header-end (save-excursion
                      (goto-char (point-min))
                      (if (re-search-forward "\\r?\\n\\r?\\n" nil t)
                          (match-beginning 0)
                        (point-max)))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^Set-Cookie:[ \t]*reader\\.session=\\([^; \t\\r\\n]*\\)"
              header-end t)
        (setq found t
              value (match-string-no-properties 1)))
      (and found (cons t value)))))

(defun emacs-reader--url-callback (status callback &optional html-p)
  "Normalize STATUS, capture the session cookie, invoke CALLBACK, and clean up.
HTML-P selects an HTML response instead of the default JSON response."
  (let ((buffer (current-buffer))
        (cookie (emacs-reader--response-session-cookie)))
    (unwind-protect
        (progn
          (when cookie
            (when (stringp emacs-reader--session-cookie)
              (clear-string emacs-reader--session-cookie))
            (setq emacs-reader--session-cookie
                  (let ((value (cdr cookie)))
                    (and (not (string-empty-p value))
                         (string-match-p "\\`[A-Za-z0-9._~%:+/=-]+\\'" value)
                         value))))
          (funcall callback (if html-p
                                (emacs-reader--http-response status t)
                              (emacs-reader--json-response status))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun emacs-reader--url-transport (request callback)
  "Send REQUEST asynchronously with `url-retrieve' and call CALLBACK.
REQUEST contains :url, :method, :headers, optional :data, and :html for HTML."
  (let* ((url-request-method (plist-get request :method))
         (url-request-data (plist-get request :data))
         (url-request-extra-headers (plist-get request :headers))
         (url-user-agent "emacs-reader/0.1")
         (url-privacy-level 'high)
         (url-max-redirections 0)
         (url-mime-encoding-string nil)
         (url-history-track nil)
         (buffer (url-retrieve (plist-get request :url)
                               #'emacs-reader--url-callback
                               (list callback (plist-get request :html)) t t)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq emacs-reader--transport-error nil))
      (let ((process (get-buffer-process buffer)))
        (when process
          (let ((filter (process-filter process))
                (received 0)
                (wire-limit (+ emacs-reader-max-response-bytes 65536)))
            (set-process-filter
             process
             (lambda (proc data)
               (setq received (+ received (string-bytes data)))
               (if (> received wire-limit)
                   (progn
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (setq emacs-reader--transport-error t)))
                     (delete-process proc))
                 (funcall filter proc data)))))))
    buffer)))

(defun emacs-reader--api-request (path method data callback &optional token)
  "Request API PATH using METHOD and JSON DATA; call CALLBACK with a result.
TOKEN, when non-nil, is sent only to the configured server origin."
  (unless (member method '("GET" "POST" "PUT" "DELETE"))
    (user-error "Unsupported Reader API method"))
  (when (and token
             (not (and (stringp token)
                       (string-match-p "\\`[A-Za-z0-9._~+/=-]+\\'" token))))
    (user-error "Invalid Reader bearer token"))
  (unless (and (integerp emacs-reader-max-response-bytes)
               (> emacs-reader-max-response-bytes 0))
    (user-error "`emacs-reader-max-response-bytes' must be positive"))
  (let* ((headers `(("Accept" . "application/json")
                    ,@(when data '(("Content-Type" . "application/json")))
                    ,@(when (and emacs-reader--session-cookie
                                 (string-prefix-p emacs-reader--reader3-prefix path))
                        `(("Cookie" . ,(concat "reader.session="
                                               emacs-reader--session-cookie))))
                    ,@(when token
                        `(("Authorization" . ,(concat "Bearer " token))))))
         (request `(:url ,(emacs-reader--api-url path)
                    :method ,method
                    :headers ,headers
                    :data ,(when data
                             (encode-coding-string
                              (json-serialize data) 'utf-8)))))
    (emacs-reader--url-transport request callback)))

(defun emacs-reader--clear-session ()
  "Forget the in-memory Reader token and session cookie."
  (setq emacs-reader--token nil)
  (when (stringp emacs-reader--session-cookie)
    (clear-string emacs-reader--session-cookie))
  (setq emacs-reader--session-cookie nil))

(defun emacs-reader--remember (symbol value)
  "Set SYMBOL to VALUE, saving it with Customize unless already saved."
  (unless (equal value (ignore-errors (eval (car (get symbol 'saved-value)) t)))
    (customize-save-variable symbol value)))

(defun emacs-reader-login (server-url username)
  "Log in to Reader at SERVER-URL as USERNAME, prompting for the password.
SERVER-URL and USERNAME are saved with Customize when they change.  After a
successful login, Emacs offers to store the password in auth-source so that
later sessions log in automatically."
  (interactive
   (list (read-string "Reader server URL: " emacs-reader-server-url)
         (read-string "Reader username: " emacs-reader-username)))
  (emacs-reader--prompt-login server-url username #'ignore))

(defun emacs-reader--prompt-login (server-url username callback)
  "Validate and save SERVER-URL and USERNAME, read a password, and log in.
CALLBACK receives non-nil on success."
  (when (string-empty-p username)
    (user-error "Reader username must not be empty"))
  (let ((emacs-reader-server-url server-url))
    (emacs-reader--parsed-server-url))
  (emacs-reader--remember 'emacs-reader-server-url server-url)
  (emacs-reader--remember 'emacs-reader-username username)
  (emacs-reader--login (read-passwd "Reader password: ") t callback))

(defun emacs-reader--login (password save callback)
  "Log in as `emacs-reader-username' with PASSWORD; call CALLBACK with success.
When SAVE is non-nil, offer to store a working PASSWORD in auth-source.
PASSWORD is cleared once the request finishes.  It is sent only in the
request body."
  (emacs-reader--clear-session)
  (condition-case err
      (emacs-reader--api-request
       (concat emacs-reader--reader3-prefix "login") "POST"
       `((username . ,emacs-reader-username)
         (password . ,password)
         (code . "")
         (isLogin . t))
       (lambda (response)
         (unwind-protect
             (let ((ok (emacs-reader--handle-login-response response)))
               (when (and ok save)
                 (emacs-reader--save-password password))
               (funcall callback ok))
           (clear-string password))))
    (error (clear-string password)
           (signal (car err) (cdr err)))))

(defun emacs-reader--handle-login-response (response)
  "Store the session from Reader3 RESPONSE; return non-nil on success."
  (let* ((json (plist-get response :ok))
         (data (and (listp json) (alist-get 'data json)))
         (token (and (listp data) (alist-get 'accessToken data))))
    (if (and (listp json)
             (eq (alist-get 'isSuccess json) t))
        (progn
          (setq emacs-reader--token
                (and (stringp token) (not (string-empty-p token)) token))
          (message "Reader login succeeded")
          t)
      (emacs-reader--clear-session)
      (message "Reader login failed; no session was retained")
      nil)))

(defun emacs-reader--ensure-session (callback)
  "Call CALLBACK with non-nil once a Reader session exists.
Logs in silently with the auth-source password when one is stored;
otherwise prompts for the missing settings and password."
  (if emacs-reader--session-cookie
      (funcall callback t)
    (let ((password (and (stringp emacs-reader-username)
                         (not (string-empty-p emacs-reader-username))
                         (stringp emacs-reader-server-url)
                         (emacs-reader--auth-source-password))))
      (if password
          (emacs-reader--login password nil callback)
        (emacs-reader--prompt-login
         (or emacs-reader-server-url (read-string "Reader server URL: "))
         (or emacs-reader-username (read-string "Reader username: "))
         callback)))))

(defun emacs-reader--need-login-p (response)
  "Return non-nil when RESPONSE says the Reader session has expired."
  (let ((json (plist-get response :ok)))
    (and (listp json) (equal (alist-get 'data json) "NEED_LOGIN"))))

(defun emacs-reader--reader3-request (name method data callback &optional retried)
  "Send Reader3 API NAME with METHOD and DATA, then call CALLBACK.
Logs in first when there is no session, and once more on NEED_LOGIN unless
RETRIED is non-nil."
  (emacs-reader--ensure-session
   (lambda (ok)
     (if (not ok)
         (funcall callback '(:error authentication))
       (emacs-reader--api-request
        (concat emacs-reader--reader3-prefix name) method data
        (lambda (response)
          (if (and (not retried) (emacs-reader--need-login-p response))
              (progn
                (emacs-reader--clear-session)
                (emacs-reader--reader3-request name method data callback t))
            (funcall callback response))))))))

(defun emacs-reader--book-time (book)
  "Return BOOK's last-read timestamp as a number, or zero if unavailable."
  (let ((time (alist-get 'durChapterTime book)))
    (cond ((numberp time) time)
          ((stringp time) (string-to-number time))
          (t 0))))

(defun emacs-reader--sort-books-by-recent (books)
  "Return a copy of BOOKS sorted by last-read time, newest first."
  (sort (copy-sequence books)
        (lambda (left right)
          (> (emacs-reader--book-time left)
             (emacs-reader--book-time right)))))

(defun emacs-reader--book-field (book field)
  "Return FIELD from BOOK when it is a string, otherwise an empty string."
  (let ((value (alist-get field book)))
    (if (stringp value) value "")))

(defun emacs-reader--book-source (book)
  "Return BOOK's human-readable source name."
  (let ((name (emacs-reader--book-field book 'originName)))
    (when (string-empty-p name)
      (setq name (emacs-reader--book-field book 'origin)))
    (if (string-prefix-p "storage/" name)
        (concat "Local " (upcase (or (file-name-extension name) "file")))
      name)))

(defun emacs-reader--book-progress (book)
  "Return BOOK's shelf progress line: bar, percentage, chapter, title."
  (let* ((chapter (alist-get 'durChapterIndex book))
         (total (alist-get 'totalChapterNum book))
         (title (emacs-reader--book-field book 'durChapterTitle))
         (known (and (integerp chapter) (integerp total) (> total 0)))
         (ratio (if known (/ (min total (1+ (max 0 chapter))) (float total)) 0))
         (filled (round (* 12 ratio))))
    (concat (propertize (make-string filled ?━) 'face 'emacs-reader-shelf-progress)
            (propertize (make-string (- 12 filled) ?─) 'face 'emacs-reader-shelf-meta)
            (if known
                (format " %3d%%  %d/%d  " (round (* 100 ratio)) (1+ (max 0 chapter)) total)
              "   —  ")
            title)))

(defun emacs-reader--shelf-entry (book)
  "Return BOOK rendered as a two-line shelf entry carrying BOOK as a property."
  (let ((author (emacs-reader--book-field book 'author))
        (source (emacs-reader--book-source book))
        (name (emacs-reader--book-field book 'name)))
    (propertize
     (concat "  " (propertize (if (string-empty-p name) "Untitled" name)
                              'face 'emacs-reader-shelf-title)
             (propertize (concat "    "
                                 (mapconcat #'identity
                                            (seq-remove #'string-empty-p
                                                        (list author source))
                                            "  ·  "))
                         'face 'emacs-reader-shelf-meta)
             "\n  " (emacs-reader--book-progress book) "\n")
     'emacs-reader-book book)))

(define-derived-mode emacs-reader-shelf-mode special-mode "Reader-Shelf"
  "Major mode for the Reader bookshelf."
  (setq-local truncate-lines t)
  (setq-local line-spacing 0.2)
  (setq header-line-format
        (propertize "  Bookshelf   RET open   g refresh   n/p move"
                    'face 'emacs-reader-shelf-meta)))

(define-key emacs-reader-shelf-mode-map (kbd "g") #'emacs-reader-shelf-refresh)
(define-key emacs-reader-shelf-mode-map (kbd "RET") #'emacs-reader-open-book)
(define-key emacs-reader-shelf-mode-map (kbd "n") #'emacs-reader-shelf-next)
(define-key emacs-reader-shelf-mode-map (kbd "p") #'emacs-reader-shelf-previous)

(defun emacs-reader-shelf-next (&optional n)
  "Move to the Nth next book on the shelf."
  (interactive "p")
  ;; ponytail: entries are fixed at 3 lines (2 text + blank); revisit if layout changes.
  (let ((pos (save-excursion (forward-line (* 3 (or n 1))) (point))))
    (when (get-text-property pos 'emacs-reader-book)
      (goto-char pos))))

(defun emacs-reader-shelf-previous (&optional n)
  "Move to the Nth previous book on the shelf."
  (interactive "p")
  (emacs-reader-shelf-next (- (or n 1))))

(defun emacs-reader-shelf ()
  "Fetch and display the Reader3 bookshelf, newest reading activity first."
  (interactive)
  (let ((buffer (get-buffer-create "*Reader Shelf*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'emacs-reader-shelf-mode)
        (emacs-reader-shelf-mode)))
    (pop-to-buffer buffer)
    (emacs-reader-shelf-refresh)))

(defun emacs-reader-shelf-refresh ()
  "Refresh the current Reader shelf buffer."
  (interactive)
  (unless (derived-mode-p 'emacs-reader-shelf-mode)
    (user-error "Not in a Reader shelf buffer"))
  (let ((buffer (current-buffer)))
    (setq mode-line-process " Loading...")
    (emacs-reader--reader3-request
     "getBookshelf" "GET" nil
     (lambda (response)
       (emacs-reader--handle-bookshelf-response buffer response)))))

(defun emacs-reader--handle-bookshelf-response (buffer response)
  "Render RESPONSE in BUFFER, or report a generic failure."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq mode-line-process nil)
      (let* ((json (plist-get response :ok))
             (books (and (listp json)
                         (eq (alist-get 'isSuccess json) t)
                         (alist-get 'data json))))
        (if (and (listp json) (eq (alist-get 'isSuccess json) t)
                 (or (null books) (listp books)))
            (let ((inhibit-read-only t)
                  (line (line-number-at-pos)))
              (setq emacs-reader--shelf-books
                    (emacs-reader--sort-books-by-recent books))
              (erase-buffer)
              (insert "\n")
              (if books
                  (insert (mapconcat #'emacs-reader--shelf-entry
                                     emacs-reader--shelf-books "\n"))
                (insert (propertize "  The shelf is empty.\n"
                                    'face 'emacs-reader-shelf-meta)))
              (goto-char (point-min))
              (forward-line (max 1 (1- line)))
              (message "Loaded %d Reader books" (length books)))
          (message "Reader shelf unavailable; run `emacs-reader-login' if needed"))))))

(defun emacs-reader--selected-shelf-book ()
  "Return the book at point in a Reader shelf buffer."
  (unless (derived-mode-p 'emacs-reader-shelf-mode)
    (user-error "Select a book from the Reader shelf first"))
  (or (get-text-property (point) 'emacs-reader-book)
      (user-error "No book on this shelf row")))

(defun emacs-reader-open-book (book)
  "Open BOOK in a native text-reading buffer.
This command only reads chapter content; it does not write reading progress."
  (interactive (list (emacs-reader--selected-shelf-book)))
  (let ((book-url (alist-get 'bookUrl book))
        (name (emacs-reader--book-field book 'name)))
    (unless (and (stringp book-url) (not (string-empty-p book-url)))
      (user-error "This shelf item has no readable book URL"))
    (let* ((buffer (generate-new-buffer
                    (format "*Reader: %s*"
                            (if (string-empty-p name) "Book" name))))
           (chapter (max 0 (or (and (integerp (alist-get 'durChapterIndex book))
                                   (alist-get 'durChapterIndex book))
                              0))))
      (with-current-buffer buffer
        (emacs-reader-reading-mode)
        (setq-local emacs-reader--current-book book))
      (pop-to-buffer buffer)
      (emacs-reader--load-chapter chapter)
      (emacs-reader--load-catalog))))

(define-derived-mode emacs-reader-reading-mode special-mode "Reader"
  "Major mode for reading text chapters from a Reader server."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  ;; Break between any CJK characters, honoring kinsoku punctuation rules.
  (setq-local word-wrap-by-category t)
  ;; U+3000 paragraph indents would otherwise get the underlined `nobreak-space' face.
  (setq-local nobreak-char-display nil)
  (setq-local line-spacing 0.4)
  (setq-local scroll-conservatively 0)
  (add-hook 'window-configuration-change-hook #'emacs-reader--fit-text-width nil t)
  (add-hook 'kill-buffer-hook #'emacs-reader--tts-stop nil t)
  (add-hook 'change-major-mode-hook #'emacs-reader--tts-stop nil t))

(defun emacs-reader--fit-text-width ()
  "Center an `emacs-reader-text-width' column in windows showing this buffer."
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (let ((margin (max 0 (/ (- (window-total-width window) emacs-reader-text-width 2)
                            2))))
      (set-window-margins window margin margin))))

(defun emacs-reader--format-chapter (content)
  "Return CONTENT as indented paragraphs separated by blank lines.
Server lines broken mid-sentence are joined; surrounding spaces, including
full-width U+3000 indents, are normalized."
  (let (paragraphs)
    (dolist (line (split-string content "\r?\n" t "[ \t\u3000\u00a0]+"))
      ;; ponytail: punctuation heuristic; a title line without punctuation merges too.
      (if (and paragraphs
               (not (string-match-p "[。！？!?…”’」』）)]\\'" (car paragraphs))))
          (setcar paragraphs (concat (car paragraphs) line))
        (push line paragraphs)))
    (mapconcat (lambda (paragraph) (concat "\u3000\u3000" paragraph))
               (nreverse paragraphs) "\n\n")))

(defun emacs-reader-scroll-half-up ()
  "Scroll the text up by half a window."
  (interactive)
  (scroll-up-command (max 1 (/ (window-body-height) 2))))

(defun emacs-reader-scroll-half-down ()
  "Scroll the text down by half a window."
  (interactive)
  (scroll-down-command (max 1 (/ (window-body-height) 2))))

(defun emacs-reader-scroll-forward ()
  "Scroll one screen, or open the next chapter when the end is visible."
  (interactive)
  (cond
   ((and emacs-reader--requested-chapter-index
         (not (equal emacs-reader--requested-chapter-index emacs-reader--current-chapter-index)))
    (message "正在加载章节，请稍候"))
   ((not (integerp emacs-reader--current-chapter-index))
    (user-error "正文尚未加载，请从目录选择章节"))
   ((pos-visible-in-window-p (point-max) nil t)
    (emacs-reader-next-chapter))
   (t
    (condition-case nil
        (scroll-up-command)
      (end-of-buffer (emacs-reader-next-chapter))))))

(define-key emacs-reader-reading-mode-map (kbd "n") #'emacs-reader-scroll-forward)
(define-key emacs-reader-reading-mode-map (kbd "p") #'scroll-down-command)
(define-key emacs-reader-reading-mode-map (kbd "SPC") #'emacs-reader-scroll-forward)
(define-key emacs-reader-reading-mode-map (kbd "j") #'scroll-up-line)
(define-key emacs-reader-reading-mode-map (kbd "k") #'scroll-down-line)
(define-key emacs-reader-reading-mode-map (kbd "d") #'emacs-reader-scroll-half-up)
(define-key emacs-reader-reading-mode-map (kbd "u") #'emacs-reader-scroll-half-down)
(define-key emacs-reader-reading-mode-map (kbd "]") #'emacs-reader-next-chapter)
(define-key emacs-reader-reading-mode-map (kbd "[") #'emacs-reader-previous-chapter)
(define-key emacs-reader-reading-mode-map (kbd "c") #'emacs-reader-table-of-contents)
(define-key emacs-reader-reading-mode-map (kbd "C") #'emacs-reader-select-chapter)
(define-key emacs-reader-reading-mode-map (kbd "r") #'emacs-reader-toggle-read-aloud)

(defun emacs-reader--tts-stop ()
  "Stop playback and discard pending speech in the current reading buffer."
  (setq emacs-reader--tts-active nil
        emacs-reader--tts-generation (1+ emacs-reader--tts-generation))
  (when (process-live-p emacs-reader--tts-process)
    (delete-process emacs-reader--tts-process))
  (setq emacs-reader--tts-process nil)
  (when (buffer-live-p emacs-reader--tts-request-buffer)
    (let ((process (get-buffer-process emacs-reader--tts-request-buffer)))
      (when (process-live-p process) (delete-process process)))
    (when (buffer-live-p emacs-reader--tts-request-buffer)
      (kill-buffer emacs-reader--tts-request-buffer)))
  (setq emacs-reader--tts-request-buffer nil)
  (when (and emacs-reader--tts-file (file-exists-p emacs-reader--tts-file))
    (delete-file emacs-reader--tts-file))
  (setq emacs-reader--tts-file nil)
  (when (overlayp emacs-reader--tts-overlay) (delete-overlay emacs-reader--tts-overlay))
  (setq emacs-reader--tts-overlay nil)
  (force-mode-line-update))

(defun emacs-reader--tts-current-p (buffer generation)
  "Whether BUFFER still owns speech GENERATION."
  (and (buffer-live-p buffer)
       (buffer-local-value 'emacs-reader--tts-active buffer)
       (= generation (buffer-local-value 'emacs-reader--tts-generation buffer))))

(defun emacs-reader--tts-player ()
  "Return a local audio player's command without shell expansion."
  (cond ((executable-find "afplay") (list (executable-find "afplay")))
        ((executable-find "mpv") (list (executable-find "mpv") "--no-video" "--really-quiet"))
        ((executable-find "ffplay")
         (list (executable-find "ffplay") "-nodisp" "-autoexit" "-loglevel" "quiet"))
        (t (user-error "朗读需要 afplay、mpv 或 ffplay"))))

(defun emacs-reader-toggle-read-aloud ()
  "Toggle paragraph-by-paragraph speech using the Reader server's HttpTTS."
  (interactive)
  (unless (derived-mode-p 'emacs-reader-reading-mode)
    (user-error "请在阅读页面使用朗读"))
  (if emacs-reader--tts-active
      (progn (emacs-reader--tts-stop) (message "朗读已停止"))
    (unless (and (integerp emacs-reader--current-chapter-index)
                 (equal emacs-reader--current-chapter-index emacs-reader--requested-chapter-index))
      (user-error "请等待章节加载完成"))
    (emacs-reader--tts-player)
    (emacs-reader--tts-stop)
    (setq emacs-reader--tts-active t)
    (emacs-reader--tts-read-paragraph (point))))

(defun emacs-reader--tts-read-paragraph (position)
  "Read the paragraph at POSITION, continuing until this chapter ends."
  (save-excursion
    (goto-char position)
    (skip-chars-forward " \t\r\n　 ")
    (if (eobp)
        (progn (emacs-reader--tts-stop) (message "本章朗读完毕"))
      (let* ((buffer (current-buffer))
             (generation emacs-reader--tts-generation)
             (separator "\n[ \t　 ]*\n")
             (start (save-excursion
                      (if (re-search-backward separator nil t) (match-end 0) (point-min))))
             (end (save-excursion
                    (if (re-search-forward separator nil t) (match-beginning 0) (point-max))))
             (text (string-trim (buffer-substring-no-properties start end))))
        (message "正在获取朗读音频…")
        (emacs-reader--ensure-session
         (lambda (ok)
           (when (emacs-reader--tts-current-p buffer generation)
             (with-current-buffer buffer
               (if (not ok)
                   (progn (emacs-reader--tts-stop) (message "朗读登录失败"))
                 (condition-case nil
                     (setq emacs-reader--tts-request-buffer
                           (emacs-reader--reader3-request
                            "book/tts" "POST"
                            `((text . ,text) (type . "httpTTS")
                              (voice . ,emacs-reader-tts-voice)
                              (rate . ,(number-to-string emacs-reader-tts-rate))
                              (pitch . "1") (base64 . "1")
                              (accessToken . ,emacs-reader--token))
                            (lambda (response)
                              (emacs-reader--tts-play-response
                               buffer generation start end response))))
                   (error (emacs-reader--tts-stop) (message "朗读请求失败"))))))))))))

(defun emacs-reader--tts-play-response (buffer generation start end response)
  "Play RESPONSE for BUFFER's paragraph START to END if GENERATION is current."
  (when (emacs-reader--tts-current-p buffer generation)
    (with-current-buffer buffer
      (setq emacs-reader--tts-request-buffer nil)
      (condition-case nil
          (let* ((json (plist-get response :ok))
                 (data (and (listp json) (eq (alist-get 'isSuccess json) t)
                            (alist-get 'data json))))
            (unless (and (stringp data) (> (length data) 0)) (error "No audio"))
            (let ((audio (base64-decode-string data)))
              (unless (> (length audio) 0) (error "Empty audio"))
              (setq emacs-reader--tts-file (make-temp-file "emacs-reader-speech-" nil ".mp3"))
              (let ((coding-system-for-write 'no-conversion))
                (write-region audio nil emacs-reader--tts-file nil 'silent)))
            (when (overlayp emacs-reader--tts-overlay) (delete-overlay emacs-reader--tts-overlay))
            (setq emacs-reader--tts-overlay (make-overlay start end))
            (overlay-put emacs-reader--tts-overlay 'face 'emacs-reader-tts-paragraph)
            (dolist (window (get-buffer-window-list buffer nil t))
              (set-window-point window start)
              (unless (pos-visible-in-window-p start window)
                (set-window-start window start)))
            (setq emacs-reader--tts-process
                  (make-process
                   :name "emacs-reader-speech" :buffer nil :connection-type 'pipe :noquery t
                   :command (append (emacs-reader--tts-player) (list emacs-reader--tts-file))
                   :sentinel
                   (lambda (process _event)
                     (when (and (memq (process-status process) '(exit signal))
                                (emacs-reader--tts-current-p buffer generation))
                       (with-current-buffer buffer
                         (when (eq process emacs-reader--tts-process)
                           (setq emacs-reader--tts-process nil)
                           (if (= (process-exit-status process) 0)
                               (progn
                                 (when emacs-reader--tts-file (delete-file emacs-reader--tts-file))
                                 (setq emacs-reader--tts-file nil)
                                 (when (overlayp emacs-reader--tts-overlay)
                                   (delete-overlay emacs-reader--tts-overlay))
                                 (setq emacs-reader--tts-overlay nil)
                                 (emacs-reader--tts-read-paragraph end))
                             (emacs-reader--tts-stop)
                             (message "朗读播放失败，请检查播放器"))))))))
            (message "正在朗读；r 停止"))
        (error (emacs-reader--tts-stop) (message "朗读失败，请检查服务器 TTS 配置或重新登录"))))))

(defun emacs-reader--load-chapter (index)
  "Request zero-based chapter INDEX for the current Reader book."
  (unless (and (integerp index) (>= index 0))
    (user-error "Chapter index must be a nonnegative integer"))
  (let* ((book emacs-reader--current-book)
         (book-url (and book (alist-get 'bookUrl book)))
         (total (emacs-reader--chapter-count book))
         (buffer (current-buffer)))
    (unless (and (stringp book-url) (not (string-empty-p book-url)))
      (user-error "No Reader book is open"))
    (when (and (integerp total) (> total 0) (>= index total))
      (user-error "Chapter is outside this book's range"))
    (emacs-reader--tts-stop)
    (setq emacs-reader--chapter-request-id
          (1+ emacs-reader--chapter-request-id)
          emacs-reader--requested-chapter-index index
          mode-line-process " Loading...")
    (let ((request-id emacs-reader--chapter-request-id))
      (emacs-reader--reader3-request
       "getBookContent" "POST"
       `((url . ,book-url) (index . ,index))
       (lambda (response)
         (emacs-reader--handle-chapter-response
          buffer request-id index response))))))

(defun emacs-reader--chapter-count (book)
  "Return BOOK's chapter count, preferring the loaded catalog."
  (if emacs-reader--chapters (length emacs-reader--chapters)
    (alist-get 'totalChapterNum book)))

(defun emacs-reader--chapter-heading (book index)
  "Return BOOK's title, chapter name and chapter-based progress for INDEX."
  (let* ((chapter (and emacs-reader--chapters
                       (< index (length emacs-reader--chapters))
                       (aref emacs-reader--chapters index)))
         (saved-index (alist-get 'durChapterIndex book))
         (title (or (alist-get 'title chapter)
                    (and (integerp saved-index) (= index saved-index)
                         (emacs-reader--book-field book 'durChapterTitle))))
         (total (emacs-reader--chapter-count book)))
    (format "%s — %s%s"
            (let ((name (emacs-reader--book-field book 'name)))
              (if (string-empty-p name) "Book" name))
            (if (and (stringp title) (not (string-empty-p title))) title
              (format "第 %d 章" (1+ index)))
            (if (and (integerp total) (> total 0))
                (format " · 全书 %.1f%%（%d/%d章）"
                        (min 100.0 (/ (* 100.0 (1+ index)) total)) (1+ index) total)
              " · 全书进度未知"))))

(defun emacs-reader--update-reading-header ()
  "Update the header for the displayed chapter without interpreting percent signs."
  (when (integerp emacs-reader--current-chapter-index)
    (setq header-line-format
          (string-replace "%" "%%"
                          (emacs-reader--chapter-heading
                           emacs-reader--current-book emacs-reader--current-chapter-index)))
    (force-mode-line-update)))

(defun emacs-reader--load-catalog (&optional show)
  "Fetch the current book's catalog once; with SHOW, open it when ready."
  (if emacs-reader--chapters
      (when show (emacs-reader--show-catalog (current-buffer)))
    (setq emacs-reader--catalog-show-pending (or show emacs-reader--catalog-show-pending))
    (unless emacs-reader--catalog-loading
      (let ((buffer (current-buffer))
            (url (alist-get 'bookUrl emacs-reader--current-book)))
        (unless (and (stringp url) (not (string-empty-p url)))
          (user-error "No Reader book is open"))
        (setq emacs-reader--catalog-loading t)
        (emacs-reader--reader3-request
         "getChapterList" "POST" `((url . ,url) (refresh . 0))
         (lambda (response)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (and (derived-mode-p 'emacs-reader-reading-mode)
                          (equal url (alist-get 'bookUrl emacs-reader--current-book)))
                 (setq emacs-reader--catalog-loading nil)
                 (let* ((json (plist-get response :ok))
                        (chapters (and (listp json) (eq (alist-get 'isSuccess json) t)
                                       (alist-get 'data json)))
                        (show emacs-reader--catalog-show-pending))
                   (setq emacs-reader--catalog-show-pending nil)
                   (if (and (consp chapters)
                            (cl-loop for chapter in chapters for index from 0
                                     always (and (listp chapter)
                                                 (equal index (alist-get 'index chapter))
                                                 (stringp (alist-get 'title chapter)))))
                       (progn
                         (setq emacs-reader--chapters (vconcat chapters))
                         (emacs-reader--update-reading-header)
                         (when show (emacs-reader--show-catalog buffer)))
                     (message "Reader 目录加载失败，可按 c 重试"))))))))))))

(define-derived-mode emacs-reader-toc-mode tabulated-list-mode "Reader-TOC"
  "Table of contents: n/p select, RET reads a chapter, q returns."
  (setq tabulated-list-format [("状态" 8 nil) ("序号" 8 nil) ("章节名称" 0 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(define-key emacs-reader-toc-mode-map (kbd "n") #'next-line)
(define-key emacs-reader-toc-mode-map (kbd "p") #'previous-line)
(define-key emacs-reader-toc-mode-map (kbd "RET") #'emacs-reader-toc-open-chapter)

(defun emacs-reader-table-of-contents ()
  "Open the current book's table of contents, fetching it if necessary."
  (interactive)
  (unless (derived-mode-p 'emacs-reader-reading-mode)
    (user-error "No Reader book is open"))
  (emacs-reader--load-catalog t))

(defun emacs-reader--show-catalog (reader)
  "Display READER's cached catalog, selecting its current chapter."
  (let* ((book (buffer-local-value 'emacs-reader--current-book reader))
         (chapters (buffer-local-value 'emacs-reader--chapters reader))
         (index (buffer-local-value 'emacs-reader--current-chapter-index reader))
         (buffer (get-buffer-create
                  (format "*Reader 目录: %s*" (emacs-reader--book-field book 'name)))))
    (with-current-buffer buffer
      (emacs-reader-toc-mode)
      (setq emacs-reader--toc-reader-buffer reader
            tabulated-list-entries
            (mapcar (lambda (chapter)
                      (let* ((chapter-index (alist-get 'index chapter))
                             (current (equal chapter-index index))
                             (title (alist-get 'title chapter)))
                        (list chapter-index
                              (vector (if current
                                          (propertize "▶ 当前" 'face '(bold success)) "")
                                      (number-to-string (1+ chapter-index))
                                      (if current (propertize title 'face 'bold) title)))))
                    (append chapters nil)))
      (tabulated-list-print)
      (goto-char (point-min))
      (forward-line (or index 0)))
    (pop-to-buffer buffer)
    (message "目录：n/p 选择，RET 跳转，q 返回")))

(defun emacs-reader-toc-open-chapter ()
  "Read the chapter on the current table-of-contents row."
  (interactive)
  (let ((index (tabulated-list-get-id))
        (reader emacs-reader--toc-reader-buffer))
    (unless (integerp index) (user-error "请选择一个章节"))
    (unless (buffer-live-p reader) (user-error "阅读缓冲区已关闭"))
    (quit-window)
    (pop-to-buffer reader)
    (with-current-buffer reader (emacs-reader--load-chapter index))))

(defun emacs-reader--book-asset-url (path)
  "Return a same-origin URL for a server-supplied book asset PATH."
  (unless (and (stringp path) (string-prefix-p "/book-assets/" path)
               (not (string-match-p "[?#\\\\[:cntrl:]]" path))
               (not (string-match-p "\\(?:^\\|/\\)\\.\\.?\\(?:/\\|$\\)" path))
               (not (string-match-p
                     "%\\(?:2[eEfF]\\|5[cC]\\|25\\|0[[:xdigit:]]\\|1[[:xdigit:]]\\|7[fF]\\)"
                     path)))
    (user-error "Invalid Reader book asset path"))
  (let ((url (emacs-reader--parsed-server-url)))
    (setf (url-filename url) (url-encode-url path)
          (url-target url) nil)
    (url-recreate-url url)))

(defun emacs-reader--html-chapter-text (html)
  "Render HTML as paragraphs without loading images or other resources."
  (unless (libxml-available-p)
    (user-error "This Emacs needs libxml support to read EPUB chapters"))
  (with-temp-buffer
    (insert html)
    (let ((dom (libxml-parse-html-region (point-min) (point-max)))
          (shr-inhibit-images t)
          (shr-use-fonts nil)
          (shr-use-colors nil)
          (shr-width most-positive-fixnum)
          (shr-max-width nil))
      (erase-buffer)
      (shr-insert-document (car (dom-by-tag dom 'body)))
      ;; HTML already defines paragraph boundaries; do not merge headings or verse.
      (mapconcat
       (lambda (paragraph)
         (concat "　　" (string-trim paragraph "[ \t\n　 ]+" "[ \t\n　 ]+")))
       (split-string (buffer-substring-no-properties (point-min) (point-max))
                     "\n[ \t]*\n+" t) "\n\n"))))

(defun emacs-reader--chapter-request-current-p (buffer request-id)
  "Whether BUFFER still expects the chapter response for REQUEST-ID."
  (and (buffer-live-p buffer)
       (= request-id
          (buffer-local-value 'emacs-reader--chapter-request-id buffer))))

(defun emacs-reader--display-chapter (buffer request-id index content &optional html-p)
  "Display CONTENT for INDEX in BUFFER if REQUEST-ID is current.
With HTML-P, render HTML; otherwise normalize plain text.  Nil means failure."
  (when (emacs-reader--chapter-request-current-p buffer request-id)
    (with-current-buffer buffer
      (setq mode-line-process nil)
      (let ((text (and (stringp content)
                       (condition-case nil
                           (if html-p (emacs-reader--html-chapter-text content)
                             (emacs-reader--format-chapter content))
                         (error nil)))))
        (if (and text (not (string-blank-p text)))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "\n" text "\n")
              (goto-char (point-min))
              (setq emacs-reader--current-chapter-index index
                    emacs-reader--requested-chapter-index index)
              (emacs-reader--update-reading-header)
              (dolist (window (get-buffer-window-list buffer nil t))
                (set-window-start window (point-min))
                (set-window-point window (point-min)))
              (message "Loaded chapter %d" (1+ index)))
          (setq emacs-reader--requested-chapter-index emacs-reader--current-chapter-index)
          (message "Reader chapter unavailable; no progress was changed"))))))

(defun emacs-reader--handle-chapter-response (buffer request-id index response)
  "Display a chapter RESPONSE or fetch its EPUB resource if still current."
  (when (emacs-reader--chapter-request-current-p buffer request-id)
    (let* ((json (plist-get response :ok))
           (content (and (listp json) (eq (alist-get 'isSuccess json) t)
                         (alist-get 'data json)))
           (path (and (stringp content) (string-trim content))))
      (if (and path (string-prefix-p "/book-assets/" path))
          (condition-case nil
              (emacs-reader--url-transport
               (list :url (emacs-reader--book-asset-url path)
                     :method "GET" :html t
                     :headers `(("Accept" . "text/html, application/xhtml+xml")
                                ,@(when emacs-reader--session-cookie
                                    `(("Cookie" . ,(concat "reader.session="
                                                           emacs-reader--session-cookie))))))
               (lambda (asset-response)
                 (emacs-reader--display-chapter
                  buffer request-id index (plist-get asset-response :ok) t)))
            (error (emacs-reader--display-chapter buffer request-id index nil)))
        (emacs-reader--display-chapter buffer request-id index content)))))

(defun emacs-reader--chapter-navigation-base ()
  "Return the best zero-based chapter index for navigation."
  (or emacs-reader--requested-chapter-index
      emacs-reader--current-chapter-index
      0))

(defun emacs-reader-next-chapter ()
  "Load the next chapter of the current Reader book."
  (interactive)
  (let* ((book emacs-reader--current-book)
         (index (1+ (emacs-reader--chapter-navigation-base)))
         (total (emacs-reader--chapter-count book)))
    (when (and (integerp total) (> total 0) (>= index total))
      (user-error "已到最后一章"))
    (emacs-reader--load-chapter index)))

(defun emacs-reader-previous-chapter ()
  "Load the previous chapter of the current Reader book."
  (interactive)
  (let ((index (1- (emacs-reader--chapter-navigation-base))))
    (when (< index 0)
      (user-error "This is the first chapter"))
    (emacs-reader--load-chapter index)))

(defun emacs-reader-select-chapter ()
  "Select a chapter by its one-based number."
  (interactive)
  (let* ((book emacs-reader--current-book)
         (total (emacs-reader--chapter-count book))
         (current (1+ (emacs-reader--chapter-navigation-base)))
         (choice (read-number "Chapter number: " current)))
    (unless (and (integerp choice) (> choice 0)
                 (or (not (and (integerp total) (> total 0)))
                     (<= choice total)))
      (user-error "Chapter number is outside this book's range"))
    (emacs-reader--load-chapter (1- choice))))

(provide 'emacs-reader)

;;; emacs-reader.el ends here
