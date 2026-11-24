;;; emacs-reader.el --- Local bookshelf and native reader -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/wowhxj/emacs-reader
;; Keywords: multimedia, reading

;;; Commentary:

;; Local file, Legado and OPDS reading with optional Reader3 access.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'hl-line)
(require 'json)
(require 'shr)
(require 'tab-bar)
(require 'tabulated-list)
(require 'transient)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-parse)

(defvar url-http-response-status nil)
(defvar url-http-content-type nil)
(defvar url-http-content-length nil)
(defvar shr-fill-text) ; Available in Emacs 30 and later.

(defgroup emacs-reader nil
  "Browse book sources, manage a shelf and read books in Emacs."
  :group 'applications)

(require 'emacs-reader-filter)
(require 'emacs-reader-local)
(require 'emacs-reader-wendian-recovery)
(require 'emacs-reader-search-progress)

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

(defcustom emacs-reader-tts-url nil
  "Full HTTP(S) URL of an OpenAI-compatible speech endpoint, or nil.
With a URL, send paragraph text directly to the endpoint.
With nil or an empty string, use macOS say, espeak-ng, or espeak."
  :type '(choice (const :tag "Use native speech" nil) string) :group 'emacs-reader)

(defcustom emacs-reader-tts-model "tts-1"
  "Speech model ID accepted by the TTS service."
  :type 'string :group 'emacs-reader)

(defcustom emacs-reader-tts-voice "zf_001"
  "Voice ID accepted by the TTS service."
  :type 'string :group 'emacs-reader)

(defcustom emacs-reader-tts-native-voice nil
  "Voice for local speech, or nil to use the speech program's default.
For Mandarin, try Tingting with macOS say, or zh with espeak."
  :type '(choice (const :tag "System default" nil) string)
  :group 'emacs-reader)

(defcustom emacs-reader-tts-format 'wav
  "Audio format requested from the TTS service."
  :type '(choice (const wav) (const mp3)) :group 'emacs-reader)

(defcustom emacs-reader-tts-api-key-env nil
  "Environment variable holding the TTS Bearer token, or nil for no token."
  :type '(choice (const :tag "No token" nil) string) :group 'emacs-reader)

(defcustom emacs-reader-tts-player-command nil
  "Audio player executable and arguments, or nil for automatic selection.
For WSL without Linux audio, use e.g. (\"mpv.exe\" \"--no-video\")."
  :type '(choice (const :tag "Automatic" nil) (repeat string))
  :group 'emacs-reader)

(defcustom emacs-reader-tts-rate 1.0
  "Speech speed multiplier; supported values depend on the service."
  :type 'number :group 'emacs-reader)

(defface emacs-reader-tts-paragraph '((t :inherit highlight))
  "Face of the paragraph currently being read aloud."
  :group 'emacs-reader)

(defvar-local emacs-reader--tts-active nil)
(defvar-local emacs-reader--tts-generation 0)
(defvar-local emacs-reader--tts-process nil)
(defvar-local emacs-reader--tts-native-command nil
  "Native speech command for the current session, or nil for endpoint audio.")
(defvar-local emacs-reader--tts-file nil)
(defvar-local emacs-reader--tts-overlay nil)
(defvar-local emacs-reader--tts-request-buffer nil)
(defvar-local emacs-reader--tts-prefetch nil)
(defvar-local emacs-reader--tts-prefetch-buffer nil)
(defvar-local emacs-reader--tts-position nil
  "Start of the paragraph actually being spoken, independent of buffer point.")
(defvar-local emacs-reader--tts-follow-pending nil
  "Non-nil when returning to this buffer should restore the speech position.")

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

(put 'emacs-reader--chapter-request-id 'permanent-local t)

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

(defun emacs-reader--http-response (status &optional html-p audio-format)
  "Normalize the current URL response and STATUS.
HTML-P selects decoded HTML; AUDIO-FORMAT selects binary speech."
  (let* ((response-status url-http-response-status)
         (content-type url-http-content-type)
         (content-length url-http-content-length)
         (audio-type
          (when (and audio-format (stringp content-type))
            (pcase (string-trim (car (split-string (downcase content-type) ";")))
              ((or "audio/mpeg" "audio/mp3") 'mp3)
              ((or "audio/wav" "audio/x-wav" "audio/wave") 'wav)
              ("application/octet-stream" audio-format)))))
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
     ((not (if audio-format audio-type
             (and (stringp content-type)
                (string-match-p
                 (if html-p
                     "\\`\\(?:text/html\\|application/xhtml\\+xml\\)\\(?:[;[:space:]]\\|\\'\\)"
                   "\\`application/\\(?:[[:alnum:].+-]+\\+\\)?json\\(?:[;[:space:]]\\|\\'\\)")
                 (downcase content-type)))))
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
             (audio-format
              (if (and (not (multibyte-string-p body)) (> (length body) 0))
                  (list :ok body :format audio-type :status response-status)
                (list :error 'invalid-audio :status response-status)))
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

(defun emacs-reader--url-callback (status callback &optional html-p audio-format)
  "Normalize STATUS, invoke CALLBACK, and clean up the response buffer.
HTML-P selects HTML; AUDIO-FORMAT selects audio and skips Reader cookies."
  (let ((buffer (current-buffer))
        (cookie (unless audio-format (emacs-reader--response-session-cookie))))
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
          (funcall callback (emacs-reader--http-response status html-p audio-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun emacs-reader--url-transport (request callback)
  "Send REQUEST asynchronously with `url-retrieve' and call CALLBACK.
REQUEST contains :url, :method, :headers, optional :data, and either
:html for HTML or :audio-format for audio."
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
                               (list callback (plist-get request :html)
                                     (plist-get request :audio-format)) t t)))
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

(defun emacs-reader--shelf-text (text)
  "Return TEXT as a single line for bookshelf display only."
  (string-trim (replace-regexp-in-string "[[:space:]\u2028\u2029]+" " " text)))

(defun emacs-reader--book-progress (book)
  "Return BOOK's shelf progress line: bar, percentage, chapter, title."
  (let* ((chapter (alist-get 'durChapterIndex book))
         (total (alist-get 'totalChapterNum book))
         (title (emacs-reader--shelf-text
                 (emacs-reader--book-field book 'durChapterTitle)))
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
  (let ((author (emacs-reader--shelf-text
                 (emacs-reader--book-field book 'author)))
        (source (emacs-reader--shelf-text (emacs-reader--book-source book)))
        (name (emacs-reader--shelf-text
               (emacs-reader--book-field book 'name))))
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

(defun emacs-reader--display-buffer (buffer)
  "Display BUFFER in the current frame's dedicated Reader tab.
Create the tab if necessary; otherwise reuse it."
  (tab-bar-switch-to-tab "emacs-reader")
  (switch-to-buffer buffer))

(defun emacs-reader-quit ()
  "Kill the current Reader buffer and leave its window."
  (interactive)
  (quit-window t))

(define-derived-mode emacs-reader-shelf-mode special-mode "Reader-Shelf"
  "Major mode for the Reader bookshelf."
  (setq-local truncate-lines t)
  (setq-local line-spacing 0.2))

(autoload 'emacs-reader-sources "emacs-reader-sources" nil t)

(transient-define-prefix emacs-reader-shelf-menu ()
  "Show commands available on the current bookshelf."
  [["阅读"
    ("RET" "打开当前书籍" emacs-reader-open-book)
    ("s" "搜索书源" emacs-reader-search)
    ("g" "刷新书架" emacs-reader-shelf-refresh)]
   ["本地书库" :if (lambda () (eq emacs-reader-backend 'local))
    ("i" "导入书籍" emacs-reader-import-file)
    ("d" "删除当前书籍…" emacs-reader-shelf-delete)
    ("o" "浏览 OPDS 目录" emacs-reader-browse-catalog)
    ("S" "管理书源" emacs-reader-sources)]])

(define-key emacs-reader-shelf-mode-map (kbd "?") #'emacs-reader-shelf-menu)
(define-key emacs-reader-shelf-mode-map (kbd "S") #'emacs-reader-sources)
(define-key emacs-reader-shelf-mode-map (kbd "d") #'emacs-reader-shelf-delete)
(define-key emacs-reader-shelf-mode-map (kbd "q") #'emacs-reader-quit)

(define-key emacs-reader-shelf-mode-map (kbd "g") #'emacs-reader-shelf-refresh)
(define-key emacs-reader-shelf-mode-map (kbd "RET") #'emacs-reader-open-book)
(define-key emacs-reader-shelf-mode-map (kbd "n") #'emacs-reader-shelf-next)
(define-key emacs-reader-shelf-mode-map (kbd "p") #'emacs-reader-shelf-previous)

(defun emacs-reader-shelf-next (&optional n)
  "Move to the Nth next book on the shelf."
  (interactive "p")
  (let* ((n (or n 1))
         (direction (if (< n 0) -1 1)))
    (dotimes (_ (abs n))
      (let* ((book (get-text-property (point) 'emacs-reader-book))
             (pos
              (save-excursion
                ;; Metadata may contain newlines: skip the entire current book
                ;; and any separator rows, rather than assuming a line count.
                (let (candidate)
                  (while (and (zerop (forward-line direction))
                              (or (null (setq candidate
                                              (get-text-property
                                               (point) 'emacs-reader-book)))
                                  (eq candidate book))))
                  (when (and candidate (not (eq candidate book)))
                    (or (previous-single-property-change
                         (1+ (point)) 'emacs-reader-book)
                        (point-min)))))))
        (when pos (goto-char pos))))))

(defun emacs-reader-shelf-previous (&optional n)
  "Move to the Nth previous book on the shelf."
  (interactive "p")
  (emacs-reader-shelf-next (- (or n 1))))

;;;###autoload
(defun emacs-reader-shelf ()
  "Display the selected bookshelf, newest reading activity first."
  (interactive)
  (let ((buffer (get-buffer-create "*Reader Shelf*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'emacs-reader-shelf-mode)
        (emacs-reader-shelf-mode)))
    (emacs-reader--display-buffer buffer)
    (emacs-reader-shelf-refresh)))

(defun emacs-reader-shelf-refresh ()
  "Refresh the current Reader shelf buffer."
  (interactive)
  (unless (derived-mode-p 'emacs-reader-shelf-mode)
    (user-error "Not in a Reader shelf buffer"))
  (let ((buffer (current-buffer)))
    (setq header-line-format
          (propertize (if (eq emacs-reader-backend 'local)
                          "  本地书架   RET 阅读   s 搜索   S 书源管理   d 删除   ? 全部操作"
                        "  Bookshelf   RET open   s search   g refresh   ? actions")
                      'face 'emacs-reader-shelf-meta)
          mode-line-process " Loading...")
    (if (eq emacs-reader-backend 'local)
        (emacs-reader--handle-bookshelf-response
         buffer `(:ok ((isSuccess . t) (data . ,(emacs-reader-local--records "shelf")))))
      (emacs-reader--reader3-request
       "getBookshelf" "GET" nil
       (lambda (response)
         (when (eq emacs-reader-backend 'reader3)
           (emacs-reader--handle-bookshelf-response buffer response)))))))

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

(defvar-local emacs-reader--search-config nil
  "Search parameters: keyword, source, group, page and source cursor.")
(defvar-local emacs-reader--search-books nil)
(defvar-local emacs-reader--search-generation 0)
(put 'emacs-reader--search-generation 'permanent-local t)
(defvar-local emacs-reader--search-loading nil)
(defvar-local emacs-reader--search-timeout nil)

(defvar-local emacs-reader--search-ended nil)
(defvar-local emacs-reader--preview-p nil)
(defvar-local emacs-reader--preview-start-pending nil)

(defun emacs-reader--stop-search ()
  "Invalidate the current search and release its timers."
  (when emacs-reader--search-timeout
    (cancel-timer emacs-reader--search-timeout)
    (setq emacs-reader--search-timeout nil))
  (setq emacs-reader--search-loading nil)
  (emacs-reader-search-progress-stop))

(defun emacs-reader--render-search ()
  "Display matching rows from the full set of search results."
  (setq tabulated-list-entries
        (emacs-reader-filter-rows
         (mapcar (lambda (book)
                   (list book (vector (emacs-reader--book-field book 'name)
                                      (emacs-reader--book-field book 'author)
                                      (emacs-reader--book-source book)
                                      (emacs-reader--book-field book 'latestChapterTitle))))
                 emacs-reader--search-books)))
  (tabulated-list-print t))

(define-derived-mode emacs-reader-search-mode tabulated-list-mode "Reader-Search"
  "Source search results: RET previews, a adds to shelf, m loads more."
  (add-hook 'kill-buffer-hook #'emacs-reader--stop-search nil t)
  (add-hook 'change-major-mode-hook #'emacs-reader--stop-search nil t)
  (add-hook 'quit-window-hook #'emacs-reader--stop-search nil t)
  (setq tabulated-list-format [("书名" 32 t) ("作者" 18 t) ("书源" 24 t)
                              ("最新章节" 0 nil)])
  (setq-local emacs-reader-filter-refresh-function
              #'emacs-reader--render-search)
  (setq mode-name
        '("Reader-Search" (:eval (emacs-reader-filter-description))))
  (tabulated-list-init-header)
  (hl-line-mode 1))

(define-key emacs-reader-shelf-mode-map (kbd "i") #'emacs-reader-import-file)
(define-key emacs-reader-shelf-mode-map (kbd "o") #'emacs-reader-browse-catalog)
(define-key emacs-reader-shelf-mode-map (kbd "s") #'emacs-reader-search)
(define-key emacs-reader-search-mode-map (kbd "/") #'emacs-reader-filter-edit)
(define-key emacs-reader-search-mode-map (kbd "C-c C-l") #'emacs-reader-filter-clear)
(define-key emacs-reader-search-mode-map (kbd "s") #'emacs-reader-search)
(define-key emacs-reader-search-mode-map (kbd "q") #'emacs-reader-quit)
(define-key emacs-reader-search-mode-map (kbd "n") #'next-line)
(define-key emacs-reader-search-mode-map (kbd "p") #'previous-line)
(define-key emacs-reader-search-mode-map (kbd "m") #'emacs-reader-search-more)
(define-key emacs-reader-search-mode-map (kbd "g") #'emacs-reader-search-more)
(define-key emacs-reader-search-mode-map (kbd "RET") #'emacs-reader-search-preview)
(define-key emacs-reader-search-mode-map (kbd "a") #'emacs-reader-add-to-shelf)

(defun emacs-reader--search-choices (sources)
  "Return completion choices for SOURCES and their groups."
  (let ((choices '(("多源：全部分组" . (:source "" :group "")))))
    (dolist (source sources)
      (let ((url (emacs-reader--book-field source 'bookSourceUrl))
            (name (emacs-reader--book-field source 'bookSourceName)))
        (unless (string-empty-p url)
          (push (cons (format "单源：%s <%s>" name url)
                      (list :source url :group "")) choices)))
      (dolist (group (split-string
                      (emacs-reader--book-field source 'bookSourceGroup) "," t))
        (let ((label (concat "多源分组：" group)))
          (unless (assoc label choices)
            (push (cons label (list :source "" :group group)) choices)))))
    (nreverse choices)))

;;;###autoload
(defun emacs-reader-search (keyword)
  "Search a configured book source for KEYWORD."
  (interactive (list (read-string "搜索书名或作者：")))
  (setq keyword (string-trim keyword))
  (when (string-empty-p keyword) (user-error "请输入搜索关键词"))
  (if (eq emacs-reader-backend 'local)
      (emacs-reader-local--search keyword)
    (emacs-reader--search-reader3 keyword)))

(defun emacs-reader--search-reader3 (keyword)
  "Select Reader3 search scope for KEYWORD."
  (message "正在读取服务器书源…")
  (emacs-reader--reader3-request
   "getBookSources" "GET" nil
   (lambda (response)
     (let* ((body (plist-get response :ok))
            (json (and (listp body) body))
            (sources (alist-get 'data json)))
       (if (and (eq (alist-get 'isSuccess json) t) (consp sources)
                (cl-every #'listp sources))
           (let* ((choices (emacs-reader--search-choices sources))
                  (choice (completing-read "搜索范围：" choices nil t nil nil
                                           "多源：全部分组")))
             (emacs-reader--start-search keyword (cdr (assoc choice choices))))
         (message "书源列表不可用或为空；请先在 Web 端配置书源"))))))

(defun emacs-reader--start-search (keyword config)
  "Start a source search for KEYWORD with CONFIG in the search buffer."
  (let ((buffer (get-buffer-create "*Reader Search*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'emacs-reader-search-mode)
        (emacs-reader-search-mode))
      (emacs-reader--stop-search)
      (setq emacs-reader-filter-query ""
            emacs-reader--search-config
            (append (list :key keyword :page 1 :last-index -1) config)
            emacs-reader--search-books nil
            emacs-reader--search-loading nil
            emacs-reader--search-ended nil
            tabulated-list-entries nil)
      (tabulated-list-print))
    (emacs-reader--display-buffer buffer)
    (emacs-reader-search-more)))

(defun emacs-reader-search-more ()
  "Load the next source search batch, or retry a failed batch."
  (interactive)
  (unless (and (derived-mode-p 'emacs-reader-search-mode)
               emacs-reader--search-config)
    (user-error "请先运行 emacs-reader-search"))
  (cond
   (emacs-reader--search-loading (message "正在搜索，请稍候"))
   (emacs-reader--search-ended (message "没有更多结果；按 s 开始新搜索"))
   (t
    (let* ((buffer (current-buffer))
           (generation (cl-incf emacs-reader--search-generation))
           (config emacs-reader--search-config)
           (multi (string-empty-p (plist-get config :source))))
      (emacs-reader--stop-search)
      (setq emacs-reader--search-loading t
            emacs-reader--search-timeout
            (run-at-time 30 nil #'emacs-reader--handle-search-response buffer generation multi nil))
      (emacs-reader-search-progress-start)
      (condition-case nil
          (if-let* ((source (plist-get config :native)))
              (emacs-reader-legado-search
               source (plist-get config :key)
               (lambda (books err)
                 (emacs-reader--handle-search-response
                  buffer generation nil
                  (unless err `(:ok ((isSuccess . t)
                                     (data . ,(mapcar #'emacs-reader-local--book books))))))
                 (when err (message "Legado 搜索失败：%s" err)))
               (plist-get config :page))
            (emacs-reader--reader3-request
             (if multi "searchBookMulti" "searchBook") "POST"
             `((key . ,(plist-get config :key))
               (bookSourceUrl . ,(plist-get config :source))
               (bookSourceGroup . ,(plist-get config :group))
               (concurrentCount . 4)
               (lastIndex . ,(plist-get config :last-index))
               (page . ,(plist-get config :page)))
             (lambda (response)
               (emacs-reader--handle-search-response buffer generation multi response))))
        (quit (emacs-reader--stop-search) (signal 'quit nil))
        (error (emacs-reader--handle-search-response buffer generation multi nil)))))))

(defun emacs-reader--handle-search-response (buffer generation multi response)
  "Append search RESPONSE in BUFFER for GENERATION; MULTI selects its shape."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'emacs-reader-search-mode)
                 emacs-reader--search-loading
                 (= generation emacs-reader--search-generation))
        (emacs-reader--stop-search)
        (let* ((body (plist-get response :ok))
               (json (and (listp body) body))
               (data (alist-get 'data json))
               (books (if multi (and (listp data) (alist-get 'list data)) data))
               (cursor (and multi (listp data) (alist-get 'lastIndex data))))
          (if (not (and (eq (alist-get 'isSuccess json) t)
                        (listp books)
                        (or (not multi) (integerp cursor))
                        (cl-every (lambda (book)
                                    (and (consp book)
                                         (not (string-empty-p
                                               (emacs-reader--book-field book 'bookUrl)))
                                         (not (string-empty-p
                                               (emacs-reader--book-field book 'origin)))))
                                  books)))
              (message "搜索失败；已有结果保留，按 g 重试")
            (dolist (book books)
              (unless (cl-find-if
                       (lambda (old)
                         (and (equal (alist-get 'bookUrl old) (alist-get 'bookUrl book))
                              (equal (alist-get 'origin old) (alist-get 'origin book))))
                       emacs-reader--search-books)
                (setq emacs-reader--search-books
                      (append emacs-reader--search-books (list book)))))
            (setq emacs-reader--search-ended
                  (if multi
                      (or (eq (alist-get 'isEnd data) t)
                          (<= cursor (plist-get emacs-reader--search-config :last-index)))
                    (null books)))
            (when multi
              (setf (plist-get emacs-reader--search-config :last-index) cursor))
            (cl-incf (plist-get emacs-reader--search-config :page))
            (emacs-reader--render-search)
            (message "搜索结果 %d 条%s" (length emacs-reader--search-books)
                     (if emacs-reader--search-ended "，没有更多结果"
                       "；按 m 继续搜索"))))))))

(defun emacs-reader--selected-search-book ()
  "Return the book on the selected search row."
  (unless (derived-mode-p 'emacs-reader-search-mode)
    (user-error "请先选择一条搜索结果"))
  (or (tabulated-list-get-id) (user-error "当前行没有搜索结果")))

(defun emacs-reader-search-preview ()
  "Preview the selected source result without adding it to the shelf."
  (interactive)
  (emacs-reader-open-book (emacs-reader--selected-search-book) t))

(defun emacs-reader-add-to-shelf (&optional selected)
  "Add SELECTED, the search result at point, or the current preview to its shelf."
  (interactive)
  (let ((book (or selected (if (derived-mode-p 'emacs-reader-reading-mode)
                               emacs-reader--current-book
                             (emacs-reader--selected-search-book)))))
    (unless (and (consp book)
                 (not (string-empty-p (emacs-reader--book-field book 'bookUrl)))
                 (not (string-empty-p (emacs-reader--book-field book 'origin))))
      (user-error "书籍缺少链接或书源信息"))
    (when (yes-or-no-p (format "将《%s》加入书架？ "
                               (emacs-reader--book-field book 'name)))
      (if (alist-get 'readerType book)
          (progn
            (setq book (if (alist-get 'previewFile book)
                           (emacs-reader-import-file
                            (alist-get 'bookUrl book) (alist-get 'name book)
                            (alist-get 'author book) (alist-get 'originName book))
                         (emacs-reader-local--add book)))
            (when (derived-mode-p 'emacs-reader-reading-mode)
              (setq emacs-reader--current-book book emacs-reader--preview-p nil)
              (when (integerp emacs-reader--current-chapter-index)
                (emacs-reader--save-local-progress emacs-reader--current-chapter-index)))
            (message "已加入本地书架"))
        (emacs-reader--reader3-request
         "saveBook" "POST" book
         (lambda (response)
           (if (and (listp (plist-get response :ok))
                    (eq (alist-get 'isSuccess (plist-get response :ok)) t))
               (progn
                 (message "已加入书架；书架按 g 刷新")
                 (let ((shelf (get-buffer "*Reader Shelf*")))
                   (when (buffer-live-p shelf)
                     (with-current-buffer shelf (emacs-reader-shelf-refresh)))))
             (message "加入书架失败；可重试，未确认保存成功"))))))))

(defun emacs-reader-open-book (book &optional preview)
  "Open BOOK in a native text-reading buffer.
With PREVIEW, fetch the catalog with source information before reading.
Local shelf books resume and save their current chapter."
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
        (setq-local emacs-reader--current-book book
                    emacs-reader--preview-p preview
                    emacs-reader--preview-start-pending (or preview (alist-get 'readerType book))))
      (emacs-reader--display-buffer buffer)
      (unless (or preview (alist-get 'readerType book)) (emacs-reader--load-chapter chapter))
      (emacs-reader--load-catalog)
      buffer)))

(define-derived-mode emacs-reader-reading-mode special-mode "Reader"
  "Major mode for reading book chapters."
  (cl-incf emacs-reader--chapter-request-id)
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
  (add-hook 'change-major-mode-hook #'emacs-reader--tts-stop nil t)
  (add-hook 'window-buffer-change-functions #'emacs-reader--tts-follow-window nil t)
  (add-hook 'window-selection-change-functions #'emacs-reader--tts-follow-window nil t))

(define-key emacs-reader-reading-mode-map (kbd "q") #'emacs-reader-quit)

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

(defun emacs-reader--scroll-screen (backward)
  "Scroll one screen or cross a chapter boundary; BACKWARD reverses direction."
  (cond
   ((and emacs-reader--requested-chapter-index
         (not (equal emacs-reader--requested-chapter-index emacs-reader--current-chapter-index)))
    (message "正在加载章节，请稍候"))
   ((not (integerp emacs-reader--current-chapter-index))
    (user-error "正文尚未加载，请从目录选择章节"))
   (backward
    (if (pos-visible-in-window-p (point-min))
        (emacs-reader-previous-chapter t)
      (condition-case nil
          (scroll-down-command)
        (beginning-of-buffer (emacs-reader-previous-chapter t)))))
   ((pos-visible-in-window-p (point-max)) (emacs-reader-next-chapter))
   (t
    (condition-case nil
        (scroll-up-command)
      (end-of-buffer (emacs-reader-next-chapter))))))

(defun emacs-reader-scroll-forward ()
  "Scroll one screen forward, or open the next chapter at its beginning."
  (interactive)
  (emacs-reader--scroll-screen nil))

(defun emacs-reader-scroll-backward ()
  "Scroll one screen backward, or open the previous chapter at its end."
  (interactive)
  (emacs-reader--scroll-screen t))

(define-key emacs-reader-reading-mode-map (kbd "n") #'emacs-reader-scroll-forward)
(define-key emacs-reader-reading-mode-map (kbd "p") #'emacs-reader-scroll-backward)
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
(define-key emacs-reader-reading-mode-map (kbd "a") #'emacs-reader-add-to-shelf)

(defun emacs-reader--tts-follow-window (window)
  "Restore WINDOW to its reading buffer's independently recorded speech position."
  (when (and (window-live-p window) (eq (window-buffer window) (current-buffer))
             (or emacs-reader--tts-active emacs-reader--tts-follow-pending)
             (integerp emacs-reader--tts-position)
             (<= (point-min) emacs-reader--tts-position (point-max)))
    (goto-char emacs-reader--tts-position)
    (set-window-point window emacs-reader--tts-position)
    (unless (pos-visible-in-window-p emacs-reader--tts-position window)
      (set-window-start window emacs-reader--tts-position))
    (setq emacs-reader--tts-follow-pending nil)))

(defun emacs-reader--tts-stop ()
  "Stop playback and discard pending speech in the current reading buffer."
  (when (and emacs-reader--tts-active emacs-reader--tts-position
             (not (eq (window-buffer (selected-window)) (current-buffer))))
    (setq emacs-reader--tts-follow-pending t))
  (setq emacs-reader--tts-active nil
        emacs-reader--tts-generation (1+ emacs-reader--tts-generation))
  (when (process-live-p emacs-reader--tts-process)
    (delete-process emacs-reader--tts-process))
  (setq emacs-reader--tts-process nil
        emacs-reader--tts-native-command nil)
  (dolist (buffer (list emacs-reader--tts-request-buffer emacs-reader--tts-prefetch-buffer))
    (when (buffer-live-p buffer)
      (let ((process (get-buffer-process buffer)))
        (when (process-live-p process) (delete-process process)))
      (when (buffer-live-p buffer) (kill-buffer buffer))))
  (setq emacs-reader--tts-request-buffer nil
        emacs-reader--tts-prefetch-buffer nil
        emacs-reader--tts-prefetch nil)
  (when (and emacs-reader--tts-file (file-exists-p emacs-reader--tts-file))
    (delete-file emacs-reader--tts-file))
  (setq emacs-reader--tts-file nil)
  (when (overlayp emacs-reader--tts-overlay)
    (when (overlay-start emacs-reader--tts-overlay)
      (goto-char (overlay-start emacs-reader--tts-overlay))
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (set-window-point window (point))))
    (delete-overlay emacs-reader--tts-overlay))
  (setq emacs-reader--tts-overlay nil)
  (force-mode-line-update))

(defun emacs-reader--tts-current-p (buffer generation)
  "Whether BUFFER still owns speech GENERATION."
  (and (buffer-live-p buffer)
       (buffer-local-value 'emacs-reader--tts-active buffer)
       (= generation (buffer-local-value 'emacs-reader--tts-generation buffer))))

(defun emacs-reader--wsl-p ()
  "Whether Emacs is running in Windows Subsystem for Linux."
  (and (eq system-type 'gnu/linux)
       (or (getenv "WSL_DISTRO_NAME") (getenv "WSL_INTEROP")
           (and (file-readable-p "/proc/sys/kernel/osrelease")
                (with-temp-buffer
                  (insert-file-contents "/proc/sys/kernel/osrelease")
                  (string-match-p "microsoft" (downcase (buffer-string))))))))

(defun emacs-reader--tts-player ()
  "Return a local or WSL-accessible audio player's command without a shell."
  (cond (emacs-reader-tts-player-command
         (unless (and (consp emacs-reader-tts-player-command)
                      (cl-every #'stringp emacs-reader-tts-player-command)
                      (executable-find (car emacs-reader-tts-player-command)))
           (user-error "无效或不可用的朗读播放器命令"))
         (cons (executable-find (car emacs-reader-tts-player-command))
               (cdr emacs-reader-tts-player-command)))
        ((executable-find "afplay") (list (executable-find "afplay")))
        ((executable-find "mpv") (list (executable-find "mpv") "--no-video" "--really-quiet"))
        ((executable-find "ffplay")
         (list (executable-find "ffplay") "-nodisp" "-autoexit" "-loglevel" "quiet"))
        ((and (emacs-reader--wsl-p) (executable-find "mpv.exe"))
         (list (executable-find "mpv.exe") "--no-video" "--really-quiet"))
        ((and (emacs-reader--wsl-p) (executable-find "ffplay.exe"))
         (list (executable-find "ffplay.exe") "-nodisp" "-autoexit" "-loglevel" "quiet"))
        (t (user-error "朗读需要 afplay、mpv 或 ffplay；WSL 也可使用 PATH 中的 mpv.exe/ffplay.exe"))))

(defun emacs-reader--tts-player-command (file)
  "Build a playback command for FILE, translating WSL paths for Windows players."
  (let ((command (emacs-reader--tts-player)))
    (when (and (emacs-reader--wsl-p) (string-suffix-p ".exe" (car command) t))
      (unless (executable-find "wslpath")
        (user-error "WSL 调用 Windows 播放器需要 wslpath"))
      (setq file
            (with-temp-buffer
              (unless (eq 0 (process-file "wslpath" nil t nil "-w" file))
                (user-error "无法将音频文件路径转换为 Windows 路径"))
              (let ((path (string-trim (buffer-string))))
                (when (or (string-empty-p path) (string-match-p "[\r\n]" path))
                  (user-error "wslpath 返回了无效路径"))
                path))))
    (append command (list file))))

(defun emacs-reader--tts-native-command ()
  "Build a local speech command that accepts UTF-8 text on standard input."
  (let* ((say (and (eq system-type 'darwin) (executable-find "say")))
         (program (or say (executable-find "espeak-ng") (executable-find "espeak")
                      (user-error "本机朗读需要 say、espeak-ng 或 espeak，也可设置 emacs-reader-tts-url")))
         (speed (unless (= emacs-reader-tts-rate 1)
                  (list (if say "-r" "-s")
                        (number-to-string (max 1 (round (* 175 emacs-reader-tts-rate))))))))
    (append (list program)
            (when emacs-reader-tts-native-voice (list "-v" emacs-reader-tts-native-voice))
            speed (unless say '("-b" "1" "--stdin")))))

(defun emacs-reader-toggle-read-aloud ()
  "Toggle paragraph speech using the configured endpoint or a local voice."
  (interactive)
  (unless (derived-mode-p 'emacs-reader-reading-mode)
    (user-error "请在阅读页面使用朗读"))
  (when emacs-reader--tts-follow-pending
    (emacs-reader--tts-follow-window (selected-window)))
  (if emacs-reader--tts-active
      (progn (emacs-reader--tts-stop) (message "朗读已停止"))
    (unless (and (integerp emacs-reader--current-chapter-index)
                 (equal emacs-reader--current-chapter-index emacs-reader--requested-chapter-index))
      (user-error "请等待章节加载完成"))
    (let ((native (if (member emacs-reader-tts-url '(nil ""))
                      (emacs-reader--tts-native-command)
                    (emacs-reader--tts-endpoint)
                    (emacs-reader--tts-player)
                    nil)))
      (emacs-reader--tts-stop)
      (setq emacs-reader--tts-active t
            emacs-reader--tts-native-command native
            emacs-reader--tts-position nil
            emacs-reader--tts-follow-pending nil)
      (emacs-reader--tts-read-paragraph (point)))))

(defun emacs-reader--tts-paragraph (position)
  "Return (START END TEXT) for POSITION's paragraph, or nil at chapter end."
  (save-excursion
    (goto-char position)
    (skip-chars-forward " \t\r\n　 ")
    (unless (eobp)
      (let* ((separator "\n[ \t　 ]*\n")
             (start (save-excursion
                      (if (re-search-backward separator nil t) (match-end 0) (point-min))))
             (end (save-excursion
                    (if (re-search-forward separator nil t) (match-beginning 0) (point-max)))))
        (list start end (string-trim (buffer-substring-no-properties start end)))))))

(defun emacs-reader--tts-endpoint ()
  "Validate and return the parsed `emacs-reader-tts-url'."
  (unless (and (stringp emacs-reader-tts-url) (not (string-empty-p emacs-reader-tts-url)))
    (user-error "请先设置 emacs-reader-tts-url"))
  (let ((url (url-generic-parse-url emacs-reader-tts-url)))
    (unless (and (member (url-type url) '("http" "https"))
                 (not (string-empty-p (or (url-host url) "")))
                 (not (or (url-user url) (url-password url)))
                 (not (string-match-p "[?#[:space:][:cntrl:]]" emacs-reader-tts-url)))
      (user-error "TTS 地址须为不含凭据、查询参数或片段的 HTTP(S) URL"))
    url))

(defun emacs-reader--tts-fetch (text callback)
  "Fetch TEXT's audio from the standalone TTS endpoint and call CALLBACK."
  (let* ((url (emacs-reader--tts-endpoint))
         (key (and emacs-reader-tts-api-key-env (getenv emacs-reader-tts-api-key-env)))
         (format emacs-reader-tts-format)
         (headers '(("Content-Type" . "application/json"))))
    (when emacs-reader-tts-api-key-env
      (unless (and key (string-match-p "\\`[A-Za-z0-9._~+/-]+=*\\'" key))
        (user-error "TTS Bearer token 未设置或格式无效"))
      (unless (or (equal (url-type url) "https")
                  (member (url-host url) '("localhost" "127.0.0.1" "[::1]")))
        (user-error "不能通过非本机 HTTP 发送 TTS Bearer token"))
      (push (cons "Authorization" (encode-coding-string (concat "Bearer " key) 'us-ascii))
            headers))
    (emacs-reader--url-transport
     (list :url emacs-reader-tts-url :method "POST" :headers headers
           :audio-format format
           :data (encode-coding-string
                  (json-serialize `((model . ,emacs-reader-tts-model)
                                    (voice . ,emacs-reader-tts-voice)
                                    (input . ,text) (speed . ,emacs-reader-tts-rate)
                                    (response_format . ,(symbol-name format))))
                  'utf-8))
     callback)))

(defun emacs-reader--tts-advance-chapter ()
  "Continue speech in the next chapter, or stop at the end of the book."
  (let ((index (1+ emacs-reader--current-chapter-index))
        (total (emacs-reader--chapter-count emacs-reader--current-book)))
    (if (and (integerp total) (> total 0) (>= index total))
        (progn (emacs-reader--tts-stop) (message "全书朗读完毕"))
      (message "正在加载下一章以继续朗读…")
      (emacs-reader--load-chapter index emacs-reader--tts-generation))))

(defun emacs-reader--tts-read-paragraph (position)
  "Read POSITION's paragraph, consuming its prefetched audio when available."
  (let* ((paragraph (emacs-reader--tts-paragraph position))
         (buffer (current-buffer))
         (generation emacs-reader--tts-generation)
         (job emacs-reader--tts-prefetch))
    (cond
     ((null paragraph) (emacs-reader--tts-advance-chapter))
     (emacs-reader--tts-native-command
      (condition-case err
          (progn
            (emacs-reader--tts-start-process (car paragraph) (cadr paragraph)
                                             emacs-reader--tts-native-command)
            (process-send-string emacs-reader--tts-process (nth 2 paragraph))
            (process-send-eof emacs-reader--tts-process))
        (error (emacs-reader--tts-stop)
               (message "本机朗读失败：%s" (error-message-string err)))))
     ((and job (= (car paragraph) (plist-get job :start)))
      (if (plist-get job :ready)
          (progn
            (setq emacs-reader--tts-prefetch nil)
            (emacs-reader--tts-play-response buffer generation (car paragraph) (cadr paragraph)
                                             (plist-get job :response)))
        (setf (plist-get job :waiting) t)
        (message "下一段音频仍在生成…")))
     (t
      (message "正在获取朗读音频…")
      (condition-case err
          (setq emacs-reader--tts-request-buffer
                (emacs-reader--tts-fetch
                 (nth 2 paragraph)
                 (lambda (response)
                   (emacs-reader--tts-play-response
                    buffer generation (car paragraph) (cadr paragraph) response))))
        (user-error (emacs-reader--tts-stop) (message "%s" (error-message-string err)))
        (error (emacs-reader--tts-stop) (message "朗读请求失败，请检查 TTS 服务配置")))))))

(defun emacs-reader--tts-prefetch-next (position)
  "Prefetch at most one paragraph after POSITION while current audio plays."
  (let ((paragraph (emacs-reader--tts-paragraph position))
        (buffer (current-buffer))
        (generation emacs-reader--tts-generation))
    (when (and paragraph (not emacs-reader--tts-prefetch))
      (let ((job (list :start (car paragraph) :end (cadr paragraph)
                       :ready nil :waiting nil :response nil)))
        (setq emacs-reader--tts-prefetch job)
        (condition-case nil
            (setq emacs-reader--tts-prefetch-buffer
                  (emacs-reader--tts-fetch
                   (nth 2 paragraph)
                   (lambda (response)
                     (when (emacs-reader--tts-current-p buffer generation)
                       (with-current-buffer buffer
                         (when (eq job emacs-reader--tts-prefetch)
                           (setq emacs-reader--tts-prefetch-buffer nil)
                           (setf (plist-get job :ready) t
                                 (plist-get job :response) response)
                           (when (plist-get job :waiting)
                             (setq emacs-reader--tts-prefetch nil)
                             (emacs-reader--tts-play-response
                              buffer generation (plist-get job :start)
                              (plist-get job :end) response))))))))
          ;; A failed prefetch must not interrupt the paragraph already playing.
          (error (setf (plist-get job :ready) t
                       (plist-get job :response) '(:error transport))))))))

(defun emacs-reader--tts-start-process (start end command)
  "Highlight the paragraph from START to END and play it using COMMAND."
  (let ((buffer (current-buffer))
        (generation emacs-reader--tts-generation))
    (when (overlayp emacs-reader--tts-overlay) (delete-overlay emacs-reader--tts-overlay))
    (setq emacs-reader--tts-position start
          emacs-reader--tts-follow-pending t)
    (goto-char start)
    (setq emacs-reader--tts-overlay (make-overlay start end))
    (overlay-put emacs-reader--tts-overlay 'face 'emacs-reader-tts-paragraph)
    (dolist (window (get-buffer-window-list buffer nil t))
      (emacs-reader--tts-follow-window window))
    (setq emacs-reader--tts-process
          (make-process
           :name "emacs-reader-speech" :buffer nil :connection-type 'pipe :noquery t
           :coding 'utf-8-unix :command command
           :sentinel
           (lambda (process _event)
             (when (and (memq (process-status process) '(exit signal))
                        (emacs-reader--tts-current-p buffer generation))
               (with-current-buffer buffer
                 (when (eq process emacs-reader--tts-process)
                   (setq emacs-reader--tts-process nil)
                   (if (zerop (process-exit-status process))
                       (progn
                         (when emacs-reader--tts-file (delete-file emacs-reader--tts-file))
                         (setq emacs-reader--tts-file nil)
                         (delete-overlay emacs-reader--tts-overlay)
                         (setq emacs-reader--tts-overlay nil)
                         (emacs-reader--tts-read-paragraph end))
                     (emacs-reader--tts-stop)
                     (message "朗读播放失败，请检查语音程序"))))))))
    (message "正在朗读；r 停止")))

(defun emacs-reader--tts-play-response (buffer generation start end response)
  "Play RESPONSE for BUFFER's paragraph START to END if GENERATION is current."
  (when (emacs-reader--tts-current-p buffer generation)
    (with-current-buffer buffer
      (setq emacs-reader--tts-request-buffer nil)
      (condition-case err
          (let ((audio (plist-get response :ok))
                (format (plist-get response :format)))
            (unless (and (stringp audio) (> (length audio) 0) (memq format '(mp3 wav)))
              (user-error "朗读合成失败（%s），请检查 TTS 服务配置"
                          (or (plist-get response :error) "无音频")))
            (setq emacs-reader--tts-file
                  (make-temp-file "emacs-reader-speech-" nil (concat "." (symbol-name format))))
            (let ((coding-system-for-write 'no-conversion))
              (write-region audio nil emacs-reader--tts-file nil 'silent))
            (emacs-reader--tts-start-process
             start end (emacs-reader--tts-player-command emacs-reader--tts-file))
            (emacs-reader--tts-prefetch-next end))
        (user-error (emacs-reader--tts-stop) (message "%s" (error-message-string err)))
        (error (emacs-reader--tts-stop) (message "朗读播放失败，请检查播放器"))))))

(defun emacs-reader--load-chapter (index &optional speech-generation at-end)
  "Request zero-based chapter INDEX for the current Reader book.
Resume speech if SPEECH-GENERATION is still active.  With AT-END, show the end."
  (when emacs-reader--preview-start-pending
    (user-error "目录尚未就绪，请按 c 加载或重试"))
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
    (unless speech-generation (emacs-reader--tts-stop))
    (emacs-reader--clear-recovery)
    (setq emacs-reader--chapter-request-id
          (1+ emacs-reader--chapter-request-id)
          emacs-reader--requested-chapter-index index
          mode-line-process " Loading...")
    (let ((request-id emacs-reader--chapter-request-id))
      (condition-case err
          (if (alist-get 'readerType book)
              (emacs-reader-local--fetch
               book (alist-get 'native (aref emacs-reader--chapters index))
               (lambda (text err)
                 (when (emacs-reader--chapter-request-current-p buffer request-id)
                   (with-current-buffer buffer
                     (emacs-reader--display-chapter
                      buffer request-id index text
                      (and (equal (alist-get 'readerType book) "file")
                           (equal (file-name-extension book-url) "epub") 'paragraphs)
                      speech-generation at-end)
                     (when err
                       (emacs-reader--offer-recovery
                        (plist-get (alist-get 'native (aref emacs-reader--chapters index)) :url)
                        index)
                       (message "正文加载失败：%s" err))))))
            (emacs-reader--reader3-request
             "getBookContent" "POST"
             `((url . ,book-url) (index . ,index))
             (lambda (response)
               (emacs-reader--handle-chapter-response
                buffer request-id index response speech-generation at-end))))
        (error
         (if (or speech-generation at-end)
             (emacs-reader--display-chapter buffer request-id index nil nil speech-generation at-end)
           (signal (car err) (cdr err))))))))

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

(defun emacs-reader--request-catalog (book callback)
  "Fetch the catalog for BOOK and deliver its indexed chapters to CALLBACK."
  (if (alist-get 'readerType book)
      (emacs-reader-local--fetch
       book nil
       (lambda (items err)
         (let ((chapters (cl-loop for item in items for i from 0
                                  collect `((index . ,i) (title . ,(plist-get item :title))
                                            (native . ,item)))))
           (funcall callback `(:ok ((isSuccess . ,(null err)) (data . ,chapters)) :error ,err)))))
    (emacs-reader--reader3-request
     "getChapterList" "POST"
     `((url . ,(alist-get 'bookUrl book)) (refresh . 0)
       ,@(when emacs-reader--preview-p `((bookSourceUrl . ,(alist-get 'origin book)))))
     callback)))

(defun emacs-reader--load-catalog (&optional show)
  "Fetch the current book's catalog once; with SHOW, open it when ready."
  (if emacs-reader--chapters
      (when show (emacs-reader--show-catalog (current-buffer)))
    (setq emacs-reader--catalog-show-pending (or show emacs-reader--catalog-show-pending))
    (unless emacs-reader--catalog-loading
      (let ((buffer (current-buffer)) (request (list 'catalog))
            (url (alist-get 'bookUrl emacs-reader--current-book)))
        (unless (and (stringp url) (not (string-empty-p url)))
          (user-error "No Reader book is open"))
        (emacs-reader--clear-recovery)
        (setq emacs-reader--catalog-loading request)
        (emacs-reader--request-catalog
         emacs-reader--current-book
         (lambda (response)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (and (derived-mode-p 'emacs-reader-reading-mode)
                          (eq request emacs-reader--catalog-loading)
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
                         (when emacs-reader--preview-start-pending
                           (setq emacs-reader--preview-start-pending nil)
                           (emacs-reader--load-chapter
                             (if emacs-reader--preview-p 0
                               (min (1- (length chapters))
                                    (max 0 (or (alist-get 'durChapterIndex emacs-reader--current-book) 0))))))
                          (when show (emacs-reader--show-catalog buffer)))
                     (emacs-reader--offer-recovery (or (alist-get 'tocUrl emacs-reader--current-book)
                                                       url) nil show)
                     (message "Reader 目录加载失败，可按 c 重试")
                     (when-let* ((err (plist-get response :error))
                                 ((alist-get 'readerType emacs-reader--current-book)))
                       (message "目录加载失败：%s" err)))))))))))))

(define-derived-mode emacs-reader-toc-mode tabulated-list-mode "Reader-TOC"
  "Table of contents: n/p select, RET reads a chapter, q kills the buffer."
  (setq tabulated-list-format [("状态" 8 nil) ("序号" 8 nil) ("章节名称" 0 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(define-key emacs-reader-toc-mode-map (kbd "q") #'emacs-reader-quit)
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
    (emacs-reader--display-buffer buffer)
    (message "目录：n/p 选择，RET 跳转，q 关闭")))

(defun emacs-reader-toc-open-chapter ()
  "Read the chapter on the current table-of-contents row."
  (interactive)
  (let ((index (tabulated-list-get-id))
        (reader emacs-reader--toc-reader-buffer))
    (unless (integerp index) (user-error "请选择一个章节"))
    (unless (buffer-live-p reader) (user-error "阅读缓冲区已关闭"))
    (quit-window)
    (emacs-reader--display-buffer reader)
    (with-current-buffer reader (emacs-reader--load-chapter index))))

(defun emacs-reader--book-asset-path-p (path)
  "Whether PATH names a resource under a Reader book asset route."
  (and (stringp path)
       (or (string-prefix-p "/book-assets/" path)
           (string-prefix-p "/epub/" path))))

(defun emacs-reader--book-asset-url (path)
  "Return a same-origin URL for a server-supplied book asset PATH."
  (unless (and (emacs-reader--book-asset-path-p path)
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

(defun emacs-reader--html-horizontal-rule (_dom)
  "Render an EPUB rule as a paragraph break rather than a line of text."
  (shr-ensure-paragraph))

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
          ;; Keep width bounded: SHR uses it to allocate rules and table cells.
          ;; Let the reading buffer wrap prose on Emacs versions with this option.
          (shr-fill-text nil)
          (shr-width (if (boundp 'shr-fill-text) 80 2048))
          (shr-external-rendering-functions
           '((hr . emacs-reader--html-horizontal-rule))))
      (erase-buffer)
      (shr-insert-document (car (dom-by-tag dom 'body)))
      (emacs-reader--format-paragraphs (buffer-string)))))

(defun emacs-reader--format-paragraphs (content)
  "Indent paragraphs in CONTENT while retaining explicit line breaks."
  (mapconcat (lambda (paragraph)
               (concat "　　" (string-trim paragraph "[ \t\n　 ]+" "[ \t\n　 ]+")))
             (split-string content "\n[ \t]*\n+" t) "\n\n"))

(defun emacs-reader--chapter-request-current-p (buffer request-id)
  "Whether BUFFER still expects the chapter response for REQUEST-ID."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'emacs-reader-reading-mode)
              (= request-id emacs-reader--chapter-request-id)))))

(defun emacs-reader--save-local-progress (index)
  "Record the successfully displayed chapter INDEX for a local shelf book."
  (when (and (alist-get 'readerType emacs-reader--current-book)
             (not emacs-reader--preview-p))
    (emacs-reader-local--progress emacs-reader--current-book
                                 (aref emacs-reader--chapters index) index
                                 (length emacs-reader--chapters))))

(defun emacs-reader--display-chapter (buffer request-id index content &optional html-p speech-generation at-end)
  "Display CONTENT for INDEX in BUFFER if REQUEST-ID is current.
With HTML-P, render HTML; otherwise normalize plain text.  Nil means failure.
Continue SPEECH-GENERATION only if it is still active.  AT-END shows the end."
  (when (emacs-reader--chapter-request-current-p buffer request-id)
    (with-current-buffer buffer
      (setq mode-line-process nil)
      (let ((text (and (stringp content)
                       (condition-case nil
                           (cond ((eq html-p 'paragraphs)
                                   (if (string-blank-p content) "本页没有可显示的文字。"
                                     (emacs-reader--format-paragraphs content)))
                                  (html-p (emacs-reader--html-chapter-text content))
                                  (t (emacs-reader--format-chapter content)))
                         (error nil)))))
        (if (and text (not (string-blank-p text)))
            (let ((inhibit-read-only t))
              (setq emacs-reader--tts-position nil emacs-reader--tts-follow-pending nil)
              (erase-buffer)
              (insert "\n" text "\n")
              (goto-char (if at-end (point-max) (point-min)))
              (setq emacs-reader--current-chapter-index index
                    emacs-reader--requested-chapter-index index)
              (emacs-reader--update-reading-header)
              (emacs-reader--save-local-progress index)
              (dolist (window (get-buffer-window-list buffer nil t))
                (set-window-point window (point))
                (if at-end
                    (with-selected-window window (recenter -1))
                  (set-window-start window (point-min))))
              (if (and speech-generation (emacs-reader--tts-current-p buffer speech-generation))
                  (emacs-reader--tts-read-paragraph (point-min))
                (message "Loaded chapter %d" (1+ index))))
          (setq emacs-reader--requested-chapter-index emacs-reader--current-chapter-index)
          (if (and speech-generation (emacs-reader--tts-current-p buffer speech-generation))
              (progn (emacs-reader--tts-stop) (message "下一章加载失败，朗读已停止"))
            (message "Reader chapter unavailable; no progress was changed")))))))

(defun emacs-reader--handle-chapter-response (buffer request-id index response &optional speech-generation at-end)
  "Display a chapter RESPONSE or fetch its EPUB resource if still current.
Pass SPEECH-GENERATION and AT-END through to the final chapter render."
  (when (emacs-reader--chapter-request-current-p buffer request-id)
    (let* ((json (plist-get response :ok))
           (content (and (listp json) (eq (alist-get 'isSuccess json) t)
                         (alist-get 'data json)))
           (path (and (stringp content) (string-trim content))))
      (if (emacs-reader--book-asset-path-p path)
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
                  buffer request-id index (plist-get asset-response :ok) t speech-generation at-end)))
            (error (emacs-reader--display-chapter buffer request-id index nil nil speech-generation at-end)))
        (emacs-reader--display-chapter buffer request-id index content nil speech-generation at-end)))))

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

(defun emacs-reader-previous-chapter (&optional at-end)
  "Load the previous chapter, landing on its last screen if AT-END is non-nil."
  (interactive)
  (let ((index (1- (emacs-reader--chapter-navigation-base))))
    (when (< index 0)
      (user-error "已到第一章"))
    (emacs-reader--load-chapter index nil at-end)))

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
