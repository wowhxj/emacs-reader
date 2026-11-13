;;; emacs-reader-test.el --- Tests for emacs-reader -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-reader)

(defvar emacs-reader-test--http-response nil)
(defvar emacs-reader-test--http-request nil)

(defun emacs-reader-test--http-server-log (_server client _message)
  (set-process-filter client #'emacs-reader-test--http-filter))

(defun emacs-reader-test--http-filter (process data)
  (setq emacs-reader-test--http-request
        (concat emacs-reader-test--http-request data))
  (when (and (not (process-get process 'reader-test-replied))
             (string-match-p "\\r?\\n\\r?\\n"
                             emacs-reader-test--http-request))
    (process-put process 'reader-test-replied t)
    (process-send-string process emacs-reader-test--http-response)
    (process-send-eof process)))

(ert-deftest emacs-reader-server-url-allows-http-by-default ()
  (let ((emacs-reader-server-url "http://reader.example:8084"))
    (should emacs-reader-allow-insecure-http)
    (should (equal (emacs-reader--api-url "/api/v1/books")
                   "http://reader.example:8084/api/v1/books"))))

(ert-deftest emacs-reader-server-url-can-disable-http-explicitly ()
  (let ((emacs-reader-server-url "http://reader.example:8084")
        (emacs-reader-allow-insecure-http nil))
    (should-error (emacs-reader--api-url "/api/v1/books") :type 'user-error)))

(ert-deftest emacs-reader-server-url-can-explicitly-allow-http ()
  (let ((emacs-reader-server-url "http://reader.example:8084/reader/")
        (emacs-reader-allow-insecure-http t))
    (should (equal (emacs-reader--api-url "/api/v1/books")
                   "http://reader.example:8084/reader/api/v1/books"))))

(ert-deftest emacs-reader-server-url-rejects-credentials-query-and-fragment ()
  (let ((emacs-reader-allow-insecure-http t))
    (dolist (server-url '("http://alice:secret@reader.example"
                          "http://reader.example/path?token=x"
                          "http://reader.example/path#fragment"))
      (let ((emacs-reader-server-url server-url))
        (should-error (emacs-reader--api-url "/api/v1/books")
                      :type 'user-error)))))

(ert-deftest emacs-reader-api-path-must-be-internal-and-normalized ()
  (let ((emacs-reader-server-url "https://reader.example"))
    (dolist (path '("https://other.example/api/v1/books"
                    "/api/v1/../auth/login"
                    "/api/v1/%2e%2e/auth/login"
                    "/api/v1/books?token=x"
                    "/api/v1"))
      (should-error (emacs-reader--api-url path) :type 'user-error))))

(ert-deftest emacs-reader-json-response-parses-json-without-retaining-body ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"count\":2}")
    (setq-local url-http-response-status 200)
    (setq-local url-http-content-type "application/json")
    (setq-local url-http-content-length 11)
    (let ((result (emacs-reader--json-response nil)))
      (should (= (plist-get result :status) 200))
      (should (= (alist-get 'count (plist-get result :ok)) 2)))))

(ert-deftest emacs-reader-json-response-does-not-expose-server-error-body ()
  (with-temp-buffer
    (insert "HTTP/1.1 500 Error\r\nContent-Type: text/plain\r\n\r\nprivate detail")
    (setq-local url-http-response-status 500)
    (setq-local url-http-content-type "text/plain")
    (let ((result (emacs-reader--json-response nil)))
      (should (eq (plist-get result :error) 'server))
      (should-not (string-match-p "private detail" (prin1-to-string result))))))

(ert-deftest emacs-reader-json-response-rejects-invalid-and-oversized-data ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\nnot-json")
    (setq-local url-http-response-status 200)
    (setq-local url-http-content-type "application/json")
    (setq-local url-http-content-length 8)
    (should (eq (plist-get (emacs-reader--json-response nil) :error)
                'invalid-json)))
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"ok\":true}")
    (setq-local url-http-response-status 200)
    (setq-local url-http-content-type "application/json")
    (setq-local url-http-content-length 11)
    (setq-local emacs-reader--transport-error t)
    (should (eq (plist-get (emacs-reader--json-response nil) :error)
                'response-too-large))))

(ert-deftest emacs-reader-json-response-classifies-redirects-and-auth-failures ()
  (with-temp-buffer
    (setq-local url-http-response-status 302)
    (should (eq (plist-get (emacs-reader--json-response nil) :error) 'http)))
  (with-temp-buffer
    (setq-local url-http-response-status 401)
    (should (eq (plist-get (emacs-reader--json-response nil) :error)
                'authentication))))

(ert-deftest emacs-reader-api-request-adds-only-request-scoped-headers ()
  (let ((emacs-reader-server-url "https://reader.example")
        captured)
    (cl-letf (((symbol-function 'emacs-reader--url-transport)
               (lambda (request _callback)
                 (setq captured request))))
      (emacs-reader--api-request "/api/v1/books" "GET" nil #'ignore)
      (should (equal (plist-get captured :url)
                     "https://reader.example/api/v1/books"))
      (should (equal (plist-get captured :headers)
                     '(("Accept" . "application/json"))))
      (should-not (plist-get captured :data))
      (emacs-reader--api-request "/api/v1/books" "POST"
                                 '((limit . 5)) #'ignore "opaque-token")
      (should (equal (assoc "Authorization" (plist-get captured :headers))
                     '("Authorization" . "Bearer opaque-token")))
      (should (equal (json-parse-string (plist-get captured :data)
                                        :object-type 'alist)
                     '((limit . 5)))))))

(ert-deftest emacs-reader-api-request-rejects-header-injection-in-token ()
  (let ((emacs-reader-server-url "https://reader.example"))
    (should-error
     (emacs-reader--api-request "/api/v1/books" "GET" nil #'ignore
                                "safe-token\r\nInjected: yes")
     :type 'user-error)))

(ert-deftest emacs-reader-reader3-uses-only-the-memory-session-cookie ()
  (let ((emacs-reader-server-url "http://reader.example:8084")
        (emacs-reader--session-cookie "cookie-value")
        captured)
    (cl-letf (((symbol-function 'emacs-reader--url-transport)
               (lambda (request _callback)
                 (setq captured request))))
      (emacs-reader--api-request "/reader3/getBookshelf" "GET" nil #'ignore)
      (should (equal (assoc "Cookie" (plist-get captured :headers))
                     '("Cookie" . "reader.session=cookie-value")))
      (should-not (assoc "Authorization" (plist-get captured :headers)))
      (emacs-reader--api-request "/api/v1/books" "GET" nil #'ignore)
      (should-not (assoc "Cookie" (plist-get captured :headers))))))

(ert-deftest emacs-reader-response-session-cookie-is-scoped-and-parsed ()
  (with-temp-buffer
    (insert (concat "HTTP/1.1 200 OK\r\n"
                    "Set-Cookie: reader.session=opaque%3Dvalue; Path=/; HttpOnly\r\n"
                    "Set-Cookie: unrelated=ignored; Path=/\r\n"
                    "Content-Type: application/json\r\n\r\n{}"))
    (should (equal (emacs-reader--response-session-cookie)
                   '(t . "opaque%3Dvalue"))))
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nSet-Cookie: reader.session=; Max-Age=0\r\n\r\n{}")
    (should (equal (emacs-reader--response-session-cookie) '(t . "")))))

(ert-deftest emacs-reader-load-chapter-requests-and-renders-reader3-text ()
  (let ((emacs-reader-server-url "http://reader.example:8084")
        (emacs-reader--session-cookie "session-cookie")
        (book '((bookUrl . "source-book-url")
                (name . "Test Book")
                (durChapterIndex . 2)
                (durChapterTitle . "Current chapter")
                (totalChapterNum . 5)))
        request)
    (with-temp-buffer
      (emacs-reader-reading-mode)
      (setq-local emacs-reader--current-book book)
      (cl-letf (((symbol-function 'emacs-reader--url-transport)
                 (lambda (captured callback)
                   (setq request captured)
                   (funcall callback
                            '(:status 200
                              :ok ((isSuccess . t) (data . "chapter body")))))))
        (emacs-reader--load-chapter 2))
      (should (equal (plist-get request :url)
                     "http://reader.example:8084/reader3/getBookContent"))
      (should (equal (plist-get request :method) "POST"))
      (should (equal (json-parse-string (plist-get request :data)
                                        :object-type 'alist)
                     '((url . "source-book-url") (index . 2))))
      (should (equal (assoc "Cookie" (plist-get request :headers))
                     '("Cookie" . "reader.session=session-cookie")))
      (should (equal (buffer-string) "\n\u3000\u3000chapter body\n"))
      (should (eq word-wrap-by-category t))
      (should-not nobreak-char-display)
      (should (= emacs-reader--current-chapter-index 2))
      (should (equal header-line-format "Test Book — Chapter 3: Current chapter"))
      (should (eq (lookup-key emacs-reader-reading-mode-map (kbd "n"))
                  #'scroll-up-command))
      (should (eq (lookup-key emacs-reader-reading-mode-map (kbd "p"))
                  #'scroll-down-command)))))

(ert-deftest emacs-reader-chapter-navigation-uses-zero-based-api-indices ()
  (let ((book '((bookUrl . "source-book-url") (name . "Test Book")
                (totalChapterNum . 3)))
        requested)
    (with-temp-buffer
      (emacs-reader-reading-mode)
      (setq-local emacs-reader--session-cookie "session"
                  emacs-reader--current-book book
                  emacs-reader--current-chapter-index 0
                  emacs-reader--requested-chapter-index 0)
      (cl-letf (((symbol-function 'emacs-reader--api-request)
                 (lambda (_path _method data callback)
                   (push (alist-get 'index data) requested)
                   (funcall callback '(:ok ((isSuccess . t) (data . "text")))))))
        (emacs-reader-next-chapter)
        (emacs-reader-previous-chapter)
        (should-error (emacs-reader-previous-chapter) :type 'user-error)
        (should-error (emacs-reader--load-chapter 3) :type 'user-error))
      (should (equal (nreverse requested) '(1 0)))
      (should (= emacs-reader--current-chapter-index 0)))))

(ert-deftest emacs-reader-url-callback-captures-session-cookie-in-memory ()
  (let ((emacs-reader--session-cookie nil))
    (with-temp-buffer
      (insert (concat "HTTP/1.1 200 OK\r\n"
                      "Set-Cookie: reader.session=opaque-cookie; Path=/\r\n"
                      "Content-Type: application/json\r\n\r\n{}"))
      (cl-letf (((symbol-function 'emacs-reader--json-response)
                 (lambda (_status) '(:ok ((isSuccess . t))))))
        (emacs-reader--url-callback nil #'ignore)))
    (should (equal emacs-reader--session-cookie "opaque-cookie"))
    (with-temp-buffer
      (insert (concat "HTTP/1.1 200 OK\r\n"
                      "Set-Cookie: reader.session=; Max-Age=0\r\n\r\n{}"))
      (cl-letf (((symbol-function 'emacs-reader--json-response)
                 (lambda (_status) '(:ok ((isSuccess . t))))))
        (emacs-reader--url-callback nil #'ignore)))
    (should-not emacs-reader--session-cookie)))

(ert-deftest emacs-reader-shelf-sorts-recent-and-renders-fields ()
  (let ((books '(((bookUrl . "old") (name . "Old") (author . "Author A")
                  (originName . "Source A") (durChapterTime . 10)
                  (durChapterIndex . 1) (totalChapterNum . 20)
                  (durChapterTitle . "Chapter 2"))
                 ((bookUrl . "new") (name . "New") (author . "Author B")
                  (originName . "Source B") (durChapterTime . 20)
                  (durChapterIndex . 3) (totalChapterNum . 40)
                  (durChapterTitle . "Chapter 4")))))
    (with-temp-buffer
      (emacs-reader-shelf-mode)
      (emacs-reader--handle-bookshelf-response
       (current-buffer) `(:ok ((isSuccess . t) (data . ,books))))
      (goto-char (point-min))
      (forward-line 1)
      (should (equal (alist-get 'bookUrl (emacs-reader--selected-shelf-book)) "new"))
      (should (string-match-p "New    Author B  ·  Source B\n" (buffer-string)))
      (should (string-match-p "  10%  4/40  Chapter 4" (buffer-string)))
      (emacs-reader-shelf-next)
      (should (equal (alist-get 'bookUrl (emacs-reader--selected-shelf-book)) "old"))
      (emacs-reader-shelf-next)
      (should (equal (alist-get 'bookUrl (emacs-reader--selected-shelf-book)) "old"))
      (emacs-reader-shelf-previous)
      (should (equal (alist-get 'bookUrl (emacs-reader--selected-shelf-book)) "new")))))

(ert-deftest emacs-reader-format-chapter-normalizes-paragraphs ()
  (should (equal (emacs-reader--format-chapter
                  "\u3000\u3000第一段。\r\n\n  \n\u3000\u3000第二段——乃是虞国公\n\u3000\u3000府的屈砚。 \n")
                 "\u3000\u3000第一段。\n\n\u3000\u3000第二段——乃是虞国公府的屈砚。")))

(ert-deftest emacs-reader-auth-source-returns-a-private-secret-copy ()
  (let ((emacs-reader-server-url "https://reader.example:8443")
        (emacs-reader-username "reader-user")
        (secret-string "test-only-secret")
        (lookup-arguments nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest arguments)
                 (setq lookup-arguments arguments)
                 (list (list :secret (lambda () secret-string))))))
      (let ((password (emacs-reader--auth-source-password)))
        (should (equal password "test-only-secret"))
        (should-not (eq password secret-string))
        (should (equal (plist-get lookup-arguments :host) "reader.example"))
        (should (equal (plist-get lookup-arguments :port) "8443"))
        (should (equal (plist-get lookup-arguments :user) "reader-user"))
        (clear-string password)))))

(ert-deftest emacs-reader-url-transport-works-against-a-loopback-fixture ()
  (let ((emacs-reader-allow-insecure-http t)
        (emacs-reader-max-response-bytes 1024)
        (url-proxy-services nil)
        (emacs-reader-test--http-response
         (concat "HTTP/1.1 200 OK\r\n"
                 "Content-Type: application/json\r\n"
                 "Content-Length: 11\r\n"
                 "Connection: close\r\n\r\n"
                 "{\"ok\":true}"))
        (emacs-reader-test--http-request "")
        (server nil)
        done result)
    (setq server
          (make-network-process :name "emacs-reader-test-server"
                                :family 'ipv4 :host "127.0.0.1" :service t
                                :server t :noquery t
                                :log #'emacs-reader-test--http-server-log))
    (unwind-protect
        (progn
          (let ((emacs-reader-server-url
                 (format "http://127.0.0.1:%d"
                         (process-contact server :service))))
            (emacs-reader--api-request
             "/api/v1/books" "GET" nil
             (lambda (value) (setq result value done t)))
            (let ((deadline (time-add (current-time) 3.0)))
              (while (and (not done) (time-less-p (current-time) deadline))
                (accept-process-output nil 0.05)))
            (should done)
            (should (= (plist-get result :status) 200))
            (should (eq (alist-get 'ok (plist-get result :ok)) t))
            (should-not (string-match-p "Authorization:" emacs-reader-test--http-request))))
      (when (process-live-p server)
        (delete-process server)))))

(ert-deftest emacs-reader-url-transport-bounds-loopback-responses ()
  (let ((emacs-reader-allow-insecure-http t)
        (emacs-reader-max-response-bytes 64)
        (url-proxy-services nil)
        (emacs-reader-test--http-response
         (concat "HTTP/1.1 200 OK\r\n"
                 "Content-Type: application/json\r\n"
                 "Content-Length: 70000\r\n"
                 "Connection: close\r\n\r\n"
                 (make-string 70000 ?x)))
        (emacs-reader-test--http-request "")
        (server nil)
        done result)
    (setq server
          (make-network-process :name "emacs-reader-large-test-server"
                                :family 'ipv4 :host "127.0.0.1" :service t
                                :server t :noquery t
                                :log #'emacs-reader-test--http-server-log))
    (unwind-protect
        (progn
          (let ((emacs-reader-server-url
                 (format "http://127.0.0.1:%d"
                         (process-contact server :service))))
            (emacs-reader--api-request
             "/api/v1/books" "GET" nil
             (lambda (value) (setq result value done t)))
            (let ((deadline (time-add (current-time) 3.0)))
              (while (and (not done) (time-less-p (current-time) deadline))
                (accept-process-output nil 0.05)))
            (should done)
            (should (eq (plist-get result :error) 'response-too-large))))
      (when (process-live-p server)
        (delete-process server)))))

(ert-deftest emacs-reader-url-transport-refuses-cross-origin-redirects ()
  (let ((emacs-reader-allow-insecure-http t)
        (url-proxy-services nil)
        (emacs-reader-test--http-request "")
        (server nil)
        (destination-server nil)
        done result)
    (setq destination-server
          (make-network-process :name "emacs-reader-redirect-target"
                                :family 'ipv4 :host "127.0.0.1" :service t
                                :server t :noquery t
                                :log #'emacs-reader-test--http-server-log))
    (setq server
          (make-network-process :name "emacs-reader-redirect-source"
                                :family 'ipv4 :host "127.0.0.1" :service t
                                :server t :noquery t
                                :log #'emacs-reader-test--http-server-log))
    (setq emacs-reader-test--http-response
          (format (concat "HTTP/1.1 302 Found\r\n"
                          "Location: http://127.0.0.1:%d/api/v1/redirect-target\r\n"
                          "Content-Length: 0\r\n"
                          "Connection: close\r\n\r\n")
                  (process-contact destination-server :service)))
    (unwind-protect
        (progn
          (let ((emacs-reader-server-url
                 (format "http://127.0.0.1:%d"
                         (process-contact server :service))))
            (emacs-reader--api-request
             "/api/v1/books" "GET" nil
             (lambda (value) (setq result value done t)) "test-only-token")
            (let ((deadline (time-add (current-time) 3.0)))
              (while (and (not done) (time-less-p (current-time) deadline))
                (accept-process-output nil 0.05)))
            (should done)
            (should (eq (plist-get result :error) 'redirect))
            (should (string-match-p "Authorization: Bearer test-only-token"
                                    emacs-reader-test--http-request))
            (should-not (string-match-p "redirect-target"
                                        emacs-reader-test--http-request))))
      (when (process-live-p server)
        (delete-process server))
      (when (process-live-p destination-server)
        (delete-process destination-server)))))

(ert-deftest emacs-reader-url-transport-disables-cookie-storage-and-redirects ()
  (let ((observed nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url _callback _arguments silent inhibit-cookies)
                 (setq observed
                       (list :redirect-limit url-max-redirections
                             :cookies inhibit-cookies
                             :silent silent
                             :privacy url-privacy-level
                             :history url-history-track))
                 nil)))
      (emacs-reader--url-transport
       '(:url "https://reader.example/api/v1/books"
         :method "GET" :headers nil)
       #'ignore))
    (should (equal (plist-get observed :redirect-limit) 0))
    (should (plist-get observed :cookies))
    (should (plist-get observed :silent))
    (should (eq (plist-get observed :privacy) 'high))
    (should-not (plist-get observed :history))))

(ert-deftest emacs-reader-login-prompts-and-retains-token-in-memory ()
  (let ((emacs-reader-server-url "http://reader.example:8084")
        (emacs-reader-username nil)
        (emacs-reader--token "old-token")
        saved captured-request payload)
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (if (string-match-p "URL" prompt)
                     "http://reader.example:8084"
                   "reader-user")))
              ((symbol-function 'customize-save-variable)
               (lambda (symbol value) (set symbol value)))
              ((symbol-function 'emacs-reader--save-password)
               (lambda (password) (push (copy-sequence password) saved)))
              ((symbol-function 'read-passwd)
               (lambda (&rest _) (copy-sequence "secret")))
              ((symbol-function 'emacs-reader--url-transport)
               (lambda (request callback)
                 (setq captured-request request
                       payload (json-parse-string
                                (decode-coding-string
                                 (plist-get request :data) 'utf-8)
                                :object-type 'alist))
                 (funcall callback
                          '(:status 200
                            :ok ((isSuccess . t)
                                 (data . ((accessToken . "temporary-token")))))))))
      (call-interactively #'emacs-reader-login))
    (should (equal saved '("secret")))
    (should (equal emacs-reader-username "reader-user"))
    (should (equal (plist-get captured-request :url)
                   "http://reader.example:8084/reader3/login"))
    (should (equal (alist-get 'username payload) "reader-user"))
    (should (equal (alist-get 'password payload) "secret"))
    (should (equal (alist-get 'code payload) ""))
    (should (eq (alist-get 'isLogin payload) t))
    (should-not (string-match-p "reader-user\\|secret\\|temporary-token"
                                (plist-get captured-request :url)))
    (should-not (assoc "Authorization" (plist-get captured-request :headers)))
    (should (equal emacs-reader--token "temporary-token"))))

(ert-deftest emacs-reader-login-failure-does-not-retain-token-or-server-message ()
  (let ((emacs-reader-server-url "http://reader.example:8084")
        (emacs-reader-username nil)
        (emacs-reader--token "old-token")
        saved messages)
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (if (string-match-p "URL" prompt)
                     "http://reader.example:8084"
                   "reader-user")))
              ((symbol-function 'customize-save-variable)
               (lambda (symbol value) (set symbol value)))
              ((symbol-function 'emacs-reader--save-password)
               (lambda (password) (push (copy-sequence password) saved)))
              ((symbol-function 'read-passwd)
               (lambda (&rest _) (copy-sequence "secret")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages)))
              ((symbol-function 'emacs-reader--url-transport)
               (lambda (_request callback)
                 (funcall callback
                          '(:status 200
                            :ok ((isSuccess . :false)
                                 (errorMsg . "private server detail")))))))
      (call-interactively #'emacs-reader-login))
    (should-not saved)
    (should-not emacs-reader--token)
    (should (equal messages '("Reader login failed; no session was retained")))))

(ert-deftest emacs-reader-need-login-relogs-silently-from-auth-source ()
  (let ((emacs-reader-server-url "http://reader.example:8084")
        (emacs-reader-username "reader-user")
        (emacs-reader--session-cookie (copy-sequence "expired"))
        paths result)
    (cl-letf (((symbol-function 'emacs-reader--auth-source-password)
               (lambda () (copy-sequence "stored")))
              ((symbol-function 'read-passwd)
               (lambda (&rest _) (error "Should not prompt")))
              ((symbol-function 'emacs-reader--api-request)
               (lambda (path _method data callback &rest _)
                 (push path paths)
                 (cond
                  ((string-suffix-p "login" path)
                   (should (equal (alist-get 'password data) "stored"))
                   (setq emacs-reader--session-cookie (copy-sequence "fresh"))
                   (funcall callback '(:ok ((isSuccess . t) (data . nil)))))
                  ((equal emacs-reader--session-cookie "fresh")
                   (funcall callback '(:ok ((isSuccess . t) (data . "ok")))))
                  (t
                   (funcall callback
                            '(:ok ((isSuccess . :false) (data . "NEED_LOGIN")))))))))
      (emacs-reader--reader3-request "getBookshelf" "GET" nil
                                     (lambda (response) (setq result response))))
    (should (equal (nreverse paths)
                   '("/reader3/getBookshelf" "/reader3/login" "/reader3/getBookshelf")))
    (should (equal (alist-get 'data (plist-get result :ok)) "ok"))))

(ert-deftest emacs-reader-reading-keys ()
  (dolist (binding '(("n" . scroll-up-command) ("SPC" . scroll-up-command)
                     ("p" . scroll-down-command) ("j" . scroll-up-line)
                     ("k" . scroll-down-line) ("d" . emacs-reader-scroll-half-up)
                     ("u" . emacs-reader-scroll-half-down)))
    (should (eq (lookup-key emacs-reader-reading-mode-map (kbd (car binding)))
                (cdr binding)))))

(ert-deftest emacs-reader-epub-chapter-loads-html-instead-of-showing-path ()
  (let ((emacs-reader-server-url "https://reader.example/reader")
        (emacs-reader--session-cookie "test-session")
        request)
    (with-temp-buffer
      (emacs-reader-reading-mode)
      (setq emacs-reader--current-book '((bookUrl . "local.epub") (name . "Test"))
            emacs-reader--chapter-request-id 1)
      (cl-letf (((symbol-function 'emacs-reader--url-transport)
                 (lambda (captured callback)
                   (setq request captured)
                   (funcall callback
                            '(:ok "<html><body><p>第一段。</p><p>第二段。</p></body></html>")))))
        (emacs-reader--handle-chapter-response
         (current-buffer) 1 2
         '(:ok ((isSuccess . t)
                (data . "/book-assets/user/测试 (书).epub/index/chapter3.html")))))
      (should (equal (plist-get request :url)
                     "https://reader.example/book-assets/user/%E6%B5%8B%E8%AF%95%20(%E4%B9%A6).epub/index/chapter3.html"))
      (should (equal (assoc "Cookie" (plist-get request :headers))
                     '("Cookie" . "reader.session=test-session")))
      (should (string-match-p "第一段。" (buffer-string)))
      (should (string-match-p "第二段。" (buffer-string)))
      (should-not (string-match-p "/book-assets/\\|<p>" (buffer-string)))
      (should (= emacs-reader--current-chapter-index 2)))))

(ert-deftest emacs-reader-book-asset-path-stays-on-the-reader-origin ()
  (let ((emacs-reader-server-url "https://reader.example/reader"))
    (should (equal (emacs-reader--book-asset-url "/book-assets/a%20b.epub/chapter.xhtml")
                   "https://reader.example/book-assets/a%20b.epub/chapter.xhtml"))
    (dolist (path '("https://other.example/book-assets/a.html"
                    "//other.example/book-assets/a.html"
                    "/book-assets/../private" "/book-assets/./private"
                    "/book-assets/%2E%2e/private" "/book-assets/%252e%252e/private"
                    "/book-assets/a%2fb" "/book-assets/a%5Cb"
                    "/book-assets/a\\b" "/book-assets/a?query=secret"
                    "/book-assets/a#fragment" "/book-assets/a%0d%0aInjected"
                    "/book-assets/a\nb" "/book-assets/a\rb"))
      (should-error (emacs-reader--book-asset-url path) :type 'user-error))))

(ert-deftest emacs-reader-html-rendering-keeps-paragraphs-without-network ()
  (cl-letf (((symbol-function 'url-retrieve)
             (lambda (&rest _) (ert-fail "HTML must not fetch external resources"))))
    (let ((text (emacs-reader--html-chapter-text
                 (concat "<html><head><title>Metadata</title>"
                         "<style>p {color:red}</style></head><body>"
                         "<h2>Chapter title</h2><p>First &amp; second。</p>"
                         "<script>untrusted-script</script>"
                         "<img src='https://other.example/image.png' alt='illustration'/>"
                         "<p>Last paragraph。</p></body></html>"))))
      (should (string-match-p "Chapter title\n\n　　First & second。" text))
      (should (string-match-p "Last paragraph。" text))
      (should-not (string-match-p "Metadata\\|untrusted-script\\|color:red" text))
      (should-not (text-properties-at 0 text)))))

(ert-deftest emacs-reader-epub-failure-and-stale-response-preserve-chapter ()
  (let ((emacs-reader-server-url "https://reader.example") callback)
    (with-temp-buffer
      (emacs-reader-reading-mode)
      (let ((inhibit-read-only t)) (insert "Existing chapter。"))
      (setq emacs-reader--current-book '((bookUrl . "local.epub"))
            emacs-reader--current-chapter-index 0
            emacs-reader--requested-chapter-index 1
            emacs-reader--chapter-request-id 1)
      (cl-letf (((symbol-function 'emacs-reader--url-transport)
                 (lambda (_request cb) (setq callback cb))))
        (emacs-reader--handle-chapter-response
         (current-buffer) 1 1
         '(:ok ((isSuccess . t) (data . "/book-assets/a.epub/chapter.html")))))
      (should callback)
      (funcall callback '(:error server :status 500))
      (should (equal (buffer-string) "Existing chapter。"))
      (should (= emacs-reader--current-chapter-index 0))
      (should (= emacs-reader--requested-chapter-index 0))
      (setq emacs-reader--chapter-request-id 2
            emacs-reader--requested-chapter-index 2
            mode-line-process " Loading...")
      (funcall callback '(:ok "<html><body><p>Stale chapter。</p></body></html>"))
      (should (equal (buffer-string) "Existing chapter。"))
      (should (= emacs-reader--requested-chapter-index 2))
      (should (equal mode-line-process " Loading..."))
      (cl-letf (((symbol-function 'emacs-reader--url-transport)
                 (lambda (&rest _) (ert-fail "Invalid asset must not be requested"))))
        (emacs-reader--handle-chapter-response
         (current-buffer) 2 2
         '(:ok ((isSuccess . t) (data . "/book-assets/../private")))))
      (should (equal (buffer-string) "Existing chapter。")))))

(ert-deftest emacs-reader-html-response-checks-size-type-and-redirects ()
  (dolist (mime '("text/html;charset=UTF-8" "application/xhtml+xml"))
    (with-temp-buffer
      (insert "HTTP/1.1 200 OK\r\n\r\n<p>Test</p>")
      (let ((url-http-response-status 200) (url-http-content-type mime)
            (url-http-content-length nil))
        (should (equal (plist-get (emacs-reader--http-response nil t) :ok)
                       "<p>Test</p>"))
        (let ((emacs-reader-max-response-bytes 2))
          (should (eq (plist-get (emacs-reader--http-response nil t) :error)
                      'response-too-large)))
        (let ((url-http-content-type "application/json"))
          (should (eq (plist-get (emacs-reader--http-response nil t) :error)
                      'unexpected-content-type)))
        (let ((url-http-response-status 302))
          (should (eq (plist-get (emacs-reader--http-response '(:redirect t) t) :error)
                      'redirect)))))))

(ert-deftest emacs-reader-epub-asset-loads-through-loopback-http ()
  (let* ((body (encode-coding-string "<html><body><p>本地章节。</p></body></html>" 'utf-8))
         (emacs-reader-test--http-response
          (concat (format "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n" (string-bytes body)) body))
         (emacs-reader-test--http-request "")
         (emacs-reader--session-cookie "fixture-cookie")
         (url-proxy-services nil)
         (server (make-network-process :name "reader-epub-fixture"
                                       :family 'ipv4 :host "127.0.0.1" :service t
                                       :server t :noquery t
                                       :log #'emacs-reader-test--http-server-log)))
    (unwind-protect
        (let ((emacs-reader-server-url
               (format "http://127.0.0.1:%d" (process-contact server :service))))
          (with-temp-buffer
            (emacs-reader-reading-mode)
            (setq emacs-reader--current-book '((bookUrl . "local.epub"))
                  emacs-reader--chapter-request-id 1)
            (emacs-reader--handle-chapter-response
             (current-buffer) 1 0
             '(:ok ((isSuccess . t) (data . "/book-assets/a.epub/chapter.html"))))
            (let ((deadline (time-add (current-time) 3.0)))
              (while (and (null emacs-reader--current-chapter-index)
                          (time-less-p (current-time) deadline))
                (accept-process-output nil 0.05)))
            (should (equal emacs-reader--current-chapter-index 0))
            (should (string-match-p "本地章节。" (buffer-string)))
            (should (string-match-p "GET /book-assets/a.epub/chapter.html"
                                    emacs-reader-test--http-request))
            (should (string-match-p "Cookie: reader.session=fixture-cookie"
                                    emacs-reader-test--http-request))))
      (delete-process server))))

(provide 'emacs-reader-test)

;;; emacs-reader-test.el ends here
