;;; emacs-reader-legado.el --- Native Legado book source rules -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Evaluate a declarative subset of Legado's rules using libxml and JSON.
;; HTML supports tag, .class, #id, descendant and child selectors, Legado
;; native class, id and tag steps with a zero-based index, and text/attribute output.
;; JSON paths support property names, array indices and [*].  URLs support
;; {{key}}, {{page}} and {{baseUrl}}.  Rule code is never run.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defvar url-http-response-status)

(defun emacs-reader-legado--unsupported (rule)
  "Report an unsupported RULE."
  (error "不支持的 Legado 规则 %S" rule))

(defun emacs-reader-legado--url (value base)
  "Resolve HTTP URL VALUE relative to BASE."
  (unless (and (stringp value) (not (string-empty-p (string-trim value))))
    (error "书源 URL 为空"))
  (when (string-match-p "{{\\|}}\\|,[[:space:]]*{\\|<js>\\|@js:" value)
    (emacs-reader-legado--unsupported value))
  (let ((url (url-expand-file-name (string-trim value) base)))
    (unless (string-match-p "\\`https?://[^/[:space:]]+" url)
      (error "书源 URL 需要 HTTP(S) 地址"))
    url))

(defun emacs-reader-legado--parse (body)
  "Return a tagged JSON or HTML document for BODY."
  (let ((body (string-trim-left body "[﻿[:space:]]+")))
    (if (string-match-p "\\`[[{]" body)
        (cons 'json (json-parse-string body :null-object nil :false-object nil))
      (unless (fboundp 'libxml-parse-html-region)
        (error "此 Emacs 需要 libxml2 才能解析 HTML 书源"))
      (cons 'html (with-temp-buffer
                    (insert body)
                    (libxml-parse-html-region (point-min) (point-max)))))))

(defun emacs-reader-legado--json-values (root rule)
  "Select a list of values below ROOT with JSON path RULE."
  (let* ((path (string-remove-prefix "$" (string-remove-prefix "@json:" rule)))
         (values (list root))
         (first t))
    (while (not (string-empty-p path))
      (cond
       ((or (string-match "\\`\\.\\([[:alnum:]_-]+\\)" path)
            (and first (string-match "\\`\\([[:alnum:]_-]+\\)" path)))
        (let ((key (match-string 1 path)))
          (setq path (substring path (match-end 0))
                values (delq nil (mapcar (lambda (v) (and (hash-table-p v)
                                                          (gethash key v)))
                                         values)))))
       ((string-match "\\`\\[\\(\\*\\|[0-9]+\\)\\]" path)
        (let ((index (match-string 1 path)))
          (setq path (substring path (match-end 0))
                values (cl-loop for v in values when (vectorp v) append
                                (if (equal index "*") (append v nil)
                                  (let ((i (string-to-number index)))
                                    (and (< i (length v)) (list (aref v i)))))))))
       (t (emacs-reader-legado--unsupported rule)))
      (setq first nil))
    (cl-loop for v in values append (if (vectorp v) (append v nil) (list v)))))

(defun emacs-reader-legado--selector (step)
  "Compile a simple CSS or Legado selector STEP."
  (let (index exclude tag tests)
    (when (string-match "\\([.!]\\)\\(-?[0-9]+\\)\\'" step)
      (setq index (string-to-number (match-string 2 step))
            exclude (equal (match-string 1 step) "!")
            step (substring step 0 (match-beginning 0))))
    (when (string-match "\\`\\(tag\\|class\\|id\\)\\.\\([[:alnum:]_-]+\\)\\'" step)
      (setq step (concat (pcase (match-string 1 step) ("class" ".") ("id" "#") (_ ""))
                         (match-string 2 step))))
    (when (string-match "\\`\\([[:alpha:]][[:alnum:]_-]*\\|\\*\\)" step)
      (setq tag (match-string 1 step) step (substring step (match-end 0))))
    (while (and (not (string-empty-p step))
                (cond ((string-match "\\`\\([.#]\\)\\([[:alnum:]_-]+\\)" step)
                       (push (cons (match-string 1 step) (match-string 2 step)) tests))
                      ((string-match (rx string-start "[" (group (+ (any alnum "-_:")))
                                         (optional "=" (optional (any "'\""))
                                                   (group (+ (any alnum "_:.-")))
                                                   (optional (any "'\""))) "]") step)
                       (push (cons (intern (match-string 1 step)) (match-string 2 step)) tests))))
      (setq step (substring step (match-end 0))))
    (unless (and (string-empty-p step) (or tag tests))
      (emacs-reader-legado--unsupported step))
    (list (lambda (node)
            (and (or (null tag) (equal tag "*") (equal tag (symbol-name (dom-tag node))))
                 (cl-every (lambda (test)
                             (pcase (car test)
                               ("#" (equal (cdr test) (dom-attr node 'id)))
                               ("." (member (cdr test)
                                            (split-string (or (dom-attr node 'class) ""))))
                               (attr (let ((value (dom-attr node attr)))
                                       (and value (or (null (cdr test))
                                                      (equal value (cdr test))))))))
                           tests)))
          index exclude)))

(defun emacs-reader-legado--select (nodes step child)
  "Select matches of STEP under NODES, limited to children if CHILD."
  (pcase-let ((`(,predicate ,index ,exclude) (emacs-reader-legado--selector step)))
    (cl-delete-duplicates
     (cl-loop for node in nodes append
              (let ((matches (if child
                                 (cl-remove-if-not predicate (dom-non-text-children node))
                               (cl-mapcan (lambda (n) (dom-search n predicate))
                                          (dom-non-text-children node)))))
                (if (null index) matches
                  (let* ((i (if (< index 0) (+ (length matches) index) index))
                         (selected (and (>= i 0) (nth i matches))))
                    (if exclude (delq selected matches) (and selected (list selected)))))))
     :test #'eq :from-end t)))

(defun emacs-reader-legado--text (node)
  "Return NODE text with paragraph and line boundaries."
  (cond ((stringp node) node)
        ((memq (dom-tag node) '(script style comment)) "")
        ((eq (dom-tag node) 'br) "\n")
        (t (concat (mapconcat #'emacs-reader-legado--text (dom-children node) "")
                   (if (memq (dom-tag node) '(p div li section)) "\n" "")))))

(defun emacs-reader-legado--html-values (root rule rows)
  "Select HTML elements or field values from ROOT using RULE and ROWS."
  (let* ((rule (string-remove-prefix "@css:" rule))
         (parts (split-string rule "@"))
         (field (unless rows
                  (if (or (cdr parts)
                          (member rule '("text" "ownText" "textNodes" "href"
                                         "src" "content" "title" "html" "all")))
                      (car (last parts)) "text")))
         (nodes (list root)))
    (when (and field (equal field (car (last parts))))
      (setq parts (butlast parts))
      (when (equal parts '("")) (setq parts nil)))
    (dolist (part parts)
      (let ((steps (split-string (replace-regexp-in-string ">" " > " part) nil t)) child)
        (unless steps (emacs-reader-legado--unsupported rule))
        (dolist (step steps)
          (if (equal step ">")
              (if child (emacs-reader-legado--unsupported rule) (setq child t))
            (setq nodes (emacs-reader-legado--select nodes step child) child nil)))
        (when child (emacs-reader-legado--unsupported rule))))
    (cond (rows nodes)
          ((member field '("text" "html")) (mapcar #'emacs-reader-legado--text nodes))
          ((member field '("ownText" "textNodes"))
           (mapcar (lambda (node)
                     (mapconcat #'identity (cl-remove-if-not #'stringp (dom-children node)) "\n"))
                   nodes))
          ((and (not (member field '("all" "js")))
                (string-match-p "\\`[[:alpha:]][[:alnum:]_-]*\\'" field))
           (delq nil (mapcar (lambda (node) (dom-attr node (intern field))) nodes)))
          (t (emacs-reader-legado--unsupported rule)))))

(defun emacs-reader-legado--regexp (pattern)
  "Translate common Java regexp constructs in field PATTERN to Emacs syntax."
  (let ((i 0) in-class chunks)
    (while (< i (length pattern))
      (let ((ch (aref pattern i)))
        (push
         (cond
          ((eq ch ?\\)
           (when (= (cl-incf i) (length pattern)) (emacs-reader-legado--unsupported pattern))
           (let ((escaped (aref pattern i)))
             (pcase escaped
               (?s (if in-class "[:space:]" "[[:space:]]"))
               (?d (if in-class "0-9" "[0-9]"))
               ((or ?\( ?\) ?\{ ?\} ?| ?- ?/ ?_) (char-to-string escaped))
               ((or ?. ?* ?+ ?? ?^ ?$ ?\[ ?\] ?\\) (concat "\\" (char-to-string escaped)))
               (_ (emacs-reader-legado--unsupported pattern)))))
          ((and (not in-class) (eq ch ?\() (< (1+ i) (length pattern))
                (eq (aref pattern (1+ i)) ??))
           (emacs-reader-legado--unsupported pattern))
          ((eq ch ?\[) (setq in-class t) "[")
          ((eq ch ?\]) (setq in-class nil) "]")
          ((and (not in-class) (memq ch '(?\( ?\) ?\{ ?\} ?|)))
           (concat "\\" (char-to-string ch)))
          (t (char-to-string ch)))
         chunks))
      (cl-incf i))
    (apply #'concat (nreverse chunks))))

(defun emacs-reader-legado--match-rule (rule)
  "Parse RULE ending in a literal result.match(/regexp/)[index] expression."
  (when-let* ((start (string-search "@js:" rule)))
    (let ((expression (substring rule start)))
      (unless (string-match
               (rx string-start "@js:result.match(/"
                   (group (+ (or (not (any "/\\\r\n")) (seq "\\" (not (any "\r\n"))))))
                   "/)[" (group (+ digit)) "]" string-end)
               expression)
        (emacs-reader-legado--unsupported rule))
      (let* ((index (string-to-number (match-string 2 expression)))
             (pattern (emacs-reader-legado--regexp (match-string 1 expression))))
        (unless (<= index (regexp-opt-depth pattern)) (emacs-reader-legado--unsupported rule))
        (list (substring rule 0 start) pattern index)))))

(defun emacs-reader-legado--json-url-values (root rule)
  "Expand a single JSON scalar as the final path component of RULE below ROOT."
  (unless (string-match (rx string-start (group "/" (+ (any alnum "-_/")) "/")
                            "{{" (group "$" (+ (seq "." (+ (any alnum "_-"))))) "}}" string-end)
                        rule)
    (emacs-reader-legado--unsupported rule))
  (let* ((prefix (match-string 1 rule))
         (values (emacs-reader-legado--json-values root (match-string 2 rule)))
         (value (car values)))
    (when (and (not (string-prefix-p "//" prefix)) (= (length values) 1)
               (or (numberp value) (stringp value)) (not (member value '("" "." ".."))))
      (list (concat prefix (url-hexify-string (format "%s" value)))))))

(defun emacs-reader-legado--values (doc rule &optional rows)
  "Evaluate RULE in DOC, returning elements for ROWS or strings otherwise."
  (unless (and (stringp rule) (not (string-blank-p rule)))
    (error "缺少 Legado 提取规则"))
  (let* ((case-fold-search nil)
         (parts (split-string rule "##" nil))
         (path (car parts))
         (capture (and (not rows) (emacs-reader-legado--match-rule path)))
         (path (or (car capture) path)))
    (when (or (string-match-p "@js:\\|<js>\\|&&\\|%%\\|@put:\\|@get:" path)
              (and (cdr parts) (or rows (string-empty-p (cadr parts)) (> (length parts) 3))))
      (emacs-reader-legado--unsupported rule))
    (let* ((values (cond ((string-search "||" path)
                          (cl-loop for option in (split-string path (regexp-quote "||"))
                                   thereis (emacs-reader-legado--values doc option rows)))
                         ((and (eq (car doc) 'json) (not rows) (string-search "{{" path))
                          (emacs-reader-legado--json-url-values (cdr doc) path))
                         ((eq (car doc) 'json) (emacs-reader-legado--json-values (cdr doc) path))
                         (t (emacs-reader-legado--html-values (cdr doc) path rows))))
           (pattern (and (cdr parts) (emacs-reader-legado--regexp (cadr parts))))
           (replacement (and pattern (replace-regexp-in-string
                                      "\\$\\([1-9]\\)" "\\\\\\1" (or (nth 2 parts) "")))))
      (if rows values
        (cl-loop for value in values when (or (numberp value) (stringp value))
                 for text = (format "%s" value)
                 for extracted = (if capture
                                     (and (string-match (nth 1 capture) text)
                                          (match-string (nth 2 capture) text))
                                   text)
                 when extracted
                 for cleaned = (string-trim (if pattern
                                                (replace-regexp-in-string
                                                 pattern replacement extracted t)
                                              extracted))
                 unless (string-empty-p cleaned) collect cleaned)))))

(defun emacs-reader-legado--field (doc rules key)
  "Read the first value for KEY in RULES from DOC."
  (when-let* ((rule (plist-get rules key)))
    (unless (equal rule "") (car (emacs-reader-legado--values doc rule)))))

(defun emacs-reader-legado--request-object (text)
  "Read a JSON object from TEXT, also accepting single-quoted strings."
  (unless (string-match-p "\\`[[:space:]]*{" text)
    (error "书源请求配置需要对象"))
  (when (string-search "'" text)
    (let (chars delimiter (i 0))
      (while (< i (length text))
        (let ((ch (aref text i)))
          (cond
           ((and delimiter (= ch ?\\) (< (1+ i) (length text)))
            (setq i (1+ i) ch (aref text i))
            (unless (and (= delimiter ?') (= ch ?')) (push ?\\ chars))
            (push ch chars))
           ((and delimiter (= delimiter ch)) (push ?\" chars) (setq delimiter nil))
           ((and (null delimiter) (memq ch '(?' ?\"))) (push ?\" chars) (setq delimiter ch))
           ((and (eq delimiter ?') (= ch ?\")) (push ?\\ chars) (push ch chars))
           (t (push ch chars))))
        (setq i (1+ i)))
      (setq text (concat (nreverse chars)))))
  (condition-case nil (json-parse-string text :object-type 'plist)
    (error (error "书源请求配置有无效的 JSON 对象"))))

(defconst emacs-reader-legado--url-script-prefix "\\`[[:space:]]*\\(?:@js:\\|<js>\\)"
  "Prefix used by Legado URL scripts.")

(defun emacs-reader-legado--request-parts (target)
  "Split TARGET into its URL template and parsed request options."
  (when (and (stringp target) (string-match-p emacs-reader-legado--url-script-prefix target))
    (error "不支持 JavaScript/WebView 书源 URL"))
  (if (and (stringp target) (string-match ",[[:space:]]*\\({\\)" target))
      (cons (substring target 0 (match-beginning 0))
            (emacs-reader-legado--request-object (substring target (match-beginning 1))))
    (list target)))

(defun emacs-reader-legado--request-charset (options)
  "Return the explicit coding system named in OPTIONS, if any."
  (when-let* ((name (plist-get options :charset)))
    (or (and (stringp name) (coding-system-from-name (downcase name)))
        (emacs-reader-legado--unsupported name))))

(defun emacs-reader-legado--headers (headers &optional expand)
  "Convert static HEADERS to an HTTP header alist, using EXPAND on values."
  (let (result)
    (when (stringp headers)
      (setq headers (unless (string-blank-p headers)
                      (condition-case nil (emacs-reader-legado--request-object headers)
                        (error (error "不支持的书源请求头格式"))))))
    (unless (proper-list-p headers) (error "不支持的书源请求头格式"))
    (while headers
      (unless (and (keywordp (car headers)) (cdr headers)) (error "不支持的书源请求头字段"))
      (let ((name (substring (symbol-name (pop headers)) 1)) (value (pop headers)))
        (when (and expand (stringp value)) (setq value (funcall expand value)))
        (unless (and (string-match-p "\\`[[:alnum:]_-]+\\'" name) (stringp value)
                     (not (string-match-p "[\r\n]\\|{{\\|@js:\\|<js>" value)))
          (error "不支持的书源请求头字段"))
        (push (cons name value) result)))
    result))

(defun emacs-reader-legado--response (status &optional fallback)
  "Decode the HTTP response for STATUS, using FALLBACK if charset is absent."
  (cond ((and (integerp url-http-response-status) (not (<= 200 url-http-response-status 299)))
         (if (= url-http-response-status 403) (error "HTTP 403：书源站点拒绝了搜索或阅读请求")
           (error "书源返回 HTTP %d" url-http-response-status)))
        ((plist-get status :error) (error "书源连接失败：%S" (plist-get status :error)))
        ((null url-http-response-status)
         (error "连接结束时未收到 HTTP 状态码（收到 %d 字节）" (buffer-size))))
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t) (error "书源响应缺少 HTTP 头"))
  (let* ((start (point))
         (charset (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward
                           "^Content-Type:.*?charset=[\"']?\\([[:alnum:]_-]+\\)" start t)
                      (intern-soft (downcase (match-string 1))))))
         (body (decode-coding-string (buffer-substring-no-properties start (point-max))
                                     (if (and charset (coding-system-p charset)) charset
                                       (or fallback 'utf-8)))))
    (when (string-blank-p body) (error "书源返回了空正文"))
    (condition-case nil (emacs-reader-legado--parse body)
      (json-error (error "书源响应正文不是有效的 JSON")))))

(defun emacs-reader-legado--origin (address)
  "Return normalized HTTP scheme, host and effective port for ADDRESS."
  (when (stringp address)
    (let ((url (url-generic-parse-url address)))
      (when (and (member (url-type url) '("http" "https")) (url-host url))
        (list (url-type url) (downcase (url-host url)) (url-port url))))))

(defun emacs-reader-legado--search-expander (search base charset)
  "Build a template expander for SEARCH (query page), BASE and CHARSET."
  (let ((key (url-hexify-string (encode-coding-string (car search) (or charset 'utf-8))))
        (page (number-to-string (cadr search))))
    (lambda (text)
      (string-replace "{{key}}" key
                      (string-replace "{{page}}" page (string-replace "{{baseUrl}}" base text))))))

(defun emacs-reader-legado--fetch (source target base callback &optional search)
  "Fetch TARGET relative to BASE for SOURCE using optional SEARCH substitutions.
Call CALLBACK with DOC, final URL, ERROR and the originally requested URL."
  (let (delivered)
    (condition-case err
        (let* ((parts (emacs-reader-legado--request-parts target))
               (options (cdr parts))
               (charset (emacs-reader-legado--request-charset options))
               (expand (if search (emacs-reader-legado--search-expander search (or base "") charset)
                         #'identity))
               (url (emacs-reader-legado--url (funcall expand (car parts)) base))
               (url-request-method (upcase (or (plist-get options :method) "GET")))
               (body (plist-get options :body))
               (url-request-extra-headers (emacs-reader-legado--headers (plist-get source :header)))
               (url-request-data
                (and body (encode-coding-string
                           (funcall expand (if (stringp body) body (json-serialize body)))
                           (if (stringp body) (or charset 'utf-8) 'utf-8)))))
          (cl-loop for (key _) on options by #'cddr
                   unless (memq key '(:method :body :header :headers :charset))
                   do (emacs-reader-legado--unsupported key))
          (unless (member url-request-method '("GET" "POST"))
            (emacs-reader-legado--unsupported url-request-method))
          (when (and body (or (equal url-request-method "GET")
                              (string-match-p "{{\\|@js:\\|<js>" url-request-data)))
            (emacs-reader-legado--unsupported body))
          (dolist (entry (emacs-reader-legado--headers
                          (or (plist-get options :headers) (plist-get options :header)) expand))
            (let ((old (assoc-string (car entry) url-request-extra-headers t)))
              (if old (setcdr old (cdr entry)) (push entry url-request-extra-headers))))
          (when (and url-request-data
                     (not (assoc-string "Content-Type" url-request-extra-headers t)))
            (push (cons "Content-Type" (if (stringp body) "application/x-www-form-urlencoded"
                                         "application/json")) url-request-extra-headers))
          (let* ((home (emacs-reader-legado--origin (plist-get source :bookSourceUrl)))
                 (credentials (cl-some (lambda (key) (assoc-string key url-request-extra-headers t))
                                       '("Cookie" "Authorization"
                                         "Proxy-Authorization" "X-API-Key")))
                 (url-max-redirections (if credentials 0 url-max-redirections)))
            (when (and credentials
                       (not (equal (emacs-reader-legado--origin url) home)))
              (error "带凭据的书源请求需要同源地址"))
            (url-retrieve
             url (lambda (status)
                   (let ((buffer (current-buffer))
                         (final (if url-current-object (url-recreate-url url-current-object) url))
                         doc message)
                     (unwind-protect
                         (condition-case problem
                             (if (and credentials (<= 300 (or url-http-response-status 0) 399))
                                 (error "带凭据的书源请求禁止自动重定向")
                               (setq doc (emacs-reader-legado--response status charset)))
                           (error (setq message (error-message-string problem))))
                       (kill-buffer buffer))
                     (setq delivered t)
                     (funcall callback doc final message url)))
             nil t t)))
      (error (if delivered (signal (car err) (cdr err))
               (funcall callback nil nil (error-message-string err) nil))))))

(defun emacs-reader-legado--map-page (source target base callback parser &optional search)
  "Fetch TARGET relative to BASE using SOURCE and run PARSER before CALLBACK.
Pass SEARCH to `emacs-reader-legado--fetch' for search substitutions."
  (emacs-reader-legado--fetch
   source target base
   (lambda (doc url message requested)
     (let (value)
       (unless message
         (condition-case err (setq value (funcall parser doc url requested))
           (error (setq message (error-message-string err)))))
       (funcall callback value message)))
   search))

(defun emacs-reader-legado--rows (doc rules key build)
  "Map BUILD over rows selected from DOC by KEY in RULES."
  (let* ((rule (plist-get rules key))
         (reverse-order (and (stringp rule) (string-prefix-p "-" rule)))
         (path (if (and (stringp rule) (or reverse-order (string-prefix-p "+" rule)))
                   (substring rule 1) rule))
         (nodes (emacs-reader-legado--values doc path t)))
    (mapcar (lambda (node) (funcall build (cons (car doc) node)))
            (if reverse-order (reverse nodes) nodes))))

(defun emacs-reader-legado--direct-result (source doc url query)
  "Read a redirected detail page at URL when SOURCE identifies QUERY in DOC."
  (let ((rules (plist-get source :ruleBookInfo)))
    (when-let* ((name-rule (plist-get rules :name))
                (detail (if-let* ((init (plist-get rules :init)) ((not (equal init ""))))
                            (when-let* ((node (car (emacs-reader-legado--values doc init t))))
                              (cons (car doc) node))
                          doc))
                (name (car (emacs-reader-legado--values detail name-rule)))
                ((equal name (string-trim query))))
      (list (list :bookUrl url :name name :author (emacs-reader-legado--field detail rules :author)
                  :origin (plist-get source :bookSourceUrl)
                  :originName (plist-get source :bookSourceName))))))

(defun emacs-reader-legado-search (source query callback &optional page)
  "Search SOURCE for QUERY on one-based PAGE and call CALLBACK with books/error."
  (let ((template (plist-get source :searchUrl)) (page (or page 1))
        (origin (plist-get source :bookSourceUrl)) (rules (plist-get source :ruleSearch)))
    (cond ((not (stringp template)) (funcall callback nil "书源缺少 searchUrl"))
          ((and (> page 1) (not (string-search "{{page}}" template))
                (not (string-match-p emacs-reader-legado--url-script-prefix template)))
           (funcall callback nil nil))
          (t
           (emacs-reader-legado--map-page
            source template origin callback
            (lambda (doc url requested)
              (or (emacs-reader-legado--rows
                   doc rules :bookList
                   (lambda (row)
                     (list :bookUrl (emacs-reader-legado--url
                                     (emacs-reader-legado--field row rules :bookUrl) url)
                           :name (or (emacs-reader-legado--field row rules :name)
                                     (error "搜索结果缺少书名"))
                           :author (emacs-reader-legado--field row rules :author)
                           :origin origin :originName (plist-get source :bookSourceName))))
                  (when (and (= page 1) (not (equal url requested)))
                    (emacs-reader-legado--direct-result source doc url query))))
            (list query page))))))

(defun emacs-reader-legado--info (doc url book rules)
  "Extract BOOK details from DOC at URL using RULES."
  (let ((info (copy-sequence book)) (init (plist-get rules :init)))
    (when (and init (not (equal init "")))
      (setq doc (cons (car doc) (car (emacs-reader-legado--values doc init t)))))
    (dolist (key '(:name :author :intro :kind :lastChapter :wordCount :coverUrl))
      (when-let* ((value (emacs-reader-legado--field doc rules key)))
        (setq info (plist-put info key
                              (if (eq key :coverUrl) (emacs-reader-legado--url value url) value)))))
    (plist-put info :tocUrl
               (if-let* ((toc (emacs-reader-legado--field doc rules :tocUrl)))
                   (emacs-reader-legado--url toc url) url))))

(defun emacs-reader-legado-book-info (source book callback)
  "Read details of BOOK from SOURCE; call CALLBACK with updated book/error."
  (emacs-reader-legado--map-page
   source (plist-get book :bookUrl) (plist-get source :bookSourceUrl) callback
   (lambda (doc url _requested)
     (emacs-reader-legado--info doc url book (plist-get source :ruleBookInfo)))))

(defun emacs-reader-legado--catalog (source doc url)
  "Read the catalog from DOC at URL according to SOURCE."
  (let ((rules (plist-get source :ruleToc)))
    (emacs-reader-legado--rows
     doc rules :chapterList
     (lambda (row)
       (list :title (or (emacs-reader-legado--field row rules :chapterName) (error "目录缺少章节名"))
             :url (emacs-reader-legado--url
                   (emacs-reader-legado--field row rules :chapterUrl) url))))))

(defun emacs-reader-legado--pages (source target first-doc next-rule read-page callback)
  "Collect lists from READ-PAGE, following NEXT-RULE in SOURCE.
Start at TARGET with optional FIRST-DOC and call CALLBACK with the complete list."
  (let ((seen (make-hash-table :test #'equal)) pieces pending)
    (cl-labels
        ((consume (doc url message &optional _requested)
           (let (next)
             (unless message
               (condition-case err
                   (progn
                     (when (gethash url seen) (error "书源分页链接循环 %s" url))
                     (puthash url t seen)
                     (push (funcall read-page doc url) pieces)
                     (when (and next-rule (not (equal next-rule "")))
                       (let (links)
                         (dolist (value (emacs-reader-legado--values doc next-rule))
                           (let ((link (emacs-reader-legado--url value url)))
                             (when (gethash link seen) (error "书源分页链接循环 %s" link))
                             (push link links)))
                         (setq pending (delete-dups (append (nreverse links) pending)))))
                     (setq next (pop pending)))
                 (error (setq message (error-message-string err)))))
             (cond (message (funcall callback nil message))
                   (next (emacs-reader-legado--fetch source next url #'consume))
                   (t (funcall callback (apply #'append (nreverse pieces)) nil))))))
      (if first-doc (consume first-doc target nil)
        (emacs-reader-legado--fetch source target (plist-get source :bookSourceUrl) #'consume)))))

(defun emacs-reader-legado-chapters (source book callback)
  "Read BOOK's catalog from SOURCE; call CALLBACK with chapters/error."
  (let ((toc (plist-get book :tocUrl))
        (next (plist-get (plist-get source :ruleToc) :nextTocUrl))
        (read-page (apply-partially #'emacs-reader-legado--catalog source)))
    (if toc (emacs-reader-legado--pages source toc nil next read-page callback)
      (emacs-reader-legado--map-page
       source (plist-get book :bookUrl) (plist-get source :bookSourceUrl)
       (lambda (value message)
         (if message (funcall callback nil message)
           (emacs-reader-legado--pages source (car value) (cdr value) next read-page callback)))
       (lambda (doc url _requested)
         (let* ((info (emacs-reader-legado--info doc url book (plist-get source :ruleBookInfo)))
                (target (plist-get info :tocUrl)))
           (cons target (and (equal target url) doc))))))))

(defun emacs-reader-legado-content (source chapter callback)
  "Read CHAPTER from SOURCE; call CALLBACK with its text/error."
  (let ((rules (plist-get source :ruleContent)))
    (emacs-reader-legado--pages
     source (plist-get chapter :url) nil (plist-get rules :nextContentUrl)
     (lambda (doc _url)
       (when-let* ((regex (plist-get rules :replaceRegex)))
         (unless (equal regex "") (emacs-reader-legado--unsupported regex)))
       (or (emacs-reader-legado--values doc (plist-get rules :content))
           (error "正文规则未提取到文本")))
     (lambda (text message) (funcall callback (and text (string-join text "\n")) message)))))

(provide 'emacs-reader-legado)
;;; emacs-reader-legado.el ends here
