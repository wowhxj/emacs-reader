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
  (let (index tag tests)
    (when (string-match
           "\\`\\(tag\\|class\\|id\\)\\.\\([[:alnum:]_-]+\\)\\(?:\\.\\(-?[0-9]+\\)\\)?\\'" step)
      (setq index (and (match-string 3 step) (string-to-number (match-string 3 step)))
            step (concat (pcase (match-string 1 step) ("class" ".") ("id" "#") (_ ""))
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
          index)))

(defun emacs-reader-legado--select (nodes step child)
  "Select matches of STEP under NODES, limited to children if CHILD."
  (pcase-let ((`(,predicate ,index) (emacs-reader-legado--selector step)))
    (cl-delete-duplicates
     (cl-loop for node in nodes append
              (let ((matches (if child
                                 (cl-remove-if-not predicate (dom-non-text-children node))
                               (cl-mapcan (lambda (n) (dom-search n predicate))
                                          (dom-non-text-children node)))))
                (if index (let ((i (if (< index 0) (+ (length matches) index) index)))
                            (and (>= i 0) (delq nil (list (nth i matches)))))
                  matches)))
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
      (setq parts (butlast parts)))
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

(defun emacs-reader-legado--values (doc rule &optional rows)
  "Evaluate RULE in DOC, returning elements for ROWS or strings otherwise."
  (unless (and (stringp rule) (not (string-blank-p rule)))
    (error "缺少 Legado 提取规则"))
  (let ((case-fold-search nil))
    (when (string-match-p "@js:\\|<js>\\|{{\\|##\\|&&\\|%%\\|@put:\\|@get:" rule)
      (emacs-reader-legado--unsupported rule))
    (if (string-match-p (regexp-quote "||") rule)
        (cl-loop for option in (split-string rule (regexp-quote "||"))
                 thereis (emacs-reader-legado--values doc option rows))
      (let ((values (if (eq (car doc) 'json)
                        (emacs-reader-legado--json-values (cdr doc) rule)
                      (emacs-reader-legado--html-values (cdr doc) rule rows))))
        (if rows values
          (cl-loop for value in values when (or (numberp value) (stringp value))
                   for text = (string-trim (format "%s" value))
                   unless (string-empty-p text) collect text))))))

(defun emacs-reader-legado--field (doc rules key)
  "Read the first value for KEY in RULES from DOC."
  (when-let* ((rule (plist-get rules key)))
    (unless (equal rule "") (car (emacs-reader-legado--values doc rule)))))

(defun emacs-reader-legado--headers (headers)
  "Convert static HEADERS to an HTTP header alist."
  (let (result)
    (when (and (stringp headers) (not (string-blank-p headers)))
      (setq headers (condition-case nil
                        (json-parse-string headers :object-type 'plist)
                      (error (emacs-reader-legado--unsupported headers)))))
    (unless (or (listp headers) (equal headers ""))
      (emacs-reader-legado--unsupported headers))
    (while (consp headers)
      (let ((name (substring (symbol-name (pop headers)) 1)) (value (pop headers)))
        (unless (and (string-match-p "\\`[[:alnum:]_-]+\\'" name) (stringp value)
                     (not (string-match-p "[\r\n]\\|{{\\|@js:\\|<js>" value)))
          (emacs-reader-legado--unsupported name))
        (push (cons name value) result)))
    result))

(defun emacs-reader-legado--response (status)
  "Decode the current HTTP response or signal an error for STATUS."
  (when (or (plist-get status :error) (not (<= 200 (or url-http-response-status 0) 299)))
    (error "书源请求失败 %s" (or url-http-response-status (plist-get status :error))))
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t) (error "书源响应缺少 HTTP 头"))
  (let* ((start (point))
         (charset (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward
                           "^Content-Type:.*?charset=[\"']?\\([[:alnum:]_-]+\\)" start t)
                      (intern-soft (downcase (match-string 1))))))
         (body (decode-coding-string (buffer-substring-no-properties start (point-max))
                                     (if (and charset (coding-system-p charset)) charset 'utf-8))))
    (emacs-reader-legado--parse body)))

(defun emacs-reader-legado--origin (address)
  "Return normalized HTTP scheme, host and effective port for ADDRESS."
  (when (stringp address)
    (let ((url (url-generic-parse-url address)))
      (when (and (member (url-type url) '("http" "https")) (url-host url))
        (list (url-type url) (downcase (url-host url)) (url-port url))))))

(defun emacs-reader-legado--fetch (source target base callback)
  "Fetch TARGET relative to BASE for SOURCE; call CALLBACK with DOC URL ERROR."
  (let (delivered)
    (condition-case err
        (let* ((options (when (and (stringp target) (string-match ",[[:space:]]*\\({\\)" target))
                          (let ((json (substring target (match-beginning 1))))
                            (setq target (substring target 0 (match-beginning 0)))
                            (json-parse-string json :object-type 'plist))))
               (url (emacs-reader-legado--url target base))
               (url-request-method (upcase (or (plist-get options :method) "GET")))
               (body (plist-get options :body))
               (url-request-extra-headers (emacs-reader-legado--headers (plist-get source :header)))
               (url-request-data (and body (encode-coding-string
                                            (if (stringp body) body (json-serialize body))
                                            'utf-8))))
          (cl-loop for (key _) on options by #'cddr
                   unless (memq key '(:method :body :header :headers))
                   do (emacs-reader-legado--unsupported key))
          (unless (member url-request-method '("GET" "POST"))
            (emacs-reader-legado--unsupported url-request-method))
          (when (and body (or (equal url-request-method "GET")
                              (string-match-p "{{\\|@js:\\|<js>" url-request-data)))
            (emacs-reader-legado--unsupported body))
          (dolist (entry (emacs-reader-legado--headers (or (plist-get options :headers)
                                                           (plist-get options :header))))
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
                               (setq doc (emacs-reader-legado--response status)))
                           (error (setq message (error-message-string problem))))
                       (kill-buffer buffer))
                     (setq delivered t)
                     (funcall callback doc final message)))
             nil t t)))
      (error (if delivered (signal (car err) (cdr err))
               (funcall callback nil nil (error-message-string err)))))))

(defun emacs-reader-legado--map-page (source target base callback parser)
  "Fetch TARGET relative to BASE using SOURCE and run PARSER before CALLBACK."
  (emacs-reader-legado--fetch
   source target base
   (lambda (doc url message)
     (let (value)
       (unless message
         (condition-case err (setq value (funcall parser doc url))
           (error (setq message (error-message-string err)))))
       (funcall callback value message)))))

(defun emacs-reader-legado--rows (doc rules key build)
  "Map BUILD over rows selected from DOC by KEY in RULES."
  (let* ((rule (plist-get rules key))
         (reverse-order (and (stringp rule) (string-prefix-p "-" rule)))
         (path (if (and (stringp rule) (or reverse-order (string-prefix-p "+" rule)))
                   (substring rule 1) rule))
         (nodes (emacs-reader-legado--values doc path t)))
    (mapcar (lambda (node) (funcall build (cons (car doc) node)))
            (if reverse-order (reverse nodes) nodes))))

(defun emacs-reader-legado-search (source query callback &optional page)
  "Search SOURCE for QUERY on one-based PAGE and call CALLBACK with books/error."
  (let ((template (plist-get source :searchUrl)) (page (or page 1))
        (origin (plist-get source :bookSourceUrl)) (rules (plist-get source :ruleSearch)))
    (cond ((not (stringp template)) (funcall callback nil "书源缺少 searchUrl"))
          ((and (> page 1) (not (string-search "{{page}}" template)))
           (funcall callback nil nil))
          (t
           (setq template (string-replace "{{baseUrl}}" (or origin "") template)
                 template (string-replace "{{page}}" (number-to-string page) template)
                 template (string-replace "{{key}}" (url-hexify-string query) template))
           (emacs-reader-legado--map-page
            source template origin callback
            (lambda (doc url)
              (emacs-reader-legado--rows
               doc rules :bookList
               (lambda (row)
                 (list :bookUrl (emacs-reader-legado--url
                                 (emacs-reader-legado--field row rules :bookUrl) url)
                       :name (or (emacs-reader-legado--field row rules :name) (error "搜索结果缺少书名"))
                       :author (emacs-reader-legado--field row rules :author)
                       :origin origin :originName (plist-get source :bookSourceName))))))))))

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
   (lambda (doc url) (emacs-reader-legado--info doc url book (plist-get source :ruleBookInfo)))))

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
        ((consume (doc url message)
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
       (lambda (doc url)
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
