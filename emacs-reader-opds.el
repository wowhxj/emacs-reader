;;; emacs-reader-opds.el --- Browse public book catalogs -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Browse Atom and JSON OPDS catalogs, and configurable JSON book directories.
;; Catalog requests and acquisition downloads use the asynchronous URL library.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'seq)

(defvar url-http-response-status)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'xml)

(autoload 'emacs-reader-import-file "emacs-reader-local")

(cl-defstruct (emacs-reader-opds--item (:constructor emacs-reader-opds--item))
  label kind url title author extension action pagination)

(defun emacs-reader-opds--http (href base)
  "Resolve HREF against BASE and accept only HTTP(S) book catalog URLs."
  (when (and (stringp href) (not (string-empty-p href)))
    (let ((url (url-expand-file-name href base)))
      (when (string-match-p "\\`https?://[^/[:space:]]" url) url))))

(defun emacs-reader-opds--format (url mime)
  "Return the importable format of URL with declared MIME type."
  (let ((type (car (split-string (or mime "") ";"))))
    (cond ((equal type "application/epub+zip") "epub")
          ((equal type "text/plain") "txt")
          ((member type '(nil "" "application/octet-stream"))
           (let ((ext (downcase (or (file-name-extension
                                     (car (split-string url "[?#]"))) ""))))
             (and (member ext '("epub" "txt")) ext))))))

(defconst emacs-reader-opds--feed-type
  "application/\\(?:atom\\+xml\\|opds\\+json\\|opds-publication\\+json\\)"
  "Media types accepted as OPDS navigation targets.")

(defun emacs-reader-opds--link (href rel mime label base &optional title author nav)
  "Normalize a catalog link with HREF, REL, MIME and LABEL relative to BASE.
TITLE and AUTHOR describe the publication.  NAV indicates a navigation entry."
  (when-let* ((url (emacs-reader-opds--http href base)))
    (let* ((rels (if (listp rel) rel (split-string rel)))
           (page (cl-find-if
                  (lambda (r) (member r '("next" "previous" "prev" "first" "last"))) rels))
           (ext (emacs-reader-opds--format url mime))
           (kind (cond ((member "search" rels) 'search)
                       ((cl-intersection rels '("http://opds-spec.org/acquisition"
                                                "http://opds-spec.org/acquisition/open-access")
                                         :test #'equal)
                        (and ext 'download))
                       ((or page nav (and title (not (member "self" rels))
                                          (string-match-p emacs-reader-opds--feed-type
                                                          (or mime ""))))
                        'feed))))
      (when kind
        (emacs-reader-opds--item
         :kind kind :pagination (and page t) :url url :title title :author author :extension ext
         :label (pcase kind
                  ('download (format "%s%s [%s]" title
                                     (if author (concat " — " author) "") (upcase ext)))
                  ('search (or label "Search catalog"))
                  (_ (or label (and page (capitalize page)) title url))))))))

(defun emacs-reader-opds--children (node tag)
  "Return direct children of NODE matching local name TAG."
  (cl-loop for child in (dom-children node)
           when (and (consp child)
                     (equal (car (last (split-string (symbol-name (dom-tag child)) ":"))) tag))
           collect child))

(defun emacs-reader-opds--text (node)
  "Extract normalized text from an Atom NODE, including inline markup."
  (if (stringp node) node
    (string-trim (mapconcat #'emacs-reader-opds--text (dom-children node) ""))))

(defun emacs-reader-opds--atom (node base &optional publication)
  "Return catalog items from Atom NODE relative to BASE.
PUBLICATION means NODE describes a book or navigation entry."
  (let* ((base (or (emacs-reader-opds--http (dom-attr node 'xml:base) base) base))
         (title (and publication
                     (emacs-reader-opds--text (car (emacs-reader-opds--children node "title")))))
         (authors (cl-loop for a in (emacs-reader-opds--children node "author")
                           collect (emacs-reader-opds--text
                                    (car (emacs-reader-opds--children a "name")))))
         (author (and authors (string-join authors ", "))))
    (append
     (cl-loop for link in (emacs-reader-opds--children node "link")
              for item = (emacs-reader-opds--link
                          (dom-attr link 'href) (dom-attr link 'rel) (dom-attr link 'type)
                          (dom-attr link 'title)
                          (or (emacs-reader-opds--http (dom-attr link 'xml:base) base) base)
                          title author)
              when item collect item)
     (cl-loop for entry in (emacs-reader-opds--children node "entry")
              append (emacs-reader-opds--atom entry base t)))))

(defun emacs-reader-opds--json-link (link base &optional title author nav)
  "Read a JSON LINK at BASE for TITLE and AUTHOR or navigation NAV."
  (emacs-reader-opds--link (alist-get 'href link) (alist-get 'rel link)
                           (alist-get 'type link) (alist-get 'title link) base title author nav))

(defun emacs-reader-opds--json-feed (feed base &optional results)
  "Return catalog items from JSON FEED at BASE.
For a RESULTS page, keep pagination and acquisitions among the feed's own links."
  (let* ((metadata (alist-get 'metadata feed))
         (authors (alist-get 'author metadata))
         (names (cl-loop for a in (cond ((stringp authors) (list authors))
                                        ((and (consp (car-safe authors)) (symbolp (caar authors)))
                                         (list authors))
                                        (t authors))
                         for name = (if (stringp a) a (alist-get 'name a))
                         when (stringp name) collect name)))
    (append
     (cl-loop for link in (alist-get 'links feed)
              for item = (emacs-reader-opds--json-link link base (alist-get 'title metadata)
                                                       (and names (string-join names ", ")))
              for rel = (alist-get 'rel link)
              when (and item (or (not results) (eq (emacs-reader-opds--item-kind item) 'download)
                                 (cl-intersection (if (stringp rel) (split-string rel) rel)
                                                  '("next" "previous" "prev" "first" "last")
                                                  :test #'equal))) collect item)
     (cl-loop for link in (alist-get 'navigation feed)
              for item = (emacs-reader-opds--json-link link base nil nil t) when item collect item)
     (cl-loop for nested in (alist-get 'publications feed)
              append (emacs-reader-opds--json-feed nested base))
     (cl-loop for group in (alist-get 'groups feed)
              append (emacs-reader-opds--json-feed group base results)))))

(defun emacs-reader-opds--xml ()
  "Parse XML at point in the response buffer."
  (car (xml-parse-region (point) (point-max))))

(defun emacs-reader-opds--json ()
  "Read JSON in the response buffer."
  (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(defun emacs-reader-opds--parse (base &optional results)
  "Parse an OPDS feed at point given the response URL BASE.
RESULTS limits feed-level actions on a search results page."
  (skip-chars-forward " \t\r\n")
  (if (eq (char-after) ?{)
      (emacs-reader-opds--json-feed (emacs-reader-opds--json) base results)
    (let* ((root (emacs-reader-opds--xml))
           (tag (car (last (split-string (symbol-name (dom-tag root)) ":")))))
      (unless (member tag '("feed" "entry")) (error "Expected an OPDS feed, received %s" tag))
      (emacs-reader-opds--atom root base (equal tag "entry")))))

(defun emacs-reader-opds--expand-search (template query)
  "Expand the search terms in TEMPLATE with QUERY."
  (let* ((value (url-hexify-string query))
         (url (replace-regexp-in-string "{\\(?:searchTerms\\|query\\)\\??}" value template t t))
         (used (not (equal url template))))
    (setq url (replace-regexp-in-string
               "{\\([?&]\\)\\([^}]+\\)}"
               (lambda (match)
                 (save-match-data
                   (let* ((sep (substring match 1 2))
                          (vars (split-string (substring match 2 -1) ","))
                          (var (cl-find-if
                                (lambda (v) (member v '("query" "searchTerms" "q"))) vars)))
                     (if var (progn (setq used t) (concat sep var "=" value)) "")))) url t t))
    (setq url (replace-regexp-in-string "{[^{}]+\\?}" "" url))
    (when (or (not used) (string-match-p "[{}]" url))
      (user-error "Unsupported catalog search template"))
    url))

(defvar-local emacs-reader-opds--name nil)
(defvar-local emacs-reader-opds--generation 0)
(defvar-local emacs-reader-opds--history nil)
(defvar-local emacs-reader-opds--on-error nil
  "Error callback for a background catalog request.")

(defvar emacs-reader-opds-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "b") #'emacs-reader-opds-back)
    map))

(define-derived-mode emacs-reader-opds-mode special-mode "Book Catalog"
  "Browse book links with TAB and RET.  Go back with `b'.")

(defun emacs-reader-opds--availability (item)
  "Return the local acquisition status of book ITEM, or nil for navigation."
  (pcase (emacs-reader-opds--item-kind item)
    ('download "可下载")
    ('record (when (emacs-reader-opds--item-title item)
               (if (emacs-reader-opds--item-action item) "可查版本" "仅书目信息")))))

(defun emacs-reader-opds--draw ()
  "Draw the current catalog page."
  (pcase-let ((`(,heading . ,items) (car emacs-reader-opds--history))
              (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize heading 'face 'bold) "\n\n")
    (when (cdr emacs-reader-opds--history)
      (insert-text-button "Back" 'action (lambda (_) (emacs-reader-opds-back)))
      (insert "\n\n"))
    (unless items (insert "No available EPUB, TXT or navigation links.\n"))
    (dolist (item items)
      (insert-text-button (concat (when-let* ((status (emacs-reader-opds--availability item)))
                                    (format "【%s】 " status))
                                  (emacs-reader-opds--item-label item))
                          'follow-link t
                          'action (lambda (_) (emacs-reader-opds--activate item)))
      (insert "\n"))
    (goto-char (point-min))))

(defun emacs-reader-opds--show (heading items)
  "Display ITEMS below HEADING in the current catalog buffer."
  (push (cons heading items) emacs-reader-opds--history)
  (emacs-reader-opds--draw))

(defun emacs-reader-opds-back ()
  "Return to the previous catalog page."
  (interactive)
  (cl-incf emacs-reader-opds--generation)
  (unless (cdr emacs-reader-opds--history) (user-error "At first catalog page"))
  (pop emacs-reader-opds--history)
  (emacs-reader-opds--draw))

(defun emacs-reader-opds--request-error (url read-body done binary reason)
  "Report REASON for a request with URL, READ-BODY, DONE and BINARY."
  (if emacs-reader-opds--on-error
      (funcall emacs-reader-opds--on-error reason)
    (let ((inhibit-read-only t))
      (if emacs-reader-opds--history (emacs-reader-opds--draw) (erase-buffer))
      (goto-char (point-min))
      (insert (format "%s · %s\n" emacs-reader-opds--name reason))
      (insert-text-button "重试" 'follow-link t
                          'action (lambda (_) (emacs-reader-opds--get url read-body done binary)))
      (insert "\n\n")
      (goto-char (point-min))
      (message "%s · %s" emacs-reader-opds--name reason))))

(defun emacs-reader-opds--get (url read-body done &optional binary)
  "Fetch URL and call READ-BODY at its body, passing the final URL.
Call DONE with the result in the originating catalog buffer.  BINARY
preserves raw response bytes for acquisition."
  (unless (string-match-p "\\`https?://" url) (user-error "Expected an HTTP(S) URL"))
  (let ((target (current-buffer))
        (generation (cl-incf emacs-reader-opds--generation))
        (url-request-extra-headers nil)
        (url-request-method "GET") (url-request-data nil))
    (unless emacs-reader-opds--on-error
      (message "Fetching %s…" (truncate-string-to-width url 90 nil nil "…")))
    (url-retrieve
     url (lambda (status)
           (let ((response (current-buffer)))
             (unwind-protect
                 (when (and (buffer-live-p target)
                            (= generation
                               (buffer-local-value 'emacs-reader-opds--generation target)))
                   (condition-case err
                       (progn
                         (cond
                          ((and (integerp url-http-response-status)
                                (not (<= 200 url-http-response-status 299)))
                           (error "HTTP %d" url-http-response-status))
                          ((plist-get status :error)
                           (error "网络请求失败 %S" (plist-get status :error)))
                          ((null url-http-response-status)
                           (error "连接结束时未收到 HTTP 状态码（收到 %d 字节）" (buffer-size))))
                         (goto-char (point-min))
                         (unless (re-search-forward "\r?\n\r?\n" nil t)
                           (error "Missing HTTP headers"))
                         (let ((base (if url-current-object
                                         (url-recreate-url url-current-object) url)))
                           (unless binary
                             (set-buffer-multibyte t)
                             (decode-coding-region (point) (point-max) 'utf-8))
                           (let ((result (funcall read-body base)))
                             (with-current-buffer target (funcall done result)))))
                     (error
                      (with-current-buffer target
                        (emacs-reader-opds--request-error url read-body done binary
                                                          (error-message-string err))))))
               (when (buffer-live-p response) (kill-buffer response))))) nil t t)))

(defun emacs-reader-opds--feed (url heading)
  "Fetch the catalog at URL and display it under HEADING."
  (emacs-reader-opds--get url #'emacs-reader-opds--parse
                          (lambda (items) (emacs-reader-opds--show heading items))))

(defun emacs-reader-opds--description (base)
  "Return the OPDS search template in an OpenSearch description at BASE."
  (or (cl-loop for node in (emacs-reader-opds--children (emacs-reader-opds--xml) "Url")
               thereis (and (string-match-p emacs-reader-opds--feed-type
                                            (or (dom-attr node 'type) ""))
                            (emacs-reader-opds--http (dom-attr node 'template) base)))
      (error "No OPDS search template in description")))

(defun emacs-reader-opds--search (url &optional keyword)
  "Search a catalog using URL with KEYWORD, prompting when omitted."
  (let ((query (or keyword (read-string "Search catalog: "))))
    (if (string-match-p "{" url)
        (emacs-reader-opds--feed (emacs-reader-opds--expand-search url query) query)
      (emacs-reader-opds--get
       url #'emacs-reader-opds--description
       (lambda (tpl)
         (emacs-reader-opds--feed (emacs-reader-opds--expand-search tpl query) query))))))

(defun emacs-reader-opds--download (item)
  "Download ITEM and import its bytes into the local shelf."
  (let ((ext (emacs-reader-opds--item-extension item)))
    (emacs-reader-opds--get
     (emacs-reader-opds--item-url item)
     (lambda (_base)
       (when (or (eobp) (and (equal ext "epub") (not (looking-at-p "PK\003\004")))
                 (and (equal ext "txt") (looking-at-p "[[:space:]]*<\\(?:!doctype html\\|html\\)")))
         (error "Server did not return a %s book" ext))
       (let ((path (make-temp-file "emacs-reader-book-" nil (concat "." ext)))
             (coding-system-for-write 'no-conversion))
         (unwind-protect
             (progn (write-region (point) (point-max) path nil 'silent)
                    (prog1 path (setq path nil)))
           (when path (delete-file path)))))
     (lambda (path)
       (unwind-protect
           (emacs-reader-import-file path (emacs-reader-opds--item-title item)
                                     (emacs-reader-opds--item-author item) emacs-reader-opds--name)
         (delete-file path))) t)))

(defun emacs-reader-json--field (key object)
  "Read a dotted field KEY from JSON OBJECT."
  (when key
    (dolist (part (split-string key "\\." t) object)
      (setq object (alist-get (intern-soft part) object)))))

(defun emacs-reader-json--href (template row document base)
  "Resolve TEMPLATE at BASE using fields of ROW or the enclosing DOCUMENT.
A single {field} holds a link.  Embedded fields are URL-encoded.
{root.field} reads from the document, and {page} is reserved."
  (catch 'missing
    (emacs-reader-opds--http
     (replace-regexp-in-string
      "{\\([^{}]+\\)}"
      (lambda (match)
        (save-match-data
          (if (equal match "{page}") match
            (let* ((key (substring match 1 -1))
                   (value (if (string-prefix-p "root." key)
                              (emacs-reader-json--field (substring key 5) document)
                            (emacs-reader-json--field key row))))
              (unless (and (stringp value) (not (string-empty-p value))) (throw 'missing nil))
              (if (equal match template) value (url-hexify-string value))))))
      template t t) base)))

(defun emacs-reader-json--batch-rows (batch row)
  "Expand ROW into lookup groups for BATCH, or return ROW alone."
  (if (not batch) (list row)
    (unless (and (integerp (alist-get 'size batch)) (> (alist-get 'size batch) 0))
      (user-error "JSON catalog batch size must be positive"))
    (when (or (not (alist-get 'whenField batch))
              (equal (emacs-reader-json--field (alist-get 'whenField batch) row)
                     (alist-get 'whenEquals batch)))
      (let* ((values (emacs-reader-json--field (alist-get 'field batch) row))
             (pattern (alist-get 'pattern batch))
             (ids (seq-filter (lambda (id) (and (stringp id) (string-match-p pattern id))) values))
             (groups (seq-partition (delete-dups ids) (alist-get 'size batch))))
        (cl-loop for group in groups for n from 1
                 collect (append `((batch . ,(string-join group (alist-get 'join batch)))
                                   (batchNumber . ,(and (cdr groups) n))) row))))))

(defun emacs-reader-json--lookup (rule row data base label heading book writer)
  "Build a lookup or download for ROW in DATA at BASE with RULE, LABEL, HEADING, BOOK and WRITER."
  (when-let* ((template (alist-get 'href rule))
              (href (emacs-reader-json--href template row data base)))
    (let* ((next (alist-get 'next rule))
           (ext (and (not next) (emacs-reader-opds--format href nil))))
      (when (or next ext)
        (emacs-reader-opds--item
         :kind (if next 'json-page 'download) :url href :title book :author writer :extension ext
         :label (concat label (when-let* ((n (alist-get 'batchNumber row))) (format " [%d]" n)))
         :action (when next
                   (lambda () (emacs-reader-json--page next href heading book writer 1))))))))

(defun emacs-reader-json--row-items (rule row data base title author)
  "Create catalog actions for ROW in DATA at BASE using RULE, TITLE and AUTHOR."
  (let* ((name (emacs-reader-json--field (alist-get 'label rule) row))
         (book (or (emacs-reader-json--field (alist-get 'title rule) row) title name))
         (by (emacs-reader-json--field (alist-get 'author rule) row))
         (writer (cond ((stringp by) by) (by (string-join by ", ")) (t author)))
         (exclude (alist-get 'excludeLabel rule)))
    (when (and (stringp name) (not (and exclude (string-match-p exclude name)))
               (not (emacs-reader-json--field (alist-get 'skipIf rule) row)))
      (let* ((label (concat name (when (and writer (equal name book)) (concat " — " writer))))
             (record (when-let* ((href (alist-get 'recordHref rule)))
                       (emacs-reader-json--href href row data base)))
             (lookup-label (if record (or (alist-get 'lookupLabel rule) "查看版本") label))
             (groups (emacs-reader-json--batch-rows (alist-get 'batch rule) row))
             (links (cl-loop for group in groups
                             for link = (emacs-reader-json--lookup
                                         rule group data base lookup-label name book writer)
                             when link collect link)))
        (if (not record) links
          (list (emacs-reader-opds--item
                 :kind 'record :url record :label label :title book :author writer
                 :action (when links
                           (lambda ()
                             (emacs-reader-opds--show
                              label (cons (emacs-reader-opds--item :kind 'record :url record
                                                                   :label "打开原站书目") links)))))))))))

(defun emacs-reader-json--items (rule data base title author)
  "Map DATA at BASE using RULE, inheriting book TITLE and AUTHOR."
  (cl-loop for row in (emacs-reader-json--field (alist-get 'rows rule) data)
           append (emacs-reader-json--row-items rule row data base title author)))

(defun emacs-reader-json--page (rule template heading title author page &optional done)
  "Fetch PAGE of TEMPLATE with RULE, HEADING, TITLE and AUTHOR.
Pass items to DONE when supplied."
  (let ((url (string-replace "{page}" (number-to-string page) template))
        (page-heading heading))
    (emacs-reader-opds--get
     url (lambda (base)
           (let* ((data (emacs-reader-opds--json))
                  (items (emacs-reader-json--items rule data base title author))
                  (size (alist-get 'pageSize rule))
                  (total (emacs-reader-json--field (alist-get 'total rule) data)))
             (when (numberp total)
               (setq page-heading (format "%s · 第 %d 页 · 本页 %d 条，共 %d 条"
                                          heading page (length items) total)))
             (if (and (integerp size) (> size 0) (numberp total) (< (* size page) total)
                      (not (equal url template)))
                 (append items (list (emacs-reader-opds--item
                                      :kind 'json-page :pagination t :label "Next page"
                                      :action (lambda () (emacs-reader-json--page
                                                          rule template heading title author
                                                          (1+ page))))))
               items)))
     (or done (lambda (items) (emacs-reader-opds--show page-heading items))))))

(defun emacs-reader-opds--activate (item)
  "Follow catalog ITEM."
  (let ((url (emacs-reader-opds--item-url item)))
    (pcase (emacs-reader-opds--item-kind item)
      ('feed (emacs-reader-opds--feed url (emacs-reader-opds--item-label item)))
      ('search (emacs-reader-opds--search url))
      ('record (if-let* ((action (emacs-reader-opds--item-action item)))
                   (funcall action) (browse-url url)))
      ('download (emacs-reader-opds--download item))
      ('json-page (funcall (emacs-reader-opds--item-action item))))))

(defun emacs-reader-opds--start (name)
  "Open a catalog buffer for NAME."
  (pop-to-buffer (generate-new-buffer (format "*Books: %s*" name)))
  (emacs-reader-opds-mode)
  (setq emacs-reader-opds--name name))

;;;###autoload
(defun emacs-reader-opds-browse (name url)
  "Browse the OPDS catalog NAME at HTTP(S) URL."
  (interactive (list (read-string "Catalog name: ") (read-string "Catalog URL: ")))
  (emacs-reader-opds--start name)
  (emacs-reader-opds--feed url name))

(defun emacs-reader-opds-search (name url keyword)
  "Search catalog NAME at URL for KEYWORD using its advertised search link."
  (emacs-reader-opds--start name)
  (emacs-reader-opds--get
   url #'emacs-reader-opds--parse
   (lambda (items)
     (emacs-reader-opds--show name items)
     (if-let* ((search (cl-find 'search items :key #'emacs-reader-opds--item-kind)))
         (emacs-reader-opds--search (emacs-reader-opds--item-url search) keyword)
       (message "目录「%s」未声明搜索入口，可按 TAB 和 RET 浏览" name)))))

;;;###autoload
(defun emacs-reader-json-catalog-search (source query)
  "Search the JSON catalog SOURCE for QUERY.
SOURCE is an alist with type, name, url and definition fields.  The url
substitutes {query} and optionally {page}.  Each definition selects a
rows array by dotted field path and maps each row's label and href.
Set next to a nested definition to navigate, or omit it for downloads.
An optional recordHref gives each row a bibliographic page and groups
its lookup actions under lookupLabel.  A batch may require whenField to
match whenEquals."
  (when (string-empty-p (string-trim query)) (user-error "Enter a search term"))
  (let* ((template (alist-get 'url source))
         (name (alist-get 'name source))
         (url (string-replace "{query}" (url-hexify-string query) template)))
    (unless (and (equal (alist-get 'type source) "json") (not (equal url template)))
      (user-error "Expected a JSON catalog URL with {query}"))
    (emacs-reader-opds--start name)
    (emacs-reader-json--page (alist-get 'definition source) url query nil nil 1)))

(defun emacs-reader-catalog--search-link (base)
  "Return the search link advertised by the OPDS catalog at BASE."
  (skip-chars-forward " \t\r\n")
  (let ((links (if (eq (char-after) ?{)
                   (alist-get 'links (emacs-reader-opds--json))
                 (let ((root (emacs-reader-opds--xml)))
                   (setq base (or (emacs-reader-opds--http (dom-attr root 'xml:base) base) base))
                   (mapcar #'dom-attributes (emacs-reader-opds--children root "link"))))))
    (or (cl-loop for link in links
                 for rel = (alist-get 'rel link)
                 for mime = (alist-get 'type link)
                 thereis (and (member "search" (if (stringp rel) (split-string rel) rel))
                              (or (null mime) (string-match-p emacs-reader-opds--feed-type mime)
                                  (string-prefix-p "application/opensearchdescription+xml" mime))
                              (emacs-reader-opds--http
                               (alist-get 'href link)
                               (or (emacs-reader-opds--http (alist-get 'xml:base link) base)
                                   base))))
        (error "目录未声明可用的搜索入口"))))

;;;###autoload
(defun emacs-reader-catalog-results (source keyword callback)
  "Search catalog SOURCE for KEYWORD without opening a window.
Call CALLBACK once with (ITEMS ERROR-STRING).  On success ITEMS contains
catalog actions and ERROR-STRING is nil, including for an empty page."
  (let ((worker (generate-new-buffer " *book catalog request*")) timer)
    (cl-labels ((finish (items reason)
                  (when (buffer-live-p worker)
                    (cancel-timer timer)
                    (kill-buffer worker)
                    (funcall callback items reason)))
                (success (items) (finish items nil))
                (fetch-results (template)
                  (emacs-reader-opds--get
                   (emacs-reader-opds--expand-search template keyword)
                   (lambda (base) (emacs-reader-opds--parse base t))
                   (lambda (items)
                     (success (cl-remove 'search items :key #'emacs-reader-opds--item-kind))))))
      (setq timer (run-at-time 30 nil #'finish nil "目录搜索超时"))
      (with-current-buffer worker
        (setq emacs-reader-opds--on-error (lambda (reason) (finish nil reason)))
        (condition-case err
            (let ((url (alist-get 'url source)))
              (when (string-empty-p (string-trim keyword)) (user-error "Enter a search term"))
              (pcase (alist-get 'type source)
                ("opds"
                 (emacs-reader-opds--get
                  url #'emacs-reader-catalog--search-link
                  (lambda (link)
                    (if (string-match-p "{" link) (fetch-results link)
                      (emacs-reader-opds--get link #'emacs-reader-opds--description
                                              #'fetch-results)))))
                ("json"
                 (unless (string-search "{query}" url) (error "JSON catalog URL lacks {query}"))
                 (emacs-reader-json--page (alist-get 'definition source)
                                          (string-replace "{query}" (url-hexify-string keyword) url)
                                          keyword nil nil 1 #'success))
                (_ (error "Unsupported catalog type"))))
          (error (finish nil (error-message-string err))))))))

;;;###autoload
(defun emacs-reader-catalog-open-item (source item)
  "Open ITEM from SOURCE, using a catalog buffer when it has further actions."
  (unless (and (eq (emacs-reader-opds--item-kind item) 'record)
               (not (emacs-reader-opds--item-action item)))
    (let ((name (alist-get 'name source)))
      (emacs-reader-opds--start name)
      (emacs-reader-opds--show name (list item))))
  (emacs-reader-opds--activate item))

(provide 'emacs-reader-opds)
;;; emacs-reader-opds.el ends here
