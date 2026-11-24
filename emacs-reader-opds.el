;;; emacs-reader-opds.el --- Browse public book catalogs -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Browse Atom and JSON OPDS catalogs, or search Open Library public editions.
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
  label kind url title author extension)

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
         :kind kind :url url :title title :author author :extension ext
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

(defun emacs-reader-opds--json-feed (feed base)
  "Return catalog items from JSON FEED at BASE."
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
              when item collect item)
     (cl-loop for link in (alist-get 'navigation feed)
              for item = (emacs-reader-opds--json-link link base nil nil t) when item collect item)
     (cl-loop for nested in (append (alist-get 'publications feed) (alist-get 'groups feed))
              append (emacs-reader-opds--json-feed nested base)))))

(defun emacs-reader-opds--xml ()
  "Parse XML at point in the response buffer."
  (car (xml-parse-region (point) (point-max))))

(defun emacs-reader-opds--json ()
  "Read JSON in the response buffer."
  (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(defun emacs-reader-opds--parse (base)
  "Parse an OPDS feed at point given the response URL BASE."
  (skip-chars-forward " \t\r\n")
  (if (eq (char-after) ?{)
      (emacs-reader-opds--json-feed (emacs-reader-opds--json) base)
    (let* ((root (emacs-reader-opds--xml))
           (tag (car (last (split-string (symbol-name (dom-tag root)) ":")))))
      (unless (member tag '("feed" "entry")) (error "Expected an OPDS feed, received %s" tag))
      (emacs-reader-opds--atom root base (equal tag "entry")))))

(defun emacs-reader-opds--expand-search (template query)
  "Expand the search terms in TEMPLATE with QUERY."
  (let* ((value (url-hexify-string query))
         (url (replace-regexp-in-string "{\\(?:searchTerms\\|query\\)\\??}" value template t t)))
    (setq url (replace-regexp-in-string
               "{\\([?&]\\)\\([^}]+\\)}"
               (lambda (match)
                 (save-match-data
                   (let* ((sep (substring match 1 2))
                          (vars (split-string (substring match 2 -1) ","))
                          (var (cl-find-if
                                (lambda (v) (member v '("query" "searchTerms" "q"))) vars)))
                     (if var (concat sep var "=" value) "")))) url t t))
    (setq url (replace-regexp-in-string "{[^{}]+\\?}" "" url))
    (when (or (equal url template) (string-match-p "[{}]" url))
      (user-error "Unsupported catalog search template"))
    url))

(defvar-local emacs-reader-opds--name nil)
(defvar-local emacs-reader-opds--generation 0)
(defvar-local emacs-reader-opds--history nil)

(defvar emacs-reader-opds-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "b") #'emacs-reader-opds-back)
    map))

(define-derived-mode emacs-reader-opds-mode special-mode "Book Catalog"
  "Browse book links with TAB and RET.  Go back with `b'.")

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
      (insert-text-button (emacs-reader-opds--item-label item) 'follow-link t
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

(defun emacs-reader-opds--get (url read-body done &optional binary)
  "Fetch URL and call READ-BODY at its body, passing the final URL.
Call DONE with the result in the originating catalog buffer.  BINARY
preserves raw response bytes for acquisition."
  (unless (string-match-p "\\`https?://" url) (user-error "Expected an HTTP(S) URL"))
  (let ((target (current-buffer))
        (generation (cl-incf emacs-reader-opds--generation))
        (url-request-extra-headers nil)
        (url-request-method "GET") (url-request-data nil))
    (message "Fetching %s…" (truncate-string-to-width url 90 nil nil "…"))
    (url-retrieve
     url (lambda (status)
           (let ((response (current-buffer)))
             (unwind-protect
                 (when (and (buffer-live-p target)
                            (= generation
                               (buffer-local-value 'emacs-reader-opds--generation target)))
                   (condition-case err
                       (progn
                         (when (or (plist-get status :error)
                                   (not (and (integerp url-http-response-status)
                                             (<= 200 url-http-response-status 299))))
                           (error "Request failed (%s)"
                                  (or url-http-response-status (plist-get status :error))))
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
                     (error (message "Book catalog: %s" (error-message-string err)))))
               (when (buffer-live-p response) (kill-buffer response))))) nil t t)))

(defun emacs-reader-opds--feed (url heading)
  "Fetch the catalog at URL and display it under HEADING."
  (emacs-reader-opds--get url #'emacs-reader-opds--parse
                          (lambda (items) (emacs-reader-opds--show heading items))))

(defun emacs-reader-opds--search (url)
  "Search a catalog using URL, a template or OpenSearch description."
  (let ((query (read-string "Search catalog: ")))
    (if (string-match-p "{" url)
        (emacs-reader-opds--feed (emacs-reader-opds--expand-search url query) query)
      (emacs-reader-opds--get
       url (lambda (base)
             (or (cl-loop for node in (emacs-reader-opds--children (emacs-reader-opds--xml) "Url")
                          when (string-match-p emacs-reader-opds--feed-type
                                               (or (dom-attr node 'type) ""))
                          return (emacs-reader-opds--http (dom-attr node 'template) base))
                 (error "No OPDS search template in description")))
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

(defun emacs-reader-opds--ol-page (query page)
  "Search Open Library for QUERY on PAGE."
  (let ((url (format (concat "https://openlibrary.org/search.json?q=%s"
                             "&fields=key,title,author_name,ia&limit=20&page=%d")
                     (url-hexify-string (concat query " ebook_access:public")) page)))
    (emacs-reader-opds--get
     url (lambda (_) (emacs-reader-opds--json))
     (lambda (data)
       (let ((items (cl-loop for work in (alist-get 'docs data)
                             for title = (alist-get 'title work)
                             for author = (string-join (alist-get 'author_name work) ", ")
                             for ids = (cl-remove-if-not
                                        (lambda (id) (string-match-p "\\`[[:alnum:]_.-]+\\'" id))
                                        (alist-get 'ia work))
                             when ids collect
                             (emacs-reader-opds--item
                              :kind 'editions :url ids :title title :author author
                              :label (format "%s — %s" title author)))))
         (when (< (* page 20) (or (alist-get 'numFound data) 0))
           (setq items (append items (list (emacs-reader-opds--item
                                            :label "Next page" :kind 'ol-page
                                            :url (1+ page) :title query)))))
         (emacs-reader-opds--show (format "Open Library · %s · %d" query page) items))))))

(defun emacs-reader-opds--editions (item)
  "Find publicly accessible Internet Archive text editions for ITEM."
  (let* ((ids (emacs-reader-opds--item-url item))
         (rest (nthcdr 40 ids))
         (batch (seq-take ids 40))
         (title (emacs-reader-opds--item-title item))
         (author (emacs-reader-opds--item-author item))
         (q (concat "mediatype:texts AND NOT access-restricted-item:true AND identifier:("
                    (mapconcat (lambda (id) (format "\"%s\"" id)) batch " OR ") ")")))
    (emacs-reader-opds--get
     (concat "https://archive.org/advancedsearch.php?fl%5B%5D=identifier&rows=40&output=json&q="
             (url-hexify-string q))
     (lambda (_) (alist-get 'docs (alist-get 'response (emacs-reader-opds--json))))
     (lambda (docs)
       (let ((items (cl-loop for doc in docs for id = (alist-get 'identifier doc)
                             when (member id batch) collect
                             (emacs-reader-opds--item :label id :kind 'archive :url id
                                                      :title title :author author))))
         (when rest
           (setq items (append items (list (emacs-reader-opds--item
                                            :label "More editions" :kind 'editions
                                            :url rest :title title :author author)))))
         (emacs-reader-opds--show (concat title " · Public editions") items))))))

(defun emacs-reader-opds--files (item data)
  "Return EPUB and text downloads for ITEM from archive metadata DATA."
  (let ((base (concat "https://archive.org/download/" (emacs-reader-opds--item-url item) "/")))
    (cl-loop for file in (alist-get 'files data)
             for name = (alist-get 'name file)
             for ext = (and name (downcase (or (file-name-extension name) "")))
             when (and (member ext '("epub" "txt")) (not (alist-get 'private file))
                       (not (string-match-p "_lcp\\.epub\\'" name)))
             collect (emacs-reader-opds--item
                      :kind 'download :url (concat base (url-hexify-string name))
                      :title (emacs-reader-opds--item-title item)
                      :author (emacs-reader-opds--item-author item)
                      :label name :extension ext))))

(defun emacs-reader-opds--activate (item)
  "Follow catalog ITEM."
  (let ((url (emacs-reader-opds--item-url item)))
    (pcase (emacs-reader-opds--item-kind item)
      ('feed (emacs-reader-opds--feed url (emacs-reader-opds--item-label item)))
      ('search (emacs-reader-opds--search url))
      ('download (emacs-reader-opds--download item))
      ('ol-page (emacs-reader-opds--ol-page (emacs-reader-opds--item-title item) url))
      ('editions (emacs-reader-opds--editions item))
      ('archive (emacs-reader-opds--get
                 (concat "https://archive.org/metadata/" url)
                 (lambda (_) (emacs-reader-opds--files item (emacs-reader-opds--json)))
                 (lambda (items) (emacs-reader-opds--show url items)))))))

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

;;;###autoload
(defun emacs-reader-openlibrary-search (query)
  "Search Open Library for public ebook editions matching QUERY."
  (interactive "sSearch Open Library: ")
  (when (string-empty-p (string-trim query)) (user-error "Enter a search term"))
  (emacs-reader-opds--start "Open Library")
  (emacs-reader-opds--ol-page query 1))

(provide 'emacs-reader-opds)
;;; emacs-reader-opds.el ends here
