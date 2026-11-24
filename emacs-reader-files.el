;;; emacs-reader-files.el --- Read TXT and EPUB -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; The chapter URL is an opaque local identifier, suitable for persisting.
;; EPUB reading uses libxml and Python 3's standard ZIP reader.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'subr-x)
(require 'url-util)

(defun emacs-reader-files--format (path)
  "Return the supported file extension of local book PATH."
  (when (file-remote-p path) (user-error "A local book file is required"))
  (unless (file-regular-p path) (user-error "Missing book file: %s" path))
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (unless (member ext '("txt" "epub"))
      (user-error "Unsupported book format: %s" ext))
    ext))

(defconst emacs-reader-files--heading
  (concat "^[ \t　]*\\(?:第[0-9０-９一二三四五六七八九十百千万零〇两]+[章节卷回部篇]"
          "\\|Chapter[ \t]+[0-9IVXLCDM]+\\b\\)[^\n]\\{0,100\\}$")
  "Heading line recognized in a text book.")

(defun emacs-reader-files--txt-chapter (title start end)
  "Make a text chapter with TITLE spanning buffer positions START to END."
  (list :title title :url (format "txt:%d:%d" start end)))

(defun emacs-reader-files--txt-chapters (path)
  "Return sections of text book PATH, retaining an untitled preface."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((case-fold-search t) (title (file-name-base path))
          (start (point-min)) chapters)
      (while (re-search-forward emacs-reader-files--heading nil t)
        (let ((heading (string-trim (match-string 0)))
              (next (line-beginning-position)))
          (when (and (< start next)
                     (save-excursion (goto-char start)
                                     (re-search-forward "[^[:space:]　]" next t)))
            (push (emacs-reader-files--txt-chapter title start next) chapters))
          (setq start next title heading)))
      (nreverse (cons (emacs-reader-files--txt-chapter title start (point-max))
                      chapters)))))

(defconst emacs-reader-files--zip-reader
  "import sys, zipfile
with zipfile.ZipFile(sys.argv[1]) as book:
    entry = book.getinfo(sys.argv[2])
    limit = 16 * 1024 * 1024
    if entry.file_size > limit or entry.is_dir():
        sys.exit(1)
    with book.open(entry) as member:
        data = member.read(limit + 1)
    if len(data) > limit:
        sys.exit(1)
    sys.stdout.buffer.write(data)"
  "Read a single ZIP member by exact name, solely into memory.")

(defun emacs-reader-files--resource (href &optional base)
  "Resolve resource HREF relative to member BASE, enforcing archive bounds."
  (let* ((raw (car (and href (split-string href "[?#]"))))
         (name (and raw (decode-coding-string
                         (url-unhex-string (encode-coding-string raw 'utf-8))
                         'utf-8)))
         (parts (reverse (butlast (split-string (or base "") "/" t)))))
    (when (or (not name) (string-empty-p name) (string-prefix-p "/" name)
              (string-match-p "[[:cntrl:]:\\\\]" name))
      (user-error "Invalid EPUB resource: %s" href))
    (dolist (part (split-string name "/" t))
      (cond ((equal part "."))
            ((equal part "..")
             (unless parts (user-error "EPUB resource escapes archive: %s" href))
             (pop parts))
            (t (push part parts))))
    (unless parts (user-error "Empty EPUB resource: %s" href))
    (string-join (nreverse parts) "/")))

(defun emacs-reader-files--dom (path member &optional html)
  "Parse MEMBER from EPUB PATH, using the HTML parser when HTML is non-nil."
  (unless (libxml-available-p) (user-error "Reading EPUB requires libxml"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (unless (equal 0 (call-process (or (executable-find "python3")
                                       (user-error "Reading EPUB requires Python 3"))
                                   nil '(t nil) nil "-I" "-c"
                                   emacs-reader-files--zip-reader
                                   (expand-file-name path) member))
      (user-error "Cannot read EPUB resource: %s" member))
    (decode-coding-region (point-min) (point-max) 'undecided)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (when (re-search-forward "<!ENTITY\\_>" nil t)
      (user-error "Unsupported XML entity declaration in %s" member))
    (or (if html (libxml-parse-html-region (point-min) (point-max))
          (libxml-parse-xml-region (point-min) (point-max)))
        (user-error "Invalid EPUB document: %s" member))))

(defun emacs-reader-files--package (path)
  "Return (MEMBER . DOM) of the package document in EPUB PATH."
  (let* ((container (emacs-reader-files--dom path "META-INF/container.xml"))
         (root (car (dom-by-tag container 'rootfile)))
         (member (emacs-reader-files--resource (dom-attr root 'full-path))))
    (cons member (emacs-reader-files--dom path member))))

(defun emacs-reader-files--text (node)
  "Return concatenated text descendants of NODE."
  (if (stringp node) node
    (mapconcat #'emacs-reader-files--text (dom-children node) "")))

(defun emacs-reader-files-metadata (path)
  "Return a plist with :title and optional :author for local book PATH."
  (if (equal (emacs-reader-files--format path) "txt")
      (list :title (file-name-base path))
    (let* ((meta (car (dom-by-tag (cdr (emacs-reader-files--package path))
                                  'metadata)))
           (title (string-trim (emacs-reader-files--text (car (dom-by-tag meta 'title)))))
           (author (string-trim (emacs-reader-files--text (car (dom-by-tag meta 'creator))))))
      (append (list :title (if (string-empty-p title) (file-name-base path) title))
              (unless (string-empty-p author) (list :author author))))))

(defun emacs-reader-files--labels (path package items)
  "Index chapter labels of EPUB PATH from PACKAGE and manifest ITEMS."
  (let* ((nav (cl-find-if (lambda (item)
                            (member "nav" (split-string (or (dom-attr item 'properties) ""))))
                          items))
         (ncx (cl-find "application/x-dtbncx+xml" items :test #'equal
                       :key (lambda (item) (dom-attr item 'media-type))))
         (item (or nav ncx))
         (labels (make-hash-table :test #'equal)))
    (when item
      (let* ((member (emacs-reader-files--resource (dom-attr item 'href) package))
             (doc (emacs-reader-files--dom path member))
             (navs (dom-by-tag doc 'nav))
             (toc (or (cl-find-if (lambda (node)
                                    (member "toc" (split-string (or (dom-attr node 'type) ""))))
                                  navs)
                      (car navs))))
        (dolist (node (if nav (dom-by-tag toc 'a) (dom-by-tag doc 'navPoint)))
          (let* ((href (if nav (dom-attr node 'href)
                         (dom-attr (car (dom-by-tag node 'content)) 'src)))
                 (label (string-clean-whitespace
                         (emacs-reader-files--text
                          (if nav node (car (dom-by-tag node 'navLabel))))))
                 (key (and href (ignore-errors (emacs-reader-files--resource href member)))))
            (when (and key (not (string-empty-p label)) (not (gethash key labels)))
              (puthash key label labels))))))
    labels))

(defun emacs-reader-files-chapters (path)
  "Return the chapters of local book PATH as plists with :title and :url."
  (if (equal (emacs-reader-files--format path) "txt")
      (emacs-reader-files--txt-chapters path)
    (pcase-let* ((`(,package . ,doc) (emacs-reader-files--package path))
                 (items (dom-by-tag (car (dom-by-tag doc 'manifest)) 'item))
                 (manifest (make-hash-table :test #'equal))
                 (labels (emacs-reader-files--labels path package items))
                 (chapters nil))
      (dolist (item items) (puthash (dom-attr item 'id) item manifest))
      (dolist (ref (dom-by-tag (car (dom-by-tag doc 'spine)) 'itemref))
        (let ((item (gethash (dom-attr ref 'idref) manifest)))
          (when (member (dom-attr item 'media-type) '("application/xhtml+xml" "text/html"))
            (let ((member (emacs-reader-files--resource (dom-attr item 'href) package)))
              (push (list :title (or (gethash member labels) (file-name-base member))
                          :url member) chapters)))))
      (or (nreverse chapters) (user-error "EPUB has no readable chapters")))))

(defun emacs-reader-files--render (node)
  "Insert plain text for HTML NODE, preserving paragraph and line breaks."
  (if (stringp node)
      (insert (replace-regexp-in-string "[ \t\n\r]+" " " node))
    (let* ((tag (dom-tag node))
           (block (memq tag '(p div section article li h1 h2 h3 h4 h5 h6
                                blockquote hr header footer tr))))
      (cond ((memq tag '(head style script iframe svg img audio video object)))
            ((eq tag 'br) (insert "\n"))
            (t (when block (insert "\n\n"))
               (mapc #'emacs-reader-files--render (dom-children node))
               (when block (insert "\n\n")))))))

(defun emacs-reader-files-content (path chapter)
  "Return plain text for CHAPTER of local book PATH.
CHAPTER may be a chapter plist or its :url string."
  (let ((url (if (stringp chapter) chapter (plist-get chapter :url))))
    (unless (stringp url) (user-error "Missing chapter identifier"))
    (if (equal (emacs-reader-files--format path) "txt")
        (with-temp-buffer
          (insert-file-contents path)
          (unless (string-match "\\`txt:\\([0-9]+\\):\\([0-9]+\\)\\'" url)
            (user-error "Invalid TXT chapter"))
          (let ((start (string-to-number (match-string 1 url)))
                (end (string-to-number (match-string 2 url))))
            (unless (<= (point-min) start end (point-max))
              (user-error "TXT chapter no longer matches the file"))
            (buffer-substring-no-properties start end)))
      ;; A stored identifier is already decoded.  Validate it without
      ;; interpreting percent escapes a second time.
      (when (or (string-match-p "[[:cntrl:]:\\\\]" url)
                (cl-intersection (split-string url "/") '("" "." "..") :test #'equal))
        (user-error "Invalid EPUB chapter identifier"))
      (let ((doc (emacs-reader-files--dom path url t)))
        (with-temp-buffer
          (emacs-reader-files--render (car (dom-by-tag doc 'body)))
          (let* ((text (replace-regexp-in-string " +" " " (buffer-string)))
                 (lines (replace-regexp-in-string " *\n *" "\n" text)))
            (string-trim (replace-regexp-in-string "\n\\{3,\\}" "\n\n" lines))))))))

(provide 'emacs-reader-files)
;;; emacs-reader-files.el ends here
