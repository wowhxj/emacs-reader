;;; emacs-reader-files.el --- Parse local books -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; The chapter URL is an opaque local identifier, suitable for persisting.
;; EPUB uses libxml and Python 3.  MOBI uses Calibre's ebook-convert.
;; PDF uses Poppler's pdftotext or MuPDF's mutool.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'subr-x)
(require 'url-util)

(require 'org-element)

(defconst emacs-reader-files--extensions
  '("txt" "epub" "pdf" "mobi" "md" "markdown" "org")
  "Local book extensions supported by the parser.")

(defun emacs-reader-files-supported-p (path)
  "Return non-nil if PATH has a supported book extension."
  (and (member (downcase (or (file-name-extension path) ""))
               emacs-reader-files--extensions)
       t))

(defun emacs-reader-files--format (path)
  "Return the supported format of local book PATH."
  (when (file-remote-p path) (user-error "A local book file is required"))
  (unless (file-regular-p path) (user-error "Missing book file: %s" path))
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (unless (emacs-reader-files-supported-p path)
      (user-error "Unsupported book format: %s" ext))
    (if (equal ext "markdown") "md" ext)))

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

(defun emacs-reader-files--markdown-headings ()
  "Return (START LEVEL TITLE END) entries for Markdown headings in this buffer."
  (save-excursion
    (goto-char (point-min))
    ;; YAML front matter uses the same delimiter as a Setext heading.
    (when (looking-at-p "---[ \t]*$")
      (forward-line 1)
      (if (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$" nil t)
          (forward-line 1)
        (goto-char (point-min))))
    (let (headings fence previous)
      (while (not (eobp))
        (cond
         ((looking-at " \\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)\\(.*\\)$")
          (let ((run (match-string-no-properties 1)) (rest (match-string-no-properties 2)))
            (cond (fence
                   (when (and (string-prefix-p fence run) (string-blank-p rest))
                     (setq fence nil)))
                  ((or (eq (aref run 0) ?~) (not (string-match-p "`" rest)))
                   (setq fence run))))
          (setq previous nil))
         (fence)
         ((looking-at " \\{0,3\\}\\(#\\{1,6\\}\\)[ \t]+\\(.*\\)$")
          (let* ((level (length (match-string 1)))
                 (end (match-end 0))
                 (title (string-trim (replace-regexp-in-string
                                      "\\(?:\\`\\|[ \t]+\\)#+[ \t]*\\'" "" (match-string 2)))))
            (unless (string-empty-p title)
              (push (list (point) level title end) headings)))
          (setq previous nil))
         ((and previous (looking-at " \\{0,3\\}\\(=+\\|-+\\)[ \t]*$"))
          (push (list (car previous) (if (eq (char-after (match-beginning 1)) ?=) 1 2)
                      (cdr previous) (line-end-position)) headings)
          (setq previous nil))
         ((looking-at-p (concat "\\(?:[ \t]*$\\|    \\|\t\\| \\{0,3\\}"
                                "\\(?:>\\|[-+*][ \t]+\\|[0-9]+[.)][ \t]+\\|---+[ \t]*$\\)\\)"))
          (setq previous nil))
         (t (setq previous (cons (point) (string-trim
                                          (buffer-substring-no-properties
                                           (point) (line-end-position)))))))
        (forward-line 1))
      (nreverse headings))))

(defun emacs-reader-files--org-headings ()
  "Return (START LEVEL TITLE END) entries for Org headlines in this buffer."
  (require 'org)
  (delay-mode-hooks (org-mode))
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (let ((start (org-element-property :begin headline)))
        (save-excursion
          (goto-char start)
          (list start (org-element-property :level headline)
                (org-element-property :raw-value headline) (line-end-position)))))))

(defun emacs-reader-files--headings (format)
  "Return outline entries in this buffer for FORMAT."
  (if (equal format "org") (emacs-reader-files--org-headings)
    (emacs-reader-files--markdown-headings)))

(defun emacs-reader-files--outline-preface-p (start end format)
  "Return non-nil if START to END includes preface text for FORMAT."
  (let ((text (buffer-substring-no-properties start end)))
    (when (equal format "org")
      (setq text (replace-regexp-in-string "^[ \t]*#\\+[^\n]*\n?" "" text)))
    (not (string-blank-p text))))

(defun emacs-reader-files--outline-chapters (path format)
  "Divide outline file PATH of FORMAT at its shallowest headings."
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((headings (emacs-reader-files--headings format))
           (level (and headings (apply #'min (mapcar #'cadr headings))))
           (start (point-min))
           (title (file-name-base path)) chapters)
      (dolist (heading headings)
        (pcase-let ((`(,next ,depth ,label ,_) heading))
          (when (= depth level)
            (when (and (< start next)
                       (or chapters (> start (point-min))
                           (emacs-reader-files--outline-preface-p start next format)))
              (push (list :title title :url (format "%s:%d:%d" format start next)) chapters))
            (setq start next title label))))
      (nreverse (cons (list :title title :url (format "%s:%d:%d" format start (point-max)))
                      chapters)))))

(defun emacs-reader-files--range-content (path format url)
  "Read the buffer range identified by URL from PATH of FORMAT."
  (unless (string-match (concat "\\`" format ":\\([0-9]+\\):\\([0-9]+\\)\\'") url)
    (user-error "Invalid %s chapter" (upcase format)))
  (let ((start (string-to-number (match-string 1 url)))
        (end (string-to-number (match-string 2 url))))
    (with-temp-buffer
      (insert-file-contents path)
      (unless (<= (point-min) start end (point-max))
        (user-error "%s chapter no longer matches the file" (upcase format)))
      (narrow-to-region start end)
      (unless (equal format "txt")
        ;; Process backwards to keep source positions valid during edits.
        (dolist (heading (reverse (emacs-reader-files--headings format)))
          (pcase-let ((`(,begin ,_ ,title ,finish) heading))
            (goto-char begin)
            (if (equal format "org")
                (when (looking-at "\\*+[ \t]+") (replace-match "" t t))
              (delete-region begin finish)
              (insert title)))))
      (buffer-substring-no-properties (point-min) (point-max)))))

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
  (pcase (emacs-reader-files--format path)
    ("epub" (emacs-reader-files--epub-metadata path (cdr (emacs-reader-files--package path))))
    ("mobi" (emacs-reader-files--with-mobi-epub path #'emacs-reader-files-metadata))
    (_ (list :title (file-name-base path)))))

(defun emacs-reader-files--epub-metadata (path doc)
  "Read metadata for PATH from package document DOC."
  (let* ((meta (car (dom-by-tag doc 'metadata)))
         (title (string-trim (emacs-reader-files--text (car (dom-by-tag meta 'title)))))
         (author (string-trim (emacs-reader-files--text (car (dom-by-tag meta 'creator))))))
    (append (list :title (if (string-empty-p title) (file-name-base path) title))
            (unless (string-empty-p author) (list :author author)))))

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

(defun emacs-reader-files--with-mobi-epub (path function &rest args)
  "Convert MOBI PATH temporarily and call FUNCTION with the EPUB and ARGS."
  (let ((converter (or (executable-find "ebook-convert")
                       (user-error "Reading MOBI requires Calibre's ebook-convert")))
        (directory (make-temp-file "reader-mobi-" t)))
    (unwind-protect
        (let ((epub (expand-file-name "book.epub" directory)))
          (with-temp-buffer
            (unless (equal 0 (call-process converter nil (list t t) nil
                                           (expand-file-name path) epub))
              (user-error "Cannot convert MOBI: %s" (string-trim (buffer-string)))))
          (apply function epub args))
      (delete-directory directory t))))

(defun emacs-reader-files--mobi-chapters (epub)
  "Return chapters of converted MOBI EPUB that contain readable text."
  (let ((chapters (cl-remove-if
                   (lambda (chapter)
                     (string-blank-p (emacs-reader-files-content epub chapter)))
                   (emacs-reader-files-chapters epub))))
    (or chapters (user-error "MOBI has no readable chapters"))))

(defun emacs-reader-files--mobi-inspect (epub)
  "Return metadata and readable chapters of converted MOBI EPUB."
  (list (emacs-reader-files-metadata epub) (emacs-reader-files--mobi-chapters epub)))

(defun emacs-reader-files--pdf-text (path &optional page)
  "Extract PDF PATH to text, or extract PAGE when given."
  (let* ((poppler (executable-find "pdftotext"))
         (mupdf (and (not poppler) (executable-find "mutool")))
         (program (or poppler mupdf
                      (user-error "Reading PDF requires pdftotext or mutool")))
         (args (if poppler
                   (append (when page (list "-f" (number-to-string page)
                                            "-l" (number-to-string page)))
                           (list "-enc" "UTF-8" (expand-file-name path) "-"))
                 (append (list "draw" "-q" "-F" "txt" (expand-file-name path))
                         (when page (list (number-to-string page)))))))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (unless (equal 0 (apply #'call-process program nil '(t nil) nil args))
          (user-error "Cannot extract PDF text: %s" (file-name-nondirectory path))))
      (buffer-string))))

(defun emacs-reader-files--pdf-chapters (path &optional allow-image)
  "Return chapters in PDF PATH; ALLOW-IMAGE accepts pages without text."
  (let* ((text (emacs-reader-files--pdf-text path))
         ;; Both Poppler and MuPDF terminate each page with a form feed.
         (pages (split-string text "\f"))
         (count (if (string-match-p "\f[ \t\r\n]*\\'" text)
                    (1- (length pages)) (length pages))))
    (when (< count 1) (user-error "PDF has no readable pages"))
    (if (string-blank-p (replace-regexp-in-string "\f" "" text))
        (if (and allow-image (string-match-p "\f" text))
            (cl-loop for page from 1 to count
                     collect (list :title (format "Page %d" page) :url (format "image-pdf:%d" page)))
          (user-error "PDF contains no extractable text; scanned pages require OCR"))
      (cl-loop for page from 1 to count
               collect (list :title (format "Page %d" page) :url (format "pdf:%d" page))))))

(defun emacs-reader-files--pdf-content (path url)
  "Return PDF PATH page identified by URL."
  (unless (and (string-match "\\`pdf:\\([1-9][0-9]*\\)\\'" url)
               (<= (length (match-string 1 url)) 8))
    (user-error "Invalid PDF chapter"))
  (let ((page (string-to-number (match-string 1 url))))
    ;; MuPDF clamps out-of-range pages to the last page, so check first.
    (when (and (not (executable-find "pdftotext"))
               (> page (length (emacs-reader-files--pdf-chapters path))))
      (user-error "PDF page is out of range"))
    (replace-regexp-in-string "\f[ \t\r\n]*\\'" ""
                              (emacs-reader-files--pdf-text path page))))

(defun emacs-reader-files-chapters (path)
  "Return the chapters of local book PATH as plists with :title and :url."
  (pcase (emacs-reader-files--format path)
    ("txt" (emacs-reader-files--txt-chapters path))
    ((or "md" "org") (emacs-reader-files--outline-chapters path (emacs-reader-files--format path)))
    ("pdf" (emacs-reader-files--pdf-chapters path))
    ("mobi" (emacs-reader-files--with-mobi-epub path #'emacs-reader-files--mobi-chapters))
    (_ (emacs-reader-files--epub-chapters path (emacs-reader-files--package path)))))

(defun emacs-reader-files--inspect (path)
  "Return (METADATA CHAPTERS) for local book PATH."
  (pcase (emacs-reader-files--format path)
    ("mobi" (emacs-reader-files--with-mobi-epub path #'emacs-reader-files--mobi-inspect))
    ("pdf" (let* ((chapters (emacs-reader-files--pdf-chapters path t))
                  (image (string-prefix-p "image-pdf:" (plist-get (car chapters) :url))))
             (list (list :title (file-name-base path) :image-pdf image) chapters)))
    ("epub" (let ((package (emacs-reader-files--package path)))
              (list (emacs-reader-files--epub-metadata path (cdr package))
                    (emacs-reader-files--epub-chapters path package))))
    (_ (list (emacs-reader-files-metadata path) (emacs-reader-files-chapters path)))))

(defun emacs-reader-files--epub-chapters (path package-document)
  "Return chapters of PATH using PACKAGE-DOCUMENT."
  (pcase-let* ((`(,package . ,doc) package-document)
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
    (or (nreverse chapters) (user-error "EPUB has no readable chapters"))))

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
    (pcase (emacs-reader-files--format path)
      ((or "txt" "md" "org")
       (emacs-reader-files--range-content path (emacs-reader-files--format path) url))
      ("pdf" (emacs-reader-files--pdf-content path url))
      ("mobi" (emacs-reader-files--with-mobi-epub path #'emacs-reader-files-content url))
      (_
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
             (string-trim (replace-regexp-in-string "\n\\{3,\\}" "\n\n" lines)))))))))

(provide 'emacs-reader-files)
;;; emacs-reader-files.el ends here
