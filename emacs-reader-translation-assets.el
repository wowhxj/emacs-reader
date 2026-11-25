;;; emacs-reader-translation-assets.el --- Editable bilingual Org books -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Each source edition has an immutable address.  The translations inside
;; are editable Org fixed-width text.  Readers always reopen the saved file.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'emacs-reader-local)

(defcustom emacs-reader-translation-directory nil
  "Directory for editable bilingual Org documents.
When nil, use translations/ under `emacs-reader-directory'.  Relative
names are resolved against the reader library."
  :type '(choice (const :tag "Inside the reader library" nil) directory)
  :group 'emacs-reader-local)

(defun emacs-reader-translation-asset-root ()
  "Return the local asset root directory without creating it."
  (let ((root (expand-file-name (or emacs-reader-translation-directory "translations")
                                emacs-reader-directory)))
    (when (file-remote-p root)
      (user-error "译文目录须为本地目录"))
    (file-name-as-directory root)))

(defun emacs-reader-translation-asset--check-path (file)
  "Check that FILE stays within the configured translation library."
  (let ((root (file-name-as-directory (file-truename (emacs-reader-translation-asset-root)))))
    (when (or (file-remote-p file) (file-symlink-p (directory-file-name file))
              (not (string-prefix-p root (file-truename file))))
      (user-error "译文路径超出目录或为符号链接")))
  file)

(defun emacs-reader-translation-asset--digest (value)
  "Return the SHA-256 digest of JSON VALUE, independently of print settings."
  (secure-hash 'sha256 (encode-coding-string (json-serialize value) 'utf-8-unix)))

(defun emacs-reader-translation-asset--label (value fallback)
  "Get a single-line title from VALUE, using FALLBACK for paths or URLs."
  (if (or (not (stringp value)) (string-blank-p value)
          (string-match-p "://\\|[?=]\\|\\`[/~]\\|\\`[A-Za-z]:[\\\\/]" value))
      fallback
    (string-trim (replace-regexp-in-string "[[:cntrl:]]+" " " value))))

(defun emacs-reader-translation-asset--slug (value)
  "Return a short Unicode filename component for display label VALUE."
  (substring (replace-regexp-in-string "[^[:alnum:]_-]" "-" value)
             0 (min 24 (length value))))

(defun emacs-reader-translation-asset-book-directory (book)
  "Return the stable asset directory for BOOK without creating it."
  (let* ((name (alist-get 'name book))
         (kind (alist-get 'readerType book))
         (origin-name (alist-get 'originName book))
         (url (alist-get 'bookUrl book))
         (file-book (equal kind "file"))
         (fallback (cond ((not file-book) (vector (alist-get 'origin book) url))
                         ((or (not (stringp name)) (string-blank-p name)
                              (not (stringp origin-name)) (string-blank-p origin-name))
                          ;; Library imports have a content-addressed basename.
                          (if (and (stringp url)
                                   (string-match-p "/books/[[:xdigit:]]\\{64\\}\\.[^.]+\\'" url))
                              (concat "books/" (file-name-nondirectory url)) url))))
         (identity (vector kind name (or (alist-get 'author book) "") origin-name fallback))
         (dir (format "%s-%s/" (emacs-reader-translation-asset--slug
                                (emacs-reader-translation-asset--label name "Book"))
                      (substring (emacs-reader-translation-asset--digest identity) 0 24))))
    (emacs-reader-translation-asset--check-path
     (expand-file-name dir (emacs-reader-translation-asset-root)))))

(defun emacs-reader-translation-asset--chapter-title (book chapter)
  "Return a stable chapter title for BOOK and CHAPTER.
The first section of an unheaded TXT is labelled with the temporary or
imported filename by the parser.  Use the book name for this section."
  (let ((title (alist-get 'title chapter)) (url (alist-get 'bookUrl book)))
    (if (and (equal (alist-get 'readerType book) "file") (stringp url)
             (string-prefix-p "txt:" (or (plist-get (alist-get 'native chapter) :url) ""))
             (equal title (file-name-base url)))
        (alist-get 'name book)
      title)))

(defun emacs-reader-translation-asset-path (book chapter direction paragraphs)
  "Return the absolute Org path for BOOK, CHAPTER, DIRECTION and PARAGRAPHS."
  (unless (memq direction '(en-zh zh-en)) (user-error "未知翻译方向"))
  (let* ((title (emacs-reader-translation-asset--chapter-title book chapter))
         (index (alist-get 'index chapter))
         (anchor (or (alist-get 'anchor chapter)
                     (plist-get (alist-get 'native chapter) :anchor)
                     (when (equal (alist-get 'readerType book) "file")
                       (plist-get (alist-get 'native chapter) :url))))
         (chapter-id (emacs-reader-translation-asset--digest (vector index anchor title)))
         (filename (format "%s-%s-%s-%s-%s.org" (if (natnump index) (1+ index) "chapter")
                           (emacs-reader-translation-asset--slug
                            (emacs-reader-translation-asset--label title "Chapter"))
                           (substring chapter-id 0 16) direction
                           (emacs-reader-translation-asset--digest (vconcat paragraphs)))))
    (emacs-reader-translation-asset--check-path
     (expand-file-name filename (emacs-reader-translation-asset-book-directory book)))))

(defconst emacs-reader-translation-asset--end "# End of bilingual text")

(defun emacs-reader-translation-asset--expect (line)
  "Consume a literal structural LINE or report the local error."
  (unless (and (looking-at (concat (regexp-quote line) "$")) (not (eobp)))
    (error "译文文档第 %d 行应为 %s" (line-number-at-pos) line))
  (forward-line 1))

(defun emacs-reader-translation-asset--text ()
  "Consume Org fixed-width lines and return their literal text."
  (let (lines)
    (while (looking-at ":\\(?: \\(.*\\)\\)?$")
      (push (or (match-string-no-properties 1) "") lines)
      (forward-line 1))
    (unless lines (error "译文文档第 %d 行缺少带冒号前缀的正文" (line-number-at-pos)))
    (string-join (nreverse lines) "\n")))

(defun emacs-reader-translation-asset--valid-text-p (source text)
  "Return non-nil if TEXT is a complete translation of SOURCE."
  (and (stringp text) (or (not (string-blank-p text)) (string-blank-p source))))

(defun emacs-reader-translation-asset-read (file paragraphs)
  "Read saved translations from FILE, checking each source against PARAGRAPHS.
Return nil for a missing file.  Signal an error for a damaged document."
  (emacs-reader-translation-asset--check-path file)
  (when (file-exists-p file)
    (unless (file-regular-p file) (user-error "译文路径不是普通文件"))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
      (let ((case-fold-search nil) translations)
        (goto-char (point-min))
        (unless (re-search-forward "^#\\+READER_TRANSLATION: 1$" nil t)
          (error "缺少受支持的双语 Org 格式标记"))
        (forward-line 1)
        (cl-loop for source in paragraphs for i from 1 do
                 (emacs-reader-translation-asset--expect
                  (format (if (looking-at-p "\\* 段落 ") "* 段落 %d" "* 行 %d") i))
                 (emacs-reader-translation-asset--expect "** 原文")
                 (unless (equal source (emacs-reader-translation-asset--text))
                   (error "译文文档第 %d 段与当前原文不符" i))
                 (emacs-reader-translation-asset--expect "** 译文")
                 (let ((text (emacs-reader-translation-asset--text)))
                   (unless (emacs-reader-translation-asset--valid-text-p source text)
                     (error "译文文档第 %d 段缺少译文" i))
                   (push text translations)))
        (emacs-reader-translation-asset--expect emacs-reader-translation-asset--end)
        (unless (looking-at-p "[[:space:]]*\\'") (error "译文文档结尾有额外内容"))
        (nreverse translations)))))

(defun emacs-reader-translation-asset--insert-text (text)
  "Insert TEXT as literal Org fixed-width lines."
  (dolist (line (split-string text "\n" nil))
    (insert (if (string-empty-p line) ":" (concat ": " line)) "\n")))

(defun emacs-reader-translation-asset-write (file book chapter direction paragraphs translations)
  "Publish a complete FILE for BOOK, CHAPTER, DIRECTION and PARAGRAPHS.
Return the published translations.  If another writer or a human already
created FILE, validate it and return its text in preference to TRANSLATIONS."
  (unless (equal file (emacs-reader-translation-asset-path book chapter direction paragraphs))
    (user-error "译文目标与书章或原文版本不符"))
  (if (file-exists-p file) (emacs-reader-translation-asset-read file paragraphs)
    (unless (and paragraphs (proper-list-p paragraphs) (cl-every #'stringp paragraphs)
                 (proper-list-p translations) (= (length translations) (length paragraphs))
                 (cl-every #'emacs-reader-translation-asset--valid-text-p paragraphs translations))
      (error "段落与译文不完整"))
    (make-directory (file-name-directory file) t)
    (let ((temp (make-temp-file (expand-file-name ".translation-" (file-name-directory file)))))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-file temp
                (insert "#+TITLE: "
                        (emacs-reader-translation-asset--label (alist-get 'name book) "Book")
                        " · " (emacs-reader-translation-asset--label
                               (emacs-reader-translation-asset--chapter-title book chapter)
                               "Chapter")
                        " (" (symbol-name direction) ")\n"
                        "# 校订「译文」下的文本。每行开头保留冒号和空格，空行只写冒号。\n"
                        "#+READER_TRANSLATION: 1\n")
                (cl-loop for source in paragraphs for text in translations for i from 1 do
                         (insert (format "* 行 %d\n** 原文\n" i))
                         (emacs-reader-translation-asset--insert-text source)
                         (insert "** 译文\n")
                         (emacs-reader-translation-asset--insert-text text))
                (insert emacs-reader-translation-asset--end "\n")))
            (emacs-reader-translation-asset-read temp paragraphs)
            ;; link(2) publishes the entire file and cannot replace a race winner.
            (condition-case nil (add-name-to-file temp file nil) (file-already-exists nil))
            (emacs-reader-translation-asset-read file paragraphs))
        (delete-file temp)))))

(provide 'emacs-reader-translation-assets)
;;; emacs-reader-translation-assets.el ends here
