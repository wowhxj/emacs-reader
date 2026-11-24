;;; emacs-reader-local.el --- Local shelf and source registry -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Store one JSON record per book and source.  Imported book files are owned
;; by the library, so downloaded temporary files can be released immediately.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defgroup emacs-reader-local nil "Local reader library." :group 'emacs-reader)
(defcustom emacs-reader-directory (locate-user-emacs-file "emacs-reader/")
  "Directory for the local bookshelf, books and source definitions."
  :type 'directory :group 'emacs-reader-local)
(defcustom emacs-reader-backend 'local
  "Bookshelf and search backend used by Reader commands."
  :type '(choice (const local) (const reader3)) :group 'emacs-reader-local)

(autoload 'emacs-reader-files-chapters "emacs-reader-files")
(autoload 'emacs-reader-files-content "emacs-reader-files")
(autoload 'emacs-reader-files-metadata "emacs-reader-files")
(autoload 'emacs-reader-legado-search "emacs-reader-legado")
(autoload 'emacs-reader-legado-chapters "emacs-reader-legado")
(autoload 'emacs-reader-legado-content "emacs-reader-legado")
(autoload 'emacs-reader-opds-browse "emacs-reader-opds" nil t)
(autoload 'emacs-reader-openlibrary-search "emacs-reader-opds" nil t)
(declare-function emacs-reader-shelf-refresh "emacs-reader")
(declare-function emacs-reader--start-search "emacs-reader")

(defun emacs-reader-local--record (folder key)
  "Return the JSON record path for KEY in FOLDER."
  (expand-file-name (concat folder "/" (secure-hash 'sha256 key) ".json")
                    emacs-reader-directory))

(defun emacs-reader-local--read (path)
  "Read a UTF-8 JSON record from PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)) (insert-file-contents path))
    (json-parse-buffer :object-type 'alist :false-object :false :null-object :null)))

(defun emacs-reader-local--records (folder)
  "Read all JSON records in library FOLDER."
  (let ((dir (expand-file-name folder emacs-reader-directory)))
    (when (file-directory-p dir)
      (mapcar #'emacs-reader-local--read (directory-files dir t "\\.json\\'")))))

(defun emacs-reader-local--write (path record)
  "Write RECORD to PATH using an atomic rename."
  (make-directory (file-name-directory path) t)
  (let ((temp (make-temp-file (expand-file-name ".reader-" (file-name-directory path)))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temp
              (let ((json (json-serialize record :false-object :false :null-object :null)))
                (insert (if (multibyte-string-p json) json
                          (decode-coding-string json 'utf-8))))))
          (rename-file temp path t))
      (when (file-exists-p temp) (delete-file temp)))))

(defun emacs-reader-local--shelf-path (book)
  "Return the shelf entry path for BOOK."
  (emacs-reader-local--record "shelf" (concat (alist-get 'origin book) "\0"
                                              (alist-get 'bookUrl book))))

(defun emacs-reader-local--add (book)
  "Add BOOK to the local shelf, preserving saved progress on reimport."
  (let ((path (emacs-reader-local--shelf-path book)))
    (unless (file-exists-p path) (emacs-reader-local--write path book)))
  (when-let* ((shelf (get-buffer "*Reader Shelf*")))
    (when (eq emacs-reader-backend 'local)
      (with-current-buffer shelf (emacs-reader-shelf-refresh))))
  book)

(defun emacs-reader-local--progress (book chapter index total)
  "Save CHAPTER at INDEX for shelf BOOK with TOTAL chapters."
  (let ((path (emacs-reader-local--shelf-path book)))
    (when (file-exists-p path)
      (let ((record (emacs-reader-local--read path)))
        (setf (alist-get 'durChapterIndex record) index
              (alist-get 'durChapterTitle record) (alist-get 'title chapter)
              (alist-get 'totalChapterNum record) total
              (alist-get 'durChapterTime record) (floor (* 1000 (float-time))))
        (emacs-reader-local--write path record)))))

;;;###autoload
(defun emacs-reader-import-file (path &optional title author origin)
  "Copy an EPUB or TXT at PATH to the local shelf with TITLE, AUTHOR and ORIGIN."
  (interactive "f导入 EPUB 或 TXT：")
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (unless (member ext '("epub" "txt")) (user-error "支持 EPUB 和 TXT 文件"))
    (let* ((metadata (emacs-reader-files-metadata path))
           (chapters (emacs-reader-files-chapters path))
           (digest (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally path)
                     (secure-hash 'sha256 (current-buffer))))
           (relative (concat "books/" digest "." ext))
           (target (expand-file-name relative emacs-reader-directory)))
      (unless chapters (user-error "文件中没有可读章节"))
      (make-directory (file-name-directory target) t)
      (unless (file-exists-p target)
        (let ((temp (make-temp-file (expand-file-name ".book-" (file-name-directory target)))))
          (unwind-protect (progn (copy-file path temp t) (rename-file temp target t))
            (when (file-exists-p temp) (delete-file temp)))))
      (prog1 (emacs-reader-local--add
              `((readerType . "file") (bookUrl . ,relative) (origin . "file")
                (originName . ,(or origin "本地文件"))
                (name . ,(or title (plist-get metadata :title) (file-name-base path)))
                (author . ,(or author (plist-get metadata :author) ""))
                (totalChapterNum . ,(length chapters))))
        (message "已加入本地书架")))))

(defun emacs-reader-local--source-path (type url)
  "Locate the record for a source of TYPE identified by URL."
  (emacs-reader-local--record "sources" (concat type "\0" url)))

(defun emacs-reader-local--source-url (url)
  "Return URL if it identifies an HTTP or HTTPS source."
  (unless (and (stringp url) (string-match-p "\\`https?://[^[:space:]/]+" url))
    (user-error "书源需要 HTTP(S) 地址"))
  url)

;;;###autoload
(defun emacs-reader-add-opds (name url)
  "Register an OPDS catalog with NAME and URL."
  (interactive "s目录名称：\nsOPDS 地址：")
  (emacs-reader-local--source-url url)
  (emacs-reader-local--write (emacs-reader-local--source-path "opds" url)
                             `((type . "opds") (name . ,name) (url . ,url)))
  (message "已保存 OPDS 目录 %s" name))

(defun emacs-reader-local--legado-json (input)
  "Read the Legado JSON file or remote URL named by INPUT."
  (if (string-match-p "\\`https?://" input)
      (let ((buf (url-retrieve-synchronously input t t 20)))
        (unless buf (user-error "书源下载失败"))
        (unwind-protect
            (with-current-buffer buf
              (unless (and (integerp url-http-response-status)
                           (<= 200 url-http-response-status 299))
                (user-error "书源下载失败 HTTP %s" url-http-response-status))
              (goto-char url-http-end-of-headers)
              (json-parse-string (decode-coding-string
                                  (buffer-substring-no-properties (point) (point-max)) 'utf-8)
                                 :object-type 'alist :false-object :false :null-object :null))
          (kill-buffer buf)))
    (emacs-reader-local--read (expand-file-name input))))

;;;###autoload
(defun emacs-reader-import-legado (input)
  "Register Legado sources from a JSON file or HTTP(S) URL INPUT."
  (interactive (list (if current-prefix-arg (read-string "书源 JSON URL：")
                       (read-file-name "Legado 书源 JSON："))))
  (let* ((json (emacs-reader-local--legado-json input))
         (sources (if (vectorp json) (append json nil) (list json))))
    (dolist (source sources)
      (unless (and (consp source) (stringp (alist-get 'bookSourceName source)))
        (user-error "Legado 书源缺少名称"))
      (emacs-reader-local--source-url (alist-get 'bookSourceUrl source)))
    (dolist (source sources)
      (let ((url (alist-get 'bookSourceUrl source)))
        (emacs-reader-local--write (emacs-reader-local--source-path "legado" url)
                                   `((type . "legado") (url . ,url)
                                     (name . ,(alist-get 'bookSourceName source))
                                     (definition . ,source)))))
    (message "已导入 %d 个 Legado 书源" (length sources))))

(defun emacs-reader-local--plist (record)
  "Convert JSON alist RECORD into a provider-facing plist."
  (json-parse-string (json-serialize record :false-object :false :null-object :null)
                     :object-type 'plist :array-type 'list :false-object nil :null-object nil))

(defun emacs-reader-local--select (types)
  "Prompt for one of the registered sources matching TYPES."
  (let* ((built-in '((type . "openlibrary") (name . "Open Library")
                     (url . "https://openlibrary.org")))
         (sources (cons built-in (emacs-reader-local--records "sources")))
         (choices (cl-loop for src in sources
                           for definition = (alist-get 'definition src)
                           when (and (member (alist-get 'type src) types)
                                     (not (eq :false (alist-get 'enabled definition))))
                           collect (cons (format "%s [%s] <%s>" (alist-get 'name src)
                                                 (alist-get 'type src) (alist-get 'url src)) src))))
    (cdr (assoc (completing-read "选择书源：" choices nil t) choices))))

;;;###autoload
(defun emacs-reader-browse-catalog ()
  "Browse a registered OPDS catalog or search Open Library."
  (interactive)
  (let ((src (emacs-reader-local--select '("opds" "openlibrary"))))
    (if (equal "opds" (alist-get 'type src))
        (emacs-reader-opds-browse (alist-get 'name src) (alist-get 'url src))
      (call-interactively #'emacs-reader-openlibrary-search))))

(defun emacs-reader-local--search (keyword)
  "Select a local source and start searching for KEYWORD."
  (let ((src (emacs-reader-local--select '("legado" "openlibrary"))))
    (if (equal (alist-get 'type src) "openlibrary")
        (emacs-reader-openlibrary-search keyword)
      (emacs-reader--start-search keyword (list :source (alist-get 'url src)
                                                :native (emacs-reader-local--plist
                                                         (alist-get 'definition src)))))))

(defun emacs-reader-local--book (result)
  "Convert Legado search RESULT into a shelf book."
  (cons '(readerType . "legado")
        (cl-loop for (key value) on result by #'cddr when (stringp value)
                 collect (cons (intern (substring (symbol-name key) 1)) value))))

(defun emacs-reader-local--source (book)
  "Return the current source definition for BOOK."
  (let ((path (emacs-reader-local--source-path "legado" (alist-get 'origin book))))
    (unless (file-readable-p path) (user-error "请重新导入该书的 Legado 书源"))
    (emacs-reader-local--plist (alist-get 'definition (emacs-reader-local--read path)))))

(defun emacs-reader-local--fetch (book chapter callback)
  "Fetch BOOK's catalog or CHAPTER, calling CALLBACK with value and error."
  (let ((filep (equal (alist-get 'readerType book) "file")) result failure)
    (condition-case err
        (setq result
              (if filep
                  (let ((path (expand-file-name (alist-get 'bookUrl book) emacs-reader-directory)))
                    (if chapter (emacs-reader-files-content path chapter)
                      (let ((items (emacs-reader-files-chapters path)))
                        ;; The stored filename is a digest.  Use the shelf title for a TXT preface.
                        (when (equal (plist-get (car items) :title) (file-name-base path))
                          (setf (plist-get (car items) :title) (alist-get 'name book)))
                        items)))
                (emacs-reader-local--source book)))
      (error (setq failure (error-message-string err))))
    (cond (failure (funcall callback nil failure))
          (filep (funcall callback result nil))
          (chapter (emacs-reader-legado-content result chapter callback))
          (t (emacs-reader-legado-chapters result (emacs-reader-local--plist book) callback)))))

(provide 'emacs-reader-local)
;;; emacs-reader-local.el ends here
