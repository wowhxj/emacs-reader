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
(autoload 'emacs-reader-opds-search "emacs-reader-opds")
(autoload 'emacs-reader-opds-browse "emacs-reader-opds" nil t)
(autoload 'emacs-reader-json-catalog-search "emacs-reader-opds")
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

(declare-function emacs-reader--selected-shelf-book "emacs-reader")

(defun emacs-reader-local--delete-path (path)
  "Check PATH is a regular file or missing under a real library subdirectory."
  (when (or (file-remote-p path)
            (file-symlink-p (directory-file-name (expand-file-name emacs-reader-directory)))
            (file-symlink-p (directory-file-name (file-name-directory path)))
            (file-symlink-p path)
            (and (file-exists-p path) (not (file-regular-p path))))
    (user-error "拒绝删除不安全的书架路径 %s" path))
  path)

(defun emacs-reader-local--shared-copy-p (copy record)
  "Return non-nil if a shelf entry other than RECORD references COPY."
  (cl-loop for path in (directory-files (file-name-directory record) t "\\.json\\'")
           unless (equal path record)
           thereis (let* ((other (emacs-reader-local--read (emacs-reader-local--delete-path path)))
                          (url (alist-get 'bookUrl other)))
                     (and (equal (alist-get 'readerType other) "file")
                          (stringp url) (not (file-remote-p url))
                          (file-equal-p (expand-file-name url emacs-reader-directory) copy)))))

(defun emacs-reader-local--shelf-copy (record)
  "Return (ACTION . PATH) for the existing copy owned by RECORD, or nil."
  (let ((book (emacs-reader-local--read (emacs-reader-local--delete-path record))))
    (unless (equal record (emacs-reader-local--shelf-path book))
      (user-error "书架记录已变更，请刷新后重试"))
    (when (equal (alist-get 'readerType book) "file")
      (let ((url (alist-get 'bookUrl book)) (case-fold-search nil))
        (unless (and (stringp url)
                     (string-match-p "\\`books/[0-9a-f]\\{64\\}\\.\\(?:epub\\|txt\\)\\'" url))
          (user-error "拒绝删除非受管书库路径 %s" url))
        (let ((copy (emacs-reader-local--delete-path
                     (expand-file-name url emacs-reader-directory))))
          (when (file-exists-p copy)
            (cons (if (emacs-reader-local--shared-copy-p copy record) 'keep 'delete) copy)))))))

;;;###autoload
(defun emacs-reader-shelf-delete ()
  "Confirm deletion of the local shelf book at point and its unshared copy."
  (interactive)
  (unless (eq emacs-reader-backend 'local) (user-error "Reader3 书架不支持删除"))
  (let* ((book (emacs-reader--selected-shelf-book))
         (record (emacs-reader-local--shelf-path book))
         (copy (emacs-reader-local--shelf-copy record))
         (detail (pcase (car copy)
                   ('delete (format "并删除书库副本 %s" (cdr copy)))
                   ('keep "，保留其他书籍引用的共享副本")
                   (_ ""))))
    (when (yes-or-no-p (format "移除《%s》的书架记录%s? " (alist-get 'name book) detail))
      (let ((current (emacs-reader-local--shelf-copy record)))
        (when (eq (car current) 'delete)
          (unless (equal current copy) (user-error "副本引用已变更，请重新确认删除"))
          (delete-file (cdr current))))
      ;; Keep the record if removing the copy fails.  A missing copy is safe to retry.
      (delete-file record)
      (emacs-reader-shelf-refresh))))

(defun emacs-reader-local--source-path (type url)
  "Locate the record for a source of TYPE identified by URL."
  (emacs-reader-local--record "sources" (concat type "\0" url)))

(defun emacs-reader-local--source-url (url)
  "Return URL if it identifies an HTTP or HTTPS source."
  (unless (and (stringp url)
               (string-match-p "\\`https?://[^/?#[:space:]]+" url)
               (not (string-match-p "[[:space:][:cntrl:]]" url)))
    (user-error "书源需要 HTTP(S) 地址"))
  url)

(autoload 'emacs-reader-sources-add-opds "emacs-reader-sources" nil t)

;;;###autoload
(defun emacs-reader-add-opds (name url)
  "Register an OPDS catalog with NAME and URL."
  (interactive "s目录名称：\nsOPDS 地址：")
  (emacs-reader-sources-add-opds name url))

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
         (sources (if (vectorp json) (append json nil) (list json)))
         (records (emacs-reader-local--records "sources")))
    (dolist (source sources)
      (unless (and (consp source) (stringp (alist-get 'bookSourceName source)))
        (user-error "Legado 书源缺少名称"))
      (let ((url (emacs-reader-local--source-url (alist-get 'bookSourceUrl source))))
        (when-let* ((owner (emacs-reader-local--alias-source url records)))
          (user-error "该地址已由书源「%s」作为旧地址保留" (alist-get 'name owner)))))
    (dolist (source sources)
      (let* ((url (alist-get 'bookSourceUrl source))
             (path (emacs-reader-local--source-path "legado" url))
             (record (when (file-exists-p path) (emacs-reader-local--read path))))
        (setf (alist-get 'type record) "legado" (alist-get 'url record) url
              (alist-get 'name record) (alist-get 'bookSourceName source)
              (alist-get 'definition record) source)
        (emacs-reader-local--write path record)))
    (message "已导入 %d 个 Legado 书源" (length sources))))

(defun emacs-reader-local--plist (record)
  "Convert JSON alist RECORD into a provider-facing plist."
  (json-parse-string (json-serialize record :false-object :false :null-object :null)
                     :object-type 'plist :array-type 'list :false-object nil :null-object nil))

(defun emacs-reader-local--select (types)
  "Prompt for one of the registered sources matching TYPES."
  (let* ((sources (emacs-reader-local--records "sources"))
         (choices (cl-loop for src in sources
                           for definition = (alist-get 'definition src)
                           when (and (member (alist-get 'type src) types)
                                     (not (eq :false (alist-get 'enabled src)))
                                     (not (eq :false (alist-get 'enabled definition))))
                           collect (cons (format "%s [%s] <%s>" (alist-get 'name src)
                                                 (alist-get 'type src) (alist-get 'url src)) src))))
    (unless choices (user-error "没有可用的匹配书源；请在书架按 S 管理书源"))
    (cdr (assoc (completing-read "选择书源：" choices nil t) choices))))

;;;###autoload
(defun emacs-reader-browse-catalog ()
  "Browse a registered OPDS catalog."
  (interactive)
  (let ((src (emacs-reader-local--select '("opds"))))
    (emacs-reader-opds-browse (alist-get 'name src) (alist-get 'url src))))

(defun emacs-reader-local--search (keyword)
  "Select a local source and start searching for KEYWORD."
  (let ((src (emacs-reader-local--select '("legado" "opds" "json"))))
    (pcase (alist-get 'type src)
      ("json" (emacs-reader-json-catalog-search src keyword))
      ("opds" (emacs-reader-opds-search (alist-get 'name src) (alist-get 'url src) keyword))
      ("legado"
       (emacs-reader--start-search
        keyword (list :source (alist-get 'url src)
                      :native (emacs-reader-local--plist (alist-get 'definition src))))))))

(defun emacs-reader-local--book (result)
  "Convert Legado search RESULT into a shelf book."
  (cons '(readerType . "legado")
        (cl-loop for (key value) on result by #'cddr when (stringp value)
                 collect (cons (intern (substring (symbol-name key) 1)) value))))

(defun emacs-reader-local--source (book)
  "Return the current source definition for BOOK."
  (let* ((origin (alist-get 'origin book))
         (path (emacs-reader-local--source-path "legado" origin))
         (record (if (file-readable-p path) (emacs-reader-local--read path)
                   (emacs-reader-local--alias-source
                    origin (emacs-reader-local--records "sources")))))
    (unless record (user-error "请重新导入该书的 Legado 书源"))
    (emacs-reader-local--plist (alist-get 'definition record))))

(defun emacs-reader-local--alias-source (url records)
  "Find a Legado source in RECORDS previously registered under URL."
  (cl-find-if (lambda (src)
                (and (equal (alist-get 'type src) "legado")
                     (seq-contains-p (alist-get 'aliases src) url #'equal)))
              records))

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
