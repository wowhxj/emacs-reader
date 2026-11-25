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
(defcustom emacs-reader-books-directory nil
  "Book storage directory, absolute or relative to `emacs-reader-directory'.
Nil means the books subdirectory of `emacs-reader-directory'.
Shared-book entries resolve against the current setting.  Legacy books/ entries
can also be recovered from this directory after their files are moved."
  :type '(choice (const :tag "Default" nil) directory) :group 'emacs-reader-local)

(defun emacs-reader-local--books-directory (&optional directory)
  "Return the configured books directory under library DIRECTORY."
  (expand-file-name (or emacs-reader-books-directory "books")
                    (or directory emacs-reader-directory)))

(defvar emacs-reader-local--relocations (make-hash-table :test 'equal)
  "Verified relocated files, keyed by library, books directory and URL.")

(defun emacs-reader-local--relocation-key (url)
  "Return the cache key for URL in the current configuration."
  (list (expand-file-name emacs-reader-directory)
        (emacs-reader-local--books-directory) url))

(defun emacs-reader-local--relocated-file (url)
  "Return a previously verified relocated URL if it is unchanged."
  (let* ((entry (gethash (emacs-reader-local--relocation-key url)
                         emacs-reader-local--relocations))
         (path (car-safe entry)))
    (and path (equal (cdr entry) (emacs-reader-local--digest-signature path)) path)))

(defun emacs-reader-local--resolve-file (url)
  "Resolve local book URL against its original or configured storage root."
  (if (string-prefix-p "shared-books/" url)
      (let ((relative (substring url (length "shared-books/"))))
        (when (or (file-name-absolute-p relative)
                  (member ".." (split-string relative "/" t)))
          (error "不安全的书籍路径：%s" url))
        (expand-file-name relative (emacs-reader-local--books-directory)))
    (let ((original (expand-file-name url emacs-reader-directory)))
      (or (and (file-regular-p original) original)
          (and (string-prefix-p "books/" url)
               (emacs-reader-local--relocated-file url))
          original))))

(defcustom emacs-reader-backend 'local
  "Bookshelf and search backend used by Reader commands."
  :type '(choice (const local) (const reader3)) :group 'emacs-reader-local)

(autoload 'emacs-reader-file-worker-start "emacs-reader-file-worker")
(autoload 'emacs-reader-files--inspect "emacs-reader-files")
(autoload 'emacs-reader-files-chapters "emacs-reader-files")
(autoload 'emacs-reader-files-content "emacs-reader-files")
(autoload 'emacs-reader-files-metadata "emacs-reader-files")
(autoload 'emacs-reader-files-supported-p "emacs-reader-files")
(autoload 'emacs-reader-wendian-import-page "emacs-reader-wendian-read")
(autoload 'emacs-reader-legado-search "emacs-reader-legado")
(autoload 'emacs-reader-legado-chapters "emacs-reader-legado")
(autoload 'emacs-reader-legado-content "emacs-reader-legado")
(autoload 'emacs-reader-opds-search "emacs-reader-opds")
(autoload 'emacs-reader-opds-browse "emacs-reader-opds" nil t)
(autoload 'emacs-reader-json-catalog-search "emacs-reader-opds")
(autoload 'emacs-reader-search-all "emacs-reader-search-all")
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

(defun emacs-reader-local--write (path record &optional new-only)
  "Write RECORD atomically to PATH.  With NEW-ONLY, refuse to replace PATH."
  (make-directory (file-name-directory path) t)
  (let ((temp (make-temp-file (expand-file-name ".reader-" (file-name-directory path)))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temp
              (let ((json (json-serialize record :false-object :false :null-object :null)))
                (insert (if (multibyte-string-p json) json
                          (decode-coding-string json 'utf-8))))))
          (rename-file temp path (not new-only)))
      (when (file-exists-p temp) (delete-file temp)))))

(defun emacs-reader-local--shelf-path (book)
  "Return the shelf entry path for BOOK."
  (emacs-reader-local--record "shelf" (concat (alist-get 'origin book) "\0"
                                              (alist-get 'bookUrl book))))

(defun emacs-reader-local--add (book &optional no-refresh)
  "Add BOOK to the local shelf, preserving saved progress on reimport.
With NO-REFRESH, leave any shelf buffer untouched."
  (let ((path (emacs-reader-local--shelf-path book)))
    (unless (file-exists-p path) (emacs-reader-local--write path book)))
  (when-let* ((shelf (and (not no-refresh) (get-buffer "*Reader Shelf*"))))
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

(declare-function emacs-reader-open-book "emacs-reader")

(defvar emacs-reader-local--sync-preview nil)
(defvar emacs-reader--current-book)
(defvar-local emacs-reader-local--retain-file nil)

(defun emacs-reader-local--retain-preview-file ()
  "Keep this preview's source available until the returned function is called."
  (if emacs-reader-local--retain-file (funcall emacs-reader-local--retain-file) #'ignore))

(defun emacs-reader-local--own-preview (path)
  "Release temporary PATH once its preview and outstanding imports finish."
  (let ((users 0))
    (setq-local emacs-reader-local--retain-file
                (lambda ()
                  (cl-incf users)
                  (let ((active t))
                    (lambda ()
                      (when active
                        (setq active nil)
                        (when (and (zerop (cl-decf users)) (file-exists-p path))
                          (delete-file path)))))))
    (let ((release (emacs-reader-local--retain-preview-file)))
      (add-hook 'kill-buffer-hook release nil t)
      (add-hook 'change-major-mode-hook release nil t))))

(defun emacs-reader-preview-file-async (path &optional title author origin temporary)
  "Open PATH immediately and parse its metadata and chapters in the background.
TITLE, AUTHOR and ORIGIN override book metadata.  A returned preview owns
TEMPORARY input until close.  The caller owns it if opening signals."
  (setq path (expand-file-name path))
  (unless (and (not (file-remote-p path)) (file-regular-p path)
               (emacs-reader-files-supported-p path))
    (user-error "请选择支持的本地书籍文件"))
  (let* ((book `((readerType . "file") (previewFile . t) (bookUrl . ,path) (origin . "file")
                 (name . ,(or title (file-name-base path))) (author . ,(or author ""))
                 (originName . ,(or origin "本地文件"))
                 (readFileTitle . ,(null title)) (readFileAuthor . ,(null author))))
         (buffer (emacs-reader-open-book book t)))
    (when temporary (with-current-buffer buffer (emacs-reader-local--own-preview path)))
    buffer))

(defun emacs-reader-preview-file (path &optional title author origin temporary)
  "Synchronously preview PATH with TITLE, AUTHOR, ORIGIN and TEMPORARY ownership."
  (let ((emacs-reader-local--sync-preview t)
        (meta (emacs-reader-files-metadata path)))
    (emacs-reader-preview-file-async path (or title (plist-get meta :title))
                                     (or author (plist-get meta :author) "") origin temporary)))

(defvar emacs-reader-local--picker-request nil)

(defun emacs-reader-local--choose-epub (choices)
  "Open the EPUB selected from labelled CHOICES or the file browser."
  (let* ((other "选择其他 EPUB 文件…")
         (choice (when choices
                   (completing-read "试读本地版本：" (append choices (list (list other))) nil t)))
         (file (or (cdr (assoc choice choices)) (read-file-name "选择要试读的 EPUB：" nil nil t))))
    (emacs-reader-preview-file-async file)))

;;;###autoload
(defun emacs-reader-preview-epub (file)
  "Preview local EPUB FILE.  Interactively collect LightRead titles in the background."
  (interactive (list nil))
  (if file (emacs-reader-preview-file file)
    (let ((dir (expand-file-name "~/Library/Application Support/com.yzfly.lightread/books/"))
          (window (selected-window)) (buffer (current-buffer)) (request (list 'preview)))
      (setq emacs-reader-local--picker-request request)
      (if (not (file-directory-p dir)) (emacs-reader-local--choose-epub nil)
        (message "正在读取本地 EPUB 书名…")
        (emacs-reader-file-worker-start
         'titles (list dir)
         (lambda (choices failure)
           (when (and (eq request emacs-reader-local--picker-request)
                      (eq window (selected-window)) (eq buffer (window-buffer window))
                      (not (active-minibuffer-window)))
             (if failure (message "读取本地书名失败：%s" failure)
               (with-current-buffer buffer (emacs-reader-local--choose-epub choices))))))))))

;;;###autoload
(defun emacs-reader-import-file (path &optional title author origin background)
  "Import PATH with TITLE, AUTHOR and ORIGIN.  With BACKGROUND, return a worker.
Interactive imports run in the background.  Lisp callers normally receive a book."
  (interactive (list (read-file-name "导入 EPUB、TXT、PDF、MOBI、Markdown 或 Org：" nil nil t)
                     nil nil nil t))
  (if background
      (emacs-reader-import-file-async path title author origin)
    (prog1 (emacs-reader-local--add
            (emacs-reader-local--prepare-import path title author origin emacs-reader-directory
                                                emacs-reader-books-directory))
      (message "已加入本地书架"))))

(defun emacs-reader-local--digest (path)
  "Return the SHA-256 digest of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun emacs-reader-local--digest-signature (path)
  "Return a reliable signature for PATH, or nil if unavailable."
  (condition-case nil
      (let* ((attrs (file-attributes path 'integer))
             (device (file-attribute-device-number attrs))
             (inode (file-attribute-inode-number attrs))
             (size (file-attribute-size attrs))
             (mtime (file-attribute-modification-time attrs))
             (ctime (file-attribute-status-change-time attrs)))
        (when (and attrs (integerp device) (integerp inode) (integerp size)
                   mtime ctime (null (file-attribute-type attrs)))
          (prin1-to-string (list device inode size mtime ctime))))
    (file-error nil)))

(defun emacs-reader-local--cached-digest (path)
  "Return SHA-256 of PATH, caching it when its file signature is stable.
Cache entries are independent per path and may be discarded at any time."
  (setq path (expand-file-name path))
  (let* ((signature (and (not (file-remote-p path))
                         (emacs-reader-local--digest-signature path)))
         (index (emacs-reader-local--record "digest-index" path))
         (entry (and signature (not (file-symlink-p index))
                     (condition-case nil
                         (let ((value (emacs-reader-local--read index)))
                           (and (listp value)
                                (cl-every #'consp value) value))
                       (error nil))))
         (cached (alist-get 'digest entry)))
    (if (and (equal signature (alist-get 'signature entry))
             (stringp cached) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" cached))
        cached
      (let ((digest (emacs-reader-local--digest path)))
        (when (and signature (equal signature (emacs-reader-local--digest-signature path)))
          ;; A failed or competing cache write must never prevent an import.
          (condition-case nil
              (emacs-reader-local--write index `((signature . ,signature) (digest . ,digest)))
            (error nil)))
        digest))))

(defun emacs-reader-local--find-digest (digest)
  "Find a shelf book whose file has DIGEST, preserving its record."
  (cl-loop for book in (emacs-reader-local--records "shelf")
           for url = (alist-get 'bookUrl book)
           when (and (equal (alist-get 'readerType book) "file") (stringp url)
                     (not (file-remote-p url))
                     (let ((path (emacs-reader-local--resolve-file url)))
                       (if (file-regular-p path)
                           (condition-case nil
                               (equal digest (emacs-reader-local--cached-digest path))
                             (file-error nil))
                         (and (string-match-p
                               (concat "\\(?:\\`\\|--\\)" (regexp-quote digest) "\\.[^.]+\\'")
                               (file-name-nondirectory url))
                              (plist-get (emacs-reader-local--relocation-scan
                                          (list url) emacs-reader-directory
                                          emacs-reader-books-directory) :matches)))))
           return book))

(defun emacs-reader-local--scan-directory (directory)
  "Find supported regular files under DIRECTORY without following symbolic links.
Return a plist with :files, :duplicates and :errors."
  (let ((pending (list directory)) (seen (make-hash-table :test 'equal))
        files errors (duplicates 0))
    (while pending
      (let ((dir (pop pending)))
        (condition-case err
            (dolist (path (directory-files dir t directory-files-no-dot-files-regexp t))
              (condition-case failure
                  (unless (file-symlink-p path)
                    (cond ((file-directory-p path) (push path pending))
                          ((and (file-regular-p path) (emacs-reader-files-supported-p path))
                           (let* ((attrs (file-attributes path 'integer))
                                  (identity (cons (file-attribute-device-number attrs)
                                                  (file-attribute-inode-number attrs))))
                             (if (gethash identity seen) (cl-incf duplicates)
                               (puthash identity t seen) (push path files))))))
                (file-error (push (format "%s: %s" path (error-message-string failure)) errors))))
          (file-error (push (format "%s: %s" dir (error-message-string err)) errors)))))
    (list :files (nreverse files) :duplicates duplicates :errors (nreverse errors))))

(defun emacs-reader-local--relocation-digest (path)
  "Hash PATH in constant memory when an external SHA-256 tool is available."
  (let ((program (or (executable-find "sha256sum")
                     (and (file-executable-p "/sbin/sha256sum") "/sbin/sha256sum")
                     (executable-find "shasum"))))
    (if (not program) (emacs-reader-local--digest path)
      (with-temp-buffer
        (let ((status (apply #'process-file program nil t nil
                             (append (when (string= (file-name-nondirectory program) "shasum")
                                       '("-a" "256"))
                                     (list path)))))
          (goto-char (point-min))
          (unless (and (eq status 0) (looking-at "[0-9a-f]\\{64\\}"))
            (error "SHA-256 计算失败：%s" path))
          (match-string 0))))))

(defun emacs-reader-local--relocation-cached-digest (path)
  "Use the digest cache for PATH with a streaming hash on cache misses."
  (let ((original (symbol-function 'emacs-reader-local--digest)))
    (cl-letf (((symbol-function 'emacs-reader-local--digest)
               (lambda (file)
                 (if (or (executable-find "sha256sum")
                         (and (file-executable-p "/sbin/sha256sum") "/sbin/sha256sum")
                         (executable-find "shasum"))
                     (emacs-reader-local--relocation-digest file)
                   (funcall original file)))))
      (emacs-reader-local--cached-digest path))))

(defun emacs-reader-local--relocation-scan (urls directory books-directory)
  "Find relocated URLS in BOOKS-DIRECTORY for library DIRECTORY in a worker."
  (let* ((emacs-reader-directory directory)
         (emacs-reader-books-directory books-directory)
         (root (emacs-reader-local--books-directory))
         (scan (and (file-directory-p root) (emacs-reader-local--scan-directory root)))
         (files (plist-get scan :files)) results (missing 0) (ambiguous 0) (unverified 0))
    (dolist (url urls)
      (when (and (stringp url) (string-prefix-p "books/" url)
                 (not (member ".." (split-string url "/" t)))
                 (not (file-regular-p (expand-file-name url directory))))
        (let* ((relative (substring url 6))
               (exact (expand-file-name relative root))
               (base (file-name-nondirectory relative))
               (digest (and (string-match "\\(?:\\`\\|--\\)\\([0-9a-f]\\{64\\}\\)\\.[^.]+\\'" base)
                            (match-string 1 base)))
               (candidates (if (member exact files) (list exact)
                             (when digest
                               (cl-remove-if-not
                                (lambda (path) (equal (file-name-nondirectory path) base)) files))))
               (matches (if digest
                            (cl-remove-if-not
                             (lambda (path)
                               (condition-case nil
                                   (let ((before (emacs-reader-local--digest-signature path)))
                                     (and before
                                          (equal digest
                                                 (emacs-reader-local--relocation-cached-digest
                                                  path))
                                          (equal before
                                                 (emacs-reader-local--digest-signature path))))
                                 (file-error nil))) candidates)
                          candidates)))
          (cond ((= (length matches) 1)
                 (let* ((path (car matches))
                        (signature (emacs-reader-local--digest-signature path)))
                   (when signature
                     (unless digest (cl-incf unverified))
                     (push (list url path signature) results))))
                ((> (length matches) 1) (cl-incf ambiguous))
                (t (cl-incf missing))))))
    (list :matches (nreverse results) :missing missing :ambiguous ambiguous
          :unverified unverified :errors (length (plist-get scan :errors)))))

(defun emacs-reader-local--recover-async (urls callback)
  "Find relocated URLS asynchronously and invoke CALLBACK after indexing."
  (let ((directory (expand-file-name emacs-reader-directory))
        (books-directory (emacs-reader-local--books-directory)))
    (emacs-reader-file-worker-start
     'relocate (list urls directory books-directory)
     (lambda (result failure)
       (unless failure
         (dolist (entry (plist-get result :matches))
           (puthash (list directory books-directory (nth 0 entry))
                    (cons (nth 1 entry) (nth 2 entry)) emacs-reader-local--relocations)))
       (funcall callback result failure)))))

(defun emacs-reader-local--prepare-import
    (path title author origin directory &optional books-directory)
  "Import PATH into DIRECTORY with TITLE, AUTHOR, ORIGIN and BOOKS-DIRECTORY."
  (let ((emacs-reader-books-directory books-directory))
    (if (eq title :scan-directory)
        (emacs-reader-local--scan-directory path)
      (unless (emacs-reader-files-supported-p path) (user-error "不支持的书籍格式：%s" path))
      (let* ((ext (downcase (or (file-name-extension path) "")))
             (emacs-reader-directory directory)
             (digest (emacs-reader-local--cached-digest path))
             (existing (emacs-reader-local--find-digest digest))
             (inspection (unless existing (emacs-reader-files--inspect path)))
             (metadata (car inspection))
             (chapters (cadr inspection))
             (name (or title (plist-get metadata :title) (file-name-base path)))
             (safe-name (replace-regexp-in-string "[[:cntrl:]/\\:*?\"<>|]+" "_" name))
             (safe-name (string-trim safe-name "[. ]+" "[. ]+"))
             (safe-name (if (string-empty-p safe-name) "book"
                          (truncate-string-to-width safe-name 80)))
             (books (emacs-reader-local--books-directory directory))
             (inside (and (file-directory-p books)
                          (file-in-directory-p (file-truename path) (file-truename books))))
             (old-copy (and (not inside) (file-directory-p books)
                            (cl-find-if
                             (lambda (candidate)
                               (and (file-regular-p candidate)
                                    (not (file-symlink-p candidate))
                                    (equal digest (emacs-reader-local--cached-digest candidate))))
                             (directory-files books t
                                              (concat "\\(?:^\\|--\\)" digest "\\.[^.]+\\'")))))
             (relative (concat (if (equal (directory-file-name (expand-file-name books))
                                          (directory-file-name
                                           (expand-file-name "books" directory)))
                                   "books/" "shared-books/")
                               (cond (inside (file-relative-name (file-truename path)
                                                                 (file-truename books)))
                                     (old-copy (file-relative-name old-copy books))
                                     (t (concat safe-name "--" digest "." ext)))))
             (target (expand-file-name (substring relative (1+ (string-match "/" relative)))
                                       books)))
        (unless (or existing chapters) (user-error "文件中没有可读章节"))
        (or existing
            (progn
              (unless inside
                (make-directory (file-name-directory target) t)
                (unless (file-exists-p target)
                  (let ((temp (make-temp-file
                               (expand-file-name ".book-" (file-name-directory target)))))
                    (unwind-protect
                        (progn (copy-file path temp t)
                               (condition-case nil (rename-file temp target nil)
                                 (file-already-exists nil)))
                      (when (file-exists-p temp) (delete-file temp)))))
                (unless (equal digest (emacs-reader-local--cached-digest target))
                  (error "书库目标文件冲突：%s" target)))
              `((readerType . "file") (bookUrl . ,relative) (origin . "file")
                (readerOwned . ,(if inside :false t))
                ,@(when (plist-get metadata :image-pdf) '((readerImagePdf . t)))
                (originName . ,(or origin "本地文件"))
                (name . ,(or title (plist-get metadata :title) (file-name-base path)))
                (author . ,(or author (plist-get metadata :author) ""))
                (totalChapterNum . ,(length chapters)))))))))

(defun emacs-reader-import-file-async (path &optional title author origin callback temporary)
  "Import PATH with TITLE, AUTHOR and ORIGIN, then call CALLBACK with (BOOK ERROR).
Once the worker starts, it owns TEMPORARY input until completion."
  (let ((directory (expand-file-name emacs-reader-directory))
        (books-directory emacs-reader-books-directory)
        (path (expand-file-name path)))
    (emacs-reader-file-worker-start
     'import (list path title author origin directory books-directory)
     (lambda (book failure)
       (unwind-protect
           (unless failure
             (condition-case err
                 (let ((emacs-reader-directory directory))
                   (emacs-reader-local--add book))
               (error (setq failure (error-message-string err) book nil))))
         (when (and temporary (file-exists-p path))
           (condition-case err (delete-file path)
             (file-error (message "临时书籍清理失败：%s" (error-message-string err))))))
       (if callback (funcall callback book failure)
         (if failure (message "导入失败：%s" failure) (message "已加入本地书架")))))))

(defvar emacs-reader-local--directory-imports (make-hash-table :test 'equal)
  "Active directory imports keyed by their library directory.")

;;;###autoload
(defun emacs-reader-cancel-directory-import ()
  "Cancel the directory import for the current library."
  (interactive)
  (let* ((library (file-name-as-directory (expand-file-name emacs-reader-directory)))
         (task (gethash library emacs-reader-local--directory-imports)))
    (unless task (user-error "当前书库没有正在进行的目录导入"))
    (funcall task)))

;;;###autoload
(defun emacs-reader-import-directory (directory &optional callback)
  "Import supported files recursively from DIRECTORY in the background.
CALLBACK, when non-nil, receives a plist with counts and errors."
  (interactive (list (read-directory-name "选择要递归导入的目录：" nil nil t)))
  (when (or (file-remote-p directory) (not (file-directory-p directory)))
    (user-error "请选择本地目录"))
  (let* ((library (file-name-as-directory (expand-file-name emacs-reader-directory)))
         (books-directory emacs-reader-books-directory)
         (root (expand-file-name directory))
         (added 0) (duplicates 0) (done 0) (total 0) errors files
         (phase 'scanning) process cancelled finished cancel)
    (when (gethash library emacs-reader-local--directory-imports)
      (user-error "当前书库已有目录导入任务"))
    (cl-labels
        ((finish ()
           (unless finished
             (setq finished t)
             (when (eq (gethash library emacs-reader-local--directory-imports) cancel)
               (remhash library emacs-reader-local--directory-imports))
             (let ((result (list :cancelled cancelled :done done :added added
                                 :duplicates duplicates :errors (nreverse errors)
                                 :total (unless (eq phase 'scanning) total))))
               (when errors
                 (with-current-buffer (get-buffer-create "*Reader Import Errors*")
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert (mapconcat #'identity (plist-get result :errors) "\n"))
                     (special-mode))))
               (when-let* ((shelf (get-buffer "*Reader Shelf*")))
                 (when (and (eq emacs-reader-backend 'local)
                            (equal (file-name-as-directory
                                    (expand-file-name emacs-reader-directory)) library))
                   (with-current-buffer shelf (emacs-reader-shelf-refresh))))
               (message "目录导入%s：已处理 %d/%s，未处理 %s，新增 %d，重复 %d，错误 %d%s"
                        (if cancelled "已取消" "完成") done
                        (if (eq phase 'scanning) "总数未知" (number-to-string total))
                        (if (eq phase 'scanning) "未知" (number-to-string (- total done)))
                        added duplicates (length errors)
                        (if errors "（见 *Reader Import Errors*）" ""))
               (when callback (funcall callback result)))))
         (next ()
           (if (or finished cancelled (null files)) (finish)
             (let ((path (pop files)) called)
               (setq phase 'importing process nil)
               (message "目录检查 %d/%d：%s（已跳过重复 %d，新增 %d）"
                        (1+ done) total (file-name-nondirectory path) duplicates added)
               (condition-case err
                   (emacs-reader-file-worker-start
                    'import (list path nil nil nil library books-directory)
                    (lambda (book failure)
                      (unless (or called finished)
                        (setq called t)
                        (let ((emacs-reader-directory library))
                          (condition-case problem
                              (if failure (push (format "%s: %s" path failure) errors)
                                (if (file-exists-p (emacs-reader-local--shelf-path book))
                                    (cl-incf duplicates)
                                  (emacs-reader-local--add book t)
                                  (cl-incf added)))
                            (error (push (format "%s: %s" path (error-message-string problem))
                                         errors))))
                        (cl-incf done)
                        (next))))
                 (error (push (format "%s: %s" path (error-message-string err)) errors)
                        (cl-incf done) (run-at-time 0 nil #'next)))))))
      (setq cancel (lambda ()
                     (unless cancelled
                       (setq cancelled t)
                       (if (eq phase 'scanning)
                           (progn (when (processp process) (delete-process process))
                                  (finish))
                         (message "正在取消目录导入：已完成 %d/%d 本，当前书完成后停止（剩余至少 %d 本）"
                                  done total (max 0 (1- (- total done))))))))
      (puthash library cancel emacs-reader-local--directory-imports)
      (message "正在后台扫描目录：%s" root)
      (condition-case err
          (setq process (emacs-reader-file-worker-start
                         'import (list root :scan-directory nil nil library books-directory)
                         (lambda (result failure)
                           (unless finished
                             (if cancelled (finish)
                               (if failure
                                   (progn (push (format "%s: %s" root failure) errors) (finish))
                                 (setq files (plist-get result :files)
                                       duplicates (plist-get result :duplicates)
                                       errors (nreverse (plist-get result :errors))
                                       total (length files) phase 'importing)
                                 (next)))))))
        (error (when (eq (gethash library emacs-reader-local--directory-imports) cancel)
                 (remhash library emacs-reader-local--directory-imports))
               (signal (car err) (cdr err)))))))

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
                          (file-equal-p (emacs-reader-local--resolve-file url) copy)))))

(defun emacs-reader-local--shelf-copy (record)
  "Return (ACTION . PATH) for the existing copy owned by RECORD, or nil."
  (let ((book (emacs-reader-local--read (emacs-reader-local--delete-path record))))
    (unless (equal record (emacs-reader-local--shelf-path book))
      (user-error "书架记录已变更，请刷新后重试"))
    (when (and (equal (alist-get 'readerType book) "file")
               (not (eq (alist-get 'readerOwned book) :false))
               (not (and (stringp (alist-get 'bookUrl book))
                         (string-prefix-p "shared-books/" (alist-get 'bookUrl book)))))
      (let ((url (alist-get 'bookUrl book)) (case-fold-search nil))
        (unless (and (stringp url)
                     (string-match-p (concat "\\`books/\\(?:[^/]+--\\)?[0-9a-f]\\{64\\}\\."
                                             "\\(?:epub\\|txt\\|pdf\\|mobi\\|md\\|"
                                             "markdown\\|org\\)\\'")
                                     url))
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

(defun emacs-reader-local--import-key (type url)
  "Identify a source by TYPE and URL, treating a bare host as its root path."
  (cons type (if (string-match-p (rx string-start "http" (? "s") "://" (+ (not (any "/?#")))
                                     string-end) url)
                 (concat url "/") url)))

(defun emacs-reader-local--import-records (incoming)
  "Import new INCOMING records and report counts of added and skipped sources."
  (let ((seen (make-hash-table :test #'equal)) (added 0))
    (dolist (record (emacs-reader-local--records "sources"))
      (let ((type (alist-get 'type record)))
        (dolist (url (cons (alist-get 'url record) (when (equal type "legado")
                                                     (append (alist-get 'aliases record) nil))))
          (puthash (emacs-reader-local--import-key type url) t seen))))
    (dolist (record incoming)
      (let* ((type (alist-get 'type record)) (url (alist-get 'url record))
             (key (emacs-reader-local--import-key type url)))
        (unless (gethash key seen)
          (condition-case nil
              (progn (emacs-reader-local--write (emacs-reader-local--source-path type url) record t)
                     (cl-incf added))
            (file-already-exists nil))
          (puthash key t seen))))
    (message "已新增 %d 个书源，跳过 %d 个重复条目" added (- (length incoming) added))))

;;;###autoload
(defun emacs-reader-import-legado (input)
  "Register Legado sources from a JSON file or HTTP(S) URL INPUT."
  (interactive (list (if current-prefix-arg (read-string "书源 JSON URL：")
                       (read-file-name "Legado 书源 JSON："))))
  (let* ((json (emacs-reader-local--legado-json input))
         (sources (if (vectorp json) (append json nil) (list json)))
         records)
    (dolist (source sources)
      (unless (and (consp source) (stringp (alist-get 'bookSourceName source)))
        (user-error "Legado 书源缺少名称"))
      (let ((url (emacs-reader-local--source-url (alist-get 'bookSourceUrl source))))
        (push `((type . "legado") (url . ,url) (name . ,(alist-get 'bookSourceName source))
                (definition . ,source)) records)))
    (emacs-reader-local--import-records (nreverse records))))

(defun emacs-reader-local--plist (record)
  "Convert JSON alist RECORD into a provider-facing plist."
  (json-parse-string (json-serialize record :false-object :false :null-object :null)
                     :object-type 'plist :array-type 'list :false-object nil :null-object nil))

(defvar emacs-reader-local--marked nil
  "Sources marked in the active search picker.")

(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--update "ext:vertico")

(defun emacs-reader-local--picker-toggle ()
  "Toggle the current source without moving point or changing the query."
  (interactive)
  (let* ((input (minibuffer-contents-no-properties))
         (table minibuffer-completion-table)
         (vertico (bound-and-true-p vertico--input))
         (candidate (or (when vertico (vertico--update) (vertico--candidate))
                        (and (fboundp 'completion--selected-candidate)
                             (completion--selected-candidate))
                        (and (string-empty-p input) minibuffer-default)
                        (and (assoc input table) input)
                        (let ((matches (all-completions input table)))
                          (and (null (cdr matches)) (car matches)))))
         (src (cdr (assoc candidate table))))
    (unless src (user-error "请先选择一个书源或【全部】"))
    (let ((sources (if (equal (alist-get 'type src) "all") (alist-get 'sources src) (list src))))
      (if (cl-every (lambda (item) (memq item emacs-reader-local--marked)) sources)
          (setq emacs-reader-local--marked (cl-set-difference emacs-reader-local--marked sources))
        (dolist (item sources) (cl-pushnew item emacs-reader-local--marked))))
    ;; Vertico redraws affixes after every command.  Native completion needs a refresh.
    (unless vertico
      (let* ((window (get-buffer-window "*Completions*"))
             (pos (and window (window-point window)))
             (completion-auto-select nil))
        (minibuffer-completion-help)
        (when (and pos (window-live-p window)) (set-window-point window pos))))))

(defun emacs-reader-local--picker-map (scope)
  "Install the source picker's keys for SCOPE in this minibuffer."
  (let ((map (make-sparse-keymap))
        (accept (key-binding (kbd "RET"))))
    (define-key map (kbd "C-SPC") #'emacs-reader-local--picker-toggle)
    (define-key map (kbd "C-@") #'emacs-reader-local--picker-toggle)
    (define-key map (kbd "RET")
                `(menu-item "" ,(lambda () (interactive) (exit-minibuffer))
                            :filter ,(lambda (cmd) (if emacs-reader-local--marked cmd accept))))
    (dolist (binding (if scope '(("DEL" . nil) ("<backspace>" . nil))
                       '(("l" . "legado") ("o" . "opds") ("s" . "json"))))
      (let ((next (cdr binding)))
        (define-key map (kbd (car binding))
                    `(menu-item "" ,(lambda () (interactive) (throw 'reader-picker-scope next))
                                :filter ,(lambda (command)
                                           (when (= (minibuffer-prompt-end) (point-max))
                                             command))))))
    (use-local-map (make-composed-keymap map (current-local-map)))))

(defun emacs-reader-local--last-sources (saved sources)
  "Resolve SAVED scope against the currently enabled SOURCES."
  (let ((kind (alist-get 'kind saved)))
    (if (or (null saved) (equal kind "scope"))
        (let ((scope (alist-get 'scope saved)))
          (if scope (seq-filter (lambda (s) (equal scope (alist-get 'type s))) sources) sources))
      (delq nil
            (mapcar (lambda (ref)
                      (cl-find-if
                       (lambda (src)
                         (and (equal (alist-get 'type src) (alist-get 'type ref))
                              (or (equal (alist-get 'url src) (alist-get 'url ref))
                                  (and (equal (alist-get 'type src) "legado")
                                       (seq-contains-p (alist-get 'aliases src)
                                                       (alist-get 'url ref) #'equal)))))
                       sources))
                    (append (alist-get 'sources saved) nil))))))

(defun emacs-reader-local--picker-refs (sources)
  "Return a JSON array of stable references to SOURCES."
  (vconcat (mapcar (lambda (s) (list (assq 'type s) (assq 'url s))) sources)))

(defun emacs-reader-local--pick-scope (choices scope)
  "Read from CHOICES within SCOPE, decorating marked source names."
  (let* ((visible (if scope
                      (cl-remove-if-not (lambda (pair) (equal (alist-get 'type (cdr pair)) scope))
                                        choices)
                    choices))
         (file (expand-file-name "last-search-sources.json" emacs-reader-directory))
         (saved (and (not scope) (file-readable-p file) (emacs-reader-local--read file)))
         (all (mapcar #'cdr visible))
         (prior (unless scope (emacs-reader-local--last-sources saved all)))
         (last-label "【按照上次选的源】")
         (last-summary
          (unless scope
            (cond ((null saved) "  首次使用，搜索全部")
                  ((null prior) "  上次选的源已不可用")
                  (t (let* ((names (mapconcat (lambda (source) (alist-get 'name source))
                                              (seq-take prior 3) "、"))
                            (count (length prior)))
                       (concat "  " (truncate-string-to-width names 44 nil nil "…")
                               (when (> count 3) (format " 等 %d 个源" count))))))))
         (default (and visible (if scope "【全部】" last-label)))
         (table (when visible
                  (append (unless scope
                            (list (list last-label '(type . "all") (cons 'sources prior))))
                          (list (list "【全部】" '(type . "all") (cons 'sources all))) visible)))
         (completion-extra-properties
          (list :affixation-function
                (lambda (names)
                  (cl-loop for name in names for src = (cdr (assoc name table))
                           collect (list name (cond ((equal (alist-get 'type src) "all") "    ")
                                                    ((memq src emacs-reader-local--marked) "[✓] ")
                                                    (t "[ ] "))
                                         (if (equal name last-label) last-summary ""))))))
         (prompt (format "选择书源 [%s，C-SPC 勾选/取消，RET 确认]："
                         (if scope (format "%s，DEL 返回" scope) "l Legado，o OPDS，s JSON"))))
    (catch 'reader-picker-scope
      (let ((choice (minibuffer-with-setup-hook (lambda () (emacs-reader-local--picker-map scope))
                      (completing-read prompt table nil t nil nil default))))
        (let* ((selected (cdr (assoc choice table)))
               (marked (reverse emacs-reader-local--marked))
               (group (lambda (items) (list '(type . "all") (cons 'sources items)))))
          (cond (marked
                 (emacs-reader-local--write file `((kind . "multi")
                                                   (sources . ,(emacs-reader-local--picker-refs
                                                                marked))))
                 (funcall group marked))
                ((null selected) (user-error "当前范围没有可选书源"))
                ((equal choice last-label)
                 (unless prior (user-error "上次选的书源已删除或停用，请重新选择"))
                 (if (equal (alist-get 'kind saved) "single") (car prior) (funcall group prior)))
                ((equal choice "【全部】")
                 (emacs-reader-local--write file (if scope `((kind . "scope") (scope . ,scope))
                                                   '((kind . "scope"))))
                 selected)
                (t
                 (emacs-reader-local--write file `((kind . "single")
                                                   (sources . ,(emacs-reader-local--picker-refs
                                                                (list selected)))))
                 selected)))))))

(defun emacs-reader-local--select (types &optional allow-all)
  "Read an enabled source matching TYPES.
With ALLOW-ALL, default to the last confirmed search selection.
From empty input, l/o/s narrow to Legado/OPDS/JSON and DEL widens.
C-SPC toggles a source or scope.  RET confirms the marked sources."
  (let* ((sources (emacs-reader-local--records "sources"))
         (choices (cl-loop for src in sources
                           for definition = (alist-get 'definition src)
                           when (and (member (alist-get 'type src) types)
                                     (not (eq :false (alist-get 'enabled src)))
                                     (not (eq :false (alist-get 'enabled definition))))
                           collect (cons (format "%s [%s] <%s>" (alist-get 'name src)
                                                 (alist-get 'type src) (alist-get 'url src)) src))))
    (unless choices (user-error "没有可用的匹配书源；请在书架按 S 管理书源"))
    (if (not allow-all)
        (cdr (assoc (completing-read "选择书源：" choices nil t) choices))
      (let (emacs-reader-local--marked selection)
        (while (not (consp selection))
          (setq selection (emacs-reader-local--pick-scope choices selection)))
        selection))))

;;;###autoload
(defun emacs-reader-wendian-import-html (url file)
  "Read the saved Wendian HTML FILE as the work or chapter at URL."
  (interactive
   (let* ((row (and (derived-mode-p 'tabulated-list-mode) (tabulated-list-get-id)))
          (book (cond ((derived-mode-p 'emacs-reader-all-mode) (cdr-safe row))
                      ((derived-mode-p 'emacs-reader-search-mode) row)
                      ((boundp 'emacs-reader--current-book) emacs-reader--current-book)))
          (url (and (listp book) (alist-get 'bookUrl book))))
     (list (read-string "研辞问典原站页面 URL："
                        (and (stringp url) (string-prefix-p "https://wendian.dicomp.net/" url) url))
           (read-file-name "浏览器保存的 HTML：" nil nil t))))
  (message "已载入研辞问典页面，共 %d 章" (emacs-reader-wendian-import-page url file)))

;;;###autoload
(defun emacs-reader-browse-catalog ()
  "Browse a registered OPDS catalog."
  (interactive)
  (let ((src (emacs-reader-local--select '("opds"))))
    (emacs-reader-opds-browse (alist-get 'name src) (alist-get 'url src))))

(defun emacs-reader-local--search (keyword)
  "Select a local source and start searching for KEYWORD."
  (let ((src (emacs-reader-local--select '("legado" "opds" "json") t)))
    (pcase (alist-get 'type src)
      ("all" (emacs-reader-search-all keyword (alist-get 'sources src)))
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

(defun emacs-reader-local--fetch-file (book chapter callback)
  "Fetch BOOK's file CHAPTER or catalog, delivering its value to CALLBACK."
  (let* ((url (alist-get 'bookUrl book))
         (path (emacs-reader-local--resolve-file url)))
    (if (and (string-prefix-p "books/" url) (not (file-regular-p path)))
        (let ((buffer (current-buffer)))
          (message "正在后台查找并验证迁移的书籍：%s" (alist-get 'name book))
          (condition-case err
              (emacs-reader-local--recover-async
               (list url)
               (lambda (_result failure)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (if (and (not failure)
                              (file-regular-p (emacs-reader-local--resolve-file url)))
                         (emacs-reader-local--fetch-file-ready book chapter callback)
                       (funcall callback nil (or failure "未找到可确认的迁移书籍，或存在重名歧义")))))))
            (error (funcall callback nil (error-message-string err)))))
      (emacs-reader-local--fetch-file-ready book chapter callback))))

(defun emacs-reader-local--fetch-file-ready (book chapter callback)
  "Fetch BOOK CHAPTER from an already resolved file, using CALLBACK."
  (let* ((path (emacs-reader-local--resolve-file (alist-get 'bookUrl book)))
         (buffer (current-buffer))
         (inspect (and (not chapter) (or (alist-get 'readFileTitle book)
                                         (alist-get 'readFileAuthor book))))
         (done
          (lambda (result failure)
            (when (and result (not chapter))
              (when inspect
                (let ((metadata (car result)))
                  (setq result (cadr result))
                  (when (and (buffer-live-p buffer)
                             (eq book (buffer-local-value 'emacs-reader--current-book buffer)))
                    (with-current-buffer buffer
                      (when (alist-get 'readFileTitle book)
                        (setf (alist-get 'name book) (plist-get metadata :title))
                        (rename-buffer (format "*Reader: %s*" (alist-get 'name book)) t))
                      (when (alist-get 'readFileAuthor book)
                        (setf (alist-get 'author book) (or (plist-get metadata :author) "")))))))
              ;; A stored TXT preface is named after the digest on disk.
              (when (equal (plist-get (car result) :title) (file-name-base path))
                (setf (plist-get (car result) :title) (alist-get 'name book))))
            (funcall callback result failure))))
    (if emacs-reader-local--sync-preview
        (let (value failure)
          (condition-case err
              (setq value (if chapter (emacs-reader-files-content path chapter)
                            (emacs-reader-files-chapters path)))
            (error (setq failure (error-message-string err))))
          (funcall done value failure))
      (condition-case err
          (emacs-reader-file-worker-start
           (cond (chapter 'content) (inspect 'inspect) (t 'chapters))
           (if chapter (list path chapter) (list path)) done)
        (error (funcall callback nil (error-message-string err)))))))

(defun emacs-reader-local--fetch (book chapter callback)
  "Fetch BOOK's catalog or CHAPTER, calling CALLBACK with value and error."
  (if (equal (alist-get 'readerType book) "file")
      (emacs-reader-local--fetch-file book chapter callback)
    (let (source failure)
      (condition-case err (setq source (emacs-reader-local--source book))
        (error (setq failure (error-message-string err))))
      (cond (failure (funcall callback nil failure))
            (chapter (emacs-reader-legado-content source chapter callback))
            (t (emacs-reader-legado-chapters source (emacs-reader-local--plist book) callback))))))

(provide 'emacs-reader-local)
;;; emacs-reader-local.el ends here
