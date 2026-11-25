;;; emacs-reader-translate-fetch.el --- Fetch chapter text for translation -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Read chapters without entering the display, speech or progress paths.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'emacs-reader)
(declare-function emacs-reader--chapter-display-text "emacs-reader" (content html-p))
(require 'emacs-reader-translate-view)

(defun emacs-reader-translate-fetch-content-error (text)
  "Return a reason if TEXT contains only whitespace or a known service notice."
  (let ((trimmed (and (stringp text) (string-trim text "[[:space:]　 ]+" "[[:space:]　 ]+"))))
    (cond ((or (null trimmed) (string-empty-p trimmed)
               (equal trimmed "本页没有可显示的文字。"))
           "本章没有正文")
          ((member trimmed '("当前版本过低，已停止使用" "当前版本过低，已停止使用。" "NEED_LOGIN"))
           (concat "本章仅含服务提示：" trimmed)))))

(defun emacs-reader-translate-fetch--unsafe-url-p (url)
  "Return non-nil if URL opts into automatic purchase."
  (and (stringp url)
       (let ((case-fold-search t))
         (string-match-p "[?&]autoBuy=1\\(?:[&#,;[:space:]]\\|\\'\\)"
                         (url-unhex-string url)))))

(defun emacs-reader-translate-fetch--chapter-error (chapter)
  "Return a reason to skip CHAPTER based on explicit chapter evidence."
  (let ((native (alist-get 'native chapter)))
    (cond ((cl-some #'emacs-reader-translate-fetch--unsafe-url-p
                    (list (plist-get native :url) (alist-get 'url chapter)
                          (alist-get 'chapterUrl chapter)))
           "章节链接启用了自动购买，已跳过")
          ((or (eq (plist-get native :access) 'paid) (plist-get native :easouLocked)
               (memq (alist-get 'isLocked chapter) '(t 1))
               (memq (alist-get 'isVip chapter) '(t 1))
               (string-prefix-p "🔒" (or (alist-get 'title chapter) "")))
           "章节已标记为锁章或收费，已跳过"))))

(defun emacs-reader-translate-fetch--reader3 (endpoint data callback)
  "Post DATA to ENDPOINT with the active session and deliver to CALLBACK.
Background work must never open the interactive login prompt."
  (if emacs-reader--session-cookie
      (emacs-reader--reader3-request endpoint "POST" data callback t)
    (funcall callback '(:error authentication))))

(defun emacs-reader-translate-fetch--asset (path callback)
  "Read an EPUB resource at PATH and deliver its transport result to CALLBACK."
  (emacs-reader--url-transport
   (list :url (emacs-reader--book-asset-url path) :method "GET" :html t
         :headers `(("Accept" . "text/html, application/xhtml+xml")
                    ,@(when emacs-reader--session-cookie
                        `(("Cookie" . ,(concat "reader.session=" emacs-reader--session-cookie))))))
   callback))

(defun emacs-reader-translate-fetch (reader-buffer chapter-index callback)
  "Fetch CHAPTER-INDEX from READER-BUFFER, calling CALLBACK with (PARAGRAPHS ERR).
ERR is nil on success, otherwise a message.  The returned no-argument
function discards late responses and skips subsequent catalog/content handoffs."
  (let ((width (buffer-local-value 'emacs-reader-translation-line-width reader-buffer)) finished)
    (cl-labels
        ((finish (paragraphs err)
           (unless finished
             (setq finished t)
             (funcall callback paragraphs err)))
         (render (content kind err)
           (unless finished
             (let* ((text (unless err (emacs-reader--chapter-display-text content kind)))
                    (reason (or err (emacs-reader-translate-fetch-content-error text)))
                    (paragraphs (unless reason (emacs-reader-translate-view-split text width))))
               (finish paragraphs (or reason (unless paragraphs "本章没有正文"))))))
         (remote-response (response)
           (unless finished
             (condition-case err
                 (let* ((json (plist-get response :ok))
                        (ok (and (listp json) (eq (alist-get 'isSuccess json) t)))
                        (content (and ok (alist-get 'data json)))
                        (path (and (stringp content) (string-trim content))))
                   (cond ((not ok) (finish nil "Reader3 读取正文失败"))
                         ((emacs-reader--book-asset-path-p path)
                          (emacs-reader-translate-fetch--asset
                           path (lambda (asset)
                                  (render (plist-get asset :ok) t
                                          (and (plist-get asset :error) "EPUB 资源读取失败")))))
                         (t (render content nil nil))))
               (error (finish nil (error-message-string err))))))
         (request (book chapters)
           (let* ((chapter (and (< chapter-index (length chapters)) (elt chapters chapter-index)))
                  (type (alist-get 'readerType book))
                  (url (alist-get 'bookUrl book))
                  (native (alist-get 'native chapter))
                  (reason (emacs-reader-translate-fetch--chapter-error chapter)))
             (cond ((not chapter) (finish nil "章节不在目录中"))
                   (reason (finish nil reason))
                   ((emacs-reader-translate-fetch--unsafe-url-p url)
                    (finish nil "书籍链接启用了自动购买，已跳过"))
                   ((not type)
                    ;; Reader3 otherwise saves the requested index to the shelf and WebDAV.
                    (emacs-reader-translate-fetch--reader3
                     "getBookContent" `((url . ,url) (index . ,chapter-index) (cache . 1))
                     #'remote-response))
                   ((not native) (finish nil "缺少章节信息"))
                   (t (emacs-reader-local--fetch
                       book native
                       (lambda (text err)
                         (render text (and (equal type "file")
                                           (equal (file-name-extension url) "epub") 'paragraphs)
                                 err))))))))
      (condition-case err
          (with-current-buffer reader-buffer
            (let ((book emacs-reader--current-book))
              (cond ((or (not (integerp chapter-index)) (< chapter-index 0))
                     (finish nil "章节序号无效"))
                    ((not (stringp (alist-get 'bookUrl book))) (finish nil "未打开书籍"))
                    ((or emacs-reader--chapters (alist-get 'readerType book))
                     (request book emacs-reader--chapters))
                    (t
                     ;; A Reader3 book may be opened by saved index before loading its catalog.
                     (emacs-reader-translate-fetch--reader3
                      "getChapterList"
                      `((url . ,(alist-get 'bookUrl book)) (refresh . 0)
                        ,@(when emacs-reader--preview-p
                            `((bookSourceUrl . ,(alist-get 'origin book)))))
                      (lambda (result)
                        (unless finished
                          (condition-case failure
                              (let ((json (plist-get result :ok)))
                                (if (and (listp json) (eq (alist-get 'isSuccess json) t))
                                    (request book (alist-get 'data json))
                                  (finish nil "Reader3 读取目录失败")))
                            (error (finish nil (error-message-string failure)))))))))))
        (error (finish nil (error-message-string err))))
      (lambda () (setq finished t)))))

(provide 'emacs-reader-translate-fetch)
;;; emacs-reader-translate-fetch.el ends here
