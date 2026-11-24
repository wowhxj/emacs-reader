;;; emacs-reader-wendian-read.el --- Read Wendian book pages -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Ancient and medical works keep all chapters on one detail page.  Classics
;; and primers link their chapters from a separate book.php table of contents.

;;; Code:

(require 'emacs-reader-legado)
(require 'emacs-reader-local)
(require 'dom)
(require 'cl-lib)
(require 'subr-x)
(require 'url-util)

(defvar emacs-reader-wendian--pages nil
  "Recent (URL . PAGE) pairs containing extracted text, without the DOM.")

(defun emacs-reader-wendian--remember (key page)
  "Cache parsed PAGE under KEY and keep at most four recent pages."
  (setq emacs-reader-wendian--pages (cons (cons key page)
                                          (assoc-delete-all key emacs-reader-wendian--pages)))
  (when-let* ((tail (nthcdr 3 emacs-reader-wendian--pages))) (setcdr tail nil))
  page)

(defun emacs-reader-wendian--url (target)
  "Return the absolute original site URL for TARGET, without its fragment."
  (let ((url (url-encode-url (emacs-reader-legado--url target "https://wendian.dicomp.net/"))))
    (setq url (car (split-string url "#")))
    (unless (string-match-p
             (rx string-start "https://wendian.dicomp.net/"
                 (or "ancient" "tcm" "classics" "mengxue") "/" (or "detail" "book") ".php?") url)
      (error "无法识别研辞问典书页地址 %s" url))
    url))

(defun emacs-reader-wendian--first (node tags)
  "Find the first element in NODE whose tag belongs to TAGS."
  (when (consp node)
    (if (memq (dom-tag node) tags) node
      (cl-some (lambda (child) (emacs-reader-wendian--first child tags)) (dom-children node)))))

(defun emacs-reader-wendian--text (node &optional skip)
  "Extract inline text from NODE, excluding SKIP tags and preserving blocks."
  (with-temp-buffer
    (cl-labels ((emit (item)
                  (cond ((stringp item) (insert item))
                        ((or (not (consp item)) (memq (dom-tag item) skip)
                             (memq (dom-tag item) '(comment script style nav button aside))) nil)
                        ((eq (dom-tag item) 'br) (insert "\n"))
                        (t (when (eq (dom-tag item) 'p) (insert "\n"))
                           (mapc #'emit (dom-children item))
                           (when (eq (dom-tag item) 'p) (insert "\n"))))))
      (emit node))
    (string-trim (buffer-string))))

(defun emacs-reader-wendian--detail (article title url)
  "Extract chapters and their corresponding text from ARTICLE with TITLE at URL."
  (let ((sections (cl-remove-if-not
                   (lambda (el) (string-match-p (rx string-start "ch-" (+ digit) string-end)
                                                (or (dom-attr el 'id) "")))
                   (dom-by-tag article 'section)))
        chapters contents)
    (dolist (node (or sections (list article)))
      (let* ((heading (emacs-reader-wendian--first node '(h2 h3 h4)))
             (name (and heading (emacs-reader-wendian--text heading)))
             (body (emacs-reader-wendian--text node '(h2 h3 h4)))
             (lines (split-string body "\n" t "[[:space:]　]+"))
             (key (if sections (dom-attr node 'id) "")))
        (when lines
          (setcar lines (replace-regexp-in-string (rx string-start "属性" (any ":：") (* space))
                                                  "" (car lines)))
          (let ((text (string-join (delete "" lines) "\n")))
            (unless (string-empty-p text)
              (push text contents)
              (push (list :title (if (string-empty-p (or name "")) title name)
                          :url (if (string-empty-p key) url (concat url "#" key))) chapters))))))
    (unless chapters (error "研辞问典页面缺少正文 %s" url))
    (list :title title :chapters (nreverse chapters) :contents (nreverse contents))))

(defun emacs-reader-wendian--catalog (main title url)
  "Extract TITLE's linked chapters in MAIN at URL."
  (let* ((toc (or (car (dom-by-id main "\\`toc\\'")) main))
         (directory (substring url 0 (string-search "/book.php?" url)))
         (seen (make-hash-table :test #'equal)) chapters)
    (dolist (link (dom-by-tag toc 'a))
      (when-let* ((href (dom-attr link 'href))
                  (target (url-expand-file-name href url))
                  ((string-match-p (concat "\\`" (regexp-quote directory)
                                           (rx "/detail.php?id=" (+ digit) string-end))
                                   target)))
        (let* ((label (car (dom-by-class link "\\btruncate\\b")))
               (name (if label
                         (string-trim (mapconcat #'identity (cl-remove-if-not
                                                             #'stringp (dom-children label)) ""))
                       (emacs-reader-wendian--text link))))
          (unless (or (gethash target seen) (string-empty-p name) (string-prefix-p "开始阅读" name))
            (puthash target t seen)
            (push (list :title name :url target) chapters)))))
    (unless chapters (error "研辞问典页面缺少章节目录 %s" url))
    (list :title title :chapters (nreverse chapters))))

(defun emacs-reader-wendian--parse (doc url)
  "Extract Wendian page data from the Legado document DOC at URL."
  (let* ((main (emacs-reader-wendian--first (cdr doc) '(main)))
         (h1 (emacs-reader-wendian--first main '(h1)))
         (title (string-remove-suffix "》" (string-remove-prefix
                                            "《" (emacs-reader-wendian--text h1 '(span))))))
    (unless main
      (error "研辞问典未返回书页（可能需要浏览器验证） %s，另存原站 HTML 后可用 M-x emacs-reader-wendian-import-html" url))
    (if (string-search "/book.php?" url)
        (emacs-reader-wendian--catalog main title url)
      (emacs-reader-wendian--detail (emacs-reader-wendian--first main '(article)) title url))))

(defun emacs-reader-wendian-import-page (url file)
  "Parse the original site's saved HTML FILE for URL and return its chapter count."
  (let* ((target (emacs-reader-wendian--url url))
         (doc (with-temp-buffer
                (insert-file-contents file)
                (cons 'html (libxml-parse-html-region (point-min) (point-max)))))
         (page (emacs-reader-wendian--parse doc target)))
    (when-let* ((head (emacs-reader-wendian--first (cdr doc) '(head)))
                (canonical (cl-find-if (lambda (node) (equal (dom-attr node 'rel) "canonical"))
                                       (dom-by-tag head 'link))))
      ;; The site omits the edition query in the canonical URL of a primer.
      (let ((expected (emacs-reader-wendian--url (dom-attr canonical 'href))))
        (unless (or (equal target expected)
                    (and (string-match-p "/mengxue/book\\.php" target)
                         (equal (car (split-string target "&var=")) expected)))
          (error "HTML 书页地址与指定地址不一致 %s" expected))))
    (let ((key (emacs-reader-local--record "wendian-pages" target)))
      (emacs-reader-local--write
       key (list :title (plist-get page :title) :chapters (vconcat (plist-get page :chapters))
                 :contents (vconcat (plist-get page :contents))))
      (emacs-reader-wendian--remember key page))
    (length (plist-get page :chapters))))

(defun emacs-reader-wendian--restore (file)
  "Read a previously imported page from FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
    (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil)))

(defun emacs-reader-wendian--with-page (source target callback read-page)
  "Apply READ-PAGE to TARGET's parsed page in SOURCE, delivering via CALLBACK."
  (let* ((url (condition-case err (emacs-reader-wendian--url target)
                (error (funcall callback nil (error-message-string err)) nil)))
         (key (and url (emacs-reader-local--record "wendian-pages" url)))
         (cached (cdr (assoc key emacs-reader-wendian--pages))))
    (cl-labels ((deliver (page message)
                  (let (value)
                    (unless message
                      (condition-case err (setq value (funcall read-page page url))
                        (error (setq message (error-message-string err)))))
                    (funcall callback value message))))
      (when url
        (cond (cached (deliver cached nil))
              ((file-exists-p key)
               (let (page message)
                 (condition-case err
                     (setq page (emacs-reader-wendian--remember
                                 key (emacs-reader-wendian--restore key)))
                   (error (setq message (error-message-string err))))
                 (deliver page message)))
              (t (emacs-reader-legado--map-page
                  source url nil #'deliver
                  (lambda (doc _final _requested)
                    (emacs-reader-wendian--remember
                     key (emacs-reader-wendian--parse doc url))))))))))

(defun emacs-reader-wendian-book-info (source book callback)
  "Read details of BOOK from SOURCE and call CALLBACK with book and error."
  (emacs-reader-wendian--with-page
   source (plist-get book :bookUrl) callback
   (lambda (page url)
     (let ((info (copy-sequence book)))
       (when (string-empty-p (or (plist-get info :name) ""))
         (setq info (plist-put info :name (plist-get page :title))))
       (plist-put info :tocUrl url)))))

(defun emacs-reader-wendian-chapters (source book callback)
  "Read the catalog of BOOK from SOURCE and call CALLBACK with chapters and error."
  (emacs-reader-wendian--with-page
   source (or (plist-get book :tocUrl) (plist-get book :bookUrl)) callback
   (lambda (page _url) (plist-get page :chapters))))

(defun emacs-reader-wendian-content (source chapter callback)
  "Read CHAPTER from SOURCE and call CALLBACK with plain text and error."
  (let ((target (plist-get chapter :url)))
    (emacs-reader-wendian--with-page
     source target callback
     (lambda (page url)
       (let ((fragment (cadr (split-string target "#"))) (texts (plist-get page :contents)))
         (or (if fragment
                 (cl-loop for entry in (plist-get page :chapters) for text in texts
                          when (equal (plist-get entry :url) (concat url "#" fragment)) return text)
               (and texts (string-join texts "\n")))
             (error "研辞问典页面没有此章 %s" target)))))))

(provide 'emacs-reader-wendian-read)
;;; emacs-reader-wendian-read.el ends here
