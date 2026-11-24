;;; emacs-reader-sources.el --- Manage local reader sources -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Manage registered catalogs and Legado definitions from a tabulated list.

;;; Code:

(require 'emacs-reader-filter)
(require 'emacs-reader-local)
(require 'emacs-reader-opds)
(require 'tabulated-list)
(require 'js)
(require 'transient)

(autoload 'emacs-reader--display-buffer "emacs-reader")

(defvar-local emacs-reader-sources--original nil)
(defconst emacs-reader-sources--buffer "*Reader Sources*")
(defconst emacs-reader-sources--examples-directory
  (expand-file-name "examples" (file-name-directory (locate-library "emacs-reader-sources"))))

(defun emacs-reader-sources--path (record)
  "Return the registry path for RECORD."
  (emacs-reader-local--source-path (alist-get 'type record) (alist-get 'url record)))

(defun emacs-reader-sources--check-current (record)
  "Report a stale RECORD before editing or deleting it."
  (let ((path (emacs-reader-sources--path record)))
    (unless (and (file-readable-p path) (equal record (emacs-reader-local--read path)))
      (user-error "书源已变更，请按 g 刷新后重新操作"))))

(defun emacs-reader-sources--validate (record)
  "Check the fields required in a registry RECORD."
  (unless (consp record) (user-error "书源配置须为 JSON 对象"))
  (let ((type (alist-get 'type record)) (name (alist-get 'name record))
        (aliases (alist-get 'aliases record)))
    (unless (and (stringp name) (not (string-empty-p (string-trim name))))
      (user-error "请填写书源名称"))
    (emacs-reader-local--source-url (alist-get 'url record))
    (unless (member type '("opds" "json" "legado")) (user-error "未知书源类型 %s" type))
    (when (and (member type '("json" "legado")) (not (consp (alist-get 'definition record))))
      (user-error "书源缺少 definition 规则对象"))
    (when (equal type "json")
      (unless (string-match-p (regexp-quote "{query}") (alist-get 'url record))
        (user-error "JSON 目录 URL 需包含 {query}"))
      (let ((rule (alist-get 'definition record)))
        (dolist (field '(rows label))
          (let ((value (alist-get field rule)))
            (unless (and (stringp value) (not (string-blank-p value)))
              (user-error "JSON 规则缺少 %s" field))))
        (unless (cl-some (lambda (key)
                           (let ((url (alist-get key rule)))
                             (and (stringp url) (not (string-blank-p url)))))
                         '(href recordHref))
          (user-error "JSON 规则需要 href 或 recordHref"))))
    (when aliases
      (unless (vectorp aliases) (user-error "别名 aliases 须为 URL 数组"))
      (mapc #'emacs-reader-local--source-url aliases))))

(defun emacs-reader-sources--save (record &optional original)
  "Save RECORD, replacing ORIGINAL when editing an existing source."
  (emacs-reader-sources--validate record)
  (let* ((type (alist-get 'type record)) (url (alist-get 'url record))
         (path (emacs-reader-sources--path record))
         (old-path (and original (emacs-reader-sources--path original)))
         (urls (cons url (append (alist-get 'aliases record) nil))))
    (when original
      (emacs-reader-sources--check-current original)
      (unless (equal type (alist-get 'type original)) (user-error "编辑时请保留书源类型"))
      (when (equal type "legado")
        (setq urls (delete-dups (append urls (list (alist-get 'url original))
                                        (append (alist-get 'aliases original) nil))))
        (when (cdr urls) (setf (alist-get 'aliases record) (vconcat (cdr urls))))))
    (when (and (not (equal old-path path)) (file-exists-p path))
      (user-error "该地址已被另一个书源使用"))
    (when (equal type "legado")
      (dolist (other (emacs-reader-local--records "sources"))
        (when (and (equal type (alist-get 'type other)) (not (equal other original))
                   (cl-intersection urls (cons (alist-get 'url other)
                                               (append (alist-get 'aliases other) nil))
                                    :test #'equal))
          (user-error "该地址已被另一个 Legado 书源或其别名使用"))))
    (emacs-reader-local--write path record)
    (when (and old-path (not (equal path old-path)))
      (condition-case err
          (delete-file old-path)
        (file-error
         (delete-file path)
         (signal (car err) (cdr err)))))
    (when original (emacs-reader-sources--forget original))))

(defcustom emacs-reader-sources-check-keyword "三体"
  "Default keyword for source checks.  A prefix argument prompts for another."
  :type 'string :group 'emacs-reader)

(defcustom emacs-reader-sources-check-concurrency 3
  "Maximum number of sources checked concurrently."
  :type 'natnum :group 'emacs-reader)

(defcustom emacs-reader-sources-check-timeout 30
  "Maximum seconds to spend checking one source."
  :type 'natnum :group 'emacs-reader)

(cl-defstruct (emacs-reader-sources--check (:constructor emacs-reader-sources--make-check))
  source keyword timer handle)

(defvar-local emacs-reader-sources--records nil
  "Registry snapshot used when filtering or updating check results.")
(defvar-local emacs-reader-sources--marked nil)
(defvar-local emacs-reader-sources--flagged nil)
(defvar-local emacs-reader-sources--states nil)
(defvar-local emacs-reader-sources--queue nil)
(defvar-local emacs-reader-sources--running nil)
(defvar-local emacs-reader-sources--pumping nil)
(defvar-local emacs-reader-sources--redraw-timer nil)

(defun emacs-reader-sources--forget (source)
  "Invalidate the last result for a deleted or edited SOURCE."
  (when-let* ((buffer (get-buffer emacs-reader-sources--buffer)))
    (with-current-buffer buffer
      (setq emacs-reader-sources--marked (delete source emacs-reader-sources--marked)
            emacs-reader-sources--flagged (delete source emacs-reader-sources--flagged))
      (when emacs-reader-sources--states (remhash source emacs-reader-sources--states)))))

(defun emacs-reader-sources--redraw ()
  "Coalesce async updates without displaying the source list."
  (unless emacs-reader-sources--redraw-timer
    (let ((buffer (current-buffer)))
      (setq emacs-reader-sources--redraw-timer
            (run-at-time 0.1 nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (setq emacs-reader-sources--redraw-timer nil)
                               (emacs-reader-sources--render)))))))))

(defun emacs-reader-sources--rule-reason (source reason)
  "Describe the failing search stage in SOURCE when REASON identifies it."
  (let* ((definition (alist-get 'definition source))
         (url (alist-get 'searchUrl definition))
         (header (alist-get 'header definition))
         (list-rule (alist-get 'bookList (alist-get 'ruleSearch definition))))
    (cond ((and (string-search "动态表达式" reason) (stringp url)
                (string-match-p "java\\.md5Encode" url) (string-search "token" url))
           "搜索 URL 依赖动态签名 token，当前不支持此请求模板")
          ((and (string-search "请求头格式" reason) (stringp header)
                (string-match-p "java\\.md5Encode" header) (string-search "sign" header))
           "搜索请求头由脚本动态签名，当前不支持此请求头")
          ((and (string-search "Java/Android" reason) (stringp list-rule)
                (string-search "JavaImporter" list-rule) (string-search "javax.crypto" list-rule))
           "搜索列表依赖 Java/Android 脚本解密响应，当前不支持")
          (t reason))))

(defun emacs-reader-sources--result (source keyword items reason)
  "Summarize the first page of ITEMS or REASON for SOURCE and KEYWORD."
  (let ((type (alist-get 'type source)))
    (if reason
        (cons (cond ((and (equal type "opds") (equal reason "目录未声明可用的搜索入口")) "仅可浏览")
                    ((string-match-p "超时\\|timed? ?out\\|timeout" reason) "超时")
                    ((string-match-p "HTTP [0-9]\\{3\\}" reason) "HTTP 错误")
                    ((string-match-p "不支持\\|Unsupported\\|缺少 searchUrl" reason) "规则不支持")
                    (t "请求失败"))
              (replace-regexp-in-string "[\n\r\t]+" " "
                                        (emacs-reader-sources--rule-reason source reason)))
      (let ((count (if (equal type "legado") (length items)
                     (cl-count-if (lambda (item)
                                    (and (not (emacs-reader-opds--item-pagination item))
                                         (memq (emacs-reader-opds--item-kind item)
                                               '(record download json-page feed)))) items))))
        (cons (cond ((zerop count) "成功 · 0 条")
                    ((and (equal type "opds")
                          (not (cl-some (lambda (item)
                                          (memq (emacs-reader-opds--item-kind item)
                                                '(record download))) items))) "有目录项")
                    (t "搜到书"))
              (concat (format "「%s」首页 %d 条结果" keyword count)
                      (when (and (equal type "legado") (> count 0))
                        (format "，书名或作者含关键词 %d 条"
                                (cl-count-if
                                 (lambda (book)
                                   (cl-some (lambda (field)
                                              (let ((value (plist-get book field)))
                                                (and (stringp value)
                                                     (string-search keyword value))))
                                            '(:name :author))) items)))))))))

(defun emacs-reader-sources--abort (check)
  "Release the outstanding request for CHECK."
  (let ((handle (emacs-reader-sources--check-handle check)))
    (cond ((functionp handle) (funcall handle))
          ((and (bufferp handle) (buffer-live-p handle))
           (when-let* ((process (get-buffer-process handle))) (delete-process process))
           (when (buffer-live-p handle) (kill-buffer handle))))))

(defun emacs-reader-sources--finish (buffer check items reason &optional timeout)
  "Deliver ITEMS or REASON for the active CHECK in BUFFER, aborting on TIMEOUT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (memq check emacs-reader-sources--running)
        (setq emacs-reader-sources--running (delq check emacs-reader-sources--running))
        (cancel-timer (emacs-reader-sources--check-timer check))
        (when timeout (emacs-reader-sources--abort check))
        (let ((source (emacs-reader-sources--check-source check)))
          (when (gethash source emacs-reader-sources--states)
            (puthash source (emacs-reader-sources--result
                             source (emacs-reader-sources--check-keyword check) items reason)
                     emacs-reader-sources--states)))
        (emacs-reader-sources--redraw)
        (emacs-reader-sources--pump)))))

(defun emacs-reader-sources--start-check (check)
  "Launch the first-page search for CHECK."
  (let* ((buffer (current-buffer)) (source (emacs-reader-sources--check-source check))
         (keyword (emacs-reader-sources--check-keyword check))
         (done (apply-partially #'emacs-reader-sources--finish buffer check)))
    (push check emacs-reader-sources--running)
    (puthash source (cons "检测中" keyword) emacs-reader-sources--states)
    (setf (emacs-reader-sources--check-timer check)
          (run-at-time (max 1 emacs-reader-sources-check-timeout) nil done nil "检测请求超时" t))
    (condition-case err
        (setf (emacs-reader-sources--check-handle check)
              (if (equal (alist-get 'type source) "legado")
                  (emacs-reader-legado-search (emacs-reader-local--plist
                                               (alist-get 'definition source)) keyword done)
                (emacs-reader-catalog-results source keyword done)))
      (error (funcall done nil (error-message-string err))))))

(defun emacs-reader-sources--pump ()
  "Start queued checks up to the concurrency limit."
  (unless emacs-reader-sources--pumping
    (let ((emacs-reader-sources--pumping t))
      (while (and emacs-reader-sources--queue
                  (< (length emacs-reader-sources--running)
                     (max 1 emacs-reader-sources-check-concurrency)))
        (let ((check (pop emacs-reader-sources--queue)))
          (when (gethash (emacs-reader-sources--check-source check) emacs-reader-sources--states)
            (emacs-reader-sources--start-check check)))))))

(defun emacs-reader-sources--stop ()
  "Cancel outstanding work and invalidate callbacks before releasing requests."
  (let ((running emacs-reader-sources--running) (queued emacs-reader-sources--queue))
    (setq emacs-reader-sources--running nil emacs-reader-sources--queue nil)
    (when emacs-reader-sources--redraw-timer
      (cancel-timer emacs-reader-sources--redraw-timer)
      (setq emacs-reader-sources--redraw-timer nil))
    (dolist (check (append running queued))
      (let ((source (emacs-reader-sources--check-source check)))
        (when (gethash source emacs-reader-sources--states)
          (puthash source '("已取消" . "") emacs-reader-sources--states))))
    (dolist (check running)
      (cancel-timer (emacs-reader-sources--check-timer check))
      (emacs-reader-sources--abort check))))

(defun emacs-reader-sources-check-cancel ()
  "Cancel all running and queued checks in this list."
  (interactive)
  (emacs-reader-sources--stop)
  (emacs-reader-sources--render))

(defun emacs-reader-sources--begin (sources keyword)
  "Replace pending work with first-page checks of SOURCES using KEYWORD."
  (when (string-blank-p keyword) (user-error "检测关键词不能为空"))
  (emacs-reader-sources--stop)
  (setq emacs-reader-sources--queue
        (mapcar (lambda (source)
                  (puthash source (cons "排队" keyword) emacs-reader-sources--states)
                  (emacs-reader-sources--make-check :source source :keyword keyword)) sources))
  (emacs-reader-sources--pump)
  (emacs-reader-sources--render))

(defun emacs-reader-sources--keyword ()
  "Read a keyword with a prefix argument, or use the configured default."
  (if current-prefix-arg (read-string "检测关键词：" nil nil emacs-reader-sources-check-keyword)
    emacs-reader-sources-check-keyword))

(defun emacs-reader-sources-check (keyword)
  "Check marked sources or the source at point using KEYWORD.  With a prefix, prompt for it."
  (interactive (list (emacs-reader-sources--keyword)))
  (let ((sources (or (emacs-reader-sources--visible emacs-reader-sources--marked)
                     (list (emacs-reader-sources--selected)))))
    (mapc #'emacs-reader-sources--check-current sources)
    (emacs-reader-sources--begin sources keyword)))

(defun emacs-reader-sources-check-all (keyword)
  "Check every registered source using KEYWORD.  With a prefix, prompt for it."
  (interactive (list (emacs-reader-sources--keyword)))
  (setq emacs-reader-sources--records (emacs-reader-local--records "sources"))
  (emacs-reader-sources--begin emacs-reader-sources--records keyword))

(defun emacs-reader-sources--entries ()
  "Build tabulated list entries from registered records."
  (emacs-reader-filter-rows
   (mapcar (lambda (src)
             (let ((state (gethash src emacs-reader-sources--states)))
               (list src (vector (alist-get 'name src) (alist-get 'type src)
                                 (or (car state) "未检测") (or (cdr state) "")
                                 (alist-get 'url src)))))
           emacs-reader-sources--records)))

(defun emacs-reader-sources--reload ()
  "Read the registry and prepare the filtered rows for printing."
  (setq emacs-reader-sources--records (emacs-reader-local--records "sources")
        tabulated-list-entries (emacs-reader-sources--entries)))

(defun emacs-reader-sources--render ()
  "Render the cached sources with current check results and filter."
  (setq tabulated-list-entries (emacs-reader-sources--entries))
  (tabulated-list-print t))

(defun emacs-reader-sources--visible (sources)
  "Return members of SOURCES in the displayed rows, in display order."
  (cl-loop for (src) in tabulated-list-entries when (member src sources) collect src))

(defun emacs-reader-sources--refresh ()
  "Refresh the sources list if it is open."
  (when-let* ((buffer (get-buffer emacs-reader-sources--buffer)))
    (with-current-buffer buffer (revert-buffer))))

(defun emacs-reader-sources--selected ()
  "Return the registry entry on the current row."
  (or (tabulated-list-get-id) (user-error "请先选择书源")))

(defun emacs-reader-sources-add-opds (name url)
  "Add an OPDS catalog with NAME and URL."
  (interactive "s目录名称：\nsOPDS 地址：")
  (emacs-reader-sources--save `((type . "opds") (name . ,name) (url . ,url)))
  (emacs-reader-sources--refresh))

(defun emacs-reader-sources-import-legado-file (file)
  "Import Legado definitions from FILE."
  (interactive "fLegado 书源 JSON：")
  (emacs-reader-import-legado file)
  (emacs-reader-sources--refresh))

(defun emacs-reader-sources-import-legado-url (url)
  "Import Legado definitions from URL."
  (interactive "sLegado 书源 JSON URL：")
  (emacs-reader-import-legado (emacs-reader-local--source-url url))
  (emacs-reader-sources--refresh))

(defun emacs-reader-sources-import-json (file)
  "Register the JSON catalog configuration in FILE."
  (interactive "fJSON 目录配置：")
  (let ((record (emacs-reader-local--read file)))
    (unless (and (consp record) (equal (alist-get 'type record) "json"))
      (user-error "请选择 type 为 json 的完整目录配置"))
    (emacs-reader-sources--save record))
  (emacs-reader-sources--refresh))

(defun emacs-reader-sources-add-preset ()
  "Select a bundled source configuration and register an editable copy."
  (interactive)
  (let* ((files (directory-files emacs-reader-sources--examples-directory t "\\.json\\'"))
         (choices (mapcar (lambda (file)
                            (let ((src (emacs-reader-local--read file)))
                              (cons (alist-get 'name src) src)))
                          files))
         (choice (completing-read "添加预置书源：" choices nil t)))
    (emacs-reader-sources--save (cdr (assoc choice choices)))
    (emacs-reader-sources--refresh)))

(transient-define-prefix emacs-reader-sources-add ()
  "Choose how to register a source."
  [["添加书源"
    ("o" "新 OPDS 目录" emacs-reader-sources-add-opds)
    ("f" "Legado 本地文件" emacs-reader-sources-import-legado-file)
    ("u" "Legado URL" emacs-reader-sources-import-legado-url)
    ("p" "预置书源" emacs-reader-sources-add-preset)
    ("j" "JSON 目录配置" emacs-reader-sources-import-json)]])

(define-derived-mode emacs-reader-sources-edit-mode js-json-mode "Reader-Source-JSON"
  "Edit a source configuration as JSON."
  (setq-local header-line-format "书源 JSON    C-c C-c 保存    C-c C-k 取消"))

(defun emacs-reader-sources-edit ()
  "Edit the selected catalog or complete Legado definition."
  (interactive)
  (let* ((src (emacs-reader-sources--selected))
         (payload (if (equal (alist-get 'type src) "legado") (alist-get 'definition src) src))
         (json (json-serialize payload :false-object :false :null-object :null))
         (buffer (generate-new-buffer (format "*Reader Source: %s*" (alist-get 'name src)))))
    (with-current-buffer buffer
      (emacs-reader-sources-edit-mode)
      (setq emacs-reader-sources--original src)
      (insert (if (multibyte-string-p json) json (decode-coding-string json 'utf-8)))
      (json-pretty-print-buffer)
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (emacs-reader--display-buffer buffer)))

(defun emacs-reader-sources-edit-cancel ()
  "Close the editor and return to the source list."
  (interactive)
  (kill-buffer (current-buffer))
  (emacs-reader-sources))

(defun emacs-reader-sources-edit-save ()
  "Validate and confirm the edited JSON, then save it in the registry."
  (interactive)
  (let* ((original emacs-reader-sources--original)
         (data (save-excursion
                 (goto-char (point-min))
                 (prog1 (json-parse-buffer :object-type 'alist :false-object :false
                                           :null-object :null)
                   (skip-chars-forward " \r\t\n")
                   (unless (eobp) (user-error "JSON 对象后有多余内容")))))
         (src data))
    (when (equal (alist-get 'type original) "legado")
      (unless (consp data) (user-error "Legado 书源须为 JSON 对象"))
      (setq src (copy-tree original))
      (setf (alist-get 'url src) (alist-get 'bookSourceUrl data)
            (alist-get 'name src) (alist-get 'bookSourceName data)
            (alist-get 'definition src) data))
    (emacs-reader-sources--validate src)
    (when (yes-or-no-p (format "保存书源「%s」的修改? " (alist-get 'name original)))
      (emacs-reader-sources--save src original)
      (emacs-reader-sources-edit-cancel))))

(define-key emacs-reader-sources-edit-mode-map (kbd "C-c C-c") #'emacs-reader-sources-edit-save)
(define-key emacs-reader-sources-edit-mode-map (kbd "C-c C-k") #'emacs-reader-sources-edit-cancel)

(defun emacs-reader-sources--print-entry (id cols)
  "Print ID and COLS with the selection or deletion flag in the margin."
  (let ((start (point)))
    (tabulated-list-print-entry id cols)
    (save-excursion
      (goto-char start)
      (cond ((member id emacs-reader-sources--flagged) (tabulated-list-put-tag "D"))
            ((member id emacs-reader-sources--marked) (tabulated-list-put-tag "*"))))))

(defun emacs-reader-sources-mark ()
  "Toggle selection of this source and advance to the next row."
  (interactive)
  (let ((src (emacs-reader-sources--selected)))
    (setq emacs-reader-sources--flagged (delete src emacs-reader-sources--flagged))
    (if (member src emacs-reader-sources--marked)
        (progn (setq emacs-reader-sources--marked (delete src emacs-reader-sources--marked))
               (tabulated-list-put-tag "" t))
      (push src emacs-reader-sources--marked)
      (tabulated-list-put-tag "*" t))))

(defun emacs-reader-sources-mark-visible ()
  "Select every visible source, replacing any deletion flag on those rows."
  (interactive)
  (dolist (entry tabulated-list-entries)
    (let ((src (car entry)))
      (setq emacs-reader-sources--flagged (delete src emacs-reader-sources--flagged))
      (cl-pushnew src emacs-reader-sources--marked :test #'equal)))
  (tabulated-list-print t))

(defun emacs-reader-sources-unmark ()
  "Clear either mark on the current row and advance."
  (interactive)
  (let ((src (emacs-reader-sources--selected)))
    (setq emacs-reader-sources--flagged (delete src emacs-reader-sources--flagged)
          emacs-reader-sources--marked (delete src emacs-reader-sources--marked))
    (tabulated-list-put-tag "" t)))

(defun emacs-reader-sources-flag ()
  "Flag selected sources or the current row for later deletion."
  (interactive)
  (if-let* ((marked (emacs-reader-sources--visible emacs-reader-sources--marked)))
      (progn
        (setq emacs-reader-sources--flagged (append marked emacs-reader-sources--flagged)
              emacs-reader-sources--marked
              (cl-set-difference emacs-reader-sources--marked marked :test #'equal))
        (tabulated-list-print t))
    (cl-pushnew (emacs-reader-sources--selected) emacs-reader-sources--flagged :test #'equal)
    (tabulated-list-put-tag "D" t)))

(defun emacs-reader-sources--delete-records (sources)
  "Confirm deletion of SOURCES, checking each record before removing it."
  (when (yes-or-no-p (format "删除这 %d 个书源（保留本地书籍）：%s? " (length sources)
                             (mapconcat (lambda (s) (alist-get 'name s)) sources "、")))
    (unwind-protect
        (progn
          (mapc #'emacs-reader-sources--check-current sources)
          (dolist (src sources)
            (emacs-reader-sources--check-current src)
            (delete-file (emacs-reader-sources--path src))
            (emacs-reader-sources--forget src)))
      (emacs-reader-sources--refresh))))

(defun emacs-reader-sources-execute ()
  "Confirm and delete only sources flagged D."
  (interactive)
  (let ((targets (emacs-reader-sources--visible emacs-reader-sources--flagged)))
    (unless targets (user-error "请先按 d 标记当前列表中待删除的书源"))
    (emacs-reader-sources--delete-records targets)))

(defun emacs-reader-sources-delete ()
  "Confirm deletion of selected sources or the source at point."
  (interactive)
  (emacs-reader-sources--delete-records
   (or (emacs-reader-sources--visible emacs-reader-sources--marked)
       (list (emacs-reader-sources--selected)))))

(transient-define-prefix emacs-reader-sources-menu ()
  "Show source management commands."
  [["书源管理"
    ("a" "添加" emacs-reader-sources-add)
    ("e" "编辑" emacs-reader-sources-edit)
    ("/" "筛选" emacs-reader-filter-edit)
    ("C-c C-l" "清除筛选" emacs-reader-filter-clear)
    ("g" "刷新" revert-buffer)]
   ["标记与删除"
    ("m" "勾选 / 取消勾选" emacs-reader-sources-mark :transient t)
    ("M" "勾选全部可见书源" emacs-reader-sources-mark-visible :transient t)
    ("u" "清除当前标记" emacs-reader-sources-unmark :transient t)
    ("d" "标记待删除" emacs-reader-sources-flag :transient t)
    ("x" "删除可见待删项" emacs-reader-sources-execute)]
   ["有效性检测（C-u 指定关键词）"
    ("t" "检测所选或当前书源" emacs-reader-sources-check)
    ("T" "检测全部书源" emacs-reader-sources-check-all)
    ("c" "取消检测" emacs-reader-sources-check-cancel)]
   ["浏览"
    ("n" "下一项" next-line :transient t)
    ("p" "上一项" previous-line :transient t)
    ("q" "退出列表" quit-window)]])

(define-derived-mode emacs-reader-sources-mode tabulated-list-mode "Reader-Sources"
  "Manage local sources using the registry at `emacs-reader-directory'."
  (setq emacs-reader-sources--states (make-hash-table :test #'equal))
  (setq-local emacs-reader-filter-refresh-function #'emacs-reader-sources--render)
  (add-hook 'tabulated-list-revert-hook #'emacs-reader-sources--reload nil t)
  (add-hook 'kill-buffer-hook #'emacs-reader-sources--stop nil t)
  (setq tabulated-list-format [("名称" 24 t) ("类型" 8 t) ("状态" 12 t) ("原因" 44 t) ("URL" 0 t)]
        tabulated-list-padding 2
        tabulated-list-printer #'emacs-reader-sources--print-entry
        tabulated-list-sort-key '("名称" . nil))
  (setq-local mode-line-process
              '((:eval (format "  %d/%d 书源" (length tabulated-list-entries)
                               (length emacs-reader-sources--records)))
                (:eval (emacs-reader-filter-description))
                "  / 筛选  C-c C-l 清筛  m 多选  M 全选可见  u 清标  d 待删  x 执行"
                "  t 检测  T 全检  c 取消  ? 帮助"))
  (tabulated-list-init-header))

(define-key emacs-reader-sources-mode-map (kbd "/") #'emacs-reader-filter-edit)
(define-key emacs-reader-sources-mode-map (kbd "C-c C-l") #'emacs-reader-filter-clear)
(define-key emacs-reader-sources-mode-map (kbd "M") #'emacs-reader-sources-mark-visible)
(define-key emacs-reader-sources-mode-map (kbd "RET") #'emacs-reader-sources-edit)
(define-key emacs-reader-sources-mode-map (kbd "a") #'emacs-reader-sources-add)
(define-key emacs-reader-sources-mode-map (kbd "e") #'emacs-reader-sources-edit)
(define-key emacs-reader-sources-mode-map (kbd "d") #'emacs-reader-sources-flag)
(define-key emacs-reader-sources-mode-map (kbd "m") #'emacs-reader-sources-mark)
(define-key emacs-reader-sources-mode-map (kbd "u") #'emacs-reader-sources-unmark)
(define-key emacs-reader-sources-mode-map (kbd "x") #'emacs-reader-sources-execute)
(define-key emacs-reader-sources-mode-map (kbd "t") #'emacs-reader-sources-check)
(define-key emacs-reader-sources-mode-map (kbd "T") #'emacs-reader-sources-check-all)
(define-key emacs-reader-sources-mode-map (kbd "c") #'emacs-reader-sources-check-cancel)
(define-key emacs-reader-sources-mode-map (kbd "?") #'emacs-reader-sources-menu)

;;;###autoload
(defun emacs-reader-sources ()
  "Display registered sources in the Reader tab."
  (interactive)
  (let ((buffer (get-buffer-create emacs-reader-sources--buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'emacs-reader-sources-mode) (emacs-reader-sources-mode))
      (revert-buffer))
    (emacs-reader--display-buffer buffer)))

(provide 'emacs-reader-sources)
;;; emacs-reader-sources.el ends here
