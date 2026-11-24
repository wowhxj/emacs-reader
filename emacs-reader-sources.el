;;; emacs-reader-sources.el --- Manage local reader sources -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Manage registered catalogs and Legado definitions from a tabulated list.

;;; Code:

(require 'emacs-reader-local)
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
         (signal (car err) (cdr err)))))))

(defun emacs-reader-sources--entries ()
  "Build tabulated list entries from registered records."
  (mapcar (lambda (src)
            (list src (vector (alist-get 'name src) (alist-get 'type src) (alist-get 'url src))))
          (emacs-reader-local--records "sources")))

(defun emacs-reader-sources--refresh ()
  "Refresh the sources list if it is open."
  (when-let* ((buffer (get-buffer emacs-reader-sources--buffer)))
    (with-current-buffer buffer (tabulated-list-print t))))

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

(defun emacs-reader-sources-delete ()
  "Confirm and delete the selected source record."
  (interactive)
  (let ((src (emacs-reader-sources--selected)))
    (when (yes-or-no-p (format "删除书源「%s」并保留本地书籍副本? " (alist-get 'name src)))
      (emacs-reader-sources--check-current src)
      (delete-file (emacs-reader-sources--path src))
      (emacs-reader-sources--refresh))))

(transient-define-prefix emacs-reader-sources-menu ()
  "Show source management commands."
  [["书源管理"
    ("a" "添加" emacs-reader-sources-add)
    ("e" "编辑" emacs-reader-sources-edit)
    ("d" "删除" emacs-reader-sources-delete)
    ("g" "刷新" revert-buffer)]
   ["浏览"
    ("n" "下一项" next-line :transient t)
    ("p" "上一项" previous-line :transient t)
    ("q" "退出列表" quit-window)]])

(define-derived-mode emacs-reader-sources-mode tabulated-list-mode "Reader-Sources"
  "Manage local sources using the registry at `emacs-reader-directory'."
  (setq tabulated-list-format [("名称" 28 t) ("类型" 10 t) ("URL" 0 t)]
        tabulated-list-sort-key '("名称" . nil)
        tabulated-list-entries #'emacs-reader-sources--entries)
  (setq-local mode-line-process "  a 添加  e 编辑  d 删除  ? 帮助")
  (tabulated-list-init-header))

(define-key emacs-reader-sources-mode-map (kbd "RET") #'emacs-reader-sources-edit)
(define-key emacs-reader-sources-mode-map (kbd "a") #'emacs-reader-sources-add)
(define-key emacs-reader-sources-mode-map (kbd "e") #'emacs-reader-sources-edit)
(define-key emacs-reader-sources-mode-map (kbd "d") #'emacs-reader-sources-delete)
(define-key emacs-reader-sources-mode-map (kbd "?") #'emacs-reader-sources-menu)

;;;###autoload
(defun emacs-reader-sources ()
  "Display registered sources in the Reader tab."
  (interactive)
  (let ((buffer (get-buffer-create emacs-reader-sources--buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'emacs-reader-sources-mode) (emacs-reader-sources-mode))
      (tabulated-list-print t))
    (emacs-reader--display-buffer buffer)))

(provide 'emacs-reader-sources)
;;; emacs-reader-sources.el ends here
