;;; emacs-reader-search-all.el --- Search several local sources -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Stream catalog pages and retain each source's native book or
;; catalog item for preview, acquisition and navigation.

;;; Code:
(require 'emacs-reader)
(require 'emacs-reader-filter)
(require 'emacs-reader-opds)
(require 'emacs-reader-search-progress)

(declare-function emacs-reader-catalog-search-pages "emacs-reader-opds")
(declare-function emacs-reader-catalog-open-item "emacs-reader-opds")

(cl-defstruct emacs-reader-search-task source (page 1) error more)
(defvar-local emacs-reader-all--keyword nil)
(defvar-local emacs-reader-all--tasks nil)
(defvar-local emacs-reader-all--queue nil)
(defvar-local emacs-reader-all--running nil)
(defvar-local emacs-reader-all--pumping nil)
(defvar-local emacs-reader-all--rows nil)
(defvar-local emacs-reader-all--seen nil)

(defun emacs-reader-all--stop ()
  "Invalidate outstanding requests and cancel their timers."
  (emacs-reader-search-progress-stop)
  (let ((runs emacs-reader-all--running))
    (setq emacs-reader-all--running nil emacs-reader-all--queue nil)
    (dolist (run runs)
      (let ((token (cdr run)))
        (if (timerp token) (cancel-timer token)
          (when (functionp (cdr token)) (funcall (cdr token))))))))

(defun emacs-reader-all--render ()
  "Show results, retryable failures and current progress."
  (let ((errors (cl-loop for task in emacs-reader-all--tasks
                         for reason = (emacs-reader-search-task-error task)
                         when reason collect
                         (list task (vector "请求失败" ""
                                            (alist-get 'name (emacs-reader-search-task-source task))
                                            reason))))
        (pending (+ (length emacs-reader-all--queue) (length emacs-reader-all--running))))
    (setq tabulated-list-entries (emacs-reader-filter-rows (append emacs-reader-all--rows errors))
          header-line-format
          (format (concat "%s  %s · %d 个书源 · %d 条结果 · 待返回 %d · 失败 %d"
                          "    RET 试读 / 打开   a 加入书架   m 更多   g 重试   / 筛选")
                  (emacs-reader-filter-description) emacs-reader-all--keyword
                  (length emacs-reader-all--tasks) (length emacs-reader-all--rows)
                  pending (length errors)))
    (tabulated-list-print t)))

(defun emacs-reader-all--append (source items)
  "Append new ITEMS from SOURCE and return the number added."
  (let (entries)
    (dolist (item items)
      (if (equal (alist-get 'type source) "legado")
          (let* ((book (emacs-reader-local--book item))
                 (key (cons (alist-get 'url source) (alist-get 'bookUrl book))))
            (unless (gethash key emacs-reader-all--seen)
              (puthash key t emacs-reader-all--seen)
              (push (list (cons source book)
                          (vector (emacs-reader--book-field book 'name)
                                  (emacs-reader--book-field book 'author)
                                  (alist-get 'name source) "试读 / 入架")) entries)))
        ;; Retrying a failed catalog starts again at its first page.
        (let ((key (list (alist-get 'type source) (alist-get 'url source)
                         (emacs-reader-opds--item-kind item) (emacs-reader-opds--item-url item)
                         (emacs-reader-opds--item-label item))))
          (unless (gethash key emacs-reader-all--seen)
            (puthash key t emacs-reader-all--seen)
            (push (list (cons source item)
                        (vector (or (emacs-reader-opds--item-label item) "")
                                (or (emacs-reader-opds--item-author item) "")
                                (alist-get 'name source)
                                (or (emacs-reader-opds--availability item) "打开目录"))) entries)))))
    (let ((count (length entries)))
      (setq emacs-reader-all--rows (nconc emacs-reader-all--rows (nreverse entries)))
      count)))

(defun emacs-reader-all--finish (buffer task token items err)
  "Accept ITEMS or ERR from TASK only while TOKEN belongs to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq (cdr (assq task emacs-reader-all--running)) token)
        (when (timerp token) (cancel-timer token))
        (setq emacs-reader-all--running (assq-delete-all task emacs-reader-all--running))
        (condition-case problem
            (when (and (not err) (timerp token))
              (setf (emacs-reader-search-task-more task)
                    (> (emacs-reader-all--append (emacs-reader-search-task-source task) items) 0))
              (cl-incf (emacs-reader-search-task-page task)))
          (error (setq err (error-message-string problem))))
        (setf (emacs-reader-search-task-error task) err)
        (emacs-reader-all--pump)))))

(defun emacs-reader-all--start (task)
  "Start TASK, appending catalog pages to the current result buffer."
  (let* ((buffer (current-buffer)) (source (emacs-reader-search-task-source task))
         (legado (equal (alist-get 'type source) "legado")) token
         (done (lambda (items err) (emacs-reader-all--finish buffer task token items err))))
    (setq token (if legado (run-at-time 30 nil done nil "请求超时") (list 'catalog)))
    (push (cons task token) emacs-reader-all--running)
    (condition-case problem
        (if legado
            (emacs-reader-legado-search (emacs-reader-local--plist (alist-get 'definition source))
                                        emacs-reader-all--keyword done
                                        (emacs-reader-search-task-page task))
          (setcdr token
                  (emacs-reader-catalog-search-pages
                   source emacs-reader-all--keyword
                   (lambda (items)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (eq (cdr (assq task emacs-reader-all--running)) token)
                           (emacs-reader-all--append source items)
                           (emacs-reader-all--render)))))
                   (lambda (err) (funcall done nil err)))))
      (quit (emacs-reader-all--stop) (signal 'quit nil))
      (error (funcall done nil (error-message-string problem))))))

(defun emacs-reader-all--pump ()
  "Fetch queued pages with up to four active sources."
  (unless emacs-reader-all--pumping
    (when emacs-reader-all--queue (emacs-reader-search-progress-start))
    (setq emacs-reader-all--pumping t)
    (unwind-protect
        (while (and emacs-reader-all--queue (< (length emacs-reader-all--running) 4))
          (emacs-reader-all--start (pop emacs-reader-all--queue)))
      (setq emacs-reader-all--pumping nil)
      (unless (or emacs-reader-all--queue emacs-reader-all--running)
        (emacs-reader-search-progress-stop)))
    (emacs-reader-all--render)))

(defun emacs-reader-all--batch (predicate)
  "Fetch pages for tasks matching PREDICATE."
  (when (or emacs-reader-all--queue emacs-reader-all--running)
    (user-error "正在搜索，请等待当前批次返回"))
  (setq emacs-reader-all--queue (cl-remove-if-not predicate emacs-reader-all--tasks))
  (unless emacs-reader-all--queue (user-error "没有匹配的书源可继续请求"))
  (dolist (task emacs-reader-all--queue) (setf (emacs-reader-search-task-error task) nil))
  (emacs-reader-all--pump))

(defun emacs-reader-all-more ()
  "Fetch another page from Legado sources with additional results."
  (interactive)
  (emacs-reader-all--batch
   (lambda (task) (and (emacs-reader-search-task-more task)
                       (not (emacs-reader-search-task-error task))))))

(defun emacs-reader-all-retry ()
  "Retry all failed sources without discarding successful results."
  (interactive)
  (emacs-reader-all--batch #'emacs-reader-search-task-error))

(defun emacs-reader-all-open (&optional add)
  "Open the selected result, or add the selected book when ADD is non-nil."
  (interactive)
  (let ((row (tabulated-list-get-id)))
    (cond ((null row) (user-error "请先选择一行结果"))
          ((emacs-reader-search-task-p row)
           (emacs-reader-all--batch (lambda (task) (eq task row))))
          ((equal (alist-get 'type (car row)) "legado")
           (if add (emacs-reader-add-to-shelf (cdr row))
             (emacs-reader-open-book (cdr row) t)))
          ((or (not add) (eq (emacs-reader-opds--item-kind (cdr row)) 'download))
           (emacs-reader-catalog-open-item (car row) (cdr row) add))
          ((eq (emacs-reader-opds--item-kind (cdr row)) 'record)
           (user-error "此行是书目，按 RET 查看详情和可用版本"))
          (t (user-error "请按 RET 打开目录，选择可下载的版本")))))

(defun emacs-reader-all-add ()
  "Add the book at point to the local shelf."
  (interactive)
  (emacs-reader-all-open t))

(define-derived-mode emacs-reader-all-mode emacs-reader-search-mode "Reader-All"
  "Display results from multiple local sources."
  (setq-local emacs-reader-filter-refresh-function #'emacs-reader-all--render)
  (setq tabulated-list-format [("书名 / 目录" 34 t) ("作者" 16 t) ("书源" 19 t) ("操作 / 状态" 0 nil)])
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'emacs-reader-all--stop nil t)
  (add-hook 'quit-window-hook #'emacs-reader-all--stop nil t)
  (add-hook 'change-major-mode-hook #'emacs-reader-all--stop nil t))

(define-key emacs-reader-all-mode-map (kbd "RET") #'emacs-reader-all-open)
(define-key emacs-reader-all-mode-map (kbd "a") #'emacs-reader-all-add)
(define-key emacs-reader-all-mode-map (kbd "m") #'emacs-reader-all-more)
(define-key emacs-reader-all-mode-map (kbd "g") #'emacs-reader-all-retry)

(defun emacs-reader-search-all (keyword sources)
  "Search SOURCES for KEYWORD and display results as they arrive."
  (let ((buffer (get-buffer-create "*Reader Search All*")))
    (emacs-reader--display-buffer buffer)
    (with-current-buffer buffer
      (emacs-reader-all-mode)
      (setq emacs-reader-all--keyword keyword
            emacs-reader-all--seen (make-hash-table :test #'equal)
            emacs-reader-all--tasks
            (mapcar (lambda (src) (make-emacs-reader-search-task :source src)) sources)
            emacs-reader-all--queue (copy-sequence emacs-reader-all--tasks))
      (emacs-reader-all--pump))))

(provide 'emacs-reader-search-all)
;;; emacs-reader-search-all.el ends here
