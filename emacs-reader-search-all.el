;;; emacs-reader-search-all.el --- Search several local sources -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Fetch one result page per selected source and retain its native book or
;; catalog item for preview, acquisition and navigation.

;;; Code:
(require 'emacs-reader)
(require 'emacs-reader-opds)

(declare-function emacs-reader-catalog-results "emacs-reader-opds")
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
  (mapc (lambda (run) (cancel-timer (cdr run))) emacs-reader-all--running)
  (setq emacs-reader-all--running nil emacs-reader-all--queue nil))

(defun emacs-reader-all--render ()
  "Show results, retryable failures and current progress."
  (let ((errors (cl-loop for task in emacs-reader-all--tasks
                         for reason = (emacs-reader-search-task-error task)
                         when reason collect
                         (list task (vector "请求失败" ""
                                            (alist-get 'name (emacs-reader-search-task-source task))
                                            reason))))
        (pending (+ (length emacs-reader-all--queue) (length emacs-reader-all--running))))
    (setq tabulated-list-entries (append emacs-reader-all--rows errors)
          header-line-format
          (format "  %s · %d 个书源 · %d 条结果 · 待返回 %d · 失败 %d    RET 打开   a 入架   m 更多   g 重试"
                  emacs-reader-all--keyword (length emacs-reader-all--tasks)
                  (cl-count-if-not
                   (lambda (row)
                     (let ((item (cdar row)))
                       (and (emacs-reader-opds--item-p item)
                            (emacs-reader-opds--item-pagination item))))
                   emacs-reader-all--rows)
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
        (push (list (cons source item)
                    (vector (or (emacs-reader-opds--item-label item) "")
                            (or (emacs-reader-opds--item-author item) "") (alist-get 'name source)
                            (or (emacs-reader-opds--availability item) "打开目录"))) entries)))
    (let ((count (length entries)))
      (setq emacs-reader-all--rows (nconc emacs-reader-all--rows (nreverse entries)))
      count)))

(defun emacs-reader-all--finish (buffer task timer items err)
  "Accept ITEMS or ERR from TASK only while TIMER belongs to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq (cdr (assq task emacs-reader-all--running)) timer)
        (cancel-timer timer)
        (setq emacs-reader-all--running (assq-delete-all task emacs-reader-all--running))
        (condition-case problem
            (unless err
              (let* ((source (emacs-reader-search-task-source task))
                     (added (emacs-reader-all--append source items)))
                (setf (emacs-reader-search-task-more task)
                      (and (> added 0) (equal (alist-get 'type source) "legado")))
                (cl-incf (emacs-reader-search-task-page task))))
          (error (setq err (error-message-string problem))))
        (setf (emacs-reader-search-task-error task) err)
        (emacs-reader-all--pump)))))

(defun emacs-reader-all--start (task)
  "Fetch one page for TASK in the current search buffer."
  (let* ((buffer (current-buffer)) (source (emacs-reader-search-task-source task)) timer
         (done (lambda (items err) (emacs-reader-all--finish buffer task timer items err))))
    (setq timer (run-at-time 30 nil done nil "请求超时"))
    (push (cons task timer) emacs-reader-all--running)
    (condition-case problem
        (if (equal (alist-get 'type source) "legado")
            (emacs-reader-legado-search (emacs-reader-local--plist (alist-get 'definition source))
                                        emacs-reader-all--keyword done
                                        (emacs-reader-search-task-page task))
          (emacs-reader-catalog-results source emacs-reader-all--keyword done))
      (error (funcall done nil (error-message-string problem))))))

(defun emacs-reader-all--pump ()
  "Fetch queued pages with up to four active sources."
  (unless emacs-reader-all--pumping
    (setq emacs-reader-all--pumping t)
    (unwind-protect
        (while (and emacs-reader-all--queue (< (length emacs-reader-all--running) 4))
          (emacs-reader-all--start (pop emacs-reader-all--queue)))
      (setq emacs-reader-all--pumping nil))
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
           (emacs-reader-catalog-open-item (car row) (cdr row)))
          ((eq (emacs-reader-opds--item-kind (cdr row)) 'record)
           (user-error "此行是书目，按 RET 查看详情和可用版本"))
          (t (user-error "请按 RET 打开目录，选择可下载的版本")))))

(defun emacs-reader-all-add ()
  "Add the book at point to the local shelf."
  (interactive)
  (emacs-reader-all-open t))

(define-derived-mode emacs-reader-all-mode emacs-reader-search-mode "Reader-All"
  "Display results from multiple local sources."
  (setq tabulated-list-format [("书名 / 目录" 34 t) ("作者" 16 t) ("书源" 19 t) ("操作 / 状态" 0 nil)])
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'emacs-reader-all--stop nil t)
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
