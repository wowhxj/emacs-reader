;;; emacs-reader-translate.el --- Bilingual reading and lookahead -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Translate the visible chapter first, then warm the on-disk cache in reading order.

;;; Code:

(require 'cl-lib)
(autoload 'emacs-reader-translate-view-paragraphs "emacs-reader-translate-view")
(autoload 'emacs-reader-translate-view-clear "emacs-reader-translate-view")
(autoload 'emacs-reader-translate-view-show "emacs-reader-translate-view")
(autoload 'emacs-reader-translate-fetch "emacs-reader-translate-fetch")
(autoload 'emacs-reader-translate-fetch-content-error "emacs-reader-translate-fetch")
(autoload 'emacs-reader-translation-request "emacs-reader-translation-engine")
(require 'emacs-reader-translation-assets)
(require 'emacs-reader-local)

(defcustom emacs-reader-translation-direction 'en-zh
  "Default translation direction for new readers."
  :type '(choice (const :tag "英语 → 中文" en-zh) (const :tag "中文 → 英语" zh-en))
  :group 'emacs-reader)
(make-variable-buffer-local 'emacs-reader-translation-direction)

(defcustom emacs-reader-translation-prefetch-chapters 1
  "Number of additional chapters to translate after the current one."
  :type 'natnum :group 'emacs-reader)

(defvar emacs-reader--current-book)
(defvar emacs-reader--current-chapter-index)
(defvar emacs-reader--chapters)
(defvar emacs-reader-translation-mode)
(defvar-local emacs-reader-translate--token nil)
(defvar-local emacs-reader-translate--cancel nil)
(defvar-local emacs-reader-translate--status "")

(defun emacs-reader-translate--status (text)
  "Display TEXT in the translation mode indicator."
  (setq emacs-reader-translate--status text)
  (force-mode-line-update))

(defun emacs-reader-translate--stop ()
  "Invalidate this reader's pending work and remove its translations."
  (setq emacs-reader-translate--token nil)
  (when emacs-reader-translate--cancel
    (funcall (prog1 emacs-reader-translate--cancel (setq emacs-reader-translate--cancel nil))))
  (emacs-reader-translate-view-clear)
  (emacs-reader-translate--status ""))

(defun emacs-reader-translate--task (start done)
  "Start an async step using START and deliver its current result to DONE."
  (let* ((token (list t)) (reader (current-buffer))
         (deliver (lambda (value err)
                    (when (buffer-live-p reader)
                      (with-current-buffer reader
                        (when (eq token emacs-reader-translate--token)
                          (setq emacs-reader-translate--token nil
                                emacs-reader-translate--cancel nil)
                          (funcall done value err)))))))
    (setq emacs-reader-translate--token token)
    (condition-case err
        (let ((cancel (funcall start deliver)))
          (when (eq token emacs-reader-translate--token)
            (setq emacs-reader-translate--cancel (and (functionp cancel) cancel))))
      (error (funcall deliver nil (error-message-string err))))))

(defun emacs-reader-translate--next (index end)
  "Schedule the next chapter INDEX through END without blocking cached reads."
  (if (> index end) (emacs-reader-translate--status "双语")
    (emacs-reader-translate--task
     (lambda (cb)
       (let ((timer (run-at-time 0 nil cb t nil))) (lambda () (cancel-timer timer))))
     (lambda (_ _err)
       (emacs-reader-translate--status (format "预译第%d章" (1+ index)))
       (emacs-reader-translate--task
        (lambda (cb) (emacs-reader-translate-fetch (current-buffer) index cb))
        (lambda (paragraphs err)
          (if (or err (not paragraphs))
              (progn (when err (message "跳过第 %d 章预译：%s" (1+ index) err))
                     (emacs-reader-translate--next (1+ index) end))
            (emacs-reader-translate--request index end paragraphs nil))))))))

(defun emacs-reader-translate--request (index end paragraphs display)
  "Translate PARAGRAPHS of INDEX, optionally DISPLAY them, then continue to END."
  (emacs-reader-translate--task
   (lambda (cb)
     (emacs-reader-translation-request emacs-reader--current-book
                                       (aref emacs-reader--chapters index)
                                       emacs-reader-translation-direction paragraphs cb))
   (lambda (translations err)
     (if err
         (progn (emacs-reader-translate--status (if display "翻译失败" "预译失败"))
                (message "第 %d 章翻译失败：%s" (1+ index) err))
       (when display (emacs-reader-translate-view-show translations))
       (emacs-reader-translate--next (1+ index) end)))))

(defun emacs-reader-translate-chapter ()
  "Translate the current chapter if bilingual reading is enabled."
  (when emacs-reader-translation-mode
    (let ((old emacs-reader-translate--cancel) (index emacs-reader--current-chapter-index))
      (setq emacs-reader-translate--token nil emacs-reader-translate--cancel nil)
      (emacs-reader-translate-view-clear)
      ;; Subscribe to the visible chapter before releasing an in-flight prefetch of it.
      (unwind-protect
          (if (not (and (natnump index) (< index (length emacs-reader--chapters))))
              (emacs-reader-translate--status "等候正文")
            (let ((problem (emacs-reader-translate-fetch-content-error (buffer-string)))
                  (parts (emacs-reader-translate-view-paragraphs)))
              (if (or problem (not parts)) (emacs-reader-translate--status "本章无正文")
                (emacs-reader-translate--status "翻译中")
                (emacs-reader-translate--request
                 index (min (+ index (max 0 emacs-reader-translation-prefetch-chapters))
                            (1- (length emacs-reader--chapters))) parts t))))
        (when old (funcall old))))))

(defvar-local emacs-reader-translate--restored nil)
(defvar emacs-reader-translate--restoring nil)
(defvar-local emacs-reader-translate--asset-reader nil)

(defun emacs-reader-translate--preferences ()
  "Return the preference file for the current book."
  (expand-file-name ".reading.json"
                    (emacs-reader-translation-asset-book-directory emacs-reader--current-book)))

(defun emacs-reader-translate--save-preferences ()
  "Remember this reader's opt-in and direction without changing its Org assets."
  (when (and emacs-reader--current-book (not emacs-reader-translate--restoring))
    (emacs-reader-local--write (emacs-reader-translate--preferences)
                               `((enabled . ,(if emacs-reader-translation-mode t :false))
                                 (direction . ,(symbol-name emacs-reader-translation-direction))))))

;;;###autoload
(defun emacs-reader-translate-on-display ()
  "Restore this book's explicit opt-in once, then refresh the bilingual view."
  (if (or emacs-reader-translate--restored emacs-reader-translation-mode)
      (when emacs-reader-translation-mode (emacs-reader-translate-chapter))
    (setq emacs-reader-translate--restored t)
    (let* ((file (emacs-reader-translate--preferences))
           (prefs (and (file-readable-p file) (emacs-reader-local--read file))))
      (when (equal (alist-get 'direction prefs) "zh-en")
        (setq-local emacs-reader-translation-direction 'zh-en))
      (when (equal (alist-get 'direction prefs) "en-zh")
        (setq-local emacs-reader-translation-direction 'en-zh))
      (when (eq t (alist-get 'enabled prefs))
        (let ((emacs-reader-translate--restoring t)) (emacs-reader-translation-mode 1))))))

(defun emacs-reader-translate--current-asset ()
  "Return (PATH PARAGRAPHS) for the currently displayed chapter, if available."
  (when (and emacs-reader--current-book (natnump emacs-reader--current-chapter-index)
             (< emacs-reader--current-chapter-index (length emacs-reader--chapters)))
    (let ((parts (emacs-reader-translate-view-paragraphs)))
      (and parts (list (emacs-reader-translation-asset-path
                        emacs-reader--current-book (aref emacs-reader--chapters
                                                         emacs-reader--current-chapter-index)
                        emacs-reader-translation-direction parts) parts)))))

(defun emacs-reader-translate--asset-saved ()
  "Apply manual edits to the reader that opened this document."
  (let ((file buffer-file-name))
    (when (buffer-live-p emacs-reader-translate--asset-reader)
      (with-current-buffer emacs-reader-translate--asset-reader
        (when emacs-reader-translation-mode
          (pcase-let ((`(,current ,parts) (emacs-reader-translate--current-asset)))
            (when (and current (file-equal-p file current))
              (condition-case err
                  (emacs-reader-translate-view-show
                   (emacs-reader-translation-asset-read file parts))
                (error (message "译文已保存；阅读页未更新：%s" (error-message-string err)))))))))))

;;;###autoload
(defun emacs-reader-translation-open-document ()
  "Open this chapter's bilingual Org document for reading or editing."
  (interactive)
  (let ((file (car (emacs-reader-translate--current-asset))) (reader (current-buffer)))
    (unless (and file (file-exists-p file)) (user-error "本章尚无译稿；按 t 开启翻译并等待本章完成"))
    (find-file-other-window file)
    (setq-local emacs-reader-translate--asset-reader reader)
    (add-hook 'after-save-hook #'emacs-reader-translate--asset-saved nil t)))

;;;###autoload
(defun emacs-reader-translation-library ()
  "Browse the bilingual Org documents in the translation library."
  (interactive)
  (let ((root (emacs-reader-translation-asset-root)))
    (unless (file-directory-p root) (user-error "当前还没有译文资产"))
    (dired root)))

;;;###autoload
(define-minor-mode emacs-reader-translation-mode
  "Toggle paragraph translations in this reader, using the configured gptel model."
  :lighter (:eval (concat " " emacs-reader-translate--status))
  (if emacs-reader-translation-mode
      (progn
        (unless (derived-mode-p 'emacs-reader-reading-mode)
          (setq emacs-reader-translation-mode nil) (user-error "请在阅读页开启翻译"))
        (setq emacs-reader-translate--restored t)
        (emacs-reader-translate--save-preferences)
        (add-hook 'kill-buffer-hook #'emacs-reader-translate--stop nil t)
        (add-hook 'change-major-mode-hook #'emacs-reader-translate--stop nil t)
        (emacs-reader-translate-chapter))
    (emacs-reader-translate--save-preferences)
    (remove-hook 'kill-buffer-hook #'emacs-reader-translate--stop t)
    (remove-hook 'change-major-mode-hook #'emacs-reader-translate--stop t)
    (emacs-reader-translate--stop)))

;;;###autoload
(defun emacs-reader-translation-select-direction (direction)
  "Select DIRECTION for this book and refresh any visible translation."
  (interactive
   (let* ((choices '(("英语 → 中文" . en-zh) ("中文 → 英语" . zh-en)))
          (current (car (rassq emacs-reader-translation-direction choices))))
     (list (cdr (assoc (completing-read "翻译方向：" choices nil t nil nil current) choices)))))
  (unless (memq direction '(en-zh zh-en)) (user-error "无效的翻译方向"))
  (setq-local emacs-reader-translation-direction direction)
  (emacs-reader-translate--save-preferences)
  (emacs-reader-translate-chapter)
  (message "翻译方向：%s%s" (if (eq direction 'en-zh) "英语 → 中文" "中文 → 英语")
           (if emacs-reader-translation-mode "" "；按 t 开启翻译")))

(provide 'emacs-reader-translate)
;;; emacs-reader-translate.el ends here
