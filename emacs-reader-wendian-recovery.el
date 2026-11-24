;;; emacs-reader-wendian-recovery.el --- Recover a failed Wendian page -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Attach manual recovery commands to the current reading buffer.

;;; Code:

(require 'browse-url)
(require 'subr-x)

(defvar emacs-reader--current-book)
(declare-function emacs-reader--load-chapter "emacs-reader"
                  (index &optional speech-generation at-end))
(declare-function emacs-reader--load-catalog "emacs-reader" (&optional show))
(declare-function emacs-reader-wendian-import-page "emacs-reader-wendian-read" (url file))
(defvar-local emacs-reader--recovery nil
  "Failed Wendian request, with its URL, chapter index and header.")

(declare-function emacs-reader-preview-epub "emacs-reader-local" (file))

(defconst emacs-reader--recovery-actions
  '(("v" "打开原站" emacs-reader-wendian-open-original)
    ("i" "载入 HTML" emacs-reader-wendian-continue-html)
    ("e" "试读本地 EPUB" emacs-reader-preview-epub)
    ("g" "重试" emacs-reader-wendian-retry)))

(defvar emacs-reader--recovery-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (action emacs-reader--recovery-actions map)
      (define-key map (kbd (car action)) (nth 2 action)))))

(define-minor-mode emacs-reader--recovery-mode
  "Enable recovery shortcuts for a failed Wendian request."
  :lighter nil :keymap emacs-reader--recovery-mode-map)

(defun emacs-reader--clear-recovery ()
  "Restore the reading header and remove the recovery commands."
  (when emacs-reader--recovery
    (setq header-line-format (plist-get emacs-reader--recovery :header)
          emacs-reader--recovery nil)
    (emacs-reader--recovery-mode -1)
    (force-mode-line-update)))

(defun emacs-reader--offer-recovery (url &optional index show)
  "Offer manual recovery of the Wendian page URL for INDEX or catalog SHOW."
  (when (and (equal (alist-get 'readerType emacs-reader--current-book) "legado")
             (member (alist-get 'origin emacs-reader--current-book)
                     '("https://wendian.dicomp.net" "https://wendian.dicomp.net/"))
             (stringp url)
             (string-match-p (rx string-start "https://wendian.dicomp.net/"
                                 (or "ancient" "tcm" "classics" "mengxue") "/"
                                 (or "detail" "book") ".php?") url))
    (emacs-reader--clear-recovery)
    (setq emacs-reader--recovery (list :url url :index index :show show :header header-line-format)
          header-line-format
          (cons "  "
                (mapcar
                 (lambda (action)
                   (let ((command (nth 2 action)) (map (make-sparse-keymap)))
                     (define-key map [header-line mouse-1]
                                 (lambda (event) (interactive "e")
                                   (with-selected-window (posn-window (event-start event))
                                     (call-interactively command))))
                     (propertize (format "[%s %s] " (car action) (cadr action))
                                 'face 'link 'mouse-face 'highlight 'local-map map)))
                 emacs-reader--recovery-actions)))
    (emacs-reader--recovery-mode 1)
    (force-mode-line-update)))

(defun emacs-reader--failed-page ()
  "Return recovery state for the current reader."
  (or emacs-reader--recovery (user-error "当前阅读器没有待恢复的研辞问典页面")))

(defun emacs-reader-wendian-open-original ()
  "Open the failed book or chapter in the system default browser."
  (interactive)
  (browse-url-default-browser (plist-get (emacs-reader--failed-page) :url)))

(defun emacs-reader-wendian-retry ()
  "Retry the failed request in this reader."
  (interactive)
  (let ((failure (emacs-reader--failed-page)))
    (if-let* ((index (plist-get failure :index)))
        (emacs-reader--load-chapter index)
      (emacs-reader--load-catalog (plist-get failure :show)))))

(defun emacs-reader-wendian-continue-html ()
  "Continue the failed book or chapter from a manually saved HTML file."
  (interactive)
  (let* ((failure (emacs-reader--failed-page))
         (file (read-file-name "浏览器保存的 HTML：" nil nil t)))
    (unless (eq failure emacs-reader--recovery) (user-error "阅读请求已更改"))
    (emacs-reader-wendian-import-page (plist-get failure :url) file)
    (emacs-reader-wendian-retry)))

(provide 'emacs-reader-wendian-recovery)
;;; emacs-reader-wendian-recovery.el ends here
