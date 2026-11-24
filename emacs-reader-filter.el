;;; emacs-reader-filter.el --- Filter reader lists by keywords -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Narrow the current buffer's rows while keeping the original records available.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar-local emacs-reader-filter-query "" "Active filter text for this buffer.")
(defvar-local emacs-reader-filter-refresh-function nil "Function that redraws this buffer's rows.")
(defvar-local emacs-reader-filter--compiled nil)
(defvar emacs-reader-filter--history nil)
(defvar orderless-matching-styles)
(declare-function completion--regex-pinyin nil (component))

(defun emacs-reader-filter--patterns ()
  "Return a list of regexps, one for each literal or pinyin keyword."
  (unless (equal (car emacs-reader-filter--compiled) emacs-reader-filter-query)
    (setq emacs-reader-filter--compiled
          (cons emacs-reader-filter-query
                (cl-loop for term in (split-string emacs-reader-filter-query nil t)
                         for literal = (regexp-quote term)
                         for pinyin = (and (string-match-p "\\`[a-zA-Z]+\\'" term)
                                           (boundp 'orderless-matching-styles)
                                           (memq 'completion--regex-pinyin
                                                 orderless-matching-styles)
                                           (fboundp 'completion--regex-pinyin)
                                           (completion--regex-pinyin term))
                         collect (if (stringp pinyin) (concat literal "\\|" pinyin) literal)))))
  (cdr emacs-reader-filter--compiled))

(defun emacs-reader-filter-match-p (text)
  "Return non-nil when TEXT matches every keyword in this buffer's filter."
  (let ((case-fold-search t))
    (cl-every (lambda (pattern) (string-match-p pattern text)) (emacs-reader-filter--patterns))))

(defun emacs-reader-filter-rows (entries)
  "Return the ENTRIES whose displayed column values match this buffer's filter."
  (if (string-blank-p emacs-reader-filter-query) entries
    (cl-remove-if-not
     (lambda (entry)
       (emacs-reader-filter-match-p
        (mapconcat (lambda (col) (if (consp col) (car col) col)) (cadr entry) " ")))
     entries)))

(defun emacs-reader-filter-description ()
  "Return a short filter label suitable for a mode or header line."
  (if (string-blank-p emacs-reader-filter-query) ""
    (format "  筛选〔%s〕" (string-replace "%" "%%" emacs-reader-filter-query))))

(defun emacs-reader-filter--set (query)
  "Update the current list to show rows matching QUERY."
  (unless (equal query emacs-reader-filter-query)
    (setq emacs-reader-filter-query query)
    (when emacs-reader-filter-refresh-function (funcall emacs-reader-filter-refresh-function))))

(defun emacs-reader-filter-clear ()
  "Clear the current list filter."
  (interactive)
  (emacs-reader-filter--set ""))

(defun emacs-reader-filter-edit ()
  "Preview a keyword filter while typing, committing on RET or restoring on quit."
  (interactive)
  (let ((target (current-buffer)) (old emacs-reader-filter-query) committed)
    (unwind-protect
        (let ((value (minibuffer-with-setup-hook
                         (lambda ()
                           (add-hook 'post-command-hook
                                     (lambda ()
                                       (let ((text (minibuffer-contents-no-properties)))
                                         (when (buffer-live-p target)
                                           (with-current-buffer target
                                             (emacs-reader-filter--set text))))) nil t))
                       (read-string "筛选（空格分词）：" old 'emacs-reader-filter--history))))
          (when (buffer-live-p target)
            (with-current-buffer target (emacs-reader-filter--set value)))
          (setq committed t))
      (when (and (not committed) (buffer-live-p target))
        (with-current-buffer target (emacs-reader-filter--set old))))))

(provide 'emacs-reader-filter)
;;; emacs-reader-filter.el ends here
