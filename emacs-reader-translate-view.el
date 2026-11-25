;;; emacs-reader-translate-view.el --- Display paragraph translations -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Project translations into a reading buffer using display-only overlays.

;;; Code:

(require 'cl-lib)

(defface emacs-reader-translate-view-face
  '((t :inherit shadow))
  "Face for paragraph translations in a reading buffer."
  :group 'emacs-reader)

(defvar-local emacs-reader-translate-view--overlays nil
  "Translation overlays owned by this buffer.")

(defun emacs-reader-translate-view--ranges ()
  "Return bounds of nonblank paragraphs in the current buffer.
Keep internal line breaks and omit surrounding whitespace and indentation."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((space " \t\r\n　 ") ranges)
        (while (progn (skip-chars-forward space) (not (eobp)))
          (let* ((start (point))
                 (next (if (re-search-forward "\n[ \t\r　 ]*\n" nil t)
                           (match-beginning 0)
                         (point-max))))
            (goto-char next)
            (skip-chars-backward space start)
            (push (cons start (point)) ranges)
            (goto-char next)))
        (nreverse ranges)))))

(defun emacs-reader-translate-view--texts ()
  "Return plain source strings for the current buffer's paragraphs."
  (mapcar (lambda (bounds)
            (buffer-substring-no-properties (car bounds) (cdr bounds)))
          (emacs-reader-translate-view--ranges)))

(defun emacs-reader-translate-view-split (text)
  "Return source paragraphs from already formatted chapter TEXT."
  (with-temp-buffer
    (insert text)
    (emacs-reader-translate-view--texts)))

(defun emacs-reader-translate-view-paragraphs ()
  "Return the complete chapter's source paragraphs in a reading buffer."
  (when (derived-mode-p 'emacs-reader-reading-mode)
    (save-restriction
      (widen)
      (emacs-reader-translate-view--texts))))

(defun emacs-reader-translate-view-clear ()
  "Remove this module's translation overlays from the current buffer."
  (mapc #'delete-overlay emacs-reader-translate-view--overlays)
  (setq emacs-reader-translate-view--overlays nil))

(defun emacs-reader-translate-view-show (translations)
  "Replace the reading buffer's translations with TRANSLATIONS in order.
Accept a list or vector.  Nil and blank entries leave their paragraph bare.
Missing or excess entries are ignored."
  (when (derived-mode-p 'emacs-reader-reading-mode)
    (save-restriction
      (widen)
      (emacs-reader-translate-view-clear)
      (add-hook 'change-major-mode-hook #'emacs-reader-translate-view-clear nil t)
      (cl-mapc
       (lambda (bounds text)
         (when (and (stringp text) (string-match-p "[^ \t\r\n　 ]" text))
           ;; Anchor on a real character even at EOF.  Erasing the chapter
           ;; then evaporates the overlay before the next chapter is inserted.
           (let* ((end (save-excursion (goto-char (cdr bounds)) (line-end-position)))
                  (overlay (make-overlay (1- end) end)))
             (overlay-put overlay 'evaporate t)
             (overlay-put overlay 'after-string
                          (propertize (concat "\n" (substring-no-properties text))
                                      'face 'emacs-reader-translate-view-face))
             (push overlay emacs-reader-translate-view--overlays))))
       (emacs-reader-translate-view--ranges) translations))))

(provide 'emacs-reader-translate-view)
;;; emacs-reader-translate-view.el ends here
