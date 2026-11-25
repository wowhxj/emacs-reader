;;; emacs-reader-translate-view.el --- Display line translations -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Project translations into a reading buffer using display-only overlays.

;;; Code:

(require 'cl-lib)

(defface emacs-reader-translate-view-face
  '((t :inherit shadow))
  "Face for line translations in a reading buffer."
  :group 'emacs-reader)

(defvar-local emacs-reader-translate-view--overlays nil
  "Translation overlays owned by this buffer.")

(defcustom emacs-reader-translation-line-width 60
  "Maximum display columns per bilingual source line before word wrapping.
Fixed source boundaries let prefetched and saved translations match on any frame."
  :type 'natnum :group 'emacs-reader)

(defun emacs-reader-translate-view--ranges ()
  "Return bounds of nonblank reading lines, preserving the source buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((width (max 1 emacs-reader-translation-line-width))
          (spaces " \t\r　 ") ranges)
      (while (< (point) (point-max))
        (let ((end (line-end-position)) (indent 0))
          (skip-chars-forward spaces end)
          (setq indent (current-column))
          (while (< (point) end)
            (let ((start (point)) (columns indent) boundary)
              (setq indent 0)
              (while (and (< (point) end)
                          (or (= (point) start) (<= (+ columns (char-width (char-after))) width)))
                (let ((char (char-after)))
                  (setq columns (+ columns (if (= char ?\t) (- 8 (% columns 8)) (char-width char))))
                  (forward-char)
                  (when (or (memq char '(?\s ?\t)) (> (char-width char) 1))
                    (setq boundary (point)))))
              (when (and (< (point) end) boundary
                         (not (memq (char-after) '(?\s ?\t))))
                (goto-char boundary))
              (let ((next (point)))
                (skip-chars-backward spaces start)
                (push (cons start (point)) ranges)
                (goto-char next))
              (skip-chars-forward spaces end))))
        (forward-line 1))
      (nreverse ranges))))

(defun emacs-reader-translate-view--texts ()
  "Return plain source strings for the current buffer's reading lines."
  (mapcar (lambda (bounds)
            (buffer-substring-no-properties (car bounds) (cdr bounds)))
          (emacs-reader-translate-view--ranges)))

(defun emacs-reader-translate-view-split (text &optional width)
  "Return source lines from formatted chapter TEXT using optional display WIDTH."
  (with-temp-buffer
    (insert text)
    (let ((emacs-reader-translation-line-width (or width emacs-reader-translation-line-width)))
      (emacs-reader-translate-view--texts))))

(defun emacs-reader-translate-view-paragraphs ()
  "Return the complete chapter's source lines in a reading buffer."
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
           (let* ((end (save-excursion
                         (goto-char (cdr bounds))
                         (skip-chars-forward " \t\r　 " (line-end-position))
                         (point)))
                  (wrap (and (< end (point-max)) (/= (char-after end) ?\n)))
                  (overlay (make-overlay (1- end) end)))
             (overlay-put overlay 'evaporate t)
             (overlay-put overlay 'after-string
                          (propertize (concat "\n" (substring-no-properties text) (if wrap "\n" ""))
                                      'face 'emacs-reader-translate-view-face))
             (push overlay emacs-reader-translate-view--overlays))))
       (emacs-reader-translate-view--ranges) translations))))

(provide 'emacs-reader-translate-view)
;;; emacs-reader-translate-view.el ends here
