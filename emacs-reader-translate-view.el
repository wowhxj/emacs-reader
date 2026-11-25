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

(defvar-local emacs-reader-translate-view--line-ranges nil
  "Vector of source bounds indexed by zero-based reading line.")
(defvar-local emacs-reader-translate-view--line-overlays nil
  "Vector of translation overlays indexed by reading line.")
(defvar-local emacs-reader-translate-view--ranges-key nil
  "Character modification tick and wrapping width used for cached bounds.")

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

(defun emacs-reader-translate-view--texts (ranges)
  "Return plain source strings for RANGES in the current buffer."
  (mapcar (lambda (bounds)
            (buffer-substring-no-properties (car bounds) (cdr bounds)))
          ranges))

(defun emacs-reader-translate-view-split (text &optional width)
  "Return source lines from formatted chapter TEXT using optional display WIDTH."
  (with-temp-buffer
    (insert text)
    (let ((emacs-reader-translation-line-width (or width emacs-reader-translation-line-width)))
      (emacs-reader-translate-view--texts (emacs-reader-translate-view--ranges)))))

(defun emacs-reader-translate-view--cached-ranges ()
  "Return cached source bounds for the widened reading buffer."
  (let ((key (cons (buffer-chars-modified-tick) emacs-reader-translation-line-width)))
    (unless (equal key emacs-reader-translate-view--ranges-key)
      (emacs-reader-translate-view-clear)
      (setq emacs-reader-translate-view--line-ranges (vconcat (emacs-reader-translate-view--ranges))
            emacs-reader-translate-view--line-overlays
            (make-vector (length emacs-reader-translate-view--line-ranges) nil)
            emacs-reader-translate-view--ranges-key key))
    emacs-reader-translate-view--line-ranges))

(defun emacs-reader-translate-view-paragraphs ()
  "Return the complete chapter's source lines in a reading buffer."
  (when (derived-mode-p 'emacs-reader-reading-mode)
    (save-restriction
      (widen)
      (emacs-reader-translate-view--texts (emacs-reader-translate-view--cached-ranges)))))

(defun emacs-reader-translate-view-clear ()
  "Remove this module's translation overlays from the current buffer."
  (mapc #'delete-overlay emacs-reader-translate-view--overlays)
  (setq emacs-reader-translate-view--overlays nil
        emacs-reader-translate-view--line-overlays nil
        emacs-reader-translate-view--line-ranges nil
        emacs-reader-translate-view--ranges-key nil))

(defun emacs-reader-translate-view-append (start translations)
  "Display TRANSLATIONS starting at zero-based reading line START.
Accept a list or vector.  Skip blank and excess entries.  Overlapping
batches update the existing overlay for each translated line."
  (when (derived-mode-p 'emacs-reader-reading-mode)
    (cl-check-type start natnum)
    (save-restriction
      (widen)
      (add-hook 'change-major-mode-hook #'emacs-reader-translate-view-clear nil t)
      (let ((ranges (emacs-reader-translate-view--cached-ranges)))
        (save-excursion
          (mapc
           (lambda (text)
             (when (and (< start (length ranges)) (stringp text)
                        (string-match-p "[^ \t\r\n　 ]" text))
               (goto-char (cdr (aref ranges start)))
               (skip-chars-forward " \t\r　 ")
               (let ((overlay (aref emacs-reader-translate-view--line-overlays start))
                     (wrap (and (char-after) (/= (char-after) ?\n))))
                 (unless overlay
                   ;; Anchor on a real character so erasing the chapter removes it.
                   (setq overlay (make-overlay (1- (point)) (point)))
                   (overlay-put overlay 'evaporate t)
                   (push overlay emacs-reader-translate-view--overlays)
                   (aset emacs-reader-translate-view--line-overlays start overlay))
                 (overlay-put overlay 'after-string
                              (propertize (concat "\n" (substring-no-properties text)
                                                  (if wrap "\n" ""))
                                          'face 'emacs-reader-translate-view-face))))
             (setq start (1+ start)))
           translations))))))

(defun emacs-reader-translate-view-show (translations)
  "Replace the reading buffer's translations with TRANSLATIONS in order.
Accept a list or vector.  Nil and blank entries leave their paragraph bare.
Missing or excess entries are ignored."
  (when (derived-mode-p 'emacs-reader-reading-mode)
    (emacs-reader-translate-view-clear)
    (emacs-reader-translate-view-append 0 translations)))

(provide 'emacs-reader-translate-view)
;;; emacs-reader-translate-view.el ends here
