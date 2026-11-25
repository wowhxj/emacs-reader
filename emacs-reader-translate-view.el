;;; emacs-reader-translate-view.el --- Display line translations -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Project translations into a reading buffer using display-only overlays.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

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
  "Character modification tick of the sampled chapter.")

(defcustom emacs-reader-translation-line-width 60
  "Fallback source line width when no reader window can be measured."
  :type 'natnum :group 'emacs-reader)

(defun emacs-reader-translate-view-layout-window ()
  "Return a visible window showing the current reader, preferring the selected one."
  (when (derived-mode-p 'emacs-reader-reading-mode)
    (get-buffer-window (current-buffer) 'visible)))

(defun emacs-reader-translate-view--fallback-ranges ()
  "Return bounds of nonblank source lines using the fallback column width."
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
              (skip-chars-forward spaces end)
              (push (cons start (point)) ranges))))
        (forward-line 1))
      (nreverse ranges))))

(defun emacs-reader-translate-view--ranges (&optional window)
  "Sample nonblank visual lines in WINDOW, retaining their exact source ends."
  (if (not window) (emacs-reader-translate-view--fallback-ranges)
    (save-excursion
      (goto-char (point-min))
      (let (ranges)
        (while (< (point) (point-max))
          (let ((start (point)))
            (vertical-motion 1 window)
            ;; At EOB, or for glyphs spanning rows, motion can stall.
            (when (<= (point) start) (forward-line 1))
            (let ((end (if (eq (char-before) ?\n) (1- (point)) (point))))
              (when (save-excursion
                      (goto-char start)
                      (re-search-forward "[^ \t\r　 \n]" end t))
                (push (cons start end) ranges)))))
        (nreverse ranges)))))

(defun emacs-reader-translate-view--texts (ranges)
  "Return trimmed source strings for RANGES, keeping display bounds intact."
  (mapcar (lambda (bounds)
            (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))
                         "[ \t\r　 ]+" "[ \t\r　 ]+"))
          ranges))

(defun emacs-reader-translate-view-split (text &optional width window)
  "Return source lines for formatted chapter TEXT measured in reader WINDOW.
Use WIDTH or `emacs-reader-translation-line-width' when WINDOW is absent."
  (let ((reader (and window (window-live-p window) (window-buffer window)))
        (fallback (or width emacs-reader-translation-line-width)))
    (with-temp-buffer
      (insert text)
      (when reader
        (dolist (var
                 '(word-wrap word-wrap-by-category truncate-lines truncate-partial-width-windows
                             face-remapping-alist tab-width nobreak-char-display
                             buffer-display-table display-line-numbers line-prefix wrap-prefix
                             bidi-display-reordering bidi-paragraph-direction))
          (when (boundp var)
            (set (make-local-variable var) (buffer-local-value var reader))))
        (set-category-table (with-current-buffer reader (category-table)))
        (set-syntax-table (with-current-buffer reader (syntax-table))))
      (let ((emacs-reader-translation-line-width fallback))
        (emacs-reader-translate-view--texts
         (emacs-reader-translate-view--ranges (and reader window)))))))

(defun emacs-reader-translate-view--cached-ranges ()
  "Return cached source bounds for the widened reading buffer."
  (let ((key (buffer-chars-modified-tick)))
    (unless (equal key emacs-reader-translate-view--ranges-key)
      (emacs-reader-translate-view-clear)
      (setq emacs-reader-translate-view--line-ranges
            (vconcat (emacs-reader-translate-view--ranges
                      (emacs-reader-translate-view-layout-window)))
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
    (mapc #'delete-overlay emacs-reader-translate-view--overlays)
    (setq emacs-reader-translate-view--overlays nil)
    (when emacs-reader-translate-view--line-overlays
      (fillarray emacs-reader-translate-view--line-overlays nil))
    (emacs-reader-translate-view-append 0 translations)))

(provide 'emacs-reader-translate-view)
;;; emacs-reader-translate-view.el ends here
