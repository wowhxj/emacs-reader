;;; emacs-reader-search-progress.el --- Search activity in the mode line -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Show progress in the results buffer, including with custom mode lines
;; that do not display `mode-line-process'.

;;; Code:

(defconst emacs-reader-search-progress--frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
(defvar-local emacs-reader-search-progress--timer nil)
(defvar-local emacs-reader-search-progress--frame 0)
(defvar-local emacs-reader-search-progress--text nil)
(defvar-local emacs-reader-search-progress--format nil)
(defvar-local emacs-reader-search-progress--was-local nil)

(defun emacs-reader-search-progress--tick (buffer)
  "Show the next search frame in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq emacs-reader-search-progress--text
            (concat " " (aref emacs-reader-search-progress--frames
                              emacs-reader-search-progress--frame) " 搜索中 ")
            emacs-reader-search-progress--frame
            (mod (1+ emacs-reader-search-progress--frame)
                 (length emacs-reader-search-progress--frames)))
      (force-mode-line-update))))

(defun emacs-reader-search-progress-stop ()
  "Remove the search indicator and its timer from this buffer."
  (when emacs-reader-search-progress--timer
    (cancel-timer emacs-reader-search-progress--timer)
    (when (eq mode-line-format emacs-reader-search-progress--format)
      (if emacs-reader-search-progress--was-local
          (setq mode-line-format (nth 2 emacs-reader-search-progress--format))
        (kill-local-variable 'mode-line-format)))
    (setq emacs-reader-search-progress--timer nil
          emacs-reader-search-progress--format nil
          emacs-reader-search-progress--text nil)
    (remove-hook 'kill-buffer-hook #'emacs-reader-search-progress-stop t)
    (remove-hook 'change-major-mode-hook #'emacs-reader-search-progress-stop t)
    (force-mode-line-update)))

(defun emacs-reader-search-progress-start ()
  "Animate the current buffer's mode line until search finishes."
  (unless emacs-reader-search-progress--timer
    (setq emacs-reader-search-progress--was-local (local-variable-p 'mode-line-format)
          emacs-reader-search-progress--format
          (list "" 'emacs-reader-search-progress--text mode-line-format)
          emacs-reader-search-progress--frame 0)
    (when mode-line-format (setq-local mode-line-format emacs-reader-search-progress--format))
    (add-hook 'kill-buffer-hook #'emacs-reader-search-progress-stop nil t)
    (add-hook 'change-major-mode-hook #'emacs-reader-search-progress-stop nil t)
    (emacs-reader-search-progress--tick (current-buffer))
    (setq emacs-reader-search-progress--timer
          (run-at-time 0.18 0.18 #'emacs-reader-search-progress--tick (current-buffer)))))

(provide 'emacs-reader-search-progress)
;;; emacs-reader-search-progress.el ends here
