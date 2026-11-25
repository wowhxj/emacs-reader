;;; emacs-reader-tts-rate-test.el --- Speech speed controls -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Verify state, adjustment, prefetch invalidation, and display.

;;; Code:
(require 'ert)
(require 'emacs-reader)

(ert-deftest emacs-reader-tts-rate-keys-and-buffer-isolation ()
  (with-temp-buffer
    (emacs-reader-reading-mode)
    (should (eq (key-binding ">") #'emacs-reader-tts-faster))
    (should (eq (key-binding "<") #'emacs-reader-tts-slower))
    (let ((initial (default-value 'emacs-reader-tts-rate)))
      (setq-local emacs-reader-tts-rate 1.0)
      (emacs-reader-tts-faster)
      (should (= 1.1 emacs-reader-tts-rate))
      (should (= initial (default-value 'emacs-reader-tts-rate)))
      (setq-local emacs-reader-tts-rate 2.0)
      (emacs-reader-tts-faster)
      (should (= 2.0 emacs-reader-tts-rate)))))

(ert-deftest emacs-reader-tts-rate-restarts-current-paragraph ()
  (with-temp-buffer
    (insert "First paragraph.\n\nNext paragraph.")
    (emacs-reader-reading-mode)
    (setq-local emacs-reader--current-book '((name . "Book"))
                emacs-reader--chapters [((title . "Chapter"))]
                emacs-reader--current-chapter-index 0
                emacs-reader--requested-chapter-index 0
                emacs-reader--tts-position 4
                emacs-reader--tts-active t
                emacs-reader-tts-rate 1.0
                emacs-reader--tts-prefetch '(:start 20 :ready t))
    (let ((generation emacs-reader--tts-generation) read-at)
      (cl-letf (((symbol-function 'emacs-reader--tts-read-paragraph)
                 (lambda (position) (setq read-at position))))
        (emacs-reader-tts-slower)
        (should (= 0.9 emacs-reader-tts-rate))
        (should emacs-reader--tts-active)
        (should (= 4 read-at))
        (should (> emacs-reader--tts-generation generation))
        (should-not emacs-reader--tts-prefetch)
        (should (string-match-p "朗读 0.9×" header-line-format))
        (emacs-reader--tts-stop)
        (should-not (string-match-p "朗读" header-line-format))))))

(provide 'emacs-reader-tts-rate-test)
;;; emacs-reader-tts-rate-test.el ends here
