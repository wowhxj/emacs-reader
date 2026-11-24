;;; emacs-reader-picker-test.el --- Source picker keyboard tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise the real minibuffer command loop with and without Vertico.

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'emacs-reader-local)

(defconst emacs-reader-picker-test--records
  '(((name . "Solo") (type . "legado") (url . "one"))
    ((name . "Library") (type . "opds") (url . "two"))
    ((name . "Open") (type . "json") (url . "three"))
    ((name . "Off") (type . "legado") (enabled . :false))
    ((name . "Also off") (type . "legado") (definition (enabled . :false)))))

(defun emacs-reader-picker-test--read (keys &optional single)
  "Select with KEYS, restricting to a SINGLE source if requested."
  (ert-simulate-keys keys
    (emacs-reader-local--select '("legado" "opds" "json") (not single))))

(defun emacs-reader-picker-test--exercise ()
  "Exercise narrowing, widening and ordinary text through the active frontend."
  (cl-letf (((symbol-function 'emacs-reader-local--records)
             (lambda (_) emacs-reader-picker-test--records)))
    (let* ((minibuffer-history '("Library [opds] <two>" "Solo [legado] <one>"))
           (legado (nth 0 emacs-reader-picker-test--records))
           (opds (nth 1 emacs-reader-picker-test--records))
           (json (nth 2 emacs-reader-picker-test--records))
           (all `((type . "all") (sources ,legado ,opds ,json))))
      (should (equal (emacs-reader-picker-test--read (kbd "RET")) all))
      (should (equal (emacs-reader-picker-test--read (kbd "l RET"))
                     `((type . "all") (sources ,legado))))
      (should (equal (emacs-reader-picker-test--read (kbd "o RET"))
                     `((type . "all") (sources ,opds))))
      (should (equal (emacs-reader-picker-test--read (kbd "l DEL RET")) all))
      ;; Native completion needs TAB to expand a unique source before RET.
      (should (equal (emacs-reader-picker-test--read (kbd "S o l o TAB RET")) legado))
      (let ((completion-ignore-case t))
        (should (equal (emacs-reader-picker-test--read (kbd "C-q l i b TAB RET")) opds))
        (should (equal (emacs-reader-picker-test--read (kbd "C-q o p e n TAB RET")) json))
        (should (equal (emacs-reader-picker-test--read (kbd "o p e n TAB RET") t) json))))))

(ert-deftest emacs-reader-picker-native-keys ()
  (let ((completing-read-function #'completing-read-default))
    (emacs-reader-picker-test--exercise)))

(declare-function vertico-mode "ext:vertico")
(ert-deftest emacs-reader-picker-vertico-keys ()
  (skip-unless (require 'vertico nil t))
  (let* ((active (bound-and-true-p vertico-mode))
         (used-vertico nil)
         (pre-command-hook (cons (lambda () (when (eq this-command 'vertico-exit)
                                              (setq used-vertico t)))
                                 pre-command-hook)))
    (unwind-protect
        (progn (vertico-mode 1) (emacs-reader-picker-test--exercise) (should used-vertico))
      (unless active (vertico-mode -1)))))

(ert-deftest emacs-reader-picker-no-empty-all ()
  (cl-letf (((symbol-function 'emacs-reader-local--records) #'ignore))
    (should-error (emacs-reader-picker-test--read (kbd "RET")) :type 'user-error))
  (cl-letf (((symbol-function 'emacs-reader-local--records)
             (lambda (_) (list (car emacs-reader-picker-test--records)))))
    (should-error (emacs-reader-picker-test--read (kbd "o RET")) :type 'user-error)))

(provide 'emacs-reader-picker-test)
;;; emacs-reader-picker-test.el ends here
