;;; emacs-reader-gutenberg-test.el --- Gutenberg catalog tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercise the official CSV adapter and SQLite cache through the catalog API.

;;; Code:

(require 'ert)
(require 'emacs-reader-opds)

(defconst emacs-reader-gutenberg-test--preset
  (expand-file-name "../examples/gutenberg.json"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-reader-gutenberg-test--source ()
  "Read the shipped Gutenberg preset."
  (with-temp-buffer
    (insert-file-contents emacs-reader-gutenberg-test--preset)
    (emacs-reader-opds--json)))

(defun emacs-reader-gutenberg-test--wait (ready)
  "Run the process loop until READY, or fail on timeout."
  (let ((deadline (+ (float-time) 12)))
    (while (and (not (funcall ready)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall ready))))

(ert-deftest emacs-reader-gutenberg-search-and-shared-cache ()
  (let* ((emacs-reader-gutenberg-cache-directory (make-temp-file "reader-pg-test-" t))
         (source (emacs-reader-gutenberg-test--source))
         (body (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert (encode-coding-string
                          (concat "Text#,Type,Title,Authors\r\n24264,Text,紅樓夢,曹雪芹\r\n"
                                  "23962,Text,西遊記,吳承恩\r\n"
                                  "1342,Text,\"Pride and\r\nPrejudice\",\"Austen, Jane\"\r\n"
                                  "1,Sound,紅樓夢,Audio\r\n") 'utf-8))
                 (should (zerop (call-process-region
                                 nil nil "python3" t t nil "-I" "-c"
                                 (concat "import gzip,sys; "
                                         "sys.stdout.buffer.write(gzip.compress("
                                         "sys.stdin.buffer.read()))"))))
                 (buffer-string)))
         (calls 0) results errors
         (server (make-network-process
                  :name "gutenberg-fixture" :server t :family 'ipv4 :host "127.0.0.1"
                  :service t :coding 'binary :noquery t
                  :filter (lambda (client chunk)
                            (let ((request (concat (process-get client 'request) chunk)))
                              (process-put client 'request request)
                              (when (string-match-p "\r\n\r\n" request)
                                (cl-incf calls)
                                (process-send-string
                                 client (concat "HTTP/1.1 200 OK\r\nConnection: close\r\n"
                                                (format "Content-Length: %d\r\n\r\n" (length body))
                                                body))
                                (process-send-eof client)))))))
    (unwind-protect
        (progn
          (setf (alist-get 'url source)
                (format "http://127.0.0.1:%d/books.gz#q={query}" (process-contact server :service)))
          (dolist (term '("红楼梦" "西游记"))
            (emacs-reader-catalog-results
             source term (lambda (rows err) (push rows results) (push err errors))))
          (emacs-reader-gutenberg-test--wait (lambda () (= (length results) 2)))
          (should (equal errors '(nil nil)))
          (should (equal (sort (mapcar (lambda (rows)
                                         (should (= 1 (length rows)))
                                         (emacs-reader-opds--item-title (car rows))) results)
                               #'string<) '("紅樓夢" "西遊記")))
          (should (= calls 1))
          (delete-process server)
          (let (page finished)
            (emacs-reader-catalog-search-pages source "prejudice AUSTEN"
                                               (lambda (items) (setq page items))
                                               (lambda (err) (push err finished)))
            (emacs-reader-gutenberg-test--wait (lambda () finished))
            (should (equal finished '(nil)))
            (let ((item (car page)))
              (should (eq 'download (emacs-reader-opds--item-kind item)))
              (should (equal (emacs-reader-opds--item-title item) "Pride and Prejudice"))
              (should (equal (emacs-reader-opds--item-author item) "Austen, Jane"))
              (should (equal (emacs-reader-opds--item-url item)
                             "https://gutenberg.pglaf.org/cache/epub/1342/pg1342-images-3.epub")))))
      (when (process-live-p server) (delete-process server))
      (delete-directory emacs-reader-gutenberg-cache-directory t))))

(ert-deftest emacs-reader-gutenberg-rejects-local-catalog ()
  (let ((source (emacs-reader-gutenberg-test--source)) done)
    (setf (alist-get 'url source) "file:///tmp/catalog.gz#q={query}")
    (emacs-reader-catalog-results source "西游记" (lambda (items why) (setq done (list items why))))
    (should-not (car done))
    (should (string-search "HTTP(S)" (cadr done)))))

(provide 'emacs-reader-gutenberg-test)
;;; emacs-reader-gutenberg-test.el ends here
