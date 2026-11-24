;;; emacs-reader-catalog-search-test.el --- Catalog result tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Network fixtures exercise catalog discovery, callbacks and actions.

;;; Code:

(require 'ert)
(require 'emacs-reader-opds)

(defconst emacs-reader-catalog-test--root "https://books.invalid/catalog/")
(defconst emacs-reader-catalog-test--opds
  `((type . "opds") (name . "Fixture") (url . ,emacs-reader-catalog-test--root)))
(defconst emacs-reader-catalog-test--open-library
  (expand-file-name "../examples/open-library.json"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-reader-catalog-test--respond (body callback)
  "Return a successful HTTP response with BODY to CALLBACK."
  (with-current-buffer (generate-new-buffer " *catalog test HTTP*")
    (set-buffer-multibyte nil)
    (setq-local url-http-response-status 200)
    (insert "HTTP/1.1 200 OK\r\n\r\n" (encode-coding-string body 'utf-8))
    (funcall callback nil)))

(ert-deftest emacs-reader-catalog-search-opds-discovery ()
  (dolist (description '(nil t))
    (let* ((home emacs-reader-catalog-test--root)
           (search (concat home "search.xml"))
           (results (concat home "find?q=%E4%B9%A6%20A"))
           (responses
            `((,home . ,(concat "<feed><link rel='search' type='text/html' href='web'/>"
                                "<link rel='search' href='"
                                (if description "search.xml" "find?q={searchTerms}")
                                "'/><link rel='next' href='browse'/></feed>"))
              (,search . "<OpenSearchDescription><Url type='application/atom+xml'
 template='find?q={searchTerms}'/></OpenSearchDescription>")
              (,results . "{\"metadata\":{\"title\":\"Results\"},\"links\":[
{\"rel\":\"search\",\"href\":\"search.xml\"},{\"rel\":\"next\",\"href\":\"next\"},
{\"rel\":\"start\",\"type\":\"application/opds+json\",\"href\":\"/\"}],\"publications\":[
{\"metadata\":{\"title\":\"书\"},\"links\":[{\"rel\":\"http://opds-spec.org/acquisition\",
\"href\":\"book.txt\"}]}]}")))
           (window (selected-window)) (buffer (window-buffer window))
           calls items error-string)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url callback &rest _)
                   (push url calls)
                   (emacs-reader-catalog-test--respond (cdr (assoc url responses)) callback))))
        (emacs-reader-catalog-results emacs-reader-catalog-test--opds "书 A"
                                      (lambda (rows reason) (setq items rows error-string reason))))
      (should-not error-string)
      (should (equal (reverse calls) (if description (list home search results)
                                       (list home results))))
      (should (equal (mapcar #'emacs-reader-opds--item-kind items) '(feed download)))
      (should (equal (emacs-reader-opds--item-url (cadr items)) (concat home "book.txt")))
      (should (eq window (selected-window)))
      (should (eq buffer (window-buffer window))))))

(ert-deftest emacs-reader-catalog-search-errors ()
  (dolist (fixture '("<feed><link rel='next' href='browse'/></feed>"
                     "<feed><link rel='search' href='find?unused={count?}'/></feed>"
                     offline))
    (let (calls replies)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url callback &rest _)
                   (push url calls)
                   (if (eq fixture 'offline) (error "Offline")
                     (emacs-reader-catalog-test--respond fixture callback)))))
        (emacs-reader-catalog-results emacs-reader-catalog-test--opds "A"
                                      (lambda (&rest args) (push args replies))))
      (should (equal calls (list emacs-reader-catalog-test--root)))
      (should (= 1 (length replies)))
      (should-not (caar replies))
      (should (stringp (cadar replies))))))

(ert-deftest emacs-reader-catalog-search-timeout-ignores-late-response ()
  (let (timeout reply replies)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_ _ fn &rest args) (setq timeout (lambda () (apply fn args))) 'timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'url-retrieve)
               (lambda (_ callback &rest _) (setq reply callback))))
      (emacs-reader-catalog-results emacs-reader-catalog-test--opds "book"
                                    (lambda (&rest args) (push args replies)))
      (funcall timeout)
      (emacs-reader-catalog-test--respond "<feed/>" reply)
      (should (= (length replies) 1))
      (should-not (caar replies))
      (should (string-match-p "超时" (cadar replies))))))

(ert-deftest emacs-reader-catalog-search-open-library-actions ()
  (let ((source (with-temp-buffer (insert-file-contents emacs-reader-catalog-test--open-library)
                                  (emacs-reader-opds--json)))
        (bodies '("{\"numFound\":21,\"docs\":[{\"title\":\"Book\",
\"author_name\":[\"A\"],\"ia\":[\"id\"]}]}"
                  "{\"numFound\":21,\"docs\":[]}"
                  "{\"response\":{\"numFound\":1,\"docs\":[{\"identifier\":\"id\"}]}}"
                  "{\"metadata\":{\"identifier\":\"id\"},\"files\":[{\"name\":\"b.txt\"}]}"
                  "Book body"))
        calls items reason imported)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _)
                 (push url calls)
                 (emacs-reader-catalog-test--respond (pop bodies) callback)))
              ((symbol-function 'emacs-reader-import-file)
               (lambda (_ title author origin) (setq imported (list title author origin)))))
      (emacs-reader-catalog-results source "needle" (lambda (i e) (setq items i reason e)))
      (should-not reason)
      (should (= (length calls) 1))
      (should (string-match-p "q=needle.*page=1" (car calls)))
      (should (equal (mapcar #'emacs-reader-opds--item-label items) '("Book — A" "Next page")))
      (save-window-excursion
        (emacs-reader-catalog-open-item source (cadr items))
        (unwind-protect (should (string-match-p "q=needle.*page=2" (car calls)))
          (kill-buffer (current-buffer)))
        (emacs-reader-catalog-open-item source (car items))
        (unwind-protect
            (progn
              (should (string-match-p "archive.org/advancedsearch" (car calls)))
              (emacs-reader-opds--activate (car (cdar emacs-reader-opds--history)))
              (should (equal (car calls) "https://archive.org/metadata/id"))
              (emacs-reader-opds--activate (car (cdar emacs-reader-opds--history)))
              (should (equal (car calls) "https://archive.org/download/id/b.txt"))
              (should (equal imported '("Book" "A" "Open Library"))))
          (kill-buffer (current-buffer)))))))

(provide 'emacs-reader-catalog-search-test)
;;; emacs-reader-catalog-search-test.el ends here
