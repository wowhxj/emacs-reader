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
      (should (equal (mapcar #'emacs-reader-opds--item-pagination items) '(t nil)))
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
        (bodies '("{\"numFound\":25,\"docs\":[{\"title\":\"Book\",\"key\":\"/works/A\",
\"ebook_access\":\"public\",\"author_name\":[\"A\"],\"ia\":[\"id\"]},
{\"title\":\"Print\",\"key\":\"/works/B\"}, {\"title\":\"Loan\",\"key\":\"/works/C\",
\"ebook_access\":\"borrowable\",\"ia\":[\"restricted\"]}]}"
                  "{\"numFound\":25,\"docs\":[{\"title\":\"Final\",\"key\":\"/works/D\"}]}"
                  "{\"response\":{\"numFound\":1,\"docs\":[{\"identifier\":\"id\"}]}}"
                  "{\"metadata\":{\"identifier\":\"id\"},\"files\":[{\"name\":\"b.txt\"}]}"
                  "Book body"))
        calls items reason imported visited)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _)
                 (push url calls)
                 (emacs-reader-catalog-test--respond (pop bodies) callback)))
              ((symbol-function 'browse-url) (lambda (url &rest _) (push url visited)))
              ((symbol-function 'emacs-reader-import-file)
               (lambda (_ title author origin) (setq imported (list title author origin)))))
      (emacs-reader-catalog-results source "needle" (lambda (i e) (setq items i reason e)))
      (should-not reason)
      (should (= (length calls) 1))
      (should (string-match-p "q=needle.*page=1" (car calls)))
      (should-not (string-search "ebook_access%3Apublic" (car calls)))
      (should (equal (mapcar #'emacs-reader-opds--item-label items)
                     '("Book — A" "Print" "Loan" "Next page")))
      (should (equal (mapcar #'emacs-reader-opds--item-kind items)
                     '(record record record json-page)))
      (should (equal (mapcar #'emacs-reader-opds--item-pagination items) '(nil nil nil t)))
      (dolist (i '(1 2)) (emacs-reader-catalog-open-item source (nth i items)))
      (should (equal visited
                     '("https://openlibrary.org/works/C" "https://openlibrary.org/works/B")))
      (should (= (length calls) 1))
      (save-window-excursion
        (emacs-reader-catalog-open-item source (nth 3 items))
        (unwind-protect
            (progn
              (should (string-match-p "q=needle.*page=2" (car calls)))
              (should (string-search "第 2 页 · 本页 1 条，共 25 条" (buffer-string))))
          (kill-buffer (current-buffer)))
        (emacs-reader-catalog-open-item source (car items))
        (unwind-protect
            (progn
              (let ((choices (cdar emacs-reader-opds--history)))
                (emacs-reader-opds--activate (car choices))
                (should (equal (car visited) "https://openlibrary.org/works/A"))
                (emacs-reader-opds--activate (cadr choices)))
              (should (string-match-p "archive.org/advancedsearch" (car calls)))
              (emacs-reader-opds--activate (car (cdar emacs-reader-opds--history)))
              (should (equal (car calls) "https://archive.org/metadata/id"))
              (emacs-reader-opds--activate (car (cdar emacs-reader-opds--history)))
              (should (equal (car calls) "https://archive.org/download/id/b.txt"))
              (should (equal imported '("Book" "A" "Open Library"))))
          (kill-buffer (current-buffer)))))))

(defconst emacs-reader-catalog-test--json
  '((type . "json") (name . "Paged") (url . "https://books.invalid/?q={query}&page={page}")
    (definition . ((rows . "docs") (label . "title") (recordHref . "{key}")
                   (pageSize . 1) (total . "count")))))

(defun emacs-reader-catalog-test--doc (key total)
  "Build a numbered search page with book KEY and catalog TOTAL."
  (format "{\"count\":%d,\"docs\":[{\"title\":\"%s\",\"key\":\"/%s\"}]}" total key key))

(ert-deftest emacs-reader-catalog-pages-follow-only-root-next ()
  (let* ((home emacs-reader-catalog-test--root)
         (first (concat home "?q=A")) (second (concat home "two"))
         (fixtures `((,home . "<feed><link rel='search' href='?q={searchTerms}'/></feed>")
                     (,first . "<feed><link rel='next' href='two'/><entry><title>A</title>
<link rel='next' type='application/atom+xml' href='versions'/><link href='a.txt'
rel='http://opds-spec.org/acquisition'/></entry></feed>")
                     (,second . "{\"links\":[{\"rel\":\"next\",\"href\":\"/catalog/?q=A\"}],
\"publications\":[{\"metadata\":{\"title\":\"A\"},\"links\":[{\"href\":\"a.txt\",
\"rel\":\"http://opds-spec.org/acquisition\"},{\"href\":\"b.txt\",
\"rel\":\"http://opds-spec.org/acquisition\"}]}]}")))
         calls pages replies next)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _)
                 (push url calls)
                 (emacs-reader-catalog-test--respond (cdr (assoc url fixtures)) callback)))
              ((symbol-function 'run-at-time)
               (lambda (delay _ fn &rest args)
                 (when (= delay emacs-reader-catalog--page-delay)
                   (setq next (lambda () (apply fn args))))
                 (timer-create))))
      (emacs-reader-catalog-search-pages
       emacs-reader-catalog-test--opds "A"
       (lambda (items) (push (mapcar #'emacs-reader-opds--item-url items) pages))
       (lambda (reason) (push reason replies)))
      (should (equal calls (list first home)))
      (funcall next)
      (should (equal calls (list second first home)))
      (should (equal pages (list (list (concat home "b.txt"))
                                 (list (concat home "versions") (concat home "a.txt")))))
      (should (equal replies '(nil))))))

(ert-deftest emacs-reader-catalog-pages-repeat-and-late-failure ()
  (dolist (second '(repeated failure))
    (let (next calls pages replies)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url callback &rest _)
                   (push url calls)
                   (if (and (eq second 'failure) (cdr calls)) (error "Offline")
                     (emacs-reader-catalog-test--respond
                      (emacs-reader-catalog-test--doc "A" 99999) callback))))
                ((symbol-function 'run-at-time)
                 (lambda (delay _ fn &rest args)
                   (when (= delay emacs-reader-catalog--page-delay)
                     (setq next (lambda () (apply fn args))))
                   (timer-create))))
        (emacs-reader-catalog-search-pages
         emacs-reader-catalog-test--json "needle"
         (lambda (items) (push (mapcar #'emacs-reader-opds--item-label items) pages))
         (lambda (reason) (push reason replies)))
        (funcall next)
        (funcall next)
        (should (= 2 (length calls)))
        (should (equal (apply #'append pages) '("A")))
        (should (= (length replies) 1))
        (should (string-match-p (if (eq second 'failure) "Offline" "重复") (car replies)))))))

(ert-deftest emacs-reader-catalog-single-source-preserves-window-and-stops ()
  (save-window-excursion
    (let (next calls late response books)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url callback &rest _)
                   (push url calls)
                   (if (string-suffix-p "page=3" url)
                       (progn (setq late callback)
                              (setq response (generate-new-buffer " *catalog pending*")))
                     (emacs-reader-catalog-test--respond
                      (emacs-reader-catalog-test--doc (number-to-string (length calls)) 3)
                      callback))))
                ((symbol-function 'run-at-time)
                 (lambda (delay _ fn &rest args)
                   (when (= delay emacs-reader-catalog--page-delay)
                     (setq next (lambda () (apply fn args))))
                   (timer-create))))
        (unwind-protect
            (progn
              (emacs-reader-json-catalog-search emacs-reader-catalog-test--json "first")
              (push (current-buffer) books)
              (goto-char (button-start (next-button (point-min))))
              (let* ((win (selected-window)) (pos (point)) (start (window-start win))
                     (other (split-window)))
                (select-window other)
                (switch-to-buffer (get-buffer-create "*scratch*"))
                (funcall next)
                (should (eq other (selected-window)))
                (should (eq (window-buffer win) (car books)))
                (should (= pos (window-point win)))
                (should (= start (window-start win)))
                (with-current-buffer (car books)
                  (should (string-search "2 条" header-line-format))))
              (funcall next)
              (should (buffer-live-p response))
              (emacs-reader-json-catalog-search emacs-reader-catalog-test--json "second")
              (push (current-buffer) books)
              (should-not (buffer-live-p response))
              (emacs-reader-catalog-test--respond (emacs-reader-catalog-test--doc "late" 4) late)
              (with-current-buffer (cadr books)
                (should (= 2 (length (cdar emacs-reader-opds--history)))))
              (quit-window)
              (funcall next)
              (should (= 4 (length calls))))
          (dolist (buffer books) (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(provide 'emacs-reader-catalog-search-test)
;;; emacs-reader-catalog-search-test.el ends here
