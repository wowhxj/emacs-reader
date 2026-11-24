;;; emacs-reader-files-test.el --- Local book tests -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; Exercises the file backend directly, with a small ZIP fixture.

;;; Code:

(require 'ert)
(require 'emacs-reader-files)

(ert-deftest emacs-reader-files-test-txt-sections ()
  (let* ((text "引言。\n\n第一章 初见\n你好。\n\nChapter 2\nTwo.\n")
         (path (make-temp-file "reader" nil ".TXT" text)))
    (unwind-protect
        (let ((chapters (emacs-reader-files-chapters path)))
          (should (equal (mapcar (lambda (c) (plist-get c :title)) chapters)
                         (list (file-name-base path) "第一章 初见" "Chapter 2")))
          (should (equal text (mapconcat (lambda (c) (emacs-reader-files-content path c))
                                         chapters "")))
          (should-error (emacs-reader-files-content path "txt:1:999")))
      (delete-file path))))

(ert-deftest emacs-reader-files-test-txt-single ()
  (let ((path (make-temp-file "reader" nil ".txt" "One\n\nTwo")))
    (unwind-protect
        (let ((chapters (emacs-reader-files-chapters path)))
          (should (= 1 (length chapters)))
          (should (equal "One\n\nTwo" (emacs-reader-files-content path (car chapters)))))
      (delete-file path))))

(defun emacs-reader-files-test--epub (path)
  "Write a small complete EPUB archive to PATH."
  (let ((entries
         '(("mimetype" . "application/epub+zip")
           ("META-INF/container.xml" . "<container><rootfiles><rootfile full-path='OPS/book.opf'/>
</rootfiles></container>")
           ("OPS/book.opf" . "<package xmlns:dc='urn:test'><metadata>
<dc:title>试读</dc:title><dc:creator>Author</dc:creator></metadata><manifest>
<item id='a' href='../Text/one.xhtml' media-type='application/xhtml+xml'/>
<item id='b' href='../Text/%E4%B9%A6%20two.xhtml' media-type='application/xhtml+xml'/>
<item id='n' href='nav.xhtml' properties='nav' media-type='application/xhtml+xml'/>
</manifest><spine><itemref idref='b'/><itemref idref='a'/></spine></package>")
           ("OPS/nav.xhtml" . "<html xmlns:epub='urn:test'><body><nav epub:type='toc'>
<a href='../Text/%E4%B9%A6%20two.xhtml#start'>第二章</a></nav></body></html>")
           ("Text/书 two.xhtml" . "<html><head><title>hidden</title></head><body>
<h1>第二章</h1><p>文字<em>相邻</em>。</p><p>A &amp; B<br/>C</p><script>ignored</script>
</body></html>")
           ("Text/one.xhtml" . "<html><body><p>One.</p></body></html>"))))
    (should (equal 0 (apply #'call-process "python3" nil nil nil "-I" "-c"
                            (concat "import sys, zipfile\n"
                                    "with zipfile.ZipFile(sys.argv[1], 'w') as z:\n"
                                    "    for k,v in zip(sys.argv[2::2],sys.argv[3::2]):\n"
                                    "        z.writestr(k,v)")
                            path (cl-mapcan (lambda (p) (list (car p) (cdr p))) entries))))))

(ert-deftest emacs-reader-files-test-epub ()
  (let ((path (make-temp-file "reader" nil ".epub")))
    (unwind-protect
        (progn
          (emacs-reader-files-test--epub path)
          (should (equal (emacs-reader-files-metadata path) '(:title "试读" :author "Author")))
          (let ((chapters (emacs-reader-files-chapters path)))
            (should (equal chapters '((:title "第二章" :url "Text/书 two.xhtml")
                                      (:title "one" :url "Text/one.xhtml"))))
            (should (equal (emacs-reader-files-content path (car chapters))
                           "第二章\n\n文字相邻。\n\nA & B\nC"))
            (should (equal "One." (emacs-reader-files-content path (cadr chapters)))))
          (should-error (emacs-reader-files-content path "../outside") :type 'user-error))
      (delete-file path))))

(ert-deftest emacs-reader-files-test-resource-paths ()
  (dolist (href '("/outside" "../../outside" "%2e%2e/%2e%2e/outside" "C:%5Cfile"
                  "//example.org/file" "https://example.org" "x%00x"))
    (should-error (emacs-reader-files--resource href "OPS/book.opf") :type 'user-error))
  (should (equal (emacs-reader-files--resource "../Text/A%20B.xhtml#p" "OPS/book.opf")
                 "Text/A B.xhtml")))

(provide 'emacs-reader-files-test)
;;; emacs-reader-files-test.el ends here
