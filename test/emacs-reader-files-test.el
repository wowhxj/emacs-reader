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

(ert-deftest emacs-reader-files-test-outlines ()
  (dolist (fixture '((".md" "Preface\n\n# One\nAlpha\n## Child\nBeta\n```\n# Code\n```\n# Two\nLast\n"
                       ("Preface" "One" "Two"))
                     (".markdown" "Title\n=====\nFirst\n\nSecond\n------\nBody\n"
                       ("Title"))
                     (".org" "Intro\n* First\nBody\n#+begin_src org\n  * Not a chapter\n#+end_src\n** Nested\nText\n* Second\nEnd\n"
                       ("Preface" "First" "Second"))))
    (pcase-let* ((`(,suffix ,text ,titles) fixture)
                 (path (make-temp-file "reader-outline" nil suffix text)))
      (unwind-protect
          (let* ((inspection (emacs-reader-files--inspect path))
                 (chapters (cadr inspection))
                 (actual (mapcar (lambda (c) (plist-get c :title)) chapters)))
            (when (equal (car titles) "Preface")
              (setcar titles (file-name-base path)))
            (should (equal actual titles))
            (let ((joined (mapconcat (lambda (c) (emacs-reader-files-content path c))
                                     chapters "")))
              (should (string-match-p (if (equal suffix ".org") "Body" "First\\|Alpha") joined))
              (should-not (string-match-p "^# One" joined)))
            (should-error (emacs-reader-files-content path "txt:1:2") :type 'user-error))
        (delete-file path)))))

(ert-deftest emacs-reader-files-test-org-metadata ()
  (let ((path (make-temp-file "reader-org" nil ".org" "#+TITLE: Book\n#+AUTHOR: A\n\n* Chapter\nText\n")))
    (unwind-protect
        (let ((chapters (emacs-reader-files-chapters path)))
          (should (= 1 (length chapters)))
          (should (equal "Chapter" (plist-get (car chapters) :title)))
          (should (equal "Chapter\nText\n" (emacs-reader-files-content path (car chapters)))))
      (delete-file path))))

(defun emacs-reader-files-test--pdf (path &optional blank)
  "Generate a small two-page PDF at PATH, blank if BLANK is non-nil."
  (let ((script (concat
                 "import sys\n"
                 "content = b'' if sys.argv[2] == 'blank' else b'BT /F1 18 Tf 72 720 Td (First page) Tj ET'\n"
                 "content2 = b'' if sys.argv[2] == 'blank' else b'BT /F1 18 Tf 72 720 Td (Second page) Tj ET'\n"
                 "objs = [b'<< /Type /Catalog /Pages 2 0 R >>', b'<< /Type /Pages /Kids [3 0 R 5 0 R] /Count 2 >>', b'<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 7 0 R >> >> /Contents 4 0 R >>', b'<< /Length %d >>\\nstream\\n' % len(content) + content + b'\\nendstream', b'<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 7 0 R >> >> /Contents 6 0 R >>', b'<< /Length %d >>\\nstream\\n' % len(content2) + content2 + b'\\nendstream', b'<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>']\n"
                 "data = b'%PDF-1.4\\n'; offsets = [0]\n"
                 "for i, obj in enumerate(objs, 1):\n"
                 "    offsets.append(len(data)); data += b'%d 0 obj\\n' % i + obj + b'\\nendobj\\n'\n"
                 "xref = len(data); data += b'xref\\n0 %d\\n0000000000 65535 f \\n' % (len(objs)+1)\n"
                 "for offset in offsets[1:]: data += b'%010d 00000 n \\n' % offset\n"
                 "data += b'trailer\\n<< /Size %d /Root 1 0 R >>\\nstartxref\\n%d\\n%%%%EOF\\n' % (len(objs)+1, xref)\n"
                 "open(sys.argv[1], 'wb').write(data)\n")))
    (should (equal 0 (call-process "python3" nil nil nil "-I" "-c" script path
                                   (if blank "blank" "text"))))))

(ert-deftest emacs-reader-files-test-pdf ()
  (skip-unless (or (executable-find "pdftotext") (executable-find "mutool")))
  (let ((path (make-temp-file "reader" nil ".pdf")))
    (unwind-protect
        (progn
          (emacs-reader-files-test--pdf path)
          (let* ((inspection (emacs-reader-files--inspect path))
                 (chapters (cadr inspection)))
            (should (equal (mapcar (lambda (c) (plist-get c :title)) chapters)
                           '("Page 1" "Page 2")))
            (should (string-match-p "First page" (emacs-reader-files-content path (car chapters))))
            (should (string-match-p "Second page" (emacs-reader-files-content path (cadr chapters))))
            (should-error (emacs-reader-files-content path "pdf:0") :type 'user-error)
            (should-error (emacs-reader-files-content path "pdf:3") :type 'user-error)
            (when (executable-find "mutool")
              (let ((find (symbol-function 'executable-find)))
                (cl-letf (((symbol-function 'executable-find)
                           (lambda (name) (unless (equal name "pdftotext")
                                            (funcall find name)))))
                  (should (= 2 (length (emacs-reader-files-chapters path))))
                  (should (string-match-p "Second page" (emacs-reader-files-content path "pdf:2")))
                  (should-error (emacs-reader-files-content path "pdf:3") :type 'user-error)))))
          (emacs-reader-files-test--pdf path t)
          (let ((inspection (emacs-reader-files--inspect path)))
            (should (plist-get (car inspection) :image-pdf))
            (should (= 2 (length (cadr inspection))))))
      (delete-file path))))

(ert-deftest emacs-reader-files-test-worker-outline ()
  (require 'emacs-reader-file-worker)
  (let ((path (make-temp-file "reader-worker" nil ".md" "# One\nText\n# Two\nMore\n")))
    (unwind-protect
        (let (done result failure chapter)
          (dolist (op '(inspect chapters content))
            (setq done nil result nil failure nil)
            (emacs-reader-file-worker-start
             op (if (eq op 'content) (list path chapter) (list path))
             (lambda (value err) (setq result value failure err done t)))
            (let ((deadline (+ (float-time) 15)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.1)))
            (should done)
            (should-not failure)
            (pcase op
              ('inspect (should (= 2 (length (cadr result)))))
              ('chapters (setq chapter (car result))
                         (should (equal "One" (plist-get chapter :title))))
              ('content (should (equal "One\nText\n" result))))))
      (delete-file path))))

(ert-deftest emacs-reader-files-test-mobi ()
  (skip-unless (executable-find "ebook-convert"))
  (let ((dir (make-temp-file "reader-mobi-test" t)))
    (unwind-protect
        (let ((epub (expand-file-name "original.epub" dir))
              (mobi (expand-file-name "book.mobi" dir)))
          (emacs-reader-files-test--epub epub)
          (should (equal 0 (call-process "ebook-convert" nil nil nil epub mobi)))
          (let* ((inspection (emacs-reader-files--inspect mobi))
                 (chapters (cadr inspection)))
            (should chapters)
            (should (equal (car inspection) (emacs-reader-files-metadata mobi)))
            (should (equal chapters (emacs-reader-files-chapters mobi)))
            (should (cl-some (lambda (c) (string-match-p "One\\.\\|文字"
                                                         (emacs-reader-files-content mobi c)))
                             chapters))))
      (delete-directory dir t))))

(provide 'emacs-reader-files-test)
;;; emacs-reader-files-test.el ends here
