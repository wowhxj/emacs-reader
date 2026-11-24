;;; emacs-reader-sources-export.el --- Export registered source backups -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:
;; One local ZIP per source type.  Backups retain private request headers.

;;; Code:

(require 'emacs-reader-local)
(require 'seq)

(defconst emacs-reader-sources-export--types '("legado" "opds" "json"))

(defun emacs-reader-sources-export--archive (stage type records archive python)
  "Build ARCHIVE for TYPE and RECORDS under STAGE with PYTHON's zipfile."
  (let* ((default-directory (file-name-as-directory (expand-file-name type stage)))
         (members '("records")))
    (make-directory "records" t)
    (dolist (record records)
      (let* ((key (emacs-reader-local--source-path type (alist-get 'url record)))
             (path (expand-file-name (file-name-nondirectory key) "records")))
        (when (file-exists-p path) (user-error "同类书源存在重复注册地址"))
        (emacs-reader-local--write path record)))
    (when (equal type "legado")
      (emacs-reader-local--write (expand-file-name "legado.json")
                                 (vconcat (mapcar (lambda (r) (alist-get 'definition r)) records)))
      (push "legado.json" members))
    (unless (equal 0 (apply #'call-process python nil nil nil
                            "-I" "-m" "zipfile" "-c" archive members))
      (user-error "创建 %s 书源 ZIP 失败" type))
    (set-file-modes archive #o600)))

(defconst emacs-reader-sources-export--exclusive-copy
  "import os, shutil, sys
src, dest = sys.argv[1:]
os.umask(0o077)
with open(src, 'rb') as source:
    try:
        target = open(dest, 'xb')
    except FileExistsError:
        sys.exit(2)
    try:
        with target:
            shutil.copyfileobj(source, target)
    except BaseException:
        os.unlink(dest)
        raise
"
  "Publish a private ZIP on filesystems without hard links.")

(defun emacs-reader-sources-export--publish (temp dest python)
  "Publish TEMP at DEST, using PYTHON for an exclusive copy if linking fails."
  (condition-case err
      (add-name-to-file temp dest)
    (file-already-exists (signal (car err) (cdr err)))
    (file-error
     (pcase (call-process python nil nil nil "-I" "-c"
                          emacs-reader-sources-export--exclusive-copy temp dest)
       (0 nil)
       (2 (signal 'file-already-exists (list "备份文件名冲突" dest)))
       (_ (signal 'file-error (list "发布备份 ZIP 失败" dest)))))))

;;;###autoload
(defun emacs-reader-sources-export (type directory)
  "Export registered sources of TYPE to local DIRECTORY and return ZIP paths.
TYPE is one of the strings \"all\", \"legado\", \"opds\" or \"json\".
The unencrypted archives include complete records, including credentials."
  (interactive
   (list (completing-read "导出书源类型：" (cons "all" emacs-reader-sources-export--types)
                          nil t nil nil "all")
         (read-directory-name "备份目录（ZIP 可能含私密请求头）：" nil nil t)))
  (unless (member type (cons "all" emacs-reader-sources-export--types))
    (user-error "未知的书源导出类型"))
  (setq directory (expand-file-name directory))
  (when (or (file-remote-p directory) (file-remote-p (expand-file-name emacs-reader-directory)))
    (user-error "书源导出需要本地注册表和备份目录"))
  (unless (file-directory-p directory) (user-error "请选择已有的备份目录"))
  (let* ((records (emacs-reader-local--records "sources"))
         (types (if (equal type "all") emacs-reader-sources-export--types (list type)))
         (stamp (format-time-string "%Y%m%d%H%M%S%3N"))
         (groups (seq-group-by (lambda (record) (alist-get 'type record)) records))
         (archives (cl-loop for kind in types when (assoc kind groups)
                            collect (cons kind (expand-file-name
                                                (format "emacs-reader-booksource-%s-backup-%s.zip"
                                                        kind stamp) directory))))
         (python (executable-find "python3"))
         published complete)
    (when (and (equal type "all")
               (cl-some (lambda (group) (not (member (car group) types))) groups))
      (user-error "注册表中含未知的书源类型"))
    (unless archives (user-error "所选类型没有已注册的书源"))
    (unless python (user-error "创建 ZIP 需要 Python 3 标准库 zipfile"))
    (dolist (archive archives)
      (when (or (file-exists-p (cdr archive)) (file-symlink-p (cdr archive)))
        (user-error "备份文件名冲突，请重试：%s" (cdr archive))))
    (let ((stage (make-temp-file (expand-file-name ".reader-export-" directory) t)))
      (unwind-protect
          (progn
            (dolist (entry archives)
              (emacs-reader-sources-export--archive
               stage (car entry) (cdr (assoc (car entry) groups))
               (expand-file-name (file-name-nondirectory (cdr entry)) stage) python))
            (dolist (entry archives)
              (let* ((dest (cdr entry))
                     (temp (expand-file-name (file-name-nondirectory dest) stage))
                     (inhibit-quit t))
                (emacs-reader-sources-export--publish temp dest python)
                (push (cons dest (file-attribute-file-identifier (file-attributes dest)))
                      published)))
            (setq complete t)
            (message "已导出 %d 个书源 ZIP 到 %s" (length archives) directory)
            (mapcar #'cdr archives))
        (unwind-protect
            (unless complete
              (dolist (pair published)
                (when-let* ((attrs (file-attributes (car pair))))
                  (when (equal (cdr pair) (file-attribute-file-identifier attrs))
                    (delete-file (car pair))))))
          (delete-directory stage t))))))

(provide 'emacs-reader-sources-export)
;;; emacs-reader-sources-export.el ends here
