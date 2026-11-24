;;; emacs-reader-legado-static-html.el --- Public HTML search pages -*- lexical-binding: t; indent-tabs-mode: nil; fill-column: 100; -*-

;;; Commentary:

;; Two imported sources have conditional browser rules and ordinary HTML lists.

;;; Code:

(require 'emacs-reader-legado)

(defconst emacs-reader-legado-static-html--yidu-url
  (concat "@js:url=\"https://www.121du.net/search/?wd=\"+encodeURI(key);"
          "url+`,{\"headers\":{\"referer\":\"${url}\"}}`"))

(defconst emacs-reader-legado-static-html--wenxue-url
  "https://m.juzixs.vip/search.php?keyword={{key}}&submit=,{\"webView\": true}")

(defconst emacs-reader-legado-static-html--preamble-sha256
  "d55cb0d37db85bf9dd02d48bf8afc29bac2e157e92b75ec75341b0cd03dbb34d"
  "Digest of the imported one-read conditional, up to the closing script tag.")

(defun emacs-reader-legado-static-html--site (source)
  "Identify the supported search endpoint and source origin in SOURCE."
  (let ((origin (plist-get source :bookSourceUrl)) (request (plist-get source :searchUrl)))
    (cond ((and (equal origin "https://www.121du.net/")
                (equal request emacs-reader-legado-static-html--yidu-url)) 'yidu)
          ((and (equal origin "https://www.juzixs.vip/")
                (equal request emacs-reader-legado-static-html--wenxue-url)) 'wenxue))))

(defun emacs-reader-legado-static-html--rules (site rules)
  "Prepare declarative list RULES for the ordinary HTML response from SITE."
  (let ((rules (copy-sequence rules)) (selector (plist-get rules :bookList)))
    (if (eq site 'yidu)
        (when (and (stringp selector) (string-prefix-p "<js>" selector))
          ;; Pin the exact imported conditional preamble.  Running it would try
          ;; to fetch a challenge cookie.  Only its ordinary-page branch is used.
          (let ((end (string-search "</js>" selector)))
            (unless (and end (equal (secure-hash 'sha256 (substring selector 0 end))
                                    emacs-reader-legado-static-html--preamble-sha256))
              (error "一读小说列表需要未支持的脚本处理"))
            (setq rules (plist-put rules :bookList (substring selector (+ end 5))))))
      (when (equal (plist-get rules :bookUrl)
                   "tag.h4@tag.a@href@js:'https://www.juzixs.vip'+result")
        (setq rules (plist-put rules :bookUrl "tag.h4@tag.a@href")))
      (let ((name (plist-get rules :name)))
        (when (and (stringp name) (string-prefix-p "tag.h4@tag.a@a@text##" name))
          (setq rules (plist-put rules :name (string-replace "tag.a@a@" "tag.a@" name))))))
    rules))

(defun emacs-reader-legado-static-html--target (site query)
  "Return the ordinary GET target for SITE and QUERY."
  (if (eq site 'wenxue) (car (split-string emacs-reader-legado-static-html--wenxue-url ","))
    (let* ((key (url-hexify-string query))
           (url (concat "https://www.121du.net/search/?wd=" key)))
      (concat url "," (json-serialize (list :headers (list :Referer url)))))))

(defun emacs-reader-legado-static-html--books (site source doc url)
  "Read confirmed book rows from DOC at URL for SITE and SOURCE."
  (unless (eq (car doc) 'html) (error "书源未返回 HTML 列表"))
  (let* ((root (cdr doc)) (title (car (dom-by-tag root 'title)))
         (text (and title (emacs-reader-legado--text title)))
         (origin (plist-get source :bookSourceUrl))
         (rules (emacs-reader-legado-static-html--rules site (plist-get source :ruleSearch))))
    (when (or (and text (string-match-p "人机验证\\|安全验证\\|Just a moment\\|Access Denied" text))
              (and (dom-by-tag root 'script) (string-blank-p (emacs-reader-legado--text root))))
      (error "书源返回网站验证或脚本页，搜索已停止"))
    (or (emacs-reader-legado--rows
         doc rules :bookList
         (lambda (row)
           (when-let* ((name (emacs-reader-legado--field row rules :name))
                       (href (emacs-reader-legado--field row rules :bookUrl))
                       (base (if (eq site 'wenxue) origin url))
                       (link (emacs-reader-legado--url href base))
                       ((equal (emacs-reader-legado--origin link)
                               (emacs-reader-legado--origin origin))))
             (list :name name :bookUrl link :origin origin
                   :originName (plist-get source :bookSourceName)
                   :author (ignore-errors (emacs-reader-legado--field row rules :author))))))
        (error "普通 HTTP 页面未匹配到书目，书源可能需要 JavaScript 渲染"))))

(defun emacs-reader-legado-static-html-search (site source query callback page)
  "Search ordinary HTML at SITE for QUERY using SOURCE and deliver PAGE to CALLBACK."
  (if (> (or page 1) 1) (funcall callback nil nil)
    (emacs-reader-legado--map-page
     source (emacs-reader-legado-static-html--target site query) (plist-get source :bookSourceUrl)
     callback (lambda (doc url _) (emacs-reader-legado-static-html--books site source doc url))
     (and (eq site 'wenxue) (list query 1)))))

(provide 'emacs-reader-legado-static-html)
;;; emacs-reader-legado-static-html.el ends here
