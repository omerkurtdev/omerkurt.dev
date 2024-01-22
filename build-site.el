;;; build-site.el --- description -*- lexical-binding: t; -*-
;;
;; copyright (c) 2023 omerkurt
;;
;; author: Omer Kurt <blog@omerkurt.dev>
;; maintainer: Omer Kurt <blog@omerkurt.dev>
;; created: aralık 27, 2023
;; modified: aralık 27, 2023
;; version: 0.0.1
;; keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; homepage: https://github.com/omerkurt/build-site
;; package-requires: ((emacs "24.3"))
;;
;; this file is not part of gnu emacs.
;;
;;; commentary:
;;
;;  description
;;
;;; code:
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(package-install 'htmlize)
(require 'ox-publish)

(defun file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(setq org-export-html-date-format-string "%Y-%m-%d")
(setq html-preamble (file-contents "./org/html/preamble.html")
      html-postamble(file-contents "./org/html/postamble.html"))

(defun m/org-publish-org-sitemap (title list)
  "Sitemap generation function."
  (concat "#+TITLE: Blog yazılarım ve notlarım\n"
          "#+MACRO: NEWLINE @@latex:\\@@ @@html:<br>@@ @@ascii:|@@\n"
          "#+OPTIONS: toc:nil num:nil title:nil\n"
          (org-list-to-subtree list)))

(defun get-subtitle (file project)
  "Find the title of FILE in PROJECT."
  (let ((file (org-publish--expand-file-name file project)))
    (or (org-publish-cache-get-file-property file :subtitle nil t)
	(let* ((parsed-title (org-publish-find-property file :subtitle project 'html))
	       (title
		(if parsed-title
		    ;; Remove property so that the return value is
		    ;; cache-able (i.e., it can be `read' back).
		    (org-no-properties
		     (org-element-interpret-data parsed-title))
		  (file-name-nondirectory (file-name-sans-extension file)))))
	  (org-publish-cache-set-file-property file :subtitle title)))))
(defun loomcom/get-preview (filename)
  "Get a preview of the content from an Org file."
  (with-temp-buffer
    (insert-file-contents (concat "org/blog/" filename))
    (goto-char (point-min))
    (let ((marker-start (or (and (re-search-forward "^#\\+begin_comment$" nil t)
                                 (match-end 0))
                            (buffer-size)))
          (marker-end (or (and (re-search-forward "^#\\+end_comment$" nil t)
                               (match-beginning 0))
                          (buffer-size))))
      (when (= marker-start (buffer-size))
        (goto-char (point-min))
        (setq marker-start (or (and (re-search-forward "^[^#]" nil t)
                                    (match-beginning 0))
                               (buffer-size))
              marker-end (progn (forward-paragraph) (point))))
      (list (not (= marker-end (buffer-size)))
            (string-trim (buffer-substring marker-start marker-end))))))

(defun loomcom/sitemap-entry (entry style project)
  "Sitemap (Blog Main Page) Entry Formatter."
  (when (not (directory-name-p entry))
    (format (concat
             "\n\n[[file:%s][%s]]\n"
             "#+BEGIN_icerik\n"
             "#+BEGIN_published\n"
             "%s\n"
             "#+END_published\n\n"
             "-----\n"
             "%s\n"
             "#+END_icerik\n")
            entry
            (get-subtitle entry project)
            (format-time-string "%d-%m-%Y" (org-publish-find-date entry project))
            (let* ((preview (loomcom/get-preview entry))
                   (needs-more (car preview))
                   (preview-text (cadr preview)))
              (if needs-more
                  (format (concat "%s\n\n"
                                  "#+BEGIN_morelink\n"
                                  "[[file:%s][Daha fazla...]]\n"
                                  "#+END_morelink\n")
                          preview-text entry)
                (format "%s" preview-text))))))



(defun loomcom/sitemap-entry-rss (entry style project)
  "Sitemap (Blog Main Page) Entry Formatter."
  (when (not (directory-name-p entry))
    (format (concat "%s\n"
                    ":properties:\n"
                    ":rss_permalink: %s\n"
                    ":pubdate: %s\n"
                    ":end:\n"
                    "#+BEGIN_published\n"
                    "%s\n"
                    "#+END_published\n"
                    "-----\n")
            (org-publish-find-title entry project)
            (concat (file-name-sans-extension entry) ".html")
            (format-time-string (car org-time-stamp-formats) (org-publish-find-date entry project))
            (let* ((preview (loomcom/get-preview entry))
                   (needs-more (car preview))
                   (preview-text (cadr preview)))
              (if needs-more
                  (format (concat "%s\n\n"
                                  "#+BEGIN_morelink\n"
                                  "[[file:%s][Daha fazla...]]\n"
                                  "#+END_morelink\n")
                          preview-text entry)
                (format "%s" preview-text))))))


(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")
(defun posts-rss-feed (title list)
  "Generate a sitemap of posts that is exported as a RSS feed.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include.  PROJECT is the current
project."
  (message "Generating RSS feed...") ; Debug çıktısı eklendi
  (concat
   "#+TITLE: " title "\n\n"
   (org-list-to-subtree list)))

(defun format-posts-rss-feed-entry (entry _style project)
  "Format ENTRY for the posts RSS feed in PROJECT."
  (let* ((title (org-publish-find-title entry project))
         (link (concat (file-name-sans-extension (file-relative-name entry (org-publish-property :base-directory project))) ".html"))
         (pubdate (format-time-string (car org-time-stamp-formats) (org-publish-find-date entry project)))
         (preview (loomcom/get-preview entry))
         (needs-more (car preview))
         (preview-text (cadr preview)))
    (format "%s
:properties:
:rss_permalink: %s
:pubdate: %s
:end:\n%s\n"
            title
            link
            pubdate
            (if needs-more
                (format "Preview: %s\n" preview-text)))))

(defun publish-posts-rss-feed (plist filename dir)
  "Publish PLIST to RSS when FILENAME is rss.org.
DIR is the location of the output."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (progn
        (message "Publishing RSS feed...") ; Debug çıktısı eklendi
        (message "PLIST: %s" plist)        ; Debug çıktısı eklendi
        (message "FILENAME: %s" filename)  ; Debug çıktısı eklendi
        (message "DIR: %s" dir)            ; Debug çıktısı eklendi
        (org-rss-publish-to-rss plist filename dir))))

(setq org-publish-project-alist
      `(("pages"
         :base-directory "org"
         :base-extension "org"
         :language "tr"
         :recursive nil
         :with-author nil
         :with-creator nil
         :with-date t
         :section-numbers nil
         :time-stamp-file nil
         :html-doctype "html5"
         :html-postamble ,html-postamble
         :html-preamble ,html-preamble
         :html-html5-fancy t
         :publishing-directory "html/"
         :publishing-function org-html-publish-to-html)
        ("blog"
         :section-numbers nil
         :time-stamp-file nil
         :sitemap-function m/org-publish-org-sitemap
         :html-html5-fancy t
         :language "tr"
         :exclude "rss.org"
         :exclude ".*/daily/.*"
         :html-doctype "html5"
         :base-directory "org/blog"
         :base-extension "org"
         :publishing-directory "html/blog/"
         :publish-function org-html-publish-to-html
         :auto-sitemap t
         :sitemap-title "blog posts"
         :html-preamble ,html-preamble
         :html-postamble ,html-postamble
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry loomcom/sitemap-entry)
        ("posts-rss"
         :publishing-directory "html/blog"
         :base-directory "org/blog"
         :language "tr"
         :author "Omer Kurt"
         :base-extension "org"
         :exclude "index.org"
         :exclude "rss.org"
         :publishing-function publish-posts-rss-feed
         :rss-extension "xml"
         :html-link-home "https://omerkurt.dev/blog/"
         :html-link-use-abs-url t
         :html-link-org-files-as-html t
         :auto-sitemap t
         :sitemap-function m/org-publish-org-sitemap
         :sitemap-title "Ömer Kurt Blog"
         :sitemap-filename "rss.org"
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry loomcom/sitemap-entry-rss)
        ("static"
         :base-directory "org"
         :base-extension "css\\|txt\\|jpg\\|gif\\|png\\|pgp"
         :recursive t
         :publishing-directory  "html/"
         :publishing-function org-publish-attachment)

        ("omerkurt.dev" :components ("pages" "blog" "static" "posts-rss" ))))
(org-publish-remove-all-timestamps)
(org-publish "omerkurt.dev" t)

(message "build complete!")

;;; build-site.el ends here
