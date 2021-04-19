;;; oht-elfeed-pre.el --- Loaded before Elfeed -*- lexical-binding: t -*-


;;; Feed Title and Tag Changes

(defadvice elfeed-search-update (before nullprogram activate)
  (let ((feed (elfeed-db-get-feed "https://www.economist.com/latest/rss.xml")))
    (setf (elfeed-feed-title feed) "Economist: Latest"))
  (let ((feed (elfeed-db-get-feed "https://sachachua.com/blog/category/emacs-news/feed")))
    (setf (elfeed-feed-title feed) "emacs-news"))
  (let ((feed (elfeed-db-get-feed "https://craigmod.com/index.xml")))
    (setf (elfeed-feed-title feed) "Craig Mod"))
  (let ((feed (elfeed-db-get-feed "https://reddit.com/r/emacs/top/.rss?sort=top&t=day")))
    (setf (elfeed-feed-title feed) "r/emacs"))
  (let ((feed (elfeed-db-get-feed "http://feeds.feedburner.com/Metafilter")))
    (setf (elfeed-feed-title feed) "MetaFilter")))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(youtube)))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "7 days ago"
                              :remove 'unread))


;;; Search Shortcuts

(define-key elfeed-search-mode-map "N"
  (lambda ()
    (interactive)
    (elfeed-search-set-filter "+unread +news")))

(define-key elfeed-search-mode-map "E"
  (lambda ()
    (interactive)
    (elfeed-search-set-filter "+unread +emacs")))

(define-key elfeed-search-mode-map "S"
  (lambda ()
    (interactive)
    (elfeed-search-set-filter "+star")))


;;; Functions

(defun oht-elfeed-show-fonts ()
  "Apply some customization to fonts in elfeed-show-mode."
  (facedancer-vadjust-mode)
  (setq-local line-spacing 3))

(defun bjm/elfeed-show-visit-gui ()
  "Wrapper for elfeed-show-visit to use gui browser instead of eww"
  (interactive)
  (let ((browse-url-generic-program "/usr/bin/open"))
    (elfeed-show-visit t)))

;; Elfeed doesn't have a built-in way of flagging or marking items for later,
;; but it does have tags, which you can use for this. The below is some simple
;; aliases for adding and removing the 'star' tag.
(defalias 'elfeed-search-tag--star
  (elfeed-expose #'elfeed-search-tag-all 'star)
  "Add the 'star' tag to all selected entries")
(defalias 'elfeed-search-untag--star
  (elfeed-expose #'elfeed-search-untag-all 'star)
  "Remove the 'star' tag to all selected entries")

;; And for show mode...
(defalias 'elfeed-show-tag--star
  (elfeed-expose #'elfeed-show-tag 'star)
  "Add the 'star' tag to current entry")
(defalias 'elfeed-show-tag--unstar
  (elfeed-expose #'elfeed-show-untag 'star)
  "Remove the 'star' tag to current entry")

;; Even though there are bindings for marking items as 'read' and 'unread' in
;; the search-mode, there are no such built-in bindings for show-mode. So we
;; add them here.
(defalias 'elfeed-show-tag--unread
  (elfeed-expose #'elfeed-show-tag 'unread)
  "Mark the current entry unread.")
(defalias 'elfeed-show-tag--read
  (elfeed-expose #'elfeed-show-untag 'unread)
  "Mark the current entry read.")

(defun hrs/elfeed-current-entry ()
  "Return the elfeed entry, in both show and search modes."
  (cond ((eq major-mode 'elfeed-show-mode)
         elfeed-show-entry)
        ((eq major-mode 'elfeed-search-mode)
         (elfeed-search-selected t))))

(defun hrs/elfeed-pinboard-current-entry ()
  "Save the current elfeed entry to pinboard."
  (interactive)
  (let ((url (elfeed-entry-link (hrs/elfeed-current-entry)))
        (title (elfeed-entry-title (hrs/elfeed-current-entry))))
    (pinboard-auth)
    (pinboard-not-too-soon :pinboard-save
                           (pinboard-save url title "" "" t nil))))

(defun oht-elfeed-show-download-video ()
  "In elfeed, download a video using youtube-dl."
  (interactive)
  (async-shell-command (format "%s -o \"%s%s\" -f mp4 \"%s\""
                               youtube-dl-path
                               youtube-dl-output-dir
                               "%(title)s.%(ext)s"
                               (elfeed-entry-link elfeed-show-entry))))


;; Creates a toggle for showing images
(make-variable-buffer-local
 (defvar elfeed-inhibit-images-status nil
   "Elfeed Inhibit Images Status"))

(defun elfeed-inhibit-images-toggle ()
  (interactive)
  (setq elfeed-inhibit-images-status (not elfeed-inhibit-images-status))
  (if elfeed-inhibit-images-status
      (progn
        (setq-local shr-inhibit-images t)
        (elfeed-show-refresh))
    (progn
      (setq-local shr-inhibit-images nil)
      (elfeed-show-refresh))))


;; Open url in background

(defun oht-elfeed-search-browse-and-bury ()
  "Browse elfeed entry and bury buffer."
  (interactive)
  (elfeed-search-browse-url)
  (bury-buffer)
  (message "Browsing in buried buffer"))

(defun oht-elfeed-show-browse-and-bury ()
  "Browse elfeed entry and bury buffer."
  (interactive)
  (elfeed-show-visit)
  (bury-buffer)
  (message "Browsing in buried buffer"))


(provide 'oht-elfeed-pre)
