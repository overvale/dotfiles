;;; oht-elfeed-pre.el --- Loaded before Elfeed -*- lexical-binding: t -*-

;;; Feeds

(setq elfeed-feeds '(
		     ("https://writings.stephenwolfram.com/feed/")
		     ("https://mjtsai.com/blog/feed/" mac)
		     ("https://panic.com/blog/feed/" mac)
		     ("https://routley.io/posts/index.xml")
		     ("https://xkcd.com/rss.xml")
		     ("https://craigmod.com/index.xml")
		     ("http://100r.co/links/rss.xml")
		     ("https://standardebooks.org/rss/new-releases")
		     ("https://idlewords.com/index.xml")
		     ("https://blog.robenkleene.com/feed/atom/")
		     ("https://nyxt.atlas.engineer/feed")
		     ("https://benjaminreinhardt.com/feed.xml")
		     ("https://kieranhealy.org/index.xml")
		     ("https://rsms.me/atom.xml")
		     ("https://waxy.org/category/links/feed/")
		     ("https://tumblr.mapsbynik.com/rss")
		     ("https://www.julian.digital/feed")
		     ("https://github.com/Hammerspoon/hammerspoon/releases.atom")
		     ("https://github.com/syncthing/syncthing-macos/releases.atom")
		     ;; vfx
		     ("https://keepcalmiminvfx.net/feed/" vfx)
		     ("https://beforesandafters.com/feed/" vfx)
		     ("https://www.artofvfx.com/feed/" vfx)
		     ("https://www.vfxvoice.com/feed/" vfx)
		     ;; news
		     ("https://www.economist.com/the-economist-explains/rss.xml" news)
		     ("https://hnrss.org/best" news)
		     ("https://www.theverge.com/rss/front-page/index.xml" news)
		     ;; emacs
		     ("https://github.com/railwaycat/homebrew-emacsmacport/releases.atom" emacs)
		     ("https://github.com/raxod502/selectrum/releases.atom" emacs)
		     ("https://github.com/minad/consult/releases.atom" emacs)
		     ("https://github.com/oantolin/embark/releases.atom" emacs)
		     ("https://github.com/minad/marginalia/releases.atom" emacs)
		     ("https://gitlab.com/protesilaos/modus-themes/-/tags?format=atom" emacs)
		     ("https://oremacs.com/atom.xml" emacs)
		     ("https://irreal.org/blog/?feed=rss2" emacs)
		     ("https://endlessparentheses.com/atom.xml" emacs)
		     ("https://200ok.ch/atom.xml" emacs)
		     ("https://with-emacs.com/rss.xml" emacs)
		     ("https://nullprogram.com/tags/emacs/feed/" emacs)
		     ("https://sachachua.com/blog/category/emacs-news/feed" emacs)
		     ("https://protesilaos.com/codelog.xml" emacs)
		     ("https://emacsredux.com/atom.xml" emacs)
		     ("https://planet.emacslife.com/atom.xml" emacs)
		     ("https://reddit.com/r/emacs/top/.rss?sort=top&t=day" emacs)
		     ;; youtube
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" emacs) ; Prot
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZYycARMM3DaeBwpVQzR5vQ") ; Craig Mod
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZYycARMM3DaeBwpVQzR5vQ" vfx) ; Keep Calm I'm in VFX
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVpankR4HtoAVtYnFDUieYA") ; zefrank1
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtqxG9IrHFU_ID1khGvx9sA") ; All Gas No Brakes
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAL3JXZSzSm8AlZyD3nQdBA") ; Primitive Technology
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCiDJtJKMICpb9B1qf7qjEOA") ; Adam Savage’s Tested
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7FkqjV8SU5I8FCHXQSQe9Q" wood) ; Ishitani Furniture
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVOpX2P5wygh7sB1KXgh_5g" wood) ; Kobeomsuk furniture
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC_FksrzP3q-IuoWTiG501LQ" wood) ; Peter Millard
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYwsgA-ZhDGHUCb4YFG00xQ" wood) ; New Brit Workshop
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCc3EpWncNq5QL0QhwUNQb7w" wood) ; Paul Sellers
		     ))


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
    (setf (elfeed-feed-title feed) "MetaFilter"))
  )


;;; Display

(defun oht-elfeed-show-fonts ()
  "Apply some customization to fonts in elfeed-show-mode."
  (text-scale-increase 1)
  (setq-local line-spacing 2)
  )


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


;;; Auto Tagging

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(youtube)))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "7 days ago"
			      :remove 'unread))

;;; Functions

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

(provide 'oht-elfeed-pre)
