;;; oht-elfeed-pre.el --- Loaded before Elfeed -*- lexical-binding: t -*-


;;; Settings

(setq elfeed-use-curl t)
(setq elfeed-curl-max-connections 10)
(setq elfeed-db-directory "~/.emacs.d/elfeed/")
(setq elfeed-enclosure-default-dir "~/Downloads/")
(setq elfeed-search-filter "@4-months-ago +unread")
(setq elfeed-sort-order 'descending)
(setq elfeed-search-clipboard-type 'CLIPBOARD)
;; (setq elfeed-search-title-max-width 100)
;; (setq elfeed-search-title-min-width 30)
;; (setq elfeed-search-trailing-width 25)
(setq elfeed-show-truncate-long-urls t)
;; (setq elfeed-show-unique-buffers t)


;;; Feeds

(setq elfeed-feeds '(
		     ("https://writings.stephenwolfram.com/feed/")
		     ("https://mjtsai.com/blog/feed/" mac)
		     ("https://panic.com/blog/feed/" mac)
		     ("https://routley.io/posts/index.xml")
		     ("https://xkcd.com/rss.xml")
		     ("https://kk.org/cooltools/feed/")
		     ("https://craigmod.com/index.xml")
		     ("http://100r.co/links/rss.xml")
		     ("https://standardebooks.org/rss/new-releases")
		     ("https://idlewords.com/index.xml")
		     ("https://www.raptitude.com/feed/")
		     ("https://blog.robenkleene.com/feed/atom/")
		     ("https://nyxt.atlas.engineer/feed")
		     ("https://benjaminreinhardt.com/feed.xml")
		     ("https://kieranhealy.org/index.xml")
		     ("https://rsms.me/atom.xml")
		     ("https://waxy.org/category/links/feed/")
		     ("https://tumblr.mapsbynik.com/rss")
		     ;; news
		     ("https://www.economist.com/latest/rss.xml" news)
		     ("https://www.economist.com/the-economist-explains/rss.xml" news)
		     ("https://hnrss.org/best" news)
		     ("https://www.theverge.com/rss/front-page/index.xml" news)
		     ("https://daringfireball.net/feeds/main" news)
		     ("https://feeds.feedburner.com/Metafilter" news)
		     ;; emacs
		     ("https://oremacs.com/atom.xml" emacs)
		     ("https://irreal.org/blog/?feed=rss2" emacs)
		     ("https://endlessparentheses.com/atom.xml" emacs)
		     ("https://200ok.ch/atom.xml" emacs)
		     ("https://with-emacs.com/rss.xml" emacs)
		     ("https://nullprogram.com/tags/emacs/feed/" emacs)
		     ("https://sachachua.com/blog/category/emacs-news/feed" emacs)
		     ("https://protesilaos.com/codelog.xml" emacs)
		     ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-only.xml" emacs)
		     ("https://emacsredux.com/atom.xml" emacs)
		     ("https://planet.emacslife.com/atom.xml" emacs)
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" emacs)
		     ("https://reddit.com/r/emacs/top/.rss?sort=top&t=day" emacs)
		     ))


;;; Feed Title Changes

(defadvice elfeed-search-update (before nullprogram activate)
  (let ((feed (elfeed-db-get-feed "https://www.economist.com/latest/rss.xml")))
    (setf (elfeed-feed-title feed) "Economist: Latest"))
  (let ((feed (elfeed-db-get-feed "https://sachachua.com/blog/category/emacs-news/feed")))
    (setf (elfeed-feed-title feed) "emacs-news"))
  (let ((feed (elfeed-db-get-feed "https://craigmod.com/index.xml")))
    (setf (elfeed-feed-title feed) "Craig Mod"))
  (let ((feed (elfeed-db-get-feed "https://www.reddit.com/r/emacs/top/.rss")))
    (setf (elfeed-feed-title feed) "r/emacs"))
  (let ((feed (elfeed-db-get-feed "http://feeds.feedburner.com/Metafilter")))
    (setf (elfeed-feed-title feed) "MetaFilter"))
  )


(provide 'oht-elfeed-post)
