;;; oht-elfeed-pre.el --- Loaded before Elfeed -*- lexical-binding: t -*-


;;; Settings

(setq elfeed-use-curl t)
(setq elfeed-curl-max-connections 10)
(setq elfeed-db-directory "~/.emacs.d/elfeed/")
(setq elfeed-enclosure-default-dir "~/Downloads/")
(setq elfeed-search-filter "@1-week-ago +unread")
(setq elfeed-sort-order 'descending)
(setq elfeed-search-clipboard-type 'CLIPBOARD)
(setq elfeed-show-truncate-long-urls t)

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
		     ("https://feeds.feedburner.com/Metafilter")
		     ("https://tumblr.mapsbynik.com/rss")
		     ;; news
		     ("https://www.economist.com/latest/rss.xml" news)
		     ("https://www.economist.com/the-economist-explains/rss.xml" news)
		     ("https://hnrss.org/best" news)
		     ("https://www.theverge.com/rss/front-page/index.xml" news)
		     ("https://daringfireball.net/feeds/main" news mac)
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
		     ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-only.xml" emacs)
		     ("https://emacsredux.com/atom.xml" emacs)
		     ("https://planet.emacslife.com/atom.xml" emacs)
		     ("https://reddit.com/r/emacs/top/.rss?sort=top&t=day" emacs)
		     ;; youtube
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" emacs) ; Prot
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMb0O2CdPBNi-QqPk5T3gsQ") ; James Hoffmann
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZYycARMM3DaeBwpVQzR5vQ") ; Craig Mod
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZYycARMM3DaeBwpVQzR5vQ") ; Keep Calm I'm in VFX
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVpankR4HtoAVtYnFDUieYA") ; zefrank1
		     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCCBJv-2y0qRjM9E2tSGtT7Q") ; Weta Workshop
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
  (let ((feed (elfeed-db-get-feed "https://www.reddit.com/r/emacs/top/.rss")))
    (setf (elfeed-feed-title feed) "r/emacs"))
  (let ((feed (elfeed-db-get-feed "http://feeds.feedburner.com/Metafilter")))
    (setf (elfeed-feed-title feed) "MetaFilter"))
  )


(provide 'oht-elfeed-post)
