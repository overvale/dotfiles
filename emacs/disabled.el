;;;; Dired

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

;;(use-package emacs
;;  :bind (:map dired-mode-map
;;	      ("." . hydra-dired/body)
;;	      ("s-\\ h" . hydra-dired/body)
;;f	      ))

;;;; Buffer Menu

(defhydra hydra-buffer-menu (:color pink
                                    :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))
(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

;;; Spelling

;; To make spell-checking a document a little easier I've made a hydra.

;; When the hydra activates you can automatically switch on flyspell mode, but I
;; have this disabled because this is a very rare case;
(defun hydra-flyspell/pre ()
  ;;(flyspell-mode t)
  )

(defhydra hydra-flyspell (:pre hydra-flyspell/pre :color red)
  "Spelling"
  (";" flyspell-goto-next-error "Next")
  (":" flyspell-correct-at-point "Correct")
  ("q" nil "cancel" :color blue))

(bind-key "s-;" 'hydra-flyspell/body)

;;; EWW

(defhydra hydra-eww (:color pink)
  "eww browser"
  ("[" eww-back-url "Go Back")
  ("]" eww-forward-url "Go Forward")
  ("s-b" eww-browse-with-external-browser "Open in Browser")
  ("r" eww-reload "Reload")
  ("g" eww "URL/Search")
  ("s-RET" eww-open-in-new-buffer "Open in new buffer")
  ("Q" oht/kill-this-buffer "Kill eww" :color blue)
  ("k" scroll-down-line "scroll line up")
  ("j" scroll-up-line "scroll line down")
  ("n" shr-next-link "Next link")
  ("p" shr-previous-link "Previous link")
  ("q" exit "Exit" :color blue))



;;; EWW Browser

;; Emacs includes a (basic) browser, so you (mostly) don't have to leave Emacs. Sometimes I think Emacs's goal is to get you to use Emacs more...

;; The below is pretty much ripped directly from /Prot/'s config.

(use-package eww
  :init
  (setq eww-restore-desktop nil)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%u")
  (setq eww-download-directory "~/Downloads/")
  (setq eww-bookmarks-directory "~/.emacs.d/eww-bookmarks/")
  (setq eww-history-limit 150)
  (setq shr-max-image-proportion 0.7)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio/\\|application/pdf\\)")
  (setq url-cookie-trusted-urls '()
	url-cookie-untrusted-urls '(".*"))
  :commands eww
  :bind (:map eww-mode-map
	      ("s-\\ h" . hydra-eww/body)
  ))

(use-package browse-url
  :after eww
  :config
  ;;(setq browse-url-browser-function 'eww-browse-url)
  )

;;; Elfeed

(use-package elfeed
  :commands elfeed
  :bind (:map elfeed-search-mode-map
	      ("B" . ar/elfeed-search-browse-background-url)
	      ("m" . elfeed-toggle-star)
	      )
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-feeds
      '(("https://writings.stephenwolfram.com/feed/")
	("https://mjtsai.com/blog/feed/" mac)
	("https://panic.com/blog/feed/" mac)
	("https://routley.io/posts/index.xml")
	("https://xkcd.com/rss.xml")
	("https://kk.org/cooltools/feed/")
	("https://craigmod.com/index.xml")
	("http://100r.co/links/rss.xml")
	("https://www.economist.com/latest/rss.xml" news)
	("https://www.economist.com/the-economist-explains/rss.xml" news)
	("https://feeds.feedburner.com/marginalrevolution/feed" news)
	("https://standardebooks.org/rss/new-releases")
	("https://idlewords.com/index.xml")
	("https://www.raptitude.com/feed/")
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
	("https://blog.robenkleene.com/feed/atom/")
	))
  (defadvice elfeed-search-update (before nullprogram activate)
    (let ((feed (elfeed-db-get-feed "https://www.economist.com/latest/rss.xml")))
      (setf (elfeed-feed-title feed) "Economist: Latest"))
    (let ((feed (elfeed-db-get-feed "https://sachachua.com/blog/category/emacs-news/feed")))
      (setf (elfeed-feed-title feed) "emacs-news"))
    (let ((feed (elfeed-db-get-feed "https://craigmod.com/index.xml")))
      (setf (elfeed-feed-title feed) "Craig Mod"))
    (let ((feed (elfeed-db-get-feed "https://www.reddit.com/r/emacs/top/.rss")))
      (setf (elfeed-feed-title feed) "r/emacs"))
    )
  ;; Add the ability to "star" entries, this just adds the "star" tag
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))
  ;; function for opening entries in default browser
  (defun ar/elfeed-search-browse-background-url ()
    "Open current `elfeed' entry (or region entries) in browser without losing focus."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (mapc (lambda (entry)
              (assert (memq system-type '(darwin)) t "open command is macOS only")
              (start-process (concat "open " (elfeed-entry-link entry))
                             nil "open" "--background" (elfeed-entry-link entry))
              (elfeed-untag entry 'unread)
              (elfeed-search-update-entry entry))
            entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))
)


;;; View-Mode

(use-package view
  :ensure nil
  :init
  (setq view-read-only t)
  (setq view-inhibit-help-message t)
  :bind
  (:map view-mode-map
	("q" . nil)
	("n" . next-line)
	("p" . previous-line)
	;;("f" . forward-char)
	;;("b" . backward-char)
	("f" . forward-word)
	("b" . backward-word)
	("{" . backward-paragraph)
	("}" . forward-paragraph)
	("(" . backward-sentence)
	(")" . forward-sentence)
	("s" . ctrlf-forward-fuzzy)
	("r" . ctrlf-backward-fuzzy)
	)
  )

;; Show "View" in mode-line when in 'view-mode'
(setq minions-direct '(view-mode))


;;; Smart Escape

;; Ever try to quit the minibuffer while the point is in another window? This
;; function makes that possible.
;; https://with-emacs.com/posts/tips/quit-current-context/

(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         ;; if we got this far just use the default so we don't miss
         ;; any upstream changes
         (keyboard-quit))))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)


