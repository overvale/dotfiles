;;; Dired

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


;;; Buffer Menu

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


;;; Startup Profiling

;; this package creates a report each time you startup
;; You'll need to add ':demand t' and restart emacs to see the report
(use-package benchmark-init
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;; Profiling Tools
(bind-keys :prefix-map oht/profiling-keys
	   :prefix "M-s-p"
	   ("s" . profiler-start)
	   ("q" . profiler-stop)
	   ("r" . profiler-report)
	   )


;; Toggle mode-line
(use-package hide-mode-line
  :commands hide-mode-line-mode
)

;; Use iMenu across all open buffers
(use-package imenu-anywhere
  :commands (imenu-anywhere)
)

(use-package fountain-mode
  :commands fountain-mode
  :custom
  (fountain-add-continued-dialog nil)
  (fountain-highlight-elements (quote (section-heading)))
  )
  
(use-package markdown-mode
  :commands markdown-mode)



;;;; General

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
	    (auto-revert-mode)
	  ))



;;; Auto-complete

;; I've tried a few completion packages and they've all left me cold.
;; =hippy-expand= generally gets me what I want, but I'd like the pop-up list
;; to use the completion framework. Some googling led me to this fucntion,
;; built for ivy, which I've modified for use with =selectum=.

;; https://gist.github.com/JohnLunzer/7c6d72a14c76c0a3057535e4f6148ef8
(defun my-hippie-expand-completions (&optional hippie-expand-function)
  "Return list of completions generated by `hippie-expand'."
  (save-excursion
    (let ((this-command 'my-hippie-expand-completions)
          (last-command last-command)
          (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'my-hippie-expand-completions)
               (not (equal he-num -1))))
      ;; Provide the options in the order in which they are normally generated.
      (delete he-search-string (reverse he-tried-table)))))

(defun my-hippie-expand-with (hippie-expand-function)
  "Offer completion using the specified hippie-expand function."
  (let* ((options (my-hippie-expand-completions hippie-expand-function)))
    (if options
        (progn
          (if (> (safe-length options) 1)
              (setq selection (completing-read "Completions: " options))
            (setq selection (car options)))
          (if selection
              (he-substitute-string selection t)))
      (message "No expansion found"))))

(defun my-hippie-expand ()
  "Offer completion for the word at point."
  (interactive)
  (my-hippie-expand-with 'hippie-expand))

(global-set-key (kbd "M-/") 'my-hippie-expand)

;; In Emacs, =TAB= is used by a lot of languages for indentation (org
;; particularly). Completion is triggered with =M-TAB=. But the setting
;; =tab-always-indent 'complete= will tell Emacs to first try to indent the
;; line, and if it's already indented trigger =completion-at-point=.

(setq tab-always-indent 'complete)

;; The following adds file-path completion to the completion framework. So
;; when your point is on something that looks like a file-path Emacs will
;; offer file-path completions. This technique is taken from
;; [[https://with-emacs.com/posts/tutorials/customize-completion-at-point/][(with-emacs]].

;; This is especially nice with selectrum which won’t exit file completions after each path level so you can conveniently navigate to the path like you would do with find-file.

(autoload 'ffap-file-at-point "ffap")
(defun complete-path-at-point+ ()
  "Return completion data for UNIX path at point."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table :exclusive 'no))))

(add-hook 'completion-at-point-functions
          #'complete-path-at-point+
          'append)

;; The two blocks of code above are taken from:
;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/

;; I've not enabled the below code because I'm still testing it, but it
;; basically creates a function witch reads and caches a file with a list of
;; words and offers completions from it when you call a custom function. It is
;; taken from
;; [[https://emacs.stackexchange.com/questions/37423/completion-by-fuzzy-search-in-large-dictionary-file-displaying-candidates-inlin/37446#37446][this
;; stackexchange message]].
;; 
;; (defun my-dictionary ()
;;   "Return hash-table whose keys comprise words.txt."
;;   (with-temp-buffer
;;     (insert-file-contents "/usr/share/dict/words")
;;     (let ((table (make-hash-table :test #'equal :size 466544)))
;;       (while (not (eobp))
;;         (puthash (buffer-substring (point) (line-end-position)) nil table)
;;         (forward-line))
;;       table)))
;; 
;; (defvar my-dictionary
;;   (lazy-completion-table my-dictionary my-dictionary)
;;   "Lazy completion table for function `my-dictionary'.")
;; 
;; ;; this is the function to call to execute the completion
;; (defun my-complete-word-in-region ()
;;   "Complete word preceding point under `my-dictionary'."
;;   (interactive)
;;   (completion-in-region
;;    (save-excursion
;;      (skip-syntax-backward "w")
;;      (point))
;;    (point)
;;    my-dictionary))

;;; Snippets

(use-package yasnippet
  :bind (:map yas-minor-mode-map
	      ("TAB" . nil)
	      ([tab] . nil))
  ("s-y" . yas-expand)
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :custom
  (yas-snippet-dirs '("~/dot/emacs/snippets/" user-emacs-directory))
  (yas-verbosity 2)
  :config
  (yas-reload-all)
  (blackout 'yas-minor-mode)
  )

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


;;; Performance Profiling

(use-package benchmark-init
  ;; this package creates a report each time you startup You'll need to add
  ;; ':demand t' and restart emacs to see the report
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  )

;; Profiling Tools
(bind-keys :prefix-map oht/profiling-keys
	   :prefix "M-s-p"
	   ("s" . profiler-start)
	   ("q" . profiler-stop)
	   ("r" . profiler-report)
	   )

;;; Modal Editing Hydra

;; hydra-modal functions
(defun hydra-modal/pre ()
  (set-face-attribute 'cursor nil :background "red")
)

(defun hydra-modal/post ()
  (set-face-attribute 'cursor nil :background "black")
)

(defhydra hydra-modal (:hint none :pre hydra-modal/pre :post hydra-modal/post :color pink)
  "** MODAL EDITING **"
  ;; move one character
  ("h" backward-char "left")
  ("l" forward-char "right")
  ("j" next-line "next")
  ("k" previous-line "previous")
  ;; move larger
  ("C-h" backward-word "previous word")
  ("C-l" forward-word "end of next word")
  ("b" backward-word "previous word")
  ("w" oht/forward-word-beginning "beginning of next word")
  ("e" forward-word "end of next word")
  ("C-k" backward-paragraph "back paragraph")
  ("C-j" forward-paragraph "forward paragraph")
  ("u" beginning-of-visual-line "start of line")
  ("p" end-of-visual-line "end of line")
  ("0" beginning-of-visual-line "start of line")
  ("$" end-of-visual-line "end of line")
  ("/" ctrlf-forward-fuzzy "search forward")
  ("?" ctrlf-backward-fuzzy "search backward")
  ;; edit
  ("J" oht/join-line-next "join")
  ("y" kill-ring-save "Copy")
  ("P" yank "paste")
  ("<DEL>" oht/kill-region-or-char "kill region")
  ("d" oht/kill-region-or-char "kill region")
  ("D" kill-line "Kill to EOL")
  ("c" oht/kill-region-or-char "change" :color blue)
  ("C" kill-line "change to EOL" :color blue)
  ("I" beginning-of-visual-line "append" :color blue)
  ("a" forward-char "append" :color blue)
  ("A" end-of-visual-line "append line" :color blue)
  ("o" oht/open-line-below "open below" :color blue)
  ("O" oht/open-line-above "open above" :color blue)
  ("!" hydra-manipulate/body "manipulate" :color blue)
  ;; view
  ("z" recenter-top-bottom "cycle recenter")
  ("[" scroll-down-line "scroll line up")
  ("]" scroll-up-line "scroll line down")
  ("{" scroll-down-command "scroll up")
  ("}" scroll-up-command "scroll down")
  ;; select
  ("v" set-mark-command "mark")
  ("V" oht/mark-whole-line "mark whole line")
  ("C-v" rectangle-mark-mode "rectangle mark")
  ("C-r" replace-rectangle "replace rectangle")
  ("x" exchange-point-and-mark "swap point/mark")
  ;; exit
  ("s-j" nil "cancel" :color blue)
  ("i" nil "cancel" :color blue))

(bind-key "s-j" 'hydra-modal/body)

;; Hydras should be reserved for mini-modes, /ie/ places where you'll want to call several functions in a row. If all you're doing is grouping similar commands then which-key should suffice.

;;; Info: Hydra Colors

;; [[https://github.com/abo-abo/hydra/wiki/Hydra-Colors][Official Documentation]]

;; | Color    | Defined keys        | Other keys          |
;; |----------+---------------------+---------------------|
;; | red      | Accept and Continue | Accept and Exit     |
;; | pink     | Accept and Continue | Accept and Continue |
;; | amaranth | Accept and Continue | Reject and Continue |
;; | teal     | Exit                | Reject and Continue |
;; | blue     | Exit                | Accept and Exit     |

;;; Text Manipulation

;; These commands pretty much require a region.

(defhydra hydra-manipulate (:color teal)
  "Manipulate Text"
  ("|" oht/shell-command-on-region-replace "Pipe to shell")
  ("j" oht/join-line-next "Join line with next" :color red)
  ("j" unfill-region "unfill region")
  ("d" downcase-region "Downcase")
  ("u" upcase-region "Upcase")
  ("c" capitalize-region "Capitalise")
  ("s" sort-lines "Sort")
  ("-" delete-duplicate-lines "Del Dupes")
  ("q" nil "cancel"))

;;; Transpose

;; There are so many ways to transpose in Emacs, why not get help?

(defhydra hydra-transpose (:color blue)
  "Transpose"
  ("c" transpose-chars "characters")
  ("w" transpose-words "words")
  ("o" org-transpose-words "Org mode words")
  ("l" transpose-lines "lines")
  ("s" transpose-sentences "sentences")
  ("e" org-transpose-elements "Org mode elements")
  ("p" transpose-paragraphs "paragraphs")
  ("t" org-table-transpose-table-at-point "Org mode table")
  ("x" transpose-sexps "s expressions")
  ("q" nil "cancel"))


;;; Window Management

(defhydra hydra-windows (:color red)
  "Windows & Splits"
  ("<tab>" other-window "Cycle active window")
  ("v" oht/split-beside "Vertical Split")
  ("s" oht/split-below "Split, Horizonal")
  ("o" delete-other-windows "Only This Window" :color blue)
  ("k" delete-window "Delete Window")
  ("r" oht/toggle-window-split "Rotate Window Split")
  ("b" balance-windows "Balance")
  ("[" shrink-window "Smaller VERT")
  ("]" enlarge-window "Bigger VERT")
  ("{" shrink-window-horizontally "Smaler HORZ")
  ("}" enlarge-window-horizontally "Bigger HORZ")
  ;; move TO other windows
  ("<up>" windmove-up "Move UP")
  ("<down>" windmove-down "Move DOWN")
  ("<left>" windmove-left "Move LEFT")
  ("<right>" windmove-right "Move RIGHT")
  ;; move WINDOWS in arrangement
  ("M-s-<left>" buf-move-left "Shift Left")
  ("M-s-<right>" buf-move-right "Shift Right")
  ("M-s-<up>" buf-move-up "Shift Up")
  ("M-s-<down>" buf-move-down "Shift Down")
  ("q" nil "cancel" :color blue))

