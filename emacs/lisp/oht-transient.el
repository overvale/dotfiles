;; oht-transient.el --- -*- lexical-binding: t -*-


(define-transient-command oht-transient-general ()
  "General shortcuts I use often"
  [
   [ "Misc"
     ("d" "Dictionary" sdcv-search)
     ("f" "Find File" find-file)
     ("m" "Magit Status" magit-status)
     ("s" "Store Org Link" org-store-link)
     ("o" "Outline" consult-outline)
     ("j" "Dired Jump" dired-jump)
     ("c" "Composition Mode" composition-mode)
     ("H" "Help..." oht-transient-help)
     ("S" "Spelling..." oht-transient-spelling)
     ]
   [ "Display"
     ("h" "Highlight Line" hl-line-mode)
     ("l" "Line Numbers" global-display-line-numbers-mode)
     ("g" "Fill Column" global-display-fill-column-indicator-mode)
     ("w" "Wrap" visual-line-mode)
     ("a" "AutoFill" auto-fill-mode)
     ("T" "Truncate" toggle-truncate-lines)
     ("W" "Whitespace" whitespace-mode)
     ("F" "Fonts..." oht-transient-fonts)
     ]
   [ "Bookmarks"
     ("b s" "Set" bookmark-set)
     ("b l" "List" list-bookmarks)
     ("b j" "Jump" consult-bookmark)
     ]
   ["Tab Bar Mode"
    ("t t" "Tab Bar Mode" tab-bar-mode)
    ("t n" "New" tab-bar-new-tab)
    ("t k" "Kill" tab-bar-close-tab)
    ("t z" "Undo Kill" tab-bar-undo-close-tab)
    ("t ]" "Next" tab-bar-switch-to-next-tab)
    ("t [" "Previous" tab-bar-switch-to-prev-tab)
    ]
   ]
  )

(define-transient-command oht-transient-help ()
    "Help commands that I use. A subset of C-h with others thrown in."
    ["Help Commands"
     ["Mode & Bindings"
      ("m" "Mode" describe-mode)
      ("b" "Major Bindings" which-key-show-full-major-mode)
      ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
      ("t" "Top Bindings  " which-key-show-top-level)
      ]
     ["Describe"
      ("C" "Command" helpful-command)
      ("f" "Function" helpful-callable)
      ("v" "Variable" helpful-variable)
      ("k" "Key" helpful-key)
      ("c" "Key Briefly" describe-key-briefly)
      ]
     ["Goto Source"
      ("L" "Library" find-library-other-frame)
      ("F" "Function" find-function-other-frame)
      ("V" "Variable" find-variable-other-frame)
      ("K" "Key" find-function-on-key-other-frame)
      ]
     ] ;; end help
    [
     ["Describe"
      ("s" "Symbol" helpful-symbol)
      ("." "At Point   " helpful-at-point)
      ("w" "Where Is" where-is)
      ("=" "Position" what-cursor-position)
      ]
     ["Info Manuals"
      ("C-i" "Info" info)
      ("C-4" "Other Window " info-other-window)
      ]
     ]
    )

(define-transient-command oht-transient-window ()
  [
   ["Splits"
    ("s" "Horizontal" split-window-below)
    ("v" "Vertical"   split-window-right)
    ("b" "Balance"    balance-windows)
    ("r" "Rotate"     oht/rotate-window-split)
    ]
   ["Window"
    ("c" "Clone Indirect" clone-indirect-buffer)
    ("t" "Tear Off" tear-off-window)
    ("k" "Kill" delete-window)
    ("K" "Kill Buffer+Win"  kill-buffer-and-window)
    ("o" "Kill Others"  delete-other-windows)
    ("m" "Maximize" maximize-window)
    ]
   ["Move"
    ("<left>"  "←" windmove-left  :transient t)
    ("<right>" "→" windmove-right :transient t)
    ("<up>"    "↑" windmove-up    :transient t)
    ("<down>"  "↓" windmove-down  :transient t)
    ]
   ["Undo/Redo"
    ("s-z" "Undo" winner-undo :transient t)
    ("s-Z" "Redo" winner-redo :transient t)
    ]
   ]
  )

(define-transient-command oht-transient-org ()
  "Transient for Org Mode"
  [
   ["Navigation"
    ("o" "Outline" consult-outline)
    ("n" "Narrow" org-narrow-to-subtree)
    ("w" "Widen" widen)
    ("g" "Go To" org-goto)
    ("m" "Visible Markup" visible-mode)
    ]
   ["Item"
    ("t" "TODO" org-todo)
    ("I" "Clock In" org-clock-in)
    ("O" "Clock Out" org-clock-out)
    ("a" "Archive Subtree" org-archive-subtree)
    ("r" "Refile" org-refile)
    ("c" "Checkbox" org-toggle-checkbox)
    ]
   ["Insert"
    ("." "Insert Date" oht/org-insert-date-today)
    ("<" "Structure Template" org-insert-structure-template)
    ]
   ["Links"
    ("s" "Store Link" org-store-link)
    ("i" "Insert Link" org-insert-last-stored-link)
    ]])

(define-transient-command oht-transient-fonts ()
  "Set Font Properties"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Modes"
    ("v" "Var Mode" variable-pitch-mode)
    ("V" "V+ Mode" larger-variable-pitch-mode)
    ("o" "Olivetti" olivetti-mode)
    ("w" "Wrap" visual-line-mode)
    ("c" "Comp" composition-mode)
    ]
   ["Size"
    ("0" "Reset Size" text-scale-mode)
    ("=" "Larger" text-scale-increase)
    ("+" "Larger" text-scale-increase)
    ("-" "Smaller" text-scale-decrease)
    ]
   ["Other"
    ("s" "Line Spacing" oht-fonts-line-spacing)
    ("m" "Modus Toggle" modus-themes-toggle)]])


(define-transient-command oht-transient-2nd ()
  "Secondary Selection"
  [["Cut/Copy"
    ("xx" "Cut 2nd" oht/cut-secondary-selection)
    ("cc" "Copy 2nd" oht/copy-secondary-selection)]
   ["& Paste"
    ("xv" "Cut 2nd & Paste" oht/cut-secondary-selection-paste)
    ("cv" "Copy 2nd & Paste" oht/copy-secondary-selection-paste)]
   ["Mark"
    ("m"  "Mark Region as 2nd" oht/mark-region-as-secondary-selection)
    ("g"  "Make 2nd the Region" oht/mark-secondary-selection)
    ("d"  "Delete 2nd" oht/delete-secondary-selection)]])


(define-transient-command oht-transient-outline ()
  "Outline Navigation"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Show/Hide"
    ("<backtab>" "Global Toggle" bicycle-cycle-global)
    ("<tab>" "Toggle Children" bicycle-cycle)
    ("o"     "Hide to This Sublevel" outline-hide-sublevels)
    ("a"     "Show All" outline-show-all)]
   ["Navigate"
    ("n" "Next" outline-next-visible-heading)
    ("p" "Previous" outline-previous-visible-heading)]
   ["Edit"
    ("M-<left>"  "Promote" outline-promote)
    ("M-<right>" "Demote"  outline-demote)
    ("M-<up>"    "Move Up" outline-move-subtree-up)
    ("M-<down>"  "Move Down" outline-move-subtree-down)]
   ["Other"
    ("s-z" "Undo" undo-fu-only-undo)
    ("s-Z" "Redo" undo-fu-only-redo)
    ("c" "Consult" consult-outline :transient nil)]])

(define-transient-command oht-transient-spelling ()
  "Spelling Interface"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Toggle Modes"
    ("m" "Flyspell" flyspell-mode)
    ("M" "Prog Flyspell" flyspell-prog-mode)]
   ["Check"
    ("b" "Buffer" flyspell-buffer)
    ("r" "Region" flyspell-region)]
   ["Correction"
    ("n" "Next" flyspell-goto-next-error)
    ("<return>" "Fix" flyspell-correct-wrapper)
    ("<SPC>" "Auto Fix" flyspell-auto-correct-word)
    ("<DEL>" "Delete Word" kill-word)
    ("s-z" "Undo" undo-fu-only-undo)
    ("s-Z" "Redo" undo-fu-only-redo)]])


(define-transient-command oht-transient-dispatch ()
  [["Work"
    ("a" "Agenda" org-agenda)
    ("t" "Today's Agenda" oht-org-agenda-today)
    ("m" "Mail" mu4e)]
   ["Browsing"
    ("e" "Elfeed" elfeed)
    ("h" "Hacker News" hackernews)
    ("E" "EWW" prot-eww-browse-dwim)
    ("n" "NPR News" oht-dispatch-NPR-news)
    ("c" "CNN News" oht-dispatch-CNN-news)
    ("r" "Read Later..." oht-dispatch-read-later)]])


;; -------------------------------------------------------------

;; https://github.com/conao3/transient-dwim.el/

(define-transient-command oht-transient-dired
  "Dired"
  [["Marks"
    ("m" "Marks..." oht-transient-dired-marks)]
   ["Command"
    ("RET" "Open file"            dired-find-file)
    ("o" "  Open in other window" dired-find-file-other-window)
    ("C-o" "Open in other window (No select)" dired-display-file)
    ("v" "  Open file (View mode)"dired-view-file)
    ("=" "  Diff"                 dired-diff)
    ("j" "  Goto file"            dired-goto-file)
    ("w" "  Copy filename"        dired-copy-filename-as-kill)
    ("W" "  Open in browser"      browse-url-of-dired-file)
    ("y" "  Show file type"       dired-show-file-type)
    ("+" "  Create directory"     dired-create-directory)
    ("<" "  Jump prev directory"  dired-prev-dirline)
    (">" "  Jump next directory"  dired-next-dirline)
    ("^" "  Move up directory"    dired-up-directory)]
  ["Display"
    ("g" "  Refresh buffer"       revert-buffer)
    ("l" "  Refresh file"         dired-do-redisplay)
    ("k" "  Remove line"          dired-do-kill-lines)
    ("s" "  Sort"                 dired-sort-toggle-or-edit)
    ("(" "  Toggle detail info"     dired-hide-details-mode)
    ("i" "  Insert subdir"        dired-maybe-insert-subdir)
    ("$" "  Hide subdir"          dired-hide-subdir)
    ("M-$" "Hide subdir all"      dired-hide-subdir)]
   ["Attribute"
    ("R"   "Rename"               dired-do-rename)
    ("G"   "Group"                dired-do-chgrp)
    ("M"   "Mode"                 dired-do-chmod)
    ("O"   "Owner"                dired-do-chown)
    ("T"   "Timestamp"            dired-do-touch)]
   ["Extension"
    ("e"   "wdired"               wdired-change-to-wdired-mode)
    ("/"   "dired-filter"         ignore)
    ("n"   "dired-narrow"         ignore)]]
  )

(define-transient-command oht-transient-dired-marks
  "Marks"
  [["Mark"
    ("mm"  "Mark"                 dired-mark)
    ("mM"  "Mark all"             dired-mark-subdir-files)
    ("mu"  "Unmark"               dired-unmark)
    ("mU"  "Unmark all"           dired-unmark-all-marks)
    ("m*"  "Executables"          dired-mark-executables)
    ("m/"  "Directories"          dired-mark-directories)
    ("m@"  "Symlinks"             dired-mark-symlinks)
    ("m&"  "Garbage files"        dired-flag-garbage-files)
    ("m#"  "Auto save files"      dired-flag-auto-save-files)
    ("m~"  "backup files"         dired-flag-backup-files)
    ("m."  "Numerical backups"    dired-clean-directory)
    ("m%"  "Regexp"               dired-mark-files-regexp)
    ("mg"  "Regexp file contents" dired-mark-files-containing-regexp)
    ("mc"  "Change mark"          dired-change-marks)
    ("mt"  "Toggle mark"          dired-toggle-marks)]
   ["Command for marked files"
    ("x"   "Do action"            dired-do-flagged-delete)
    ("C"   "Copy"                 dired-do-copy)
    ("D"   "Delete"               dired-do-delete)
    ("S"   "Symlink"              dired-do-symlink)
    ("H"   "Hardlink"             dired-do-hardlink)
    ("P"   "Print"                dired-do-print)
    ("A"   "Find"                 dired-do-find-regexp)
    ("Q"   "Replace"              dired-do-find-regexp-and-replace)
    ("B"   "Elisp bytecompile"    dired-do-byte-compile)
    ("L"   "Elisp load"           dired-do-load)
    ("X"   "Shell command"        dired-do-shell-command)
    ("Z"   "Compress"             dired-do-compress)
    ("z"   "Compress to"          dired-do-compress-to)
    ("!"   "Shell command"        dired-do-shell-command)
    ("&"   "Async shell command"  dired-do-async-shell-command)]
   ])

;; -------------------------------------------------------------

;; IDEAS

;; org-agenda commands
;; ibuffer


(provide 'oht-transient)
