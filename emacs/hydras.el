;; -*- lexical-binding: t -*-

;;; Hydras

;; Hydras should be reserved for mini-modes, /ie/ places where you'll want to call several functions in a row. If all you're doing is grouping similar commands then which-key should suffice.

;;;; Info: Hydra Colors

;; [[https://github.com/abo-abo/hydra/wiki/Hydra-Colors][Official Documentation]]

;; | Color    | Defined keys        | Other keys          |
;; |----------+---------------------+---------------------|
;; | red      | Accept and Continue | Accept and Exit     |
;; | pink     | Accept and Continue | Accept and Continue |
;; | amaranth | Accept and Continue | Reject and Continue |
;; | teal     | Exit                | Reject and Continue |
;; | blue     | Exit                | Accept and Exit     |

;;;; Text Manipulation

;; These commands pretty much require a region.

(defhydra hydra-manipulate (:color teal)
  "Manipulate Text"
  ("|" oht/shell-command-on-region-replace "Pipe to shell")
  ("j" oht/join-line-next "Join line with next" :color red)
  ("J" unfill-region "Unfill region")
  ("d" downcase-region "Downcase")
  ("u" upcase-region "Upcase")
  ("c" capitalize-region "Capitalise")
  ("s" sort-lines "Sort")
  ("-" delete-duplicate-lines "Del Dupes")
  ("q" nil "cancel"))

;;;; Transpose
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

;;; Smart Occur

;; From [[https://oremacs.com/2015/01/26/occur-dwim/][oremacs]]. This will offer as the default candidate:

;; - the current region, if it's active
;; - the current symbol, otherwise

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; and a hydra to go with it
;; TODO update hydra formatting to defhydra
(defhydra occur-hydra (:color amaranth)
  "Create/Navigate occur errors"
  ("o" occur-dwim "occur")
  ("f" first-error "first")
  ("n" next-error "next")
  ("p" previous-error "prev")
  ("q" exit "exit" :color blue))
(bind-key "s-' o" 'occur-hydra/body)

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

;;; Modal Editing Hydra

;; hydra-modal functions
(defun hydra-modal/pre ()
  "When activating the hydra-modal, change the cursor to a box"
  (set-face-attribute 'cursor nil :background "red")
)

(defun hydra-modal/post ()
  "When exiting the hydra-modal, change the cursor to a bar"
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
  ("<DEL>" kill-region "kill region")
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
  ("<f12>" nil "cancel" :color blue)
  ("i" nil "cancel" :color blue))

(bind-key "s-j" 'hydra-modal/body)

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

