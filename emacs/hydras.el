;; -*- lexical-binding: t -*-

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
  ("J" unfill-region "Unfill region")
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

