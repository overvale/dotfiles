;;; Functions
;;  --------------------------------------------------------

(defun oht/writing-mode ()
  "Enable variable-pitch, flyspell, and increased line-spacing and margins."
  (interactive)
  (cursor-color "#24B0FF")
  (variable-pitch-mode t)
  (flyspell-mode t)
  (setq-local line-spacing 0.15)
  ;; define width of buffer margins
  (setq-default left-margin-width 1 right-margin-width 1)
  ;;(set-window-buffer nil (current-buffer)) ; Use them now.
  )

;;; Mode Hooks

(defun oht/markdown-mode-hook ()
  (oht/writing-mode)
  )

(defun oht/org-mode-hook ()
  (oht/writing-mode)
  )

(defun oht/emacs-lisp-mode ()
  (outline-minor-mode t)
  (rainbow-delimiters-mode t)
  )

;;; Functions

(defun oht/fix-variable-org-indent ()
  "Fix for org-indent not hiding markup in org-indent-mode.
from: https://maxjmartin.com/Emacs%20Dotfile.html"
  (interactive)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  )

(defun oht/counsel-find-settings ()
  "Quickly open ~/dot/emacs/settings.el"
  (interactive)
  (find-file "~/dot/emacs/settings.el"))

(defun oht/counsel-find-org ()
  "Quickly open ~/Documents/org-files/"
  (interactive)
  (counsel-find-file "~/Documents/org-files/"))

(defun oht/kill-this-buffer ()
  "Quickly kill current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun oht/find-scratch ()
  (interactive)
  (if (string= (buffer-name) "*scratch*")
       (previous-buffer)
     (switch-to-buffer "*scratch*")))

;; Move Lines
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)
(defun move-line-up ()
  "Move the current line up by 1 line"
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))
(defun move-line-down ()
  "More the current line down by 1 line"
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun oht/mark-whole-line ()
  "Mark the entirety of the current line."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(defun oht/toggle-window-split ()
  "Toggle between vertical and horizontal split."
  ;; Source: https://www.emacswiki.org/emacs/ToggleWindowSplit.
  ;; Author: Jeff Dwork
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun oht/open-in-bbedit ()
  "Open current file or dir in BBEdit.
Adapted from:
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) (expand-file-name default-directory ) )))
    (message "path is %s" $path)
    (string-equal system-type "darwin")
      (shell-command (format "open -a BBEdit \"%s\"" $path))))

(defun oht/expand-to-beginning-of-visual-line ()
  "Set mark and move to beginning of visual line"
  (interactive)
  (set-mark-command nil)
  (beginning-of-visual-line)
  )
(defun oht/expand-to-end-of-visual-line ()
  "Set mark and move to end of visual line"
  (interactive)
  (set-mark-command nil)
  (end-of-visual-line)
  )

(defun oht/kill-line-backward ()
  "Kill from the point to beginning of visual line"
  (interactive)
  (kill-line 0))

(defun oht/toggle-line-numbers ()
    "Toggles display of line numbers. Applies to all buffers."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode)))

(defun oht/toggle-whitespace ()
    "Toggles display of indentation and space characters."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))

(defun oht/open-line-below (arg)
  "Open a new indented line below the current one."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun oht/open-line-above (arg)
  "Open a new indented line above the current one."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun oht/join-line-next ()
  (interactive)
  (join-line -1))


;;; Hydra
;;  --------------------------------------------------------

;; Hydras should be reserved for modes,
;; ie: places where you'll want to call several functions in a row.
;; If all you're doing is grouping similar commands
;; then which-key should suffice.

;; Transpose
;; There are so many ways to transpose in Emacs, why not get help?
(defhydra hydra-transpose (:color red)
  "Transpose"
  ("c" transpose-chars "characters")
  ("w" transpose-words "words")
  ("o" org-transpose-words "Org mode words")
  ("l" transpose-lines "lines")
  ("s" transpose-sentences "sentences")
  ("e" org-transpose-elements "Org mode elements")
  ("p" transpose-paragraphs "paragraphs")
  ("t" org-table-transpose-table-at-point "Org mode table")
  ("q" nil "cancel" :color blue))

;; Buffer-menu
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

;; VIM-POWER functions
(defun hydra-vim/pre ()
  "When activating the hydra-vim, change the cursor to a box"
  (set-default 'cursor-type 'box))
(defun hydra-vim/post ()
    "When exiting the hydra-vim, change the cursor to a bar"
  (set-default 'cursor-type 'bar))

;; the VIM-POWER Hydra!
;; Since the color is set to amaranth, only actions labeled :blue will quit
(defhydra hydra-vim (:columns 4 :pre hydra-vim/pre :post hydra-vim/post :color amaranth)
  "VIM POWER"
  ("h" backward-char "left")
  ("l" forward-char "right")
  ("j" next-line "next")
  ("k" previous-line "previous")
  ("b" backward-word "previous word")
  ("e" forward-word "next word")
  ("0" beginning-of-visual-line "start of line")
  ("$" end-of-visual-line "end of line")
  ("{" backward-paragraph "back paragraph")
  ("}" forward-paragraph "forward paragraph")
  ("(" backward-sentence "back sentence")
  (")" forward-sentence "forward sentence")
  ("v" set-mark-command "mark")
  ("o" exchange-point-and-mark "swap point/mark")
  ("C-v" rectangle-mark-mode "rectangle mark")
  ("d" delete-region "del" :color blue)
  ("y" kill-ring-save "yank" :color blue)
  ("/" swiper-isearch "search forward")
  ("?" swiper-isearch-backward "search backward")
  ("C-l" recenter-top-bottom "cycle recenter")
  ("q" nil "cancel" :color blue))

;; Window Management
(defhydra hydra-windows (:color red)
  "Windows & Splits"
  ("<tab>" other-window "Cycle active window")
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   "Vertical Split")
  ("s" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   "Split, Horizonal")
  ("o" delete-other-windows "Only This Window" :color blue)
  ("k" delete-window "Delete Window")
  ("r" oht/toggle-window-split "Rotate Window Split")
  ("b" balance-windows "Balance")
  ("<up>" enlarge-window "Bigger VERT")
  ("<down>" shrink-window "Smaller VERT")
  ("=" enlarge-window-horizontally "Bigger HORZ")
  ("-" shrink-window-horizontally "Smaler HORZ")
  ("q" nil "cancel" :color blue))

;; Spelling
(defun hydra-flyspell/pre ()
  ;;(flyspell-buffer)
  )

(defhydra hydra-flyspell (:pre hydra-flyspell/pre :color red)
  "Spelling"
  (";" flyspell-goto-next-error "Next")
  (":" flyspell-correct-word-before-point "Correct")
  ("q" nil "cancel" :color blue))

;; Text Manipulation
(defhydra hydra-manipulate (:color red)
  "Manipulate Text"
  ("|" shell-command-on-region "Pipe to shell" :color blue)
  ("j" oht/join-line-next "Join line with next")
  ("d" downcase-region "Downcase")
  ("u" upcase-region "Upcase")
  ("c" capitalize-region "Capitalise")
  ("s" sort-lines "Sort")
  ("-" delete-duplicate-lines "Del Dupes")
  ("q" nil "cancel" :color blue))


;;; Keybindings
;;  --------------------------------------------------------

;; Keybindings Philosophy
;;
;; 1. Standard mac shortcuts should be supported wherever possible.
;;    And since mac inherits a lot of emacs keybindings anyway, this
;;    makes a lot of sense.
;;
;; 2. Global Leader - I borrow the concept of a "leader key" from vim
;;    and put every custom function I can there. This prevents
;;    conflicts with existing bindings and, since I'm using which-key,
;;    helps me remember the possibilities.
;;
;; 3. Uniform Mode-Spesific Leader - All mode-spesific bindings (for
;;    example org-time-stamp), which don't make any sense elsewhere
;;    should go behind a uniform leader key.
;;
;; 4. Keybindings which I use all the time, get taken out from behind
;;    leaders for faster access.

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Standard Mac Shortcuts
;; https://support.apple.com/en-us/HT201236
(global-set-key (kbd "s-,") 'oht/counsel-find-settings)
(global-set-key (kbd "s-n") 'make-frame-command)
(global-set-key (kbd "s-s") 'save-buffer)         ;save
(global-set-key (kbd "s-S") 'write-file)          ;save as
(global-set-key (kbd "M-s-s") 'save-some-buffers) ;save others
(global-set-key (kbd "s-o") 'counsel-find-file)
(global-set-key (kbd "M-s-o") 'counsel-buffer-or-recentf)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-<backspace>") 'oht/kill-line-backward)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-w") 'oht/kill-this-buffer)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-<up>") (kbd "M-<"))
(global-set-key (kbd "s-<down>") (kbd "M->"))
(global-set-key (kbd "s-l") 'oht/mark-whole-line)
(global-set-key (kbd "s-M-l") 'mark-paragraph)
(global-set-key (kbd "s-]") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-[") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "S-s-<left>") 'oht/expand-to-beginning-of-visual-line)
(global-set-key (kbd "S-s-<right>") 'oht/expand-to-end-of-visual-line)
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "M-s-f") 'swiper-all)
(global-set-key (kbd "S-s-f") 'counsel-ag)
(global-set-key (kbd "s-<return>") 'oht/open-line-below)
(global-set-key (kbd "S-s-<return>") 'oht/open-line-above)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)

;; Enhancements to emacs standards
;; -------------------------------
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
;; readline-style shortcuts, because I love them
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-u") 'oht/kill-line-backward)
;; No reason not to use command-u for this
(global-set-key (kbd "s-u") 'universal-argument)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-o") 'counsel-outline)
(global-set-key (kbd "M-t") 'hydra-transpose/body)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-<tab>") 'other-window)

;; HYDRA mini-modes
(global-set-key (kbd "s-j") 'hydra-vim/body)
(global-set-key (kbd "s-k") 'hydra-windows/body)
(global-set-key (kbd "s-;") 'hydra-flyspell/body)
(global-set-key (kbd "s-|") 'hydra-manipulate/body)

;; Most frequenctly used, therefore no leader
(global-set-key (kbd "s-a") 'org-agenda)
(global-set-key (kbd "s-p") 'counsel-M-x)
(global-set-key (kbd "s-b") 'counsel-ibuffer)
(global-set-key (kbd "M-s-b") 'ibuffer)
(global-set-key (kbd "s-e") 'er/expand-region)
(global-set-key (kbd "s-m") 'magit-status)

;; GLOBAL LEADER
(global-set-key (kbd "s-' c") 'org-capture)
(global-set-key (kbd "s-' f") 'oht/counsel-find-org)

;; org-mode bindings
;; You'll notice all these use the mode-spesific-leader
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "s-\\ v") 'oht/fix-variable-org-indent)
  (define-key org-mode-map (kbd "s-\\ .") 'org-time-stamp)
  (define-key org-mode-map (kbd "s-\\ t") 'org-todo)
  (define-key org-mode-map (kbd "s-\\ s-t") 'counsel-org-tag)
  (define-key org-mode-map (kbd "s-\\ g") 'counsel-org-goto-all)
  (define-key org-mode-map (kbd "s-\\ n") 'org-narrow-to-subtree)
  (define-key org-mode-map (kbd "s-\\ w") 'widen)
  (define-key org-mode-map (kbd "s-\\ s") 'org-search-view)
  (define-key org-mode-map (kbd "s-\\ <") 'org-insert-structure-template)
  )

;; buffer menu bindings
(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

