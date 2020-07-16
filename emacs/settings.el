;; NOTE:
;; Many ideas stolen from:
;; https://github.com/aaronbieber/dotfiles/tree/master/configs/emacs.d
;; https://protesilaos.com/dotemacs/

(setq package-archives
      '(;; Restore gnu back to https when they fix their cert.
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ;;("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ))


;;; General Settings
;;  --------------------------------------------------------

;; Set default encoding
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; When visiting 2 files with the same name, use this:
;; foo/makefile & bar/makefile
;; instead of this: makefile<foo/foo> & makefile<bar/bar>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Startup preferences
(setq initial-scratch-message
      (concat
       ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
       ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n"
       ";; WELCOME TO EMACS\n"))
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)


;;; What you might call "App Preferences"

(menu-bar-mode 1) ;this effects full-screen
(tool-bar-mode -1)
(show-paren-mode t)
(setq show-paren-delay 0)
(delete-selection-mode t)
(global-auto-revert-mode t)
;;(desktop-save-mode 1) ;sessions

(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq visible-bell t)
(setq vc-follow-symlinks t)
(setq tab-width 4)

;; Tell Emacs to use paths/apps unique to my setup
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-list-command "list")


;;; Mac-Like Settings

;; Command to super
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)
;; Left-option to meta for commands
(setq mac-option-modifier 'meta)
(setq mac-left-option-modifier 'meta)
;; Right-option to option, for special characters: ¡™£¢∞¶•ªº
(setq mac-right-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)


;;; Apperance
;;  --------------------------------------------------------

(set-default 'cursor-type 'bar)
(global-visual-line-mode t) ; word-wrap
;; with visual-line-mode set C-a and C-b
;; go to beginning/end-of-visual-line
;; which is inconsistant with standard Mac behaviour
(global-set-key (kbd "C-a") 'beginning-of-line)
(global-set-key (kbd "C-e") 'end-of-line)

;; Modus theme customizations
(setq modus-operandi-theme-bold-constructs t
      modus-vivendi-theme-bold-constructs t
      modus-operandi-theme-slanted-constructs t
      modus-vivendi-theme-slanted-constructs t
      )

;; Setup functions for my favorite fonts
(defun font-iosevka ()
  (set-face-attribute 'default nil
		      :family "Iosevka" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "Iosevka Sparkle" :height 140 :weight 'normal))

(defun font-fira ()
  (set-face-attribute 'default nil
		      :family "Fira Mono" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "Fira Sans" :height 150 :weight 'normal))

(defun font-roboto ()
  (set-face-attribute 'default nil
		      :family "Roboto Mono" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "Roboto Sans" :height 150 :weight 'normal))

(defun font-sf ()
  (set-face-attribute 'default nil
		      :family "SF Mono" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "New York Medium" :height 160 :weight 'normal))

(defun font-ibm ()
  (set-face-attribute 'default nil
		      :family "IBM Plex Mono" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "IBM Plex Serif" :height 150 :weight 'normal))

(font-ibm)
(load-theme 'modus-operandi t)


;;; Mode Settings
;;  --------------------------------------------------------

;; Activate the modes/packages I like to use
(which-key-mode t)              ;hints
(global-undo-tree-mode t)       ;activate undo-tree everywhere
(require 'expand-region)        ;lovely plugin

;; counsel settings
(counsel-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
;(setq ivy-initial-inputs-alist nil) ;removes ^ from initial input

(add-hook 'markdown-mode-hook
  (lambda ()
    (variable-pitch-mode t)
    (flyspell-mode t)
    )
  t)

(add-hook 'org-mode-hook
  (lambda ()
    (variable-pitch-mode 1)
    (flyspell-mode)
    (org-indent-mode)
    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
    )
  t)

(add-hook 'emacs-lisp-mode
  (lambda ()
    (outline-minor-mode t)
    )
  t)

;;; Org Settings
;;  --------------------------------------------------------

;; This adds [COMPLETED: DATE] and logs status changes
(setq org-log-done 'time)

;; This prevents editing inside folded sections
(setq org-catch-invisible-edits 'show-and-error)

;; This sets the sequence of plain list bullets
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "*") ("*" . "+")))

;; Increase sub-item indentation by this amount
;; the default is 2 so the below means 2+2 = 4 (spaces)
(setq org-list-indent-offset 2)

(setq org-agenda-window-setup 'current-window)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "WAIT(w)" "|" "CANCELLED(c)")))

(setq org-agenda-custom-commands '(

	("d" "Do Now - Not scheduled, #A, TODO"
         ((agenda "d" ((org-agenda-span 'day)))
          (tags "+PRIORITY={A}/TODO"
                ((org-agenda-overriding-header "Tasks you should do NOW:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                 ))))

	("c" "Complete - Agenda and ALL todos"
         ((agenda "")
          (todo "TODO|WAIT"
           ((org-agenda-overriding-header "Global list of TODO items of type: ALL (non-scheduled)")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
            ))
          ))

        ))

(setq org-agenda-files
      (quote ("~/Documents/org-files/")))

(setq org-refile-targets
      '((org-agenda-files :maxlevel . 3)))
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-capture-templates
      '(("p" "Personal Inbox" entry
	 (file+headline "~/Documents/org-files/refile.org" "Personal")
	 "* %?\n\n")
	("P" "Personal Log Entry" entry
	 (file "~/Documents/org-files/logbook.org")
	 "* %?\n%t\n\n")
	("i" "Ingenuity Inbox" entry
	 (file+headline "~/Documents/org-files/refile.org" "Ingenuity")
	 "* %?\n\n")
	("I" "Ingenuity Log Entry" entry
	 (file "~/Documents/org-files/ing_log.org")
	 "* %^{Log type|Meeting: |Call: } %? %t\n\n")
	))

	

;;; Functions
;;  --------------------------------------------------------

(defun oht/fix-variable-org-indent ()
  "Fix for org-indent not hiding markup in org-indent-mode.
from: https://maxjmartin.com/Emacs%20Dotfile.html"
  (interactive)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  )

(defun oht/find-file-settings ()
  "Quickly open ~/dot/emacs/settings.el"
  (interactive)
  (find-file "~/dot/emacs/settings.el"))

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
(global-set-key
 (kbd "M-t")
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
   ("q" nil "cancel" :color blue)))

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

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)


;; VIM-POWER functions
(defun hydra-vim/pre ()
  "When activating the hydra-vim, change the cursor to a box"
  (set-default 'cursor-type 'box))
(defun hydra-vim/post ()
    "When exiting the hydra-vim, change the cursor to a bar"
  (set-default 'cursor-type 'bar))

;; the VIM-POWER Hydra!
;; Since the color is set to amaranth, only actions labeled :blue will quit
(global-set-key
 (kbd "s-j")
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
 )

;; Window Management
(global-set-key
 (kbd "s-k")
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
   ("q" nil "cancel" :color blue)))

;; Spelling
(defun hydra-flyspell/pre ()
  (flyspell-buffer))

(global-set-key
 (kbd "s-;")
 (defhydra hydra-flyspell (:pre hydra-flyspell/pre :color red)
   "Spelling"
   (";" flyspell-goto-next-error "Next")
   (":" flyspell-correct-word-before-point "Correct")
   ("q" nil "cancel" :color blue)))

;; Text Manipulation
(global-set-key
 (kbd "s-\\")
 (defhydra hydra-manipulate (:color red)
   "Manipulate Text"
   ("|" shell-command-on-region "Pipe to shell" :color blue)
   ("j" oht/join-line-next "Join line with next")
   ("d" downcase-region "Downcase")
   ("u" upcase-region "Upcase")
   ("c" capitalize-region "Capitalise")
   ("s" sort-lines "Sort")
   ("-" delete-duplicate-lines "Del Dupes")
   ("q" nil "cancel" :color blue)))


;;; Keybindings
;;  --------------------------------------------------------

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "C-S-k") 'oht/kill-line-backward)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-o") 'counsel-outline)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-<tab>") 'other-window)

;;                    s-j    hydra-vim
;;                    s-k    hydra-windows
;;                    s-;    hydra-flyspell
(global-set-key (kbd "s-a") 'org-agenda)
(global-set-key (kbd "s-p") 'counsel-M-x)
(global-set-key (kbd "s-b") 'counsel-ibuffer)
(global-set-key (kbd "M-s-b") 'list-buffers)
(global-set-key (kbd "s-o") 'counsel-find-file)
(global-set-key (kbd "M-s-o") 'counsel-buffer-or-recentf)
(global-set-key (kbd "s-e") 'er/expand-region)
(global-set-key (kbd "s-m") 'magit-status)
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "M-s-f") 'swiper-all)
(global-set-key (kbd "C-M-s-f") 'counsel-ag)
(global-set-key (kbd "s-<return>") 'oht/open-line-below)
(global-set-key (kbd "S-s-<return>") 'oht/open-line-above)

(global-set-key (kbd "s-g f") 'counsel-org-goto-all)
(global-set-key (kbd "s-g c") 'org-capture)
(global-set-key (kbd "s-g t") 'counsel-org-tag)
(global-set-key (kbd "s-g n") 'org-narrow-to-subtree)
(global-set-key (kbd "s-g w") 'widen)
(global-set-key (kbd "s-g s") 'org-search-view)
(global-set-key (kbd "s-g ,") 'org-insert-structure-template)

(global-set-key (kbd "s-.") 'org-time-stamp)
(global-set-key (kbd "s-t") 'org-todo)
(global-set-key (kbd "s-0") 'oht/find-scratch)

;; Standard Mac Shortcuts
;; https://support.apple.com/en-us/HT201236
(global-set-key (kbd "s-,") 'oht/find-file-settings) ;preferences
(global-set-key (kbd "s-n") 'make-frame-command)
(global-set-key (kbd "s-s") 'save-buffer)         ;save
(global-set-key (kbd "s-S") 'write-file)          ;save as
(global-set-key (kbd "M-s-s") 'save-some-buffers) ;save others
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-w") 'oht/kill-this-buffer)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-<up>") (kbd "M-<"))
(global-set-key (kbd "s-<down>") (kbd "M->"))
(global-set-key (kbd "s-<left>") (kbd "C-a"))
(global-set-key (kbd "s-<right>") (kbd "C-e"))
(global-set-key (kbd "s-l") 'oht/mark-whole-line)
(global-set-key (kbd "s-M-l") 'mark-paragraph)
(global-set-key (kbd "s-]") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-[") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "S-s-<left>") 'oht/expand-to-beginning-of-visual-line)
(global-set-key (kbd "S-s-<right>") 'oht/expand-to-end-of-visual-line)


(provide 'settings)
;; settings.el ends here
