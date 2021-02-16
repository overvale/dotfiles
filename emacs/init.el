;;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; To see the outline of this file, run M-x outline-minor-mode (which I have
;; created a global minor mode for, thus it is always on) and then use the
;; `outline' functions to move around. I have created 3 separate ways of
;; making this easier.
;;
;; 1. The package `bicycle' provides the command `bicycle-cycle'.
;;    I've used these bindings:
;;    a) "S-<tab>" to cycle globally.
;;    b) "C-<tab>" to cycle at point.
;; 2. A hydra called `hydra-outline' (s-0)
;; 3. An `occur' buffer with the query: [^;;; ]


;;; Preamble


;;;; Performance

;; Let's start by doing everything we can to improve emacs performance.
;; All of the below are stolen (with comments) from Doom Emacs.
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; https://github.com/hlissner/doom-emacs/blob/develop/core/core.el

;; This is actually the 2nd step, for the first step, see `early-init.el'
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)
    (garbage-collect)) t)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(setq command-line-x-option-alist nil)

;; When testing startup performance, it can be useful to print a message
;; saying how long it took to start up.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;;;; Straight Use Package

;; Required Bootstrap to ensure it is installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package integration

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)


;;;; Basics

;; Always follow symlinks. init files are normally stowed/symlinked.
(setq vc-follow-symlinks t
      find-file-visit-truename t
      ;; Avoid stale compiled code shadow newer source code
      load-prefer-newer t)

;; Set encoding to be UTF-8 everywhere. I've seen a lot of conflicting
;; information about what's needed to use UTF-8 everywhere. Mastering Emacs
;; seems like a trustworthy source.
;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Starting the scratch buffer in `fundamental-mode', rather than, say,
      ;; `org-mode' or `text-mode', which pull in a ton of packages.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;;;; Remember

;; The below replaces Emacs's regular *scratch* buffer with the remember-notes
;; file/buffer, and sets some functions/bindings for quickly adding
;; information to it. Think of it like a super lightweight org-mode capture
;; whose inbox you see every time Emacs starts up.

;; + remember-notes -- visits remember file
;; + remember -- capture
;; + C-u remember -- capture region, with prompt
;; + remember-region -- capture region, without prompt
;; + remember-clipboard -- capture clipboard, with prompt

;; Set the location of the remember-notes file.
(setq remember-data-file "~/home/org/remember-notes")

;; Auto-save the remember-notes file, and name the buffer *scratch*
(setq remember-notes-auto-save-visited-file-name t
      remember-notes-buffer-name "*scratch*")

;; Set initial-buffer-choice to a function which kills the *scratch* buffer
;; and opens the remember-notes buffer.
(setq initial-buffer-choice
      (lambda () (kill-buffer remember-notes-buffer-name)
                 (remember-notes)))

(defun oht-remember-dwim ()
  "If the region is active, capture with region, otherwise just capture."
  (interactive)
  (if (use-region-p)
      (let ((current-prefix-arg 4)) (call-interactively 'remember))
    (remember))
  )

(bind-keys
 ("s-_" . oht-remember-dwim)
 ("s--" . remember-notes)
 )

;;; Settings

(global-auto-revert-mode t)                ; update buffer when file on disk changes
(save-place-mode 1)                        ; reopens the file to the same spot you left
(recentf-mode 1)                           ; enables "Open Recent..." in file menu
(setq tab-width 4)                         ; tabs=4 char
(setq help-window-select t)                ; focus new help windows when opened
(setq sentence-end-double-space nil)       ; ends sentence after 1 space
(fset 'yes-or-no-p 'y-or-n-p)              ; Changes all yes/no questions to y/n type
(setq create-lockfiles nil)                ; No need for ~ files when editing
(setq-default fill-column 78)              ; Set column used for fill-paragraph
(setq ring-bell-function 'ignore)          ; Don't beep

;; Set a variable for where your emacs dotfiles are located.
(defvar oht-dotfiles "~/home/dot/emacs/")

;; Add lisp files to the load path.
(add-to-list 'load-path (concat oht-dotfiles "lisp/"))

;; When customizing Emacs interactively (ie: not in this document or init.el)
;; Emacs appends code to your init.el file, which can be annoying when editing
;; it by hand. This tells Emacs to place these customizations in a separate
;; file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; place backup files in a single place
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))

; Use Spotlight to search with M-x locate
(setq locate-command "mdfind")

;;Use the system trash folder to delete files.
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash/emacs")

;; Echo unfinished commands after this delay
;; setting to 0 means do not echo commands
(setq echo-keystrokes 0.1)

;; When exiting emacs, kill all running processes
(setq confirm-kill-processes nil)

;; Since emacs seems to love spawning new windows, and taking over your existing
;; ones, this allows you to undo and redo those arrangements. So you if a
;; command kills a window arrangement you were using you can go back to it with
;; winner-undo and winner-redo.
(winner-mode 1)

;; An example: you call `switch-buffer' and search for something in the
;; minibuffer. You then want to call a command inside that minibuffer.
(setq enable-recursive-minibuffers 1)


(defun sanemacs/backward-kill-word ()
  (interactive "*")
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(global-set-key (kbd "M-DEL") 'sanemacs/backward-kill-word)    ; Kill word without copying it to your clipboard
(global-set-key (kbd "C-DEL") 'sanemacs/backward-kill-word)    ; Kill word without copying it to your clipboard


;;; Packages


;;;; Critical Packages

;; These packages are relied upon by lots of things that come after, therefore
;; they must come first.

(use-package bind-key
  :demand
  )

(use-package blackout
  :demand
  :config
  (blackout 'eldoc-mode)
  (blackout 'emacs-lisp-mode "Elisp")
  (blackout 'auto-fill-function " Fill")
  )

;;;; Other Packages

(use-package hydra
  :bind
  ("s-F" . hydra-fonts/body)
  ("s-2" . hydra-secondary-selection/body)
  ("s-0" . hydra-outline/body)
  :config
  (defhydra hydra-fonts (:exit nil :foreign-keys warn)
    "Set Font Properties"
    ("d" oht-fonts-set "Default Font")
    ("v" variable-pitch-mode "Variable")
    ("=" text-scale-increase "Larger")
    ("+" text-scale-increase "Larger")
    ("-" text-scale-decrease "Smaller")
    ("0" text-scale-mode "Reset Size")
    ("s" oht/set-line-spacing "Line Spacing")
    ("m" modus-themes-toggle "Modus Toggle")
    ("o" olivetti-mode "Olivetti")
    ("w" visual-line-mode "Wrap")
    ("c" composition-mode "Comp")
    ("q" nil "cancel"))
  (defhydra hydra-secondary-selection (:exit t)
    "Secondary Selection"
    ("xx" oht/cut-secondary-selection "Cut 2nd")
    ("cc" oht/copy-secondary-selection "Copy 2nd")
    ("xv" oht/cut-secondary-selection-paste "Cut 2nd & Paste")
    ("cv" oht/copy-secondary-selection-paste "Copy 2nd & Paste")
    ("m" (lambda () (interactive)(secondary-selection-from-region)) "Mark as 2nd")
    ("g" (lambda () (interactive)(secondary-selection-to-region)) "Goto 2nd")
    ("q" nil "cancel"))
  (defhydra hydra-outline (:foreign-keys warn)
    "Hydra for navigating outline mode"
    ("o" outline-hide-sublevels "Hide to This Sublevel")
    ("<tab>" bicycle-cycle "Show Subtree")
    ("a" outline-show-all "Show All")
    ("c" consult-outline "Consult" :exit t)
    ("n" outline-next-visible-heading "Next")
    ("p" outline-previous-visible-heading "Previous")
    ("q" nil "Cancel" :exit t)
    )
  )

;; this package creates a report each time you startup
;; You'll need to add ':demand' and restart emacs to see the report
(use-package benchmark-init
  ;; :demand
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package org
  :commands org-mode
  :config
  (load (concat oht-dotfiles "lisp/oht-org.el"))
  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))
  ;; :hook (org-mode . variable-pitch-mode)
  :bind (:map org-mode-map
	      ("s-\\ o" . consult-outline)
	      ("s-\\ ." . oht/org-insert-date-today)
	      ("s-\\ t" . org-todo)
	      ("s-\\ n" . org-narrow-to-subtree)
	      ("s-\\ w" . widen)
	      ;;("s-\\ s" . org-search-view)
	      ("s-\\ <" . org-insert-structure-template)
	      ("s-\\ s" . org-store-link)
	      ("s-\\ i" . org-insert-last-stored-link)
	      ("s-\\ m" . visible-mode)
	      ("s-\\ I" . org-clock-in)
	      ("s-\\ O" . org-clock-out)
	      ("s-\\ a" . org-archive-subtree)
	      ("s-\\ r" . org-refile)
	      ("s-\\ g" . org-goto)
	      ("s-\\ c" . org-toggle-checkbox)
	      ))

(use-package bookmark
  :straight nil
  :commands (list-bookmarks)
  :init
  (defun oht-bookmark-fonts ()
    (hl-line-mode)
    )
  :hook (bookmark-bmenu-mode . oht-bookmark-fonts)
  :custom
  (bookmark-save-flag 1)
  (bookmark-default-file "~/home/bookmarks")
  )

(use-package magit
  :commands magit-status
  )

(use-package exec-path-from-shell
  :demand
  )

(use-package olivetti
  :commands olivetti-mode
  :custom (olivetti-body-width 84)
  :blackout " Olvti"
  )

(use-package which-key
  :demand
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0.4)
  :blackout
  )

(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo)
  )

(use-package expand-region
  :bind
  ("s-r" . er/expand-region)
  ("s-R" . er/contract-region)
  )

(use-package zzz-to-char
  ;; replaces zap-to-char with an avy-like interface
  ;; note that it searches forward and backward
  :bind ("M-z" . zzz-up-to-char))

(use-package sdcv-mode
  :straight (sdcv-mode :type git :host github :repo "gucong/emacs-sdcv" :branch "master")
  :commands sdcv-search
  )

(use-package unfill
  :commands (unfill-paragraph unfill-toggle unfill-region)
  :bind
  ("M-q" . unfill-toggle)
  )

(use-package whole-line-or-region
  ;; When no region is active (nothing is selected), and you invoke the
  ;; kill-region (cut) or kill-ring-save (copy) commands, Emacs acts on the
  ;; range of characters between the mark and the point. This is a really good way
  ;; to accidentally kill half your document. I have done this more times than I'd
  ;; like to admit. This package makes it so that without a region those commands
  ;; act on a whole line.
  :init
  (whole-line-or-region-global-mode 1)
  :blackout whole-line-or-region-local-mode
  )

(use-package helpful
  ;; Neat package that brings together a lot of useful information when you
  ;; ask Emacs for help. https://github.com/Wilfred/helpful
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h F" . #'helpful-function)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-h C" . #'helpful-command)
  ("C-h p" . #'helpful-at-point)
  :bind*
  ("C-?" . #'help-command)
 )

(use-package pinboard
  :commands (pinboard pinboard-add pinboard-add-for-later)
  :init
  (setf epa-pinentry-mode 'loopback)
  )

(use-package move-text
  :bind
  ("M-<up>" . move-text-up)
  ("M-<down>" . move-text-down)
  )

;;;; Languages

(use-package fountain-mode
  :commands fountain-mode
  :custom
  (fountain-add-continued-dialog nil)
  (fountain-highlight-elements (quote (section-heading)))
  )

(use-package markdown-mode
  :commands markdown-mode)

(use-package lua-mode
  :commands lua-mode)


;;;; Spelling

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode turn-on-flyspell)
  :init
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (setq ispell-program-name "/usr/local/bin/aspell")
  (customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-list-command "list")
  :blackout " Spell"
)

(use-package flyspell-correct
  ;; Allows you to pass spelling suggestions to completion
  ;; and search frameworks, such as selectrum. This setup code is copied
  ;; directly from the selectrum documentation.
  :bind
  ("M-;" . #'flyspell-auto-correct-previous-word)
  ("M-:" . #'flyspell-correct-wrapper)
  :custom
  (flyspell-correct-interface 'flyspell-correct-dummy)
  :init
  (advice-add 'flyspell-correct-dummy :around
	      (defun my--fsc-wrapper (func &rest args)
		(let ((selectrum-should-sort-p nil))
		  (apply func args))))
  )


;;; My Pseudo-Packages

;; The code in each of the files in ~/.emacs/lisp/ is available to
;; `use-package' because, at the beginning of this file, I've added the path
;; to my 'load-path'. To actually use the code in those files you need to do
;; two things:
;;
;; 1. Tell `straight' not to install it.
;; 2. Make sure `use-package' loads the file when needed. If the code is only
;;    needed when a command is called, you should name the `:command'. You can
;;    also `:demand' that the file be loaded, or say that it should be loaded
;;    `:after' another package.

(use-package oht-dispatch
  :straight nil
  :commands (oht-dispatch)
  :config
  (setq oht-dispatch-functions
	'(remember-notes
	  elfeed
	  org-agenda
	  list-bookmarks
	  ))
  :bind
  ("s-d" . #'oht-dispatch)
  )

(use-package oht-functions
  :straight nil
  :demand
  )

(use-package oht-mac
  :straight nil
  :demand
  )

(use-package oht-composition
  :straight nil
  :commands (composition-mode)
  )

;;; Dired

(use-package dired
  :straight nil
  :commands (dired dired-jump dired-jump-other-window)
  :init
  (setq dired-use-ls-dired nil) ; no more warning message
  :config
  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "open" nil 0 nil file)
      (message "Opening %s done" file)))
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (dired-hide-details-mode 1)
	      (auto-revert-mode)
	      (hl-line-mode 1)
	      ))
  :bind (:map dired-mode-map
	      ("O" . dired-open-file)
	      )
  )

(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))


;;; IBuffer

(use-package ibuffer
  :straight nil
  :commands ibuffer
  :config
  (defun oht-ibuffer-hook ()
    (hl-line-mode 1)
    )
  :hook (ibuffer-mode . oht-ibuffer-hook)
  )

;;; Transient Mark
;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode

(use-package oht-transient-mark
  :straight nil
  :config
  (defun oht-activate-or-swap-mark ()
    "If no region is active, activate it. If a region is active swap the point and mark."
    (interactive)
    (if (use-region-p)
	(exchange-point-and-mark)
      (activate-mark)))
  (defun push-mark-no-activate ()
    "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
    (interactive)
    (push-mark (point) t nil)
    (message "Pushed mark to ring"))
  (defun jump-to-mark ()
    "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
    (interactive)
    (set-mark-command 1))
  :bind
  ("C-x C-x" . oht-activate-or-swap-mark)
  ("C-`" . push-mark-no-activate)
  ("M-`" . jump-to-mark)
  )

;;; Keybindings

(define-key global-map (kbd "RET") 'newline-and-indent)

(bind-keys ("M-s-s" . save-some-buffers)
	   ("M-c" . capitalize-dwim)
	   ("M-l" . downcase-dwim)
	   ("M-u" . upcase-dwim)
	   ("M-SPC" . cycle-spacing)
	   ("M-/" . hippie-expand)
	   ("C-l" . oht/recenter-top-bottom)
	   ("C-=" . pulse-line)
	   ("C-x r r" . replace-rectangle)
	   )

(bind-key* "M-o" 'oht/other-window)

(bind-keys ("s-p" . execute-extended-command)
	   ("s-k" . kill-this-buffer)
	   ("s-B" . ibuffer)
	   ("M-s-b" . list-bookmarks)
	   ("s-C" . org-capture)
	   ("s-|" . oht/pipe-region)
	   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
	   ("s-1" . org-agenda)
	   ("s-/" . comment-or-uncomment-region-or-line)
	   ("s-l" . oht-mac-mark-whole-line)
	   ("s-<return>" . oht-mac-open-line-below)
	   ("S-s-<return>" . oht-mac-open-line-above)
	   )

(bind-keys :prefix-map oht/global-leader
	   :prefix "s-'"
	   ("a" . auto-fill-mode)
	   ("d" . sdcv-search)
	   ("h" . hl-line-mode)
	   ("l" . oht/toggle-line-numbers)
	   ("w" . visual-line-mode)
	   ("t" . toggle-truncate-lines)
	   ("W" . oht/toggle-whitespace)
	   ("m" . magit-status)
	   ("M" . consult-marks)
	   ("<left>" . winner-undo)
	   ("<right>" . winner-redo)
	   ("s" . org-store-link)
	   ("o" . consult-outline)
	   ("!" . font-lock-mode)
	   ("j" . dired-jump)
	   ("b s" . bookmark-set)
	   ("b l" . list-bookmarks)
	   ("b j" . consult-bookmark)
	   ("c" . composition-mode)
	   )

(bind-keys :prefix-map oht/windows-leader
	   :prefix "s-w"
	   ("s" . oht/split-below)
	   ("v" . oht/split-beside)
	   ("k" . oht/delete-window)
	   ("o" . delete-other-windows)
	   ("b" . balance-windows)
	   ("r" . oht/rotate-window-split)
	   ("t" . tear-off-window)
	   )


;;; Appearance

;;;; Display Settings

;; This is actually in both early-init.el and here because it being in
;; early-init prevents the scroll bar from showing up when you start Emacs,
;; but you need it here to prevent it from showing in new frames. ¯\_(ツ)_/¯
(scroll-bar-mode -1)

(menu-bar-mode 1)                          ; ensures full-screen avail on macOS
(show-paren-mode t)                        ; highlight parens
(setq show-paren-delay 0)                  ; and show immediately
(setq visible-bell t)                      ; disable beep
(setq-default frame-title-format '("%b"))  ; show buffer name in titlebar
(setq x-underline-at-descent-line nil)     ; underline at descent, not baseline
(setq-default indicate-empty-lines nil)    ; show where the file ends
(set-default 'cursor-type 'box)            ; use a box for cursor
(blink-cursor-mode -1)                     ; no blinking please

;;;; Mode Line

(column-number-mode t)
(setq display-time-format "%H:%M  %Y-%m-%d")
(setq display-time-interval 60)
(setq display-time-mail-directory nil)
(setq display-time-default-load-average nil)
(display-time-mode t)


;;;; Fonts

;; Line spacing (in pixels)
(setq-default line-spacing nil)

;; When using `text-scale-incraese', this sets each 'step' to about one point size.
(setq text-scale-mode-step 1.08)

;; Keep in mind that when in variable-pitch-mode the fixed-pitch size is based
;; not on the default size but the variable-pitch size.
(defun oht-fonts-set ()
  "Sets default, variable-pitch, and fixed-pitch fonts to my liking."
  (interactive)
  (set-face-attribute 'default nil
		      :family "Iosevka" :height 130 :weight 'normal :width 'expanded)
  (set-face-attribute 'variable-pitch nil
		      :family "Iosevka Sparkle" :height 1.0 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :family "Iosevka" :height 1.0 :weight 'normal :width 'expanded)
  )

(defun oht/set-line-spacing (&optional arg)
  "Buffer local, set the line spacing. Takes an argument, 0 by default"
  (interactive "P")
  (setq-local line-spacing arg)
  )

;; This sets the default fonts
(oht-fonts-set)


;;;; Themes

(use-package modus-themes
  :demand
  :init
  (setq
   modus-themes-bold-constructs nil
   modus-themes-slanted-constructs t
   modus-themes-syntax 'alt-syntax
   modus-themes-links 'faint-neutral-underline
   modus-themes-prompts 'subtle
   modus-themes-mode-line 'borderless
   modus-themes-completions 'moderate
   modus-themes-region 'bg-only
   modus-themes-diffs 'desaturated
   modus-themes-org-blocks 'grayscale
   modus-themes-scale-headings nil
   modus-themes-variable-pitch-ui t
   modus-themes-variable-pitch-headings nil
   )
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  )

(use-package tron-legacy-theme)


;;; Narrowing & Searching/Replacing

;; Navigating and using the thousands of things Emacs can do is built around
;; the idea of searching and narrowing a selection down to the thing you're
;; looking for. To make this easier I've installed a few packages that enhance
;; Emacs built-in facilities for doing this.

;; There are MANY approaches to this. I'm following the popular current trend
;; lead by @oantolin, @raxod502, @minad, and @protesilaos. Generally, it works
;; like this:
;;
;; 1. Use Selectrum as the main interface for completion narrowing (by default).
;; 2. Use Prescient to sort those completions and provide fuzzy matching.
;; 3. Use Marginalia to decorate the completions.
;; 4. Use Embark so you can act on the list of completions (among other things).
;; 5. Fall back on Orderless for use in Embark minibuffers.
;; 6. Use Consult and Consult-Selectrum to enable new commands.

(use-package orderless
  :straight (:host github :repo "oantolin/orderless" :branch "master")
  :custom (completion-styles '(orderless)))

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum" :branch "master")
  :init
  (selectrum-mode +1)
  )

(use-package selectrum-prescient
  :after selectrum
  :init
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1)
  )

(use-package marginalia
  :straight (:type git :host github :repo "minad/marginalia" :branch "main")
  :bind (:map minibuffer-local-map
         ("M-q" . marginalia-cycle))
  :init
  (marginalia-mode 1)
  (setq marginalia-annotators
	'(marginalia-annotators-heavy marginalia-annotators-light))
  ;; if using Selectrum
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  )

(use-package embark
  ;; This setup for Embark is admittedly a little complex at first glance. It
  ;; is designed to integrate Embark with Selectrum. Additionally, the
  ;; functions `use-embark-completions' and `use-selectrum-completions'
  ;; provide everything needed to switch between using Selectrum and Embark
  ;; for completions.
  :straight (:host github :repo "oantolin/embark" :branch "master")
  :bind
  ("s-e" . embark-act)
  (:map minibuffer-local-map
        ("C-o" . embark-switch-to-collect-completions))
  (:map embark-url-map
	("d" . youtube-dl-URL-at-point))
  :config
  (add-to-list 'embark-keymap-alist '(straight . embark-straight-map))
  (setq embark-collect-initial-view-alist
        '((file . list)
          (buffer . list)
          (symbol . list)
          (line . list)
          (xref-location . list)
          (kill-ring . zebra)
          (t . list)))

  ;; Show which-key help when you call Embark
  (setq embark-action-indicator
	(lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
	embark-become-indicator embark-action-indicator)

  ;; Resize Completions Buffer
  (add-hook 'embark-collect-post-revert-hook
          (defun resize-embark-collect-window (&rest _)
            (when (memq embark-collect--kind '(:live :completions))
              (fit-window-to-buffer (get-buffer-window)
                                    (floor (frame-height) 2) 1))))

  ;; Pause Selectrum while using embark-collect-live
  (add-hook 'embark-collect-mode-hook
	    (defun pause-selectrum ()
	      (when (eq embark-collect--kind :live)
		(with-selected-window (active-minibuffer-window)
		  (shrink-window selectrum-num-candidates-displayed)
		  (setq-local selectrum-num-candidates-displayed 0)))))

  )

(defun use-embark-completions ()
  "Use Embark for completions.

These hooks make it so that the embark-live-collect buffer is
automatically displayed after you type something. Keep in mind that this
separate from listing all the potential completions when you press TAB.

That means pressing TAB when there are multiple possible
candidates (or is empty) will result in BOTH the
embark-live-collect and *completions* buffers being shown. When
there is only one candidate, however, TAB will complete."
  (interactive)
  (selectrum-mode -1)
  (add-hook 'minibuffer-setup-hook 'embark-collect-completions-after-input)
  (add-hook 'embark-post-action-hook 'embark-collect--update-linked)
  (add-hook 'embark-collect-mode-hook 'hl-line-mode)
  )

(defun use-selectrum-completions ()
  "Use Selectrum for completions.

This simply removes the hooked added by the function `use-embark-completions'."
  (interactive)
  (selectrum-mode 1)
  (remove-hook 'minibuffer-setup-hook 'embark-collect-completions-after-input)
  (remove-hook 'embark-post-action-hook 'embark-collect--update-linked)
  (remove-hook 'embark-collect-mode-hook 'hl-line-mode)
  )

(use-package consult
  :straight (:type git :host github :repo "minad/consult" :branch "main")
  :bind
  ("s-b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ("M-s-o" . consult-recent-file)
  ("s-f" . consult-line)
  ([remap yank-pop] . consult-yank-pop)
  :config
  (setq consult-preview-key (kbd "C-<return>"))
  )

(use-package embark-consult
  :straight nil
  :after (embark consult)
  ;; :demand ; only necessary if you have the hook below
  ;; ;; if you want to have consult previews as you move around an
  ;; ;; auto-updating embark collect buffer
  ;; :hook
  ;; (embark-collect-mode . embark-consult-preview-minor-mode)
  )

(use-package ctrlf
  :bind
  ("C-s" . ctrlf-forward-fuzzy)
  ("C-r" . ctrlf-backward-fuzzy)
  :config
  (setq ctrlf-minibuffer-bindings
        `(("C-n"          . ctrlf-next-match)
          (,(kbd "C-p")   . ctrlf-previous-match)
	  (,(kbd "M-O")   . ctrlf-occur)
	  (,(kbd "M-c")   . ctrlf-toggle-case-fold-search)
	  (,(kbd "M-r")   . ctrlf-toggle-regexp)
	  (,(kbd "C-o s") . ctrlf-change-search-style)
	  ))
  )

(use-package visual-regexp
  ;; Provides an alternate version of `query-replace' which highlights matches
  ;; and replacements as you type.
  :bind (([remap query-replace] . #'vr/query-replace)))

(use-package visual-regexp-steroids
  ;; Allows `visual-regexp' to use regexp engines other than Emacs'; for
  ;; example, Python or Perl regexps.
  :after visual-regexp
  :bind (([remap query-replace-regexp] . #'radian-query-replace-literal))
  :init
  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs)
  (defun radian-query-replace-literal ()
    "Do a literal query-replace using `visual-regexp'."
    (interactive)
    (let ((vr/engine 'emacs-plain))
      (call-interactively #'vr/query-replace)))
  )


;;; Secondary Selection

;; A few functions for working with the Secondary Selection. The primary way I
;; interact with these is through a hydra.

(defun oht/cut-secondary-selection ()
  "Cut the secondary selection."
  (interactive)
  (mouse-kill-secondary))

(defun oht/copy-secondary-selection ()
  "Copy the secondary selection."
  (interactive)
  ;; there isn't a keybinding-addressable function to kill-ring-save
  ;; the 2nd selection so here I've made my own. This is extracted
  ;; directly from 'mouse.el:mouse-secondary-save-then-kill'
  (kill-new 
   (buffer-substring (overlay-start mouse-secondary-overlay)
		     (overlay-end mouse-secondary-overlay))
   t))

(defun oht/cut-secondary-selection-paste ()
  "Cut the secondary selection and paste at point."
  (interactive)
  (mouse-kill-secondary)
  (yank))

(defun oht/copy-secondary-selection-paste ()
  "Copy the secondary selection and paste at point."
  (interactive)
  (oht/copy-secondary-selection)
  (yank))


;;; Outline

;; `outline' provides major and minor modes for collapsing sections of a
;; buffer into an outline-like format. Let's turn that minor mode into a
;; global minor mode and enable it.
(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)
(global-outline-minor-mode +1)
(blackout 'outline-minor-mode)

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle))
  )

(bind-keys :map emacs-lisp-mode-map
	   ("<backtab>" . bicycle-cycle-global))


;;; View-Mode

(use-package view
  ;; Coming from vim, I know the utility of modal editing. However, I have no
  ;; desire to make Emacs into something it is not with `evil-mode'. That
  ;; said, there are times when modal editing and navigation are very handy
  ;; and there are, in fact, circumstances in which Emacs uses modal
  ;; navigation. For example, when in a dired buffer you can use the n and p
  ;; keys to move the point around. In addition to the bindings below
  ;; `view-mode' has its own standard bindings, which you can find in the
  ;; minor mode's help.
  :straight nil
  :init
  ;; Visit read-only buffers in view-mode
  (setq view-read-only t)
  (defun oht/view-mode-enter ()
    (interactive)
    (view-mode t)
    (hl-line-mode t)
    )
  (defun oht/view-mode-exit ()
    (interactive)
    (view-mode -1)
    (hl-line-mode -1)
    )
  (defun oht/exit-view-replace-rectangle ()
    (interactive)
    (oht/view-mode-exit)
    (call-interactively 'replace-rectangle)
    )
  (bind-key "s-j" 'oht/view-mode-enter)
  :bind
  (:map view-mode-map
	("n" . next-line)
	("p" . previous-line)
	("f" . forward-char)
	("b" . backward-char)
	("F" . forward-word)
	("B" . backward-word)
	("a" . beginning-of-visual-line)
	("e" . end-of-visual-line)
	("{" . backward-paragraph)
	("}" . forward-paragraph)
	("(" . backward-sentence)
	(")" . forward-sentence)
	("s" . ctrlf-forward-fuzzy)
	("r" . ctrlf-backward-fuzzy)
	("m" . set-mark-command)
	("[" . scroll-down-line)
	("]" . scroll-up-line)
	("M" . rectangle-mark-mode)
	("R" . oht/exit-view-replace-rectangle)
	("x" . exchange-point-and-mark)
	("<RET>" . oht/view-mode-exit)
	("s-j" . oht/view-mode-exit)
	("q" . oht/view-mode-exit)
	)
  :blackout " VIEW"
  )


;;; EWW Browser

(use-package eww
  :init
  (setq eww-restore-desktop nil)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%t %u")
  (setq eww-download-directory "~/Downloads/")
  (setq eww-bookmarks-directory "~/.emacs.d/eww-bookmarks/")
  (setq eww-history-limit 150)
  (setq shr-max-image-proportion 0.7)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio/\\|application/pdf\\)")
  (setq url-cookie-trusted-urls '()
	url-cookie-untrusted-urls '(".*"))
  ;; Make eww Emacs's default browser, if you remove this the system default
  ;; will be used instead.
  (setq browse-url-browser-function 'eww-browse-url)
  :commands eww
  :bind
  (:map eww-mode-map
	("k" . kill-this-buffer)
	("b" . bookmark-set)
	)
  :custom
  (shr-width 80)
  )

(use-package oht-eww
  :straight nil
  :demand
  )


;;; Elfeed

(use-package elfeed
  :commands elfeed
  :hook (elfeed-show-mode . oht-elfeed-show-fonts)
  :init
  (load (concat oht-dotfiles "lisp/oht-elfeed-pre.el"))
  :config
  (load (concat oht-dotfiles "lisp/oht-elfeed-post.el"))
  :bind
  (:map elfeed-search-mode-map
	("a" . hrs/elfeed-pinboard-current-entry)
	("b" . elfeed-search-browse-url)
	("m" . elfeed-search-toggle--star)
	)
  (:map elfeed-show-mode-map
	("a" . hrs/elfeed-pinboard-current-entry)
	("&" . bjm/elfeed-show-visit-gui)
	;; TODO toggle star binding
	("r" . elfeed-show-tag--read)
	("u" . elfeed-show-tag--unread)
	("d" . oht-elfeed-show-download-video)
	)
  )


;;; Youtube-dl

;; A few utilities for working with videos

(setq youtube-dl-path "/usr/local/bin/youtube-dl")
(setq youtube-dl-output-dir "~/Desktop/")

(defun youtube-dl-URL-at-point ()
  "Send the URL at point to youtube-dl."
  (interactive)
  (async-shell-command (format "%s -o \"%s%s\" -f best \"%s\""
                               youtube-dl-path
                               youtube-dl-output-dir
                               "%(title)s.%(ext)s"
                               (ffap-url-at-point))))

;;; Closing

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
