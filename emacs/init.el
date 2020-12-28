;;; init.el --- Oliver Taylor's Config -*- lexical-binding: t -*-

;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; To see the outline of this file, run M-x outline-minor-mode (which I have
;; created a global minor mode for, thus it is always on) and then use the
;; `outline' functions to move around. I have created 3 separate ways of
;; making this easier.
;;
;; 1. The package `bicycle' provides the command `bicycle-cycle'.
;; 2. A hydra called `hydra-outline'.
;; 3. An `occur' buffer with the query: [^;;; ]


;;; Preamble


;;;; Performance

;; Let's start by doing everything we can to improve emacs performance.
;; All of the below are stolen (with comments) from Doom Emacs.
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; https://github.com/hlissner/doom-emacs/blob/develop/core/core.el

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
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

;; When customizing Emacs interactively (ie: not in this document or init.el)
;; Emacs appends code to your init.el file, which can be annoying when editing
;; it by hand. This tells Emacs to place these customizations in a separate
;; file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; I have a bunch of misc custom functions that I keep in a separate file to
;; make this document a little cleaner. Some of the bindings further down in
;; this document depend on these functions, so I load them here.
(load "~/dot/emacs/functions.el")


;;;; Startup and Scratch

;; By default, Emacs starts up with a "splash screen" containing some useful
;; links, but I don't need that any more, so this turns it off.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; By default, Emacs always starts up with a *scratch* buffer that is NOT
;; intended to be saved; when you kill it or quit emacs the contents of your
;; *scratch* buffer are gone forever without warning. I don't think I'm alone in
;; thinking that this is not very useful. Thankfully, emacs 24.4 introduced a
;; replacement for the *scratch* buffer that is saved when emacs exits, thus
;; your scratch pad NEVER deletes data. And THAT makes a lot more sense to me.
;; More info:
;; https://www.masteringemacs.org/article/whats-new-in-emacs-24-4#remember

;; This sets the name of the buffer to *scratch*, which I suppose I'm just doing
;; for tradition's sake.
(setq remember-notes-auto-save-visited-file-name t
      remember-notes-buffer-name "*scratch*")
;; If you just tell initial-buffer-choice that you want to use remember-notes as
;; your initial buffer emacs will still create a *scratch* buffer that you'll
;; never use. So we set initial-buffer-choice to a function which kills the
;; scratch buffer and opens the remember-notes file.
(setq initial-buffer-choice
      (lambda () (kill-buffer remember-notes-buffer-name)
                 (remember-notes)))

;; Now set the location of the remember-notes file.
(setq remember-data-file "~/Documents/org-files/remember-notes")


;;; Settings

;;;; General

(global-auto-revert-mode t)                ; update buffer when file on disk changes
(save-place-mode 1)                        ; reopens the file to the same spot you left
(recentf-mode 1)                           ; enables "Open Recent..." in file menu
(add-to-list 'recentf-exclude "/elpa")     ; don't show package files in recentf
(setq tab-width 4)                         ; tabs=4 char
(setq help-window-select t)                ; focus new help windows when opened
(setq sentence-end-double-space nil)       ; ends sentence after 1 space
(fset 'yes-or-no-p 'y-or-n-p)              ; Changes all yes/no questions to y/n type
(setq create-lockfiles nil)                ; No need for ~ files when editing
(setq-default fill-column 78)              ; Set column used for fill-paragraph
(setq ring-bell-function 'ignore)          ; Don't beep

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

;; When visiting read-only buffers, enter view-mode
(setq view-read-only t)

;; Since emacs seems to love spawning new windows, and taking over your existing
;; ones, this allows you to undo and redo those arrangements. So you if a
;; command kills a window arrangement you were using you can go back to it with
;; winner-undo and winner-redo.
(winner-mode 1)

;; This makes it so that when you click/right-click on the mode-line
;; emacs scrolls the window. Why not? I actually like how Acme does this.
(global-set-key [mode-line mouse-1] 'scroll-up-command)
(global-set-key [mode-line mouse-3] 'scroll-down-command)


;;;; Mode Hooks

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
	    (auto-revert-mode)
	    (hl-line-mode 1)
	    ))


;;; Packages


;;;; Critical Packages

;; These packages are relied upon by lots of things that come after, therefore
;; they must come first.

(use-package bind-key
  :demand t
  )

(use-package hydra
  :demand t
  )

(use-package blackout
  :demand t
  :config
  (blackout 'eldoc-mode)
  (blackout 'visual-line-mode)
  (blackout 'emacs-lisp-mode "Elisp")
  )


;;;; Other Packages

(use-package magit
  :commands magit-status
  )

(use-package exec-path-from-shell
  :demand t
  )

(use-package olivetti
  :commands olivetti-mode
  :custom (olivetti-body-width 80)
  :blackout " Olvti"
  )

(use-package which-key
  :demand t
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0.4)
  (blackout 'which-key-mode)
  )

(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo)
  )

(use-package expand-region
  :bind
  ("s-e" . er/expand-region)
  ("s-E" . er/contract-region)
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
  )

(use-package bufler
  ;; Bufler - a better buffer list
  :commands bufler
  )

(use-package whole-line-or-region
  ;; When no region is active (nothing is selected), and you invoke the
  ;; kill-region (cut) or kill-ring-save (copy) commands, Emacs acts on the
  ;; range of characters between the mark and the point. This is a really good way
  ;; to accidentally kill half your document. I have done this more times than I'd
  ;; like to admit. This package makes it so that without a region those commands
  ;; act on a whole line.
  :demand t
  :config
  (whole-line-or-region-global-mode 1)
  (blackout 'whole-line-or-region-local-mode)
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
  ("s-/" . #'help-command)
 )

(use-package pinboard
  :commands (pinboard pinboard-add pinboard-add-for-later)
  :init
  (setf epa-pinentry-mode 'loopback)
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
  :commands (flyspell-mode flyspell-prog-mode)
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
  ("M-;" . #'flyspell-auto-correct-word)
  ("M-:" . #'flyspell-correct-wrapper)
  :custom
  (flyspell-correct-interface 'flyspell-correct-dummy)
  :init
  (advice-add 'flyspell-correct-dummy :around
	      (defun my--fsc-wrapper (func &rest args)
		(let ((selectrum-should-sort-p nil))
		  (apply func args))))
  )


;;; Keybindings

;; Generally speaking, I want Emacs to behave in the most mac-like fashion possible.
(load "~/dot/emacs/mac.el")

;;;; Enhance Emacs

(define-key global-map (kbd "RET") 'newline-and-indent)

(bind-keys ("s-g" . keyboard-quit)
	   ("M-<up>" . oht/move-line-up)
	   ("M-<down>" . oht/move-line-down)
	   ("M-o" . oht/other-window)
	   ("M-s-s" . save-some-buffers)
	   ("M-c" . capitalize-dwim)
	   ("M-l" . downcase-dwim)
	   ("M-u" . upcase-dwim)
	   ("M-SPC" . cycle-spacing)
	   ("C-l" . oht/recenter-top-bottom)
	   ("C-=" . pulse-line)
	   ("C-x r r" . replace-rectangle)
	   )


;;;; Primary Bindings

(bind-keys ("s-p" . execute-extended-command)
	   ("s-k" . oht/kill-this-buffer)
	   ("M-s-b" . bufler)
	   ("s-C" . org-capture)
	   ("s-|" . oht/pipe-region)
	   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
	   ("s-1" . org-agenda)
	   ("s-2" . hydra-secondary-selection/body)
	   )


;;;; Global Leader Bindings

(bind-keys :prefix-map oht/global-leader
	   :prefix "s-'"
	   ("a" . auto-fill-mode)
	   ("d" . sdcv-search)
	   ("h" . hl-line-mode)
	   ("l" . oht/toggle-line-numbers)
	   ("w" . oht/toggle-whitespace)
	   ("m" . magit-status)
	   ("M" . consult-marks)
	   ("<left>" . winner-undo)
	   ("<right>" . winner-redo)
	   ("s" . org-store-link)
	   ("o" . consult-outline)
	   ("f" . hydra-fonts/body)
	   ("!" . font-lock-mode)
	   ("j" . dired-jump)
	   )


;;; Window Control

(bind-keys :prefix-map oht/windows-leader
	   :prefix "s-="
	   ("s" . oht/split-below)
	   ("v" . oht/split-beside)
	   ("k" . oht/delete-window)
	   ("o" . delete-other-windows)
	   ("b" . balance-windows)
	   ("r" . oht/toggle-window-split))




;;; Appearance

;;;; Display

;; Line spacing (in pixels)
(setq-default line-spacing 1)

;; Frame default parameters
(setq default-frame-alist
   (append (list
          '(width . 80)                ;; width (in characters)
          '(height . 50)               ;; height (in characters)
          '(internal-border-width . 0) ;; border (in pixels)
	  )))

(tool-bar-mode -1)                         ; hide menu-bar
(scroll-bar-mode -1)                       ; hide scroll bars
(menu-bar-mode 1)                          ; ensures full-screen avail on macOS
(show-paren-mode t)                        ; highlight parens
(setq show-paren-delay 0)                  ; and show immediately
(setq visible-bell t)                      ; disable beep
(setq-default frame-title-format '("%b"))  ; show buffer name in titlebar
(setq x-underline-at-descent-line t)       ; underline at descent, not baseline
(setq-default indicate-empty-lines t)      ; show where the file ends
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

;; It can be quite refreshing to change the font I'm working in. So below are
;; a bunch of functions that set my preferred fonts, and a hydra for picking
;; them.

(defun oht/set-font-ibm ()
  (interactive)
  (set-face-attribute 'default nil
		      :family "IBM Plex Mono" :height 120 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "IBM Plex Serif" :height 140 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :family "IBM Plex Mono" :height 120 :weight 'normal)
  )

(defun oht/set-font-ibm-large ()
  (interactive)
  (set-face-attribute 'default nil
		      :family "IBM Plex Mono" :height 140 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "IBM Plex Serif" :height 160 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :family "IBM Plex Mono" :height 140 :weight 'normal)
  )

(defun oht/set-font-triplicate ()
  (interactive)
  (set-face-attribute 'default nil
		      :family "Triplicate T4" :height 140 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "Triplicate T4p" :height 140 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :family "Triplicate T4" :height 140 :weight 'normal)
  )

(defhydra hydra-fonts (:color amaranth)
  "Set Font Properties"
  ("i" oht/set-font-ibm "IBM Plex")
  ("I" oht/set-font-ibm-large "IBM Plex Large")
  ("t" oht/set-font-triplicate "Triplicate")
  ("v" variable-pitch-mode "Variable")
  ("q" nil "cancel"))

;; This sets the default fonts
(oht/set-font-ibm)


;;;; Theme

(use-package modus-themes
  :init
  (setq
   modus-themes-org-blocks 'grayscale
   modus-themes-syntax 'alt-syntax
   modus-themes-slanted-constructs t
   modus-themes-bold-constructs nil
   modus-themes-mode-line nil
   modus-themes-diffs 'desaturated
   modus-themes-variable-pitch-headings nil
   modus-themes-links 'faint-neutral-underline
   modus-themes-completions nil
   modus-themes-prompts 'subtle
   modus-themes-region 'bg-only
   )
  (load-theme 'modus-operandi t)
  )

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (load-theme 'modus-vivendi t))
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)))


;;; Narrowing & Searching

;; Navigating and using the thousands of things Emacs can do is built around
;; the idea of searching and narrowing a selection down to the thing you're
;; looking for. To make this easier I've installed a few packages that enhance
;; Emacs built-in facilities for doing this.

(use-package selectrum-prescient
  :init
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  )

(use-package prescient
  :init
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1)
  )

(use-package selectrum
  :init
  (selectrum-mode +1)
  )

(use-package consult
  :init
  (consult-preview-mode)
  :bind
  ("s-b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ("M-s-o" . consult-recent-file)
  ("s-f" . consult-line)
  )

(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia" :branch "main")
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package ctrlf
  :config
  (setq ctrlf-minibuffer-bindings
        `(("C-n"          . ctrlf-next-match)
          (,(kbd "C-p")   . ctrlf-previous-match)
	  (,(kbd "M-O")   . ctrlf-occur)
	  (,(kbd "M-c")   . ctrlf-toggle-case-fold-search)
	  (,(kbd "M-r")   . ctrlf-toggle-regexp)
	  (,(kbd "C-o s") . ctrlf-change-search-style)
	  ))
  :bind
  ("C-s" . ctrlf-forward-fuzzy)
  ("C-r" . ctrlf-backward-fuzzy)
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

;;; Org Mode

(use-package org
  :init (load-file "~/dot/emacs/org-mode.el")
  :commands org-mode
  :config
  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))
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


;;; Secondary Selection

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

(defhydra hydra-secondary-selection (:color blue)
  "Secondary Selection"
  ("xx" oht/cut-secondary-selection "Cut 2nd")
  ("cc" oht/copy-secondary-selection "Copy 2nd")
  ("xv" oht/cut-secondary-selection-paste "Cut 2nd & Paste")
  ("cv" oht/copy-secondary-selection-paste "Copy 2nd & Paste")
  ("m" (lambda () (interactive)(secondary-selection-from-region)) "Mark as 2nd")
  ("g" (lambda () (interactive)(secondary-selection-to-region)) "Goto 2nd")
  ("q" nil "cancel"))


;;; Outline

;; `outline' provides major and minor modes for collapsing
;; sections of a buffer into an outline-like format.
;; Let's turn that minor mode into a global minor mode.
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


(defhydra hydra-outline (:color amaranth)
  "Hydra for navigating outline mode"
  ("o" outline-hide-sublevels "Hide to This Sublevel")
  ("<tab>" outline-cycle "Show Subtree")
  ("a" outline-show-all "Show All" :color: blue)
  ("c" consult-outline "Consult" :color blue)
  ("n" outline-next-visible-heading "Next")
  ("p" outline-previous-visible-heading "Previous")
  ("q" nil "Cancel" :color blue)
  )
(bind-key "s-0" 'hydra-outline/body)


;;; View-Mode

(use-package view
  ;; Coming from vim, I know the utility of modal editing. However, I have no
  ;; desire to make Emacs into something it is not with `evil-mode', if I want
  ;; to use vim I'll just use vim. That said, there are times when modal
  ;; editing and navigation are very handy and there are, in fact,
  ;; circumstances in which Emacs uses modal navigation. For example, when in
  ;; a dired buffer you can use the n and p keys to move the point around. In
  ;; addition to the bindings below `view-mode' has its own standard bindings,
  ;; which you can find in the minor mode's help.
  :ensure nil
  :init
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
	("q" . nil)
	;; vim
	("j" . next-line)
	("k" . previous-line)
	("h" . backward-char)
	("l" . forward-char)
	("H" . backward-word)
	("L" . forward-word)
	;; emacs
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
	)
  :blackout " VIEW"
  )


;;; EWW Browser

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
	      ;;("s-\\ h" . hydra-eww/body)
  ))

(use-package browse-url
  :after eww
  :config
  ;; This makes it so any link you follow in emacs opens in eww, rather than
  ;; your default browser.
  ;; (setq browse-url-browser-function 'eww-browse-url)
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
	("https://standardebooks.org/rss/new-releases")
	("https://idlewords.com/index.xml")
	("https://www.raptitude.com/feed/")
	("https://blog.robenkleene.com/feed/atom/")
	("https://nyxt.atlas.engineer/feed")
	;; news
	("https://www.economist.com/latest/rss.xml" news)
	("https://www.economist.com/the-economist-explains/rss.xml" news)
	("https://feeds.feedburner.com/marginalrevolution/feed" news)
	("https://hnrss.org/best" hackernews)
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
	;; https://reddit.com/r/emacs/top/.rss?sort=top&t=day
	;; REDDIT RSS GUIDE: https://www.reddit.com/r/pathogendavid/comments/tv8m9/pathogendavids_guide_to_rss_and_reddit/
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

  (defun hrs/elfeed-current-entry ()
    (cond ((eq major-mode 'elfeed-show-mode)
           elfeed-show-entry)
          ((eq major-mode 'elfeed-search-mode)
           (elfeed-search-selected t))))
  (defun hrs/elfeed-pinboard-current-entry ()
    (interactive)
    (let ((url (elfeed-entry-link (hrs/elfeed-current-entry)))
          (title (elfeed-entry-title (hrs/elfeed-current-entry))))
      (pinboard-auth)
      (pinboard-not-too-soon :pinboard-save
	(pinboard-save url title "" "" t nil))))

  (define-key elfeed-show-mode-map "a" 'hrs/elfeed-pinboard-current-entry)
  (define-key elfeed-search-mode-map "a" 'hrs/elfeed-pinboard-current-entry)
  )


;;; Dispatch

;; This lets you pass arbitrary functions to `completing-read' and call the
;; selected candidate.

;; First, these are some functions for things I do on the computer all the time.
(defun dispatch/hacker-news ()
  "Open news.ycombinator.com in browser."
       (interactive)
       (start-process "open HN" nil "open" "https://news.ycombinator.com"))
(defun dispatch/mail-personal ()
  "Open Mail.app"
       (interactive)
       (start-process "open mail" nil "open" "-a" "Mail"))
(defun dispatch/mail-work ()
  "Open Work Mail."
       (interactive)
       (start-process "open work email" nil "open" "-a" "Mimestream"))
(defun dispatch/slack ()
  "Open Slack in browser."
       (interactive)
       (start-process "open slack" nil "open" "https://ievfx.slack.com"))


;; Next, create a function to pass a list of functions to `completing-read'.
(defun dispatch ()
  (interactive)
  (call-interactively
   (intern (completing-read "Choose one: "
			    '(
			      dispatch/hacker-news
			      dispatch/mail-personal
			      dispatch/mail-work
			      dispatch/slack
			      elfeed
			      org-agenda
			      eww-list-bookmarks
			      )))))

;; Finally, bind it to something useful.
(bind-key "s-d" 'dispatch)


;;; Closing

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
