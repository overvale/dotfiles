;;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Copyright (C) 2021 Oliver Taylor

;; Author: Oliver Taylor
;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Configuration

;;;; Settings

;; This is the 2nd step, for the 1st step, see `early-init.el'
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)
            (garbage-collect)) t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message (concat "Emacs startup: " (emacs-init-time)))))

(setq custom-file (make-temp-file "emacs-custom-"))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-buffer-choice 'remember-notes)

(defvar oht-dotfiles "~/home/dot/emacs/")
(defvar oht-orgfiles "~/home/org/")
(defvar oht-ingenuity-dir "~/home/ingenuity/")
(defvar user-downloads-directory "~/Downloads")

(add-to-list 'load-path (concat oht-dotfiles "lisp/"))

(defun find-emacs-dotfiles ()
  "Find lisp files in your Emacs dotfiles directory, pass to completing-read."
  (interactive)
  (find-file (completing-read "Find Elisp Dotfile: "
                              (directory-files-recursively oht-dotfiles "\.el$"))))

(defun find-org-files ()
  "Find org files in your org directory, pass to completing-read."
  (interactive)
  (find-file (completing-read "Find Org Files: "
                              (directory-files-recursively oht-orgfiles "\.org$"))))

(custom-set-variables
 '(delete-selection-mode nil)
 '(global-auto-revert-mode t)
 '(save-place-mode t)
 '(recentf-mode t)
 '(winner-mode t)
 '(show-paren-mode t)
 '(blink-cursor-mode nil)
 '(ring-bell-function #'ignore)
 '(scroll-bar-mode nil) ; set in early-init AND here ¯\_(ツ)_/¯
 '(minibuffer-depth-indicate-mode t)
 '(set-language-environment "UTF-8"))

(setq-default cursor-type 'box)
(setq visible-bell nil)
(setq-default indicate-empty-lines nil)
(setq frame-title-format '("%b"))
(setq uniquify-buffer-name-style 'forward)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq display-time-format "%H:%M  %Y-%m-%d"
      display-time-interval 60
      display-time-mail-directory nil
      display-time-default-load-average nil)

(column-number-mode t)
(display-time-mode t)

(setq enable-recursive-minibuffers 1)
(setq vc-follow-symlinks t
      find-file-visit-truename t)
(setq create-lockfiles nil
      make-backup-files nil)
(setq load-prefer-newer t)
(setq delete-by-moving-to-trash t)
(setq confirm-kill-processes nil)
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t)

(when (string= system-type "darwin")
  (setq locate-command "mdfind"
        trash-directory "~/.Trash/emacs"))

(setq split-window-keep-point nil)
;; (defadvice split-window-below (after split-window-below activate) (other-window 1))
;; (defadvice split-window-right (after split-window-right activate) (other-window 1))

(setq sentence-end-double-space nil)
(setq-default tab-width 4
              indent-tabs-mode nil
              fill-column 78)

;;;; Functions

;;;;; Miscellaneous

(defun toggle-window-split ()
  "Toggle window split from vertical to horizontal."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows.")
    (let ((was-full-height (window-full-height-p)))
      (delete-other-windows)
      (if was-full-height
          (split-window-vertically)
        (split-window-horizontally))
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun macos-open-file ()
  "Open the file inferred by ffap using `open'."
  (interactive)
  (if-let* ((file? (ffap-guess-file-name-at-point))
            (file (expand-file-name file?)))
      (progn
        (message "Opening %s..." file)
        (call-process "open" nil 0 nil file))
    (message "No file found at point.")))

(defun sanemacs/backward-kill-word ()
  "Kill word backward, without copying to clipboard."
  (interactive "*")
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(defun pipe-region (start end command)
  ;; https://github.com/oantolin/emacs-config/blob/master/my-lisp/text-extras.el
  "Pipe region through shell command. If the mark is inactive,
pipe whole buffer."
  (interactive (append
                (if (use-region-p)
                    (list (region-beginning) (region-end))
                  (list (point-min) (point-max)))
                (list (read-shell-command "Pipe through: "))))
  (let ((exit-status (call-shell-region start end command t t)))
    (unless (equal 0 exit-status)
      (let ((error-msg (string-trim-right (buffer-substring (mark) (point)))))
        (undo)
        (cond
         ((null exit-status)
          (message "Unknown error"))
         ((stringp exit-status)
          (message "Signal %s" exit-status))
         (t
          (message "[%d] %s" exit-status error-msg)))))))

(defun find-file-recursively ()
  "Find Files Recursively using completing read."
  (find-file (completing-read "Find File Recursively: "
                              (directory-files-recursively default-directory ".+"))))

(defun oht-toggle-comment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun my/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (<= oldpos (point))
         (/= (line-beginning-position) oldpos)
         (beginning-of-line))))

(defun mark-whole-line ()
  "Put the point at end of this whole line, mark at beginning"
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))


;;;;; Transient Mark

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode

(defun exchange-point-and-mark-dwim ()
  "If a region is active, then leave it activated and swap point and mark.
If no region is active, then stay active and swap."
  (interactive)
  (if (use-region-p)
      (exchange-point-and-mark)
    (exchange-point-and-mark)
    (deactivate-mark nil)))

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


;;;;; Secondary Selection

;; Emacs's Secondary Selection assumes you only want to interact with it via
;; the mouse, however it is perfectly possible to do it via the keyboard, all
;; you need is some wrapper functions to make things keybinding-addressable.

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

(defun oht/mark-region-as-secondary-selection ()
  "Make the region the secondary selection."
  (interactive)
  (secondary-selection-from-region))

(defun oht/mark-secondary-selection ()
  "Mark the Secondary Selection as the region."
  (interactive)
  (secondary-selection-to-region))

(defun oht/delete-secondary-selection ()
  "Delete the Secondary Selection."
  (interactive)
  (delete-overlay mouse-secondary-overlay))


;;;;; Youtube-dl

;; A few utilities for working with videos

(setq youtube-dl-path "/usr/local/bin/youtube-dl")
(setq youtube-dl-output-dir "~/Downloads/")

(defun youtube-dl-URL-at-point ()
  "Send the URL at point to youtube-dl."
  (interactive)
  (async-shell-command (format "%s -o \"%s%s\" -f best \"%s\""
                               youtube-dl-path
                               youtube-dl-output-dir
                               "%(title)s.%(ext)s"
                               (ffap-url-at-point))))


;;;; Keybindings

;; I do keybindings first because I've found the most fragile parts of my
;; config are the package declarations. This way if something gets screwed up
;; in the packages (missing parenthesis) all my most important keybindings
;; (none of which require packages) are still loaded.

;; Minor modes override global bindings (see README), so any bindings you
;; don't want overridden should be placed in a minor mode. This technique is
;; stolen from the package bind-key.

(defvar oht-keys-mode-keymap (make-keymap)
  "Keymap for oht-keys-mode")

(define-minor-mode oht-keys-mode
  "Minor mode for my personal keybindings.

The only purpose of this minor mode is to override global keybindings.
Keybindings you define here will take precedence."
  :init-value t
  :global t
  :keymap oht-keys-mode-keymap)

;; Stolen from bind-key:
;; the keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((oht-keys-mode . ,oht-keys-mode-keymap)))

;; If on a Mac, use the command key as Super, left-option for Meta, and
;; right-option for Alt.
(if (equal system-type 'darwin)
    (progn
      (setq mac-command-modifier 'super
            mac-option-modifier 'meta
            mac-right-option-modifier 'nil)))

;; Mac-like
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-m") 'iconify-frame)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file) ;save as
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-n") 'make-frame-command)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-<backspace>") (lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

;; Emacs SUPER!
(global-set-key (kbd "s-k")   'kill-this-buffer)
(global-set-key (kbd "s-b")   'switch-to-buffer)
(global-set-key (kbd "s-B")   'ibuffer)
(global-set-key (kbd "s-o")   'other-window)
(global-set-key (kbd "s-O")   'find-file)
(global-set-key (kbd "s-l")   'mark-whole-line)
(global-set-key (kbd "s-/")   'oht-toggle-comment-region-or-line)
(global-set-key (kbd "s-|")   'pipe-region)

;; Emacs Misc
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(define-key oht-keys-mode-keymap (kbd "<C-return>") 'execute-extended-command)
(global-set-key (kbd "M-s-s")   'save-some-buffers)
(global-set-key (kbd "M-c")     'capitalize-dwim)
(global-set-key (kbd "M-l")     'downcase-dwim)
(global-set-key (kbd "M-u")     'upcase-dwim)
(global-set-key (kbd "M-SPC")   'cycle-spacing)
(global-set-key (kbd "C-x r r") 'replace-rectangle)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)

;; Custom functions
(global-set-key [remap beginning-of-line] #'my/smart-beginning-of-line)
(global-set-key (kbd "s-,") 'find-emacs-dotfiles)
(global-set-key (kbd "s-<") 'find-org-files)
(global-set-key (kbd "C-x C-x") 'exchange-point-and-mark-dwim)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'consult-mark)
(global-set-key (kbd "M-DEL") 'sanemacs/backward-kill-word)
(global-set-key (kbd "C-DEL") 'sanemacs/backward-kill-word)



;;;; Visual Line Mode, Fixes

;; `visual-line-mode', `word-wrap', and `truncate-lines' all do different
;; things. `visual-line-mode' is a wrapper around a bunch of things, probably
;; best explained here: http://ergoemacs.org/emacs/emacs_long_line_wrap.html
;; `word-wrap' ONLY wraps lines word-wise instead of character-wise.
;; `truncate-lines' ONLY controls if wrapping happens at all; if set to
;; non-nil it is supposed to let lines run off the window, but this is a
;; buffer-local setting that I cannot (no matter what I try) get to be global.
(setq-default truncate-lines t)

;; When visual-line-mode is off and truncate-lines is toggled off, I still
;; want wrapping to happen at the word instead of character.
(setq-default word-wrap 1)

;; Turning on `visual-line-mode' binds "C-a" to `beginning-of-visual-line'.
;; This is inconsistent with macOS behavior, which is that "C-a" always goes
;; to the beginning of the logical line and "s-<left>" goes to the beginning
;; of the visual line.
(define-key oht-keys-mode-keymap (kbd "C-a") 'beginning-of-line)
(define-key oht-keys-mode-keymap (kbd "C-e") 'end-of-line)
(global-set-key (kbd "s-<left>") 'beginning-of-visual-line)
(global-set-key (kbd "s-<right>") 'end-of-visual-line)

;; Additionally, in visual-line-mode the function kill-line kills to the end
;; of the VISUAL line, which is inconsistent with Mac OS, and my own
;; expectations. Therefore we need to remap it to a function which kills to
;; the end of the whole line.
(defun kill-rest-whole-line ()
  "Kill the rest of the whole line."
  (interactive)
  (kill-line nil))
(global-set-key (kbd "C-k") 'kill-rest-whole-line)


;;;; Mouse

;; Gasp! An Emacs user that actually uses the mouse?! Scandalous.

;; Start by making shift-click extend the selection (region)
(global-set-key [S-down-mouse-1] 'ignore)
(global-set-key [S-mouse-1] 'mouse-save-then-kill)

;; s-click to split windows at that exact spot
(global-set-key [s-mouse-1] 'mouse-split-window-horizontally)
(global-set-key [S-s-mouse-1] 'mouse-split-window-vertically)

;; Delete a window with M-s--click
(global-set-key [M-s-mouse-1] 'mouse-delete-window)

;; The below bindings are taken directly from the source of `mouse.el'
;; but I've swapped the modifier keys. This makes more sense to me.

;; Use M-drag-mouse-1 to create rectangle regions
(global-set-key [M-down-mouse-1] #'mouse-drag-region-rectangle)
(global-set-key [M-drag-mouse-1] #'ignore)
(global-set-key [M-mouse-1]      #'mouse-set-point)

;; Use C-M-drag-mouse-1 to create secondary selections
(global-set-key [C-M-mouse-1]      'mouse-start-secondary)
(global-set-key [C-M-drag-mouse-1] 'mouse-set-secondary)
(global-set-key [C-M-down-mouse-1] 'mouse-drag-secondary)


;;; Packages

;;;; Straight / Use Package

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

;; By default, use-package adds the suffix "-hook" to all your hook
;; declarations, which I think is a reasonable default. I tend to forget that
;; use-package does this and then forget that Emacs requires the suffix, then
;; spend 15 minutes banging my head against a wall trying to figure out why
;; the non-use-package hooks I'm writing don't work. So to avoid that I make
;; it consistent, use-package or not, the hooks are named the same.
(setq use-package-hook-name-suffix nil)


;;;; Minimum

(use-package blackout
  :demand
  :config
  (blackout 'eldoc-mode)
  (blackout 'emacs-lisp-mode "Elisp")
  (blackout 'auto-fill-function " Fill"))

(use-package modus-themes
  :custom
  (modus-themes-links 'faint-neutral-underline)
  (modus-themes-mode-line 'accented)
  (modus-themes-region 'bg-only)
  (modus-themes-diffs 'desaturated)
  (modus-themes-org-blocks 'grayscale)
  :init
  (modus-themes-load-operandi))

(add-hook 'mac-effective-appearance-change-hook 'modus-themes-toggle)


;;;; Narrowing & Searching

(use-package orderless
  :straight (:host github :repo "oantolin/orderless" :branch "master")
  :demand
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :straight (:host github :repo "minad/vertico" :branch "main")
  :init
  (vertico-mode))

(use-package marginalia
  :straight (:type git :host github :repo "minad/marginalia" :branch "main")
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light))
  :init
  (marginalia-mode 1))

(use-package embark
  :straight (:host github :repo "oantolin/embark" :branch "master")
  :commands use-embark-completions
  :bind
  ("s-e" . embark-act)
  (:map embark-file-map
        ("O" . macos-open-file)
        ("j" . dired-jump))
  (:map embark-url-map
        ("d" . youtube-dl-URL-at-point)
        ("&" . browse-url-default-macosx-browser)))

(use-package consult
  :straight (:type git :host github :repo "minad/consult" :branch "main")
  :bind
  ("s-b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ("s-f" . consult-line)
  ([remap yank-pop] . consult-yank-pop)
  :custom
  (consult-preview-key (kbd "C-="))
  (consult-config
   `((consult-mark :preview-key any))))

(use-package embark-consult
  :after (embark consult)
  :demand t)

(use-package ctrlf
  :bind
  ("C-s" . ctrlf-forward-fuzzy)
  ("C-r" . ctrlf-backward-fuzzy)
  :custom
  (ctrlf-go-to-end-of-match nil))


;;;; Built-In Packages

(use-package info
  :straight nil
  :bind (:map Info-mode-map
              ("s-\\" . oht-transient-info)))

(use-package remember
  :straight nil
  :custom
  (remember-data-file (concat oht-orgfiles "remember-notes"))
  (remember-notes-initial-major-mode 'fundamental-mode)
  (remember-notes-auto-save-visited-file-name t)
  :init
  (defun oht-remember-dwim ()
    "If the region is active, capture with region, otherwise just capture."
    (interactive)
    (if (use-region-p)
        (let ((current-prefix-arg 4)) (call-interactively 'remember))
      (remember)))
  :bind
  ("s-_" . oht-remember-dwim)
  ("s--" . remember-notes))

(use-package bookmark
  :straight nil
  :commands (list-bookmarks)
  :custom
  (bookmark-save-flag 1)
  (bookmark-bmenu-file-column 45)
  :init
  (add-hook 'bookmark-bmenu-mode (lambda ()
                                   (hl-line-mode 1)))
  :bind ("M-s-b" . list-bookmarks))

(use-package ibuffer
  :straight nil
  :commands ibuffer
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("default"
      ("Read"  (or (mode . eww-mode)
                   (mode . elfeed-search-mode)
                   (mode . elfeed-show-mode)
                   (mode . hackernews-mode)))
      ("Org"   (mode . org-mode))
      ("Mail"  (or (mode . mu4e-view-mode)
                   (mode . mu4e-main-mode)
                   (mode . mu4e-headers-mode)
                   (mode . mu4e-view-raw-mode)
                   (mode . mu4e-compose-mode)
                   (mode . message-mode)
                   (mode . mail-mode)))
      ("Dired" (mode . dired-mode))
      ("ELisp" (mode . emacs-lisp-mode))
      ("Help"  (or (name . "\*Help\*")
                   (name . "\*Apropos\*")
                   (name . "\*Info\*"))))))
  :init
  (defun oht-ibuffer-hook ()
    (hl-line-mode 1)
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default"))
  :hook (ibuffer-mode-hook . oht-ibuffer-hook))

(use-package pulse
  :straight nil
  :init
  (defun pulse-line (&rest _)
    "Interactive function to pulse the current line."
    (interactive)
    (pulse-momentary-highlight-one-line (point)))

  (defadvice other-window (after other-window-pulse activate) (pulse-line))
  (defadvice delete-window (after delete-window-pulse activate) (pulse-line))
  (defadvice recenter-top-bottom (after recenter-top-bottom-pulse activate) (pulse-line))

  (defun ct/yank-pulse-advice (orig-fn &rest args)
    "Pulse line when yanking"
    ;; From https://christiantietze.de/posts/2020/12/emacs-pulse-highlight-yanked-text/
    ;; Define the variables first
    (let (begin end)
      ;; Initialize `begin` to the current point before pasting
      (setq begin (point))
      ;; Forward to the decorated function (i.e. `yank`)
      (apply orig-fn args)
      ;; Initialize `end` to the current point after pasting
      (setq end (point))
      ;; Pulse to highlight!
      (pulse-momentary-highlight-region begin end)))
  (advice-add 'yank :around #'ct/yank-pulse-advice))

(use-package hippie-exp
  :straight nil
  :custom
  (hippie-expand-try-functions-list
   '(try-complete-file-name-partially
     try-complete-file-name
     try-expand-line
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))
  :bind
  ("M-/" . hippie-expand))


;;;; View / Selected

(use-package view
  :straight nil
  :custom
  (view-read-only t)
  :init
  (defun oht/view-mode-exit ()
    (interactive)
    (view-mode -1)
    (hl-line-mode -1))
  (defun oht/exit-view-replace-rectangle ()
    (interactive)
    (oht/view-mode-exit)
    (call-interactively 'replace-rectangle))
  :bind
  ("s-j" . view-mode)
  (:map view-mode-map
        ;; common
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
        ("[" . scroll-down-line)
        ("]" . scroll-up-line)
        ("x" . exchange-point-and-mark)
        ("M" . rectangle-mark-mode)
        ;; unique
        ("R" . oht/exit-view-replace-rectangle)
        ("m" . set-mark-command)
        ("<RET>" . oht/view-mode-exit)
        ("s-j" . oht/view-mode-exit)
        ("q" . quit-window))
  :hook (view-mode-hook . hl-line-mode)
  :blackout " VIEW")

(use-package selected
  :commands selected-minor-mode
  :init
  (selected-global-mode 1)
  :bind (:map selected-keymap
              ;; common
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
              ("[" . scroll-down-line)
              ("]" . scroll-up-line)
              ("x" . exchange-point-and-mark)
              ("M" . rectangle-mark-mode)
              ;; unique
              ("u" . upcase-dwim)
              ("d" . downcase-dwim)
              ("w" . kill-ring-save)
              ("|" . pipe-region)
              ("R" . replace-rectangle)
              ("E" . eval-region)
              ("q" . selected-off))
  :config
  (add-hook 'elfeed-show-mode (lambda ()
                                (selected-minor-mode -1)))
  (add-hook 'elfeed-search-mode (lambda ()
                                (selected-minor-mode -1)))
  :blackout selected-minor-mode)


;;;; Dired

(use-package dired
  :straight nil
  :commands (dired dired-jump dired-jump-other-window)
  :custom
  (dired-use-ls-dired nil) ; no more warning message
  :bind (:map dired-mode-map
              ("s-\\" . oht-transient-dired)
              ("O" . dired-open-file)
              ("s-z" . dired-undo))
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
              (hl-line-mode 1))))


;;;; Flyspell

(use-package flyspell
  :straight nil
  :commands (flyspell-mode flyspell-prog-mode turn-on-flyspell)
  :custom
  (ispell-program-name "/usr/local/bin/aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  (ispell-list-command "list")
  :hook
  (text-mode-hook . turn-on-flyspell)
  (prog-mode-hook . flyspell-prog-mode)
  :blackout " Spell")

(use-package flyspell-correct
  :bind
  ("M-;" . #'flyspell-auto-correct-previous-word)
  :custom
  (flyspell-correct-interface 'flyspell-correct-dummy)
  :init
  (advice-add 'flyspell-correct-dummy :around
              (defun my--fsc-wrapper (func &rest args)
                (let ((selectrum-should-sort-p nil))
                  (apply func args)))))


;;;; Misc Packages

(use-package magit
  :commands magit-status)

(use-package exec-path-from-shell
  :demand)

(use-package olivetti
  :commands olivetti-mode
  :custom (olivetti-body-width 84)
  :blackout " Olvti")

(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo))

(use-package vundo
  :straight (:type git :host github :repo "casouri/vundo" :branch "master")
  :commands vundo
  ;; The below is back-ported from Emacs 28, once you upgrade you can safely remove this:
  :config (load (concat oht-dotfiles "lisp/undo-backport.el")))

(use-package expand-region
  :bind
  ("s-r" . er/expand-region)
  ("s-R" . er/contract-region))

(use-package zzz-to-char
  ;; replaces zap-to-char with an avy-like interface
  ;; note that it searches forward and backward
  :bind ("M-z" . zzz-up-to-char))

(use-package unfill
  :commands (unfill-paragraph unfill-toggle unfill-region)
  :bind
  ("M-q" . unfill-toggle))

(use-package whole-line-or-region
  :init
  (whole-line-or-region-global-mode 1)
  :blackout whole-line-or-region-local-mode)

(use-package helpful
  :bind
  ("C-h f" . #'helpful-function)
  ("C-h v" . #'helpful-variable)
  ("C-h o" . #'helpful-symbol)
  ("C-h k" . #'helpful-key)
  ("C-h p" . #'helpful-at-point))

(use-package move-text
  :bind
  ("M-<up>" . move-text-up)
  ("M-<down>" . move-text-down))

(use-package buffer-move
  :commands (buf-move-up
             buf-move-down
             buf-move-left
             buf-move-right))

(use-package visual-regexp
  ;; Provides an alternate version of `query-replace' which highlights matches
  ;; and replacements as you type.
  :bind (([remap query-replace] . #'vr/query-replace)))

(use-package visual-regexp-steroids
  ;; Allows `visual-regexp' to use regexp engines other than Emacs'; for
  ;; example, Python or Perl regexps.
  :after visual-regexp
  :bind (([remap query-replace-regexp] . #'vr/query-replace))
  :custom
  (vr/engine 'pcre2el))


;;;; Languages

(use-package fountain-mode
  :commands fountain-mode
  :custom
  (fountain-add-continued-dialog nil)
  (fountain-highlight-elements (quote (section-heading))))

(use-package markdown-mode
  :mode ("\\.text" . markdown-mode)
  :magic ("%text" . markdown-mode)
  :commands markdown-mode)

(use-package lua-mode
  :commands lua-mode)


;;;; Org

(use-package org
  :commands (org-mode oht-org-agenda-today)
  :bind ("s-C" . org-capture)
  :config
  (load (concat oht-dotfiles "lisp/org-extras.el"))
  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))
  (add-hook 'org-agenda-mode-hook
            (lambda () (hl-line-mode 1)))
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "s-\\") 'oht-transient-org))))

(use-package org-agenda
  :straight nil
  :commands org-agenda
  :bind
  (:map org-agenda-mode-map
        ("t" . oht-transient-org-agenda)
        ("s-z" . org-agenda-undo)))


;;;; Outline

;; `outline' provides major and minor modes for collapsing sections of a
;; buffer into an outline-like format. Let's turn that minor mode into a
;; global minor mode and enable it.
(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)
(global-outline-minor-mode +1)
(blackout 'outline-minor-mode)

(use-package bicycle
  :after outline
  :bind
  (:map outline-minor-mode-map
        ([C-tab] . bicycle-cycle))
  (:map emacs-lisp-mode-map
        ("<backtab>" . bicycle-cycle-global)))


;;;; Browser & News

(setq shr-max-image-proportion 0.5)
(setq shr-use-fonts t)
(setq shr-width 80)

;; Set default browser in Emacs
(setq browse-url-browser-function 'eww-browse-url)

;; Prefixing with universal argument uses browse-url-default-browser
;; which, for me, is `browse-url-default-macosx-browser'

(use-package eww
  :straight nil
  :custom
  (eww-restore-desktop nil)
  (eww-desktop-remove-duplicates t)
  (eww-header-line-format "%t %u")
  (eww-download-directory user-downloads-directory)
  (eww-bookmarks-directory (concat user-emacs-directory "eww-bookmarks/"))
  (eww-history-limit 150)
  (eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/pdf\\)")
  (url-cookie-trusted-urls '()
                           url-cookie-untrusted-urls '(".*"))
  :init
  (defun oht-eww-bookmark-handler (record)
    "Jump to an eww bookmarked location using EWW."
    (eww (bookmark-prop-get record 'location)))
  :config
  (load (concat oht-dotfiles "lisp/eww-extras.el"))
  :commands (eww prot-eww-browse-dwim)
  :bind
  (:map eww-mode-map
        ("s-\\" . oht-transient-eww)))

(use-package elfeed
  :commands elfeed
  :hook (elfeed-show-mode-hook . oht-elfeed-show-fonts)
  :config
  (when (string= (system-name) "shadowfax.local")
    (load "~/home/src/rss-feeds.el"))
  (load (concat oht-dotfiles "lisp/elfeed-extras.el"))
  :custom
  (elfeed-use-curl t)
  (elfeed-curl-max-connections 10)
  (elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (elfeed-enclosure-default-dir user-downloads-directory)
  (elfeed-search-filter "@4-week-ago +unread")
  (elfeed-sort-order 'descending)
  (elfeed-search-clipboard-type 'CLIPBOARD)
  (elfeed-show-truncate-long-urls t)
  :bind
  (:map elfeed-search-mode-map
        ("a" . hrs/elfeed-pinboard-current-entry)
        ("b" . elfeed-search-browse-url)
        ("B" . oht-elfeed-search-browse-and-bury)
        ("*" . elfeed-search-tag--star)
        ("8" . elfeed-search-untag--star)
        ("o" . delete-other-windows))
  (:map elfeed-show-mode-map
        ("a" . hrs/elfeed-pinboard-current-entry)
        ("&" . bjm/elfeed-show-visit-gui)
        ("r" . elfeed-show-tag--read)
        ("u" . elfeed-show-tag--unread)
        ("*" . elfeed-show-tag--star)
        ("8" . elfeed-show-tag--unstar)
        ("o" . delete-other-windows)
        ("d" . oht-elfeed-show-download-video)
        ("i" . elfeed-inhibit-images-toggle)
        ("B" . oht-elfeed-show-browse-and-bury)))

(use-package hackernews
  :commands hackernews
  :bind
  (:map hackernews-mode-map
        ("o" . delete-other-windows))
  :custom
  (hackernews-items-per-page 30)
  (hackernews-default-feed 'top))


;;;; Mail

(setq mail-user-agent 'mu4e-user-agent)

(use-package message
  :straight nil
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (message-cite-style 'message-cite-style-thunderbird)
  (message-cite-function 'message-cite-original)
  (message-kill-buffer-on-exit t)
  (message-citation-line-format "On %d %b %Y at %R, %f wrote:\n")
  (message-citation-line-function 'message-insert-formatted-citation-line))

(use-package mu4e
  :load-path "/usr/local/Cellar/mu/1.4.15/share/emacs/site-lisp/mu/mu4e"
  :commands mu4e
  :bind (:map mu4e-headers-mode-map
              ("G" . mu4e-update-mail-and-index))
  :custom
  (mu4e-attachments-dir user-downloads-directory)
  (mu4e-update-interval (* 5 60))
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-format-flowed nil)
  (mu4e-confirm-quit nil)
  (mu4e-headers-date-format "%Y-%m-%d")
  (mu4e-headers-include-related nil)
  (mu4e-headers-skip-duplicates t)
  (mu4e-headers-time-format "%H:%M")
  (mu4e-headers-visible-lines 20)
  (mu4e-use-fancy-chars nil)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-headers-fields
   '((:human-date . 12)
     (:flags . 6)
     (:from . 22)
     (:thread-subject)))
  :config
  (load (concat oht-ingenuity-dir "mu4e.el"))
  (defun jcs-view-in-eww (msg)
    (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)
  (add-hook 'mu4e-compose-mode-hook
            (defun oht-mu4e-compose-settings ()
              "My settings for message composition."
              (auto-fill-mode -1)
              (visual-line-mode t)))
  (add-hook 'mu4e-view-mode-hook
            (defun oht-mu4e-view-settings ()
              "My settings for message composition."
              (facedancer-vadjust-mode 1)
              )))

(use-package smtpmail
  :straight nil
  :custom
  (auth-sources '((concat oht-ingenuity-dir "authinfo")))
  (smtpmail-stream-type 'starttls)
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587))


;;;; PDFs

;; To get started, use homebrew to install a couple things:
;; $ brew install poppler automake
;; INITIALIZATION --- un-comment this on first-run
;; (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")

(use-package pdf-tools
  ;; To get Emacs to open PDFs in pdf-view-mode you need to add the extension
  ;; to the `auto-mode-alist', which pdf-tools suggests you do by calling the
  ;; function `pdf-tools-install'. But doing it this way loads the entire
  ;; pdf-tools package simply to make that alist association. Much easier to
  ;; set it manually (this code is taken directly from the pdf-tools source).
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations nil)
  (pdf-annot-list-format '((page . 3) (type . 24) (contents . 200)))
  ;; Required for retina scaling to work
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  :config
  ;; pdf-tools uses the `pdf-tools-install' function to set up hooks and
  ;; various things to ensure everything works as it should. PDFs will open
  ;; just fine without this, but not all features will be available.
  (pdf-tools-install)
  (defun oht-pdf-annot-fonts ()
    (hl-line-mode 1)
    (pdf-annot-list-follow-minor-mode))
  :hook (pdf-annot-list-mode-hook . oht-pdf-annot-fonts)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward)
              ("A" .   pdf-annot-add-highlight-markup-annotation)
              ("L" .   pdf-annot-list-annotations)
              ("O" .   pdf-occur)
              ("G" .   pdf-view-goto-page)
              ("<" .   pdf-view-first-page)
              (">" .   pdf-view-last-page)))

(use-package pdf-tools-org
  ;; This package seems pretty out of date, modifying the code as suggested here:
  ;; https://github.com/machc/pdf-tools-org/issues/7
  ;; seems to fix export
  :straight (:host github :repo "machc/pdf-tools-org" :branch "master")
  :commands pdf-tools-org-export-to-org)

;; The bad news is that pdf-tools might be entering a period of being
;; unmaintained. The github issue tracker is full of people commenting that
;; the maintainer cannot be reached. [2021-04-21]


;;;; Transient

(use-package transient
  ;; comes installed with Magit, no need to install
  :straight nil
  ;; Anything not in a binding below needs to be called-out as a command
  :commands (oht-transient-org
             oht-transient-dispatch
             oht-transient-org-agenda
             oht-transient-eww
             oht-transient-info)
  :init
  ;; Any commands these transients use, whose packages are potentially not
  ;; loaded yet, need to be autoloaded.
  (autoload 'org-store-link "org")
  :custom
  (transient-mode-line-format 'line)
  (transient-display-buffer-action '(display-buffer-below-selected))
  :config
  (load (concat oht-dotfiles "lisp/transient-extras.el"))
  :bind*
  ("s-<return>" . oht-transient-general)
  ("s-w" . oht-transient-window)
  ("s-2" . oht-transient-2nd)
  ("s-d" . oht-transient-dispatch))


;;;; Pseudo-Packages, or my lisp files

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
;;
;; Keep in mind that a file must exist for each of these use-package
;; declarations. If you don't want to separate the code into a separate file
;; you can "use" the Emacs package.

(use-package facedancer
  :straight nil
  :demand
  :init
  (setq-default line-spacing nil)
  (setq text-scale-mode-step 1.09)
  (setq facedancer-monospace "IBM Plex Mono"
        facedancer-variable  "IBM Plex Serif"
        facedancer-variable-weight 'normal
        facedancer-monospace-size 12
        facedancer-variable-size  14)
  (set-face-attribute 'mode-line nil          :family "IBM Plex Sans" :height 130)
  (set-face-attribute 'mode-line-inactive nil :family "IBM Plex Sans" :height 130)
  (defun facedancer-prot-fonts ()
    (interactive)
    (setq-local facedancer-monospace "Iosevka Comfy"
                facedancer-variable  "Inter"
                facedancer-monospace-size 14
                facedancer-variable-size  13)
    (facedancer-mode 'toggle))
  :config
  (facedancer-font-set))

(use-package find-file-directories
  ;; BEAUTIFUL set of functions from Radian for creating directories when
  ;; finding files.
  :straight nil
  :demand)


;;;; Composition mode

(define-minor-mode composition-mode
  "A tiny minor-mode to toggle some settings I like when writing

This is really just a wrapper around some extant features I toggle on/off
when I'm writing. I've wrapped them in a minor mode to make it easy to
toggle them on/off. It also allows me to define a lighter for the
mode-line."
  :init-value nil
  :lighter " Comp"
  (if composition-mode
      (progn
        (visual-line-mode t)
        (setq-local line-spacing 2)
        (olivetti-mode t)
        (text-scale-increase 1)
        (variable-pitch-mode 1))
    (progn
      (visual-line-mode -1)
      (setq-local line-spacing 0)
      (olivetti-mode -1)
      (text-scale-increase 0)
      (variable-pitch-mode -1)
      ;; This shouldn't be needed, but is:
      (toggle-truncate-lines 1))))




;;; End of init.el
