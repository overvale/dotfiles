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

(defun narrow-or-widen-dwim (p)
  ;; https://github.com/oantolin/emacs-config/blob/master/my-lisp/narrow-extras.el
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.
With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	    ((and (bound-and-true-p org-src-mode) (not p))
	     (org-edit-src-exit))
	    ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (or (ignore-errors (org-edit-src-code))
             (ignore-errors (org-narrow-to-block))
             (org-narrow-to-subtree)))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
	    ((derived-mode-p 'tex-mode)
	     (TeX-narrow-to-group))
        (t (narrow-to-defun))))


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

;; I use a lot of transients. The best way I've found to configure them is to
;; place all the mode-specific ones in their mode's use-package declaration,
;; and autoload the transient-define-prefix command. Doing this also requires
;; that you autoload the command BEFORE you declare any of the commands. Which
;; is why this bit is here, and not down with the rest of my transient
;; configuration.
(use-package transient
  :commands (transient-define-prefix))

;; Because many of my use-package declarations use the ':blackout' statement I
;; need to configure this first.
(use-package blackout
  :demand
  :config
  (blackout 'eldoc-mode)
  (blackout 'emacs-lisp-mode "Elisp")
  (blackout 'auto-fill-function " Fill"))

;; This is only here because I love it so much and don't have another good
;; place for it below.
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
              ("s-\\" . oht-transient-info))
  :config
  (transient-define-prefix oht-transient-info ()
    "Transient for Info mode"
    ["Info"
     [("d" "Info Directory" Info-directory)
      ("m" "Menu" Info-menu)
      ("F" "Go to Node" Info-goto-emacs-command-node)]
     [("s" "Search regex Info File" Info-search)
      ("i" "Index" Info-index)
      ("I" "Index, Virtual" Info-virtual-index)]]
    ["Navigation"
     [("l" "Left, History" Info-history-back)
      ("r" "Right, History" Info-history-forward)
      ("L" "List, History" Info-history)]
     [("T" "Table of Contents" Info-toc)
      ("n" "Next Node" Info-next)
      ("p" "Previous Node" Info-prev)
      ("u" "Up" Info-up)]
     [("<" "Top Node" Info-top-node)
      (">" "Final Node" Info-final-node)
      ("[" "Forward Node" Info-backward-node)
      ("]" "Backward Node" Info-forward-node)]]))

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
      ("Org"   (or (mode . org-mode)
                   (mode . org-agenda-mode)))
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
              (hl-line-mode 1)))

  (transient-define-prefix oht-transient-dired ()
    "Transient for dired commands"
    ["Dired Mode"
     ["Action"
      ("RET" "Open file"            dired-find-file)
      ("o" "  Open in other window" dired-find-file-other-window)
      ("C-o" "Open in other window (No select)" dired-display-file)
      ("v" "  Open file (View mode)"dired-view-file)
      ("=" "  Diff"                 dired-diff)
      ("w" "  Copy filename"        dired-copy-filename-as-kill)
      ("W" "  Open in browser"      browse-url-of-dired-file)
      ("y" "  Show file type"       dired-show-file-type)]
     ["Attribute"
      ("R"   "Rename"               dired-do-rename)
      ("G"   "Group"                dired-do-chgrp)
      ("M"   "Mode"                 dired-do-chmod)
      ("O"   "Owner"                dired-do-chown)
      ("T"   "Timestamp"            dired-do-touch)]
     ["Navigation"
      ("j" "  Goto file"            dired-goto-file)
      ("+" "  Create directory"     dired-create-directory)
      ("<" "  Jump prev directory"  dired-prev-dirline)
      (">" "  Jump next directory"  dired-next-dirline)
      ("^" "  Move up directory"    dired-up-directory)]
     ["Display"
      ("g" "  Refresh buffer"       revert-buffer)
      ("l" "  Refresh file"         dired-do-redisplay)
      ("k" "  Remove line"          dired-do-kill-lines)
      ("s" "  Sort"                 dired-sort-toggle-or-edit)
      ("(" "  Toggle detail info"   dired-hide-details-mode)
      ("i" "  Insert subdir"        dired-maybe-insert-subdir)
      ("$" "  Hide subdir"          dired-hide-subdir)
      ("M-$" "Hide subdir all"      dired-hide-subdir)]
     ["Extension"
      ("e"   "wdired"               wdired-change-to-wdired-mode)
      ("/"   "dired-filter"         ignore)
      ("n"   "dired-narrow"         ignore)]]
    [["Marks"
      ("m" "Marks..." oht-transient-dired-marks)]])

  (transient-define-prefix oht-transient-dired-marks ()
    "Sub-transient for dired marks"
    ["Dired Mode -> Marks"
     ["Toggles"
      ("mm"  "Mark"                 dired-mark)
      ("mM"  "Mark all"             dired-mark-subdir-files)
      ("mu"  "Unmark"               dired-unmark)
      ("mU"  "Unmark all"           dired-unmark-all-marks)
      ("mc"  "Change mark"          dired-change-marks)
      ("mt"  "Toggle mark"          dired-toggle-marks)]
     ["Type"
      ("m*"  "Executables"          dired-mark-executables)
      ("m/"  "Directories"          dired-mark-directories)
      ("m@"  "Symlinks"             dired-mark-symlinks)
      ("m&"  "Garbage files"        dired-flag-garbage-files)
      ("m#"  "Auto save files"      dired-flag-auto-save-files)
      ("m~"  "backup files"         dired-flag-backup-files)
      ("m."  "Numerical backups"    dired-clean-directory)]
     ["Search"
      ("m%"  "Regexp"               dired-mark-files-regexp)
      ("mg"  "Regexp file contents" dired-mark-files-containing-regexp)]]
    [["Act on Marked"
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
      ("&"   "Async shell command"  dired-do-async-shell-command)]])

  ) ; End "use-package dired"


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
  :blackout selected-minor-mode)


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
  :bind
  ("M-;" . #'flyspell-auto-correct-previous-word)
  :config
  (transient-define-prefix oht-transient-spelling ()
    "Transient for a spelling interface"
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
  :blackout " Spell")

(use-package flyspell-correct
  ;; used in spelling transient
  :commands (flyspell-correct-wrapper)
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
  :init
  (exec-path-from-shell-initialize))

(use-package olivetti
  :commands olivetti-mode
  :custom (olivetti-body-width 84)
  :blackout " Olvti")

(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo))

(use-package expand-region
  :bind
  ("s-r" . er/expand-region)
  ("s-R" . er/contract-region))

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
  :bind
  ("s-C" . org-capture)
  (:map org-mode-map
        ("s-\\" . oht-transient-org))
  :config

  (custom-set-variables
   '(org-list-allow-alphabetical t)
   '(org-enforce-todo-dependencies t)
   '(org-enforce-todo-checkbox-dependencies t)
   '(org-log-done 'time)
   '(org-log-into-drawer t))

  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-adapt-indentation nil
        org-catch-invisible-edits 'show-and-error
        org-outline-path-complete-in-steps nil
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-hide-emphasis-markers t
        org-ellipsis "..."
        org-insert-heading-respect-content t
        org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+"))
        org-list-indent-offset 2)

  ;; src blocks
  (setq org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0)
  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))

  ;; Agenda Settings
  (setq org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-start-with-log-mode t
        org-agenda-use-time-grid nil
        org-deadline-warning-days 5
        org-agenda-todo-ignore-scheduled 'all
        org-agenda-todo-ignore-deadlines 'near
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-sorting-strategy '(((agenda habit-down time-up priority-down category-up)
                                       (todo category-up priority-down)
                                       (tags priority-down category-keep)
                                       (search category-keep))))

  (setq org-agenda-files (list oht-orgfiles))

  (when (string= (system-name) "shadowfax.local")
    (add-to-list 'org-agenda-files "~/home/writing/kindred/compendium.org"))

  (setq org-agenda-custom-commands
        '(("1" "TODAY: Today's Agenda + Priority Tasks"
           ((agenda "d" ((org-agenda-span 'day)))
            (todo "TODO"
                  ((org-agenda-sorting-strategy '(todo-state-up))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))))
          ("0" "COMPLETE: Week Agenda + All Tasks"
           ((agenda "w" ((org-agenda-span 'week)))
            (todo "TODO|LATER"
                  ((org-agenda-sorting-strategy '(todo-state-up))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))
                  )))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "LATER(l)" "|" "DONE(d)" "CANCELED(c)")))

  (setq org-capture-templates
        `(("p" "Personal")
          ("pi" "Personal Inbox" entry
           (file+headline ,(concat oht-orgfiles "life.org") "Inbox")
           "* %?\n\n" :empty-lines 1)
          ("pl" "Personal Log Entry" entry
           (file+olp+datetree ,(concat oht-orgfiles "logbook.org"))
           "* %?\n%T\n\n" :empty-lines 1 :tree-type month )
          ;; -----------------------------
          ("i" "Ingenuity")
          ("ii" "Ingenuity Inbox" entry
           (file+headline ,(concat oht-orgfiles "ingenuity.org") "Inbox")
           "* %?\n\n" :empty-lines 1)
          ("il" "Ingenuity Log Entry" entry
           (file ,(concat oht-orgfiles "ingenuity_logbook.org"))
           "* %? %T\n\n" :empty-lines 1)
          ("if" "Ingenuity Mail Follow Up" entry
           (file+headline ,(concat oht-orgfiles "ingenuity.org") "Mail")
           "* TODO %a\n\n  %i" :empty-lines 1)
          ;; -----------------------------
          ("s" "Scanline")
          ("sl" "Scanline Log Entry" entry
           (file+olp+datetree ,(concat oht-orgfiles "scanline_logbook.org"))
           "* %^{prompt}\n%T\n\n%?" :empty-lines 1 :tree-type week )
          ;; -----------------------------
          ("e" "Emacs Config" entry
           (file+headline ,(concat oht-orgfiles "emacs.org") "Emacs Config")
           "* TODO %?" :empty-lines 1)))

  (defun oht/org-insert-date-today ()
    "Insert today's date using standard org formatting."
    (interactive)
    (org-insert-time-stamp (current-time)))

  (defun oht/org-insert-date-today-inactive ()
    "Inserts today's date in org inactive format."
    (interactive)
    (insert (format-time-string "\[%Y-%m-%d %a\]")))

  ;; Functions for directly calling agenda commands, skipping the prompt.
  ;; Useful when paired with transient.
  (defun oht-org-agenda-today () (interactive) (org-agenda nil "1"))
  (defun oht-org-agenda-complete () (interactive) (org-agenda nil "0"))
  (defun oht-org-agenda-agenda () (interactive) (org-agenda nil "a"))
  (defun oht-org-agenda-todos () (interactive) (org-agenda nil "t"))

  ;; Functions for directly setting todo status, skipping the prompt.
  ;; Useful when paired with transient.
  (defun org-todo-set-todo () (interactive) (org-todo "TODO"))
  (defun org-agenda-todo-set-todo () (interactive) (org-agenda-todo "TODO"))
  (defun org-todo-set-later () (interactive) (org-todo "LATER"))
  (defun org-agenda-todo-set-later () (interactive) (org-agenda-todo "LATER"))
  (defun org-todo-set-done () (interactive) (org-todo "DONE"))
  (defun org-agenda-todo-set-done () (interactive) (org-agenda-todo "DONE"))
  (defun org-agenda-todo-set-canceled () (interactive) (org-agenda-todo "CANCELED"))
  (defun org-todo-set-canceled () (interactive) (org-todo "CANCELED"))

  (transient-define-prefix oht-transient-org ()
    "Transient for Org Mode"
    ["Org Mode"
     ["Navigation"
      ("o" "Outline" consult-outline)
      ("n" "Narrow/Widen" narrow-or-widen-dwim)
      ("g" "Go To" org-goto)
      ("m" "Visible Markup" visible-mode)]
     ["Item"
      ("t" "TODO" oht-transient-org-todo)
      ("I" "Clock In" org-clock-in)
      ("O" "Clock Out" org-clock-out)
      ("a" "Archive Subtree" org-archive-subtree)
      ("r" "Refile" org-refile)
      ("c" "Checkbox" org-toggle-checkbox)]
     ["Insert"
      ("." "Insert Date, Active" oht/org-insert-date-today)
      (">" "Insert Date, Inactive" oht/org-insert-date-today-inactive)
      ("<" "Structure Template" org-insert-structure-template)]
     ["Links"
      ("s" "Store Link" org-store-link)
      ("i" "Insert Link" org-insert-last-stored-link)]])

  (transient-define-prefix oht-transient-org-todo ()
    "A transient for setting org todo status.
I've created this because I don't like how org-todo messes with
windows. There is likely a much better way to automatically map
org-todo-keywords to a transient command."
    ["Org mode -> Change Status To..."
     [("t" "TODO"     org-todo-set-todo)
      ("l" "LATER"    org-todo-set-later)]
     [("d" "DONE"     org-todo-set-done)
      ("c" "CANCELED" org-todo-set-canceled)]])

  ) ; End "use-package org"


(use-package org-agenda
  :straight nil
  :commands org-agenda
  :bind
  (:map org-agenda-mode-map
        ("t" . oht-transient-org-agenda)
        ("s-z" . org-agenda-undo))
  :hook (org-agenda-mode-hook . hl-line-mode)
  :config
  (transient-define-prefix oht-transient-org-agenda ()
    "A transient for setting org-agenda todo status.
I've created this because I don't like how org-todo messes with
windows. There is likely a much better way to automatically map
org-todo-keywords to a transient command."
    ["Change Status To..."
     [("t" "TODO"     org-agenda-todo-set-todo)
      ("l" "LATER"    org-agenda-todo-set-later)]
     [("d" "DONE"     org-agenda-todo-set-done)
      ("c" "CANCELED" org-agenda-todo-set-canceled)]])
  ) ; End "use-package org-agenda"


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
  (defun oht-eww-fonts ()
    "Apply some customization to fonts in eww-mode."
    (facedancer-vadjust-mode)
    (text-scale-increase 1)
    (setq-local line-spacing 2))
  :commands (eww)
  :hook (eww-mode-hook . oht-eww-fonts)
  :bind
  (:map eww-mode-map
        ("s-\\" . oht-transient-eww))
  :config

  (make-variable-buffer-local
   (defvar eww-inhibit-images-status nil
     "EWW Inhibit Images Status"))

  (defun eww-inhibit-images-toggle ()
    (interactive)
    (setq eww-inhibit-images-status (not eww-inhibit-images-status))
    (if eww-inhibit-images-status
        (progn (setq-local shr-inhibit-images t)
               (eww-reload t))
      (progn (setq-local shr-inhibit-images nil)
             (eww-reload t))))

  (defun oht-eww-open-in-new-buffer-bury ()
    "Open URL at point in a new buried buffer"
    (interactive)
    (eww-open-in-new-buffer)
    (quit-window)
    (message "Browsing in buried buffer"))

  (defun prot-eww--rename-buffer ()
    "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
    (let ((name (if (eq "" (plist-get eww-data :title))
                    (plist-get eww-data :url)
                  (plist-get eww-data :title))))
      (rename-buffer (format "*eww # %s*" name) t)))

  (add-hook 'eww-after-render-hook #'prot-eww--rename-buffer)
  (advice-add 'eww-back-url :after #'prot-eww--rename-buffer)
  (advice-add 'eww-forward-url :after #'prot-eww--rename-buffer)

  (transient-define-prefix oht-transient-eww ()
    "Transient for EWW"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["EWW"
     ["Actions"
      ("G" "Browse" eww)
      ("M-<return>" "Open in new buffer" oht-eww-open-in-new-buffer-bury)
      ("&" "Browse With External Browser" eww-browse-with-external-browser)
      ("w" "Copy URL" eww-copy-page-url)]
     ["Display"
      ("i" "Toggle Images" eww-inhibit-images-toggle)
      ("F" "Toggle Fonts" eww-toggle-fonts)
      ("R" "Readable" eww-readable)
      ("M-C" "Colors" eww-toggle-colors)]
     ["History"
      ("H" "History" eww-list-histories)
      ("l" "Back" eww-back-url)
      ("r" "Forward" eww-forward-url)]
     ["Bookmarks"
      ("a" "Add Eww Bookmark" eww-add-bookmark)
      ("b" "Bookmark" bookmark-set)
      ("B" "List Bookmarks" eww-list-bookmarks)
      ("M-n" "Next Bookmark" eww-next-bookmark)
      ("M-p" "Previous Bookmark" eww-previous-bookmark)]])

  ) ; End "use-package eww"

(use-package elfeed
  :if (string= (system-name) "shadowfax.local")
  :commands elfeed
  :init
  (defun oht-elfeed-show-fonts ()
    "Apply some customization to fonts in elfeed-show-mode."
    (facedancer-vadjust-mode)
    (setq-local line-spacing 3))
  :hook (elfeed-show-mode-hook . oht-elfeed-show-fonts)
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
        ("b" . elfeed-search-browse-url)
        ("B" . oht-elfeed-search-browse-and-bury)
        ("*" . elfeed-search-tag--star)
        ("8" . elfeed-search-untag--star)
        ("o" . delete-other-windows)
        ("N" . oht-elfeed-unread-news)
        ("E" . oht-elfeed-unread-emacs)
        ("O" . oht-elfeed-unread-other)
        ("S" . oht-elfeed-starred))
  (:map elfeed-show-mode-map
        ("&" . oht-elfeed-show-visit-generic)
        ("r" . elfeed-show-tag--read)
        ("u" . elfeed-show-tag--unread)
        ("*" . elfeed-show-tag--star)
        ("8" . elfeed-show-tag--unstar)
        ("o" . delete-other-windows)
        ("d" . oht-elfeed-show-download-video)
        ("i" . elfeed-inhibit-images-toggle)
        ("B" . oht-elfeed-show-browse-and-bury))
  :config
  ;; My feed list is stored outside my dotfiles -- not public.
  (load "~/home/src/rss-feeds.el")

  ;; Elfeed doesn't have a built-in way of flagging or marking items for later,
  ;; but it does have tags, which you can use for this. The below is some simple
  ;; aliases for adding and removing the 'star' tag.
  (defalias 'elfeed-search-tag--star
    (elfeed-expose #'elfeed-search-tag-all 'star)
    "Add the 'star' tag to all selected entries")
  (defalias 'elfeed-search-untag--star
    (elfeed-expose #'elfeed-search-untag-all 'star)
    "Remove the 'star' tag to all selected entries")
  (defalias 'elfeed-show-tag--star
    (elfeed-expose #'elfeed-show-tag 'star)
    "Add the 'star' tag to current entry")
  (defalias 'elfeed-show-tag--unstar
    (elfeed-expose #'elfeed-show-untag 'star)
    "Remove the 'star' tag to current entry")

  ;; Even though there are bindings for marking items as 'read' and 'unread' in
  ;; the search-mode, there are no such built-in bindings for show-mode. So we
  ;; add them here.
  (defalias 'elfeed-show-tag--unread
    (elfeed-expose #'elfeed-show-tag 'unread)
    "Mark the current entry unread.")
  (defalias 'elfeed-show-tag--read
    (elfeed-expose #'elfeed-show-untag 'unread)
    "Mark the current entry read.")

  (defun oht-elfeed-unread-news  () (interactive) (elfeed-search-set-filter "+unread +news"))
  (defun oht-elfeed-unread-emacs () (interactive) (elfeed-search-set-filter "+unread +emacs"))
  (defun oht-elfeed-unread-other () (interactive) (elfeed-search-set-filter "+unread -emacs -news"))
  (defun oht-elfeed-starred      () (interactive) (elfeed-search-set-filter "+star"))

  (defun oht-elfeed-show-visit-generic ()
    "Wrapper for elfeed-show-visit to use system browser instead of eww"
    (interactive)
    (let ((browse-url-generic-program "/usr/bin/open"))
      (elfeed-show-visit t)))

  (make-variable-buffer-local
   (defvar elfeed-inhibit-images-status nil
     "Elfeed Inhibit Images Status"))

  (defun elfeed-inhibit-images-toggle ()
    "Toggle display of images in elfeed-show."
    (interactive)
    (setq elfeed-inhibit-images-status (not elfeed-inhibit-images-status))
    (if elfeed-inhibit-images-status
        (progn
          (setq-local shr-inhibit-images t)
          (elfeed-show-refresh))
      (progn
        (setq-local shr-inhibit-images nil)
        (elfeed-show-refresh))))

  (defun oht-elfeed-show-download-video ()
    "In elfeed-show-mode, download a video using youtube-dl."
    (interactive)
    (async-shell-command (format "%s -o \"%s%s\" -f mp4 \"%s\""
                                 youtube-dl-path
                                 youtube-dl-output-dir
                                 "%(title)s.%(ext)s"
                                 (elfeed-entry-link elfeed-show-entry))))

  (defun oht-elfeed-search-browse-and-bury ()
    "Browse elfeed entry and bury buffer."
    (interactive)
    (elfeed-search-browse-url)
    (bury-buffer)
    (message "Browsing in buried buffer"))

  (defun oht-elfeed-show-browse-and-bury ()
    "Browse elfeed entry and bury buffer."
    (interactive)
    (elfeed-show-visit)
    (bury-buffer)
    (message "Browsing in buried buffer"))

  ) ; End "use-package elfeed"

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


;;;; Transient

;; I LOVE transient commands. Basically, I don't want to learn a lot of
;; keybindings, I want a simple set of bindings that reveal the vast power
;; at your fingertips. So any time I find myself using a mode/package that I
;; can't remember the bindings/commands for I take the time to comb through
;; the source code, find anything I think I might use, and pop it in a
;; transient.

(use-package transient
  :straight nil
  :init
  ;; Any commands these transients use, whose packages are potentially not
  ;; loaded yet, need to be autoloaded.
  (autoload 'org-store-link "org")
  :custom
  (transient-mode-line-format 'line)
  (transient-display-buffer-action '(display-buffer-below-selected))
  :bind*
  ("s-<return>" . oht-transient-general)
  ("s-w" . oht-transient-window)
  ("s-2" . oht-transient-2nd)
  ("s-d" . oht-transient-dispatch)
  :config

  (transient-define-prefix oht-transient-general ()
    "General-purpose transient.
I use this transient command as a jumping-off point. Many of the
more following specific transients are included here. The idea is
that, when lost, one can simply call this one transient and get
wherever you need to go."
    ["General"
     ["Quick Actions!"
      ("f" "Find File" find-file)
      ("m" "Magit Status" magit-status)
      ("o" "Consult Outline" consult-outline)
      ("a" "AutoFill" auto-fill-mode)
      ("j" "Dired Jump" dired-jump)
      ("s" "Store Org Link" org-store-link)
      ("g" "Consult Grep" consult-grep)]
     ["Transients"
      ("O" "Outline Navigation..." oht-transient-outline)
      ("D" "Display..."   oht-transient-display)
      ("F" "Fonts..." oht-transient-fonts)
      ("T" "Tabs..."      oht-transient-tabs)
      ("B" "Bookmarks..." oht-transient-bookmarks)
      ("S" "Spelling..." oht-transient-spelling)
      ("H" "Helpful Commands..." oht-transient-help)
      ("M" "Toggle Minor Modes" consult-minor-mode-menu)]])

  (transient-define-prefix oht-transient-outline ()
    "Transient for Outline Minor Mode navigation"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["General -> Outline Navigation"
     ["Show/Hide"
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

  (transient-define-prefix oht-transient-display ()
    "A transient for controlling Emacs display"
    ["General -> Display"
     [("h" "Highlight Line" hl-line-mode)
      ("l" "Line Numbers" global-display-line-numbers-mode)
      ("g" "Fill Column" global-display-fill-column-indicator-mode)
      ("w" "Wrap" visual-line-mode)
      ("t" "Truncate" toggle-truncate-lines)
      ("W" "Whitespace" whitespace-mode)
      ("c" "Composition Mode" composition-mode)]])

  (transient-define-prefix oht-transient-fonts ()
    "Set Font Properties"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["General -> Fonts"
     ["Modes"
      ("v" "Var Mode" variable-pitch-mode)
      ("V" "V+ Mode" facedancer-vadjust-mode)
      ("o" "Olivetti" olivetti-mode)
      ("w" "Wrap" visual-line-mode)
      ("c" "Composition" composition-mode)]
     ["Size"
      ("0" "Reset Size" text-scale-mode)
      ("=" "Larger" text-scale-increase)
      ("+" "Larger" text-scale-increase)
      ("-" "Smaller" text-scale-decrease)]
     ["Other"
      ("s" "Line Spacing" facedancer-line-spacing)
      ("m" "Modus Toggle" modus-themes-toggle)]])

  (transient-define-prefix oht-transient-tabs ()
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["General -> Tabs"
     [("t" "Tab Bar Mode" tab-bar-mode)
      ("n" "New" tab-bar-new-tab)
      ("k" "Kill" tab-bar-close-tab)
      ("z" "Undo Kill" tab-bar-undo-close-tab)
      ("]" "Next" tab-bar-switch-to-next-tab)
      ("[" "Previous" tab-bar-switch-to-prev-tab)]])

  (transient-define-prefix oht-transient-bookmarks ()
    "A transient for controlling bookmarks"
    [["General -> Bookmarks"
      ("s" "Set"  bookmark-set)
      ("l" "List" list-bookmarks)
      ("j" "Jump" consult-bookmark)]])

  (transient-define-prefix oht-transient-help ()
    "Transient for Helpful commands"
    ["General -> Helpful Commands"
     [("p" "At Point" helpful-at-point)]
     [("c" "Callable" helpful-callable)
      ("f" "Function" helpful-function)
      ("C" "Command" helpful-command)
      ("v" "Variable" helpful-variable)
      ("s" "Symbol" helpful-symbol)
      ("M" "Macro" helpful-macro)
      ("k" "Key" helpful-key)
      ("m" "Mode" helpful-mode)]
     [("u" "Update" helpful-update)
      ("V" "Visit Reference" helpful-visit-reference)
      ("K" "Kill Helpful Buffers" helpful-kill-buffers)]])

  (transient-define-prefix oht-transient-window ()
    "Most commonly used window commands"
    [["Splits"
      ("s" "Horizontal" split-window-below)
      ("v" "Vertical"   split-window-right)
      ("b" "Balance"    balance-windows)
      ("f" "Fit"        fit-window-to-buffer)
      ("r" "Rotate"     toggle-window-split)]
     ["Window"
      ("c" "Clone Indirect" clone-indirect-buffer)
      ("t" "Tear Off" tear-off-window)
      ("k" "Kill" delete-window)
      ("K" "Kill Buffer+Win"  kill-buffer-and-window)
      ("o" "Kill Others"  delete-other-windows)
      ("m" "Maximize" maximize-window)]
     ["Navigate"
      ("<left>"  "←" windmove-left  :transient t)
      ("<right>" "→" windmove-right :transient t)
      ("<up>"    "↑" windmove-up    :transient t)
      ("<down>"  "↓" windmove-down  :transient t)]
     ["Move"
      ("S-<left>"  "Move ←" buf-move-left  :transient t)
      ("S-<right>" "Move →" buf-move-right :transient t)
      ("S-<up>"    "Move ↑" buf-move-up    :transient t)
      ("S-<down>"  "Move ↓" buf-move-down  :transient t)]
     ["Undo/Redo"
      ("s-z" "Winner Undo" winner-undo :transient t)
      ("s-Z" "Winner Redo" winner-redo :transient t)]])

  ) ; End "use-package transient"


;;;; Secondary Selection

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

(transient-define-prefix oht-transient-2nd ()
  "Transient for working with the secondary selection"
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

;;;; Dispatch

(defun oht-dispatch-downloads () (interactive) (find-file "~/Downloads"))
(defun oht-dispatch-reading () (interactive) (find-file "~/Downloads/reading"))
(defun oht-dispatch-watch () (interactive) (find-file "~/Downloads/watch"))
(defun oht-dispatch-NPR-news () (interactive) (browse-url "https://text.npr.org"))
(defun oht-dispatch-CNN-news () (interactive) (browse-url "https://lite.cnn.com/en"))
(defun oht-dispatch-google-news () (interactive) (browse-url "http://68k.news/"))

(transient-define-prefix oht-transient-dispatch ()
  "Jump directly to your most-used stuff."
  ["Work"
   [("t" "Today + Priority" oht-org-agenda-today)
    ("0" "Week + TODOs" oht-org-agenda-complete)
    ("a" "Agenda" oht-org-agenda-agenda)
    ("T" "TODOs" oht-org-agenda-todos)
    ("A" "Org Agenda Command..." org-agenda)]
   [("m" "Mail" mu4e)]]
  ["Browsing"
   [("e" "Elfeed"      elfeed)
    ("h" "Hacker News" hackernews)]
   [("E" "EWW"         eww)
    ("n" "NPR News"    oht-dispatch-NPR-news)
    ("c" "CNN News"    oht-dispatch-CNN-news)
    ("g" "Google News" oht-dispatch-google-news)]
   [("d" "Downloads"   oht-dispatch-downloads)
    ("r" "Reading"     oht-dispatch-reading)
    ("w" "Watch"       oht-dispatch-watch)]])


;;;; Facedancer

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


;;;; Find File Directories

;; BEAUTIFUL set of functions from Radian for creating directories when
;; finding files.

;; The following code is taken directly from Radian
;; https://github.com/raxod502/radian

(defvar radian--dirs-to-delete nil
  "List of directories to try to delete when killing buffer.
This is used to implement the neat feature where if you kill a
new buffer without saving it, then Radian will prompt to see if
you want to also delete the parent directories that were
automatically created.")

(defun radian--advice-find-file-create-directories
    (find-file filename &rest args)
  "Automatically create and delete parent directories of new files.
This advice automatically creates the parent directory (or directories) of
the file being visited, if necessary. It also sets a buffer-local
variable so that the user will be prompted to delete the newly
created directories if they kill the buffer without saving it.

This advice has no effect for remote files.

This is an `:around' advice for `find-file' and similar
functions.

FIND-FILE is the original `find-file'; FILENAME and ARGS are its
arguments."
  (if (file-remote-p filename)
      (apply find-file filename args)
    (let ((orig-filename filename)
          ;; For relative paths where none of the named parent
          ;; directories exist, we might get a nil from
          ;; `file-name-directory' below, which would be bad. Thus we
          ;; expand the path fully.
          (filename (expand-file-name filename))
          ;; The variable `dirs-to-delete' is a list of the
          ;; directories that will be automatically created by
          ;; `make-directory'. We will want to offer to delete these
          ;; directories if the user kills the buffer without saving
          ;; it.
          (dirs-to-delete ()))
      ;; If the file already exists, we don't need to worry about
      ;; creating any directories.
      (unless (file-exists-p filename)
        ;; It's easy to figure out how to invoke `make-directory',
        ;; because it will automatically create all parent
        ;; directories. We just need to ask for the directory
        ;; immediately containing the file to be created.
        (let* ((dir-to-create (file-name-directory filename))
               ;; However, to find the exact set of directories that
               ;; might need to be deleted afterward, we need to
               ;; iterate upward through the directory tree until we
               ;; find a directory that already exists, starting at
               ;; the directory containing the new file.
               (current-dir dir-to-create))
          ;; If the directory containing the new file already exists,
          ;; nothing needs to be created, and therefore nothing needs
          ;; to be destroyed, either.
          (while (not (file-exists-p current-dir))
            ;; Otherwise, we'll add that directory onto the list of
            ;; directories that are going to be created.
            (push current-dir dirs-to-delete)
            ;; Now we iterate upwards one directory. The
            ;; `directory-file-name' function removes the trailing
            ;; slash of the current directory, so that it is viewed as
            ;; a file, and then the `file-name-directory' function
            ;; returns the directory component in that path (which
            ;; means the parent directory).
            (setq current-dir (file-name-directory
                               (directory-file-name current-dir))))
          ;; Only bother trying to create a directory if one does not
          ;; already exist.
          (unless (file-exists-p dir-to-create)
            ;; Make the necessary directory and its parents.
            (make-directory dir-to-create 'parents))))
      ;; Call the original `find-file', now that the directory
      ;; containing the file to found exists. We make sure to preserve
      ;; the return value, so as not to mess up any commands relying
      ;; on it.
      (prog1 (apply find-file orig-filename args)
        ;; If there are directories we want to offer to delete later,
        ;; we have more to do.
        (when dirs-to-delete
          ;; Since we already called `find-file', we're now in the
          ;; buffer for the new file. That means we can transfer the
          ;; list of directories to possibly delete later into a
          ;; buffer-local variable. But we pushed new entries onto the
          ;; beginning of `dirs-to-delete', so now we have to reverse
          ;; it (in order to later offer to delete directories from
          ;; innermost to outermost).
          (setq-local radian--dirs-to-delete (reverse dirs-to-delete))
          ;; Now we add a buffer-local hook to offer to delete those
          ;; directories when the buffer is killed, but only if it's
          ;; appropriate to do so (for instance, only if the
          ;; directories still exist and the file still doesn't
          ;; exist).
          (add-hook 'kill-buffer-hook
                    #'radian--kill-buffer-delete-directory-if-appropriate
                    'append 'local)
          ;; The above hook removes itself when it is run, but that
          ;; will only happen when the buffer is killed (which might
          ;; never happen). Just for cleanliness, we automatically
          ;; remove it when the buffer is saved. This hook also
          ;; removes itself when run, in addition to removing the
          ;; above hook.
          (add-hook 'after-save-hook
                    #'radian--remove-kill-buffer-delete-directory-hook
                    'append 'local))))))

(defun radian--kill-buffer-delete-directory-if-appropriate ()
  "Delete parent directories if appropriate.
This is a function for `kill-buffer-hook'. If
`radian--advice-find-file-create-directories' created the
directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if the local variables have been killed.
         (boundp 'radian--dirs-to-delete)
         ;; Stop if there aren't any directories to delete (shouldn't
         ;; happen).
         radian--dirs-to-delete
         ;; Stop if `radian--dirs-to-delete' somehow got set to
         ;; something other than a list (shouldn't happen).
         (listp radian--dirs-to-delete)
         ;; Stop if the current buffer doesn't represent a
         ;; file (shouldn't happen).
         buffer-file-name
         ;; Stop if the buffer has been saved, so that the file
         ;; actually exists now. This might happen if the buffer were
         ;; saved without `after-save-hook' running, or if the
         ;; `find-file'-like function called was `write-file'.
         (not (file-exists-p buffer-file-name)))
    (cl-dolist (dir-to-delete radian--dirs-to-delete)
      ;; Ignore any directories that no longer exist or are malformed.
      ;; We don't return immediately if there's a nonexistent
      ;; directory, because it might still be useful to offer to
      ;; delete other (parent) directories that should be deleted. But
      ;; this is an edge case.
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete))
        ;; Only delete a directory if the user is OK with it.
        (if (y-or-n-p (format "Also delete directory `%s'? "
                              ;; The `directory-file-name' function
                              ;; removes the trailing slash.
                              (directory-file-name dir-to-delete)))
            (delete-directory dir-to-delete)
          ;; If the user doesn't want to delete a directory, then they
          ;; obviously don't want to delete any of its parent
          ;; directories, either.
          (cl-return)))))
  ;; It shouldn't be necessary to remove this hook, since the buffer
  ;; is getting killed anyway, but just in case...
  (radian--remove-kill-buffer-delete-directory-hook))

(defun radian--remove-kill-buffer-delete-directory-hook ()
  "Clean up directory-deletion hooks, if necessary.
This is a function for `after-save-hook'. Remove
`radian--kill-buffer-delete-directory-if-appropriate' from
`kill-buffer-hook', and also remove this function from
`after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'radian--kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'radian--remove-kill-buffer-delete-directory-hook
               'local))

(dolist (fun '(find-file           ; C-x C-f
               find-alternate-file ; C-x C-v
               write-file          ; C-x C-w
               ))
  (advice-add fun :around #'radian--advice-find-file-create-directories))


;;; End of init.el
