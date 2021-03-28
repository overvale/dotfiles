;;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Homepage: https://github.com/olivertaylor/dotfiles

;;; Settings


;;;; Performance

;; Let's start by doing everything we can to improve emacs performance.
;; All of the below are stolen (with comments) from Doom Emacs.
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; https://github.com/hlissner/doom-emacs/blob/develop/core/core.el

;; Garbage Collection!
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

;; Inhibit most of the default stuff on startup, and ensure nothing
;; extra is loaded.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;;;; Preferences

;; Activate modes to set settings -- sure Emacs!
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(winner-mode 1)
(show-paren-mode t)
(blink-cursor-mode -1)
(set-language-environment "UTF-8")

;; There are two kinds of variables, global ones, and buffer local ones.
;;
;; `setq' simply sets the value of a variable, so if the variable global it
;; sets its value globally, and if the variable is buffer local it sets the
;; value locally (and new buffers will inherit the default value).
;;
;; `setq-local' takes a global variable and makes a buffer local "copy" that
;; doesn't effect the global value.
;;
;; `setq-default' takes a local variable and sets a new default value for all
;; new buffers, but doesn't change it in existing buffers.

;; Visual/Interface Stuff
(setq-default cursor-type 'box)
(setq visible-bell t)
(setq-default indicate-empty-lines nil)
(setq frame-title-format '("%b"))
(setq uniquify-buffer-name-style 'forward)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq split-window-keep-point nil)

;; Settings
(setq vc-follow-symlinks t
      find-file-visit-truename t)
(setq create-lockfiles nil
      make-backup-files nil)
(setq sentence-end-double-space nil)
(setq locate-command "mdfind")
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash/emacs")
(setq confirm-kill-processes nil)


;;;; Minibuffer

;; An example: you call `switch-buffer' and search for something in the
;; minibuffer. You then want to call a command inside that minibuffer.
(setq enable-recursive-minibuffers 1)

;; And show an indicator when you do that.
(minibuffer-depth-indicate-mode 1)


;;;; Kill Ring

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

;; And remove duplicates from the kill-ring
(setq kill-do-not-save-duplicates t)


;;;; Emacs Directory

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


;;;; Tab/Fill Settings, Visual Line Mode

(setq-default tab-width 4
			  fill-column 78)

;; Confusingly, `visual-line-mode', `word-wrap', and `truncate-lines' are all
;; different things. `visual-line-mode' is a wrapper around a bunch of
;; things, probably best explained here:
;; http://ergoemacs.org/emacs/emacs_long_line_wrap.html
;; `word-wrap' ONLY wraps lines word-wise instead of character-wise.
;; `truncate-lines' ONLY controls if wrapping happens at all. If set to
;; non-nil it is supposed to let lines run off the window, but this is a
;; buffer-local setting that I cannot (no matter what I try) get to be global.
(setq-default truncate-lines t)

;; So, instead, I take the brute-force approach of adding a hook for text-mode
;; and prog-mode to call a function which toggles the value on. It's not
;; perfect, but it works 99% of the time. Take that Emacs.
(defun oht-mac-truncate-lines()
  (toggle-truncate-lines 1))
(add-hook 'text-mode-hook 'oht-mac-truncate-lines)
(add-hook 'prog-mode-hook 'oht-mac-truncate-lines)

;; When visual-line-mode is off and truncate-lines is toggled off, I still
;; want wrapping to happen at the word instead of character.
(setq-default word-wrap 1)


;;;; Mode Line

(column-number-mode t)
(display-time-mode t)
(setq display-time-format "%H:%M  %Y-%m-%d")
(setq display-time-interval 60)
(setq display-time-mail-directory nil)
(setq display-time-default-load-average nil)


;;; Packages


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


;;;; bind-key, backout

;; These packages are relied upon by my use-package declarations, therefore
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


;;;; Narrowing & Searching

;; Navigating and using the thousands of things Emacs can do is built around
;; the idea of searching and narrowing a selection down to the thing you're
;; looking for. To make this easier I've installed a few packages that enhance
;; Emacs built-in facilities for doing this.
;;
;; There are MANY approaches to this. I'm following the popular trend lead by
;; @oantolin, @raxod502, @minad, and @protesilaos. Generally, it works like
;; this:
;;
;; 1. Use Selectrum as the main interface for completion narrowing (by default).
;; 2. Use Prescient to sort those completions and provide fuzzy matching.
;; 3. Use Marginalia to decorate the completions.
;; 4. Use Embark so you can act on the list of completions (among other things).
;; 5. When Selectrum is disabled (and thus also prescient) fall back on Orderless.
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
  :init
  (marginalia-mode 1)
  (setq marginalia-annotators
	'(marginalia-annotators-heavy marginalia-annotators-light))
  ;; if using Selectrum
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  )

(use-package embark
  :straight (:host github :repo "oantolin/embark" :branch "master")
  :bind
  ("s-e" . embark-act)
  (:map minibuffer-local-map
        ("C-o" . embark-switch-to-collect-completions))
  (:map embark-url-map
	("d" . youtube-dl-URL-at-point)
	("&" . browse-url-default-macosx-browser))
  :config
  (load (concat oht-dotfiles "lisp/oht-embark.el"))
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

This simply removes the hooks added by the function `use-embark-completions'."
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
  ("s-f" . consult-line)
  ([remap yank-pop] . consult-yank-pop)
  :config
  (setq consult-preview-key (kbd "C-="))
  (setq consult-config
	`((consult-mark :preview-key any)
	  (consult-buffer :preview-key any)))
  (setq consult-project-root-function #'vc-root-dir)
  )

(use-package embark-consult
  :straight nil
  :after (embark consult)
  )

(use-package ctrlf
  :bind
  ("C-s" . ctrlf-forward-fuzzy)
  ("C-r" . ctrlf-backward-fuzzy)
  :config
  (setq ctrlf-go-to-end-of-match nil)
  )

;;;; Built-In Packages

(use-package remember
  ;; The below replaces Emacs's regular *scratch* buffer with the remember-notes
  ;; file/buffer, and sets some functions/bindings for quickly adding
  ;; information to it. Think of it like a super lightweight org-mode capture
  ;; whose inbox you see every time Emacs starts up.
  :straight nil
  :init
  (setq remember-data-file "~/home/org/remember-notes")
  (setq remember-notes-buffer-name "*scratch*")
  (setq remember-notes-auto-save-visited-file-name t)
  ;; Set initial-buffer-choice to a function which kills the *scratch* buffer
  ;; and opens the remember-notes buffer. This loads `remember'.
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
  :bind
  ("s-_" . oht-remember-dwim)
  ("s--" . remember-notes)
 )

(use-package bookmark
  :straight nil
  :commands (list-bookmarks)
  :init
  (defun oht-bookmark-fonts ()
    (hl-line-mode 1)
    )
  :hook (bookmark-bmenu-mode . oht-bookmark-fonts)
  :custom
  (bookmark-save-flag 1)
  (bookmark-default-file "~/home/bookmarks")
  (bookmark-bmenu-file-column 45)
  )

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
	      ("s-\\" . oht-transient-dired)
	      )
  )

(use-package ibuffer
  :straight nil
  :commands ibuffer
  :config
  (setq ibuffer-show-empty-filter-groups nil
	ibuffer-saved-filter-groups
	'(("default"
	   ("Eww"   (mode . eww-mode))
	   ("Dired" (mode . dired-mode))
	   ("Org"   (mode . org-mode))
	   ("ELisp" (mode . emacs-lisp-mode))
	   ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*Info\*"))))))
  (defun oht-ibuffer-hook ()
    (hl-line-mode 1)
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default")
    )
  :hook (ibuffer-mode . oht-ibuffer-hook)
  )

(use-package flyspell
  :straight nil
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
  (advice-add 'yank :around #'ct/yank-pulse-advice)
  )

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
  (add-hook 'view-mode-hook 'hl-line-mode)
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
  (bind-key "s-j" 'view-mode)
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
	("q" . quit-window)
	)
  :blackout " VIEW"
  )


;;;; Miscellaneous Packages

(use-package benchmark-init
  ;; This package creates a report each time you startup
  ;; You'll need to add ':demand' and restart emacs to see the report
  :demand
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

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
  :defer 2
  :init
  ;; Echo unfinished commands after this delay.
  ;; Setting to 0 means do not echo commands.
  (setq echo-keystrokes 0.01)
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 1.0)
  :blackout
  )

(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo)
  )

(use-package vundo
  :straight (:type git :host github :repo "casouri/vundo" :branch "master")
  :commands vundo
  ;; The below is back-ported from Emacs 28, once you upgrade you can safely remove this:
  :config (load (concat oht-dotfiles "lisp/oht-undo-backport.el"))
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

(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))


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

(use-package selected
  :commands selected-minor-mode
  :init
  (selected-global-mode 1)
  :bind (:map selected-keymap
			  ("q" . selected-off)
			  ("u" . upcase-dwim)
			  ("d" . downcase-dwim)
			  ("w" . kill-ring-save)
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
			  ("M" . rectangle-mark-mode)
			  ("R" . replace-rectangle)
			  ("x" . exchange-point-and-mark)
			  ))


;;;; Themes

(use-package modus-themes
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
   modus-themes-variable-pitch-ui nil
   modus-themes-variable-pitch-headings nil
   )
  (modus-themes-load-operandi)
  )

(use-package tron-legacy-theme)


;;;; Languages

(use-package fountain-mode
  :commands fountain-mode
  :custom
  (fountain-add-continued-dialog nil)
  (fountain-highlight-elements (quote (section-heading)))
  )

(use-package markdown-mode
  :mode ("\\.text" . markdown-mode)
  :magic ("%text" . markdown-mode)
  :commands markdown-mode)

(use-package lua-mode
  :commands lua-mode)

(use-package bookmark-view
  :straight (:type git :host github :repo "minad/bookmark-view" :branch "master")
  :commands (bookmark-view)
  )

;;;; Visual Regexp

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


;;;; Org

(use-package org
  :commands (org-mode oht-org-agenda-today)
  :config
  (load (concat oht-dotfiles "lisp/oht-org.el"))
  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))
  ;; :hook (org-mode . variable-pitch-mode)
  :bind
  ("s-1" . org-agenda)
  (:map org-mode-map
	("s-\\" . oht-transient-org)
	)
  )

(use-package org-agenda
  :straight nil
  :commands org-agenda
  :bind
  (:map org-agenda-mode-map
	("s-z" . org-agenda-undo)
	)
  )

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
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle))
  )

(bind-keys :map emacs-lisp-mode-map
	   ("<backtab>" . bicycle-cycle-global))


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
  :init
  (setq eww-restore-desktop nil)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%t %u")
  (setq eww-download-directory "~/Downloads/")
  (setq eww-bookmarks-directory "~/.emacs.d/eww-bookmarks/")
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio/\\|application/pdf\\)")
  (setq url-cookie-trusted-urls '()
	url-cookie-untrusted-urls '(".*"))
  (defun oht-eww-bookmark-handler (record)
    "Jump to an eww bookmarked location using EWW."
    (eww (bookmark-prop-get record 'location)))
  :config
  (load (concat oht-dotfiles "lisp/oht-eww.el"))
  :commands (eww prot-eww-browse-dwim)
  :bind
  (:map eww-mode-map
	("b" . bookmark-set)
	("M-<return>" . oht-eww-open-in-new-buffer-bury)
	("i" . eww-inhibit-images-toggle)
	)
  )

(use-package elfeed
  :commands elfeed
  :hook (elfeed-show-mode . oht-elfeed-show-fonts)
  :config
  (load (concat oht-dotfiles "lisp/oht-elfeed.el"))
  (setq elfeed-use-curl t
	elfeed-curl-max-connections 10
	elfeed-db-directory "~/.emacs.d/elfeed/"
	elfeed-enclosure-default-dir "~/Downloads/"
	elfeed-search-filter "@4-week-ago +unread"
	elfeed-sort-order 'descending
	elfeed-search-clipboard-type 'CLIPBOARD
	elfeed-show-truncate-long-urls t)
  :bind
  ("M-s-e" . elfeed)
  (:map elfeed-search-mode-map
	("a" . hrs/elfeed-pinboard-current-entry)
	("b" . elfeed-search-browse-url)
	("B" . oht-elfeed-search-browse-and-bury)
	("*" . elfeed-search-tag--star)
	("8" . elfeed-search-untag--star)
	)
  (:map elfeed-show-mode-map
	("a" . hrs/elfeed-pinboard-current-entry)
	("&" . bjm/elfeed-show-visit-gui)
	("r" . elfeed-show-tag--read)
	("u" . elfeed-show-tag--unread)
	("*" . elfeed-show-tag--star)
	("8" . elfeed-show-tag--unstar)
	("d" . oht-elfeed-show-download-video)
	("i" . elfeed-inhibit-images-toggle)
	("B" . oht-elfeed-show-browse-and-bury)
	)
  )

(use-package hackernews
  :commands hackernews
  :custom
  (hackernews-items-per-page 30)
  (hackernews-default-feed 'top)
  )


;;;; Mail

(setq mail-user-agent 'mu4e-user-agent)

(use-package message
  :straight nil
  :config
  (setq message-send-mail-function 'smtpmail-send-it
	message-cite-style 'message-cite-style-thunderbird
	message-cite-function 'message-cite-original
	message-kill-buffer-on-exit t
	message-citation-line-format "On %d %b %Y at %R, %f wrote:\n"
	message-citation-line-function 'message-insert-formatted-citation-line))

(use-package mu4e
  :load-path "/usr/local/Cellar/mu/1.4.15/share/emacs/site-lisp/mu/mu4e"
  :commands mu4e
  :bind (:map mu4e-headers-mode-map
	      ("G" . mu4e-update-mail-and-index))
  :config
  (load "~/home/ingenuity/mu4e.el")
  (setq mu4e-attachments-dir "~/Downloads"
	mu4e-update-interval (* 5 60)
	mu4e-change-filenames-when-moving t
	mu4e-completing-read-function 'completing-read
	mu4e-compose-dont-reply-to-self t
	mu4e-compose-format-flowed nil
	mu4e-confirm-quit nil
	mu4e-headers-date-format "%Y-%m-%d"
	mu4e-headers-include-related nil
	mu4e-headers-skip-duplicates t
	mu4e-headers-time-format "%H:%M"
	mu4e-headers-visible-lines 20
	mu4e-use-fancy-chars nil
	mu4e-view-show-addresses t
	mu4e-view-show-images t
	mu4e-sent-messages-behavior 'delete)
  (setq mu4e-headers-fields
	'((:human-date . 12)
	  (:flags . 6)
	  (:from . 22)
	  (:thread-subject)))
  ;; Hooks and settings
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
	      (larger-variable-pitch-mode 1)
	      ))
  )

(use-package smtpmail
  :straight nil
  :init
  (setq auth-sources '("~/home/ingenuity/authinfo"))
  (setq	smtpmail-stream-type 'starttls
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587))


;;;; Youtube-dl

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
  :config
  ;; pdf-tools uses the `pdf-tools-install' function to set up hooks and
  ;; various things to ensure everything works as it should. PDFs will open
  ;; just fine without this, but not all features will be available.
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations nil)
  ;; Required for retina scaling to work
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (defun oht-pdf-view-fonts ()
    (hl-line-mode 1)
    (pdf-annot-list-follow-minor-mode))
  :hook (pdf-annot-list-mode-hook . oht-pdf-view-fonts)
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward)
	      ("C-r" . isearch-backward)
	      ("A" .   pdf-annot-add-highlight-markup-annotation)
	      ("L" .   pdf-annot-list-annotations)
	      ("O" .   pdf-occur)
	      ("G" .   pdf-view-goto-page)
	      ("<" .   pdf-view-first-page)
	      (">" .   pdf-view-last-page))
	      )

(use-package pdf-tools-org
  :straight (:host github :repo "machc/pdf-tools-org" :branch "master")
  :commands pdf-tools-org-export-to-org
  ;; This package seems pretty out of date, modifying the code as suggested here:
  ;; https://github.com/machc/pdf-tools-org/issues/7
  ;; seems to fix export
  )

;;;; Transient

(use-package transient
  ;; comes installed with Magit, no need to install
  :straight nil
  :config
  (load (concat oht-dotfiles "lisp/oht-transient.el"))
  :bind
  ("s-H" . oht-transient-help)
  ("s-<return>" . oht-transient-general)
  ("s-w" . oht-transient-window)
  ("s-F" . oht-transient-fonts)
  ("s-2" . oht-transient-2nd)
  ("s-0" . oht-transient-outline)
  ("s-d" . oht-transient-dispatch)
  )


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

(use-package oht-fonts
  :straight nil
  :demand
  :init
  (setq-default line-spacing nil)
  (setq text-scale-mode-step 1.09)
  (setq oht-fonts-monospace "IBM Plex Mono")
  (setq oht-fonts-variable  "IBM Plex Sans")
  (setq oht-fonts-monospace-size 12)
  (setq oht-fonts-variable-size  14)
  (set-face-attribute 'mode-line nil          :family "IBM Plex Sans" :height 130)
  (set-face-attribute 'mode-line-inactive nil :family "IBM Plex Sans" :height 130)
  )

(use-package oht-dispatch
  :straight nil
  :demand
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

(use-package oht-transient-mark
  :straight nil
  :bind
  ("C-x C-x" . exchange-point-and-mark-dwim)
  ("C-`" . push-mark-no-activate)
  ("M-`" . consult-mark)
  )

(use-package oht-find-file-directories
  ;; Beautiful set of functions from Radian for creating directories when
  ;; finding files.
  :straight nil
  :demand
  )


;;; Mouse

;; Gasp! An Emacs user that actually uses the mouse?! Scandalous.
;; Consider the possibility that using the mouse for some things is AWESOME.
;; Also, it pays to read the source code!

;; Start by making shift-click extend the selection (region)
(global-set-key [S-down-mouse-1] 'ignore)
(global-set-key [S-mouse-1] 'mouse-save-then-kill)

;; Using the mouse for horizontal/vertical splits is great because
;; the windows split exactly where you click the mouse.
;; See 'Rebinding Mouse Buttons' for more info.
;;
;; 1. Use s-click to create splits
;; 2. Use M-s-click to delete windows

;; To add a vertical split to a window, just s-click on the mode-line
(global-set-key [mode-line s-mouse-1] 'mouse-split-window-horizontally)
;; and make this work everywhere else
(global-set-key [s-mouse-1] 'mouse-split-window-horizontally)
;; Use shift to add a horizontal split to the window
(global-set-key [S-s-mouse-1] 'mouse-split-window-vertically)

;; Delete a window with M-s--click on its mode-line
(global-set-key [mode-line M-s-mouse-1] 'mouse-delete-window)
;; And make this work everywhere
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


;;; Keybindings

;; Make it so every time you type RET you also indent the next line.
;; (define-key global-map (kbd "RET") 'newline-and-indent)

(bind-keys* ("C-<return>" . execute-extended-command)
	    ("C-;" . backward-word)
	    ("C-'" . forward-word))


(bind-keys ("M-s-s" . save-some-buffers)
	   ("M-c" . capitalize-dwim)
	   ("M-l" . downcase-dwim)
	   ("M-u" . upcase-dwim)
	   ("M-SPC" . cycle-spacing)
	   ("M-/" . hippie-expand)
	   ("C-x r r" . replace-rectangle)
	   ("C-h C-f" . find-function)
	   ("C-h C-v" . find-variable)
	   ("M-DEL" . sanemacs/backward-kill-word)
	   ("C-DEL" . sanemacs/backward-kill-word)
	   )

(bind-keys ("s-k" . kill-this-buffer)
	   ("s-B" . ibuffer)
	   ("M-s-b" . list-bookmarks)
	   ("s-C" . org-capture)
	   ("s-|" . oht/pipe-region)
	   ("s-/" . oht-toggle-comment-region-or-line)
	   ("s-l" . oht-mac-mark-whole-line)
	   ("s-o" . other-window))

;;; Mac Shortcuts

;; Most of the time I want to use standard macOS shortcuts[1]. macOS actually
;; inherits many Emacs keybindings, but adds to it a few from =readline= and
;; old terminal interfaces. Because these are available system-wide I want
;; Emacs to do the same thing. That way the way I type/move in Mail.app or
;; Safari is the same as Emacs. Some of these require custom functions, but
;; that's usually a simple matter of stringing a couple existing commands
;; together into a function.
;;
;; 1: https://support.apple.com/en-us/HT201236

;; Turning on `visual-line-mode' binds "C-a" to `beginning-of-visual-line'.
;; This is inconsistent with macOS behavior, which is that "C-a" always goes
;; to the beginning of the logical line and "s-<left>" goes to the beginning
;; of the visual line. So these bindings correct that.
(bind-keys* ("C-a" . beginning-of-line)
	    ("C-e" . end-of-line))
(bind-keys ("s-<left>" . beginning-of-visual-line)
	   ("s-<right>" . end-of-visual-line)
	   ;; C-k only killing the visual line also isn't how macOS works.
	   ;; This has to be set to a custom function so minor modes can't
	   ;; hijack it.
	   ("C-k" . oht-mac-kill-line))

;; And now remap `beginning-of-line' to a custom fuction
(global-set-key [remap beginning-of-line] #'my/smart-beginning-of-line)

(bind-keys
 ("s-," . oht-mac-find-settings)
 ("s-n" . make-frame-command)
 ("s-N" . make-frame-command)
 ("s-t" . oht-mac-new-tab)
 ("s-m" . iconify-frame)
 ("s-s" . save-buffer)
 ("s-S" . write-file) ;save as
 ("s-a" . mark-whole-buffer)
 ;; ("s-o" . find-file)
 ("s-x" . kill-region)
 ("s-c" . kill-ring-save)
 ("s-v" . yank)
 ("s-<backspace>" . oht-mac-kill-visual-line-backward)
 ;; ("s-w" . delete-frame)
 ("s-q" . save-buffers-kill-terminal)
 ("S-s-<left>" . oht-mac-expand-to-beginning-of-visual-line)
 ("S-s-<right>" . oht-mac-expand-to-end-of-visual-line)
 ("s-<up>" . beginning-of-buffer)
 ("s-<down>" . end-of-buffer)
 ;; navigation and indentation
 ("s-[" . previous-buffer)
 ("s-]" . next-buffer)
 ("s-}" . indent-rigidly-right-to-tab-stop)
 ("s-{" . indent-rigidly-left-to-tab-stop)
 ;; readline-style shortcuts, because I love them
 ("C-w" . backward-kill-word)
 ("C-u" . oht-mac-kill-line-backward)
 ;; No reason not to use command-u for this instead
 ("s-u" . universal-argument)
 )
