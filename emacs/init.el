;;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Set Up

;;;; Startup

;; Garbage Collection!
;; This is actually the 2nd step, for the first step, see `early-init.el'
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)
    (garbage-collect)) t)

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
	  initial-buffer-choice 'remember-notes)


;;;; Preferences

;; Activate modes to set settings -- sure Emacs!
(delete-selection-mode -1)
(global-auto-revert-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(winner-mode 1)
(show-paren-mode t)
(blink-cursor-mode -1)
(set-language-environment "UTF-8")

;; Needs to be set in BOTH early-init AND init ¯\_(ツ)_/¯
(scroll-bar-mode -1)

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
(setq load-prefer-newer t)

;; When splitting the window, go to it
(defadvice split-window-below (after split-window-below activate) (other-window 1))
(defadvice split-window-right (after split-window-right activate) (other-window 1))


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

;; Quickly edit your emacs config by binding this
(defun oht-find-emacs-dotfiles ()
  "Finds lisp files in dotfiles directory and passes to completing-read."
  (interactive)
  (find-file (completing-read "Find Elisp Dotfile: "
							  (directory-files-recursively oht-dotfiles "\.el$"))))


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

;; When visual-line-mode is off and truncate-lines is toggled off, I still
;; want wrapping to happen at the word instead of character.
(setq-default word-wrap 1)


;; Turning on `visual-line-mode' binds "C-a" to `beginning-of-visual-line'.
;; This is inconsistent with macOS behavior, which is that "C-a" always goes
;; to the beginning of the logical line and "s-<left>" goes to the beginning
;; of the visual line. Also note the bindings in `oht-keys-mode'.
(global-set-key (kbd "s-<left>") 'beginning-of-visual-line)
(global-set-key (kbd "s-<right>") 'end-of-visual-line)

;; and while we're at it...
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)


;;;; Mode Line

(setq display-time-format "%H:%M  %Y-%m-%d")
(setq display-time-interval 60)
(setq display-time-mail-directory nil)
(setq display-time-default-load-average nil)

(column-number-mode t)
(display-time-mode t)


;;;; Modifiers & Emacs Anachronisms

;; Make the command keys 'super'. Super is basically not used by Emacs so
;; they're a safe playground for assigning your own bindings.
(setq mac-command-modifier 'super)
;; Meta is used a lot in Emacs, and I only ever use the left option key, so
;; this works well for shortcuts.
(setq mac-option-modifier 'meta)
;; But sometimes you want to insert special characters like £¢∞§¶•≠
(setq mac-right-option-modifier 'nil)

;; In the old days ESC was used as a prefix key, but I want ESC to act like it
;; does everywhere else on my system and, you know, escape from things. So
;; I've remapped ESC to `keyboard-quit'.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))


;;;; Basic Keybindings

;; This is done first so that if later parts of the config fail I still have
;; these familiar bindings to work with. They are all built-in functions.

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
(global-set-key (kbd "s-,") 'oht-find-emacs-dotfiles)

;; Emacs SUPER!
(global-set-key (kbd "s-k")   'kill-this-buffer)
(global-set-key (kbd "s-b")   'switch-to-buffer)
(global-set-key (kbd "s-B")   'ibuffer)
(global-set-key (kbd "s-M-b") 'list-bookmarks)
(global-set-key (kbd "s-o")   'other-window)
(global-set-key (kbd "s-O")   'find-file)
(global-set-key (kbd "s-u")   'universal-argument)

;; Emacs Misc
(global-set-key (kbd "M-s-s")   'save-some-buffers)
(global-set-key (kbd "M-c")     'capitalize-dwim)
(global-set-key (kbd "M-l")     'downcase-dwim)
(global-set-key (kbd "M-u")     'upcase-dwim)
(global-set-key (kbd "M-SPC")   'cycle-spacing)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x r r") 'replace-rectangle)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)


;;;; Minor Mode for Personal Keybindings

;; Minor modes can override global bindings (see README), so any bindings you
;; don't want overridden should be placed in their own minor mode.

;; Define the keymap, mode, and switch it on
(defvar oht-keys-mode-keymap (make-keymap) "Keymap for oht-keys-mode")
(define-minor-mode oht-keys-mode
  "Minor mode for my personal keybindings."
  :global t
  :keymap oht-keys-mode-keymap)
(oht-keys-mode 1)

;; My bindings
(define-key oht-keys-mode-keymap (kbd "<C-return>") 'execute-extended-command)
(define-key oht-keys-mode-keymap (kbd "C-;") 'backward-word)
(define-key oht-keys-mode-keymap (kbd "C-'") 'forward-word)
(define-key oht-keys-mode-keymap (kbd "C-a") 'beginning-of-line)
(define-key oht-keys-mode-keymap (kbd "C-e") 'end-of-line)


;;;; Mouse

;; Gasp! An Emacs user that actually uses the mouse?! Scandalous.

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
;; it consistent, use-package or not the hooks are named the same.
(setq use-package-hook-name-suffix nil)

;;; Packages

(use-package benchmark-init
  :demand
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package blackout
  :demand
  :config
  (blackout 'eldoc-mode)
  (blackout 'emacs-lisp-mode "Elisp")
  (blackout 'auto-fill-function " Fill"))

(use-package modus-themes
  :init (setq modus-themes-bold-constructs nil
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
              modus-themes-variable-pitch-headings nil)
  (modus-themes-load-operandi))

(add-hook 'mac-effective-appearance-change-hook 'modus-themes-toggle)

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
;; 1. Use Selectrum as the main interface for completion narrowing.
;; 2. Use Prescient to sort those completions and provide fuzzy matching.
;; 3. Use Marginalia to decorate the completions, and provide into to Embark.
;; 4. Use Embark for lots of stuff.
;; 5. When Selectrum is disabled (and thus also prescient) fall back on Orderless.
;; 6. Use Consult and Consult-Selectrum to enable new commands.

(use-package orderless
  :straight (:host github :repo "oantolin/orderless" :branch "master")
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum" :branch "master")
  :init (selectrum-mode +1)
  :config
  ;; (setq selectrum-display-action '(display-buffer-in-side-window
  ;; 								   (side . bottom)
  ;; 								   (slot . -1)
  ;; 								   ))
  )

(use-package selectrum-prescient
  :after selectrum
  :init
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

(use-package marginalia
  :straight (:type git :host github :repo "minad/marginalia" :branch "main")
  :init
  (marginalia-mode 1)
  (setq marginalia-annotators
	'(marginalia-annotators-heavy marginalia-annotators-light))
  ;; if using Selectrum
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

(use-package embark
  :straight (:host github :repo "oantolin/embark" :branch "master")
  :commands use-embark-completions
  :bind
  ("s-e" . embark-act)
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
  :config
  (setq consult-preview-key (kbd "C-="))
  (setq consult-config
	`((consult-mark :preview-key any))))

(use-package embark-consult
  :straight nil
  :after (embark consult))

(use-package ctrlf
  :bind
  ("C-s" . ctrlf-forward-fuzzy)
  ("C-r" . ctrlf-backward-fuzzy)
  :config
  (setq ctrlf-go-to-end-of-match nil))


;;;; Built-In Packages

(use-package remember
  :straight nil
  :init
  (setq remember-data-file "~/home/org/remember-notes")
  (setq remember-notes-initial-major-mode 'fundamental-mode)
  (setq remember-notes-auto-save-visited-file-name t)
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
  :init
  (add-hook 'bookmark-bmenu-mode (lambda ()
                                  (hl-line-mode 1)))
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file "~/home/bookmarks")
  (setq bookmark-bmenu-file-column 45)
  :bind ("M-s-b" . list-bookmarks))

(use-package ibuffer
  :straight nil
  :commands ibuffer
  :config
  (setq ibuffer-show-empty-filter-groups nil
	ibuffer-saved-filter-groups
	'(("default"
	   ("Read"  (or (mode . eww-mode)
					(mode . elfeed-search-mode)
					(mode . elfeed-show-mode)))
	   ("Org"   (mode . org-mode))
	   ("Mail"  (or
                 (mode . mu4e-view-mode)
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


;;;; View / Selected

(use-package view
  :straight nil
  :init
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
  :bind (:map view-mode-map
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
  :blackout " VIEW")

(use-package selected
  :commands selected-minor-mode
  :init
  (selected-global-mode 1)
  :blackout selected-minor-mode
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
			  ("R" . replace-rectangle)
			  ("E" . eval-region)
			  ("q" . selected-off))
  :config
  (add-hook 'elfeed-show-mode (lambda ()
								(selected-minor-mode -1))))


;;;; Dired

(use-package dired
  :straight nil
  :commands (dired dired-jump dired-jump-other-window)
  :init
  (setq dired-use-ls-dired nil) ; no more warning message
  :bind (:map dired-mode-map
		 ("s-\\" . oht-transient-dired))
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

(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))


;;;; Flyspell

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

(use-package which-key
  :disabled
  :init
  ;; Echo unfinished commands after this delay.
  ;; Setting to 0 means do not echo commands.
  (setq echo-keystrokes 0.01)
  :config
  ;; (which-key-mode t)
  (setq which-key-idle-delay 1.0)
  :blackout)

(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo))

(use-package vundo
  :straight (:type git :host github :repo "casouri/vundo" :branch "master")
  :commands vundo
  ;; The below is back-ported from Emacs 28, once you upgrade you can safely remove this:
  :config (load (concat oht-dotfiles "lisp/oht-undo-backport.el")))

(use-package expand-region
  :bind
  ("s-r" . er/expand-region)
  ("s-R" . er/contract-region))

(use-package zzz-to-char
  ;; replaces zap-to-char with an avy-like interface
  ;; note that it searches forward and backward
  :bind ("M-z" . zzz-up-to-char))

(use-package sdcv-mode
  :straight (sdcv-mode :type git :host github :repo "gucong/emacs-sdcv" :branch "master")
  :commands sdcv-search)

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
  ("C-h k" . #'helpful-key)
  ("C-h p" . #'helpful-at-point))

(use-package pinboard
  :commands (pinboard pinboard-add pinboard-add-for-later)
  :init
  (setf epa-pinentry-mode 'loopback))

(use-package move-text
  :bind
  ("M-<up>" . move-text-up)
  ("M-<down>" . move-text-down))

(use-package buffer-move
  :commands (buf-move-up
			 buf-move-down
			 buf-move-left
			 buf-move-right))

(use-package bookmark-view
  :straight (:type git :host github :repo "minad/bookmark-view" :branch "master")
  :commands (bookmark-view))

(use-package visual-regexp
  ;; Provides an alternate version of `query-replace' which highlights matches
  ;; and replacements as you type.
  :bind (([remap query-replace] . #'vr/query-replace)))

(use-package visual-regexp-steroids
  ;; Allows `visual-regexp' to use regexp engines other than Emacs'; for
  ;; example, Python or Perl regexps.
  :after visual-regexp
  :bind (([remap query-replace-regexp] . #'vr/query-replace))
  :init
  (setq vr/engine 'pcre2el))


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
  (load (concat oht-dotfiles "lisp/oht-org.el"))
  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))
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
		("s-\\" . oht-transient-eww)))

(use-package elfeed
  :commands elfeed
  :hook (elfeed-show-mode-hook . oht-elfeed-show-fonts)
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
	      )))

(use-package smtpmail
  :straight nil
  :init
  (setq auth-sources '("~/home/ingenuity/authinfo"))
  (setq	smtpmail-stream-type 'starttls
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587))


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


;;;; Transient

(use-package transient
  ;; comes installed with Magit, no need to install
  :straight nil
  ;; Anything not in a binding below needs to be called-out as a command
  :commands (oht-transient-org
			 oht-transient-dispatch
			 oht-transient-org-agenda
			 oht-transient-eww)
  :init
  (autoload 'org-store-link "org")
  :config
  (load (concat oht-dotfiles "lisp/oht-transient.el"))
  :bind*
  ("s-<return>" . oht-transient-general)
  ("C-h H" . oht-transient-help)
  ("s-w" . oht-transient-window)
  ("s-F" . oht-transient-fonts)
  ("s-2" . oht-transient-2nd)
  ("s-0" . oht-transient-outline)
  ("s-d" . oht-transient-dispatch)
  ("C-," . oht-transient-spelling))


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
  :config (oht-fonts-set))


(use-package oht-dispatch
  :straight nil
  :demand)

(use-package oht-functions
  :straight nil
  :demand
  :config
  (global-set-key [remap beginning-of-line] #'my/smart-beginning-of-line)
  :bind
  ("M-DEL" . sanemacs/backward-kill-word)
  ("C-DEL" . sanemacs/backward-kill-word)
  ("s-/" . oht-toggle-comment-region-or-line)
  ("s-|" . oht/pipe-region))

(use-package oht-mac
  :straight nil
  :demand
  :bind
  ("s-l" . oht-mac-mark-whole-line)
  ;; C-k only killing the visual line also isn't how macOS works.
  ("C-k" . oht-mac-kill-line))

(use-package oht-composition
  :straight nil
  :commands (composition-mode))

(use-package oht-transient-mark
  :straight nil
  :bind
  ("C-x C-x" . exchange-point-and-mark-dwim)
  ("C-`" . push-mark-no-activate)
  ("M-`" . consult-mark))

(use-package oht-find-file-directories
  ;; Beautiful set of functions from Radian for creating directories when
  ;; finding files.
  :straight nil
  :demand)


;;; End of init.el
