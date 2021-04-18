;;; minimum.el --- Oliver Minimum Config -*- lexical-binding: t -*-

;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; There are many "Emacs starter kits" out there, this one is mine. It is
;; designed to be exceedingly simple. Just copy this one file to
;; ~/.emacs/init.el and open Emacs. Everything should get installed and
;; configured automatically.

;; This is really meant for someone who is comfortable configuring their
;; dotfiles and wants to get started with Emacs in a way that is familiar to
;; that experience. Simply put: if I could travel back in time to the
;; beginning of my Emacs journey, I would give myself this file and say "start
;; with this".


;;; Goals and Philosophy

;; 1. Don't bother learning the Emacs key bindings you don't want to. Once you
;;    learn how to configure Emacs you can create whatever bindings make
;;    sense to you.
;; 2. I'm a Mac user, so this config targets Mac users and sets some of the
;;    most common shortcuts, as well as making the modifier keys behave in a
;;    slightly more predictable way.
;; 3. I use, and recommend, the combination of `straight' and
;;    `use-package' to manage the installation and configuration of packages.
;;    Using these 2 tools makes the package declarations in your init file the
;;    single source of what should be loaded and used by Emacs. Which is a
;;    better first-run experience than the default (in my opinion).
;; 4. I highly recommend, for first-time users, the combination of the
;;    packages `selectrum', `selectrum-prescient', and `marginalia'. These
;;    tools make Emacs easier to explore and discover capabilities. You may
;;    eventually decide they're not for you, but I think they're a great place
;;    to start.
;; 5. Provides some convenience bindings for my most used Emacs features.

;; ---------------------------------------------------------------------------


;;; Settings

;; Discovering the exact behavior of these settings is left as an exercise for
;; the reader. Documentation can be found with "C-h o <symbol>".

;; Keep in mind that there are two kinds of variables, global ones, and
;; buffer-local ones.
;;
;; 'setq' simply sets the value of a variable, so if the variable is global it
;; sets its value globally, and if the variable is buffer local it sets the
;; value locally.
;;
;; 'setq-local' takes a global variable and makes a buffer local "copy" that
;; doesn't effect the global value.
;;
;; 'setq-default' takes a local variable and sets a new default value for all new
;; buffers, but doesn't change it in existing buffers or the default.

(delete-selection-mode t)
(global-visual-line-mode t)
(setq-default truncate-lines t)
(global-auto-revert-mode t)
(set-language-environment "UTF-8")
(setq uniquify-buffer-name-style 'forward)
(setq save-interprogram-paste-before-kill t)


;;; Keyboard modifiers setup

;; The below is designed for Mac users.

;; This makes your command key the 'super' key, which you can bind with "s-a",
;; keep in mind that shift is "S-a".
(setq mac-command-modifier 'super)

;; This makes the left option META and right one OPTION.
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)


;;; Keybindings

;; To decide what to do, Emacs looks at keymaps in this order: (1) Minor Mode,
;; (2) Major Mode, (3) Global. So if you find yourself with a binding that
;; just doesn't work, it is likely because of an active minor mode binding.

;; <Control>-modified key bindings are case-insensitive. So 'C-A' is identical to
;; 'C-a'. You can bind shifted <Control> keystrokes with 'C-S-a'. All other
;; modifiers are case-sensitive.

;; In the old days ESC was used as a prefix key, but I want ESC to act like it
;; does everywhere else on my system and, you know, escape from things. So
;; I've remapped ESC to `keyboard-quit'.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; I recommend leaving all of these bindings here and not relying on external
;; pacakges, even if you rebind them later in the config, that way if something
;; gets messed up in your package declarations all of these bindings still work.

;; Mac-like bindings
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-m") 'iconify-frame)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-n") 'make-frame-command)

;; Emacs SUPER!
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "s-B") 'ibuffer)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-O") 'find-file)
(global-set-key (kbd "s-u") 'universal-argument)

;; Emacs Misc
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)

;; Make shift-click extend the selection (region)
(global-set-key [S-down-mouse-1] 'ignore)
(global-set-key [S-mouse-1] 'mouse-save-then-kill)

;; Use M-drag-mouse-1 to create rectangle regions
(global-set-key [M-down-mouse-1] #'mouse-drag-region-rectangle)
(global-set-key [M-drag-mouse-1] #'ignore)
(global-set-key [M-mouse-1]      #'mouse-set-point)

;; My approach to commonly used shortcuts is to place them all behind a
;; single prefix.
(global-set-key (kbd "s-<return> x") 'execute-extended-command)
(global-set-key (kbd "s-<return> w") 'visual-line-mode)
(global-set-key (kbd "s-<return> h") 'hl-line-mode)
(global-set-key (kbd "s-<return> l") 'display-line-numbers-mode)
(global-set-key (kbd "s-<return> a") 'auto-fill-mode)

;; These let you manage windows (splits)
(global-set-key (kbd "s-w s") 'split-window-below)
(global-set-key (kbd "s-w v") 'split-window-right)
(global-set-key (kbd "s-w k") 'delete-window)
(global-set-key (kbd "s-w o") 'delete-other-windows)
(global-set-key (kbd "s-w b") 'balance-windows)


;;; Package Management

;; - `straight' is used to install/update packages.
;; - `use-package' is used to precisely control the loading of packages and
;;    configure them.

;; Install the `straight' package if it isn't installed
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

;; Install and load `use-package'
(straight-use-package 'use-package)

;; Tell Straight to use `use-package' config declarations for installation of packages.
(setq straight-use-package-by-default t)

;; Don't load any packages unless explicitly told to do so in the config, see
;; `use-package' documentation for more info. I HIGHLY recommend reading up on this.
;; Until you really understand how use-package loads packages you might drive yourself
;; crazy trying to get a package to load properly.
(setq use-package-always-defer t)


;;; Packages

;; Packages come last in the config because, in my experience, they are the
;; most common source of breakage, so if something goes wrong below all of the
;; above settings are still loaded.

(use-package undo-fu
  ;; Undo in Emacs is confusing, for example there's no redo command and you can
  ;; undo an undo. That's fine if you're an Emacs wizard, but this package
  ;; simplifies things so Emacs behaves like you might expect.
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo))

(use-package which-key
  ;; Displays useful pop-ups for when you type an incomplete binding.
  :init
  (which-key-mode 1))

(use-package whole-line-or-region
  ;; the region isn't always 'active' (visible), in those cases, if you call a
  ;; command that acts on a region you'll be acting on an invisible region.
  ;; This package makes it so that only the active region is acted upon, and
  ;; the fallback in the current line, instead of an invisible region.
  :init
  (whole-line-or-region-global-mode 1))

(use-package orderless
  :demand
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  ;; Display useful information about the selection candidates.
  :init
  (marginalia-mode 1)
  (setq marginalia-annotators
	'(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package modus-themes
  ;; My preferred theme.
  :init
  (modus-themes-load-operandi))


;;; End of Config --- Good luck out there!
