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

;; 1. Don't bother learning the Emacs shortcuts you don't want to. Once you
;;    learn how to configure Emacs you can make up whatever shortcuts makes
;;    sense to you.
;; 2. I'm a Mac user, so this config targets Mac users and sets some of the
;;    most common shortcuts, as well as making the modifier keys behave in a
;;    slightly more predictable way
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


;;; Package Management

;; A note on package installation and use:
;;
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
;; `use-package' documentation for more info.
(setq use-package-always-defer t)


;;; Packages

(use-package which-key
  :init
  (which-key-mode t)
  )

(use-package bind-key
  :demand t
  )

(use-package whole-line-or-region
  :init
  (whole-line-or-region-global-mode 1)
  )

(use-package selectrum
  :init
  (selectrum-mode +1)
  )

(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  )

(use-package marginalia
  :init
  (marginalia-mode 1)
  (setq marginalia-annotators
	'(marginalia-annotators-heavy marginalia-annotators-light))
  )

(use-package modus-themes
  :init
  ;; includes a dark variant
  (load-theme 'modus-operandi t)
  )


;;; Keyboard modifiers setup

;; The below is designed for Mac users. You may need to tweak.

;; This makes your command key the 'super' key, which you can bind with "s-a",
;; keep in mind that shift is "S-a".
(setq mac-command-modifier 'super)

;; This makes the left option META and right one OPTION.
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)


;;; Settings

;; Discovering the exact behavior of these settings is left as an exercise for
;; the reader. Documentation can be found with "C-h o <symbol>".
(delete-selection-mode t)
(global-visual-line-mode t)
(setq-default truncate-lines t)
(global-auto-revert-mode t)


;;; Keybindings

;; Make Esc do the right thing
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; These are to make Emacs more predicable in MacOS
(bind-keys
 ("s-n" . make-frame-command)
 ("s-m" . iconify-frame)
 ("s-s" . save-buffer)
 ("s-o" . find-file)
 ("s-w" . delete-frame)
 ("s-q" . save-buffers-kill-terminal)
 ("s-a" . mark-whole-buffer)
 ("s-z" . undo-only) ;; Why no redo? Read up on it.
 ("s-x" . kill-region)
 ("s-c" . kill-ring-save)
 ("s-v" . yank)
 ("s-<up>" . beginning-of-buffer)
 ("s-<down>" . end-of-buffer)
 ("s-<left>" . beginning-of-visual-line)
 ("s-<right>" . end-of-visual-line)
 )

;; Buffer management / navigation
(bind-keys
 ("s-b" . switch-to-buffer)
 ("s-B" . ibuffer)
 ("s-[" . previous-buffer)
 ("s-]" . next-buffer)
 ("s-k" . kill-this-buffer)
 )

;; These are handy display toggles
(bind-keys :prefix-map my/global-leader
	   :prefix "s-'"
	   ("w" . visual-line-mode)
	   ("h" . hl-line-mode)
	   ("l" . display-line-numbers-mode)
	   ("a" . auto-fill-mode)
	   )

;; These let you manage windows (splits)
(bind-keys :prefix-map my/windows-leader
	   :prefix "s-="
	   ("s" . split-window-below)
	   ("v" . split-window-right)
	   ("k" . delete-window)
	   ("o" . delete-other-windows)
	   ("b" . balance-windows)
	   )

;;; End of Config --- Good luck out there!
