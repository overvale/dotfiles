;;; minimum.el --- Oliver Minimum Config -*- lexical-binding: t -*-

;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; There are many "Emacs starter kits" out there, this one is mine. It is
;; designed to be exceedingly simple. Just copy this one file to
;; ~/.emacs/init.el and open Emacs. Everything should get installed and
;; configured automatically.
;;
;; This is really meant for someone who is comfortable configuring their
;; dotfiles and wants to get started with Emacs in a way that is familiar to
;; that experience. Simply put: if I could travel back in time to the
;; beginning of my Emacs journey, I would give myself this file and say "start
;; with this".
;;
;; A note on package installation and use:
;;
;; - `straight' is used to install/update packages.
;; - `use-package' is used to precisely control the loading of packages and
;;    configure them.


;;; Straight - package installation

;; Install the Straight package if it isn't installed
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


;;; Use Package - package loading and configuration

;; Install and load `use-package'
(straight-use-package 'use-package)

;; Tell Straight to use `use-package' configs for installation of packages.
(setq straight-use-package-by-default t)

;; Don't load any packages unless explicitly told to do so in the config, see
;; `use-package' documentation for more info.
(setq use-package-always-defer t)


;;; Packages

(use-package which-key
  :demand t
  :config
  (which-key-mode t)
  )

(use-package bind-key
  :demand t
  )

(use-package whole-line-or-region
  :demand t
  :config
  (whole-line-or-region-global-mode 1)
  )

(use-package icomplete-vertical
  :demand t
  :custom
  (completion-styles '(partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)))

(use-package orderless
  :demand t
  :custom (completion-styles '(orderless)))


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
