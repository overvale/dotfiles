;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Copyright © Oliver Taylor
;; Learning Emacs was my COVID pandemic project, now I cannot escape.

;; Author: Oliver Taylor
;; URL: https://olivertaylor.net
;; URL: https://github.com/olivertaylor/dotfiles

;; This config targets Emacs 29.1

;;; Commentary:

;; Every Emacs configuration is unique to the person who created it, to their
;; needs and their taste. This one takes the following approach:
;;
;;   + I prefer to write my own code instead of installing packages, because
;;     I love pain and hate sleep.
;;   + Symbol names are not prefixed (for the most part) so take care to
;;     to avoid collisions if you copy something into your own config. This
;;     is probably not one of my good ideas.

;; If you like this config I would recommend reading these:
;; https://github.com/raxod502/radian
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; https://github.com/casouri/lunarymacs
;; https://github.com/oantolin/emacs-config

;;; Code:

;;;; Basic Setup, Personal Lisp

(add-to-list 'load-path (concat user-home-dir "dot/emacs/lisp/"))

(require 'overvale-setup)
(require 'overvale-functions)
(require 'overvale-bindings)
(require 'overvale-theme)
(require 'overvale-minibuffer)
(require 'overvale-modeline)
(require 'overvale-tools)

;;;; External Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-archive-priorities '(("gnu" . 30)
                                   ("nongnu" . 25)
                                   ("melpa-stable" . 20)
                                   ("melpa" . 10)))

;; Keep in mind that all of these packages are loaded at startup, even if you
;; do not configure them.
(setq package-selected-packages
      '(consult
        delight
        embark
        embark-consult
        exec-path-from-shell
        lua-mode
        magit
        markdown-mode
        marginalia
        modus-themes
        move-text
        no-littering
        olivetti
        orderless
        orgalist
        vertico
        visual-regexp
        visual-regexp-steroids
        vundo
        wc-mode))

(use-package exec-path-from-shell
  :demand
  :init
  (exec-path-from-shell-initialize))

(use-package no-littering
  :demand
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package orderless
  :custom
  (completion-styles '(substring orderless basic)))

(use-package vertico
  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :custom
  (vertico-multiform-commands
   '((consult-grep buffer)
     (grep-logbook-personal buffer)
     (execute-extended-command unobtrusive)
     (switch-to-buffer unobtrusive)))
  :bind
  (:map vertico-map
        ("M-v" . vertico-multiform-vertical)
        ("M-r" . vertico-multiform-reverse)
        ("M-u" . vertico-multiform-unobtrusive)
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult
  :custom
  (consult-find-command "fd --color=never --full-path ARG OPTS")
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
                #'consult-completion-in-region
              #'completion--in-region)
            args)))
  :config
  (consult-customize consult-line
                     consult-outline
                     consult-imenu
                     consult-mark
                     :preview-key 'any
                     consult-buffer
                     consult-buffer-other-window
                     :preview-key nil)
  :bind (([remap imenu] . consult-imesnu)
         ([remap yank-pop] . consult-yank-pop)
         ([remap repeat-complex-command] . consult-complex-command)))

(use-package visual-regexp
  :bind ([remap query-replace] . vr/query-replace)
  :config
  (with-eval-after-load 'visual-regexp-steroids
    (custom-set-variables
     '(vr/engine 'pcre2el))))

(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package embark
  :bind ("C-." . embark-act))

(use-package delight
  :init
  (with-eval-after-load 'flyspell (delight 'flyspell-mode " Spell" "flyspell"))
  (delight 'outline-minor-mode " Out" "outline")
  (delight 'auto-fill-function " Fill" "simple"))

(use-package markdown-mode
  :init
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)))

(use-package olivetti
  :custom
  (olivetti-body-width 82))

(use-package move-text
  :init
  (move-text-default-bindings))

(use-package wc-mode
  :custom
  (wc-modeline-format "%tww"))


;;;; Built-in Libraries

(use-package prog-mode
  :init
  (defun prog-mode-hook-config nil
    (setq-local show-trailing-whitespace t)
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode))
  (add-hook 'prog-mode-hook 'prog-mode-hook-config))

(use-package flyspell
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (ispell-program-name "/usr/local/bin/aspell")
  :init
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package dired
  :custom
  (dired-use-ls-dired nil)
  (dired-auto-revert-buffer t)
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :config
  (defun dired-finder nil
    "Open the current dired directory in Finder."
    (interactive)
    (shell-command (concat "open " dired-directory)))
  :bind (:map dired-mode-map
              ("." . dired-finder)))

(use-package outline
  :init
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  :custom
  (outline-minor-mode-cycle t)
  :bind
  (:map outline-minor-mode-cycle-map
        ("TAB" . nil) ;; too often conflicts with tab-complete
        ("M-TAB" . outline-cycle))
  (:map outline-minor-mode-map
        ("<backtab>" . outline-cycle-buffer)))

(use-package time
  :custom
  (world-clock-time-format "%Z, %d %b, %I:%M %p")
  (world-clock-list
   '(("America/Los_Angeles" "Los Angeles")
     ("America/Chicago" "Chicago")
     ("America/Montreal" "Montreal")
     ("Europe/London" "London")))
  :init
  (add-to-list 'display-buffer-alist
               '("\\*wclock.*"
                 (display-buffer-below-selected)
                 (window-height . fit-window-to-buffer)
                 (window-parameters . ((select . t))))
               t))


;;; End of init.el
