;;; PACKAGES
;; setup package system
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ))
(package-initialize)

;;; ENCODING
;; Set default encoding
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;;; CUSTOM.EL
;; Don't add custom section directly under init.el.
;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;;; FILENAMES
;; When visiting 2 files with the same name, use this:
;; foo/makefile & bar/makefile
;; instead of this: makefile<foo/foo> & makefile<bar/bar>
;;(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; MAC-LIKE SETTINGS
;; Command to super
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)
;; Left-option to meta for commands
(setq mac-option-modifier 'meta)
(setq mac-left-option-modifier 'meta)
;; Right-option to option, for special characters: ¡™£¢∞¶•ªº
(setq mac-right-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)

;; STARTUP PREFERENCES
(setq initial-scratch-message
      (concat
       ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
       ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n"
       ";; WELCOME TO EMACS\n"))
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;; General Settings
;;  --------------------------------------------------------

(setq vc-follow-symlinks t)
(menu-bar-mode 1) ;this effects full-screen
(tool-bar-mode -1)
(show-paren-mode t)
(setq show-paren-delay 0)
(delete-selection-mode t)
(global-auto-revert-mode t)
(desktop-save-mode 1) ;sessions
(recentf-mode 1) ;enables "Open Recent..." in file menu
;;(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq visible-bell t)
(setq tab-width 4)

;; WORD WRAP
(global-visual-line-mode t)
;; with visual-line-mode set,
;; C-a and C-b go to beginning/end-of-visual-line
;; which is inconsistant with standard Mac behaviour
;; so I've bound those to command-left/right
(global-set-key (kbd "C-a") 'beginning-of-line)
(global-set-key (kbd "C-e") 'end-of-line)
(global-set-key (kbd "s-<left>") 'beginning-of-visual-line)
(global-set-key (kbd "s-<right>") 'end-of-visual-line)

;; Tell Emacs to use paths/apps unique to my setup
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-list-command "list")


;;; Packages
;;  --------------------------------------------------------

;; Activate the modes/packages I like to use
(which-key-mode t)              ;hints
(global-undo-tree-mode t)       ;activate undo-tree everywhere
(require 'expand-region)        ;lovely plugin
(smart-mode-line-enable)

;; counsel settings
(counsel-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
;;(setq ivy-initial-inputs-alist nil) ;removes ^ from initial input


;;; Hooks
;;  --------------------------------------------------------

(add-hook 'markdown-mode-hook 'oht/markdown-mode-hook)
(add-hook 'org-mode-hook      'oht/org-mode-hook)
(add-hook 'emacs-lisp-mode    'oht/emacs-lisp-mode)

(load "~/dot/emacs/appearance.el")
(load "~/dot/emacs/org.el")
(load "~/dot/emacs/settings.el")

;; init.el ends here
