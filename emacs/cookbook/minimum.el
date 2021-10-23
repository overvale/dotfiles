;;; minimum.el --- Oliver Minimum Config -*- lexical-binding: t -*-

;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; There are many "Emacs starter kits" out there, this one is mine. It is
;; designed to be exceedingly simple. Just copy this one file to
;; ~/.emacs/init.el and open Emacs. then innstall the below packages by
;; calling `package-install-selected-packages'.


;;; Goals and Philosophy

;; 1. Don't bother learning the Emacs key bindings you don't want to. Once you
;;    learn how to configure Emacs you can create whatever bindings make
;;    sense to you.
;;
;; 2. I'm a Mac user, so this config targets Mac users and sets some of the
;;    most common shortcuts, as well as making the modifier keys behave in a
;;    slightly more predictable way.
;;
;; 3. I use the built-in `package' to install packages and the package
;;    `use-package' to configure them. Along with the code below, this allows
;;    you use your init file as a single "source of truth" for your
;;    configuration. Much of the confusion a beginner experiences (in my
;;    opinion) is rooted in Emacs's ability to be customized both by an init
;;    file and interactively (which saves to an init file).
;;
;; 4. I highly recommend the combination of the packages `vertico',
;;    `orderless', and `marginalia'. These tools make it easier to explore
;;    Emacs and discover capabilities. You may eventually decide they're not
;;    for you, but I think they're a great place to start.
;;
;; 5. Provides some convenience bindings for my most used Emacs features.

;; ---------------------------------------------------------------------------


;;; Settings

;; There are 2 ways to customize Emacs, one is to edit your init file, which
;; is what this file is, and the other is to customize the settings
;; interactively (like any other program). Interactive customization is saved
;; to your `custom-file'. I, however, don't want any interactive
;; customization to be saved. This is because I want this init file to be the
;; single source of truth for how Emacs behaves. The below makes Emacs save
;; interactive customization in a temp file.
(setq custom-file (make-temp-file "emacs-custom-"))

;; Discovering the exact behavior of the below settings is left as an exercise
;; for the reader. Documentation can be found with "C-h o <symbol>".
;; `custom-set-variables' is a good general purpose tool for both setting the
;; value of variables and activating modes.
(custom-set-variables
 '(delete-selection-mode t)
 '(mark-even-if-inactive nil)
 '(ring-bell-function 'ignore)
 '(set-language-environment "UTF-8")
 '(create-lockfiles nil)
 '(make-backup-files nil)
 '(word-wrap t)
 '(truncate-lines t)
 '(global-auto-revert-mode t)
 '(uniquify-buffer-name-style 'forward)
 '(save-interprogram-paste-before-kill t))


;;; Package Management

(require 'package)

;; By default, the only source of packages is elpa.gnu.org, but there are tons
;; of great packages on MELPA (a repository of packages). To be able to
;; install those we add the URL to the following variable:
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; A list of all packages you'd like to install. Install packages with
;; `package-install-selected-packages' Remove packages, once removed from this
;; list, with `package-autoremove'.
(setq package-selected-packages
      '(modus-themes
        orderless
        transient
        undo-fu
        vertico
        visual-regexp
        visual-regexp-steroids))

;; I use a lot of transients in this config, so I need to make sure it is
;; loaded and configured before those are declared below.
(autoload 'transient-define-prefix "transient" nil t)
(setq transient-detect-key-conflicts t)

;; Orderless allows for "fuzzy matching" when filtering lists of commands,
;; variables, and functions. This is extremely useful since it is not always
;; easy to predict how those symbols are named.
(setq completion-styles '(orderless))

;; Vertico creates a useful interface for picking things from a list, which
;; is something you do all the time in Emacs.
(vertico-mode)

;; Marginalia displays useful information about completion candidates in
;; Vertico.
(marginalia-mode)

;; My preferred theme.
(modus-themes-load-operandi)

;; Visual Regexp, and Visual Regexp on Steroids, provides a sensible and
;; predictable search/replace interface using a regular expression engine that
;; you're already familiar with.
(define-key global-map [remap query-replace] 'vr/query-replace)
(with-eval-after-load 'visual-regexp
  (with-eval-after-load 'visual-regexp-steroids
    (custom-set-variables
     '(vr/engine 'pcre2el))))


;;; Functions

(defun find-user-init-file ()
  "Find the user-init-file."
  (interactive)
  (find-file user-init-file))


;;; Keybindings

;; In the old days ESC was used as a prefix key, but I want ESC to act like it
;; does everywhere else on my system and, you know, escape from things. So
;; I've remapped ESC to `keyboard-quit'.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Make shift-click extend the selection (region)
(global-set-key [S-down-mouse-1] 'ignore)
(global-set-key [S-mouse-1] 'mouse-save-then-kill)

;; Use M-drag-mouse-1 to create rectangle regions
(global-set-key [M-down-mouse-1] #'mouse-drag-region-rectangle)
(global-set-key [M-drag-mouse-1] #'ignore)
(global-set-key [M-mouse-1]      #'mouse-set-point)

;; I am of the opinion that you shouldn't need to learn Emacs's archaic
;; keybindings to use Emacs. You will probably need to eventually but until
;; then there should be an easier way to get around. To me this means
;; interacting with Emacs using responsive visual menus. Below is a 'Main
;; Menu' that offers the primary commands you'll need to get around.

(transient-define-prefix main-menu-transient ()
  "A 'Main Menu' for Emacs."
  ["Main Menu -- Press ESC to exit"
   ["Emacs"
    ("q" "Quit Emacs" save-buffers-kill-terminal)
    ("n" "New Frame" make-frame-command)
    ("o" "Open File" find-file)
    ("s" "Save" save-buffer)
    ("," "Settings" find-user-init-file)]
   ["Commands"
    ("z" "Undo" undo-fu-only-undo :transient t)
    ("Z" "Redo" undo-fu-only-redo :transient t)
    ("x" "M-x" execute-extended-command)
    ("h" "Help For..." describe-symbol)
    ("f" "Find" isearch-forward)
    ("r" "Replace" vr/query-replace)]
   ["Buffers"
    ("k" "Kill This Buffer" kill-current-buffer)
    ("b" "Switch to Buffer" switch-to-buffer)
    ("l" "List Buffers" ibuffer)
    "Toggle"
    ("t w" "Wrap Lines" visual-line-mode)
    ("t l" "Line Numbers" display-line-numbers-mode)
    ("t a" "Auto Wrap Lines" auto-fill-mode)]
   ["Windows"
    ("w h" "Split Horizontal" split-window-below)
    ("w v" "Split Vertical"   split-window-right)
    ("w b" "Balance"    balance-windows)
    ("w k" "Kill" delete-window)
    ("w o" "Kill Others"  delete-other-windows)
    ("w c" "Clone Window" clone-indirect-buffer)
    ("w t" "Tear Off" tear-off-window)]])

;; If you use the Main Menu you'll only ever need to learn one keybinding:
(define-key global-map (kbd "C-<return>") 'main-menu-transient)


;;; End of Config --- Good luck out there!
