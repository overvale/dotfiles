;; vimacs.el --- A lightweight starter kit for vim-style modes in Emacs  -*- lexical-binding: t -*-

;; Author: Oliver Taylor
;; URL: olivertaylor.net


;;; Commentary:

;; This file contains code required to create a very lightweight vim emulation
;; in Emacs. It was written as an exercise in creating Emacs minor modes and
;; keymap bindings. As a vim emulator it is laughably incomplete, and only
;; designed to be a starting point for those who want to write a totally
;; custom vim emulation using no external packages.

;; It is designed to cover only a few specific goals:

;; 1. Provide some approximation of the following modes: Normal, Visual,
;;    Replace. And assumes Insert mode is regular Emacs with no modifications.

;; 2. Adds a "mode indicator" to the beginning of the mode-line, which is
;;    distinct from mode "lighters" (which are also implemented).

;; 3. Changes the cursor shape based on mode.

;; 4. Provides keymaps for the above modes, in which any key can be bound to
;;    any Emacs function. It does not provide any functions which emulate vim
;;    behavior, those are left as an exercise for the reader.


;;; Config

;; Blink cursor globally. Disable if you don't like that.
(blink-cursor-mode 1)

;; Assign a key to enter NORMAL mode from INSERT (default Emacs). Note that it
;; will be very difficult to make this the default state for ALL new buffers.
;; It is simple enough to add hooks for your most-used modes, but difficult to
;; cover all circumstances. If you are that committed to vim emulation in
;; every corner of Emacs you're better off using an existing, and well
;; maintained, vim emulator like Evil.
(global-set-key (kbd "C-c v") 'vimacs-insert-to-normal)


;;; Mode Line Indicators

;; These variables define the text that should be prepended to the mode-line
;; to indicate what mode you are in. Emacs standard convention is to use
;; "lighters" for this purpose (the text shown in parenthesis in the
;; mode-line).

;; Define the "mode indicators" you want in the mode line. One for each mode.
;; To customize these see below.
(defvar vimacs-normal-modeline  " [Normal] "
  "Text to be used at start of mode-line to indicate Normal Mode.")
(defvar vimacs-insert-modeline  " [Insert] "
  "Text to be used at start of mode-line to indicate Insert Mode.")
(defvar vimacs-visual-modeline  " [Visual] "
  "Text to be used at start of mode-line to indicate Visual Mode.")
(defvar vimacs-replace-modeline " [Replace] "
  "Text to be used at start of mode-line to indicate Replace Mode.")

;; To emulate Evil's style, just set these variables after loading this code.
;; (setq vimacs-normal-modeline  " [N] "
;;       vimacs-insert-modeline  " [I] "
;;       vimacs-visual-modeline  " [V] "
;;       vimacs-replace-modeline " [R] ")

;; It is also possible to change the color of the mode-line with each mode
;; using the `set-face-attribute' function, but that is left as an exercise
;; for the reader.


;;; Cursor Settings

;; These variables define the various cursor shapes in each mode.
;; For details on acceptable formats for these settings see the documentation
;; for the variable 'cursor-type'.

(defvar vimacs-normal-cursor 'box
  "Cursor shape to display in Normal mode. See `cursor-type' for info.")
(defvar vimacs-insert-cursor 'bar
  "Cursor shape to display in Insert mode. See `cursor-type' for info.")
(defvar vimacs-visual-cursor 'hollow
  "Cursor shape to display in Visual mode. See `cursor-type' for info.")
(defvar vimacs-replace-cursor 'hbar
  "Cursor shape to display in Replace mode. See `cursor-type' for info.")

;; To customize put something like this in your config, after this code:
;; (setq vimacs-visual-cursor '(hbar . 9))

;;; Utility Functions and Variables

;; To actually change the content of the mode-line you need to customize the
;; `mode-line-format' variable either globally or buffer-locally. You'll need
;; to do this each time you move between modes. This is done by creating a
;; function which sets the prefix to the correct value and sets the current
;; buffer's mode line.

;; First, set the custom mode-line, which a placeholder where you want the
;; "mode indicator". Other than that this is 100% standard Emacs.
(setq vimacs-mode-line-format
      '("%e"
        vimacs-mode-indicator
        mode-line-front-space
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        mode-line-frame-identification
        mode-line-buffer-identification
        "   "
        mode-line-position
        (vc-mode vc-mode)
        "  "
        mode-line-modes
        mode-line-misc-info
        mode-line-end-spaces))

(defun vimacs-update-modeline (arg)
  "Set vimacs-mode-indicator to ARG and update mode-line locally.

This is a utility function for setting the mode-line-prefix
and updating the mode-line in the current buffer."
  (setq-local vimacs-mode-indicator arg)
  (setq-local mode-line-format vimacs-mode-line-format))


;;; The Modes

;; For the most part the only function of these modes is to define key
;; bindings. It is also useful to use the "lighter" so the modes can be
;; individually deactivated with the mouse when debugging keybindings.

;; Don't call these modes directly, rather use the "Enter/Exit Modes"
;; functions provided below to move between the modes.

;; I've bound only a small subset of the keys you might want to bind for a vim
;; emulation. You'll need to map each binding to an equivalent Emacs function,
;; or write your own function that approximates the vim behavior you want.

(define-minor-mode vimacs-normal-mode
  "Custom vim/emacs NORMAL mode

Note that all keyboard keys are either bound to Emacs functions or set
to be ignored. However, keybindings using modifier keys are not ignored,
thus typing is disabled but Emacs keybinding sequences are still allowed."
  :init-value nil
  :lighter " V:Normal"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "!") 'ignore)
            (define-key map (kbd "#") 'ignore)
            (define-key map (kbd "$") 'ignore)
            (define-key map (kbd "%") 'ignore)
            (define-key map (kbd "&") 'ignore)
            (define-key map (kbd "'") 'ignore)
            (define-key map (kbd "(") 'ignore)
            (define-key map (kbd ")") 'ignore)
            (define-key map (kbd "*") 'ignore)
            (define-key map (kbd "+") 'ignore)
            (define-key map (kbd ",") 'ignore)
            (define-key map (kbd "-") 'ignore)
            (define-key map (kbd ".") 'ignore)
            (define-key map (kbd "/") 'isearch-forward)
            (define-key map (kbd "0") 'ignore)
            (define-key map (kbd "1") 'ignore)
            (define-key map (kbd "2") 'ignore)
            (define-key map (kbd "3") 'ignore)
            (define-key map (kbd "4") 'ignore)
            (define-key map (kbd "5") 'ignore)
            (define-key map (kbd "6") 'ignore)
            (define-key map (kbd "7") 'ignore)
            (define-key map (kbd "8") 'ignore)
            (define-key map (kbd "9") 'ignore)
            (define-key map (kbd ":") 'execute-extended-command)
            (define-key map (kbd ":") 'ignore)
            (define-key map (kbd ";") 'ignore)
            (define-key map (kbd "<") 'ignore)
            (define-key map (kbd "=") 'ignore)
            (define-key map (kbd ">") 'ignore)
            (define-key map (kbd "?") 'ignore)
            (define-key map (kbd "@") 'ignore)
            (define-key map (kbd "A") 'ignore)
            (define-key map (kbd "B") 'ignore)
            (define-key map (kbd "C") 'ignore)
            (define-key map (kbd "D") 'ignore)
            (define-key map (kbd "E") 'ignore)
            (define-key map (kbd "F") 'ignore)
            (define-key map (kbd "G") 'ignore)
            (define-key map (kbd "H") 'ignore)
            (define-key map (kbd "I") 'ignore)
            (define-key map (kbd "J") 'ignore)
            (define-key map (kbd "K") 'ignore)
            (define-key map (kbd "L") 'ignore)
            (define-key map (kbd "M") 'ignore)
            (define-key map (kbd "N") 'ignore)
            (define-key map (kbd "O") 'ignore)
            (define-key map (kbd "P") 'ignore)
            (define-key map (kbd "Q") 'ignore)
            (define-key map (kbd "R") 'vimacs-normal-to-replace)
            (define-key map (kbd "S") 'ignore)
            (define-key map (kbd "T") 'ignore)
            (define-key map (kbd "U") 'ignore)
            (define-key map (kbd "V") 'ignore)
            (define-key map (kbd "W") 'ignore)
            (define-key map (kbd "X") 'ignore)
            (define-key map (kbd "Y") 'ignore)
            (define-key map (kbd "Z") 'ignore)
            (define-key map (kbd "[") 'ignore)
            (define-key map (kbd "\"") 'ignore)
            (define-key map (kbd "\\") 'ignore)
            (define-key map (kbd "]") 'ignore)
            (define-key map (kbd "^") 'ignore)
            (define-key map (kbd "_") 'ignore)
            (define-key map (kbd "`") 'ignore)
            (define-key map (kbd "a") 'ignore)
            (define-key map (kbd "b") 'backward-word)
            (define-key map (kbd "c") 'ignore)
            (define-key map (kbd "d") 'ignore)
            (define-key map (kbd "e") 'forward-word)
            (define-key map (kbd "f") 'ignore)
            (define-key map (kbd "g") 'ignore)
            (define-key map (kbd "h") 'backward-char)
            (define-key map (kbd "i") 'vimacs-normal-to-insert)
            (define-key map (kbd "j") 'next-line)
            (define-key map (kbd "k") 'previous-line)
            (define-key map (kbd "l") 'forward-char)
            (define-key map (kbd "m") 'ignore)
            (define-key map (kbd "n") 'ignore)
            (define-key map (kbd "o") 'ignore)
            (define-key map (kbd "p") 'ignore)
            (define-key map (kbd "q") 'ignore)
            (define-key map (kbd "r") 'ignore)
            (define-key map (kbd "s") 'ignore)
            (define-key map (kbd "t") 'ignore)
            (define-key map (kbd "u") 'ignore)
            (define-key map (kbd "v") 'vimacs-normal-to-visual)
            (define-key map (kbd "w") 'ignore)
            (define-key map (kbd "x") 'ignore)
            (define-key map (kbd "y") 'ignore)
            (define-key map (kbd "z") 'ignore)
            (define-key map (kbd "{") 'backward-paragraph)
            (define-key map (kbd "|") 'ignore)
            (define-key map (kbd "}") 'forward-paragraph)
            (define-key map (kbd "~") 'ignore)
            map))

;; One interesting feature of Emacs is that you can have multiple minor modes
;; active at the same time. So you can leave NORMAL mode active and layer
;; VISUAL mode on top of it, that way all the NORMAL mode bindings still
;; apply, and you can override any of those bindings with a binding in VISUAL
;; mode.

(define-minor-mode vimacs-visual-mode
  "Custom vim/emacs VISUAL mode"
  :init-value nil
  :lighter " V:Select"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<ESC>") 'vimacs-visual-to-normal)
            (define-key map (kbd "i") 'vimacs-visual-to-insert)
            ;; it doesn't make sense to go from visual to replace, so prohibit
            ;; that since the normal binding would otherwise be active.
            (define-key map (kbd "R") 'ignore)
            map))

(define-minor-mode vimacs-replace-mode
  "Custom vim/emacs REPLACE mode"
  :init-value nil
  :lighter " V:Replace"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<ESC>") 'vimacs-replace-to-normal)
            map))


;;; Functions for Entering/Exiting Modes

;; Each time you switch from one mode to another, you'll need a separate
;; function which sets the cursor type, sets the mode-line prefix,
;; and enters/exits the minor modes.

;; Each of these should be bound in the modes above so that, for example,
;; typing "i" in NORMAL mode calls the normal-to-insert function below.

;; Insert to Normal
(defun vimacs-insert-to-normal ()
  (interactive)
  (vimacs-normal-mode 1)
  (setq-local cursor-type vimacs-normal-cursor)
  (vimacs-update-modeline vimacs-normal-modeline))

;; Normal to Insert
(defun vimacs-normal-to-insert ()
  (interactive)
  (vimacs-normal-mode -1)
  (setq-local cursor-type vimacs-insert-cursor)
  (vimacs-update-modeline vimacs-insert-modeline))

;; Normal to Visual
;; Notice that normal mode is NOT deactivated. Visual mode is LAYERED on top
;; of normal mode, that way we retain all of the normal mode key bindings.
(defun vimacs-normal-to-visual ()
  (interactive)
  (set-mark (point))
  (vimacs-visual-mode 1)
  (setq-local cursor-type vimacs-visual-cursor)
  (vimacs-update-modeline vimacs-visual-modeline))

;; Visual to Normal
(defun vimacs-visual-to-normal ()
  (interactive)
  (vimacs-visual-mode -1)
  ;; Normal mode should still be active
  (deactivate-mark)
  (setq-local cursor-type vimacs-normal-cursor)
  (vimacs-update-modeline vimacs-normal-modeline))

;; Visual to Insert
(defun vimacs-visual-to-insert ()
  (interactive)
  (vimacs-visual-mode -1)
  (vimacs-normal-mode -1)
  (setq-local cursor-type vimacs-insert-cursor)
  (vimacs-update-modeline vimacs-insert-modeline))

;; Normal to Replace
(defun vimacs-normal-to-replace ()
  (interactive)
  (vimacs-normal-mode -1)
  (vimacs-replace-mode 1)
  (overwrite-mode 1)
  (setq-local cursor-type vimacs-replace-cursor)
  (vimacs-update-modeline vimacs-replace-modeline))

;; Replace to Normal
(defun vimacs-replace-to-normal ()
  (interactive)
  (overwrite-mode -1)
  (vimacs-replace-mode -1)
  (vimacs-normal-mode 1)
  (setq-local cursor-type vimacs-normal-cursor)
  (vimacs-update-modeline vimacs-normal-modeline))


(provide 'vimacs)
;; vimacs.el ends here -- happy hacking!
