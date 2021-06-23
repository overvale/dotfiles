;;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Copyright (C) 2021 Oliver Taylor

;; Author: Oliver Taylor
;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; This file has an outline which can be viewed by looking at comments
;; starting with three or more semicolons. `outline-minor-mode' supports this
;; convention by default and helps with navigation. You can also create an
;; occur buffer with the search [^;;;+].


;;; Configuration

;;;; Variables

(when (eq system-type 'darwin)
  (cd "~/home")
  (defvar oht-dotfiles             "~/home/dot/emacs/")
  (defvar oht-orgfiles             "~/home/org/")
  (defvar user-downloads-directory "~/Downloads/"))

(when (eq system-type 'windows-nt)
  (cd "~/")
  (defvar oht-dotfiles             "~/.emacs.d/")
  (defvar oht-orgfiles             "~/home/org/")
  (defvar user-downloads-directory "~/home/Downloads/"))

;;;; Settings

;; Save all interactive customization to a temp file, which is never loaded.
;; This means interactive customization is session-local. Only this init file persists sessions.
(setq custom-file (make-temp-file "emacs-custom-"))

;; For most of my "settings" I use custom-set-variables, which does a bunch of neat stuff.
;; First, it calls a variable's "setter" function, if it has one.
;; Second, it can activate modes as well as set variables.
;; Third, it handles setting buffer-local variables correctly.
;; https://with-emacs.com/posts/tutorials/almost-all-you-need-to-know-about-variables/#_user_options
;; https://old.reddit.com/r/emacs/comments/exnxha/withemacs_almost_all_you_need_to_know_about/fgadihl/

(custom-set-variables
 '(inhibit-startup-screen t)
 '(global-auto-revert-mode t)
 '(save-place-mode t)
 '(recentf-mode t)
 '(winner-mode t)
 '(show-paren-mode t)
 '(blink-cursor-mode t)
 '(cursor-type '(bar . 3))
 '(cursor-in-non-selected-windows 'hollow)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(minibuffer-depth-indicate-mode t)
 '(ring-bell-function 'ignore)
 '(set-language-environment "UTF-8")
 '(frame-title-format '("%b"))
 '(uniquify-buffer-name-style 'forward)
 '(vc-follow-symlinks t)
 '(find-file-visit-truename t)
 '(create-lockfiles nil)
 '(make-backup-files nil)
 '(load-prefer-newer t)
 '(bookmark-save-flag 1)
 ;;'(bookmark-menu-confirm-deletion t) ; Emacs 28
 '(word-wrap t)
 '(truncate-lines t)
 '(delete-by-moving-to-trash t)
 '(trash-directory "~/.Trash")
 '(confirm-kill-processes nil)
 '(save-interprogram-paste-before-kill t)
 '(kill-do-not-save-duplicates t)
 '(split-window-keep-point nil)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(mark-even-if-inactive nil)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(fill-column 78))

(when (eq system-type 'darwin)
  (setq locate-command "mdfind"
        trash-dircetory "~/.Trash"))

;; Mode Line
(custom-set-variables
 '(display-time-format " %Y-%m-%d  %H:%M")
 '(display-time-interval 60)
 '(display-time-default-load-average nil)
 '(column-number-mode t)
 '(display-time-mode t))

;; Include battery in mode-line on laptop
(when (string= (system-name) "shadowfax.local")
  (custom-set-variables
   '(display-battery-mode t)
   '(battery-mode-line-format " [%b%p%%]")))


;;;; Functions

(defun mark-line (arg)
  "Put mark at end of line.
ARG works as in `forward-line'.  If this command is repeated,
it marks the next ARG lines after the ones already marked."
  (interactive "p")
  (push-mark
   (save-excursion
     (if (and (eq last-command this-command) (mark t))
	 (goto-char (mark)))
     (forward-line arg)
     (point))
   nil t))

(defalias 'mark-sentence 'mark-end-of-sentence)

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

(defun pipe-region (start end command)
  ;; https://github.com/oantolin/emacs-config/blob/master/my-lisp/text-extras.el
  "Pipe region/buffer through shell command."
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

(defun find-user-init-file ()
  "Find the user-init-file"
  (interactive)
  (find-file user-init-file))

(defun find-file-recursively ()
  "Find Files Recursively using completing read."
  (interactive)
  (find-file (completing-read "Find File Recursively: "
                              (directory-files-recursively default-directory ".+"))))

(defun exchange-point-and-mark-dwim ()
  "Respect region active/inactive and swap point and mark.
If a region is active, then leave it activated and swap point and mark.
If no region is active, then just swap point and mark."
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

(defun org-insert-date-today ()
  "Insert today's date using standard org formatting."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(defun org-insert-date-today-inactive ()
  "Inserts today's date in org inactive format."
  (interactive)
  (insert (format-time-string "\[%Y-%m-%d %a\]")))

;; Dispatch Functions -- I use these to launch frequently-used stuff
(defun oht-dispatch-downloads () (interactive) (find-file "~/Downloads"))
(defun oht-dispatch-reading () (interactive) (find-file "~/Downloads/reading"))
(defun oht-dispatch-watch () (interactive) (find-file "~/Downloads/watch"))
(defun oht-dispatch-google-news () (interactive) (browse-url "http://68k.news/"))

(defun kill-buffer-dwim (&optional u-arg)
  "Call kill-current-buffer, with C-u: call kill-buffer."
  (interactive "P")
  (if u-arg
      (call-interactively 'kill-buffer)
    (call-interactively 'kill-current-buffer)))

(defun line-spacing-interactive (&optional arg)
  "Locally sets the `line-spacing' variable. Takes an argument, 0 by default."
  (interactive "P")
  (setq-local line-spacing arg))


;;; Keybindings

;;;; Modifiers

;; If on a Mac, use the command key as Super, left-option for Meta, and
;; right-option for Alt.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-right-command-modifier 'meta
        mac-right-option-modifier 'nil))

;; If on Windows, use Windows key as Super
(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-]))


;;;; Keybinding Macros

;; Let's start by making my own macro to define multiple keys. Why would I do
;; this if I'm also using `bind-key'? So that I can bind keys to keymaps.
;; `bind-key' assumes you always want to bind keys to commands, but you can
;; bind them to keymaps too, which is super handy, thus the macro.

(defmacro define-keys (keymap &rest body)
  "Defines key bindings in BODY for keymap in KEYMAP.
Accepts CONS where CAR is a key in string form, to be passed to `kbd', and CADR is a command."
  `(progn
     ,@(cl-loop for binding in body
                collect
                `(let ((map ,keymap)
                       (key ,(car binding))
                       (def ,(cadr binding)))
                   (define-key map (kbd key) def)))))

;; Minor modes override global bindings, so any bindings you don't want
;; overridden should be placed in a minor mode.

(defvar bosskey-mode-map (make-keymap)
  "Keymap for bosskey-mode.")

(define-minor-mode bosskey-mode
  "Minor mode for my personal keybindings, which override others.
The only purpose of this minor mode is to override global keybindings.
Keybindings you define here will take precedence."
  :init-value t
  :global t
  :keymap bosskey-mode-map)

(add-to-list 'emulation-mode-map-alists
             `((bosskey-mode . ,bosskey-mode-map)))

(defmacro boss-key (key command)
  "Defines a key binding for bosskey-mode."
  `(define-key bosskey-mode-map (kbd ,key) ,command))

(defmacro boss-keys (&rest body)
  "Defines key bindings for bosskey-mode.
Accepts CONS where CAR is a key in string form, to be passed to `kbd', and CADR is a command."
  `(progn
     ,@(cl-loop for binding in body
                collect
                `(let ((key ,(car binding))
                       (def ,(cadr binding)))
                   (define-key bosskey-mode-map (kbd key) def)))))


;;;; Actual Keybindings

;; https://www.reddit.com/r/emacs/comments/67rlfr/esc_vs_cg/dgsozkc/
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Mac-like bindings
(when (eq system-type 'darwin)
  (boss-keys
   ("s-q"       'save-buffers-kill-terminal)
   ("s-m"       'iconify-frame)
   ("s-n"       'make-frame-command)
   ("s-s"       'save-buffer)
   ("s-,"       'find-user-init-file)
   ("s-o"       'find-file)
   ("s-z"       'undo-fu-only-undo)
   ("s-Z"       'undo-fu-only-redo)
   ("s-x"       'kill-region)
   ("s-c"       'kill-ring-save)
   ("s-v"       'yank)
   ("s-<left>"  'beginning-of-visual-line)
   ("s-<right>" 'end-of-visual-line)
   ("s-<up>"    'beginning-of-buffer)
   ("s-<down>"  'end-of-buffer)))

;; Personal keybindings
(boss-keys
 ("C-<return>" 'oht-transient-general)
 ("M-["        'previous-buffer)
 ("M-]"        'next-buffer)
 ("M-o"        'other-window)
 ("M-."        'embark-act)
 ("M-'"        'my:hippie-expand)
 ("M-\\"       'cycle-spacing)
 ("M-z"        'zap-up-to-char)
 ("M-H"        'oht-transient-marks)
 ("M-N"        'navigation-keymap--activate)
 ("C-d"        'delete-forward-char)
 ("C-x k"      'kill-buffer-dwim)
 ("C-x C-x"    'exchange-point-and-mark-dwim)
 ("C-x C-b"    'ibuffer-other-window)
 ("C-x C-n"    'make-frame-command)
 ("C-x C-,"    'find-user-init-file)
 ("M-g ."      'xref-find-definitions)
 ("M-0"        'delete-window)
 ("M-1"        'delete-other-windows)
 ("M-2"        'split-window-below)
 ("M-3"        'split-window-right)
 ("M-4"        'undefined)
 ("M-5"        'undefined)
 ("M-6"        'undefined)
 ("M-7"        'undefined)
 ("M-8"        'undefined)
 ("M-9"        'undefined)
 ("M--"        'undefined))

(global-set-key [remap capitalize-word] 'capitalize-dwim)
(global-set-key [remap downcase-word]   'downcase-dwim)
(global-set-key [remap upcase-word]     'upcase-dwim)

;;;; Mouse

;; Gasp! An Emacs user that actually uses the mouse?! Scandalous.

;; Start by making shift-click extend the selection (region)
(global-set-key [S-down-mouse-1] 'ignore)
(global-set-key [S-mouse-1] 'mouse-save-then-kill)

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


;;; Package Management

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archive-priorities '(("gnu" . 20)("melpa" . 10)))

(defmacro select-package (package)
  "Adds package to `package-selected-packages'."
  `(add-to-list 'package-selected-packages ,package))

;; Install packages with `package-install-selected-packages', remove packages
;; with `package-autoremove'. Both functions look at the variable
;; `package-selected-packages' for the canonical list of packages.

;; You can automatically remove anything not in `package-selected-packages'
;; (thus not in this init file) by un-commenting this hook:
;; (add-hook 'emacs-startup-hook 'package-autoremove)

(defvar pkg-ops-map
  (let ((map (make-sparse-keymap "Packages")))
    (define-key map "h" '("describe" . describe-package))
    (define-key map "r" '("reinstall" . package-reinstall))
    (define-key map "a" '("autoremove" . package-autoremove))
    (define-key map "d" '("delete" . package-delete))
    (define-key map "i" '("install" . package-install))
    (define-key map "f" '("refresh" . package-refresh-contents))
    (define-key map "l" '("list" . list-packages))
    map))

(global-set-key (kbd "C-x p") pkg-ops-map)


;;;; Blackout, Transient

;; This config requires these 2 packages to run properly.

(select-package 'blackout)
(autoload 'blackout "blackout" nil t)
(blackout 'eldoc-mode)
(blackout 'emacs-lisp-mode "Elisp")
(blackout 'auto-fill-function " Fill")

(select-package 'transient)
(autoload 'transient-define-prefix "transient" nil t)


;;; Built-In Packages & Lisp

;;;; Flyspell

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(with-eval-after-load 'flyspell

  (blackout 'flyspell-mode " Spell")

  (transient-define-prefix flyspell-mode-transient ()
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
      ("<return>" "Fix" ispell-word)
      ("<SPC>" "Auto Fix" flyspell-auto-correct-word)
      ("<DEL>" "Delete Word" kill-word)
      ("C-/" "Undo" undo-fu-only-undo)
      ("M-/" "Redo" undo-fu-only-redo)]])

  ) ; end flyspell


;;;; Facedancer Mode

;; Facedancer defines a group of user options which set various attributes of
;; the default, fixed-pitch, and variable-pitch faces. Each option should be
;; set either via the `customize' interface or by calling
;; `custom-set-variables' in your init file as each option has "setter"
;; functions and this is the only way to set your fonts.

(defgroup facedancer ()
  "Options for facedancer."
  :group 'faces
  :prefix "facedancer-")

(defcustom facedancer-monospace-family nil
  "Monospace font family of the default and fixed-pitch faces."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'default nil     :family value)
         (set-face-attribute 'fixed-pitch nil :family value)))

(defcustom facedancer-monospace-height 12
  "Font size, as an integer, for the default and fixed-pitch sizes."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'default nil     :height (* value 10))
         (set-face-attribute 'fixed-pitch nil :height 1.0)))

(defcustom facedancer-monospace-weight 'normal
  "Weight of both the default and fixed-pitch faces."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'default nil     :weight value)
         (set-face-attribute 'fixed-pitch nil :weight value)))

(defcustom facedancer-monospace-width 'normal
  "Width of both the default and fixed-pitch faces."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'default nil     :width value)
         (set-face-attribute 'fixed-pitch nil :width value)))

(defcustom facedancer-variable-family nil
  "Variable font family of the variable-pitch face."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'variable-pitch nil :family value)))

(defcustom facedancer-variable-height 14
  "Font point size, as an integer, for the variable-pitch size."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'variable-pitch nil :height 1.0)))

(defcustom facedancer-variable-weight 'normal
  "Weight of the variable-pitch face."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'variable-pitch nil :weight value)))

(defcustom facedancer-variable-width 'normal
  "Width of the variable-pitch face."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'variable-pitch nil :width value)))

(defcustom facedancer-mode-line-family nil
  "Font family of both the active and inactive mode-lines."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'mode-line nil          :family value)
         (set-face-attribute 'mode-line-inactive nil :family value)))

(defcustom facedancer-mode-line-height 12
  "Font point size, as an integer, for the active and inactive mode-lines."
  :group 'facedancer
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-face-attribute 'mode-line nil          :height (* value 10))
         (set-face-attribute 'mode-line-inactive nil :height (* value 10))))

(define-minor-mode facedancer-vadjust-mode
  "Minor mode to adjust the variable-pitch face height buffer-locally.
A minor mode to scale (in the current buffer) the variable-pitch
face up to the height defined by ‘facedancer-variable-height’ and
the fixed-pitch face down to the height defined by
‘facedancer-monospace-height’."
  :init-value nil
  :lighter " V+"
  (if facedancer-vadjust-mode
      (progn
        (setq-local variable-pitch-remapping
                    (face-remap-add-relative 'variable-pitch
                                             :height (/ (float facedancer-variable-height)
                                                        (float facedancer-monospace-height))))
        (setq-local fixed-pitch-remapping
                    (face-remap-add-relative 'fixed-pitch
                                             :height (/ (float facedancer-monospace-height)
                                                        (float facedancer-variable-height))))
        (force-window-update (current-buffer)))
    (progn
      (face-remap-remove-relative variable-pitch-remapping)
      (face-remap-remove-relative fixed-pitch-remapping)
      (force-window-update (current-buffer)))))

(add-hook 'buffer-face-mode-hook (lambda () (facedancer-vadjust-mode 'toggle)))


;;;; Secondary Selection

;; Emacs's Secondary Selection assumes you only want to interact with it via
;; the mouse, however it is perfectly possible to do it via the keyboard, all
;; you need is some wrapper functions to make things keybinding-addressable.

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


;;;; Minibuffer

(custom-set-variables
 '(enable-recursive-minibuffers t)
 '(savehist-mode t)
 '(completion-show-help nil)
 '(resize-mini-windows t))

;; The completions list itself is read-only, so why not allow some nice navigation?
(define-key completion-list-mode-map (kbd "n") 'next-completion)
(define-key completion-list-mode-map (kbd "p") 'previous-completion)

;; I want to use my usual other-window binding to switch between the
;; minibuffer and the completions list. There is a built-in
;; `switch-to-completions' but it doesn't support Embark or fall-back to
;; `other-window', so I made my own.
(defun switch-to-completions-or-other-window ()
  "Switch to the completions window, if it exists, or another window."
  (interactive)
  (if (get-buffer-window "*Embark Collect Completions*")
      (select-window (get-buffer-window "*Embark Collect Completions*"))
    (if (get-buffer-window "*Completions*")
        (progn
          (select-window (get-buffer-window "*Completions*"))
          (when (bobp) (next-completion 1)))
      (other-window 1))))

(defun switch-to-minibuffer ()
  "Focus the active minibuffer.
Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

(define-key minibuffer-local-completion-map (kbd "M-o") 'switch-to-completions-or-other-window)
(define-key completion-list-mode-map (kbd "M-o") 'switch-to-minibuffer)


;;;; Isearch

(setq search-whitespace-regexp ".*?")
(setq isearch-lax-whitespace t)
(setq isearch-lazy-count t)

(defun isearch-exit-at-start ()
  "Exit search at the beginning of the current match."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook 'isearch-exit-at-start)

;; The package 'isearch-mb' allows you to edit the incremental search in the
;; minibuffer as you type, rather than having to call `isearch-edit-string'.
(select-package 'isearch-mb)
(isearch-mb-mode)

;; Quit isearch when calling occur
(add-to-list 'isearch-mb--after-exit #'occur)


;;;; Outline

;; `outline' provides major and minor modes for collapsing sections of a
;; buffer into an outline-like format. Let's turn that minor mode into a
;; global minor mode and enable it.
(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)
(global-outline-minor-mode +1)

(blackout 'outline-minor-mode)

(transient-define-prefix oht-transient-outline ()
  "Transient for Outline Minor Mode navigation"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Show/Hide"
    ("<tab>" "Show Subtree" outline-show-subtree)
    ("<backtab>" "Hide Subtree" outline-hide-subtree)
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
    ("C-/" "Undo" undo-fu-only-undo)
    ("M-/" "Redo" undo-fu-only-redo)
    ("c" "Consult" consult-outline :transient nil)]])


;;;; Pulse

(defun pulse-line ()
  "Interactive function to pulse the current line."
  (interactive)
  (pulse-momentary-highlight-one-line (point)))

(defadvice other-window (after other-window-pulse activate) (pulse-line))
(defadvice delete-window (after delete-window-pulse activate) (pulse-line))
(defadvice recenter-top-bottom (after recenter-top-bottom-pulse activate) (pulse-line))

(defun ct/yank-pulse-advice (orig-fn &rest args)
  "Pulse line when yanking"
  ;; From https://christiantietze.de/posts/2020/12/emacs-pulse-highlight-yanked-text/
  (let (begin end)
    (setq begin (point))
    (apply orig-fn args)
    (setq end (point))
    (pulse-momentary-highlight-region begin end)))

(advice-add 'yank :around #'ct/yank-pulse-advice)


;;;; Remember Mode

(custom-set-variables
 '(remember-data-file (concat oht-orgfiles "remember-notes"))
 '(remember-notes-initial-major-mode 'fundamental-mode)
 '(remember-notes-auto-save-visited-file-name t))

(defun remember-dwim ()
  "If the region is active, capture with region, otherwise just capture."
  (interactive)
  (if (use-region-p)
      (let ((current-prefix-arg 4)) (call-interactively 'remember))
    (remember)))


;;;; iBuffer

(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Org"   (or (mode . org-mode)
                      (mode . org-agenda-mode)))
         ("Dired" (mode . dired-mode))
         ("ELisp" (mode . emacs-lisp-mode))
         ("Help"  (or (name . "\*Help\*")
                      (name . "\*Apropos\*")
                      (name . "\*Info\*"))))))

(defun ibuffer-setup ()
  (hl-line-mode 1)
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook 'ibuffer-setup)


;;;; Hippie Expand

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        try-expand-line-all-buffers
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; https://github.com/rails-to-cosmos/.emacs.d/blob/master/lisp/completion/hippie-expand-completion.el

(defun my:hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
   The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'my:hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (cl-flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'my:hippie-expand-completions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

(defun my:hippie-expand-with (hippie-expand-function)
  "Offer completion using the specified hippie-expand function."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (stop (or (cdr bounds) (point)))
         (collection (my:hippie-expand-completions hippie-expand-function))
         (selection (and collection
                         (completing-read
                          "Completions: " collection nil nil
                          ))))
    (when selection
      (delete-region start stop)
      (insert selection)
      )))

(defun my:hippie-expand ()
  "Offer ido-based completion for the word at point."
  (interactive)
  (my:hippie-expand-with 'hippie-expand))



;;;; Dired

(setq dired-use-ls-dired nil) ; no more warning message

(defun dired-open-file ()
  "In dired, open the file named on this line using the 'open' shell command."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "open" nil 0 nil file)
    (message "Opening %s done" file)))

(with-eval-after-load 'dired
  (define-keys dired-mode-map
    ("O"   'dired-open-file)
    ("C-/" 'dired-undo)))

(defun dired-mode-setup ()
  "Settings for dired mode."
  (dired-hide-details-mode 1)
  (auto-revert-mode)
  (hl-line-mode 1))

(add-hook 'dired-mode-hook 'dired-mode-setup)


;;; Appearance

(select-package 'modus-themes)

(custom-set-variables
  '(modus-themes-slanted-constructs t)
  '(modus-themes-links 'faint-neutral-underline)
  '(modus-themes-mode-line 'accented)
  '(modus-themes-region 'bg-only)
  '(modus-themes-diffs 'desaturated)
  '(modus-themes-org-blocks 'grayscale)
  '(modus-themes-syntax 'faint))

(modus-themes-load-operandi)

;; If on a Mac, assume Mitsuharu Yamamoto’s fork -- check for dark/light mode,
;; if dark mode load the dark theme, also add a hook for syncing with the
;; system.
(when (eq system-type 'darwin)
  (if (string= (plist-get (mac-application-state) :appearance) "NSAppearanceNameDarkAqua")
      (modus-themes-load-vivendi))
  (add-hook 'mac-effective-appearance-change-hook 'modus-themes-toggle))

(setq text-scale-mode-step 1.09)

(when (eq system-type 'darwin)
  (custom-set-variables
   '(facedancer-monospace-family "SF Mono")
   '(facedancer-variable-family  "New York")
   '(facedancer-mode-line-family "SF Compact Text")
   '(facedancer-mode-line-height 13)))

(when (eq system-type 'windows-nt)
  (custom-set-variables
   '(facedancer-monospace-family "Consolas")
   '(facedancer-variable-family  "Calibri")
   '(facedancer-mode-line-family "Calibri")
   '(facedancer-monospace-height 10)
   '(facedancer-variable-height 11)
   '(facedancer-mode-line-height 11)))


;;; External Packages

;;;; Narrowing & Searching

;;;;; Orderless

(select-package 'orderless)
(require 'orderless)
(custom-set-variables
 '(completion-styles '(orderless))
 '(completion-category-defaults nil)
 '(completion-category-overrides '((file (styles . (partial-completion))))))

(select-package 'vertico)
(vertico-mode)

(select-package 'marginalia)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
(marginalia-mode)

(select-package 'embark)
(with-eval-after-load 'embark

  (when (eq system-type 'darwin)
    (setq youtube-dl-path "/usr/local/bin/youtube-dl"))
  (defun youtube-dl-URL-at-point ()
    "Send the URL at point to youtube-dl."
    (interactive)
    (async-shell-command (format "%s -o \"%s%s\" -f best \"%s\""
                                 youtube-dl-path
                                 user-downloads-directory
                                 "%(title)s.%(ext)s"
                                 (ffap-url-at-point))))

  (autoload 'dired-jump "dired")
  (define-keys embark-file-map
    ("O" 'macos-open-file)
    ("j" 'dired-jump))

  (define-keys embark-url-map
    ("d" 'youtube-dl-URL-at-point)
    ("&" 'browse-url-default-macosx-browser))

  ) ; end embark

(select-package 'consult)
(global-set-key [remap yank-pop] 'consult-yank-pop)
(custom-set-variables
 '(consult-preview-key (kbd "C-="))
 '(consult-config
   `((consult-mark :preview-key any))))

(select-package 'embark-consult)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))


;;;; Org

(autoload 'oht-org-agenda-today-pop-up "org")
(autoload 'oht-org-agenda-today "org")

(select-package 'org)
(with-eval-after-load 'org

  (custom-set-variables
   '(org-list-allow-alphabetical t)
   '(org-enforce-todo-dependencies t)
   '(org-enforce-todo-checkbox-dependencies t)
   '(org-log-done 'time)
   '(org-log-into-drawer t))

  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-return-follows-link t
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
          ("s" "Scanline")
          ("si" "Scanline Inbox" entry
           (file+headline ,(concat oht-orgfiles "scanline.org") "Inbox")
           "* %?\n\n" :empty-lines 1)
          ("sl" "Scanline Log Entry" entry
           (file+olp+datetree ,(concat oht-orgfiles "scanline_logbook.org"))
           "* %^{prompt}\n%T\n\n%?" :empty-lines 1 :tree-type week )
          ;; -----------------------------
          ("e" "Emacs Config" entry
           (file+headline ,(concat oht-orgfiles "emacs.org") "Emacs Config")
           "* TODO %?" :empty-lines 1)))

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

  (defun oht-org-agenda-exit-delete-window ()
    "Wrapper around org-agenda-exit & delete-window."
    (interactive)
    (org-agenda-exit)
    (delete-window))

  (defun oht-org-agenda-today-pop-up ()
    "Displays oht-org-agenda-today in a small window.
Also provides bindings for deleting the window, thus burying the
buffer, and exiting the agenda and releasing all the buffers."
    (interactive)
    (split-window-below)
    (other-window 1)
    (oht-org-agenda-today)
    (fit-window-to-buffer)
    (use-local-map (copy-keymap org-agenda-mode-map))
    (local-set-key (kbd "x") 'oht-org-agenda-exit-delete-window)
    (local-set-key (kbd "q") 'delete-window))

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
      ("." "Insert Date, Active" org-insert-date-today)
      (">" "Insert Date, Inactive" org-insert-date-today-inactive)
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

  ) ; End org config

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(with-eval-after-load 'org-agenda
  (defvar org-agenda-todo-map
    (let ((map (make-sparse-keymap "Org TODO")))
      (define-key map "t" '("TODO"     . org-agenda-todo-set-todo))
      (define-key map "l" '("LATER"    . org-agenda-todo-set-later))
      (define-key map "d" '("DONE"     . org-agenda-todo-set-done))
      (define-key map "c" '("CANCELED" . org-agenda-todo-set-canceled))
      map) "A map for setting org statuses.")
  (define-key org-agenda-mode-map (kbd "s-z") 'org-agenda-undo)
  (define-key org-agenda-mode-map (kbd "C-/") 'org-agenda-undo)
  (define-key org-agenda-mode-map (kbd "t") org-agenda-todo-map))


;;;; Navigation Mode

(defun define-navigation-keys (map)
  "Defines navigation keys for a map supplied by argument."
  (interactive "S")
  (define-key map (kbd "n") 'next-line)
  (define-key map (kbd "p") 'previous-line)
  (define-key map (kbd "f") 'forward-char)
  (define-key map (kbd "b") 'backward-char)
  (define-key map (kbd "M-f") 'forward-word)
  (define-key map (kbd "M-b") 'backward-word)
  (define-key map (kbd "a") 'beginning-of-line)
  (define-key map (kbd "e") 'end-of-line)
  (define-key map (kbd "{") 'backward-paragraph)
  (define-key map (kbd "}") 'forward-paragraph)
  (define-key map (kbd "(") 'backward-sentence)
  (define-key map (kbd ")") 'forward-sentence)
  (define-key map (kbd "s") 'isearch-forward)
  (define-key map (kbd "r") 'isearch-backward)
  (define-key map (kbd "[") 'scroll-down-line)
  (define-key map (kbd "]") 'scroll-up-line)
  (define-key map (kbd "x") 'exchange-point-and-mark)
  (define-key map (kbd "SPC") 'rectangle-mark-mode))

(defvar navigation-keymap (make-keymap)
  "Transient keymap for navigating buffers.")

;; Assign navigation-keys to the map
(define-navigation-keys navigation-keymap)

(defun navigation-keymap-eldoc-function ()
  (eldoc-message "Navigation Keymap"))

(defun navigation-keymap--activate ()
  (interactive)
  (pulse-line)
  (message "Navigation Keymap Activated")
  (add-function :before-until (local 'eldoc-documentation-function)
                #'navigation-keymap-eldoc-function)
  (set-transient-map navigation-keymap t 'navigation-keymap--deactivate))

(defun navigation-keymap--deactivate ()
  (interactive)
  (pulse-line)
  (message "Navigation Keymap Deactivated")
  (remove-function (local 'eldoc-documentation-function)
                   #'navigation-keymap-eldoc-function))

;;;; Selected

(select-package 'selected)

(selected-global-mode 1)

(defun disable-selected-minor-mode ()
  (selected-minor-mode -1))

(with-eval-after-load 'selected
  (define-keys selected-keymap
    ("u" 'upcase-dwim)
    ("d" 'downcase-dwim)
    ("c" 'capitalize-dwim)
    ("w" 'kill-ring-save)
    ("|" 'pipe-region)
    ("R" 'replace-rectangle)
    ("E" 'eval-region)
    ("q" 'selected-off))
   (define-navigation-keys selected-keymap)
   (blackout 'selected-minor-mode))


;;;; Transient

(select-package 'transient)

(autoload 'org-store-link "org")
(autoload 'dired-jump "dired" nil t)

(with-eval-after-load 'transient

  (transient-define-prefix oht-transient-general ()
    "General-purpose transient."
    [["Actions/Toggles"
      ("a" "AutoFill" auto-fill-mode)
      ("j" "Dired Jump" dired-jump)
      ("v" "View Mode" view-mode)
      ("b" "Switch Buffer" consult-buffer)
      ("B" "iBuffer" ibuffer)
      ("m" "Mode Transient..." call-mode-help-transient)]
     ["Transients"
      ("o" "Org..." oht-transient-general--org)
      ("t" "Toggle..." oht-transient-general--toggles)
      ("w" "Windows..." oht-transient-window)
      ("c" "Consult..." oht-transient-general--consult)]
     [""
      ("0" "Outline..." oht-transient-outline)
      ("2" "Secondary..." oht-transient-2nd)
      ("f" "Fonts..." oht-transient-fonts)
      ("s" "Spelling..." flyspell-mode-transient)]])

  (transient-define-prefix oht-transient-general--org ()
    "Transient for Org commands useful outside org mode."
    ["Org Mode"
     ["Agenda Commands"
      ("t" "Today" oht-org-agenda-today)
      ("p" "Today (pop-up)" oht-org-agenda-today-pop-up)
      ("0" "Complete" oht-org-agenda-complete)
      ("a" "Agenda..." org-agenda)]
     ["Other"
      ("k" "Capture" org-capture)
      ("s" "Store Link" org-store-link)]])

  (transient-define-prefix oht-transient-general--toggles ()
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    [["Toggle"
     ("h" "Highlight Line" hl-line-mode)
     ("l" "Line Numbers" global-display-line-numbers-mode)
     ("g" "Fill Column" global-display-fill-column-indicator-mode)
     ("w" "Wrap" visual-line-mode)
     ("t" "Truncate" toggle-truncate-lines)
     ("W" "Whitespace" whitespace-mode)]
     ["Action"
      ("q" "Quit" transient-quit-all)]])

  (transient-define-prefix oht-transient-general--consult ()
    ["Consult"
     ("l" "Line" consult-line)
     ("o" "Outline" consult-outline)
     ("g" "Grep" consult-grep)
     ("b" "Buffer" consult-buffer)
     ("a" "Apropos" consult-apropos)
     ("m" "Marks" consult-mark)
     ("M" "Minor Modes" consult-minor-mode-menu)])

  (transient-define-prefix oht-transient-fonts ()
    "Set Font Properties"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    [["Modes"
      ("v" "Var Mode" variable-pitch-mode)
      ("V" "V+ Mode" facedancer-vadjust-mode)
      ("o" "Olivetti" olivetti-mode)
      ("w" "Wrap" visual-line-mode)]
     ["Size"
      ("0" "Reset Size" text-scale-mode)
      ("=" "Larger" text-scale-increase)
      ("+" "Larger" text-scale-increase)
      ("-" "Smaller" text-scale-decrease)]
     ["Other"
      ("s" "Line Spacing" line-spacing-interactive)
      ("m" "Modus Toggle" modus-themes-toggle)]])

  (transient-define-prefix oht-transient-window ()
    "Most commonly used window commands"
    [["Splits"
      ("s" "Horizontal" split-window-below)
      ("v" "Vertical"   split-window-right)
      ("b" "Balance"    balance-windows)
      ("f" "Fit"        fit-window-to-buffer)
      ("r" "Rotate"     toggle-window-split)
      ("F" "Find Other Win" find-file-other-window)]
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
     ["Undo/Redo"
      ("C-/" "Winner Undo" winner-undo :transient t)
      ("M-/" "Winner Redo" winner-redo :transient t)]
     ["Exit"
      ("q" "Quit" transient-quit-all)]])

  (transient-define-prefix oht-transient-marks ()
    "Transient for setting the mark."
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-exit
    ["Mark"
     [("@" "Word" mark-word)
      ("s" "Sexp" mark-sexp)
      ("d" "Defun" mark-defun)]
     [("n" "Line" mark-line)
      (")" "Sentence" mark-sentence)
      ("}" "Paragraph" mark-paragraph)]
     [("<" "Beginning of Buffer" mark-beginning-of-buffer)
      (">" "End of Buffer" mark-end-of-buffer)]
     [("x" "Exchange Point/Mark" exchange-point-and-mark :transient nil)
      ("q" "Quit" transient-quit-all)]])

  (transient-define-prefix oht-transient-transpose ()
    "Transient for transpose commands."
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-exit
    ["Transpose"
     ["Forward"
      ("f" "Char" transpose-chars)
      ("@" "Word" transpose-words)
      ("n" "Line" transpose-lines)
      (")" "Sentence" transpose-sentences)
      ("}" "Paragraph" transpose-paragraphs)
      ("s" "Sexps" transpose-sexps)
      ("r" "Regions" transpose-regions)]])

  ) ; End transient config


;;;; Mode Help Transients

;; Emacs has so many modes. Who can remember all the commands? These
;; mode-specific transients are designed to help with that.

(defun call-mode-help-transient ()
  "Call a helpful transient based on the mode you're in."
  (interactive)
  (if (progn
        (when (derived-mode-p 'Info-mode)
          (info-mode-help-transient))
        (when (derived-mode-p 'dired-mode)
          (dired-mode-help-transient)))
      nil ; if the above succeeds, do nothing, else...
    (message "No transient defined for this mode.")))

(with-eval-after-load 'info
  (transient-define-prefix info-mode-help-transient ()
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

(with-eval-after-load 'dired
  (transient-define-prefix dired-mode-help-transient ()
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
      ("m" "Marks..." dired-mode-help-transient--marks)]])
  (transient-define-prefix dired-mode-help-transient--marks ()
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
  ) ; End dired config


;;;; Misc Packages

(when (eq system-type 'darwin)
  (select-package 'magit))

(select-package 'olivetti)
(with-eval-after-load 'olivetti
  (custom-set-variables '(olivetti-body-width 84)))

(select-package 'undo-fu)

(select-package 'unfill)
(global-set-key (kbd "M-q") 'until-toggle)

(select-package 'helpful)
(global-set-key (kbd "C-h f") 'helpful-function)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h o") 'helpful-symbol)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h p") 'helpful-at-point)

(select-package 'move-text)
(global-set-key (kbd "C-x C-t") 'oht-transient-transpose-lines)
(transient-define-prefix oht-transient-transpose-lines ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Move Lines"
    ("n" "Down" move-text-down)
    ("p" "Up" move-text-up)]])

(select-package 'visual-regexp)
(global-set-key [remap query-replace] 'vr/query-replace)

(select-package 'visual-regexp-steroids)
(with-eval-after-load 'visual-regexp
  (with-eval-after-load 'visual-regexp-steroids
    (custom-set-variables
     '(vr/engine 'pcre2el))))

(select-package 'fountain-mode)
(custom-set-variables
 '(fountain-add-continued-dialog nil)
 '(fountain-highlight-elements (quote (section-heading))))

(select-package 'markdown-mode)
(add-to-list 'magic-mode-alist
             '("%text" . markdown-mode))
(add-to-list 'auto-mode-alist
             '("\\.text" . markdown-mode))

(select-package 'lua-mode)

(when (string= (system-name) "shadowfax.local")
  (select-package 'oblique)
  (add-to-list 'load-path "~/home/src/oblique-strategies/")
  (autoload 'oblique-strategy "oblique")
  (setq initial-scratch-message (concat
                                 ";; Welcome to Emacs!\n;; This is the scratch buffer, for unsaved text and Lisp evaluation.\n"
                                 ";; Oblique Strategy: " (oblique-strategy) "\n\n")))

(select-package 'orgalist)
(add-hook 'git-commit-mode-hook 'orgalist-mode)


;;; End of init.el
