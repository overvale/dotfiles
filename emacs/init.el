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

;;;; User/Machine Settings

(when (eq system-type 'darwin)
  (defvar oht-dotfiles "~/home/dot/emacs/")
  (defvar oht-orgfiles "~/home/org/")
  (defvar user-downloads-directory "~/Downloads"))

(when (eq system-type 'windows-nt)
  (cd "~/")
  (defvar oht-dotfiles "~/.emacs.d/")
  (defvar oht-orgfiles "~/home/org/")
  (defvar user-downloads-directory "~/home/Downloads"))


;;;; Settings

(setq custom-file (make-temp-file "emacs-custom-"))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

(custom-set-variables
 '(delete-selection-mode nil)
 '(global-auto-revert-mode t)
 '(save-place-mode t)
 '(recentf-mode t)
 '(winner-mode t)
 '(show-paren-mode t)
 '(blink-cursor-mode nil)
 '(ring-bell-function #'ignore)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(minibuffer-depth-indicate-mode t)
 '(set-language-environment "UTF-8"))

(setq-default cursor-type 'box)
(setq visible-bell nil)
(setq-default indicate-empty-lines nil)
(setq frame-title-format '("%b"))
(setq uniquify-buffer-name-style 'forward)
(defalias 'yes-or-no-p 'y-or-n-p)

(blackout 'eldoc-mode)
(blackout 'emacs-lisp-mode "Elisp")
(blackout 'auto-fill-function " Fill")

(setq display-time-format " %Y-%m-%d  %H:%M"
      display-time-interval 60
      display-time-mail-directory nil
      display-time-default-load-average nil)

(column-number-mode t)
(display-time-mode t)

(setq vc-follow-symlinks t
      find-file-visit-truename t)
(setq create-lockfiles nil
      make-backup-files nil)
(setq bookmark-save-flag 1)
(setq load-prefer-newer t)
(setq delete-by-moving-to-trash t)
(setq confirm-kill-processes nil)
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t)

(when (eq system-type 'darwin)
  (setq locate-command "mdfind"
        trash-directory "~/.Trash/emacs"))

(setq split-window-keep-point nil)
;; (defadvice split-window-below (after split-window-below activate) (other-window 1))
;; (defadvice split-window-right (after split-window-right activate) (other-window 1))

(setq sentence-end-double-space nil)
(setq-default tab-width 4
              indent-tabs-mode nil
              fill-column 78)

;; The Mark
(setq set-mark-command-repeat-pop t
      mark-even-if-inactive nil)

;;;; Functions

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

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

(defun backward-delete-word ()
  "Delete word backward, without saving to kill ring."
  (interactive "*")
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(defun kill-to-beg-line ()
  "Kill from point to the beginning of the line."
  (interactive)
  (kill-line 0))

(defun pipe-region (start end command)
  ;; https://github.com/oantolin/emacs-config/blob/master/my-lisp/text-extras.el
  "Pipe region through shell command. If the mark is inactive,
pipe whole buffer."
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

(defun find-file-recursively ()
  "Find Files Recursively using completing read."
  (interactive)
  (find-file (completing-read "Find File Recursively: "
                              (directory-files-recursively default-directory ".+"))))

(defun comment-or-uncomment-region-dwim ()
  "Toggle comment for region, or line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun mark-whole-line ()
  "Put the point at end of this whole line, mark at beginning"
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

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

(when (eq system-type 'darwin)
  (setq youtube-dl-path "/usr/local/bin/youtube-dl")
  (setq youtube-dl-output-dir "~/Downloads/"))

(defun youtube-dl-URL-at-point ()
  "Send the URL at point to youtube-dl."
  (interactive)
  (async-shell-command (format "%s -o \"%s%s\" -f best \"%s\""
                               youtube-dl-path
                               youtube-dl-output-dir
                               "%(title)s.%(ext)s"
                               (ffap-url-at-point))))


;;;; Keybindings

;; If on a Mac, use the command key as Super, left-option for Meta, and
;; right-option for Alt.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-right-option-modifier 'nil))

;; If on Windows, use Windows key as Super
(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-]))

;; Mode Leader
(defvar oht-mode-leader "s-\\"
  "Defines leader key for mode-specific transients.
Borrows the concept of a 'leader key' from Vim so I can press
a single shortcut in any mode and get transient that I've defined
for that mode.")

;; Minor modes override global bindings, so any bindings you don't want
;; overridden should be placed in a minor mode. This technique is stolen from
;; the package bind-key.

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

;; Super Bindings
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-m") 'iconify-frame)
(global-set-key (kbd "s-n") 'make-frame-command)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-,") 'find-user-init-file)
(global-set-key (kbd "s-o") 'find-file)
(global-set-key (kbd "s-z") 'undo-fu-only-undo)
(global-set-key (kbd "s-Z") 'undo-fu-only-redo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-]") 'next-buffer)

(global-set-key (kbd "s-e") 'embark-act)
(global-set-key (kbd "s-b") 'consult-buffer)
(global-set-key (kbd "s-f") 'consult-line)
(global-set-key (kbd "s-<return>") 'oht-transient-general)
(global-set-key (kbd "s-w") 'oht-transient-window)
(global-set-key (kbd "s-2") 'oht-transient-2nd)
(global-set-key (kbd "s-d") 'oht-transient-dispatch)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(define-key bosskey-mode-map (kbd "<C-return>") 'execute-extended-command)
(define-key bosskey-mode-map (kbd "M-o") 'other-window)

(global-set-key (kbd "M-<tab>") 'hippie-expand)
(global-set-key (kbd "M-s-s")   'save-some-buffers)
(global-set-key (kbd "M-c")     'capitalize-dwim)
(global-set-key (kbd "M-l")     'downcase-dwim)
(global-set-key (kbd "M-u")     'upcase-dwim)
(global-set-key (kbd "M-SPC")   'cycle-spacing)
(global-set-key (kbd "M-z")     'zap-to-char)
(global-set-key (kbd "M-Z")     'zap-up-to-char)
(global-set-key (kbd "C-x C-x") 'exchange-point-and-mark-dwim)
(global-set-key (kbd "C-M-k")   'kill-to-beg-line)


;;;; Visual Line Mode, Fixes

;; `visual-line-mode', `word-wrap', and `truncate-lines' all do different
;; things. `visual-line-mode' is a wrapper around a bunch of things, probably
;; best explained here: http://ergoemacs.org/emacs/emacs_long_line_wrap.html
;; `word-wrap' ONLY wraps lines word-wise instead of character-wise.
;; `truncate-lines' ONLY controls if wrapping happens at all.
(setq-default truncate-lines t)

;; When visual-line-mode is off and truncate-lines is toggled off, I still
;; want wrapping to happen at the word instead of character.
(setq-default word-wrap 1)

;; Turning on `visual-line-mode' binds "C-a" to `beginning-of-visual-line'.
;; This is inconsistent with macOS behavior, which is that "C-a" always goes
;; to the beginning of the logical line and "s-<left>" goes to the beginning
;; of the visual line.
;; (define-key bosskey-mode-map (kbd "C-a") 'beginning-of-line)
;; (define-key bosskey-mode-map (kbd "C-e") 'end-of-line)
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-<left>") 'beginning-of-visual-line)
  (global-set-key (kbd "s-<right>") 'end-of-visual-line))

;; Additionally, in visual-line-mode the function kill-line kills to the end
;; of the VISUAL line, which is inconsistent with Mac OS, and my own
;; expectations. Therefore we need to remap it to a function which kills to
;; the end of the whole line.
(defun kill-rest-whole-line ()
  "Kill the rest of the whole line."
  (interactive)
  (kill-line nil))
;;(global-set-key (kbd "C-k") 'kill-rest-whole-line)


;;;; Mouse

;; Gasp! An Emacs user that actually uses the mouse?! Scandalous.

;; Start by making shift-click extend the selection (region)
(global-set-key [S-down-mouse-1] 'ignore)
(global-set-key [S-mouse-1] 'mouse-save-then-kill)

;; s-click to split windows at that exact spot
(global-set-key [s-mouse-1] 'mouse-split-window-horizontally)
(global-set-key [S-s-mouse-1] 'mouse-split-window-vertically)

;; Delete a window with M-s--click
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


;;; Package Management

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archive-priorities '(("gnu" . 20)("melpa" . 10)))

;; On first run, with no packages installed, Emacs will stop loading this file
;; as soon as it encounters code from a package not yet installed. So, when
;; setting up a new machine, it is important for the very first thing to be a
;; listing of all the packages you'd like to install so you can install all
;; the packages you need with `package-install-selected-packages'.
(setq package-selected-packages
      '(bicycle
        buffer-move
        consult
        embark
        embark-consult
        fountain-mode
        hackernews
        helpful
        isearch-mb
        marginalia
        markdown-mode
        modus-themes
        move-text
        olivetti
        orderless
        selected
        transient
        undo-fu
        unfill
        vertico
        visual-regexp
        visual-regexp-steroids
        blackout
        lua-mode
        use-package))

(when (string= (system-name) "shadowfax.local")
  (add-to-list 'package-selected-packages 'elfeed))

(when (eq system-type 'darwin)
  (add-to-list 'package-selected-packages 'magit))

;; Install packages with `package-install-selected-packages', remove packages
;; with `package-autoremove'. Both functions look at the variable
;; `package-selected-packages' for the canonical list of packages.

;; You can automatically remove anything not in `package-selected-packages'
;; (thus not in this init file) by un-commenting this hook:
;; (add-hook 'emacs-startup-hook 'package-autoremove)

;; Use-Package, Blackout, Transient
;; This config requires these 3 packages to run properly.

(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer t
      use-package-hook-name-suffix nil)

(autoload 'blackout "blackout" nil t)
(autoload 'transient-define-prefix "transient" nil t)


;;; Built-In Packages & Lisp

;;;; Flyspell

(when (eq system-type 'darwin)
  (custom-set-variables
   '(ispell-program-name "/usr/local/bin/aspell")
   '(ispell-extra-args '("--sug-mode=ultra"))
   '(ispell-list-command "list")))

(eval-after-load 'flyspell
  '(blackout 'flyspell-mode " Spell"))

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-set-key (kbd "M-;") #'flyspell-auto-correct-previous-word)

(transient-define-prefix oht-transient-spelling ()
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


;;;; Facedancer Mode

;; Facedancer's goal is to make customizing Emacs' fonts easy.
;; facedancer-font-set allows you to set the global fonts.
;; facedancer-mode allows you to set per-buffer fonts.
;; facedancer-vadjust-mode allows optical adjustment of the variable-pitch height.


;;;;; Variables

(defvar facedancer-monospace "Courier"
  "Monospace font to be used for the default and fixed-pitch faces.")

(defvar facedancer-variable "Georgia"
  "Variable font to to used for the variable-pitch face.")

(defvar facedancer-monospace-size 12
  "Font size, as an integer, to be used for the default and fixed-pitch sizes.

This value will be multiplied by 10, so 12 will become 120. This is to comply
with Emacs' set-face-attribute requirements.")

(defvar facedancer-variable-size 14
  "Font point size, as an integer, to be used for the variable-pitch size.

This value will be used to determine a relative (float) size based on the size
of facedancer-monospace. So if your monospace size is 12 and your variable size
is 14 the derived size will be 1.16.")

(defvar facedancer-monospace-weight 'normal
  "Weight of both the default and fixed-pitch faces.")

(defvar facedancer-variable-weight 'normal
  "Weight of the variable-pitch face.")

(defvar facedancer-monospace-width 'normal
  "Width of both the default and fixed-pitch faces.")

(defvar facedancer-variable-width 'normal
  "Width of the variable-pitch face.")


;;;;; Functions

(defun facedancer-line-spacing (&optional arg)
  "Interactive wrapper around line-spacing var. Takes an argument, 0 by default."
  (interactive "P")
  (setq-local line-spacing arg))

(defun facedancer-font-set ()
  "Function for globally setting the default, variable-, and fixed-pitch faces.

All three faces will be set to exactly the same size, with the variable-
and fixed-pitch faces as relative (float) sizes. This allows
text-scale-adjust to work correctly."
  (interactive)
  (set-face-attribute 'default nil
                      :family facedancer-monospace
                      :weight facedancer-monospace-weight
                      :width  facedancer-monospace-width
                      :height (* facedancer-monospace-size 10))
  (set-face-attribute 'fixed-pitch nil
                      :family facedancer-monospace
                      :weight facedancer-monospace-weight
                      :width  facedancer-monospace-width
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family facedancer-variable
                      :weight facedancer-variable-weight
                      :width  facedancer-variable-width
                      :height 1.0))


;;;;; Facedancer Mode

;; There are a number of built-in functions for dealing with setting
;; per-buffer fonts, but all of them are built on buffer-face-mode, which
;; works by remapping ONLY the default face to a new value. If you'd like to
;; remap specific faces (for example the variable-pitch face)
;; buffer-face-mode won't cut it. The below approach applies the exact same
;; approach as buffer-face-mode but allows you to target individual faces.

(define-minor-mode facedancer-mode
  "Local minor mode for setting custom fonts per buffer.

Reads the following variables, all are optional.

VARIABLE                      DEFAULT VALUE
---------------------------   -------------
facedancer-monospace          Monaco
facedancer-variable           Georgia
facedancer-monospace-size     12
facedancer-variable-size      14
facedancer-monospace-weight   normal
facedancer-variable-weight    normal
facedancer-monospace-width    normal
facedancer-variable-width     normal

To use, create a function which sets the variables locally, then
call that function with a hook, like so:

    (defun my/custom-elfeed-fonts ()
      (setq-local facedancer-monospace \"Iosevka\"
                  facedancer-variable  \"Inter\")
      (facedancer-mode 'toggle))

    (add-hook 'elfeed-show-mode 'my/custom-elfeed-fonts)"
  :init-value nil
  :lighter " FaceD"
  (if facedancer-mode
      (progn
        (setq-local facedancer-default-remapping
                    (face-remap-add-relative 'default
                                             :family facedancer-monospace
                                             :weight facedancer-monospace-weight
                                             :width  facedancer-monospace-width
                                             :height (/ (float facedancer-monospace-size)
                                                        (float facedancer-variable-size))))
        (setq-local facedancer-fixed-pitch-remapping
                    (face-remap-add-relative 'fixed-pitch
                                             :family facedancer-monospace
                                             :weight facedancer-monospace-weight
                                             :width  facedancer-monospace-width
                                             :height (/ (float facedancer-monospace-size)
                                                        (float facedancer-variable-size))))
        (setq-local facedancer-variable-pitch-remapping
                    (face-remap-add-relative 'variable-pitch
                                             :family facedancer-variable
                                             :weight facedancer-variable-weight
                                             :width  facedancer-variable-width
                                             :height (/ (float facedancer-variable-size)
                                                        (float facedancer-monospace-size))))
        (force-window-update (current-buffer)))
    (progn
      (face-remap-remove-relative facedancer-default-remapping)
      (face-remap-remove-relative facedancer-fixed-pitch-remapping)
      (face-remap-remove-relative facedancer-variable-pitch-remapping)
      (force-window-update (current-buffer)))))


;;;;; Facedancer Variable Adjust Mode

(define-minor-mode facedancer-vadjust-mode
  "Minor mode to adjust the variable-pitch face size buffer-locally.

A minor mode to scale (in the current buffer) the variable-pitch
face up to the height defined by ‘facedancer-variable-size’ and
the fixed-pitch face down to the height defined by
‘facedancer-monospace-size’."
  :init-value nil
  :lighter " V+"
  (if facedancer-vadjust-mode
      (progn
        (setq-local variable-pitch-remapping
                    (face-remap-add-relative 'variable-pitch
                                             :height (/ (float facedancer-variable-size)
                                                        (float facedancer-monospace-size))))
        (setq-local fixed-pitch-remapping
                    (face-remap-add-relative 'fixed-pitch
                                             :height (/ (float facedancer-monospace-size)
                                                        (float facedancer-variable-size))))
        (force-window-update (current-buffer)))
    (progn
      (face-remap-remove-relative variable-pitch-remapping)
      (face-remap-remove-relative fixed-pitch-remapping)
      (force-window-update (current-buffer)))))

;; Add a hook which enables facedancer-vadjust-mode when buffer-face-mode
;; activates.
(add-hook 'buffer-face-mode-hook (lambda () (facedancer-vadjust-mode 'toggle)))


;;;; Dispatch

;; Almost like an "app launcher" inside Emacs. These are the most common
;; things I do in Emacs.

(defun oht-dispatch-downloads () (interactive) (find-file "~/Downloads"))
(defun oht-dispatch-reading () (interactive) (find-file "~/Downloads/reading"))
(defun oht-dispatch-watch () (interactive) (find-file "~/Downloads/watch"))
(defun oht-dispatch-NPR-news () (interactive) (browse-url "https://text.npr.org"))
(defun oht-dispatch-CNN-news () (interactive) (browse-url "https://lite.cnn.com/en"))
(defun oht-dispatch-google-news () (interactive) (browse-url "http://68k.news/"))

(transient-define-prefix oht-transient-dispatch ()
  "Jump directly to your most-used stuff."
  ["Work"
   [("t" "Today + Priority" oht-org-agenda-today)
    ("p" "Today + Priority (pop-up)" oht-org-agenda-today-pop-up)
    ("0" "Week + TODOs" oht-org-agenda-complete)
    ("a" "Agenda" oht-org-agenda-agenda)
    ("T" "TODOs" oht-org-agenda-todos)
    ("A" "Org Agenda Command..." org-agenda)]]
  ["Browsing"
   [("e" "Elfeed"      elfeed)
    ("h" "Hacker News" hackernews)]
   [("E" "EWW"         eww)
    ("n" "NPR News"    oht-dispatch-NPR-news)
    ("c" "CNN News"    oht-dispatch-CNN-news)
    ("g" "Google News" oht-dispatch-google-news)]
   [("d" "Downloads"   oht-dispatch-downloads)
    ("r" "Reading"     oht-dispatch-reading)
    ("w" "Watch"       oht-dispatch-watch)]])


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

(defun exit-with-top-completion ()
  "Exit minibuffer with top completion candidate."
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (unless (test-completion content
                             minibuffer-completion-table
                             minibuffer-completion-predicate)
      (when-let ((completions (completion-all-sorted-completions)))
        (delete-minibuffer-contents)
        (insert
         (concat
          (substring content 0 (or (cdr (last completions)) 0))
          (car completions)))))
    (exit-minibuffer)))

(define-key minibuffer-local-completion-map          (kbd "<return>") 'exit-with-top-completion)
(define-key minibuffer-local-must-match-map          (kbd "<return>") 'exit-with-top-completion)
(define-key minibuffer-local-filename-completion-map (kbd "<return>") 'exit-with-top-completion)

;; I want to use my usual 's-o' binding (other-window) to switch between the
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

;; isearch-mb allows you to edit the isearch in the minibuffer. Lovely.
(isearch-mb-mode)


;;;; Outline

;; `outline' provides major and minor modes for collapsing sections of a
;; buffer into an outline-like format. Let's turn that minor mode into a
;; global minor mode and enable it.
(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)
(global-outline-minor-mode +1)

(blackout 'outline-minor-mode)

;;;; Pulse

(defun pulse-line (&rest _)
  "Interactive function to pulse the current line."
  (interactive)
  (pulse-momentary-highlight-one-line (point)))

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

(defadvice other-window (after other-window-pulse activate) (pulse-line))
(defadvice delete-window (after delete-window-pulse activate) (pulse-line))
(defadvice recenter-top-bottom (after recenter-top-bottom-pulse activate) (pulse-line))

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

(with-eval-after-load 'ibuffer

  (setq ibuffer-show-empty-filter-groups nil)

  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Read"  (or (mode . eww-mode)
                        (mode . elfeed-search-mode)
                        (mode . elfeed-show-mode)
                        (mode . hackernews-mode)))
           ("Org"   (or (mode . org-mode)
                        (mode . org-agenda-mode)))
           ("Dired" (mode . dired-mode))
           ("ELisp" (mode . emacs-lisp-mode))
           ("Help"  (or (name . "\*Help\*")
                        (name . "\*Apropos\*")
                        (name . "\*Info\*"))))))

  (defun oht-ibuffer-hook ()
    (hl-line-mode 1)
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default"))

  (add-hook 'ibuffer-mode-hook 'oht-ibuffer-hook)

  ) ; End ibuffer


;;;; Hippie Expand

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;;;; Info

(with-eval-after-load 'info

  (define-key Info-mode-map (kbd oht-mode-leader) 'oht-transient-info)

  (transient-define-prefix oht-transient-info ()
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

;;;; Dired

(with-eval-after-load 'dired

  (setq dired-use-ls-dired nil) ; no more warning message

  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "open" nil 0 nil file)
      (message "Opening %s done" file)))

  (define-key dired-mode-map (kbd oht-mode-leader) 'oht-transient-dired)
  (define-key dired-mode-map (kbd "O") 'dired-open-file)
  (define-key dired-mode-map (kbd "C-/") 'dired-undo)

  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode 1)
              (auto-revert-mode)
              (hl-line-mode 1)))

  (transient-define-prefix oht-transient-dired ()
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
      ("m" "Marks..." oht-transient-dired-marks)]])

  (transient-define-prefix oht-transient-dired-marks ()
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


;;; Appearance

(use-package modus-themes
  :custom
  (modus-themes-slanted-constructs t)
  (modus-themes-links 'faint-neutral-underline)
  (modus-themes-mode-line 'accented)
  (modus-themes-region 'bg-only)
  (modus-themes-diffs 'desaturated)
  (modus-themes-org-blocks 'grayscale)
  :init
  (modus-themes-load-operandi))

;; If on a Mac, assume Mitsuharu Yamamoto’s fork -- check for dark/light mode,
;; if dark mode load the dark theme, also add a hook for syncing with the
;; system.
(when (eq system-type 'darwin)
  (if (string= (plist-get (mac-application-state) :appearance) "NSAppearanceNameDarkAqua")
      (modus-themes-load-vivendi))
  (add-hook 'mac-effective-appearance-change-hook 'modus-themes-toggle))

(setq-default line-spacing 1)
(setq text-scale-mode-step 1.09)

(when (eq system-type 'darwin)
  ;; config
  (setq facedancer-monospace "SF Mono"
        facedancer-variable  "New York"
        facedancer-monospace-size 12
        facedancer-variable-size  14)
  ;; set faces
  (facedancer-font-set)
  ;; customize mode-line
  (set-face-attribute 'mode-line nil          :family "SF Compact Text" :height 130)
  (set-face-attribute 'mode-line-inactive nil :family "SF Compact Text" :height 130))

(when (eq system-type 'windows-nt)
  ;; config
  (setq facedancer-monospace "Consolas"
        facedancer-variable  "Calibri"
        facedancer-monospace-size 10
        facedancer-variable-size  11)
  ;; set faces
  (facedancer-font-set)
  ;; customize mode-line
  (set-face-attribute 'mode-line nil          :family "Calibri" :height 110)
  (set-face-attribute 'mode-line-inactive nil :family "Calibri" :height 110))

(defun facedancer-prot-fonts ()
  (interactive)
  (setq-local facedancer-monospace "Iosevka Comfy"
              facedancer-variable  "Inter"
              line-spacing nil
              facedancer-monospace-size 13
              facedancer-variable-size  13)
  (facedancer-mode 'toggle))


;;; External Packages

;;;; Narrowing & Searching

(use-package orderless
  :demand
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :init (vertico-mode)
  :config
  (advice-add #'vertico--setup :after
              (lambda (&rest _)
                (setq-local completion-auto-help nil
                            completion-show-inline-help nil))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (:map embark-file-map
        ("O" . macos-open-file)
        ("j" . dired-jump))
  (:map embark-url-map
        ("d" . youtube-dl-URL-at-point)
        ("&" . browse-url-default-macosx-browser)))

(use-package consult
  :bind
  ([remap yank-pop] . consult-yank-pop)
  :custom
  (consult-preview-key (kbd "C-="))
  (consult-config
   `((consult-mark :preview-key any))))

(use-package embark-consult
  :after (embark consult)
  :demand t)


;;;; Org

(use-package org
  :commands (org-mode oht-org-agenda-today)
  :config

  (define-key org-mode-map (kbd oht-mode-leader) 'oht-transient-org)

  (custom-set-variables
   '(org-list-allow-alphabetical t)
   '(org-enforce-todo-dependencies t)
   '(org-enforce-todo-checkbox-dependencies t)
   '(org-log-done 'time)
   '(org-log-into-drawer t))

  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t
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
          ("i" "Ingenuity")
          ("ii" "Ingenuity Inbox" entry
           (file+headline ,(concat oht-orgfiles "ingenuity.org") "Inbox")
           "* %?\n\n" :empty-lines 1)
          ("il" "Ingenuity Log Entry" entry
           (file ,(concat oht-orgfiles "ingenuity_logbook.org"))
           "* %? %T\n\n" :empty-lines 1)
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

  (defun oht/org-insert-date-today ()
    "Insert today's date using standard org formatting."
    (interactive)
    (org-insert-time-stamp (current-time)))

  (defun oht/org-insert-date-today-inactive ()
    "Inserts today's date in org inactive format."
    (interactive)
    (insert (format-time-string "\[%Y-%m-%d %a\]")))

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
      ("." "Insert Date, Active" oht/org-insert-date-today)
      (">" "Insert Date, Inactive" oht/org-insert-date-today-inactive)
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

  ) ; End "use-package org"


(use-package org-agenda
  :commands org-agenda
  :bind
  (:map org-agenda-mode-map
        ("t" . oht-transient-org-agenda)
        ("C-/" . org-agenda-undo))
  :hook (org-agenda-mode-hook . hl-line-mode)
  :config
  (transient-define-prefix oht-transient-org-agenda ()
    "A transient for setting org-agenda todo status.
I've created this because I don't like how org-todo messes with
windows. There is likely a much better way to automatically map
org-todo-keywords to a transient command."
    ["Change Status To..."
     [("t" "TODO"     org-agenda-todo-set-todo)
      ("l" "LATER"    org-agenda-todo-set-later)]
     [("d" "DONE"     org-agenda-todo-set-done)
      ("c" "CANCELED" org-agenda-todo-set-canceled)]]))


;;;; View / Selected

(use-package view
  :custom
  (view-read-only t)
  :init
  (defun oht/view-mode-exit ()
    (interactive)
    (view-mode -1)
    (hl-line-mode -1))
  (defun oht/exit-view-replace-rectangle ()
    (interactive)
    (oht/view-mode-exit)
    (call-interactively 'replace-rectangle))
  :bind
  (:map view-mode-map
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
        ("q" . quit-window))
  :hook (view-mode-hook . hl-line-mode)
  :blackout " VIEW")

(use-package selected
  :commands selected-minor-mode
  :init
  (selected-global-mode 1)
  (defun disable-selected-minor-mode ()
    (selected-minor-mode -1))
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
              ("l" . mark-whole-line)
              ("|" . pipe-region)
              ("R" . replace-rectangle)
              ("E" . eval-region)
              ("q" . selected-off))
  :blackout selected-minor-mode)


;;;; Browser & News

(setq shr-max-image-proportion 0.5)
(setq shr-width 80)
(setq shr-bullet "• ")

;; Set default browser in Emacs
(setq browse-url-browser-function 'eww-browse-url)
;; Prefixing with universal argument uses browse-url-default-browser
;; which seems to be the system browser.

(use-package eww
  :custom
  (eww-restore-desktop nil)
  (eww-desktop-remove-duplicates t)
  (eww-header-line-format "%t %u")
  (eww-download-directory user-downloads-directory)
  (eww-bookmarks-directory (concat user-emacs-directory "eww-bookmarks/"))
  (eww-history-limit 150)
  (eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/pdf\\)")
  (url-cookie-trusted-urls '()
                           url-cookie-untrusted-urls '(".*"))
  :init
  (defun oht-eww-fonts ()
    "Apply some customization to fonts in eww-mode."
    (facedancer-vadjust-mode)
    (text-scale-increase 1)
    (setq-local line-spacing 2))
  :commands (eww)
  :hook (eww-mode-hook . oht-eww-fonts)
  :config

  (define-key eww-mode-map (kbd oht-mode-leader) 'oht-transient-eww)

  (make-variable-buffer-local
   (defvar eww-inhibit-images-status nil
     "EWW Inhibit Images Status"))

  (defun eww-inhibit-images-toggle ()
    (interactive)
    (setq eww-inhibit-images-status (not eww-inhibit-images-status))
    (if eww-inhibit-images-status
        (progn (setq-local shr-inhibit-images t)
               (eww-reload t))
      (progn (setq-local shr-inhibit-images nil)
             (eww-reload t))))

  (defun oht-eww-open-in-new-buffer-bury ()
    "Open URL at point in a new buried buffer"
    (interactive)
    (eww-open-in-new-buffer)
    (quit-window)
    (message "Browsing in buried buffer"))

  (defun prot-eww--rename-buffer ()
    "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
    (let ((name (if (eq "" (plist-get eww-data :title))
                    (plist-get eww-data :url)
                  (plist-get eww-data :title))))
      (rename-buffer (format "*eww # %s*" name) t)))

  (add-hook 'eww-after-render-hook #'prot-eww--rename-buffer)
  (advice-add 'eww-back-url :after #'prot-eww--rename-buffer)
  (advice-add 'eww-forward-url :after #'prot-eww--rename-buffer)

  (transient-define-prefix oht-transient-eww ()
    "Transient for EWW"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["EWW"
     ["Actions"
      ("G" "Browse" eww)
      ("M-<return>" "Open in new buffer" oht-eww-open-in-new-buffer-bury)
      ("&" "Browse With External Browser" eww-browse-with-external-browser)
      ("w" "Copy URL" eww-copy-page-url)]
     ["Display"
      ("i" "Toggle Images" eww-inhibit-images-toggle)
      ("F" "Toggle Fonts" eww-toggle-fonts)
      ("R" "Readable" eww-readable)
      ("M-C" "Colors" eww-toggle-colors)]
     ["History"
      ("H" "History" eww-list-histories)
      ("l" "Back" eww-back-url)
      ("r" "Forward" eww-forward-url)]
     ["Bookmarks"
      ("a" "Add Eww Bookmark" eww-add-bookmark)
      ("b" "Bookmark" bookmark-set)
      ("B" "List Bookmarks" eww-list-bookmarks)
      ("M-n" "Next Bookmark" eww-next-bookmark)
      ("M-p" "Previous Bookmark" eww-previous-bookmark)]])

  ) ; End "use-package eww"

(use-package elfeed
  :if (string= (system-name) "shadowfax.local")
  :commands elfeed
  :init
  (defun oht-elfeed-show-fonts ()
    "Apply some customization to fonts in elfeed-show-mode."
    (facedancer-vadjust-mode)
    (setq-local line-spacing 3))
  :hook (elfeed-show-mode-hook . oht-elfeed-show-fonts)
  :custom
  (elfeed-use-curl t)
  (elfeed-curl-max-connections 10)
  (elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (elfeed-enclosure-default-dir user-downloads-directory)
  (elfeed-search-filter "@4-week-ago +unread")
  (elfeed-sort-order 'descending)
  (elfeed-search-clipboard-type 'CLIPBOARD)
  (elfeed-show-truncate-long-urls t)
  :bind
  (:map elfeed-search-mode-map
        ("b" . elfeed-search-browse-url)
        ("B" . oht-elfeed-search-browse-and-bury)
        ("*" . elfeed-search-tag--star)
        ("8" . elfeed-search-untag--star)
        ("o" . delete-other-windows)
        ("N" . oht-elfeed-unread-news)
        ("E" . oht-elfeed-unread-emacs)
        ("O" . oht-elfeed-unread-other)
        ("S" . oht-elfeed-starred))
  (:map elfeed-show-mode-map
        ("&" . oht-elfeed-show-visit-generic)
        ("r" . elfeed-show-tag--read)
        ("u" . elfeed-show-tag--unread)
        ("*" . elfeed-show-tag--star)
        ("8" . elfeed-show-tag--unstar)
        ("o" . delete-other-windows)
        ("d" . oht-elfeed-show-download-video)
        ("i" . elfeed-inhibit-images-toggle)
        ("B" . oht-elfeed-show-browse-and-bury))
  :config
  ;; My feed list is stored outside my dotfiles -- not public.
  (load "~/home/src/rss-feeds.el")

  ;; Elfeed doesn't have a built-in way of flagging or marking items for later,
  ;; but it does have tags, which you can use for this. The below is some simple
  ;; aliases for adding and removing the 'star' tag.
  (defalias 'elfeed-search-tag--star
    (elfeed-expose #'elfeed-search-tag-all 'star)
    "Add the 'star' tag to all selected entries")
  (defalias 'elfeed-search-untag--star
    (elfeed-expose #'elfeed-search-untag-all 'star)
    "Remove the 'star' tag to all selected entries")
  (defalias 'elfeed-show-tag--star
    (elfeed-expose #'elfeed-show-tag 'star)
    "Add the 'star' tag to current entry")
  (defalias 'elfeed-show-tag--unstar
    (elfeed-expose #'elfeed-show-untag 'star)
    "Remove the 'star' tag to current entry")

  ;; Even though there are bindings for marking items as 'read' and 'unread' in
  ;; the search-mode, there are no such built-in bindings for show-mode. So we
  ;; add them here.
  (defalias 'elfeed-show-tag--unread
    (elfeed-expose #'elfeed-show-tag 'unread)
    "Mark the current entry unread.")
  (defalias 'elfeed-show-tag--read
    (elfeed-expose #'elfeed-show-untag 'unread)
    "Mark the current entry read.")

  (defun oht-elfeed-unread-news  () (interactive) (elfeed-search-set-filter "+unread +news"))
  (defun oht-elfeed-unread-emacs () (interactive) (elfeed-search-set-filter "+unread +emacs"))
  (defun oht-elfeed-unread-other () (interactive) (elfeed-search-set-filter "+unread -emacs -news"))
  (defun oht-elfeed-starred      () (interactive) (elfeed-search-set-filter "+star"))

  (defun oht-elfeed-show-visit-generic ()
    "Wrapper for elfeed-show-visit to use system browser instead of eww"
    (interactive)
    (let ((browse-url-generic-program "/usr/bin/open"))
      (elfeed-show-visit t)))

  (make-variable-buffer-local
   (defvar elfeed-inhibit-images-status nil
     "Elfeed Inhibit Images Status"))

  (defun elfeed-inhibit-images-toggle ()
    "Toggle display of images in elfeed-show."
    (interactive)
    (setq elfeed-inhibit-images-status (not elfeed-inhibit-images-status))
    (if elfeed-inhibit-images-status
        (progn
          (setq-local shr-inhibit-images t)
          (elfeed-show-refresh))
      (progn
        (setq-local shr-inhibit-images nil)
        (elfeed-show-refresh))))

  (defun oht-elfeed-show-download-video ()
    "In elfeed-show-mode, download a video using youtube-dl."
    (interactive)
    (async-shell-command (format "%s -o \"%s%s\" -f mp4 \"%s\""
                                 youtube-dl-path
                                 youtube-dl-output-dir
                                 "%(title)s.%(ext)s"
                                 (elfeed-entry-link elfeed-show-entry))))

  (defun oht-elfeed-search-browse-and-bury ()
    "Browse elfeed entry and bury buffer."
    (interactive)
    (elfeed-search-browse-url)
    (bury-buffer)
    (message "Browsing in buried buffer"))

  (defun oht-elfeed-show-browse-and-bury ()
    "Browse elfeed entry and bury buffer."
    (interactive)
    (elfeed-show-visit)
    (bury-buffer)
    (message "Browsing in buried buffer"))

  ) ; End "use-package elfeed"

(add-hook 'elfeed-search-mode-hook 'disable-selected-minor-mode)
(add-hook 'elfeed-show-mode-hook 'disable-selected-minor-mode)

(use-package hackernews
  :commands hackernews
  :bind
  (:map hackernews-mode-map
        ("o" . delete-other-windows))
  :custom
  (hackernews-items-per-page 30)
  (hackernews-default-feed 'top))


;;;; Transient

;; I LOVE transient commands. Basically, I don't want to learn a lot of
;; keybindings, I want a simple set of bindings that reveal the vast power
;; at your fingertips. So any time I find myself using a mode/package that I
;; can't remember the bindings/commands for I take the time to comb through
;; the source code, find anything I think I might use, and pop it in a
;; transient.

(use-package transient
  :init
  ;; Any commands these transients use, whose packages are potentially not
  ;; loaded yet, need to be autoloaded.
  (autoload 'org-store-link "org")
  (autoload 'dired-jump "dired" nil t)
  :custom
  (transient-mode-line-format 'line)
  (transient-display-buffer-action '(display-buffer-below-selected))
  :config

  (transient-define-prefix oht-transient-general ()
    "General-purpose transient.
I use this transient command as a jumping-off point. Many of the
more following specific transients are included here. The idea is
that, when lost, one can simply call this one transient and get
wherever you need to go."
    ["General"
     ["Quick Actions!"
      ("f" "Find File" find-file)
      ("b" "Switch Buffer" consult-buffer)
      ("B" "iBuffer" ibuffer)
      ("k" "Org Capture" org-capture)
      ("K" "Kill Buffer" kill-this-buffer)
      ("o" "Consult Outline" consult-outline)
      ("a" "AutoFill" auto-fill-mode)
      ("j" "Dired Jump" dired-jump)
      ("s" "Store Org Link" org-store-link)
      ("v" "View Mode" view-mode)]
     ["Consult"
      ("c a" "Consult Apropos" consult-apropos)
      ("c m" "Consult Mode Commands" consult-mode-command)
      ("c g" "Consult Grep" consult-grep)]
     ["Windows"
      ("w" "Window Transient..." oht-transient-window)
      ("0" "Kill Window" delete-window)
      ("1" "Only Window" delete-other-windows)
      ("2" "Split Below" split-window-below)
      ("3" "Split Right" split-window-right)]
     ["Transients"
      ("O" "Outline Navigation..." oht-transient-outline)
      ("D" "Display..."   oht-transient-display)
      ("F" "Fonts..." oht-transient-fonts)
      ("T" "Tabs..."      oht-transient-tabs)
      ("S" "Spelling..." oht-transient-spelling)
      ("H" "Helpful Commands..." oht-transient-help)
      ("M" "Toggle Minor Modes" consult-minor-mode-menu)]])

  (transient-define-prefix oht-transient-outline ()
    "Transient for Outline Minor Mode navigation"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["General -> Outline Navigation"
     ["Show/Hide"
      ("<backtab>" "Global Toggle" bicycle-cycle-global)
      ("<tab>" "Toggle Children" bicycle-cycle)
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

  (transient-define-prefix oht-transient-display ()
    "A transient for controlling Emacs display"
    ["General -> Display"
     [("h" "Highlight Line" hl-line-mode)
      ("l" "Line Numbers" global-display-line-numbers-mode)
      ("g" "Fill Column" global-display-fill-column-indicator-mode)
      ("w" "Wrap" visual-line-mode)
      ("t" "Truncate" toggle-truncate-lines)
      ("W" "Whitespace" whitespace-mode)]])

  (transient-define-prefix oht-transient-fonts ()
    "Set Font Properties"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["General -> Fonts"
     ["Modes"
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
      ("s" "Line Spacing" facedancer-line-spacing)
      ("m" "Modus Toggle" modus-themes-toggle)]])

  (transient-define-prefix oht-transient-tabs ()
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["General -> Tabs"
     [("t" "Tab Bar Mode" tab-bar-mode)
      ("n" "New" tab-bar-new-tab)
      ("k" "Kill" tab-bar-close-tab)
      ("z" "Undo Kill" tab-bar-undo-close-tab)
      ("]" "Next" tab-bar-switch-to-next-tab)
      ("[" "Previous" tab-bar-switch-to-prev-tab)]])

  (transient-define-prefix oht-transient-help ()
    "Transient for Helpful commands"
    ["General -> Helpful Commands"
     [("p" "At Point" helpful-at-point)]
     [("c" "Callable" helpful-callable)
      ("f" "Function" helpful-function)
      ("C" "Command" helpful-command)
      ("v" "Variable" helpful-variable)
      ("s" "Symbol" helpful-symbol)
      ("M" "Macro" helpful-macro)
      ("k" "Key" helpful-key)
      ("m" "Mode" helpful-mode)]
     [("u" "Update" helpful-update)
      ("V" "Visit Reference" helpful-visit-reference)
      ("K" "Kill Helpful Buffers" helpful-kill-buffers)]])

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
     ["Move"
      ("S-<left>"  "Move ←" buf-move-left  :transient t)
      ("S-<right>" "Move →" buf-move-right :transient t)
      ("S-<up>"    "Move ↑" buf-move-up    :transient t)
      ("S-<down>"  "Move ↓" buf-move-down  :transient t)]
     ["Undo/Redo"
      ("C-/" "Winner Undo" winner-undo :transient t)
      ("M-/" "Winner Redo" winner-redo :transient t)]])

  ) ; End "use-package transient"


;;;; Misc Packages

(use-package magit
  :if (when (eq system-type 'darwin))
  :commands magit-status)

(use-package olivetti
  :commands olivetti-mode
  :custom (olivetti-body-width 84)
  :blackout " Olvti")

(use-package undo-fu
  :bind
  ("C-/" . undo-fu-only-undo)
  ("M-/" . undo-fu-only-redo))

(use-package unfill
  :commands (unfill-paragraph unfill-toggle unfill-region)
  :bind
  ("M-q" . unfill-toggle))

(use-package helpful
  :bind
  ("C-h f" . #'helpful-function)
  ("C-h v" . #'helpful-variable)
  ("C-h o" . #'helpful-symbol)
  ("C-h k" . #'helpful-key)
  ("C-h p" . #'helpful-at-point))

(use-package move-text
  :commands (move-text-up move-text-down)
  :bind
  ("C-x C-t" . oht-transient-transpose-lines)
  :config
  (transient-define-prefix oht-transient-transpose-lines ()
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    [["Move Lines"
      ("n" "Down" move-text-down)
      ("p" "Up" move-text-up)]]))

(use-package buffer-move
  :commands (buf-move-up
             buf-move-down
             buf-move-left
             buf-move-right))

(use-package visual-regexp
  ;; Provides an alternate version of `query-replace' which highlights matches
  ;; and replacements as you type.
  :bind (([remap query-replace] . #'vr/query-replace)))

(use-package visual-regexp-steroids
  ;; Allows `visual-regexp' to use regexp engines other than Emacs'; for
  ;; example, Python or Perl regexps.
  :after visual-regexp
  :bind (([remap query-replace-regexp] . #'vr/query-replace))
  :custom
  (vr/engine 'pcre2el))

(use-package bicycle
  :after outline
  :bind
  (:map outline-minor-mode-map
        ([C-tab] . bicycle-cycle))
  (:map emacs-lisp-mode-map
        ("<backtab>" . bicycle-cycle-global)))

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


;;; End of init.el
