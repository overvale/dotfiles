;;; mac.el -*- lexical-binding: t -*-


;;; Commentary

;; This file makes Emacs behave more like a standard Mac app.

;; The below is probably the biggest reason I managed get over the
;; intimidation of using Emacs in those first few days. They're designed to make
;; all the shortcuts you use in every other Mac app to work the same way in
;; Emacs. Some of these simply remap existing bindings, and some of them call
;; custom functions that emulate macOS behavior.


;;;; Modifiers & Emacs Anachronisms

;; Make the command keys 'super'. Super is basically not used by Emacs so
;; they're a safe playground for assigning your own bindings.
(setq mac-command-modifier 'super)
;; Meta is used a lot in Emacs, and I only ever use the left option key, so
;; this works well for shortcuts.
(setq mac-option-modifier 'meta)
;; But sometimes you want to insert special characters like £¢∞§¶•≠
(setq mac-right-option-modifier 'nil)

;; Due to historical reasons, Emacs thinks C-i is the same as TAB and C-m
;; is the same as RETURN. The below undoes that assumption. This will allow
;; you to re-bind them later.
(define-key input-decode-map [?\C-i] [C-i])
(bind-key "<C-i>" nil)
(define-key input-decode-map [?\C-m] [C-m])
(bind-key "<C-m>" nil)

;; By default, Emacs doesn't replace the selection (region) with anything you
;; type, it just removes your selection and appends what you type. The below
;; makes what you type /replace/ your selection.
(delete-selection-mode t)

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

;; Eliminate duplicates in the kill ring. That is, if you kill the
;; same thing twice, you won't have to use M-y twice to get past it to
;; older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

;; When editing 2 files with the same name, like ~/foo/file and ~/bar/file,
;; Emacs (amazingly) refers to those files as file<~/foo> and file<~/bar>.
;; This makes Emacs refer to them as foo/file and bar/file, like a sane
;; program.
(setq uniquify-buffer-name-style 'forward)


;;;; Visual Line Mode

;; Confusingly, `visual-line-mode', `word-wrap', and `truncate-lines' are all
;; different things. `visual-line-mode' is a wrapper around a bunch of
;; things, probably best explained here:
;; http://ergoemacs.org/emacs/emacs_long_line_wrap.html
;; `word-wrap' ONLY wraps lines word-wise instead of character-wise.
;; `truncate-lines' ONLY controls if wrapping happens at all. If set to
;; non-nil it is supposed to let lines run off the window, but this is a
;; buffer-local setting that I cannot (no matter what I try) get to be global.
(setq-default truncate-lines t)

;; So, instead, I take the brute-force approach of adding a hook for text-mode
;; and prog-mode to call a function which toggles the value on. Take that Emacs.
(defun oht/truncate-lines()
  (interactive)
  (toggle-truncate-lines 1)
  )
(add-hook 'text-mode-hook 'oht/truncate-lines)
(add-hook 'prog-mode-hook 'oht/truncate-lines)


;; But I want every buffer to start with what mac apps normally call
;; 'soft-wrapping' of long lines to the window boundaries.
(global-visual-line-mode t)

;; However, turning on `visual-line-mode' also binds "C-a" to
;; `beginning-of-visual-line'. This is inconsistent with macOS behavior, which
;; is that "C-a" always goes to the beginning of the logical line and
;; "s-<left>" goes to the beginning of the visual line. So these bindings
;; correct that.
(bind-keys* ("C-a" . beginning-of-line)
	    ("C-e" . end-of-line))
(bind-keys ("s-<left>" . beginning-of-visual-line)
	   ("s-<right>" . end-of-visual-line)
	   ;; C-k only killing the visual line also isn't how macOS works.
	   ;; This has to be set to a custom function so minor modes can't
	   ;; hijack it.
	   ("C-k" . oht/kill-line))


;;; Emulate Mouse Buttons

(setq mac-emulate-three-button-mouse t)
;; mouse-1: Click
;; mouse-2: Option + Click
;; mouse-3: Command + Click
;; Keep in mind, however, that a 2-finger click on the track pad still sends
;; mouse-3 no matter what you set `mac-emulate-three-button-mouse' to.


;;;; Standard Mac Shortcuts

;; Wherever possible I want to use standard macOS shortcuts[1]. macOS actually
;; inherits many Emacs keybindings, but adds to it a few from =readline= and old
;; terminal interfaces. Because these are available system-wide I want Emacs to
;; do the same thing. That way the way I type/move in Mail.app or Safari is the
;; same as Emacs. There are also conventions that, while not officially
;; standard, have become widely accepted, those should be respected too. Some of
;; these require custom functions, but that's usually a simple matter of
;; stringing a couple existing commands together into a function.
;; [1]: https://support.apple.com/en-us/HT201236

;; C-[ sends ESC so let's make ESC more predictable
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(bind-keys
 ("s-," . oht/find-settings)
 ("s--" . oht/find-scratch)
 ("s-n" . make-frame-command)
 ("s-N" . make-frame-command)
 ("s-t" . oht/new-tab)
 ("s-m" . iconify-frame)
 ("s-s" . save-buffer)
 ("s-S" . write-file) ;save as
 ("s-a" . mark-whole-buffer)
 ("s-o" . find-file)
 ("s-x" . kill-region)
 ("s-c" . kill-ring-save)
 ("s-v" . yank)
 ("s-<backspace>" . oht/kill-visual-line-backward)
 ("s-w" . delete-frame)
 ("s-q" . save-buffers-kill-terminal)
 ("s-l" . oht/mark-whole-line)
 ("s-M-l" . mark-paragraph)
 ("S-s-<left>" . oht/expand-to-beginning-of-visual-line)
 ("S-s-<right>" . oht/expand-to-end-of-visual-line)
 ("s-<up>" . beginning-of-buffer)
 ("s-<down>" . end-of-buffer)
 ("s-<return>" . oht/open-line-below)
 ("S-s-<return>" . oht/open-line-above)
 ;; navigation and indentation
 ("s-[" . previous-buffer)
 ("s-]" . next-buffer)
 ("s-}" . indent-rigidly-right-to-tab-stop)
 ("s-{" . indent-rigidly-left-to-tab-stop)
 ;; readline-style shortcuts, because I love them
 ("C-w" . backward-kill-word)
 ("C-u" . oht/kill-line-backward)
 ;; No reason not to use command-u for this instead
 ("s-u" . universal-argument)
 )

;; These need to 'bubble-up' above major-mode bindings
(bind-keys*
 ;; Mac follows the UNIX convention of C-h being the same as <DEL>
 ("C-h" . delete-backward-char)
 ;; since ctrl+alt+b/f are system shortcuts for word movement, do that in Emacs
 ("C-M-b" . left-word)
 ("C-M-f" . right-word)
 ;; in emacs <del/backspace> is backward-delete and <delete> is forward-delete
 ;; and by default option+forward-delete has no mapping
 ("M-<delete>" . kill-word)
 )
