;;; init.el  -*- lexical-binding: t -*-

;;; Commentary

;; To see the outline of this file, run M-x outline-minor-mode and then press
;; C-c @ C-t. You might also want to run M-x occur with the following query:
;; [^;;; ] OR... you can use the included hydra for outline navigation: s-0 o

;;; Emacs Performance

;; Let's start by doing everything we can to improve emacs performance.
;; INFO: https://blog.d46.us/advanced-emacs-startup

;;;; Bootstrap

;; To improve startup time, I follow the advice given by Doom Emacs:
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)
    (garbage-collect)) t)

;; Always follow symlinks. init files are normally stowed/symlinked.
(setq vc-follow-symlinks t
      find-file-visit-truename t
      ;; Avoid stale compiled code shadow newer source code
      load-prefer-newer t)

(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ))

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package
(eval-when-compile
  (require 'use-package))
(setq-default use-package-always-ensure t
	      use-package-always-defer t
	      )

;;;; Startup Profiling

;; When you have a large config file, and a lot of packages, Emacs can be very
;; slow to startup. Fortunately, you can identify the exact things that are
;; making Emacs slow using the tools below, and do something about it using
;; =use-package=.

;; this package creates a report each time you startup
;; You'll need to add ':demand t' and restart emacs to see the report
(use-package benchmark-init
  ;;:demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Print a message saying how long it took to start up
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Profiling Tools
(bind-keys :prefix-map oht/profiling-keys
	   :prefix "M-s-p"
	   ("s" . profiler-start)
	   ("q" . profiler-stop)
	   ("r" . profiler-report)
	   )


;;;; Startup Preferences

;; By default Emacs starts up with a "splash screen" containing some useful
;; links, but I don't need that any more, so this turns it off.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(tool-bar-mode -1)                         ; hide menu-bar
(scroll-bar-mode -1)                       ; hide scroll bars

;;;; Remember-Notes

;; By default, Emacs always starts up with a *scratch* buffer that is NOT
;; intended to be saved; when you kill it or quit emacs the contents of your
;; *scratch* buffer are gone forever without warning. I don't think I'm alone in
;; thinking that this is not very useful. Thankfully, emacs 24.4 introduced a
;; replacement for the *scratch* buffer that is saved when emacs exits, thus
;; your scratch pad NEVER deletes data. And THAT makes a lot more sense to me.
;; More info:
;; https://www.masteringemacs.org/article/whats-new-in-emacs-24-4#remember

;; This sets the name of the buffer to *scratch*, which I suppose I'm just doing
;; for tradition's sake.
(setq remember-notes-auto-save-visited-file-name t
      remember-notes-buffer-name "*scratch*")
;; If you just tell initial-buffer-choice that you want to use remember-notes as
;; your initial buffer emacs will still create a *scratch* buffer that you'll
;; never use. So we set initial-buffer-choice to a function which kills the
;; scratch buffer and opens the remember-notes file.
(setq initial-buffer-choice
      (lambda () (kill-buffer remember-notes-buffer-name)
                 (remember-notes)))

;; Now set the location of the remember-notes file.
(setq remember-data-file "/Users/oht/Documents/remember-notes")

;;;; Performance Enhancers

;; All of these are all stolen (with comments) from Doom Emacs:
;; https://github.com/hlissner/doom-emacs/blob/develop/core/core.el

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only


;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)


;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(setq command-line-x-option-alist nil)


;;; Encoding

;; Set encoding to be UTF-8 everywhere. I've seen a lot of conflicting
;; information about what's needed to use UTF-8 everywhere. Mastering Emacs
;; seems like a trustworthy source.
;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Interactive Customization

;; When customizing Emacs interactively (ie: not in this document or =init.el=)
;; Emacs appends code to your =init.el= file, which can be annoying when editing
;; it by hand. This tells Emacs to place these customizations in a separate
;; file.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;;; Emacs Load Paths

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; I have a bunch of misc custom functions that I keep in a separate file to make this document a little cleaner. Some of the bindings further down in this document depend on these functions, so I load them here.

(load "~/dot/emacs/functions.el")
(load "~/dot/emacs/selectrum.el")
(load "~/dot/emacs/hydras.el")

;;; Preferences/Settings

;;;; Display

(menu-bar-mode 1)                          ; ensures full-screen avail on macOS
(show-paren-mode t)                        ; highlight parens
(setq show-paren-delay 0)                  ; and show immediately
(setq visible-bell t)                      ; disable beep
(setq-default frame-title-format '("%b"))  ; show buffer name in titlebar
(setq x-underline-at-descent-line t)       ; underline at descent, not baseline
(setq-default indicate-empty-lines t)      ; show where the file ends

;; cursor settings
(set-default 'cursor-type 'box)
(blink-cursor-mode -1)

;;;; General

(global-auto-revert-mode t)            ; update buffer when file on disk changes
(save-place-mode 1)                    ; reopens the file to the same spot you left
(recentf-mode 1)                       ; enables "Open Recent..." in file menu
(add-to-list 'recentf-exclude "/elpa") ; don't show package files in recentf
(setq tab-width 4)                     ; tabs=4 char
(setq help-window-select t)            ; focus new help windows when opened
(setq sentence-end-double-space nil)   ; ends sentence after 1 space
(fset 'yes-or-no-p 'y-or-n-p)          ; Changes all yes/no questions to y/n type
(setq create-lockfiles nil)            ; No need for ~ files when editing
(setq-default fill-column 80)          ; Set column used for fill-paragraph
(setq ring-bell-function 'ignore)      ; Don't beep

;; place backup files in a single place
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

; Use Spotlight to search with M-x locate
(setq locate-command "mdfind")

;;Use the system trash folder to delete files.
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash/emacs")

;; Echo unfinished commands after this delay
;; setting to 0 means do not echo commands
(setq echo-keystrokes 0.1)

;; When exiting emacs, kill all running processes
(setq confirm-kill-processes nil)

;;;; Mouse

;; Emacs was built for 3 button mice. In the Mac Port the 3 buttons are used like so:
;; mouse-1: Left Click
;; mouse-2: Fn + Left Click
;; mouse-3: Right Click

;; But you can change this to:
;; mouse-1: Click
;; mouse-2: Option + Click
;; mouse-3: Command + Click

;; With this setting:
(setq mac-emulate-three-button-mouse t)

;; Keep in mind, however, that a 2-finger click on the track pad still sends
;; =mouse-3= no matter what you set =mac-emulate-three-button-mouse= to.

;; By default, the mouse-buttons are bound to the following actions:
;; - mouse-1: moves point
;; - mouse-2: yanks from kill-ring
;; - mouse-3: extends region from point to click, and saves to kill-ring, click again to kill.

;; And if =mouse-yank-at-point= is set to =t= then =mouse-2= yanks to point instead of click.

;;;; Spelling

;; Tell ispell where to find the =aspell= executable, and some settings.

(use-package flyspell
  :config
  (setq ispell-program-name "/usr/local/bin/aspell")
  (customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-list-command "list")
)

;; =flyspell-correct= allows you to pass spelling suggestions to completion and search frameworks, such as =selectrum=. This setup code is copied directly from the selectrum documentation.

(use-package flyspell-correct
  :demand t
  :custom
  (flyspell-correct-interface 'flyspell-correct-dummy)
  )
(advice-add 'flyspell-correct-dummy :around
	    (defun my--fsc-wrapper (func &rest args)
	      (let ((selectrum-should-sort-p nil))
		(apply func args))))

(bind-key "M-;" 'flyspell-correct-wrapper)
(bind-key "M-:" 'flyspell-correct-at-point)

;;; Emacs Help

;; =helpful= is a really neat package that brings together a lot of useful information when you ask Emacs for help.

(use-package helpful
  ;; https://github.com/Wilfred/helpful
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h F" . #'helpful-function)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-h C" . #'helpful-command)
  ("C-h p" . #'helpful-at-point)
 )

;; Normally, C-? is used for undo/redo,
;; but I've rebound that elsewhere, so I can use it here
(bind-key* "C-?" 'help-command)
(bind-key* "s-/" 'help-command)

;;; macOS Consistency

;; The below is probably the biggest reason why I managed get over the
;; intimidation of using Emacs in those first few days. They're designed to make
;; all the shortcuts you use in every other Mac app to work the same way in
;; Emacs. Some of these simply remap existing bindings, and some of them call
;; custom functions that emulate macOS behaviour.

;;;; Modifiers & Emacs Anachronisms

;; The below does 3 things:

;; ONE:
;; Makes the command keys act as =super=. =super= keybindings are basically not
;; used by Emacs so they're a safe playground for assigning your own
;; keybindings. I setup =s-q= for quit, =s-s= for save, =s-z= for undo, =s-o=
;; for open file, basically, all the standard Mac shortcuts. Once I did that
;; Emacs became very usable immediately and that ease-of-use made learning Emacs
;; a lot less painful.
;;
;; TWO:
;; Makes the left option key act =meta= so I can use meta-keybindings.
;;
;; THREE:
;; 3. Makes the right option key act as =option= to I can insert characters like: £¢∞§¶•≠.

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)

;; Due to historical reasons, Emacs thinks =C-i= is the same as =TAB= and =C-m=
;; is the same as =RETURN=. The below undoes that assumption. This will allow
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

;; When editing 2 files with the same name, like =~/foo/file= and =~/bar/file=,
;; Emacs (amazingly) refers to those files as =file<~/foo>= and =file<~/bar>=.
;; This makes Emacs refer to them as =foo/file= and =bar/file=, like a sane
;; program.
(setq uniquify-buffer-name-style 'forward)

;; By default Emacs window sizes always line-up with the character-grid, meaning
;; the windows resize only by character-widths and line-heights. This setting
;; allows the windows to be unconstrained by the grid, thus resize smoothly. The
;; downside of this approach is that your frame contents need to refresh when
;; you're done resizing the frame. When set to =nil= the frame contents refresh
;; live, to the character grid.
(setq frame-resize-pixelwise nil)

;;;; Visual Line Mode

;; When in visual line mode the out-of-the-box movement commands behave inconsistently with the rest of macOS, so the below code brings them back in line.

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)
;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)

;; Turn on word-wrap globally
(global-visual-line-mode t)

;; with visual-line-mode set,
;; C-a and C-b go to beginning/end-of-visual-line
;; which is inconsistant with standard Mac behaviour
(bind-key* "C-a" 'beginning-of-line)
(bind-key* "C-e" 'end-of-line)
(bind-key "s-<left>" 'beginning-of-visual-line)
(bind-key "s-<right>" 'end-of-visual-line)
;; C-k only killing the visual line also isn't how macOS works.
;; This has to be set to a custom function so minor modes can't hijack it.
(bind-key "C-k" 'oht/kill-line)

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
 ("s-n" . oht/switch-to-new-buffer)
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
 ("s-<return>" . oht/open-line-below)
 ("S-s-<return>" . oht/open-line-above)
 )

;; these don't work with 'bind-keys' (above)
(bind-key "s-<up>" (kbd "M-<"))
(bind-key "s-<down>" (kbd "M->"))

;; navigation and indentation
(bind-key "s-[" 'previous-buffer)
(bind-key "s-]" 'next-buffer)
(bind-key "s-}" 'indent-rigidly-right-to-tab-stop)
(bind-key "s-{" 'indent-rigidly-left-to-tab-stop)
;; Mac follows the UNIX convention of C-h being the same as <DEL>
(bind-key* "C-h" 'delete-backward-char)
;; readline-style shortcuts, because I love them
(bind-key "C-w" 'backward-kill-word)
(bind-key "C-u" 'oht/kill-line-backward)
;; No reason not to use command-u for this instead
(bind-key "s-u" 'universal-argument)
;; since ctrl+alt+b/f are system shortcuts for word movement, do that in Emacs
(bind-key* "C-M-b" 'left-word)
(bind-key* "C-M-f" 'right-word)
;; in emacs <del/backspace> is backward-delete and <delete> is forward-delete
;; and by default option+forward-delete has no mapping
(bind-key* "M-<delete>" 'kill-word)

;;; Narrowing & Searching

;; Navigating and using the thousands of things Emacs can do is built around the idea of searching and narrowing a selection down to the thing you're looking for. To make this easier I've installed a few packages that enhance Emacs built-in facilities for doing this.

;;;; Selectrum

;; I've tried a number of them (including =ivy=, =helm=, and =icomplete=) but I find =selectrum= to be the most Emacs-y (in a good way). It is very simple, very fast, and doesn't try to do more than its basic function.

;; selectrum is the live-search framework
(use-package selectrum
  :demand t
  :config (selectrum-mode +1)
  :bind
  ("s-b" . selectrum-switch-buffer+)
  ("M-y" . yank-pop+)
  ("M-s-o" . recentf-open-files+)
  )

;; prescient is for sorting search candidates
(use-package prescient
  :demand t
  :config (prescient-persist-mode +1)
  )

;; this combines them
(use-package selectrum-prescient
  :demand t
  :config (selectrum-prescient-mode +1)
)

;;;; CTRLF

;; The creator of these packages also created an enhanced version of =isearch= which I find very useful, and in keeping with the philosophy of minimalism.

(use-package ctrlf
  ;; When you enable ctrlf-mode, you get these DEFAULT BINDINGS:
  ;; C-s - ctrlf-forward-literal
  ;; C-r - ctrlf-backward-literal
  ;; C-M-s - ctrlf-forward-regexp
  ;; C-M-r - ctrlf-backward-regexp
  ;; M-s _ - ctrlf-forward-symbol
  ;; M-s . - ctrlf-forward-symbol-at-point
  ;; C-o s - change search style, which includes fuzzy & fuzzy-regex
  ;; but since this is a minor mode, its bindings will override your explicit
  ;; bindings in this config. No thank you. I'd rather manually create the same
  ;; bindings.

  ;; And these bindings are standard for the minibuffer:
  ;; M-n - insert symbol-at-point
  ;; M-p - previous in minibuffer history
  ;; M-n - next in minibuffer history

  :config
  (setq ctrlf-minibuffer-bindings
        `(("C-n"          . ctrlf-next-match)
          (,(kbd "C-p")   . ctrlf-previous-match)
	  (,(kbd "M-O")   . ctrlf-occur)
	  (,(kbd "M-c")   . ctrlf-toggle-case-fold-search)
	  (,(kbd "M-r")   . ctrlf-toggle-regexp)
	  (,(kbd "C-o s") . ctrlf-change-search-style)
	  ))

  :bind
  ("C-s" . ctrlf-forward-fuzzy)
  ("C-r" . ctrlf-backward-fuzzy)
  )

;;;; Search and Replace

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :bind (([remap query-replace] . #'vr/query-replace)))

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; regexp engines other than Emacs'; for example, Python or Perl
;; regexps.
(use-package visual-regexp-steroids
  :demand t
  :after visual-regexp
  :bind (([remap query-replace-regexp] . #'radian-query-replace-literal))
  :config

  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs)

  (defun radian-query-replace-literal ()
    "Do a literal query-replace using `visual-regexp'."
    (interactive)
    (let ((vr/engine 'emacs-plain))
      (call-interactively #'vr/query-replace))))


;;; Packages

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
;; (setq use-package-always-defer t)

(use-package magit
  :commands magit-status
  )
(use-package bind-key)
(use-package exec-path-from-shell)
(use-package olivetti
  :commands olivetti-mode
  :custom (olivetti-body-width 80)
  )
(use-package unfill
  :commands (unfill-paragraph unfill-toggle unfill-region)
  )
(use-package which-key
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0.4)
  )
(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo)
  )
(use-package expand-region
  :bind
  ("s-e" . er/expand-region)
  ("s-E" . er/contract-region)
  )
(use-package sdcv-mode
  :commands sdcv-search
  :load-path "lisp/emacs-sdcv/")

;; Since emacs seems to love spawning new windows, and taking over your existing
;; ones, this allows you to undo and redo those arrangements. So you if a
;; command kills a window arrangement you were using you can go back to it with
;; winner-undo and winner-redo.
(winner-mode 1)

;; Toggle mode-line
(use-package hide-mode-line
  :commands hide-mode-line-mode
)

;; Use iMenu across all open buffers
(use-package imenu-anywhere
  :commands (imenu-anywhere)
)

;; replaces zap-to-char with an avy-like interface
(use-package zzz-to-char
  :bind ("M-z" . zzz-up-to-char))

;; Whole Line or Region

;; When no region is active (nothing is selected), and you invoke the
;; =kill-region= (cut) or =kill-ring-save= (copy) commands, Emacs acts on the
;; range of characters between the mark and the point. This is a really good way
;; to accidentally kill half your document. I have done this more times than I'd
;; like to admit. This package makes it so that without a region those commands
;; act on a whole line.

(use-package whole-line-or-region
  :demand t
  :config
  (whole-line-or-region-global-mode 1)
  )

;; Bufler - a better buffer list
(use-package bufler
  :commands bufler
  )

(use-package fountain-mode
  :commands fountain-mode
  :custom
  (fountain-add-continued-dialog nil)
  (fountain-highlight-elements (quote (section-heading)))
  )
(use-package lua-mode
  :commands lua-mode)
(use-package markdown-mode
  :commands markdown-mode)

;;;; Spelling

;; Flyspell offers on-the-fly spell checking. We can enable flyspell for all text-modes with this snippet.

(add-hook 'text-mode-hook 'turn-on-flyspell)

;; To use flyspell for programming there is flyspell-prog-mode, that only enables spell checking for comments and strings. We can enable it for all programming modes using the prog-mode-hook.

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;; General

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
	    (auto-revert-mode)
	  ))

;;; Appearance

;;;; Fonts

;; Scaling fonts in Emacs can be... difficult. Sure, there are built-in functions like =text-scale-adjust= but when using a theme like Modus that takes full advantage of Emacs's mixed-pitch capabilities it can be difficult to get /just/ right.

;; Then I realized that I only care about 2 sizes, normal and large. So I've created some functions that set things up the way I like them and bindings for those functions. Done.

(defun oht/set-font-normal ()
  (interactive)
  (set-face-attribute 'default nil
		      :family "SF Mono" :height 120 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "IBM Plex Serif" :height 140 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :family "SF Mono" :height 120 :weight 'normal)
  )

(defun oht/set-font-large ()
  (interactive)
  (set-face-attribute 'default nil
		      :family "SF Mono" :height 140 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "IBM Plex Serif" :height 160 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :family "SF Mono" :height 140 :weight 'normal)
  )

;; This sets the default fonts
(oht/set-font-normal)

;;;; Theme

;; I use, and *love* /prot/'s [[https://gitlab.com/protesilaos/modus-themes][Modus Themes]].

(use-package modus-vivendi-theme
  :custom
  (modus-vivendi-theme-faint-syntax t)
  (modus-vivendi-theme-slanted-constructs t)
  (modus-vivendi-theme-bold-constructs t)
  (modus-vivendi-theme-mode-line '3d)
  (modus-vivendi-theme-org-blocks 'greyscale)
  (modus-vivendi-theme-completions 'moderate)
  (modus-vivendi-theme-diffs 'desaturated)
  (set-face-attribute 'bold nil :weight 'semibold)
)

(use-package modus-operandi-theme
  :demand t
  :custom
  (modus-operandi-theme-faint-syntax t)
  (modus-operandi-theme-slanted-constructs t)
  (modus-operandi-theme-bold-constructs t)
  (modus-operandi-theme-org-blocks 'greyscale)
  (modus-vivendi-theme-mode-line '3d)
  (modus-operandi-theme-completions 'moderate)
  (modus-operandi-theme-diffs 'desaturated)
  :config
  (load-theme 'modus-operandi t)
  (set-face-attribute 'bold nil :weight 'semibold)
)

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (load-theme 'modus-vivendi t))
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)))

;; Though I rarely use them, I like these themes too.

(use-package gruvbox-theme)
(use-package nord-theme)
(use-package tron-legacy-theme)
(use-package zenburn-theme)

;;;; Mode Line

(use-package minions
  :demand t
  :config (minions-mode t))

;; add columns to the mode-line
(column-number-mode t)
(setq display-time-format "%H:%M  %Y-%m-%d")
;; Covered by `display-time-format'
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
(setq display-time-interval 60)
(setq display-time-mail-directory nil)
(setq display-time-default-load-average nil)
(display-time-mode t)

;;; Auto-complete

;; I've tried a few completion packages and they've all left me cold. =hippy-expand= generally gets me what I want, but I'd like the pop-up list to use the completion framework. Some googling led me to this fucntion, built for ivy, which I've modified for use with =selectum=.

;; https://gist.github.com/JohnLunzer/7c6d72a14c76c0a3057535e4f6148ef8
(defun my-hippie-expand-completions (&optional hippie-expand-function)
  "Return list of completions generated by `hippie-expand'."
  (save-excursion
    (let ((this-command 'my-hippie-expand-completions)
          (last-command last-command)
          (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'my-hippie-expand-completions)
               (not (equal he-num -1))))
      ;; Provide the options in the order in which they are normally generated.
      (delete he-search-string (reverse he-tried-table)))))

(defun my-hippie-expand-with (hippie-expand-function)
  "Offer completion using the specified hippie-expand function."
  (let* ((options (my-hippie-expand-completions hippie-expand-function)))
    (if options
        (progn
          (if (> (safe-length options) 1)
              (setq selection (completing-read "Completions: " options))
            (setq selection (car options)))
          (if selection
              (he-substitute-string selection t)))
      (message "No expansion found"))))

(defun my-hippie-expand ()
  "Offer completion for the word at point."
  (interactive)
  (my-hippie-expand-with 'hippie-expand))

(global-set-key (kbd "M-/") 'my-hippie-expand)

;; In Emacs, =TAB= is used by a lot of languages for indentation (org particularly). Completion is triggered with =M-TAB=. But the setting =tab-always-indent 'complete= will tell Emacs to first try to indent the line, and if it's already indented trigger =completion-at-point=.

(setq tab-always-indent 'complete)

;; The following adds file-path completion to the completion framework. So when your point is on something that looks like a file-path Emacs will offer file-path completions. This technique is taken from [[https://with-emacs.com/posts/tutorials/customize-completion-at-point/][(with-emacs]].

;; This is especially nice with selectrum which won’t exit file completions after each path level so you can conveniently navigate to the path like you would do with find-file.

(autoload 'ffap-file-at-point "ffap")
(defun complete-path-at-point+ ()
  "Return completion data for UNIX path at point."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table :exclusive 'no))))

(add-hook 'completion-at-point-functions
          #'complete-path-at-point+
          'append)

;; I've not enabled the below code because I'm still testing it, but it basically creates a function witch reads and caches a file with a list of words and offers completions from it when you call a custom function. It is taken from [[https://emacs.stackexchange.com/questions/37423/completion-by-fuzzy-search-in-large-dictionary-file-displaying-candidates-inlin/37446#37446][this stackexchange message]].

;; (defun my-dictionary ()
;;   "Return hash-table whose keys comprise words.txt."
;;   (with-temp-buffer
;;     (insert-file-contents "/usr/share/dict/words")
;;     (let ((table (make-hash-table :test #'equal :size 466544)))
;;       (while (not (eobp))
;;         (puthash (buffer-substring (point) (line-end-position)) nil table)
;;         (forward-line))
;;       table)))
;; 
;; (defvar my-dictionary
;;   (lazy-completion-table my-dictionary my-dictionary)
;;   "Lazy completion table for function `my-dictionary'.")
;; 
;; ;; this is the function to call to execute the completion
;; (defun my-complete-word-in-region ()
;;   "Complete word preceding point under `my-dictionary'."
;;   (interactive)
;;   (completion-in-region
;;    (save-excursion
;;      (skip-syntax-backward "w")
;;      (point))
;;    (point)
;;    my-dictionary))

;;; Org

(use-package org
  :commands org-mode
  :config
  (load-file "~/dot/emacs/org-mode.el")
  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))
  :bind (:map org-mode-map
	      ("s-\\ o" . selectrum-outline)
	      ("s-\\ ." . oht/org-insert-date-today)
	      ("s-\\ t" . org-todo)
	      ("s-\\ n" . org-narrow-to-subtree)
	      ("s-\\ w" . widen)
	      ;;("s-\\ s" . org-search-view)
	      ("s-\\ <" . org-insert-structure-template)
	      ("s-\\ s" . org-store-link)
	      ("s-\\ i" . org-insert-last-stored-link)
	      ("s-\\ m" . visible-mode)
	      ("s-\\ I" . org-clock-in)
	      ("s-\\ O" . org-clock-out)
	      ("s-\\ h" . hydra-org/body)
	      ("s-\\ a" . org-archive-subtree)
	      ("s-\\ r" . org-refile)
	      ("s-\\ g" . org-goto)
	      ("s-\\ c" . org-toggle-checkbox)
	      ))

;;; Secondary Selection

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
  "Paste the secondary selection and paste at point."
  (interactive)
  (oht/copy-secondary-selection)
  (yank))

(defhydra hydra-secondary-selection (:color blue)
  "Secondary Selection"
  ("xx" oht/cut-secondary-selection "Cut 2nd")
  ("cc" oht/copy-secondary-selection "Copy 2nd")
  ("xv" oht/cut-secondary-selection-paste "Cut 2nd & Paste")
  ("cv" oht/copy-secondary-selection-paste "Copy 2nd & Paste")
  ("m" (lambda () (interactive)(secondary-selection-from-region)) "Mark as 2nd")
  ("g" (lambda () (interactive)(secondary-selection-to-region)) "Goto 2nd")
  ("q" nil "cancel"))

;;; Outline

;; `outline' provides major and minor modes for collapsing
;; sections of a buffer into an outline-like format.
;; Let's turn that minor mode into a global minor mode.
(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)
(global-outline-minor-mode +1)

(defun oht/outline-show-entry-branches ()
  "This unfolds the 'entry' and shows all the subheadings, similar to how org-mode works."
  (interactive)
  (outline-show-entry)
  (outline-show-branches)
  )

(defhydra hydra-outline (:color amaranth)
  "Hydra for navigating outline mode"
  ("o" outline-hide-sublevels "Hide to This Sublevel")
  ("<tab>" oht/outline-show-entry-branches "Show Subtree")
  ("S-<tab>" outline-hide-subtree "Hide Subtree")
  ("a" outline-show-all "Show All" :color: blue)
  ("s" selectrum-outline "Selectrum" :color blue)
  ("n" outline-next-visible-heading "Next")
  ("p" outline-previous-visible-heading "Previous")
  ("q" nil "Cancel" :color blue)
  )
(bind-key "s-0" 'hydra-outline/body)

;;; Window Control

;; Many of these bindings are duplicates of those in the windows hydra, which is
;; intentional, there should be some consistency between them.
(bind-keys :prefix-map oht/windows-leader
	   :prefix "s-="
	   ("s" . oht/split-below)
	   ("v" . oht/split-beside)
	   ("h" . hydra-windows/body)
	   ("k" . delete-window)
	   ("o" . delete-other-windows)
	   ("b" . balance-windows)
	   ("r" . oht/toggle-window-split))


;;; Smart Occur

;; From [[https://oremacs.com/2015/01/26/occur-dwim/][oremacs]]. This will offer as the default candidate:

;; - the current region, if it's active
;; - the current symbol, otherwise

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

;; and a hydra to go with it
;; TODO update hydra formatting to defhydra
(defhydra occur-hydra (:color amaranth)
  "Create/Navigate occur errors"
  ("o" occur-dwim "occur")
  ("f" first-error "first")
  ("n" next-error "next")
  ("p" previous-error "prev")
  ("q" exit "exit" :color blue))
(bind-key "s-' o" 'occur-hydra/body)


;;; Keybindings

;;;; Enhance Emacs

;; Make Emacs indent new lines automatically.
(define-key global-map (kbd "RET") 'newline-and-indent)

(bind-key "s-g" 'keyboard-quit)

(bind-key "M-<up>" 'oht/move-line-up)
(bind-key "M-<down>" 'oht/move-line-down)
(bind-key "M-o" 'other-window)
(bind-key "M-s-s" 'save-some-buffers) ;save others

;; When region is active, make `capitalize-word' and friends act on it.
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; This cycles the spacing around point between a single space, no spaces, or the original spacing:
(bind-key "M-SPC" 'cycle-spacing)

;;;; Primary Bindings

(bind-key "s-p" 'execute-extended-command)
(bind-key "s-k" 'oht/kill-this-buffer)
(bind-key "M-s-b" 'bufler)
;; vim has the wonderful . command, and emacs has repeat
;; s-y is my keybinding because excel has (a version of) repeat bound to that
(bind-key "s-y" 'repeat)
(bind-key "s-C" 'org-capture)
(bind-key "s-|" 'hydra-manipulate/body)
(bind-key "C-M-t" 'hydra-transpose/body)
(bind-key "C-S-<mouse-1>" 'mc/add-cursor-on-click)

(bind-key "s-1" 'org-agenda)
(bind-key "s-2" 'hydra-secondary-selection/body)

;;;; Global Leader Bindings

(bind-keys :prefix-map oht/global-leader
	   :prefix "s-'"
	   ("d" . sdcv-search)
	   ("h" . hl-line-mode)
	   ("l" . oht/toggle-line-numbers)
	   ("w" . oht/toggle-whitespace)
	   ("m" . magit-status)
	   ("M" . selectrum-marks)
	   ("<left>" . winner-undo)
	   ("<right>" . winner-redo)
	   ("v" . variable-pitch-mode)
	   ("s" . org-store-link)
	   ("o" . selectrum-outline)
	   ("-" . oht/set-font-normal)
	   ("=" . oht/set-font-large)
	   )

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
