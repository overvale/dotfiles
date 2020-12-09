;;; init.el  -*- lexical-binding: t -*-

;; To see the outline of this file, run M-x outline-minor-mode and then press
;; C-c @ C-t. You might also want to run M-x occur with the following query:
;; [^;;; ] OR... you can use the included hydra for outline navigation: s-0 o


;;; Preamble


;;;; Performance

;; Let's start by doing everything we can to improve emacs performance.
;; All of the below are stolen (with comments) from Doom Emacs.
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; https://github.com/hlissner/doom-emacs/blob/develop/core/core.el

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)
    (garbage-collect)) t)

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

;; When testing startup performance, it can be useful to print a message
;; saying how long it took to start up.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;;;; Straight Use Package

;; Required Bootstrap to ensure it is installed
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


;; use-package integration

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)


;;;; General Settings

;; Always follow symlinks. init files are normally stowed/symlinked.
(setq vc-follow-symlinks t
      find-file-visit-truename t
      ;; Avoid stale compiled code shadow newer source code
      load-prefer-newer t)

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

;; When customizing Emacs interactively (ie: not in this document or =init.el=)
;; Emacs appends code to your =init.el= file, which can be annoying when editing
;; it by hand. This tells Emacs to place these customizations in a separate
;; file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; I have a bunch of misc custom functions that I keep in a separate file to
;; make this document a little cleaner. Some of the bindings further down in
;; this document depend on these functions, so I load them here.
(load "~/dot/emacs/functions.el")


;;;; Startup and Scratch

;; By default, Emacs starts up with a "splash screen" containing some useful
;; links, but I don't need that any more, so this turns it off.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

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
(setq remember-data-file "~/Documents/org-files/remember-notes")



;;; Settings

;;;; Display

(tool-bar-mode -1)                         ; hide menu-bar
(scroll-bar-mode -1)                       ; hide scroll bars
(menu-bar-mode 1)                          ; ensures full-screen avail on macOS
(show-paren-mode t)                        ; highlight parens
(setq show-paren-delay 0)                  ; and show immediately
(setq visible-bell t)                      ; disable beep
(setq-default frame-title-format '("%b"))  ; show buffer name in titlebar
(setq x-underline-at-descent-line t)       ; underline at descent, not baseline
(setq-default indicate-empty-lines t)      ; show where the file ends
(set-default 'cursor-type 'box)            ; use a box for cursor
(blink-cursor-mode -1)                     ; no blinking please

;;;; General

(global-auto-revert-mode t)                ; update buffer when file on disk changes
(save-place-mode 1)                        ; reopens the file to the same spot you left
(recentf-mode 1)                           ; enables "Open Recent..." in file menu
(add-to-list 'recentf-exclude "/elpa")     ; don't show package files in recentf
(setq tab-width 4)                         ; tabs=4 char
(setq help-window-select t)                ; focus new help windows when opened
(setq sentence-end-double-space nil)       ; ends sentence after 1 space
(fset 'yes-or-no-p 'y-or-n-p)              ; Changes all yes/no questions to y/n type
(setq create-lockfiles nil)                ; No need for ~ files when editing
(setq-default fill-column 78)              ; Set column used for fill-paragraph
(setq ring-bell-function 'ignore)          ; Don't beep

;; place backup files in a single place
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))

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

;; When visiting read-only buffers, enter view-mode
(setq view-read-only t)

;; Emulate Mouse Buttons
(setq mac-emulate-three-button-mouse t)
;; mouse-1: Click
;; mouse-2: Option + Click
;; mouse-3: Command + Click
;; Keep in mind, however, that a 2-finger click on the track pad still sends
;; =mouse-3= no matter what you set =mac-emulate-three-button-mouse= to.

;; Since emacs seems to love spawning new windows, and taking over your existing
;; ones, this allows you to undo and redo those arrangements. So you if a
;; command kills a window arrangement you were using you can go back to it with
;; winner-undo and winner-redo.
(winner-mode 1)

;; This makes it so that when you click/right-click on the mode-line
;; emacs scrolls the window. Why not? I actually like how Acme does this.
(global-set-key [mode-line mouse-1] 'scroll-up-command)
(global-set-key [mode-line mouse-3] 'scroll-down-command)



;;; Packages

;;;; Critical Packages

;; These packages are relied upon by lots of things that come after, therefore
;; they must come first.

(use-package bind-key
  :demand t
  )

(use-package hydra
  :demand t
  :config
  (load "~/dot/emacs/hydras.el")
  )

(use-package blackout
  :demand t
  :config
  (blackout 'eldoc-mode)
  (blackout 'flyspell-mode " Spell")
  (blackout 'emacs-lisp-mode "Elisp")
  )

;;;; Other Packages

(use-package magit
  :commands magit-status
  )

(use-package exec-path-from-shell
  :demand t
  )

(use-package olivetti
  :commands olivetti-mode
  :custom (olivetti-body-width 80)
  )

(use-package which-key
  :demand t
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0.4)
  (blackout 'which-key-mode)
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

(use-package zzz-to-char
  ;; replaces zap-to-char with an avy-like interface
  ;; note that it searches forward and backward
  :bind ("M-z" . zzz-up-to-char))

;;(use-package sdcv-mode
;;  :commands sdcv-search
;;  :load-path "~/emacs.d/lisp/emacs-sdcv/")

(use-package unfill
  :commands (unfill-paragraph unfill-toggle unfill-region)
  )

(use-package bufler
  ;; Bufler - a better buffer list
  :commands bufler
  )

(use-package whole-line-or-region
  ;; When no region is active (nothing is selected), and you invoke the
  ;; =kill-region= (cut) or =kill-ring-save= (copy) commands, Emacs acts on the
  ;; range of characters between the mark and the point. This is a really good way
  ;; to accidentally kill half your document. I have done this more times than I'd
  ;; like to admit. This package makes it so that without a region those commands
  ;; act on a whole line.
  :demand t
  :config
  (whole-line-or-region-global-mode 1)
  (blackout 'whole-line-or-region-local-mode)
  )


;;;; Languages

(use-package fountain-mode
  :commands fountain-mode
  :custom
  (fountain-add-continued-dialog nil)
  (fountain-highlight-elements (quote (section-heading)))
  )

(use-package markdown-mode
  :commands markdown-mode)

(use-package lua-mode
  :commands lua-mode)


;;;; Spelling

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (setq ispell-program-name "/usr/local/bin/aspell")
  (customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-list-command "list")
)

(use-package flyspell-correct
  ;; =flyspell-correct= allows you to pass spelling suggestions to completion
  ;; and search frameworks, such as =selectrum=. This setup code is copied
  ;; directly from the selectrum documentation.
  :bind
  ("M-;" . #'flyspell-correct-wrapper)
  ("M-:" . #'flyspell-correct-at-point)
  :custom
  (flyspell-correct-interface 'flyspell-correct-dummy)
  :init
  (advice-add 'flyspell-correct-dummy :around
	      (defun my--fsc-wrapper (func &rest args)
		(let ((selectrum-should-sort-p nil))
		  (apply func args))))
  )


;;; Dired

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
	    (auto-revert-mode)
	    ))


;;; Emacs Help

;; =helpful= is a really neat package that brings together a lot of useful
;; information when you ask Emacs for help.

(use-package helpful
  ;; https://github.com/Wilfred/helpful
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h F" . #'helpful-function)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-h C" . #'helpful-command)
  ("C-h p" . #'helpful-at-point)
  :bind*
  ("C-?" . #'help-command)
  ("s-/" . #'help-command)  
 )


;;; macOS Consistency

;; The below is probably the biggest reason I managed get over the
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

;; When in visual line mode the out-of-the-box movement commands behave
;; inconsistently with the rest of macOS, so the below code brings them back
;; in line.

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
 ("s--" . oht/find-scratch)
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
 ("s-<up>" . beginning-of-buffer)
 ("s-<down>" . buffer)
 ("s-<return>" . oht/open-line-below)
 ("S-s-<return>" . oht/open-line-above)
 )

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

;; Navigating and using the thousands of things Emacs can do is built around
;; the idea of searching and narrowing a selection down to the thing you're
;; looking for. To make this easier I've installed a few packages that enhance
;; Emacs built-in facilities for doing this.

(use-package selectrum-prescient
  :init
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  )

(use-package prescient
  :init
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1)
  )

(use-package selectrum
  :init
  (selectrum-mode +1)
  )

(use-package consult
  :init
  (consult-preview-mode)
  :bind
  ("s-b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ("M-s-o" . consult-recent-file)
  )

(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia" :branch "main")
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package ctrlf
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

(use-package visual-regexp
  ;; Provides an alternate version of `query-replace' which highlights matches
  ;; and replacements as you type.
  :bind (([remap query-replace] . #'vr/query-replace)))

(use-package visual-regexp-steroids
  ;; Allows `visual-regexp' to use regexp engines other than Emacs'; for
  ;; example, Python or Perl regexps.
  :after visual-regexp
  :bind (([remap query-replace-regexp] . #'radian-query-replace-literal))
  :init
  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs)
  (defun radian-query-replace-literal ()
    "Do a literal query-replace using `visual-regexp'."
    (interactive)
    (let ((vr/engine 'emacs-plain))
      (call-interactively #'vr/query-replace)))
  )


;;; Appearance

;;;; Fonts

;; It can be quite refreshing to change the font I'm working in. So below are
;; a bunch of functions that set my preferred fonts, and a hydra for picking
;; them.

(defun oht/set-font-ibm ()
  (interactive)
  (set-face-attribute 'default nil
		      :family "IBM Plex Mono" :height 120 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "IBM Plex Serif" :height 140 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :family "IBM Plex Mono" :height 120 :weight 'normal)
  )

(defun oht/set-font-ibm-large ()
  (interactive)
  (set-face-attribute 'default nil
		      :family "IBM Plex Mono" :height 140 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "IBM Plex Serif" :height 160 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :family "IBM Plex Mono" :height 140 :weight 'normal)
  )

(defun oht/set-font-triplicate ()
  (interactive)
  (set-face-attribute 'default nil
		      :family "Triplicate T4" :height 140 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "Triplicate T4p" :height 140 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :family "Triplicate T4" :height 140 :weight 'normal)
  )

(defhydra hydra-fonts (:color amaranth)
  "Set Font Properties"
  ("i" oht/set-font-ibm "IBM Plex")
  ("I" oht/set-font-ibm-large "IBM Plex Large")
  ("t" oht/set-font-triplicate "Triplicate")
  ("v" variable-pitch-mode "Variable")
  ("q" nil "cancel"))

;; This sets the default fonts
(oht/set-font-ibm)


;;;; Theme

(use-package modus-themes
  :init
  (setq
   modus-themes-org-blocks 'grayscale
   modus-themes-syntax 'alt-syntax
   modus-themes-slanted-constructs t
   modus-themes-bold-constructs nil
   modus-themes-mode-line '3d
   modus-themes-diffs 'desaturated
   modus-themes-variable-pitch-headings nil
   modus-themes-links 'faint-neutral-underline
   modus-themes-completions nil
   modus-themes-prompts 'subtle
   modus-themes-region 'bg-only
   )
  (load-theme 'modus-operandi t)
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


;;;; Mode Line

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


;;; Org Mode

(use-package org
  :init (load-file "~/dot/emacs/org-mode.el")
  :commands org-mode
  :config
  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))
  :bind (:map org-mode-map
	      ("s-\\ o" . consult-outline)
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


;;; Window Control

;; Many of these bindings are duplicates of those in the windows hydra, which is
;; intentional, there should be some consistency between them.
(bind-keys :prefix-map oht/windows-leader
	   :prefix "s-="
	   ("s" . oht/split-below)
	   ("v" . oht/split-beside)
	   ("h" . hydra-windows/body)
	   ("k" . oht/delete-window)
	   ("o" . delete-other-windows)
	   ("b" . balance-windows)
	   ("r" . oht/toggle-window-split))


;;; Keybindings


;;;; Enhance Emacs

;; Make Emacs indent new lines automatically.
(define-key global-map (kbd "RET") 'newline-and-indent)

(bind-key "s-g" 'keyboard-quit)

(bind-key "s-f" #'occur)
(bind-key "s-F" #'all-occur)

(bind-key "M-<up>" 'oht/move-line-up)
(bind-key "M-<down>" 'oht/move-line-down)
(bind-key "M-o" 'oht/other-window)
(bind-key "M-s-s" 'save-some-buffers) ;save others

;; When region is active, make `capitalize-word' and friends act on it.
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; This cycles the spacing around point between a single space, no spaces, or the original spacing:
(bind-key "M-SPC" 'cycle-spacing)

;; This adds pulse-line to this keybinding, but that's it.
(bind-key "C-l" 'oht/recenter-top-bottom)

;; Show location of point with a pulse
(bind-key "C-=" 'pulse-line)


;;;; Primary Bindings

(bind-key "s-p" 'execute-extended-command)
(bind-key "s-k" 'oht/kill-this-buffer)
(bind-key "M-s-b" 'bufler)
(bind-key "s-C" 'org-capture)
(bind-key "s-|" 'hydra-manipulate/body)
(bind-key "C-M-t" 'hydra-transpose/body)
(bind-key "C-S-<mouse-1>" 'mc/add-cursor-on-click)

(bind-key "s-1" 'org-agenda)
(bind-key "s-2" 'hydra-secondary-selection/body)


;;;; Global Leader Bindings

(bind-keys :prefix-map oht/global-leader
	   :prefix "s-'"
	   ("a" . auto-fill-mode)
	   ("d" . sdcv-search)
	   ("h" . hl-line-mode)
	   ("l" . oht/toggle-line-numbers)
	   ("w" . oht/toggle-whitespace)
	   ("m" . magit-status)
	   ("M" . consult-marks)
	   ("<left>" . winner-undo)
	   ("<right>" . winner-redo)
	   ("s" . org-store-link)
	   ("o" . consult-outline)
	   ("f" . hydra-fonts/body)
	   ("!" . font-lock-mode)
	   )


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
(blackout 'outline-minor-mode)

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
  ("c" consult-outline "Consult" :color blue)
  ("n" outline-next-visible-heading "Next")
  ("p" outline-previous-visible-heading "Previous")
  ("q" nil "Cancel" :color blue)
  )
(bind-key "s-0" 'hydra-outline/body)


;;; View-Mode

(use-package view
  :ensure nil
  :init
  (setq view-read-only t)
  ;;(setq view-inhibit-help-message t)
  :bind
  (:map view-mode-map
	("q" . nil)
	("n" . next-line)
	("p" . previous-line)
	("f" . forward-word)
	("b" . backward-word)
	("{" . backward-paragraph)
	("}" . forward-paragraph)
	("(" . backward-sentence)
	(")" . forward-sentence)
	("s" . ctrlf-forward-fuzzy)
	("r" . ctrlf-backward-fuzzy)
	)
  )


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
(bind-key "s-' O" 'occur-hydra/body)


;;; Closing

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
