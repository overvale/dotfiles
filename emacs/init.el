;;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Copyright (C) 2021 Oliver Taylor

;; Author: Oliver Taylor
;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; This file has an outline which can be viewed by looking at comments
;; starting with three or more semicolons. `outline-minor-mode' supports this
;; convention by default and helps with navigation. You can also create an
;; occur buffer with the search /^;;;+/.

;; Some inspiration:
;; https://github.com/oantolin/emacs-config
;; https://github.com/raxod502/radian
;; https://github.com/skeeto/.emacs.d
;; https://github.com/bbatsov/prelude
;; https://github.com/emacscollective/emacs.g
;; https://codeberg.org/jao/elibs


;;; Preamble

;; It is useful to know the impact of your init file on Emacs startup time so
;; you can avoid introducing slowdowns. There are many ways to do it, but this
;; is very simple and does the trick for me.

(defvar before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")

(message "Loading Emacs, pre-init...done (%.3fs)"
         (float-time (time-subtract before-user-init-time
                                    before-init-time)))

(message "Loading %s..." user-init-file)


;;; Package Management

;; Packages are configured first so that when you open this config on a new
;; machine, all the packages needed to make it work are in
;; `package-selected-packages' and you can quickly install them with
;; `package-install-selected-packages'. At which point the whole config should
;; be ready to rock.

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")))

(setq package-archive-priorities '(("gnu" . 30)
                                   ("melpa-stable" . 20)
                                   ("melpa" . 10)
                                   ("gnu-devel" . 0)))

(setq pkg-ops-map
  (let ((map (make-sparse-keymap "Packages")))
    (define-key map "h" '("describe" . describe-package))
    (define-key map "a" '("autoremove" . package-autoremove))
    (define-key map "d" '("delete" . package-delete))
    (define-key map "i" '("install" . package-install))
    (define-key map "s" '("selected" . package-install-selected-packages))
    (define-key map "r" '("refresh" . package-refresh-contents))
    (define-key map "l" '("list" . list-packages))
    map))

(global-set-key (kbd "C-c p") pkg-ops-map)

(setq package-selected-packages
      '(consult
        dash
        delight
        embark
        embark-consult
        exec-path-from-shell
        expand-region
        fountain-mode
        gruvbox-theme
        isearch-mb
        lua-mode
        magit
        marginalia
        markdown-mode
        modus-themes
        olivetti
        orderless
        org
        selected
        vertico
        visual-regexp
        visual-regexp-steroids
        ytdl
        transient))

(defun package-menu-filter-by-status (status)
  ;; https://github.com/jcs090218/jcs-emacs/blob/38cce9fc9046ef436c59e13d9942a719dc1e8f2e/.emacs.jcs/jcs-package.el#L582
  "Filter the *Packages* buffer by STATUS."
  (interactive
   (list (completing-read
          "Status: " '("ALL"
                       "available"
                       "built-in"
                       "dependency"
                       "incompat"
                       "installed"
                       "new"
                       "obsolete"))))
  (if (string= status "ALL")
      (package-list-packages)
    (package-menu-filter (concat "status:" status))))

(define-key package-menu-mode-map (kbd "/ s") 'package-menu-filter-by-status)


;;; Mitsuharu Yamamoto Emacs

;; I use Mitsuharu Yamamoto’s fork of Emacs. Here are some modifications
;; specific to that.

;; Setup modifier keys
(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

;; Turn off swiping to switch buffers (defined in mac-win.el)
(global-unset-key [swipe-left])
(global-unset-key [swipe-right])


;;; Critical Setup

;; These variables, packages, macros, and functions are used throughout the
;; config --- and are required for it to work correctly.

(defvar user-dotemacs-directory  "~/home/dot/emacs/")
(defvar user-orgfiles-directory  "~/home/org/")
(defvar user-downloads-directory "~/Downloads/")

(add-to-list 'load-path (concat user-dotemacs-directory "lisp/"))
(require 'undo-backport)
(require 'radian-directories)

(autoload 'transient-define-prefix "transient" nil t)
(require 'dash)
(exec-path-from-shell-initialize)

(defun define-keys (keymap &rest pairs)
  "Define alternating key-def PAIRS for KEYMAP."
  ;; https://github.com/RioZRon/dotspace/blob/master/layers/macros/local/macros/macros.el
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (if (stringp key)
          (define-key keymap (read-kbd-macro key) def)
        (define-key keymap key def)))))

(defun global-set-keys (&rest pairs)
  "Set alternating key-def PAIRS globally."
  ;; https://github.com/RioZRon/dotspace/blob/master/layers/macros/local/macros/macros.el
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (if (stringp key)
          (global-set-key (read-kbd-macro key) def)
        (global-set-key key def)))))

(defmacro elisp-group (name doc &rest body)
  "Group elisp by wrapping in progn.

This is a silly macro, all it does is wrap the body in `progn'.
But it allows you to neatly group small bits of elisp in your
config and include a docstring. Mildly convenient."
  (declare (indent defun)
           (doc-string 2))
  `(progn ,@body))


;;; Customizations

;; Save all interactive customization to a temp file, which is never loaded.
;; This means interactive customization is session-local. Only this init file persists sessions.
(setq custom-file (make-temp-file "emacs-custom-"))

;; For most of my "settings" I use custom-set-variables, which does a bunch of neat stuff.
;; First, it calls a variable's "setter" function, if it has one.
;; Second, it can activate modes as well as set variables.
;; Third, it takes care of setting the default for buffer-local variables correctly.
;; https://with-emacs.com/posts/tutorials/almost-all-you-need-to-know-about-variables/#_user_options
;; https://old.reddit.com/r/emacs/comments/exnxha/withemacs_almost_all_you_need_to_know_about/fgadihl/
(custom-set-variables
 '(inhibit-startup-screen t)
 '(global-auto-revert-mode t)
 '(ibuffer-auto-mode t)
 '(save-place-mode t)
 '(recentf-mode t)
 '(winner-mode t)
 '(show-paren-mode t)
 '(blink-cursor-mode t)
 '(cursor-type 'box)
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
 '(confirm-kill-processes nil)
 '(save-interprogram-paste-before-kill t)
 '(kill-do-not-save-duplicates t)
 '(split-window-keep-point nil)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(mark-even-if-inactive nil)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(fill-column 78)
 '(locate-command "mdfind")
 '(trash-dircetory "~/.Trash"))


;;; Misc Functions

;; Some of these functions I wrote myself, many of them I copied (and perhaps
;; modified) from other people's configs.

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

(defun rotate-windows (count)
  "Rotate your windows.
Dedicated windows are left untouched. Giving a negative prefix
argument makes the windows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

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

(defun find-user-init-file ()
  "Find the user-init-file."
  (interactive)
  (find-file user-init-file))

(defun find-file-recursively (&optional path)
  "Find Files Recursively using completing read.
Uses the `default-directory' unless a path is supplied."
  (interactive)
  (find-file (completing-read "Find File Recursively: "
                              (directory-files-recursively (if path path default-directory) ".+"))))

(defalias 'find-files-recursively 'find-file-recursively)

(defun browse-url-macos-background (url)
  "Open URL with macOS `open'."
  (interactive)
  (start-process "open url"
                 nil "open" "--background" url))

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

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))

(defun unfill-paragraph ()
  "Remove all newlines from paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  "Remove all newlines from paragraphs in region."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil t)))

(defun unfill-dwim ()
  "If there's a region, `unfill-region', otherwise `unfill-paragraph'."
  (interactive)
  (if (use-region-p)
      (unfill-region)
    (unfill-paragraph)))

(defun crux-open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

(defun crux-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-file-name "New name: " (file-name-directory filename)))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun crux-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun olivertaylor.net ()
  "Helpful stuff for coding my website."
  (interactive)
  (load "~/home/src/olivertaylor/lib/helper.el")
  (oht-site-transient))

(defun frames-p-save-buffers-kill-emacs ()
  "If more than one frame exists, confirm exit of Emacs."
  (interactive)
  (if (nth 1 (frame-list))
      (if (y-or-n-p "Multiple frames exist; exit anyway?")
          (save-buffers-kill-emacs)
        nil)
    (save-buffers-kill-emacs)))

(defun ora-move-beginning-of-line ()
  "Move back to indentation or beginning of line."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(defun split-window-dwim ()
  "Interactive wrapper around `split-window-sensibly'."
  (interactive)
  (split-window-sensibly))


;;; Personal Keybindings

;; Minor modes override global bindings, so any bindings you don't want
;; overridden should be placed in a minor mode. I stole this technique from
;; the `bind-key' package.

(defvar bosskey-mode-map (make-sparse-keymap))

(define-minor-mode bosskey-mode
  "Minor mode for my personal keybindings, which override others.
The only purpose of this minor mode is to override global keybindings.
Keybindings you define here will take precedence."
  :init-value t
  :global t
  :keymap bosskey-mode-map)

(add-to-list 'emulation-mode-map-alists
             `((bosskey-mode . ,bosskey-mode-map)))

;; https://www.reddit.com/r/emacs/comments/67rlfr/esc_vs_cg/dgsozkc/
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Mac-like bindings
(define-keys bosskey-mode-map
  "s-q"       'frames-p-save-buffers-kill-emacs
  "s-m"       'iconify-frame
  "s-n"       'new-buffer
  "s-N"       'make-frame-command
  "s-s"       'save-buffer
  "s-,"       'find-user-init-file
  "s-z"       'undo-only
  "s-Z"       'undo-redo
  "s-x"       'kill-region
  "s-c"       'kill-ring-save
  "s-v"       'yank
  "s-<left>"  'beginning-of-visual-line
  "s-<right>" 'end-of-visual-line
  "s-<up>"    'beginning-of-buffer
  "s-<down>"  'end-of-buffer)

;; Emacs keybinding tweaks
(define-keys bosskey-mode-map
  "s-<return>" 'general-transient
  "s-S-<return>" 'call-mode-help-transient
  "s-]"        'next-buffer
  "s-["        'previous-buffer
  "s-w"        'general-transient--window
  "s-k"        'org-capture
  "s-f"        'find-file
  "s-F"        'find-file-other-window
  "s-b"        'consult-buffer
  "s-B"        'consult-buffer-other-window
  "C-M-h"      'mark-line
  "M-."        'embark-act
  "M-'"        'completion-at-point
  "M-\\"       'cycle-spacing
  "M-z"        'zap-up-to-char
  "M-<SPC>"    'push-mark-no-activate
  "C-="        'er/expand-region
  "C-d"        'delete-forward-char
  "C-x C-x"    'exchange-point-and-mark-dwim
  "C-x k"      'kill-buffer-dwim)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; These should be overridden when appropriate:
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "C-a") 'ora-move-beginning-of-line)

(global-set-keys [remap query-replace] 'vr/query-replace
                 [remap capitalize-word] 'capitalize-dwim
                 [remap downcase-word]   'downcase-dwim
                 [remap upcase-word]     'upcase-dwim
                 ;; -- Mouse + Mode Line Magic
                 [mode-line S-mouse-1] 'mouse-delete-other-windows
                 [mode-line M-mouse-1] 'mouse-delete-window
                 [mode-line C-mouse-1] 'mouse-split-window-horizontally
                 ;; -- Make shift-click extend the region.
                 [S-down-mouse-1] 'ignore
                 [S-mouse-1] 'mouse-save-then-kill
                 ;; -- Use M-drag-mouse-1 to create rectangle regions.
                 [M-down-mouse-1] #'mouse-drag-region-rectangle
                 [M-drag-mouse-1] #'ignore
                 [M-mouse-1]      #'mouse-set-point)


;;; Light/Dark Themes

;; By default I use a light theme, but sometimes (when all the lights are off)
;; I use a dark theme. The below are some functions that allow me to quickly
;; switch between light and dark themes.

(defvar light-theme nil
  "Preferred light-theme.")

(defvar dark-theme nil
  "Preferred dark-theme.")

(defvar default-theme-color 'light
  "Default theme to load, accepts 'light and 'dark.")

(defvar current-theme-color nil
  "Is the current theme color light or dark?")

(defun disable-current-themes nil
  "Disables all currently enabled themes."
  (mapcar 'disable-theme custom-enabled-themes))

(defun load-theme-color (color)
  "Load theme based on `dark-theme' and `light-theme'."
  (interactive
   (list (completing-read
          "Theme Color: " '("dark" "light" "toggle"))))
  (disable-current-themes)
  (if (string= color "dark")
      (progn
        (setq current-theme-color 'dark)
        (load-theme dark-theme t))
    (if (string= color "light")
        (progn
          (setq current-theme-color 'light)
          (load-theme light-theme t))
      (theme-color-toggle))))

(defun theme-color-toggle nil
  "Toggle between `light-theme' and `dark-theme'."
  (interactive)
  (if (eq current-theme-color 'light)
      (progn
        (disable-current-themes)
        (setq current-theme-color 'dark)
        (load-theme dark-theme t))
    (progn
      (disable-current-themes)
      (setq current-theme-color 'light)
      (load-theme light-theme t))))

(defun macos-appearance-dark nil
  "Return color of macOS appearance, t is dark, nil is light."
   (string= (plist-get (mac-application-state) :appearance) "NSAppearanceNameDarkAqua"))

(add-hook 'mac-effective-appearance-change-hook 'theme-color-toggle)


;;; Theme and Fonts Config

(setq light-theme 'modus-operandi)
(setq dark-theme  'gruvbox)
(setq default-theme-color 'light)

(elisp-group modus-themes-config
  "Require, cofigure, and load modus-themes."
  (require 'modus-themes)
  (custom-set-variables
   '(modus-themes-italic-constructs t)
   '(modus-themes-links '(neutral-underline))
   '(modus-themes-mode-line '(accented))
   '(modus-themes-prompts '(bold))
   '(modus-themes-completions 'moderate)
   '(modus-themes-region '(bg-only))
   '(modus-themes-org-blocks '(gray-background)))
  (modus-themes-load-themes))

(elisp-group set-theme-on-startup
  "On startup, try to match system color, otherwise load user preference."
  (if (ignore-errors (macos-appearance-dark))
      (load-theme-color 'dark)
    (if (eq default-theme-color 'light)
        (load-theme-color 'light)
      (load-theme-color 'dark))))

(setq text-scale-mode-step 1.09)

(custom-set-variables
 '(facedancer-monospace-family "SF Mono")
 '(facedancer-monospace-height 12)
 '(facedancer-variable-family  "SF Pro Text")
 '(facedancer-variable-height  13)
 '(facedancer-mode-line-family "SF Compact Text")
 '(facedancer-mode-line-height 14))


;;; Mode-Line

(delight 'eldoc-mode nil "eldoc")
(delight 'emacs-lisp-mode "Elisp" "elisp-mode")
(delight 'auto-fill-function " Fill" "simple")

(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(defalias 'hide-mode-line-mode 'hidden-mode-line-mode)


;;; Facedancer

;; Facedancer defines a group of user options which set various attributes of
;; the default, fixed-pitch, and variable-pitch faces. Each option should be
;; set either via the `customize' interface or by calling
;; `custom-set-variables' in your init file as each option has "setter"
;; functions.

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


;;; Secondary Selection

;; Emacs's Secondary Selection assumes you only want to interact with it via
;; the mouse, however it is perfectly possible to do it via the keyboard, all
;; you need is some wrapper functions to make things keybinding-addressable.
;; I've also included a set of mouse-bindings and a transient to make things
;; easier still.

(defun kill-secondary ()
  "Kill the secondary selection."
  (interactive)
  (mouse-kill-secondary))

(defun kill-ring-save-secondary ()
  "Save the secondary selection to the kill ring."
  (interactive)
  ;; there isn't a keybinding-addressable function to kill-ring-save
  ;; the 2nd selection so here I've made my own. This is extracted
  ;; directly from 'mouse.el:mouse-secondary-save-then-kill'
  (kill-new
   (buffer-substring (overlay-start mouse-secondary-overlay)
                     (overlay-end mouse-secondary-overlay))
   t))

(defun kill-secondary-yank ()
  "Kill the secondary selection and yank at point."
  (interactive)
  (mouse-kill-secondary)
  (yank))

(defun kill-ring-save-secondary-yank ()
  "Save the secondary selection to kill ring and yank at point."
  (interactive)
  (oht/copy-secondary-selection)
  (yank))

(defun mark-region-as-secondary ()
  "Mark the region as the secondary selection."
  (interactive)
  (secondary-selection-from-region))

(defun mark-secondary ()
  "Mark the secondary selection."
  (interactive)
  (secondary-selection-to-region))

(defun deactivate-secondary ()
  "Deactivate the secondary selection."
  (interactive)
  (delete-overlay mouse-secondary-overlay))

(global-set-keys [C-M-mouse-1]      'mouse-start-secondary
                 [C-M-drag-mouse-1] 'mouse-set-secondary
                 [C-M-down-mouse-1] 'mouse-drag-secondary)

(transient-define-prefix secondary-selection-transient ()
  "Transient for working with the secondary selection"
  [["Cut/Copy"
    ("xx" "Cut 2nd" kill-secondary)
    ("cc" "Copy 2nd" kill-ring-save-secondary)]
   ["& Paste"
    ("xv" "Cut 2nd & Paste" kill-secondary-yank)
    ("cv" "Copy 2nd & Paste" kill-ring-save-secondary-yank)]
   ["Mark"
    ("m"  "Mark Region as 2nd" mark-region-as-secondary)
    ("g"  "Make 2nd the Region" mark-secondary)
    ("d"  "Delete 2nd" deactivate-secondary)]])


;;; Dedicated Mode

(define-minor-mode dedicated-mode
  "Minor mode for dedicating windows.
This minor mode dedicates the current window to the current buffer.
The code is taken from here: https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el"
  :init-value nil
  :lighter " [D]"
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))


;;; Confirm Killing Modified Buffers

;; Emacs only prompts the user for confirmation when killing a file-visiting
;; buffer. Non-file-visiting buffers are killed immediately without
;; confirmation.
;;
;; There is a variable, buffer-offer-save, which will make Emacs prompt the
;; user to save modified non-file-visiting buffers when exiting Emacs, but no
;; such option exists for killing buffers.
;;
;; The below adds a variable you can set buffer-locally, and adds a function
;; to the buffer-killing process.

(defvar-local buffer-confirm-kill nil
  "Non-nil means confirm killing buffer when modified.
Variable is checked by `buffer-confirm-kill-p'.")

(defun buffer-confirm-kill-p ()
  "Return nil if buffer is modified and `buffer-confirm-kill' is t.
This function is designed to be called from `kill-buffer-query-functions'."
  (if (and (buffer-modified-p)
           buffer-confirm-kill)
      (yes-or-no-p
       (format "Buffer %S is modified; kill anyway? " (buffer-name)))
    t))

(add-hook 'kill-buffer-query-functions #'buffer-confirm-kill-p)


;;; Scratch Buffers

;; It is sometimes useful to quickly create a scratch buffer in markdown- or
;; org-mode (for editing text you're going to paste into a reddit
;; comment/post, for example).

(defvar scratch-markdown-initial-message nil
  "Message to be inserted in markdown scratch buffer.")

;; Add date/time to scratch-org-buffer message
(setq scratch-markdown-initial-message
      (concat "<!-- Scratch Buffer for Markdown Mode"
              " -- " (format-time-string "%Y-%m-%d %a %H:%M -->\n\n")))

(defvar scratch-markdown-buffer "*scratch-markdown*"
  "Name of markdown scratch buffer.")

(defun scratch-buffer-markdown ()
  "Create a *scratch* buffer in Markdown Mode and switch to it."
  (interactive)
  (let ((buf scratch-markdown-buffer))
    (if (get-buffer buf)
        (switch-to-buffer buf)
      (progn
        (switch-to-buffer buf)
        (markdown-mode)
        (with-current-buffer buf
          (setq-local buffer-confirm-kill t)
          (setq-local buffer-offer-save t)
          (insert scratch-markdown-initial-message)
          (not-modified))))))

(defvar scratch-org-initial-message nil
  "Message to be inserted in org scratch buffer.")

;; Add date/time to scratch-org-buffer message
(setq scratch-org-initial-message
      (concat "# Scratch Buffer for Org Mode\n"
              "# " (format-time-string "[%Y-%m-%d %a %H:%M]\n\n")))

(defvar scratch-org-buffer "*scratch-org*"
  "Name of org-mode scratch buffer.")

(defun scratch-buffer-org ()
  "Create a *scratch* buffer in Org Mode and switch to it."
  (interactive)
  (let ((buf scratch-org-buffer))
    (if (get-buffer buf)
        (switch-to-buffer buf)
      (progn
        (switch-to-buffer buf)
        (org-mode)
        (with-current-buffer buf
          (setq-local buffer-confirm-kill t)
          (setq-local buffer-offer-save t)
          (insert scratch-org-initial-message)
          (not-modified))))))

(defun new-buffer (name)
  "Create a new untitled buffer."
  (interactive (list (read-string "Create buffer (default \"untitled\"): " nil nil "untitled")))
  (let ((buffer (generate-new-buffer name)))
    (switch-to-buffer buffer)
    (setq-local buffer-offer-save t)
    (setq-local buffer-confirm-kill t)))


;;; Prelude Search

;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el

(defun prelude-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro prelude-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "prelude-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (prelude-search ,search-engine-url ,search-engine-prompt)))

(prelude-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(prelude-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(prelude-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(prelude-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

(prelude-install-search-engine "wikipedia"
                               "http://en.wikipedia.org/wiki/Special:Search?go=Go&search="
                               "Search Wikipedia: ")

(prelude-install-search-engine "github-elisp"
                               "https://github.com/search?l=Emacs+Lisp&type=Code&q="
                               "Github Elisp: ")


;;; Outline

(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

(delight 'outline-minor-mode " Out" "outline")

(defun outline-up-transient--hide nil
  "Move to parent heading, hide to that level, and enter outlined-transient."
  (interactive)
  (when (not (outline-on-heading-p))
    (outline-up-heading 1 nil))
  (outline-hide-sublevels (outline-level))
  (outline-transient))

(defun outline-transient-dwim nil
  "Move to parent heading, if not on heading, and enter outline-transient."
  (interactive)
  (when (not (outline-on-heading-p))
    (outline-up-heading 1 nil))
  (outline-transient))

(with-eval-after-load 'outline
  (define-key outline-minor-mode-map (kbd "S-<tab>") 'outline-up-transient--hide)
  (define-key outline-minor-mode-map (kbd "C-<tab>") 'outline-transient-dwim))

(transient-define-prefix outline-transient ()
  "Transient for Outline Minor Mode navigation"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-stay
  [["Show/Hide"
    ("<right>" "Show Subtree" outline-show-subtree)
    ("<left>" "Hide Subtree" outline-hide-subtree)
    ("o" "Hide to This Sublevel" outline-hide-sublevels)
    ("a" "Show All" outline-show-all)]
   ["Navigate"
    ("<down>" "Next" outline-forward-same-level)
    ("<up>" "Previous" outline-backward-same-level)]
   ["Edit"
    ("M-<left>"  "Promote" outline-promote)
    ("M-<right>" "Demote"  outline-demote)
    ("M-<up>"    "Move Up" outline-move-subtree-up)
    ("M-<down>"  "Move Down" outline-move-subtree-down)]
   ["Other"
    ("s-z" "Undo" undo-only)
    ("s-Z" "Redo" undo-redo)
    ("c" "Consult" consult-outline :transient nil)]])


;;; Navigation Keymap

;; There are a few keymaps which I think should share common movement
;; bindings. To facilitate this I've made a command which you can pass a
;; keymap and those common bindings will be set in the keymap. In my case,
;; this allows me to share a common set of bindings between a custom
;; navigation keymap and the `selected' package.

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

(defvar navigation-keymap (make-sparse-keymap)
  "Transient keymap for navigating buffers.")

(define-key navigation-keymap [t] 'undefined)

;; Assign navigation-keys to the map
(define-navigation-keys navigation-keymap)

(defvar nagivation-keymap-header-text
  (propertize " ** Navigation Keymap ACTIVE **" 'face 'warning)
  "Text to display in the buffer's header line when navigation keymap is active.")

(defun navigation-keymap-set-header nil
  (setq-local og-header-line-format header-line-format)
  (setq-local header-line-format (concat nagivation-keymap-header-text og-header-line-format)))

(defun navigation-keymap--activate ()
  "Activate the navigation-keymap transiently and add modify header-line."
  (interactive)
  (navigation-keymap-set-header)
  (scroll-up-line)
  (message "Navigation Keymap Activated")
  (set-transient-map navigation-keymap t 'navigation-keymap--deactivate))

(defun navigation-keymap--deactivate ()
  "Deactivate the navigation-keymap transiently and restore header-line."
  (interactive)
  (setq-local header-line-format og-header-line-format)
  (scroll-down-line)
  (message "Navigation Keymap Deactivated"))


;;; Selected

;; One of the more sublime packages I've stumbled across, `selected' creates a
;; keymap that is active any time (and only when) the region is active.
;; Wonderful for quickly acting on the active region.
;;
;; When combined with my navigation keymap the two act like a very lightweight
;; vim emulation, but in an entirely emacs-y way.

(delete-selection-mode -1)

(selected-global-mode 1)

(defun disable-selected-minor-mode ()
  "Disabled the selected minor mode."
  (selected-minor-mode -1))

(with-eval-after-load 'selected
  ;; Careful not to bind - or = as they may collide with `expand-region'.
  (define-keys selected-keymap
    "u" 'upcase-dwim
    "d" 'downcase-dwim
    "c" 'capitalize-dwim
    "w" 'kill-ring-save
    "|" 'pipe-region
    "R" 'replace-rectangle
    "E" 'eval-region
    "q" 'fill-paragraph)
   (define-navigation-keys selected-keymap)
   (delight 'selected-minor-mode nil "selected"))


;;; Minibuffer / Embark / Consult

(custom-set-variables
 '(completion-cycle-threshold nil)
 '(enable-recursive-minibuffers t)
 '(savehist-mode t))

(elisp-group orderless
  nil
  (require 'orderless)
  (custom-set-variables
   '(completion-styles '(orderless))
   '(completion-category-defaults nil)
   '(completion-category-overrides '((file (styles . (partial-completion)))))))

(vertico-mode)

(elisp-group marginalia
  nil
  (marginalia-mode)
  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle))

(elisp-group embark
  nil
  (require 'embark)
  (require 'embark-consult)

  (custom-set-variables
   '(embark-indicator 'embark-verbose-indicator)
   '(embark-verbose-indicator-display-action
     '(display-buffer-below-selected (window-height . fit-window-to-buffer))))

  (set-face-attribute 'embark-verbose-indicator-title nil :height 1.0)

  (setq prefix-help-command 'embark-prefix-help-command)

  (define-key bosskey-mode-map (kbd "C-h b") 'embark-bindings)

  (define-keys embark-file-map
    "O" 'crux-open-with
    "j" 'dired-jump)

  (define-keys embark-url-map
    "d" 'ytdl-download
    "b" 'browse-url-default-macosx-browser))

(elisp-group isearch-extras
  nil
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

  (isearch-mb-mode)
  (add-to-list 'isearch-mb--after-exit #'occur))

(elisp-group consult
  nil
  (custom-set-variables
   '(consult-find-command "fd --color=never --full-path ARG OPTS"))

  (consult-customize consult-line
                     :preview-key nil)

  (setq completion-in-region-function 'consult-completion-in-region)
  (consult-customize consult-completion-in-region
                     :cycle-threshold 3)

  (global-set-key [remap yank-pop] 'consult-yank-pop))


;;; Miscellaneous

(with-eval-after-load 'visual-regexp
  (with-eval-after-load 'visual-regexp-steroids
    (custom-set-variables
     '(vr/engine 'pcre2el))))

(elisp-group fountain-mode
  "Settings for fountain-mode."
  (custom-set-variables
   '(fountain-add-continued-dialog nil)
   '(fountain-highlight-elements (quote (section-heading)))))

(elisp-group markdown
  nil
  (add-to-list 'magic-mode-alist
               '("%text" . markdown-mode))
  (add-to-list 'auto-mode-alist
               '("\\.text" . markdown-mode)))

(elisp-group oblique
  "Config for oblique package"
  (add-to-list 'load-path "~/home/src/lisp/oblique-strategies/")
  (autoload 'oblique-strategy "oblique")
  (setq initial-scratch-message (concat
                                 ";; Welcome to Emacs!\n;; This is the scratch buffer, for unsaved text and Lisp evaluation.\n"
                                 ";; Oblique Strategy: " (oblique-strategy) "\n\n")))

(with-eval-after-load 'flyspell
  (delight 'flyspell-mode " Spell" "flyspell"))
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(with-eval-after-load 'dired
  (setq dired-use-ls-dired nil)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (define-keys dired-mode-map
    "O"   'crux-open-with
    "C-/" 'dired-undo))

(custom-set-variables
 '(olivetti-body-width 86))

(setq ytdl-media-player "open")
(setq ytdl-always-query-default-filename 'yes-confirm) ; Get filename from server

(elisp-group vundo
  "vundo creates a tree-like visualization of your undo history
using only standard Emacs undo commands and data. Requires either
Emacs 28 or its backported undo functions."
  (add-to-list 'load-path "~/home/src/lisp/vundo/")
  (require 'vundo))


;;; Org

(defun find-org-files ()
  "Find org files in your org directory, pass to completing-read."
  (interactive)
  (find-file (completing-read "Find Org Files: "
                              (directory-files-recursively user-orgfiles-directory "\.org$"))))

(defun find-org-directory ()
  "Open org directory in dired."
  (interactive)
  (find-file user-orgfiles-directory))

(defun consult-grep-orgfiles ()
  (interactive)
  (consult-grep user-orgfiles-directory))


(autoload 'oht-org-agenda-today-pop-up "org")
(autoload 'oht-org-agenda-today "org")
(autoload 'consult-grep-orgfiles "org")
(autoload 'find-org-directory "org")

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
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-startup-with-inline-images t
        org-image-actual-width '(600)
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
        org-agenda-sorting-strategy '(((agenda habit-down time-up category-up priority-down)
                                       (todo priority-down category-up)
                                       (tags priority-down category-keep)
                                       (search category-keep))))

  (setq org-agenda-files (list user-orgfiles-directory))

  (add-to-list 'org-agenda-files "~/home/writing/kindred/compendium.org")

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
           (file+headline ,(concat user-orgfiles-directory "life.org") "Inbox")
           "* %?\n\n" :empty-lines 1)
          ("pl" "Personal Log Entry" entry
           (file+olp+datetree ,(concat user-orgfiles-directory "logbook.org"))
           "* %?\n%T\n\n" :empty-lines 1 :tree-type month )
          ;; -----------------------------
          ("s" "Scanline")
          ("si" "Scanline Inbox" entry
           (file+headline ,(concat user-orgfiles-directory "scanline.org") "Inbox")
           "* %?\n\n" :empty-lines 1)
          ("sf" "Scanline FRANK Inbox" entry
           (file+headline ,(concat user-orgfiles-directory "scanline.org") "FRANK Inbox")
           "* %?\n\n" :empty-lines 1)
          ("sz" "Scanline ZERO Inbox" entry
           (file+headline ,(concat user-orgfiles-directory "scanline.org") "ZERO Inbox")
           "* %?\n\n" :empty-lines 1)
          ("sl" "Scanline Log Entry" entry
           (file+olp+datetree ,(concat user-orgfiles-directory "scanline_logbook.org"))
           "* %?\n%T\n\n" :empty-lines 1 :tree-type week )
          ;; -----------------------------
          ("e" "Emacs Config" entry
           (file+headline ,(concat user-orgfiles-directory "emacs.org") "Emacs Config")
           "* TODO %?" :empty-lines 1)
          ("k" "Kiddos Log Entry" entry
           (file+olp+datetree ,(concat user-orgfiles-directory "kiddos_logbook.org"))
           "* %T\n\n%?" :empty-lines 1 :tree-type month )))

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

  (setq org-todo-map
    (let ((map (make-sparse-keymap "Org TODO")))
      (define-key map "t" '("TODO"     . org-todo-set-todo))
      (define-key map "l" '("LATER"    . org-todo-set-later))
      (define-key map "d" '("DONE"     . org-todo-set-done))
      (define-key map "c" '("CANCELED" . org-todo-set-canceled))
      map))

  ;; replace the regular binding with the above map
  (define-key org-mode-map (kbd "C-c C-t") org-todo-map)

  (defun echo-area-tooltips ()
    "Show tooltips in the echo area automatically for current buffer."
    (setq-local help-at-pt-display-when-idle t
                help-at-pt-timer-delay 0)
    (help-at-pt-cancel-timer)
    (help-at-pt-set-timer))

  (add-hook 'org-mode-hook #'echo-area-tooltips)

  ) ; End org config

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(with-eval-after-load 'org-agenda
  (setq org-agenda-todo-map
    (let ((map (make-sparse-keymap "Org Agenda TODO")))
      (define-key map "t" '("TODO"     . org-agenda-todo-set-todo))
      (define-key map "l" '("LATER"    . org-agenda-todo-set-later))
      (define-key map "d" '("DONE"     . org-agenda-todo-set-done))
      (define-key map "c" '("CANCELED" . org-agenda-todo-set-canceled))
      map))

  (define-key org-agenda-mode-map (kbd "S") 'org-agenda-schedule)
  (define-key org-agenda-mode-map (kbd "s-z") 'org-agenda-undo)
  (define-key org-agenda-mode-map (kbd "C-/") 'org-agenda-undo)
  (define-key org-agenda-mode-map (kbd "t") org-agenda-todo-map))


;;; Transient

(transient-define-prefix general-transient ()
  "General-purpose transient."
  [["Actions/Toggles"
    ("x" "M-x" execute-extended-command)
    ("a" "AutoFill" auto-fill-mode)
    ("j" "Dired Jump" dired-jump)
    ("SPC" "Mark..." general-transient--mark)
    ("n" "Navigation..." navigation-keymap--activate)
    ("m" "Mode Transient..." call-mode-help-transient)]
   [""
    ("." "Repeat Command" repeat-complex-command)
    ("k" "Kill Buffer" kill-buffer-dwim)
    ("b" "Switch Buffer" switch-to-buffer)
    ("s-b" "iBuffer" ibuffer)]
   ["Transients"
    ("o" "Org..." general-transient--org)
    ("t" "Toggle..." general-transient--toggles)
    ("w" "Windows..." general-transient--window)
    ("c" "Consult..." general-transient--consult)]
   [""
    ("0" "Outline..." outline-transient)
    ("2" "Secondary..." secondary-selection-transient)
    ("f" "Fonts..." general-transient--fonts)
    ("s" "Spelling..." flyspell-mode-transient)]])

(transient-define-prefix general-transient--org ()
  "Transient for Org commands useful outside org mode."
  ["Org Mode"
   ["Agenda Commands"
    ("t" "Today" oht-org-agenda-today)
    ("p" "Today (pop-up)" oht-org-agenda-today-pop-up)
    ("0" "Complete" oht-org-agenda-complete)
    ("a" "Agenda..." org-agenda)]
   ["Find"
    ("d" "Find Org Dir" find-org-directory)
    ("D" "Find Org Files..." find-org-files)
    ("f" "Find Org Heading" consult-org-agenda)
    ("g" "Grep Org Files" consult-grep-orgfiles)]
   ["Other"
    ("k" "Capture" org-capture)
    ("s" "Store Link" org-store-link)
    ]])

(transient-define-prefix general-transient--toggles ()
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
    ("<return>" "Quit" transient-quit-all)
    ("q" "Quit" transient-quit-all)]])

(transient-define-prefix general-transient--consult ()
  ["Consult"
   ("l" "Line" consult-line)
   ("o" "Outline" consult-outline)
   ("g" "Grep" consult-grep)
   ("b" "Buffer" consult-buffer)
   ("a" "Apropos" consult-apropos)
   ("m" "Marks" consult-mark)
   ("M" "Minor Modes" consult-minor-mode-menu)])

(transient-define-prefix general-transient--fonts ()
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
    ("m" "Theme Color Toggle" theme-color-toggle)]])

(transient-define-prefix general-transient--window ()
  "Most commonly used window commands"
  [["Splits"
    ("s" "Split Sensibly" split-window-dwim)
    ("H" "Split Horizontal" split-window-below)
    ("V" "Split Vertical"   split-window-right)
    ("b" "Balance"    balance-windows)
    ("f" "Fit"        fit-window-to-buffer)
    ("r" "Rotate"     toggle-window-split)
    ("R" "Swap"       rotate-windows)]
   ["Window"
    ;; TODO: https://www.gnu.org/software/emacs/manual/html_node/emacs/Configuration-Registers.html
    ("d" "Dedicate Window" dedicated-mode)
    ("c" "Clone Indirect" clone-indirect-buffer)
    ("t" "Tear Off" tear-off-window)
    ("k" "Kill" delete-window)
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

(transient-define-prefix general-transient--mark ()
  "Transient for setting the mark."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  ["Mark"
   [("w" "Word" mark-word)
    ("s" "Sexp" mark-sexp)
    ("d" "Defun" mark-defun)]
   [("n" "Line" mark-line)
    (")" "Sentence" mark-sentence)
    ("}" "Paragraph" mark-paragraph)]
   [("<" "Beginning of Buffer" mark-beginning-of-buffer)
    (">" "End of Buffer" mark-end-of-buffer)]
   [("x" "Exchange Point/Mark" exchange-point-and-mark :transient nil)
    ("q" "Quit" transient-quit-all)]])

(transient-define-prefix transpose-transient ()
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

(with-eval-after-load 'flyspell
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
      ("C-/" "Undo" undo-only)
      ("M-/" "Redo" undo-redo)]]))


;;;; Mode Help Transients

;; Emacs has so many modes. Who can remember all the commands? These
;; mode-specific transients are designed to help with that.

(defun call-mode-help-transient ()
  "Call a helpful transient based on the mode you're in."
  (interactive)
  (if (progn
        (when (derived-mode-p 'org-mode)
          (org-mode-help-transient))
        (when (derived-mode-p 'Info-mode)
          (info-mode-help-transient))
        (when (derived-mode-p 'dired-mode)
          (dired-mode-help-transient)))
      nil ; if the above succeeds, do nothing else, otherwise...
    (message "No transient defined for this mode.")))

(with-eval-after-load 'org
  (transient-define-prefix org-mode-help-transient ()
    "Transient for Org Mode"
    ["Org Mode"
     ["Navigation"
      ("o" "Outline" consult-org-heading)
      ("f" "Find Heading" consult-org-agenda)
      ("n" "Narrow/Widen" narrow-or-widen-dwim)
      ("m" "Visible Markup" visible-mode)]
     ["Item"
      ("t" "TODO" org-todo)
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
      ("i" "Insert Link" org-insert-last-stored-link)]]))

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


;;; Wrap-up

;; Restore garbage collection to a reasonable value.
;; This is step 2, step 1 is in early-init.
;; In my case this saves about .3 seconds in startup time.
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

(message "Loading init file...done (%.3fs)"
         (float-time (time-subtract (current-time)
                                    before-user-init-time)))


;;; End of init.el
