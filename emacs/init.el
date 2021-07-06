;;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Copyright (C) 2021 Oliver Taylor

;; Author: Oliver Taylor
;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; This file has an outline which can be viewed by looking at comments
;; starting with three or more semicolons. `outline-minor-mode' supports this
;; convention by default and helps with navigation. You can also create an
;; occur buffer with the search [^;;;+].


;;; Package Management

;; Packages are configured first so that then you open this config one a new
;; machine, all the packages needed to make it work are in
;; `package-selected-packages' and you can quickly install them all with
;; `package-install-selected-packages'. At which point the whole config should
;; be ready to rock.

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archive-priorities '(("gnu" . 20)("melpa" . 10)))

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
      '(dash
        transient
        blackout
        modus-themes
        isearch-mb
        bicycle
        orderless
        vertico
        marginalia
        embark
        consult
        embark-consult
        selected
        expand-region
        undo-fu
        helpful
        visual-regexp
        visual-regexp-steroids
        olivetti
        fountain-mode
        markdown-mode
        tron-legacy-theme
        lua-mode
        org))

(when (eq system-type 'darwin)
  (add-to-list 'package-selected-packages 'magit t))

(when (string= (system-name) "shadowfax.local")
  (add-to-list 'package-selected-packages 'oblique t))


;;; Macros & Critical Functions

;; These packages, macros, and functions are used throughout the config and
;; are required for it to work correctly.

(autoload 'transient-define-prefix "transient" nil t)
(require 'dash)

(defun define-keys (keymap &rest pairs)
  "Define alternating key-def PAIRS for KEYMAP."
  ; https://github.com/RioZRon/dotspace/blob/master/layers/macros/local/macros/macros.el
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (define-key keymap key def))))

(defun global-set-keys (&rest pairs)
  "Set alternating key-def PAIRS globally."
  ; https://github.com/RioZRon/dotspace/blob/master/layers/macros/local/macros/macros.el
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (global-set-key key def))))

(defmacro λ (&rest body)
  "Shorthand for interactive lambdas."
  `(lambda ()
     (interactive)
     ,@body))


;;; Preferences

(when (eq system-type 'darwin)
  (cd "~/home")
  (defvar oht-dotfiles             "~/home/dot/emacs/")
  (defvar oht-orgfiles             "~/home/org/")
  (defvar user-downloads-directory "~/Downloads/")
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-right-command-modifier 'meta
        mac-right-option-modifier 'nil))

(when (eq system-type 'windows-nt)
  (cd "~/")
  (defvar oht-dotfiles             "~/.emacs.d/")
  (defvar oht-orgfiles             "~/home/org/")
  (defvar user-downloads-directory "~/home/Downloads/")
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-]))

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
 '(dired-hide-details-mode t)
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


;;; Personal Keybindings

;; Minor modes override global bindings, so any bindings you don't want
;; overridden should be placed in a minor mode. I stole this technique from
;; the `bind-key' package.

(defvar bosskey-mode-map (make-keymap))

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
(when (eq system-type 'darwin)
  (define-keys bosskey-mode-map
    (kbd "s-q")       'save-buffers-kill-terminal
    (kbd "s-m")       'iconify-frame
    (kbd "s-w")       'delete-frame
    (kbd "s-n")       'make-frame-command
    (kbd "s-s")       'save-buffer
    (kbd "s-,")       'find-user-init-file
    (kbd "s-o")       'find-file
    (kbd "s-z")       'undo-fu-only-undo
    (kbd "s-Z")       'undo-fu-only-redo
    (kbd "s-x")       'kill-region
    (kbd "s-c")       'kill-ring-save
    (kbd "s-v")       'yank
    (kbd "s-<left>")  'beginning-of-visual-line
    (kbd "s-<right>") 'end-of-visual-line
    (kbd "s-<up>")    'beginning-of-buffer
    (kbd "s-<down>")  'end-of-buffer))

(define-keys bosskey-mode-map
  (kbd "C-<return>") 'general-transient
  (kbd "M-]")        'next-buffer
  (kbd "M-[")        'previous-buffer
  (kbd "C-M-h")      'mark-line
  (kbd "M-.")        'embark-act
  (kbd "M-'")        'my:hippie-expand
  (kbd "M-\\")       'cycle-spacing
  (kbd "M-z")        'zap-up-to-char
  (kbd "C-=")        'er/expand-region
  (kbd "C-d")        'delete-forward-char
  (kbd "C-x C-x")    'exchange-point-and-mark-dwim
  (kbd "C-x k")      'kill-buffer-dwim
  (kbd "M-0")        'delete-window
  (kbd "M-1")        'delete-other-windows
  (kbd "M-2")        'split-window-below
  (kbd "M-3")        'split-window-right
  (kbd "M-4")        'undefined
  (kbd "M-5")        'undefined
  (kbd "M-6")        'undefined
  (kbd "M-7")        'undefined
  (kbd "M-8")        'undefined
  (kbd "M-9")        'undefined
  (kbd "C-h f")      'helpful-function
  (kbd "C-h v")      'helpful-variable
  (kbd "C-h o")      'helpful-symbol
  (kbd "C-h k")      'helpful-key
  (kbd "C-h p")      'helpful-at-point)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; I actually want this to be overridden by other modes, like minibuffer modes.
(global-set-key (kbd "M-o") 'other-window)

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


;;; Appearance & Mode-Line

(custom-set-variables
  '(modus-themes-slanted-constructs t)
  '(modus-themes-links 'faint-neutral-underline)
  '(modus-themes-mode-line 'accented)
  '(modus-themes-region 'bg-only)
  '(modus-themes-diffs 'desaturated)
  '(modus-themes-org-blocks 'grayscale)
  '(modus-themes-syntax 'faint))

(modus-themes-load-operandi)

(setq tron-legacy-theme-softer-bg t)

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
   '(facedancer-monospace-family "IBM Plex Mono")
   '(facedancer-variable-family  "IBM Plex Sans")
   '(facedancer-mode-line-family "IBM Plex Sans")
   '(facedancer-mode-line-height 13)))

(when (eq system-type 'windows-nt)
  (custom-set-variables
   '(facedancer-monospace-family "Consolas")
   '(facedancer-variable-family  "Calibri")
   '(facedancer-mode-line-family "Calibri")
   '(facedancer-monospace-height 10)
   '(facedancer-variable-height 11)
   '(facedancer-mode-line-height 11)))

(autoload 'blackout "blackout" nil t)
(blackout 'eldoc-mode)
(blackout 'emacs-lisp-mode "Elisp")
(blackout 'auto-fill-function " Fill")

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



;;; Facedancer Mode

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


;;; Secondary Selection

;; Emacs's Secondary Selection assumes you only want to interact with it via
;; the mouse, however it is perfectly possible to do it via the keyboard, all
;; you need is some wrapper functions to make things keybinding-addressable.

;; TODO: overhaul names

;; kill-secondary
(defun oht/cut-secondary-selection ()
  "Cut the secondary selection."
  (interactive)
  (mouse-kill-secondary))

;; kill-ring-save-secondary
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

;; kill-secondary-yank
(defun oht/cut-secondary-selection-paste ()
  "Cut the secondary selection and paste at point."
  (interactive)
  (mouse-kill-secondary)
  (yank))

;; kill-ring-save-secondary-yank
(defun oht/copy-secondary-selection-paste ()
  "Copy the secondary selection and paste at point."
  (interactive)
  (oht/copy-secondary-selection)
  (yank))

;; mark-region-as-secondary
(defun oht/mark-region-as-secondary-selection ()
  "Make the region the secondary selection."
  (interactive)
  (secondary-selection-from-region))

;; mark-secondary
(defun oht/mark-secondary-selection ()
  "Mark the Secondary Selection as the region."
  (interactive)
  (secondary-selection-to-region))

;; deactivate-secondary
(defun oht/delete-secondary-selection ()
  "Delete the Secondary Selection."
  (interactive)
  (delete-overlay mouse-secondary-overlay))

;; -- Use C-M-drag-mouse-1 to create secondary selections.
(global-set-keys [C-M-mouse-1]      'mouse-start-secondary
                 [C-M-drag-mouse-1] 'mouse-set-secondary
                 [C-M-down-mouse-1] 'mouse-drag-secondary)

;; kill-secondary
;; kill-ring-save-secondary
;; kill-secondary-yank
;; kill-ring-save-secondary-yank
;; mark-region-as-secondary
;; mark-secondary
;; deactivate-secondary
(transient-define-prefix secondary-selection-transient ()
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


;;; Hippie Expand Completion

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        try-expand-line-all-buffers
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; The below code is taken from:
;; https://github.com/rails-to-cosmos/.emacs.d/blob/master/lisp/completion/hippie-expand-completion.el
;; with a few minor modifications to make it work with my setup.
;; It works very well and I highly recommend it when combined with `orderless'.

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
  "Offer completion for the word at point."
  (interactive)
  (my:hippie-expand-with 'hippie-expand))


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

(defvar navigation-keymap (make-keymap)
  "Transient keymap for navigating buffers.")

;; Assign navigation-keys to the map
(define-navigation-keys navigation-keymap)

(defun navigation-keymap-eldoc-function ()
  (eldoc-message "Navigation Keymap"))

(defun navigation-keymap--activate ()
  "Make the navigation-keymap transient and add eldoc message."
  (interactive)
  (message "Navigation Keymap Activated")
  (add-function :before-until (local 'eldoc-documentation-function)
                #'navigation-keymap-eldoc-function)
  (set-transient-map navigation-keymap t 'navigation-keymap--deactivate))

(defun navigation-keymap--deactivate ()
  "Remove the navigation-keymap eldoc message."
  (interactive)
  (message "Navigation Keymap Deactivated")
  (remove-function (local 'eldoc-documentation-function)
                   #'navigation-keymap-eldoc-function))

(define-key bosskey-mode-map (kbd "s-j") 'navigation-keymap--activate)


;;; Selected

;; One of the more sublime packages I've stumbled across, `selected' creates a
;; keymap that is active any time (and only when) the region is active.
;; Wonderful for quickly acting on the active region.
;;
;; When combined with my navigation keymap the two act like a very lightweight
;; vim emulation, but in an entirely emacs-y way.

(selected-global-mode 1)

(defun disable-selected-minor-mode ()
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
    "q" 'selected-off)
   (define-navigation-keys selected-keymap)
   (blackout 'selected-minor-mode))


;;; Minibuffer

(require 'orderless)

(vertico-mode)
(marginalia-mode)

(custom-set-variables
 '(enable-recursive-minibuffers t)
 '(savehist-mode t)
 '(completion-show-help nil)
 '(resize-mini-windows t)
 '(completion-styles '(orderless))
 '(completion-category-defaults nil)
 '(completion-category-overrides '((file (styles . (partial-completion))))))

(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

(with-eval-after-load 'embark
  (autoload 'dired-jump "dired")
  (define-keys embark-file-map
    "O" 'crux-open-with
    "j" 'dired-jump)
  (define-key embark-url-map
    "&" 'browse-url-default-macosx-browser))

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
(isearch-mb-mode)

;; Quit isearch when calling occur
(add-to-list 'isearch-mb--after-exit #'occur)


;;; Outline

;; `outline' provides major and minor modes for collapsing sections of a
;; buffer into an outline-like format. Let's turn that minor mode into a
;; global minor mode and enable it.
(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)
(global-outline-minor-mode +1)

(blackout 'outline-minor-mode)

(define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle)
(define-key outline-minor-mode-map (kbd "S-<tab>") 'bicycle-cycle-global)

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
    ("<down>" "Next" outline-next-visible-heading)
    ("<up>" "Previous" outline-previous-visible-heading)]
   ["Edit"
    ("M-<left>"  "Promote" outline-promote)
    ("M-<right>" "Demote"  outline-demote)
    ("M-<up>"    "Move Up" outline-move-subtree-up)
    ("M-<down>"  "Move Down" outline-move-subtree-down)]
   ["Other"
    ("C-/" "Undo" undo-fu-only-undo)
    ("M-/" "Redo" undo-fu-only-redo)
    ("c" "Consult" consult-outline :transient nil)]])


;;; Miscellaneous

(global-set-key [remap yank-pop] 'consult-yank-pop)
(custom-set-variables
 '(consult-preview-key (kbd "C-="))
 '(consult-config
   `((consult-mark :preview-key any))))

(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))

(with-eval-after-load 'visual-regexp
  (with-eval-after-load 'visual-regexp-steroids
    (custom-set-variables
     '(vr/engine 'pcre2el))))

(custom-set-variables
 '(fountain-add-continued-dialog nil)
 '(fountain-highlight-elements (quote (section-heading))))

(add-to-list 'magic-mode-alist
             '("%text" . markdown-mode))
(add-to-list 'auto-mode-alist
             '("\\.text" . markdown-mode))

(when (string= (system-name) "shadowfax.local")
  (add-to-list 'load-path "~/home/src/oblique-strategies/")
  (autoload 'oblique-strategy "oblique")
  (setq initial-scratch-message (concat
                                 ";; Welcome to Emacs!\n;; This is the scratch buffer, for unsaved text and Lisp evaluation.\n"
                                 ";; Oblique Strategy: " (oblique-strategy) "\n\n")))

(with-eval-after-load 'flyspell
  (blackout 'flyspell-mode " Spell"))
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(with-eval-after-load 'dired
  (setq dired-use-ls-dired nil)
  (define-keys dired-mode-map
    (kbd "O")   'crux-open-with
    (kbd "C-/") 'dired-undo))


;;; Org

(autoload 'oht-org-agenda-today-pop-up "org")
(autoload 'oht-org-agenda-today "org")

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

  (defun find-org-files ()
    "Find org files in your org directory, pass to completing-read."
    (interactive)
    (find-file (completing-read "Find Org Files: "
                                (directory-files-recursively oht-orgfiles "\.org$"))))

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

  (setq org-tag-alist '(("mtg1" . ?1) ("mtg2" . ?2) ("mtg3" . ?3)))

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
           "* TODO %?" :empty-lines 1)
          ("k" "Kiddos Log Entry" entry
           (file+olp+datetree ,(concat oht-orgfiles "kiddos_logbook.org"))
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
  (define-key org-agenda-mode-map (kbd "s-z") 'org-agenda-undo)
  (define-key org-agenda-mode-map (kbd "C-/") 'org-agenda-undo)
  (define-key org-agenda-mode-map (kbd "t") org-agenda-todo-map))


;;; Transient

(autoload 'org-store-link "org")
(autoload 'dired-jump "dired" nil t)
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
    ("C-b" "iBuffer" ibuffer)]
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
   ["Other"
    ("k" "Capture" org-capture)
    ("s" "Store Link" org-store-link)]])

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
    ("m" "Modus Toggle" modus-themes-toggle)]])

(transient-define-prefix general-transient--window ()
  "Most commonly used window commands"
  [["Splits"
    ("s" "Horizontal" split-window-below)
    ("v" "Vertical"   split-window-right)
    ("b" "Balance"    balance-windows)
    ("f" "Fit"        fit-window-to-buffer)
    ("r" "Rotate"     toggle-window-split)
    ("F" "Find Other Win" find-file-other-window)]
   ["Window"
    ("d" "Dedicate Window" dedicated-mode)
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
      ("C-/" "Undo" undo-fu-only-undo)
      ("M-/" "Redo" undo-fu-only-redo)]]))

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
      nil ; if the above succeeds, do nothing, else...
    (message "No transient defined for this mode.")))

(with-eval-after-load 'org
  (transient-define-prefix org-mode-help-transient ()
    "Transient for Org Mode"
    ["Org Mode"
     ["Navigation"
      ("o" "Outline" consult-outline)
      ("n" "Narrow/Widen" narrow-or-widen-dwim)
      ("g" "Go To" org-goto)
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


;;; End of init.el
