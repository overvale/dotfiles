;;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Copyright (C) 2021 Oliver Taylor

;; Author: Oliver Taylor
;; URL: https://olivertaylor.net
;; URL: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; This file has an outline which can be viewed by looking at comments
;; starting with three or more semicolons. `outline-minor-mode' supports this
;; convention by default and helps with navigation. You can also create an
;; occur buffer with the search /^;;;+/.

;; Every Emacs configuration is unique to the person who created it, to their
;; needs and their taste. This one takes the following approach:
;;
;;   + I prefer to write my own code instead of installing packages.
;;   + I use package.el and don't use Use-Package.
;;   + The config is a single file (with exceptions for large libraries
;;     which I never edit and didn't author).
;;   + I make heavy use of transient for interacting with Emacs.
;;   + I'm a heavy Org user for my job and life.
;;   + It is NOT modular, so if you don't have all the listed packages
;;     installed large parts of the config won't work.
;;   + Symbol names are not prefixed (for the most part) so take care to
;;     to avoid collisions if you copy them into your own config.

;; If you like this config I would recommend reading these as well:
;;
;; https://github.com/oantolin/emacs-config
;; https://github.com/raxod502/radian
;; https://github.com/skeeto/.emacs.d
;; https://github.com/bbatsov/prelude


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
      '(bicycle
        consult
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

(setq package-pinned-packages
      '((embark . "melpa")))

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
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(mark-even-if-inactive nil)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(fill-column 78)
 '(locate-command "mdfind")
 '(trash-dircetory "~/.Trash"))

(dolist (cmd '(upcase-region
               downcase-region))
  (put cmd 'disabled nil))

(defun prog-mode-hook-config nil
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode))

(add-hook 'prog-mode-hook 'prog-mode-hook-config)

(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

(defun find-user-init-file ()
  "Find the user-init-file."
  (interactive)
  (find-file user-init-file))

;; If I break my init file it almost always happens after this point in the
;; config. So I keep. up here, a basic set of critical keybindings that are
;; useful when troubleshooting a broken init file.
(let ((map global-map))
  (define-key map (kbd "s-q") 'save-buffers-kill-emacs)
  (define-key map (kbd "s-N") 'make-frame-command)
  (define-key map (kbd "s-m") 'iconify-frame)
  (define-key map (kbd "s-b") 'switch-to-buffer)
  (define-key map (kbd "s-s") 'save-buffer)
  (define-key map (kbd "s-f") 'find-file)
  (define-key map (kbd "s-F") 'find-file-other-window)
  (define-key map (kbd "s-o") 'other-window)
  (define-key map (kbd "s-,") 'find-user-init-file)
  (define-key map (kbd "s-z") 'undo)
  (define-key map (kbd "s-x") 'kill-region)
  (define-key map (kbd "s-c") 'kill-ring-save)
  (define-key map (kbd "s-v") 'yank)
  (define-key map (kbd "s-<left>") 'beginning-of-visual-line)
  (define-key map (kbd "s-<right>") 'end-of-visual-line)
  (define-key map (kbd "s-<up>") 'beginning-of-buffer)
  (define-key map (kbd "s-<down>") 'end-of-buffer))


;;; Critical Setup

;; Start in my home directory
(cd "~/home/")

;; These variables, packages, macros, and functions are used throughout the
;; config --- and are required for it to work correctly.

(defvar user-dotemacs-directory  "~/home/dot/emacs/")
(defvar user-orgfiles-directory  "~/home/org/")
(defvar user-downloads-directory "~/Downloads/")

;; I've taken these 2 large pieces of code from other configurations and load
;; them as-is. There's nothing to configure, they provide no interactive
;; commands, they just improve Emacs a bit.
(add-to-list 'load-path (concat user-dotemacs-directory "lisp/"))
(require 'undo-backport)
(require 'radian-directories)

;; vundo creates a tree-like visualization of your undo history
;; using only standard Emacs undo commands and data. Requires either
;; Emacs 28 or its backported undo functions.
(add-to-list 'load-path "~/home/src/lisp/vundo/")
(require 'vundo)

(autoload 'transient-define-prefix "transient" nil t)
(exec-path-from-shell-initialize)

;; I use 3 macros to control how/when code and packages are loaded. I don't
;; use use-package, which is designed as a comprehensive way of dealing with
;; this problem, so I need a few little tools of my own.
;;
;; 1. `with-eval-after-load' -- which evaluates the code after the library is loaded.
;; 2. elisp-group -- this is simply a "group" of lines of code. It provides no
;;    functional benefit, only a VISUAL one.
;; 3. config-package -- this evaluates the contained lisp only if the named package
;;    is installed, and optionally adds it to `package-selected-packages'.

(defmacro elisp-group (name doc &rest body)
  "Group elisp by wrapping in progn.
Is this a useless macro? Maybe. But it helps keep my init file tidy."
  (declare (indent defun) (doc-string 2))
  `(progn ,@body))

(defmacro config-package (package doc &rest body)
  "Eval BODY only if PACKAGE is installed."
  (declare (indent defun) (doc-string 2))
  ;; this could also be an argument, t to select the package, nil to not
  ;;(add-to-list 'package-selected-packages ,package)
  `(if (package-installed-p ,package)
       (progn ,@body)
     (message (concat "Package \'"
                      (symbol-name ,package)
                      "\' is not installed... skipping config."))))


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
  ;; https://github.com/oantolin/emacs-config/blob/master/my-lisp/window-extras.el
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

(defun split-window-dwim ()
  "Interactive wrapper around `split-window-sensibly'."
  (interactive)
  (split-window-sensibly))

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

(defun find-file-recursively (&optional path)
  "Find Files Recursively using completing read.
Uses the `default-directory' unless a path is supplied."
  (interactive)
  (find-file (completing-read "Find File Recursively: "
                              (directory-files-recursively (if path path default-directory) ".+" t))))

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

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  ;; https://stackoverflow.com/a/14539202
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

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

(defun backward-kill-line nil
  "Kill backward to the start of line."
  (interactive)
  (kill-line 0))

(defun describe-symbol-at-point ()
  "Run `describe-symbol' for the `symbol-at-point."
  (interactive)
  (describe-symbol (symbol-at-point)))


;;; Keybindings

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

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key [swipe-left])
(global-unset-key [swipe-right])

(let ((map bosskey-mode-map))
  ;; Mac-like bindings
  (define-key map (kbd "s-q") 'frames-p-save-buffers-kill-emacs)
  (define-key map (kbd "s-m") 'iconify-frame)
  (define-key map (kbd "s-n") 'new-buffer)
  (define-key map (kbd "s-N") 'make-frame-command)
  (define-key map (kbd "s-s") 'save-buffer)
  (define-key map (kbd "s-,") 'find-user-init-file)
  (define-key map (kbd "s-z") 'undo-only)
  (define-key map (kbd "s-Z") 'undo-redo)
  (define-key map (kbd "s-x") 'kill-region)
  (define-key map (kbd "s-c") 'kill-ring-save)
  (define-key map (kbd "s-v") 'yank)
  (define-key map (kbd "s-<backspace>") 'backward-kill-line)
  (define-key map (kbd "s-<left>") 'beginning-of-visual-line)
  (define-key map (kbd "s-<right>") 'end-of-visual-line)
  (define-key map (kbd "s-<up>") 'beginning-of-buffer)
  (define-key map (kbd "s-<down>") 'end-of-buffer)
  ;; Emacs keybinding tweaks
  (define-key map (kbd "s-<return>") 'general-transient)
  (define-key map (kbd "s-S-<return>") 'call-mode-help-transient)
  (define-key map (kbd "s-]") 'next-buffer)
  (define-key map (kbd "s-[") 'previous-buffer)
  (define-key map (kbd "s-=") 'ibuffer)
  (define-key map (kbd "s-{") 'pop-to-mark-command)
  (define-key map (kbd "s-}") 'unpop-to-mark-command)
  (define-key map (kbd "s-+") 'consult-mark)
  (define-key map (kbd "s-b") 'consult-buffer)
  (define-key map (kbd "s-B") 'consult-buffer-other-window)
  (define-key map (kbd "s-w") 'window-transient)
  (define-key map (kbd "s-k") 'org-capture)
  (define-key map (kbd "s-f") 'find-file)
  (define-key map (kbd "s-F") 'find-file-other-window)
  (define-key map (kbd "C-M-h") 'mark-line)
  (define-key map (kbd "M-.") 'embark-act)
  (define-key map (kbd "M-'") 'completion-at-point)
  (define-key map (kbd "M-\\") 'cycle-spacing)
  (define-key map (kbd "M-z") 'zap-up-to-char)
  (define-key map (kbd "M-<SPC>") 'push-mark-no-activate)
  (define-key map (kbd "C-=") 'er/expand-region)
  (define-key map (kbd "C-d") 'delete-forward-char)
  (define-key map (kbd "C-x C-x") 'exchange-point-and-mark-dwim)
  (define-key map (kbd "C-x k") 'kill-buffer-dwim))

(let ((map global-map))
  (define-key map (kbd "C-a") 'ora-move-beginning-of-line)
  ;; replace mappings
  (define-key map [remap query-replace] 'vr/query-replace)
  (define-key map [remap capitalize-word] 'capitalize-dwim)
  (define-key map [remap downcase-word]   'downcase-dwim)
  (define-key map [remap upcase-word]     'upcase-dwim)
  ;; Mouse + Mode Line Magic
  (define-key map [mode-line S-mouse-1] 'mouse-delete-other-windows)
  (define-key map [mode-line M-mouse-1] 'mouse-delete-window)
  (define-key map [mode-line C-mouse-1] 'mouse-split-window-horizontally)
  ;; Make shift-click extend the region.
  (define-key map [S-down-mouse-1] 'ignore)
  (define-key map [S-mouse-1] 'mouse-save-then-kill)
  ;; Use M-drag-mouse-1 to create rectangle regions.
  (define-key map [M-down-mouse-1] #'mouse-drag-region-rectangle)
  (define-key map [M-drag-mouse-1] #'ignore)
  (define-key map [M-mouse-1]      #'mouse-set-point))

(define-key help-map "s" 'describe-symbol-at-point)


;;; Themes

;; By default I use a light theme, but sometimes (when all the lights are off)
;; I use a dark theme. The below are some functions that allow me to quickly
;; switch between light and dark themes.

(defvar light-theme nil
  "Preferred light-theme.")

(defvar dark-theme nil
  "Preferred dark-theme.")

(defvar default-theme-color 'light
  "Default theme to load, accepts 'light and 'dark.")

(defvar current-theme-color default-theme-color
  "Is the current theme color light or dark?")

(defun disable-current-themes nil
  "Disables all currently enabled themes."
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes))

(defun macos-appearance-dark-p nil
  "Return t if macOS appearance is dark."
   (string= (plist-get (mac-application-state) :appearance) "NSAppearanceNameDarkAqua"))

(defun load-theme-dwim (&optional color)
  "Load users preferred theme, based on ARG or macOS appearance.

Disables all current themes, then:

- if COLOR is \"light\", load the `light-theme'.
- if COLOR is \"dark\" load the `dark-theme'
- if COLOR is \"system\" check macOS's appearance state and match it with
  either the light or dark theme.
- If called without an argument toggle between light and dark themes."
  (interactive
   (list (completing-read "Load Theme Color: " '("dark" "light" "system"))))
  (disable-current-themes)
  (cond ((string= color "light")
         (setq current-theme-color 'light)
         (load-theme light-theme t))
        ((string= color "dark")
         (setq current-theme-color 'dark)
         (load-theme dark-theme t))
        ((string= color "system")
         (if (macos-appearance-dark-p)
             (load-theme-dwim 'dark)
           (load-theme-dwim 'light)))
        ((eq color nil)
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

(add-hook 'mac-effective-appearance-change-hook 'theme-color-toggle)

(config-package 'modus-themes
  "Require, cofigure, and load modus-themes."
  (require 'modus-themes)
  (custom-set-variables
   '(modus-themes-italic-constructs t)
   '(modus-themes-links '(neutral-underline))
   '(modus-themes-mode-line '(accented))
   '(modus-themes-prompts '(bold))
   '(modus-themes-completions 'moderate)
   '(modus-themes-region '(bg-only))
   '(modus-themes-org-blocks '(gray-background))
   '(modus-themes-org-agenda '((header-block . (variable-pitch scale-title))
                               (header-date . (bold-today))
                               (scheduled . rainbow))))
  (modus-themes-load-themes))

(setq light-theme 'modus-operandi)
(setq dark-theme  'gruvbox)
(setq default-theme-color 'light)

(elisp-group set-theme-on-startup
  "On startup, try to match system color, otherwise load `default-color-theme'."
  (if (ignore-errors (macos-appearance-dark-p))
      (load-theme-dwim 'dark)
    (if (eq default-theme-color 'light)
        (load-theme-dwim 'light)
      (load-theme-dwim 'dark))))


;;; Fonts

(setq text-scale-mode-step 1.09)

(elisp-group fontface-setup
  "Set fonts and their sizes.
The variable-pitch and fixed-pitch faces have a default height of 1.0,
which I don't want to mess with because that's what's required to make
`text-scale-adjust' work correctly. The default height needs to be set,
and I want the mode-line to be a fixed height, so I set those."
  (let ((mono "Roboto Mono")
        (vari "Roboto Slab")
        (mode "Roboto")
        (mono-height 120)
        (ml-height 140))
    (custom-set-faces
     `(default ((t :family ,mono :height ,mono-height)))
     `(fixed-pitch ((t :family ,mono)))
     `(variable-pitch ((t :family ,vari)))
     `(mode-line ((t :family ,mode :height ,ml-height)))
     `(mode-line-inactive ((t :family ,mode :height ,ml-height))))))


;;; Mode-Line

(setq display-time-default-load-average nil)
(setq display-time-format "  [%F]  %R")
(display-time-mode)
(setq battery-mode-line-format "  %b%p%%")
(display-battery-mode)

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



;;; Windows

;; Settings for how Emacs displays windows, splits, etc.

(custom-set-variables
 '(winner-mode t)
 '(split-window-keep-point nil)
 '(even-window-sizes nil))

;; This introduces a fundamental change to how Emacs works. Normally, when
;; Emacs displays a new buffer, it tries to intelligently select the best
;; window for that buffer. Sometimes this is a new window (split), sometimes
;; it is the current window, sometimes it re-uses an existing window. The
;; below code makes it so the default window is always the current one. The
;; advantage of this is that your window layouts are never changed for you.
;; https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts

(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window display-buffer-same-window)
                          (reusable-frames . t)))

;; Since the default is to always reuse the same window, you need to declare
;; any exceptions to that. Though there are some windows that do their own
;; thing anyway; *Completions*, *Org Select*.
(setq display-buffer-alist
      '(("\\*Calendar.*"
         (display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((no-other-window . nil)))
         (window-height . fit-window-to-buffer))
        ("\\*wclock.*"
         (display-buffer-at-bottom)
         (window-parameters . ((select . t))))))

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

(transient-define-prefix window-transient ()
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

(let ((map global-map))
  (define-key map [C-M-mouse-1] 'mouse-start-secondary)
  (define-key map [C-M-drag-mouse-1] 'mouse-set-secondary)
  (define-key map [C-M-down-mouse-1] 'mouse-drag-secondary))

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


;;; Confirm Killing Modified Buffers

;; Emacs asks for confirmation when killing modified file-visiting buffers,
;; but does not ask for non-file-visiting buffers.
;;
;; The option `buffer-offer-save' tells Emacs to prompt to you save modified
;; non-file-visiting buffers when EXITING Emacs, but no such option exists for
;; killing buffers (as described in the docstring for `buffer-offer-save').
;;
;; The below create a buffer-local variable and function for
;; `kill-buffer-query-functions' that provides this functionality.

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
  "Create a new buffer, prompting for NAME."
  (interactive
   (list (read-string
          "Create buffer (default \"untitled\"): "
          nil nil "untitled")))
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
  ;; If you instead bind these in `outline-minor-mode' they might not be
  ;; properly overridden by other minor modes, like org-mode. So a global
  ;; binding is safer.
  (global-set-key (kbd "C-<tab>") 'bicycle-cycle)
  (global-set-key (kbd "S-<tab>") 'bicycle-cycle-global))

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


;;; Navigation And Selection

;; Emacs is littered with what I call "navigation modes", ones in which
;; typing 'n' is a shortcut for 'C-n', or '<' is a shortcut for 'M-<'.
;; These modes assume you don't want to insert text and instead want to move
;; around the buffer. I wanted to generalize this idea into a minor mode I
;; could enable at any time.

;; I also use (and love) the package `selected', which provides a keymap that
;; is active only then the region is active. By applying the same movement
;; bindings to selected I can easily refine the active region with convenient
;; bindings.


;;;; Common Navigation Keys

;; There are a few keymaps which I think should share common movement
;; bindings. To facilitate this I've made a command which you can pass a
;; keymap and those common bindings will be set in the keymap. In my case,
;; this allows me to share a common set of bindings between a custom
;; navigation keymap and the `selected' package.

(defun define-navigation-keys (map)
  "Defines navigation keys for a map supplied by argument."
  (interactive "S")
  (define-key map (kbd ".") 'embark-act)
  (define-key map (kbd "n") 'next-line)
  (define-key map (kbd "p") 'previous-line)
  (define-key map (kbd "<right>") 'forward-char)
  (define-key map (kbd "<left>") 'backward-char)
  (define-key map (kbd "f") 'forward-word)
  (define-key map (kbd "b") 'backward-word)
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
  (define-key map (kbd "x") 'exchange-point-and-mark))


;;;; Navigation Mode

(defvar navigation-mode-map (make-sparse-keymap)
  "Keymap for `navigation-mode'.")

(defun navigation-mode-eldoc-function ()
  (eldoc-message "** Navigation Mode Active **"))

(define-minor-mode navigation-mode
  "Minor mode for nagivating buffers."
  :init-value nil
  :lighter " NΛV"
  :keymap navigation-mode-map
  (if navigation-mode
      (add-function :before-until
                    (local 'eldoc-documentation-function)
                    #'navigation-mode-eldoc-function)
    (remove-function (local 'eldoc-documentation-function)
                   #'navigation-mode-eldoc-function)))

(defun navigation-mode-exit-and-mark ()
  "Exit `navigation-mode' and set the mark."
  (interactive)
  (navigation-mode -1)
  (set-mark-command nil))

(define-key global-map (kbd "s-j") 'navigation-mode)

(let ((map navigation-mode-map))
  ;; Any key that is not specifically bound should be ignored:
  (define-key map [t] 'undefined)
  ;; Apply common navigation keys:
  (define-navigation-keys map)
  ;; If you want to override them, just redefine below...
  (define-key map (kbd "SPC") 'navigation-mode-exit-and-mark)
  (define-key map (kbd "q")   'navigation-mode))


;;;; Selected Mode

(config-package 'selected
  "A keymap which activates when the region is active.
One of the more sublime packages I've stumbled across, `selected' creates a
keymap that is active any time (and only when) the region is active.
Wonderful for quickly acting on the active region.

When combined with my navigation keymap the two act like a very lightweight
vim emulation, but in an entirely emacs-y way."
  (selected-global-mode 1)
  (delete-selection-mode -1)

  (defun disable-selected-minor-mode ()
    "Disabled the selected minor mode."
    (selected-minor-mode -1))

  (with-eval-after-load 'selected
    (delight 'selected-minor-mode nil "selected")
    ;; Careful not to bind - or = as they may collide with `expand-region'.
    (let ((map selected-keymap))
      (define-navigation-keys map)
      (define-key map (kbd ".") 'embark-act)
      (define-key map (kbd "u") 'upcase-region)
      (define-key map (kbd "d") 'downcase-region)
      (define-key map (kbd "c") 'capitalize-region)
      (define-key map (kbd "|") 'pipe-region)
      (define-key map (kbd "R") 'replace-rectangle)
      (define-key map (kbd "e") 'eval-region)
      (define-key map (kbd "q") 'fill-paragraph)
      ;; In selected mode, move the mark instead of the point:
      (define-key map (kbd "M-f") 'mark-word)
      (define-key map (kbd "C-n") 'mark-line)
      (define-key map (kbd "M-e") 'mark-sentence)
      (define-key map (kbd "M-}") 'mark-paragraph)
      (define-key map (kbd "C-M-f") 'mark-sexp))))


;;; Minibuffer / Embark / Consult

(custom-set-variables
 '(completion-cycle-threshold nil)
 '(enable-recursive-minibuffers t)
 '(savehist-mode t))

(config-package 'orderless
  "I would love Emacs less if it weren't for orderless.
It needs to be required because so many commands rely on the
completion framework."
  (require 'orderless)
  (custom-set-variables
   '(completion-styles '(orderless))
   '(completion-category-defaults nil)
   '(completion-category-overrides '((file (styles . (partial-completion)))))))

(config-package 'vertico
  nil
  (vertico-mode))

(config-package 'marginalia
  nil
  (marginalia-mode)
  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle))

(config-package 'embark
  "embark and embark-consult are two separate packages, but I
configure them here in a single group. Embark is being very
heavily developed at the moment, and the embark-indicators are
only present in the most reason versions."
  (require 'embark)
  (require 'embark-consult)

  (custom-set-variables
   '(embark-indicators '(embark-verbose-indicator
                         embark-highlight-indicator
                         embark-isearch-highlight-indicator))
   '(embark-verbose-indicator-display-action
     '(display-buffer-at-bottom (window-height . fit-window-to-buffer))))

  (setq prefix-help-command 'embark-prefix-help-command)

  (define-key bosskey-mode-map (kbd "C-h b") 'embark-bindings)

  (defun embark-describe-keymap (keymap)
    ;; https://github.com/oantolin/emacs-config/blob/master/my-lisp/help-extras.el
    "Prompt for KEYMAP and show its bindings using `completing-read'."
    (interactive
     (list
      (completing-read "Keymap: "
                       (cl-loop for x being the symbols
                                if (and (boundp x) (keymapp (symbol-value x)))
                                collect (symbol-name x))
                       nil t nil 'variable-name-history)))
    (require 'embark)
    (embark-completing-read-prompter (symbol-value (intern keymap)) nil))

  (let ((map embark-file-map))
    (define-key map (kbd "O") 'crux-open-with)
    (define-key map (kbd "j") 'dired-jump))

  (let ((map embark-url-map))
    (define-key map (kbd "d") 'ytdl-download)
    (define-key map (kbd "b") 'browse-url-default-macosx-browser)))

(elisp-group isearch-config
  "This does two things, makes the matching fuzzy
per-line (though not orderless, use consult-lines for that), and
makes exits exit at the beginning of the match. I think this is a
more vim-like behavior, which I prefer because it makes it easier
to set the mark, and search to expand the region to the desired
spot."
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-lazy-count t)

  (defun isearch-exit-at-start ()
    "Exit search at the beginning of the current match."
    (when (and isearch-forward
               (number-or-marker-p isearch-other-end)
               (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end)))

  (add-hook 'isearch-mode-end-hook 'isearch-exit-at-start))

(config-package 'isearch-mb
  nil
  (isearch-mb-mode)
  (add-to-list 'isearch-mb--after-exit #'occur))

(config-package 'consult
  nil
  (custom-set-variables
   '(consult-find-command "fd --color=never --full-path ARG OPTS"))

  (consult-customize consult-line
                     :preview-key nil)

  (consult-customize consult-completion-in-region
                     :cycle-threshold 3)

  (global-set-key [remap yank-pop] 'consult-yank-pop))


;;; Miscellaneous

(with-eval-after-load 'visual-regexp
  (with-eval-after-load 'visual-regexp-steroids
    (custom-set-variables
     '(vr/engine 'pcre2el))))

(config-package 'fountain-mode
  "Settings for fountain-mode."
  (custom-set-variables
   '(fountain-add-continued-dialog nil)
   '(fountain-highlight-elements (quote (section-heading)))))

(config-package 'markdown-mode
  "It seems everyone wants me to use the extension 'mdown' for
markdown documents, which for some reason I hate. I have no idea
why. I prefer 'text'. I probably got the idea here:
https://daringfireball.net/linked/2014/01/08/markdown-extension"
  (add-to-list 'auto-mode-alist
               '("\\.text" . markdown-mode)))

(config-package 'oblique
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
  (let ((map dired-mode-map))
    (define-key map (kbd "O") 'crux-open-with)
    (define-key map (kbd "C-/") 'dired-undo)))

(config-package 'olivetti
  nil
  (custom-set-variables
   '(olivetti-body-width 86)))

(config-package 'ytdl
  "YouTube Download"
  (setq ytdl-media-player "open")
  (setq ytdl-always-query-default-filename 'yes-confirm))

(elisp-group world-clock
  "Emacs has a world clock!"
  (defalias 'world-clock 'display-time-world)
  (setq display-time-world-time-format "%Z%t%R%t%F"
        zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Chicago" "Chicago")
          ("America/Montreal" "Montreal"))))


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
(autoload 'org-export-dispatch "org")

;; calling capture templates directly
(defun org-capture-scanline-log   () (interactive) (org-capture nil "sl"))
(defun org-capture-scanline-frank () (interactive) (org-capture nil "sf"))
(defun org-capture-scanline-zero  () (interactive) (org-capture nil "sz"))

(setq org-quick-capture-map
      (let ((map (make-sparse-keymap "Scanline Capture")))
        (define-key map "l" '("logbook" . org-capture-scanline-log))
        (define-key map "f" '("frank"   . org-capture-scanline-frank))
        (define-key map "z" '("zero"    . org-capture-scanline-zero))
        map))

(define-key bosskey-mode-map (kbd "s-K") org-quick-capture-map)


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
        org-deadline-warning-days 7
        org-agenda-todo-ignore-scheduled 'all
        org-agenda-todo-ignore-deadlines 'near
        ;; if you're using log-mode you don't need these 2:
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-sorting-strategy '(((agenda habit-down time-up category-up priority-down)
                                       (todo todo-state-up priority-down category-up)
                                       (tags priority-down category-keep)
                                       (search category-keep))))

  (setq org-agenda-files (list user-orgfiles-directory))

  (add-to-list 'org-agenda-files "~/home/writing/kindred/compendium.org")

  (setq org-agenda-custom-commands
        '(("1" "TODAY: Today's Agenda + Priority Tasks"
           ((agenda "d" ((org-agenda-span 'day)))
            (todo "TODO|DELG"
                  ((org-agenda-sorting-strategy '(todo-state-up priority-down))))))
          ("0" "COMPLETE: Week Agenda + All Tasks"
           ((agenda "w" ((org-agenda-span 'week)))
            (todo "TODO|LATER"
                  ((org-agenda-sorting-strategy '(todo-state-up priority-down))))))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "DELG(g)" "LATER(l)" "|" "DONE(d)" "MOVED(m)" "CANCELED(c)")))

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
  (defun org-todo-set-delegated () (interactive) (org-todo "DELG"))
  (defun org-agenda-todo-set-delegated () (interactive) (org-agenda-todo "DELG"))
  (defun org-todo-set-later () (interactive) (org-todo "LATER"))
  (defun org-agenda-todo-set-later () (interactive) (org-agenda-todo "LATER"))
  (defun org-todo-set-done () (interactive) (org-todo "DONE"))
  (defun org-agenda-todo-set-done () (interactive) (org-agenda-todo "DONE"))
  (defun org-agenda-todo-set-canceled () (interactive) (org-agenda-todo "CANCELED"))
  (defun org-todo-set-canceled () (interactive) (org-todo "CANCELED"))
  (defun org-agenda-todo-set-moved () (interactive) (org-agenda-todo "MOVED"))
  (defun org-todo-set-moved () (interactive) (org-todo "MOVED"))

  (setq org-todo-map
        (let ((map (make-sparse-keymap "Org TODO")))
          (define-key map "t" '("TODO"     . org-todo-set-todo))
          (define-key map "g" '("DELG"     . org-todo-set-delegated))
          (define-key map "l" '("LATER"    . org-todo-set-later))
          (define-key map "d" '("DONE"     . org-todo-set-done))
          (define-key map "m" '("MOVED"    . org-todo-set-moved))
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
      (define-key map "g" '("DELG"     . org-agenda-todo-set-delegated))
      (define-key map "l" '("LATER"    . org-agenda-todo-set-later))
      (define-key map "d" '("DONE"     . org-agenda-todo-set-done))
      (define-key map "m" '("MOVED"    . org-agenda-todo-set-moved))
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
    ("a" "AutoFill" auto-fill-mode)
    ("j" "Dired Jump" dired-jump)]
   [""
    ("." "Repeat Command" repeat-complex-command)
    ("k" "Kill Buffer" kill-buffer-dwim)
    ("b" "Switch Buffer" switch-to-buffer)]
   ["Transients"
    ("o" "Org..." general-transient--org)
    ("t" "Toggle..." general-transient--toggles)
    ("w" "Windows..." window-transient)
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
      ("t" "TODO" org-todo-set-todo)
      ("T" "Set Tags" org-set-tags-command)
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
