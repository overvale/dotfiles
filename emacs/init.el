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
;;   + The config is a single file.
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
        doom-themes
        elfeed
        embark
        embark-consult
        exec-path-from-shell
        fountain-mode
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
 '(save-interprogram-paste-before-kill t)
 '(kill-do-not-save-duplicates t)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(mark-even-if-inactive nil)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(fill-column 78)
 '(locate-command "mdfind")
 '(delete-by-moving-to-trash t)
 '(trash-dircetory "~/.Trash"))

(dolist (cmd '(upcase-region
               downcase-region))
  (put cmd 'disabled nil))

(defun prog-mode-hook-config nil
  (setq-local show-trailing-whitespace t)
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
;; config. So I keep, up here, a basic set of critical keybindings that are
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

;; I use 3 macros to control how/when code and packages are loaded. I don't
;; use use-package, which is designed as a comprehensive way of dealing with
;; this problem, so I need a few little tools of my own.
;;
;; 1. `with-eval-after-load' (built-in) -- which evaluates the code after the
;;    library is loaded.
;; 2. `elisp-group' -- this is simply a "group" of lines of code. It provides no
;;    functional benefit, only a VISUAL one.
;; 3. `config-package' -- this evaluates the contained lisp only if the named package
;;    is installed.

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

;; Now that those macros are setup, let's setup some important things that are
;; required for the rest of this config to work correctly.

(elisp-group environment
  "Important paths for this setup to work."
  (cd "~/home/") ; Start in my personal home directory
  (setq org-directory "~/home/org/")
  (defvar user-downloads-directory "~/Downloads/")
  (add-to-list 'load-path "~/home/dot/emacs/lisp/"))

(elisp-group undo/redo
  "I think Emacs's undo/redo could be simpler. Emacs 28 provides
everything I need, but I'm still on 27, so I've included a
backport of those functions, and a handy package (which is not on
MELPA) for visualizing the undo tree."
  (require 'undo-backport) ; from Emacs 28
  ;; vundo creates a tree-like visualization of your undo history
  ;; using only standard Emacs undo commands and data. Requires either
  ;; Emacs 28 or its backported undo functions.
  (add-to-list 'load-path "~/home/src/lisp/vundo/")
  (require 'vundo))

(elisp-group transient
  "I use a lot of transients in this config, so I need to make sure it is
loaded and configured before those are declared below."
  (autoload 'transient-define-prefix "transient" nil t)
  (setq transient-show-popup 1)
  (setq transient-detect-key-conflicts t))

(config-package 'exec-path-from-shell
  (exec-path-from-shell-initialize))


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

(defun rotate-window-split ()
  ;; https://github.com/oantolin/emacs-config/blob/master/my-lisp/window-extras.el
  "Rotate window split from vertical to horizontal."
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

(defun swap-windows (count)
  "Swap your windows.
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

(defun backward-kill-line nil
  "Kill backward to the start of line."
  (interactive)
  (kill-line 0))

(defun describe-symbol-at-point ()
  "Run `describe-symbol' for the `symbol-at-point."
  (interactive)
  (describe-symbol (symbol-at-point)))

(defun pop-to-buffer-same-mode (&rest modes)
  "Pop to a buffer with a mode among MODES, or the current one if not given."
  ;; https://jao.io/blog/2021-09-08-high-signal-to-noise-emacs-command.html
  (interactive)
  (let* ((modes (or modes (list major-mode)))
         (pred (lambda (b)
                 (let ((b (get-buffer (if (consp b) (car b) b))))
                   (member (buffer-local-value 'major-mode b) modes)))))
    (pop-to-buffer (read-buffer "Buffer: " nil t pred))))


;;; Keybindings

;; Emacs Keymap Lookup Order:
;; 1. overriding-terminal-local-map
;; 2. overriding-local-map
;; 3. text property's 'keymap property
;; 4. emulation-mode-map-alists
;; 5. minor-mode-overriding-map-alist
;; 6. minor-mode-map-alist (Minor Mode)
;; 7. text property's 'local-map property
;; 8. (current-local-map) (Major Mode)
;; 9. (current-global-map) (Global Map)

;; The technique below is taken from the bind-key package. It places all the
;; bindings I don't want overridden into a minor mode which is inserted into
;; the `emulation-mode-map-alists' (number 4 on the list above), so only very
;; few things can override them.

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
  (define-key map (kbd "s-{") 'pop-to-mark-command)
  (define-key map (kbd "s-}") 'unpop-to-mark-command)
  (define-key map (kbd "s-b") 'consult-buffer)
  (define-key map (kbd "s-B") 'consult-buffer-other-window)
  (define-key map (kbd "s-w") 'window-transient)
  (define-key map (kbd "s-k") 'org-capture)
  (define-key map (kbd "s-a") 'org-agenda)
  (define-key map (kbd "s-f") 'find-file)
  (define-key map (kbd "s-F") 'find-file-other-window)
  (define-key map (kbd "M-.") 'embark-act)
  (define-key map (kbd "M-'") 'completion-at-point)
  (define-key map (kbd "M-\\") 'cycle-spacing)
  (define-key map (kbd "M-z") 'zap-up-to-char)
  (define-key map (kbd "M-<SPC>") 'push-mark-no-activate)
  (define-key map (kbd "C-d") 'delete-forward-char)
  (define-key map (kbd "C-x C-x") 'exchange-point-and-mark-dwim)
  (define-key map (kbd "C-x k") 'kill-buffer-dwim))

;; For bindings that I do want to be overriden by minor modes and the like, I
;; use the built-in `global-map' and all the standard tools. Nothing fancy.

;; https://www.reddit.com/r/emacs/comments/67rlfr/esc_vs_cg/dgsozkc/
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key [swipe-left])
(global-unset-key [swipe-right])

(let ((map global-map))
  ;; replace mappings
  (define-key map [remap query-replace] 'vr/query-replace)
  (define-key map [remap capitalize-word] 'capitalize-dwim)
  (define-key map [remap downcase-word]   'downcase-dwim)
  (define-key map [remap upcase-word]     'upcase-dwim)
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
;; switch between light and dark themes. The main entry point is
;; `load-theme-dwim'.

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

(defun macos-toggle-system-appearance nil
  "Toggle macOS's system appearance between dark and light modes."
  (interactive)
  (shell-command-to-string "osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'"))

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

(defun load-theme-dwim (&optional color)
  "Load users preferred theme, based on ARG or macOS appearance.
Disables all current themes, then:
- if COLOR is \"light\" or \"dark\", load the `light-theme' or `dark-theme'.
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

(add-hook 'mac-effective-appearance-change-hook 'theme-color-toggle)

(defun load-theme-cleanly (theme)
  "Disable active themes, then load theme."
  (interactive
   (list (intern
          (completing-read "Load Theme: "
                           (mapcar 'symbol-name (custom-available-themes))))))
  (disable-current-themes)
  (load-theme theme t))

(config-package 'modus-themes
  "Require and cofigure the modus-themes."
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
(setq dark-theme  'modus-vivendi)
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
  (setq line-spacing nil)
  (let ((mono "IBM Plex Mono")
        (vari "IBM Plex Serif")
        (mode "IBM Plex Sans")
        (mono-height 120)
        (mode-height 140))
    (custom-set-faces
     `(default ((t :family ,mono :height ,mono-height)))
     `(fixed-pitch ((t :family ,mono)))
     `(variable-pitch ((t :family ,vari)))
     `(mode-line ((t :family ,mode :height ,mode-height)))
     `(mode-line-inactive ((t :family ,mode :height ,mode-height))))))


;;; Mode-Line

(setq display-time-default-load-average nil)
(setq display-time-format "  [%F]  %R")
(setq battery-mode-line-format "  %b%p%%")

(defvar date-time-battery nil
  "Used by `toggle-date-time-battery' for displaying date, time, battery, in mode-line.")

(defun toggle-date-time-battery nil
  "Toggle `display-time-mode' & `display-battery-mode' based on `date-time-battery'."
  (interactive)
  (if (eq date-time-battery nil)
      (progn
        (setq date-time-battery t)
        (display-time-mode 1)
        (display-battery-mode 1))
    (progn
      (setq date-time-battery nil)
      (display-time-mode -1)
      (display-battery-mode -1))))

(delight 'eldoc-mode nil "eldoc")
(delight 'emacs-lisp-mode "Elisp" "elisp-mode")
(delight 'auto-fill-function " Fill" "simple")


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
;; thing anyway, *Completions*, for example. Also, Org Mode seems to do
;; whatever the hell it wants.
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
  "Most commonly used window commands."
  [["Splits"
    ("s" "Split Sensibly" split-window-dwim)
    ("h" "Split Horizontal" split-window-below)
    ("v" "Split Vertical"   split-window-right)
    ("b" "Balance"    balance-windows)
    ("f" "Fit"        fit-window-to-buffer)
    ("r" "Rotate Split" rotate-window-split)
    ("R" "Swap Windows" swap-windows)]
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
;; but does not request confirmation for regular buffers.
;;
;; The option `buffer-offer-save' tells Emacs to prompt to you save modified
;; regular buffers when EXITING Emacs, but no such option exists for KILLING
;; regular buffers (as described in the docstring for `buffer-offer-save').
;;
;; The below creates a buffer-local variable and function for
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

;; Aside from creating markdown and org-mode buffers, sometimes it is nice to
;; just have a quick place to work with text.
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

(with-eval-after-load 'outline
  ;; If you instead bind these in `outline-minor-mode' they might not be
  ;; properly overridden by other minor modes, like org-mode. So a global
  ;; binding is safer.
  (global-set-key (kbd "C-<tab>") 'bicycle-cycle)
  (global-set-key (kbd "S-<tab>") 'bicycle-cycle-global))


;;; Navigation And Selection


;;;; Navigation Bindings

;; These are bindings I think should be available in a few different modes.
;; They are defined inside a function so you can apply these bindings to any
;; keymap you want simply by calling this function and passing the keymap as
;; an argument.
(defun define-navigation-keys (map)
  "Defines navigation keys for MAP supplied by argument."
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
  (define-key map (kbd "<") 'beginning-of-buffer)
  (define-key map (kbd ">") 'end-of-buffer)
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

(defvar navigation-mode-eldoc-message "** Navigation Mode Active **"
  "Message to be displayed when `navigation-mode' is active.")

(defun navigation-mode-eldoc-function ()
  (eldoc-message navigation-mode-eldoc-message))

(define-minor-mode navigation-mode
  "Minor mode for nagivating buffers."
  :init-value nil
  :lighter " =NΛV="
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

(let ((map navigation-mode-map))
  ;; Apply common navigation keys:
  (define-navigation-keys map)
  ;; If you want to override them, just redefine below...
  (define-key map (kbd "SPC") 'navigation-mode-exit-and-mark)
  (define-key map (kbd "C-SPC") 'navigation-mode-exit-and-mark)
  (define-key map (kbd "q")   'navigation-mode))

(define-key global-map (kbd "s-j") 'navigation-mode)


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
    "Disable the selected minor mode.
Useful for mode hooks where you don't want selected to be active."
    (selected-minor-mode -1))

  (with-eval-after-load 'selected
    (delight 'selected-minor-mode nil "selected")
    (let ((map selected-keymap))
      (define-navigation-keys map)
      (define-key map (kbd "r") 'rectangle-mark-mode)
      (define-key map (kbd "R") 'replace-rectangle))))


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
only present in the most recent versions."
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

  (let ((map embark-region-map))
    (define-key map (kbd "q") 'fill-region))

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

(elisp-group 'oblique
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
    (define-key map (kbd "s-z") 'dired-undo)))

(config-package 'olivetti
  nil
  (custom-set-variables
   '(olivetti-body-width 86)))

(config-package 'ytdl
  "YouTube Download"
  (setq ytdl-download-folder user-downloads-directory)
  (setq ytdl-media-player "open")
  (setq ytdl-always-query-default-filename 'yes-confirm))

(elisp-group world-clock
  "Emacs has a world clock!"
  (defalias 'world-clock 'display-time-world)
  (setq display-time-world-time-format "%Z%t%R%t%F"
        zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Chicago" "Chicago")
          ("America/Montreal" "Montreal")
          ("Europe/London" "London"))))


;;; Org

(defun find-org-files ()
  "Find org files in your org directory, pass to completing-read."
  (interactive)
  (find-file (completing-read "Find Org Files: "
                              (directory-files-recursively org-directory "\.org$"))))

(defun find-org-directory ()
  "Open org directory in dired."
  (interactive)
  (find-file org-directory))

(defun consult-grep-orgfiles ()
  (interactive)
  (consult-grep org-directory))

(autoload 'org-export-dispatch "org")
(autoload 'org-store-link "org")


;;;; Org Configuration

(with-eval-after-load 'org

  (custom-set-variables
   '(org-list-allow-alphabetical t)
   '(org-log-done 'time)
   '(org-log-into-drawer t)
   '(org-special-ctrl-a/e t)
   '(org-special-ctrl-k t)
   '(org-return-follows-link t)
   '(org-adapt-indentation t)
   '(org-catch-invisible-edits 'show-and-error)
   '(org-outline-path-complete-in-steps nil)
   '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
   '(org-startup-with-inline-images t)
   '(org-image-actual-width '(600))
   '(org-hide-emphasis-markers t)
   '(org-hide-leading-stars t)
   '(org-ellipsis "...")
   '(org-insert-heading-respect-content t)
   '(org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+")))
   '(org-list-indent-offset 2)
   ;; src blocks
   '(org-src-fontify-natively t)
   '(org-fontify-quote-and-verse-blocks t)
   '(org-src-tab-acts-natively t)
   '(org-edit-src-content-indentation 0)
   ;; Agenda Settings
   '(org-agenda-window-setup 'current-window)
   '(org-agenda-restore-windows-after-quit t)
   '(org-agenda-span 'day)
   '(org-agenda-start-with-log-mode t)
   '(org-agenda-log-mode-items '(closed clock state))
   '(org-agenda-use-time-grid nil)
   '(org-deadline-warning-days 7)
   '(org-agenda-todo-ignore-scheduled nil)
   '(org-agenda-todo-ignore-deadlines nil)
   '(org-agenda-skip-deadline-if-done t)
   '(org-agenda-skip-deadline-prewarning-if-scheduled t))

  (setq org-agenda-files (list org-directory))
  (add-to-list 'org-agenda-files "~/home/writing/kindred/compendium.org")

  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "DELG(g)" "LATER(l)" "|" "DONE(d)" "MOVED(m)" "CANCELED(c)")))

  (setq org-todo-keyword-faces
        '(("DELG" . org-scheduled-previously)
          ("LATER" . org-scheduled-previously)))

  (setq org-capture-templates
        `(("p" "Personal")
          ("pi" "Personal Inbox" entry
           (file+headline ,(concat org-directory "life.org") "Inbox")
           "* %?\n\n" :empty-lines 1)
          ("pl" "Personal Log Entry" entry
           (file+olp+datetree ,(concat org-directory "logbook.org"))
           "* %?\n%T\n\n" :empty-lines 1 :tree-type month )
          ;; -----------------------------
          ("s" "Scanline")
          ("si" "Scanline OPS Inbox" entry
           (file+headline ,(concat org-directory "scanline.org") "Inbox")
           "* %?\n\n" :empty-lines 1)
          ("sf" "Scanline FRANK Inbox" entry
           (file+headline ,(concat org-directory "scanline_frank.org") "FRANK Inbox")
           "* %?\n\n" :empty-lines 1)
          ("sl" "Scanline Log Entry" entry
           (file+olp+datetree ,(concat org-directory "scanline_logbook.org"))
           "* %?\n%T\n\n" :empty-lines 1 :tree-type week )
          ;; -----------------------------
          ("e" "Emacs Config" entry
           (file+headline ,(concat org-directory "emacs.org") "Emacs Config")
           "* TODO %?" :empty-lines 1)
          ("k" "Kiddos Log Entry" entry
           (file+olp+datetree ,(concat org-directory "kiddos_logbook.org"))
           "* %T\n\n%?" :empty-lines 1 :tree-type month )))

  (defun echo-area-tooltips ()
    "Show tooltips in the echo area automatically for current buffer."
    (setq-local help-at-pt-display-when-idle t
                help-at-pt-timer-delay 0)
    (help-at-pt-cancel-timer)
    (help-at-pt-set-timer))

  (add-hook 'org-mode-hook #'echo-area-tooltips)

  (defun org-toggle-checkbox-presence ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-toggle-checkbox)))

  ) ; End org config


;;;; Org Agenda

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("1" "Priority Tasks"
           ((todo "TODO|DELG"
                  ((org-agenda-sorting-strategy '(todo-state-up priority-down))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                   (org-agenda-overriding-header "Active, not scheduled, Tasks: ")))))
          ("!" "Today + Priority Tasks"
           ((agenda 'day)
            (todo "TODO|DELG"
                  ((org-agenda-sorting-strategy '(todo-state-up priority-down))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                   (org-agenda-overriding-header "Active, not scheduled, Tasks: ")))))))

  (define-key org-agenda-mode-map (kbd "S") 'org-agenda-schedule)
  (define-key org-agenda-mode-map (kbd "D") 'org-agenda-deadline)
  (define-key org-agenda-mode-map (kbd "s-z") 'org-agenda-undo))


;;;; Calendar <--> Org Integration

;; You can jump to org's agenda from the calendar, and capture from any date
;; in the agenda, why now capture from the calendar as well?

(defun org-calendar-capture (&optional with-time)
  "Call `org-capture' with the date at point.
With a `C-1' prefix, use the HH:MM value at point, if any, or the
current HH:MM time."
  (interactive "P")
  (if (not (eq major-mode 'calendar-mode))
      (user-error "You cannot do this outside of calendar buffers")
    (progn
      (require 'org)
      (let ((org-overriding-default-time
	         (org-get-cursor-date (equal with-time 1))))
        (delete-window)
        (call-interactively 'org-capture)))))

(with-eval-after-load 'calendar
  (define-key calendar-mode-map "k" 'org-calendar-capture))


;;;; Fixing `org-todo' and `org-agenda-todo'

;; Org has an absolutely infuriating habit of destroying window layouts to
;; display its various pop-up windows. These two transients help me retain a
;; small dose of my sanity. I really wish this wasn't necessary.
;; Also see: https://emacs.stackexchange.com/a/14818

(define-transient-command org-todo-transient ()
  [["Org TODO Status"
    ("t" "TODO"     (lambda () (interactive) (org-todo "TODO")))
    ("g" "DELG"     (lambda () (interactive) (org-todo "DELG")))
    ("l" "LATER"    (lambda () (interactive) (org-todo "LATER")))
    ("d" "DONE"     (lambda () (interactive) (org-todo "DONE")))
    ("c" "CANCELED" (lambda () (interactive) (org-todo "CANCELED")))
    ("m" "MOVED"    (lambda () (interactive) (org-todo "MOVED")))
    ]])

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-t") 'org-todo-transient))


(define-transient-command org-agenda-todo-transient ()
  [["Org TODO Status"
    ("t" "TODO"     (lambda () (interactive) (org-agenda-todo "TODO")))
    ("g" "DELG"     (lambda () (interactive) (org-agenda-todo "DELG")))
    ("l" "LATER"    (lambda () (interactive) (org-agenda-todo "LATER")))
    ("d" "DONE"     (lambda () (interactive) (org-agenda-todo "DONE")))
    ("c" "CANCELED" (lambda () (interactive) (org-agenda-todo "CANCELED")))
    ("m" "MOVED"    (lambda () (interactive) (org-agenda-todo "MOVED")))
    ]])

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "t") 'org-agenda-todo-transient))


;;; Transient

(transient-define-prefix general-transient ()
  "General-purpose transient."
  [["Actions/Toggles"
    ("a" "AutoFill" auto-fill-mode)
    ("j" "Dired Jump" dired-jump)
    ("." "Repeat Command" repeat-complex-command)
    ("k" "Kill Buffer" kill-buffer-dwim)
    ("b" "iBuffer" ibuffer)
    ("p" "Pop to Mode" pop-to-buffer-same-mode)]
   ["Transients"
    ("o" "Org..." general-transient--org)
    ("c" "Consult..." general-transient--consult)
    ("2" "Secondary..." secondary-selection-transient)
    ("S" "Spelling..." flyspell-mode-transient)]
   ["Other"
    ("T" "Toggle macOS Apperance" macos-toggle-system-appearance)
    ("t" "Load Theme" load-theme-cleanly)
    ("d" "Date/Time mode-line" toggle-date-time-battery)
    ("w" "World Clock" world-clock)
    ("s o" "*scratch-org*" scratch-buffer-org)
    ("s m" "*scratch-markdown*" scratch-buffer-markdown)]])

(transient-define-prefix general-transient--org ()
  "Transient for Org commands useful outside org mode."
  ["Org Mode"
   [("s" "Store Link" org-store-link)
    ("g" "Go to Last Capture" org-capture-goto-last-stored)]
   [("D" "Find Org Dir" find-org-directory)
    ("F" "Find Org Files..." find-org-files)
    ("H" "Find Org Heading" consult-org-agenda)
    ("G" "Grep Org Files" consult-grep-orgfiles)]])

(transient-define-prefix general-transient--consult ()
  ["Consult"
   ("l" "Line" consult-line)
   ("o" "Outline" consult-outline)
   ("g" "Grep" consult-grep)
   ("b" "Buffer" consult-buffer)
   ("a" "Apropos" consult-apropos)
   ("m" "Marks" consult-mark)
   ("M" "Minor Modes" consult-minor-mode-menu)])

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
          (info-mode-help-transient)))
      nil ; if the above succeeds, do nothing else, otherwise...
    (message "No transient defined for this mode.")))

(with-eval-after-load 'org
  (transient-define-prefix org-mode-help-transient ()
    "Transient for Org Mode"
    ["Org Mode"
     ["Navigation"
      ("o" "Outline" consult-org-heading)
      ("f" "Find Heading" consult-org-agenda)
      ("c" "Go To Calendar" org-goto-calendar)
      ("n" "Narrow/Widen" narrow-or-widen-dwim)
      ("v" "Visible Markup" visible-mode)]
     ["Item"
      (":" "Set Tags" org-set-tags-command)
      ("a" "Archive Subtree" org-archive-subtree)
      ("r" "Refile" org-refile)
      ("x" "Checkbox State" org-toggle-checkbox)
      ("X" "Checkbox Presence" org-toggle-checkbox-presence)]
     ["Insert"
      ("." "Insert Date, Active" org-time-stamp)
      ("!" "Insert Date, Inactive" org-time-stamp-inactive)
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


;;; Elfeed

(add-hook 'elfeed-search-mode-hook 'disable-selected-minor-mode)
(add-hook 'elfeed-show-mode-hook   'disable-selected-minor-mode)

(setq shr-max-image-proportion 0.5
      shr-width 80
      shr-bullet "• ")

(with-eval-after-load 'elfeed

  (custom-set-variables
   '(elfeed-use-curl t)
   '(elfeed-db-directory (concat user-emacs-directory "elfeed/"))
   '(elfeed-enclosure-default-dir user-downloads-directory))

  (load "~/home/src/lisp/rss-feeds.el")

  ;; Why doesn't this exist in show mode?
  (defalias 'elfeed-show-tag--unread (elfeed-expose #'elfeed-show-tag 'unread)
    "Mark the current entry unread.")
  (defalias 'elfeed-show-tag--read (elfeed-expose #'elfeed-show-untag 'unread)
    "Mark the current entry read.")

  ;; Stars in search mode
  (defalias 'elfeed-search-tag--star (elfeed-expose #'elfeed-search-tag-all 'star)
    "Add the 'star' tag to all selected entries")
  (defalias 'elfeed-search-untag--star (elfeed-expose #'elfeed-search-untag-all 'star)
    "Remove the 'star' tag to all selected entries")

  ;; Stars in show mode
  (defalias 'elfeed-show-tag--star (elfeed-expose #'elfeed-show-tag 'star)
    "Add the 'star' tag to current entry")
  (defalias 'elfeed-show-tag--unstar (elfeed-expose #'elfeed-show-untag 'star)
    "Remove the 'star' tag to current entry")

  (defun elfeed-search:emacs () (interactive) (elfeed-search-set-filter "+unread +emacs"))
  (defun elfeed-search:other () (interactive) (elfeed-search-set-filter "+unread -emacs"))
  (defun elfeed-search:star  () (interactive) (elfeed-search-set-filter "+star"))

  (defun elfeed-search-browse-url-background ()
    "Visit the current entry, or region entries, in browser without losing focus."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (mapc (lambda (entry)
              (browse-url-macos-background (elfeed-entry-link entry))
              (elfeed-untag entry 'unread)
              (elfeed-search-update-entry entry))
            entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))

  ;; TODO narrow to feed at point (below does not work):
  ;; (elfeed-feed-id (elfeed-search-selected t))

  (defun elfeed-show-visit-background ()
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (message "Sent to browser: %s" link)
        (browse-url-macos-background link))))

  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "b") 'elfeed-search-browse-url-background)
    (define-key map (kbd "*") 'elfeed-search-tag--star)
    (define-key map (kbd "8") 'elfeed-search-untag--star)
    (define-key map (kbd "o") 'delete-other-windows)
    (define-key map (kbd "E") 'elfeed-search:emacs)
    (define-key map (kbd "O") 'elfeed-search:other)
    (define-key map (kbd "S") 'elfeed-search:star))

  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "r") 'elfeed-show-tag--read)
    (define-key map (kbd "u") 'elfeed-show-tag--unread)
    (define-key map (kbd "*") 'elfeed-show-tag--star)
    (define-key map (kbd "8") 'elfeed-show-tag--unstar)
    (define-key map (kbd "b") 'elfeed-show-visit-background)
    (define-key map (kbd "o") 'delete-other-windows)
    (define-key map (kbd "d") 'ytdl-download))

  ) ; End elfeed


;;; End of init.el
