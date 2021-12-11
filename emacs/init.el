;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Oliver Taylor
;; Learning Emacs was my COVID pandemic project, now I cannot escape.

;; Author: Oliver Taylor
;; URL: https://olivertaylor.net
;; URL: https://github.com/olivertaylor/dotfiles

;; This config targets Emacs 28.1

;; This file has an outline which can be viewed by looking at comments
;; starting with three or more semicolons. `outline-minor-mode' supports this
;; convention by default and helps with navigation. You can also create an
;; occur buffer with the search /^;;;+/.


;;; Commentary:

;; Every Emacs configuration is unique to the person who created it, to their
;; needs and their taste. This one takes the following approach:
;;
;;   + I prefer to write my own code instead of installing packages, because
;;     I love pain and hate sleep.
;;   + I use package.el and don't use Use-Package. Get out your pitchfork,
;;     I'll be waiting for you at the castle.
;;   + It is NOT modular, so if you don't have all the listed packages
;;     installed large parts of the config won't work. It is also (largely)
;;     grouped by topic and not by package, so a package's keybindings might
;;     be in a few different places. If this drives you crazy simply accept
;;     that the world is an insane and unfair place and have an ice cream,
;;     you'll soon feel better.
;;   + Symbol names are not prefixed (for the most part) so take care to
;;     to avoid collisions if you copy something into your own config. This
;;     is probably not one of my good ideas.

;; If you like this config I would recommend reading these:
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; https://github.com/raxod502/radian
;; https://github.com/oantolin/emacs-config
;; https://github.com/skeeto/.emacs.d
;; https://github.com/bbatsov/prelude


;;; Package Management

;; The very first thing in the config should be setting up the packages I
;; need. This is done because when Emacs encounters an error in the init file
;; (which can happen when packages are missing) Emacs stops loading the init
;; file altogether. Setting up packages first thing ensures that at a minimum
;; Emacs will load the settings required for installing the packages this
;; config needs.

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-archive-priorities '(("gnu" . 30)
                                   ("nongnu" . 25)
                                   ("melpa-stable" . 20)
                                   ("melpa" . 10)))

(setq package-selected-packages
      '(consult
        corfu
        delight
        embark
        embark-consult
        exec-path-from-shell
        fountain-mode
        hide-mode-line
        lua-mode
        magit
        markdown-mode
        modus-themes
        olivetti
        orderless
        org
        vertico
        visual-regexp
        visual-regexp-steroids))


;;; Settings

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
 '(repeat-mode 1)
 '(global-auto-revert-mode t)
 '(save-place-mode t)
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(ring-bell-function 'ignore)
 '(pixel-scroll-precision-mode 1)
 '(set-language-environment "UTF-8")
 '(frame-title-format '("%b"))
 '(uniquify-buffer-name-style 'forward)
 '(vc-follow-symlinks t)
 '(find-file-visit-truename t)
 '(create-lockfiles nil)
 '(make-backup-files nil)
 '(bookmark-save-flag 1)
 '(bookmark-menu-confirm-deletion t)
 '(describe-bindings-outline t)
 '(word-wrap t)
 '(truncate-lines t)
 '(save-interprogram-paste-before-kill t)
 '(kill-do-not-save-duplicates t)
 '(kill-ring-max 512)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(mark-even-if-inactive nil)
 '(delete-selection-mode t)
 '(mark-ring-max 512)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(fill-column 78)
 '(delete-by-moving-to-trash t)
 '(trash-directory "~/.Trash"))


;;; Critical Setup

;; There are 2 ways I install packages/lisp that aren't a part of this file:
;; 1. Using `package-install-selected-packages',
;; 2. in a "local-package" directory that contains cloned git repositories
;;    that I can't install from a package archive such as elpa.gnu.org.
;;
;; I configure those packages using the `elpa-package' and `local-package'
;; macros, which do some sanity checks and ensure the packages are loaded.

(defmacro elpa-package (package &rest body)
  "Eval BODY only if PACKAGE is installed."
  (declare (indent defun))
  `(if (package-installed-p ,package)
       (progn ,@body)
     (message (concat "Package \'"
                      (symbol-name ,package)
                      "\' is not installed... skipping config."))))

(defvar local-package-dir nil
  "Directory containing lisp files which are not in dotfiles or an ELPA package.")

(defmacro local-package (package-dir &rest body)
  "Eval BODY if PACKAGE-DIR exists in `local-package-dir'."
  (declare (indent defun))
  `(let* ((package-path (concat local-package-dir ,package-dir)))
     (if (file-exists-p package-path)
         (progn
           (add-to-list 'load-path package-path)
           ,@body)
       (message (concat "\'" ,package-dir "\'"
                        " cannot be found at \'"
                        package-path
                        "\'... skipping config.")))))

(prog1 "environment"
  ;; Important paths for this setup to work.
  (setq-default default-directory "~/home/")
  (setq org-directory "~/home/org/")
  (defvar user-downloads-directory "~/Desktop/")
  (setq local-package-dir "~/home/src/lisp/")
  (add-to-list 'load-path "~/home/dot/emacs/lisp/"))

(prog1 "transient"
  ;; I use a lot of transients in this config, so I need to make sure it is
  ;; loaded and configured before those are declared below.
  (autoload 'transient-define-prefix "transient" nil t)
  (setq transient-detect-key-conflicts t))


;;; Functions

;; Some of these functions I wrote myself, many of them I copied (and perhaps
;; modified) from other people's configs.

(defun find-user-init-file ()
  "Find the user-init-file."
  (interactive)
  (find-file user-init-file))

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

(defun other-window-previous nil
  "Select previous window."
  (interactive)
  (other-window -1))

(defun split-window-dwim ()
  "Interactive wrapper around `split-window-sensibly'."
  (interactive)
  (split-window-sensibly))

(defun find-file-recursively (&optional path)
  "Find Files Recursively using completing read.
Uses the `default-directory' unless a path is supplied."
  (interactive)
  (find-file (completing-read "Find File Recursively: "
                              (directory-files-recursively (if path path default-directory) ".+" t))))

(defalias 'find-files-recursively 'find-file-recursively)

(defun exchange-point-and-mark-dwim ()
  "Respect region active/inactive and swap point and mark.
If a region is active, then leave it activated and swap point and mark.
If no region is active, then just swap point and mark."
  (interactive)
  (if (use-region-p)
      (exchange-point-and-mark)
    (exchange-point-and-mark)
    (deactivate-mark nil)))

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

(defun kill-buffer-dwim (&optional u-arg)
  "Call kill-current-buffer, with C-u: call kill-buffer."
  (interactive "P")
  (if u-arg
      (call-interactively 'kill-buffer)
    (call-interactively 'kill-current-buffer)))

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

(defun kf-make-file-executable ()
  "Make current buffer's file have permissions 755 \(rwxr-xr-x)\.
This will save the buffer if it is not currently saved."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer)
  (chmod (buffer-file-name) 493))

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

(defun describe-symbol-at-point ()
  "Run `describe-symbol' for the `symbol-at-point'."
  (interactive)
  (describe-symbol (symbol-at-point)))

(defun split-window-below-select nil
  "Split window below and select it."
  (interactive)
  (select-window (split-window-below)))

(defun split-window-right-select nil
  "Split window to the right and select it."
  (interactive)
  (select-window (split-window-right)))

(defun backward-kill-line nil
  "Kill from point to beginning of line."
  (interactive)
  (kill-line 0))


;;; Bindings

;;;; Bosskey Mode

;; The technique below is taken from the bind-key package. It places all the
;; bindings I don't want overridden into a minor mode which is inserted into
;; the `emulation-mode-map-alists', so only very few things can override them.

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


;;;; Emacsen Improvements

;; These bindings are basically replacements or improvements on Emacs's
;; default bindings. No, nothing fancy, just some improvements.

;; https://www.reddit.com/r/emacs/comments/67rlfr/esc_vs_cg/dgsozkc/
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Because they're in `bosskey-mode-map' these bindings will never be
;; overridden by minor modes.
(let ((map bosskey-mode-map))
  (define-key map (kbd "C-x C-f") 'find-file)
  (define-key map (kbd "C-x f")   'find-file-other-window)
  (define-key map (kbd "C-x C-b") 'switch-to-buffer)
  (define-key map (kbd "C-x b")   'switch-to-buffer-other-window)
  (define-key map (kbd "C-x k")   'kill-buffer-dwim)
  (define-key map (kbd "M-/") 'completion-at-point)
  (define-key map (kbd "M-\\") 'cycle-spacing)
  (define-key map (kbd "M-z") 'zap-up-to-char)
  (define-key map (kbd "M-i") 'imenu)
  (define-key map (kbd "C-M-h") 'mark-line)
  (define-key map (kbd "C-d") 'delete-forward-char)
  (define-key map (kbd "C-x C-x") 'exchange-point-and-mark-dwim)
  (define-key map (kbd "M-o") 'other-window)
  (define-key map (kbd "M-O") 'other-window-previous)
  (define-key map (kbd "M-[") 'pop-to-mark-command)
  (define-key map (kbd "M-]") 'unpop-to-mark-command)
  (define-key map (kbd "<C-return>") 'universal-transient)
  (define-key map (kbd "C-.") 'embark-act))

(with-eval-after-load 'magit
  ;; Magit overrides `bosskey-mode-map', so I need to override this here:
  (define-key magit-file-section-map (kbd "<C-return>") 'universal-transient))

;; For bindings that I do want to be overriden by minor/major modes, I use the
;; built-in `global-map'.
(let ((map global-map))
  ;; replace mappings
  (define-key map [remap capitalize-word] 'capitalize-dwim)
  (define-key map [remap downcase-word]   'downcase-dwim)
  (define-key map [remap upcase-word]     'upcase-dwim)
  ;; Make shift-click extend the region.
  (define-key map [S-down-mouse-1] 'ignore)
  (define-key map [S-mouse-1] 'mouse-save-then-kill)
  ;; Use M-drag-mouse-1 to create rectangle regions.
  (define-key map [M-down-mouse-1] #'mouse-drag-region-rectangle) ; down
  (define-key map [M-drag-mouse-1] #'ignore)                      ; drag
  (define-key map [M-mouse-1]      #'mouse-set-point))            ; up

(define-key help-map "s" 'describe-symbol-at-point)

;; I find the default mark-setting bindings to be difficult to remember. Who
;; the heck can remember all these esoteric bindings? Much better to make
;; these a simple transient dispatcher and give it a nice binding.

(transient-define-prefix set-mark-transient ()
  "Transient dispatcher for marking commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  ["Set/Extend Mark To..."
   [("b" "Whole Buffer" mark-whole-buffer)
    ("<" "Beginning of Buffer" mark-beginning-of-buffer)
    (">" "End of Buffer" mark-end-of-buffer)]
   [("w" "Word" mark-word)
    ("l" "Line" mark-line)
    ("p" "Paragraph" mark-paragraph)
    ("s" "Sentence" mark-sentence)
    ("P" "Page" mark-page)]
   [("S" "Sexp" mark-sexp)
    ("D" "Defun" mark-defun)]])

(define-key global-map (kbd "C-S-SPC") 'set-mark-transient)


;;;; Repeat Mode

(defvar buffer-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") 'next-buffer)
    (define-key map (kbd "<right>") 'previous-buffer)
    map)
  "Keymap to repeat buffer navigation commands. Used in `repeat-mode'.")
(put 'next-buffer 'repeat-map 'buffer-navigation-repeat-map)
(put 'previous-buffer 'repeat-map 'buffer-navigation-repeat-map)

(defvar winner-mode-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") 'winner-undo)
    (define-key map (kbd "<right>") 'winner-redo)
    map)
  "Keymap to repeat winner-undo/redo. Used in `repeat-mode'.")
(put 'winner-undo 'repeat-map 'winner-mode-repeat-map)
(put 'winner-redo 'repeat-map 'winner-mode-repeat-map)


;;;; Package Operations

(define-prefix-command 'pkg-ops-map nil "Packages")

(let ((map pkg-ops-map))
  (define-key map "h" '("describe" . describe-package))
  (define-key map "a" '("autoremove" . package-autoremove))
  (define-key map "d" '("delete" . package-delete))
  (define-key map "i" '("install" . package-install))
  (define-key map "s" '("selected" . package-install-selected-packages))
  (define-key map "r" '("refresh" . package-refresh-contents))
  (define-key map "l" '("list" . list-packages)))

(global-set-key (kbd "C-c p") 'pkg-ops-map)


;;;; MacOS

;; I do value consistency, and I do use Emacs on a Windows machine where the
;; below is not available, but I just can't give up these bindings on a Mac.
(when (string= system-type "darwin")
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta)
  (let ((map global-map))
    (define-key map (kbd "s-z") 'undo-only)
    (define-key map (kbd "s-Z") 'undo-redo)
    (define-key map (kbd "s-x") 'kill-region)
    (define-key map (kbd "s-c") 'kill-ring-save)
    (define-key map (kbd "s-v") 'yank)
    (define-key map (kbd "s-<backspace>") 'backward-kill-line)
    (define-key map (kbd "s-s") 'save-buffer)
    (define-key map (kbd "s-m") 'iconify-frame)
    (define-key map (kbd "s-,") 'find-user-init-file)
    (define-key map (kbd "s-q") 'save-buffers-kill-emacs)))


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

(defun macos-dark-p ()
  "Return t if macOS appearance is dark."
  (interactive)
  (string= (shell-command-to-string "defaults read -g AppleInterfaceStyle")
           "Dark\n"))

(defun macos-toggle-system-appearance nil
  "Toggle macOS's system appearance between dark and light modes."
  (interactive)
  (shell-command-to-string "osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'"))

(add-hook 'mac-effective-appearance-change-hook 'theme-color-toggle)

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
         (if (macos-dark-p)
             (load-theme-dwim 'dark)
           (load-theme-dwim 'light)))
        ((eq color nil)
         (theme-color-toggle))))

(defun load-theme-cleanly (theme)
  "Disable active themes, then load theme."
  (interactive
   (list (intern
          (completing-read "Load Theme: "
                           (mapcar 'symbol-name (custom-available-themes))))))
  (disable-current-themes)
  (load-theme theme t))

(defun load-system-color-theme nil
  "On startup, try to match system color, otherwise load `default-color-theme'."
  (if (macos-dark-p)
      (load-theme-dwim 'dark)
    (if (eq default-theme-color 'light)
        (load-theme-dwim 'light)
      (load-theme-dwim 'dark))))

(setq light-theme 'modus-operandi)
(setq dark-theme  'modus-vivendi)
(setq default-theme-color 'light)

(elpa-package 'modus-themes
  (require 'modus-themes)
  (custom-set-variables
   '(modus-themes-mixed-fonts t)
   '(modus-themes-italic-constructs t)
   '(modus-themes-syntax '(yellow-comments green-strings))
   '(modus-themes-links '(neutral-underline))
   '(modus-themes-region '(bg-only))
   '(modus-themes-mode-line '(accented 3d))
   '(modus-themes-mode-line-padding 2)
   '(modus-themes-vivendi-color-overrides '((fg-main . "#d5d5d5")))
   '(modus-themes-org-blocks '(tinted-background))
   '(modus-themes-org-agenda '((header-block . (variable-pitch scale-title))
                               (header-date . (bold-today))
                               (scheduled . rainbow))))
  (modus-themes-load-themes)
  (load-system-color-theme))


;;; Fonts

;; The below code provides the following features:

;; 1. A way to define sets of fonts and font sizes, which you can load as the
;;    default for all frames or per-buffer.
;; 2. A mode for scaling only the variable-pitch face up to the exact size of
;;    your choosing so that mixed-font buffers look great.

(defvar custom-fonts-alist nil
  "An alist of font properties used by `set-custom-fonts'.
The alist should be formatted thus:

(setq custom-fonts-alist
      '((Apple . ( :mono \"SF Mono\"
                   :vari \"New York\"
                   :mode \"SF Compact Text\"
                   :line nil
                   :mono-height 120
                   :mode-height 130
                   :vari-height 140))))")

(defun set-custom-fonts (font-settings)
  "Prompt user and set fonts according to selection from `custom-fonts-alist'.
FONT-SETTINGS should be a string."
  (interactive
   (list (completing-read
          "Select default font pairings: "
          (mapcar #'car custom-fonts-alist))))
  (let* ((fonts (intern font-settings))
         (properties (alist-get fonts custom-fonts-alist))
         (mono (plist-get properties :mono))
         (vari (plist-get properties :vari))
         (mode (plist-get properties :mode))
         (line (plist-get properties :line))
         (mono-height (plist-get properties :mono-height))
         (mode-height (plist-get properties :mode-height))
         (vari-height (plist-get properties :vari-height)))
    (setq line-spacing line
          variable-pitch-adjust-height vari-height)
    (custom-set-faces
     `(default ((t :family ,mono :height ,mono-height)))
     `(fixed-pitch ((t :family ,mono)))
     `(variable-pitch ((t :family ,vari)))
     `(mode-line ((t :family ,mode :height ,mode-height)))
     `(mode-line-inactive ((t :family ,mode :height ,mode-height))))))

(defvar variable-pitch-adjust-height nil
  "Used by `variable-pitch-adjust-mode' to determine the
variable-pitch scaling amount in that mode.")

(define-minor-mode variable-pitch-adjust-mode
  "Minor mode to adjust only the variable-pitch face height buffer-locally.
Scales the variable-pitch height up to the height defined by
‘variable-pitch-adjust-height’ and the fixed-pitch face down to
match the default face height. Thus, in mixed-font settings you
can scale the variable-pitch height independently of the
fixed-pitch and default face heights."
  :init-value nil
  :lighter " V+"
  (if variable-pitch-adjust-mode
      (progn
        (setq-local variable-pitch-remapping
                    (face-remap-add-relative 'variable-pitch
                                             :height (/ variable-pitch-adjust-height
                                                        (float (face-attribute 'default :height)))))
        (setq-local fixed-pitch-remapping
                    (face-remap-add-relative 'fixed-pitch
                                             :height (/ (float (face-attribute 'default :height))
                                                        variable-pitch-adjust-height)))
        (force-window-update (current-buffer)))
    (progn
      (face-remap-remove-relative variable-pitch-remapping)
      (face-remap-remove-relative fixed-pitch-remapping)
      (force-window-update (current-buffer)))))

(add-hook 'buffer-face-mode-hook (lambda () (variable-pitch-adjust-mode 'toggle)))

(defvar-local buffer-remap-faces-default-cookie nil
  "Cookie used for `buffer-remap-faces'.")
(defvar-local buffer-remap-faces-fixed-pitch-cookie nil
  "Cookie used for `buffer-remap-faces'.")
(defvar-local buffer-remap-faces-variable-pitch-cookie nil
  "Cookie used for `buffer-remap-faces'.")

(defun buffer-remap-faces--clear nil
  "In the current buffer, remove all buffer-remap-faces cookies."
  (interactive)
  (face-remap-remove-relative buffer-remap-faces-default-cookie)
  (face-remap-remove-relative buffer-remap-faces-fixed-pitch-cookie)
  (face-remap-remove-relative buffer-remap-faces-variable-pitch-cookie)
  (setq-local buffer-remap-faces-default-cookie nil)
  (setq-local buffer-remap-faces-fixed-pitch-cookie nil)
  (setq-local buffer-remap-faces-variable-pitch-cookie nil)
  (force-window-update (current-buffer)))

(defun buffer-remap-faces (font-settings)
  "Prompt user and set fonts for the current buffer according to selection from `custom-fonts-alist'.
It should probably be a mode instead."
  (interactive
   (list (completing-read
          "Select font pairings for Buffer: "
          (mapcar #'car custom-fonts-alist))))
  (let* ((fonts (intern font-settings))
         (properties (alist-get fonts custom-fonts-alist))
         (mono (plist-get properties :mono))
         (vari (plist-get properties :vari))
         (mono-height (plist-get properties :mono-height)))
    (buffer-remap-faces--clear)
    (setq-local buffer-remap-faces-default-cookie
                (face-remap-add-relative 'default
                                         :family mono
                                         :height mono-height))
    (setq-local buffer-remap-faces-variable-pitch-cookie
                (face-remap-add-relative 'fixed-pitch
                                         :family mono))
    (setq-local buffer-remap-faces-fixed-pitch-cookie
                (face-remap-add-relative 'variable-pitch
                                         :family vari))
    (force-window-update (current-buffer))))

;; This makes it so text-scale adjustments operate exactly one point size at a
;; time. The original value is 1.2, which jumps point-sizes when stepping
;; up/down.
(setq text-scale-mode-step 1.09)

;; Set the different font-pairings (and settings for them) you want to be able
;; to switch between.
(setq custom-fonts-alist
      '((Apple . ( :mono "SF Mono"
                   :vari "New York"
                   :mode "SF Compact Text"
                   :line nil
                   :mono-height 120
                   :mode-height 130
                   :vari-height 140))
        (IBM . ( :mono "IBM Plex Mono"
                 :vari "IBM Plex Serif"
                 :mode "IBM Plex Sans"
                 :line nil
                 :mono-height 120
                 :mode-height 140
                 :vari-height 130))
        (Go . ( :mono "Go Mono"
                :vari "Go"
                :mode "Go"
                :line nil
                :mono-height 120
                :mode-height 130
                :vari-height 130))
        (Pragmata . ( :mono "PragmataPro"
                      :vari "Fira Sans"
                      :mode "PragmataPro"
                      :line 1
                      :mono-height 120
                      :mode-height 120
                      :vari-height 130))))

;; On startup, use this set of fonts:
(set-custom-fonts "IBM")


;;; Mode-Line

;;;; Display Battery/Time in mode-line

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


;;;; Radian Mode line

;; The following code is a modified version of the code found here:
;; https://github.com/raxod502/radian/blob/a8bb096c80d2daf34d380331fb23fa338ac1bb17/emacs/radian.el#L5168
;; It customizes the mode line to something like:
;; [*] radian.el   18% (18,0)     [radian:develop*]  (Emacs-Lisp)

(defun radian-mode-line-buffer-modified-status ()
  "Return a mode line construct indicating buffer modification status.
This is [*] if the buffer has been modified and whitespace
otherwise. (Non-file-visiting buffers are never considered to be
modified.) It is shown in the same color as the buffer name, i.e.
`mode-line-buffer-id'."
   (if (and (buffer-modified-p)
            (buffer-file-name))
       "  ●"))

(defun radian-mode-line-buffer-read-only-status ()
  "Return a mode-line construct indicating buffer read-only status."
  (if buffer-read-only
      "  -RO-"))

(defun radian-mode-line-buffer-confirm-kill-status ()
  "Return a mode-line construct indicating `buffer-confirm-kill' status."
  (if (and buffer-confirm-kill
           (buffer-modified-p))
      "  -CK-"))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here. Check the docstrings for more
;; information.
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

;; https://emacs.stackexchange.com/a/7542/12534
(defun radian--mode-line-align (left right)
  "Render a left/right aligned string for the mode line.
LEFT and RIGHT are strings, and the return value is a string that
displays them left- and right-aligned respectively, separated by
spaces."
  (let ((width (- (window-total-width) (length left))))
    (format (format "%%s%%%ds" width) left right)))

(defcustom radian-mode-line-left
  '(;; Show indicator if the buffer is modified.
    (:eval (radian-mode-line-buffer-modified-status))
    ;; Show the name of the current buffer.
    "   " mode-line-buffer-identification
    ;; Show indicator if buffer is protected by `buffer-confirm-kill'.
    (:eval (radian-mode-line-buffer-confirm-kill-status))
    ;; Show indicator if buffer is read-only.
    (:eval (radian-mode-line-buffer-read-only-status))
    ;; Show the row and column of point.
    "   " mode-line-position
    ;; Show the active major and minor modes.
    "   " mode-line-modes)
  "Composite mode line construct to be shown left-aligned."
  :type 'sexp)

(defcustom radian-mode-line-right mode-line-misc-info
    "Composite mode line construct to be shown right-aligned."
    :type 'sexp)

;; Actually reset the mode line format to show all the things we just
;; defined.
(setq-default mode-line-format
              '(:eval (replace-regexp-in-string
                       "%" "%%"
                       (radian--mode-line-align
                        (format-mode-line radian-mode-line-left)
                        (format-mode-line radian-mode-line-right))
                       'fixedcase 'literal)))


;;; Windows

;; Settings for how Emacs displays windows, splits, etc.

(custom-set-variables
 '(winner-mode t)
 '(split-window-keep-point nil)
 '(even-window-sizes nil)
 '(switch-to-buffer-obey-display-actions t))

;; This introduces a fundamental change to how Emacs works. Normally, when
;; Emacs displays a new buffer, it tries to intelligently select the best
;; window for that buffer. Sometimes this is a new window (split), sometimes
;; it is the current window, sometimes it re-uses an existing window. The
;; below code makes it so the default window is always the current one. The
;; advantage of this is that your window layouts are never changed for you.
;; https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts
;; (customize-set-variable 'display-buffer-base-action
;;                         '((display-buffer-reuse-window display-buffer-same-window)
;;                           (reusable-frames . t)))

;; The order of the below items matter. The first one that matches is applied.
;; That's why all these add-to-list items have the APPEND flag.
(add-to-list 'display-buffer-alist
             '("\\*Calendar.*"
               (display-buffer-below-selected)
               (window-parameters . ((no-other-window . nil)))
               (window-height . fit-window-to-buffer))
             t)

(add-to-list 'display-buffer-alist
             '("\\*Org Select\\*"
               (display-buffer-in-side-window)
               (dedicated . t)
               (side . bottom)
               (slot . 0)
               (window-parameters . ((mode-line-format . none))))
             t)

(add-to-list 'display-buffer-alist
             '("\\*\\(.* # Help.*\\|Help\\)\\*"
               (display-buffer-reuse-window display-buffer-same-window)
               (reusable-frames . t))
             t)

(add-to-list 'display-buffer-alist
             '("\\*wclock.*"
               (display-buffer-below-selected)
               (window-height . fit-window-to-buffer)
               (window-parameters . ((select . t))))
             t)

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
    ("h" "Split Horizontal" split-window-below-select)
    ("v" "Split Vertical"   split-window-right-select)
    ("b" "Balance"    balance-windows)
    ("f" "Fit"        fit-window-to-buffer)
    ("r" "Rotate Split" rotate-window-split)
    ("S" "Swap Windows" swap-windows)]
   ["Window"
    ("d" "Dedicate Window" dedicated-mode)
    ("c" "Clone Indirect" clone-indirect-buffer)
    ("t" "Tear Off" tear-off-window)
    ("k" "Kill" delete-window)
    ("K" "Kill Buffer & Window" kill-buffer-and-window)
    ("o" "Kill Others"  delete-other-windows)
    ("m" "Maximize" maximize-window)]
   ["Navigate"
    ("<left>"  "←" windmove-left  :transient t)
    ("<right>" "→" windmove-right :transient t)
    ("<up>"    "↑" windmove-up    :transient t)
    ("<down>"  "↓" windmove-down  :transient t)]
   ["Exit"
    ("q" "Quit" transient-quit-all)]])

(prog1 "windmove"
  (setq windmove-create-window t)
  (let ((map global-map))
    (define-key map (kbd "C-M-<up>") #'windmove-up)
    (define-key map (kbd "C-M-<right>") #'windmove-right)
    (define-key map (kbd "C-M-<down>") #'windmove-down)
    (define-key map (kbd "C-M-<left>") #'windmove-left)
    (define-key map (kbd "C-M-S-<up>") #'windmove-swap-states-up)
    (define-key map (kbd "C-M-S-<right>") #'windmove-swap-states-right)
    (define-key map (kbd "C-M-S-<down>") #'windmove-swap-states-down)
    (define-key map (kbd "C-M-S-<left>") #'windmove-swap-states-left)))


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

(defun toggle-buffer-confirm-kill nil
  "Sets the buffer-local variable `buffer-confirm-kill' to t."
  (interactive)
  (if buffer-confirm-kill
      (progn
        (setq-local buffer-confirm-kill nil)
        (message "'buffer-confirm-kill' set to 'nil'"))
    (progn
        (setq-local buffer-confirm-kill t)
        (message "'buffer-confirm-kill' set to 't'"))))


;;; Scratch Buffers

;; It is sometimes useful to quickly create a scratch buffer in markdown- or
;; org-mode (for editing text you're going to paste into a reddit
;; comment/post, for example).

(defvar scratch-markdown-initial-message "<!-- Scratch Buffer for Markdown Mode -->\n\n"
  "Message to be inserted in markdown scratch buffer.")

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

(defvar scratch-org-initial-message "# Scratch Buffer for Org Mode\n\n"
  "Message to be inserted in org scratch buffer.")

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


;;; Minibuffer

(custom-set-variables
 '(completions-format 'one-column)
 '(completions-detailed t)
 '(completion-show-help nil)
 '(completion-cycle-threshold nil)
 '(enable-recursive-minibuffers t)
 '(savehist-mode t)
 '(minibuffer-eldef-shorten-default t)
 '(minibuffer-depth-indicate-mode t)
 '(file-name-shadow-mode 1)
 '(minibuffer-depth-indicate-mode 1)
 '(minibuffer-electric-default-mode 1))

(elpa-package 'vertico
  (setq vertico-count 15)
  (vertico-mode 1))

(elpa-package 'orderless
  (require 'orderless)
  (custom-set-variables
   '(completion-styles '(orderless))))


;;; Packages

(elpa-package 'corfu
  (corfu-global-mode 1))

(elpa-package 'cape
  (require 'cape)
  (transient-define-prefix cape-transient ()
    "Transient for cape commands."
    [[("/" "capf" completion-at-point) ;; capf
      ("d" "dabbrev" dabbrev-completion)  ;; dabbrev
      ("t" "etags" complete-tag)        ;; etags
      ("a" "abbrev" cape-abbrev)]
     [("f" "file" cape-file)
      ("k" "keyword" cape-keyword)
      ("o" "symbol" cape-symbol)
      ("i" "ispell" cape-ispell)
      ;;("w" "dictionary" cape-dict)
      ]])
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line)
  (define-key bosskey-mode-map (kbd "C-M-/") 'cape-transient))

(local-package "vundo"
  ;; Vundo creates a tree-like visualization of your undo history
  ;; using only standard Emacs undo commands and data. Requires either
  ;; Emacs 28 or its backported undo functions.
  (require 'vundo))

(elpa-package 'exec-path-from-shell
  (exec-path-from-shell-initialize))

(elpa-package 'embark
  (custom-set-variables
   '(prefix-help-command 'embark-prefix-help-command)
   '(embark-indicators '(embark-mixed-indicator
                         embark-highlight-indicator
                         embark-isearch-highlight-indicator))
   '(embark-mixed-indicator-delay 1)
   '(embark-verbose-indicator-display-action
     '(display-buffer-at-bottom (window-height . fit-window-to-buffer))))

  (custom-set-faces
   '(embark-verbose-indicator-title ((t (:inherit 'modus-themes-heading-1)))))

  (with-eval-after-load 'embark
    (let ((map embark-file-map))
      (define-key map (kbd "O") 'crux-open-with)
      (define-key map (kbd "j") 'dired-jump))))

(elpa-package 'consult
  (with-eval-after-load 'consult
    (consult-customize consult-buffer consult-buffer-other-window :preview-key nil))
  (custom-set-variables
   '(consult-find-command "fd --color=never --full-path ARG OPTS"))
  (global-set-key [remap imenu] 'consult-imenu)
  (global-set-key [remap yank-pop] 'consult-yank-pop)
  (global-set-key [remap repeat-complex-command] #'consult-complex-command))

(elpa-package 'visual-regexp
  ;; A reasonable regex engine? Live preview of search and replacement? Yes please!
  (define-key global-map [remap query-replace] 'vr/query-replace)
  (with-eval-after-load 'visual-regexp
    (with-eval-after-load 'visual-regexp-steroids
      (custom-set-variables
       '(vr/engine 'pcre2el)))))

(elpa-package 'delight
  (with-eval-after-load 'flyspell (delight 'flyspell-mode " Spell" "flyspell"))
  (delight 'outline-minor-mode " Out" "outline")
  (delight 'eldoc-mode nil "eldoc")
  (delight 'emacs-lisp-mode "Elisp" "elisp-mode")
  (delight 'auto-fill-function " Fill" "simple"))

(elpa-package 'fountain-mode
  (custom-set-variables
   '(fountain-add-continued-dialog nil)
   '(fountain-highlight-elements (quote (section-heading)))))

(elpa-package 'markdown-mode
  ;; It seems everyone wants me to use the extension 'mdown' for
  ;; markdown documents, which for some reason I hate. I have no idea
  ;; why. I prefer 'text'. I probably got the idea here:
  ;; https://daringfireball.net/linked/2014/01/08/markdown-extension
  (add-to-list 'auto-mode-alist
               '("\\.text" . markdown-mode)))

(elpa-package 'olivetti
  (custom-set-variables
   '(olivetti-body-width 100)))

(local-package "oblique-strategies"
  (autoload 'oblique-strategy "oblique")
  (setq initial-scratch-message (concat
                                 ";; Welcome to Emacs!\n;; This is the scratch buffer, for unsaved text and Lisp evaluation.\n"
                                 ";; Oblique Strategy: " (oblique-strategy) "\n\n")))


;;; Libraries

(prog1 "spelling"
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "/usr/local/bin/aspell"))

(with-eval-after-load 'dired
  (setq dired-use-ls-dired nil)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (let ((map dired-mode-map))
    (define-key map (kbd "O") 'crux-open-with)
    (define-key map (kbd "C-/") 'dired-undo)))

(prog1 "prog-mode"
  (defun prog-mode-hook-config nil
    (setq-local show-trailing-whitespace t)
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode))
  (add-hook 'prog-mode-hook 'prog-mode-hook-config))

(prog1 "outline"
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (setq outline-minor-mode-cycle t)
  (with-eval-after-load 'outline
    (define-key outline-minor-mode-map (kbd "<backtab>") 'outline-cycle-buffer)))

(prog1 "calendar"
  (require 'solar)
  (setq calendar-latitude 34.157
        calendar-longitude -118.324))


(prog1 "world-clock"
  (setq world-clock-time-format "%Z%t%R%t%F"
        world-clock-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Chicago" "Chicago")
          ("America/Montreal" "Montreal")
          ("Europe/London" "London"))))

(prog1 "isearch"
  (setq isearch-lazy-count t)
  (setq isearch-repeat-on-direction-change t)
  (defun isearch-exit-at-start ()
    "Exit search at the beginning of the current match."
    (when (and isearch-forward
               (number-or-marker-p isearch-other-end)
               (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end)))
  (add-hook 'isearch-mode-end-hook 'isearch-exit-at-start))

(prog1 "ibuffer"
  (setq ibuffer-display-summary nil))


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

(defun org-toggle-checkbox-presence ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-toggle-checkbox)))

(autoload 'org-export-dispatch "org")
(autoload 'org-store-link "org")

(with-eval-after-load 'org
  (custom-set-variables
   '(org-list-allow-alphabetical t)
   '(org-log-done 'time)
   '(org-log-into-drawer t)
   '(org-special-ctrl-a/e t)
   '(org-special-ctrl-k t)
   '(org-return-follows-link t)
   '(org-catch-invisible-edits 'show-and-error)
   '(org-outline-path-complete-in-steps nil)
   '(org-refile-use-outline-path 'file)
   '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
   '(org-startup-with-inline-images t)
   '(org-image-actual-width '(600))
   '(org-hide-emphasis-markers t)
   '(org-hide-leading-stars nil)
   '(org-adapt-indentation t)
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
   '(org-agenda-start-with-log-mode nil)
   '(org-agenda-log-mode-items '(closed clock state))
   '(org-agenda-use-time-grid nil)
   '(org-deadline-warning-days 7)
   '(org-agenda-todo-ignore-scheduled nil)
   '(org-agenda-todo-ignore-deadlines nil)
   '(org-agenda-skip-deadline-if-done t)
   '(org-agenda-skip-scheduled-if-done t)
   '(org-agenda-skip-deadline-prewarning-if-scheduled t))

  (setq org-agenda-files (list org-directory))

  (when (string= system-name "shadowfax.local")
    (add-to-list 'org-agenda-files "~/home/writing/kindred/compendium.org"))

  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))

  (setq org-todo-keywords
        '((sequence "TODO" "DELG" "LATER" "|" "DONE" "MOVED" "CANCELED")))

  (setq org-todo-keyword-faces
        '(("DELG" . org-scheduled-previously)
          ("LATER" . org-scheduled-previously)))

  (when (string= system-name "shadowfax.local")
    (setq org-capture-templates
          `(("i" "Personal Inbox" entry
             (file+headline ,(concat org-directory "life.org") "Inbox")
             "* %?\n\n" :empty-lines 1)
            ("l" "Personal Log Entry" entry
             (file+olp+datetree ,(concat org-directory "logbook.org"))
             "* %?\n%T\n\n" :empty-lines 1 :tree-type month )
            ("e" "Emacs Config" entry
             (file+headline ,(concat org-directory "emacs.org") "Emacs Config")
             "* TODO %?" :empty-lines 1))))

  (defun my/org-refile-preserve-collapsed-parent ()
    (org-up-heading-safe)
    (when (save-excursion
            (end-of-line)
            (org-invisible-p))
      (outline-hide-subtree)))

  (add-hook 'org-after-refile-insert-hook #'my/org-refile-preserve-collapsed-parent)

  ) ; End Org


;;;; Org Agenda

(with-eval-after-load 'org-agenda

  (add-hook 'org-agenda-mode-hook 'hl-line-mode)

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

  (let ((map org-agenda-mode-map))
    (define-key map (kbd "k") 'org-capture)
    (define-key map (kbd "K") 'org-agenda-capture)
    (define-key map (kbd "S") 'org-agenda-schedule)
    (define-key map (kbd "D") 'org-agenda-deadline)
    (define-key map (kbd "C-/") 'org-agenda-undo))

  ) ; End Org Agenda


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
;; display its various pop-up windows (though, to be fair, it is good about
;; restoring them). These two transients help me retain a small dose of my
;; sanity. I really wish this wasn't necessary. Also see:
;; https://emacs.stackexchange.com/a/14818

(transient-define-prefix org-todo-transient ()
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


(transient-define-prefix org-agenda-todo-transient ()
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


;;; Transients

(defun universal-transient ()
  "If ARG is nil, display the `general-transient', otherwise `call-mode-help-transient'."
  (interactive)
  (if (equal current-prefix-arg nil)
      (general-transient)
    (call-mode-help-transient)))

(transient-define-prefix general-transient ()
  "General-purpose transient."
  [["Actions/Toggles"
    ("o f" "Other Frame Prefix..." other-frame-prefix)
    ("o w" "Other Window Prefix..." other-window-prefix)
    ("," "Find Init" find-user-init-file)
    ("a" "AutoFill" auto-fill-mode)
    ("j" "Dired Jump" dired-jump)
    ("l" "List Buffers" bs-show)
    ("k" "Kill Buffer" kill-buffer-dwim)
    ("n" "Make Frame" make-frame)
    ("w" "Windows..." window-transient)]
   ["Org"
    ("o a" "Org Agenda" org-agenda)
    ("o k" "Org Capture" org-capture)
    ("o s" "Store Link" org-store-link)
    ("o i" "Insert Link" org-insert-link)
    "Consult"
    ("c b" "Buffer" consult-buffer)
    ("c B" "Buffer Other Window" consult-buffer-other-window)
    ("c l" "Line" consult-line)
    ("c o" "Outline" consult-outline)
    ("c g" "Grep" consult-grep)]
   ["Other"
    ("f" "Set Fonts" set-custom-fonts)
    ("t" "Toggle Modus" modus-themes-toggle :transient t)
    ("T" "Toggle macOS Apperance" macos-toggle-system-appearance :transient t)
    ("d" "Date/Time mode-line" toggle-date-time-battery)
    ("C" "Calendar" calendar)
    ("W" "World Clock" world-clock)
    ("x o" "*scratch-org*" scratch-buffer-org)
    ("x m" "*scratch-markdown*" scratch-buffer-markdown)]
   ["Macros"
    ("m s" "Start" start-kbd-macro)
    ("m e" "End" end-kbd-macro)
    ("m c" "Call" call-last-kbd-macro)
    ("m r" "Region Lines" apply-macro-to-region-lines)]])


;; Emacs has so many modes. Who can remember all the commands? These
;; mode-specific transients are designed to help with that.

(defun call-mode-help-transient ()
  "Call a helpful transient based on the mode you're in."
  (interactive)
  (unless
      (cond ((derived-mode-p 'org-mode)
             (org-mode-help-transient))
            ((derived-mode-p 'Info-mode)
             (info-mode-help-transient)))
    (message "No transient defined for this mode.")))

(with-eval-after-load 'org
  (transient-define-prefix org-mode-help-transient ()
    "Transient for Org Mode"
    ["Org Mode"
     ["Navigation"
      ("o" "Outline" consult-org-heading)
      ("f" "Find Heading" consult-org-agenda)
      ("c" "Go To Calendar" org-goto-calendar)
      ("v" "Visible Markup" visible-mode)]
     ["Item"
      ("t" "TODO State" org-todo-transient)
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


;;; End of init.el
