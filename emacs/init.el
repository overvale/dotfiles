;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Oliver Taylor
;; Learning Emacs was my COVID pandemic project, now I cannot escape.

;; Author: Oliver Taylor
;; URL: https://olivertaylor.net
;; URL: https://github.com/olivertaylor/dotfiles

;; This config targets Emacs 28.1

;;; Commentary:

;; This file has an outline which can be viewed by looking at comments
;; starting with three or more semicolons. `outline-minor-mode' supports this
;; convention by default and helps with navigation. You can also create an
;; occur buffer with the search /^;;;+/.

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


;;; Startup

;; I don't think of myself as fanatical about startup time, but I do try to
;; keep it below 0.5 seconds (as reported by `emacs-init-time'). That's
;; roughly double the speed that Emacs starts up with no init file. Ultimately,
;; I suspect that the 3 biggest factors (in this init file anyway) are the
;; number of installed packages, the number of lines of code (I try to keep it
;; under 2,000), and the theme.

;; Part 2 of 2 (part 1 is in early-init). This is stolen from here:
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; In this config, this 2-part dance saves about 0.3 seconds on startup time.
;; Is it really worth it? Probably not.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;; I can never seem to actually suppress the Emacs startup echo area message,
;; despite following the documentation carefully, so just redefine the
;; function instead.
(defun display-startup-echo-area-message ()
  (message "Welcome to Emacs!"))


;;; Settings

;; Save all interactive customization to a temp file, which is never loaded.
;; This means interactive customization is session-local. Only this init file persists sessions.
(setq custom-file (make-temp-file "emacs-custom-"))

;; For most of my "settings" I use `custom-set-variables', which does a bunch of neat stuff:
;;
;; 1. It calls a variable's "setter" function, if it has one. A perfect
;;    example of why this is needed is `truncate-lines', which simply will not
;;    work unless set this way.
;;
;; 2. It can activate modes as well as set variables, which makes it tidy.
;;
;; 3. It takes care of setting the default for buffer-local variables correctly.
;;
;; https://with-emacs.com/posts/tutorials/almost-all-you-need-to-know-about-variables/#_user_options
;; https://old.reddit.com/r/emacs/comments/exnxha/withemacs_almost_all_you_need_to_know_about/fgadihl/

(custom-set-variables
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(ring-bell-function 'ignore)
 '(echo-keystrokes 0.5)
 '(pixel-scroll-precision-mode 1)
 '(scroll-step 1)
 '(scroll-margin 1)
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
 '(truncate-lines t)
 '(save-interprogram-paste-before-kill t)
 '(kill-do-not-save-duplicates t)
 '(kill-ring-max 512)
 '(sentence-end-double-space nil)
 '(tab-always-indent 'complete)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(fill-column 78)
 '(delete-by-moving-to-trash t)
 '(trash-directory "~/.Trash"))

;; You can delay activating these modes a moment to save a very tiny bit of
;; startup time.

(defun after-init-hook-setup nil
  (global-auto-revert-mode 1)
  (repeat-mode 1)
  (save-place-mode 1)
  (recentf-mode 1))

(add-hook 'after-init-hook 'after-init-hook-setup)


;;; Critical Setup

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-archive-priorities '(("gnu" . 30)
                                   ("nongnu" . 25)
                                   ("melpa-stable" . 20)
                                   ("melpa" . 10)))

(setq package-selected-packages
      '(consult
        delight
        elfeed
        embark
        embark-consult
        expand-region
        fountain-mode
        hide-mode-line
        lua-mode
        magit
        markdown-mode
        marginalia
        no-littering
        olivetti
        orderless
        org
        vertico
        visible-mark
        visual-regexp
        visual-regexp-steroids
        wolfram))

;; So... Like any Emacs hacker who has spent entirely too much time thinking
;; about their config I've created my own version of `use-package'. Compared
;; to it my version is laughably incomplete, but it does only a few very
;; simple things, all of which I understand well, which is not something I can
;; say for `use-package'. It is a modified version of this:
;; https://github.com/casouri/lunarymacs/blob/master/site-lisp/luna-load-package.el

(defvar local-package-dir nil
  "Directory containing lisp files which are not in dotfiles or an ELPA package.")

(defun luna-split-command-args (args)
  "Split args into commands and args.
If ARGS is (:command args args args :command args),
return: ((:command . (args args args)) (:command . (args)))."
  (let (ret-list arg-list command)
    (dolist (token (append args '(:finish)))
      (if (keywordp token)
          ;; Finish previous command
          (progn (if command (push (cons command (reverse arg-list))
                                   ret-list))
                 (setq arg-list nil)
                 ;; Start new command
                 (setq command token))
        (push token arg-list)))
    (reverse ret-list)))

(defmacro load-package (package &rest args)
  "Load PACKAGE according to ARGS, similar to `use-package'.
Available keywords are (must use one):

  :local-dir     Name of package directory as string. This string
                 is appended to `local-package-dir' and added to
                 `load-path'.
  :eval          Code to be evaluated without conditions.
  :after-load    Code to be evaluated after the package is loaded
                 using `with-eval-after-load'.
  :autoload      Add autoloads for each command, listed literally."
  (declare (indent 1))
  (let* ((arg-list (luna-split-command-args args))
         (body
          (mapcan
           (lambda (arg)
             (let ((command (car arg))
                   (arg-list (cdr arg)))
               (pcase command
                 (:require `((require ,package)))
                 (:local-dir `((add-to-list 'load-path
                                            (concat local-package-dir
                                                    ,@arg-list))))
                 (:after-load `((with-eval-after-load ,package
                                  ,@arg-list)))
                 (:autoload
                  (mapcar (lambda (cmd)
                            `(autoload ',cmd (symbol-name ,package) nil t))
                          arg-list))
                 (:eval arg-list))))
           arg-list)))
    `(progn ,@body)))

;; Let's be honest, it's not hard to bind a key in Emacs, and the syntax for
;; it is... fine. But it can be easier, and you can save yourself some typing
;; by creating a function or macro to do it for you. There are many like it
;; but this one is mine:

(defun defkey (map &rest body)
  "Define a key for MAP with specs in BODY.
A custom wrapper around `define-key' that does 2 things:
1. All string defs of a key are wrapped in `kbd'.
2. You can define multiple keys in the style of `setq'.

Define your keys like this:

    (defkey global-map
      \"s-1\" 'switch-to-buffer
      [f1]  pkg-ops-map
      \"s-2\" 'find-file)"
  ;; https://emacs.stackexchange.com/a/58058
  (while body
    (let ((key (car body))
          (def (cadr body)))
      (define-key
        map
        (if (stringp key) (kbd key) key)
        def)
      (setq body (cddr body)))))

;; The key binding technique below is taken from the bind-key package. It
;; places all the bindings I don't want overridden into a minor mode which is
;; inserted into the `emulation-mode-map-alists', so only very few things can
;; override them.

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

;; I use a lot of transients in this config, so I need to make sure it is
;; loaded and configured before those are declared below.

(load-package 'transient
  :autoload transient-define-prefix
  :after-load
  (setq transient-detect-key-conflicts t
        transient-show-popup t))

;; In addition to the above packages and macros, a few variables need to be
;; set.

(defvar user-home-dir "~/home/")
(defvar user-downloads-directory "~/Desktop/")

(setq local-package-dir (concat user-home-dir "opt/"))
(setq-default default-directory user-home-dir)
(setq org-directory (concat user-home-dir "org/"))

(load (concat user-home-dir "src/lisp/private.el"))

(add-to-list 'load-path (concat user-home-dir "dot/emacs/lisp/"))

(add-to-list 'exec-path "/usr/local/bin/") ; homebrew
(add-to-list 'exec-path (concat user-home-dir "dot/bin"))

(load-package 'no-littering
  :require
  :eval
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


;;; Functions

;; Some of these functions I wrote myself, many of them I copied (and perhaps
;; modified) from other people's configs.

(defun find-user-init-file ()
  "Find the user-init-file."
  (interactive)
  (find-file user-init-file))

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

(defun browse-url-macos-background (url)
  "Open URL with macOS `open'."
  (interactive)
  (start-process "open url"
                 nil "open" "--background" url)
  (message "URL opened in background."))

(defun macos-open-app (app)
  "Open APP with macOS `open'."
  (start-process "open app" nil "open" "-a" app))

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

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (--when-let (thing-at-point 'symbol)
            (regexp-quote it)))
        regexp-history)
  (call-interactively 'occur))

(defun wrap-region-in-xml-tag (start end arg)
  "Wrap the region in an xml tag of ARG."
  (interactive "r\nsTagname: ")
  (goto-char start)
  (insert "<" arg ">")
  (goto-char (+ end 2 (length arg)))
  (insert "</" arg ">"))

(defun wrap-http-link-backwards ()
  "Looks backwards from point for a link and wraps in HTML tag."
  (interactive)
  (let ((start (copy-marker (point))))
    (re-search-backward "\\(^\\|\\s-+\\)https?://")
    (forward-word 1)
    (forward-word -1)
    (insert "<a href=\"")
    (search-forward " ")
    (just-one-space)
    (forward-char -1)
    (insert "\"")
    (forward-char 1)
    (when (looking-at "$")
      (delete-char -1)
      (forward-char 1))
    (insert ">")
    (goto-char start)
    (insert "</a>")))

(defun safari-read-later (url)
  "Add URL to Safari's Reading List."
  (interactive
   (list (completing-read "URL to save: " nil)))
  (shell-command-to-string
   (concat "osascript -e \'tell application \"Safari\" to add reading list item \""
           url "\"\'"))
  (message "URL sent to Safari's Reading List."))


;;; Advice

;; The below allows for block-undo after running keyboard macros.
;; From u/oantolin.
(defun block-undo (fn &rest args)
  (let ((marker (prepare-change-group)))
    (unwind-protect (apply fn args)
      (undo-amalgamate-change-group marker))))

(dolist (fn '(kmacro-call-macro
              kmacro-exec-ring-item
              dot-mode-execute
              apply-macro-to-region-lines))
  (advice-add fn :around #'block-undo))

(advice-add 'indent-rigidly-left-to-tab-stop :after 'activate-mark)
(advice-add 'indent-rigidly-right-to-tab-stop :after 'activate-mark)


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

(load-package 'vertico
  :local-dir "vertico"
  :require
  :eval (vertico-mode 1))

(load-package 'vertico-extensions
  :local-dir "vertico/extensions"
  :eval
  (require 'vertico-buffer)
  (require 'vertico-directory)
  (require 'vertico-flat)
  (require 'vertico-grid)
  (require 'vertico-indexed)
  (require 'vertico-mouse)
  (require 'vertico-multiform)
  (require 'vertico-quick)
  (require 'vertico-repeat)
  (require 'vertico-reverse)
  (require 'vertico-unobtrusive)

  (vertico-multiform-mode 1)

  (setq vertico-multiform-categories
        '((file reverse)
          (consult-grep buffer)))

  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (consult-buffer unobtrusive)
          (consult-line buffer)
          (execute-extended-command unobtrusive)
          (completion-at-point reverse)
          (describe-symbol buffer)))

  (defkey vertico-map
    "M-g" #'vertico-multiform-grid
    "M-f" #'vertico-multiform-flat
    "M-r" #'vertico-multiform-reverse
    "M-u" #'vertico-multiform-unobtrusive)

  ;; vertico-directory
  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ) ; end vertico/extensions

(load-package 'orderless
  :require
  :eval
  (custom-set-variables
   '(completion-styles '(orderless))))

;; The below code introduces some improvements to the default completion
;; experience. I took these ideas from the emacs-devel mailing list.

(defun switch-to-completions-bottom ()
  "Switch to the *Completions* buffer, at the bottom."
  (interactive)
  (switch-to-completions)
  (move-to-window-line -1))

(defun completion-quit ()
  "Close the completion buffer and return to the minibuffer."
  (interactive)
  (quit-window)
  (switch-to-minibuffer))

(defun completion-kill-buffer ()
  "Close the completion buffer and return to the minibuffer."
  (interactive)
  (let ((win (get-buffer-window "*Completions*")))
    (when win (quit-window t win)))
  (switch-to-minibuffer))

(let ((minibuffer minibuffer-local-completion-map)
      (list completion-list-mode-map))
  (defkey minibuffer
    "C-n" 'switch-to-completions
    "C-p" 'switch-to-completions-bottom)
  (defkey list
    [remap other-window] 'switch-to-minibuffer
    "z" #'completion-kill-buffer
    [remap keyboard-quit] #'delete-completion-window
    [remap quit-window] #'completion-quit
    "n" 'next-completion
    "p" 'previous-completion))


;;; Bindings

(defkey key-translation-map "ESC" (kbd "C-g"))

;; I'm trying to unlearn these and use the `general-transient' and mac-like
;; bindings instead.
(defkey bosskey-mode-map
  "C-x C-f" 'undefined
  "C-x f"   'undefined
  "C-x C-b" 'undefined
  "C-x b"   'undefined
  "C-x k"   'undefined)

;; Because they're in the `bosskey-mode-map' these bindings won't be
;; overridden by minor modes and the like.
(defkey bosskey-mode-map
  "<C-return>" 'universal-transient
  "C-." 'embark-act
  "s-b" 'consult-buffer
  "s-f" 'consult-line
  "s-o" 'find-file
  "s-j" 'dired-jump
  "s-w" 'window-transient
  "M-/" 'completion-at-point
  "M-\\" 'cycle-spacing
  "M-z" 'zap-up-to-char
  "M-i" 'imenu
  "C-d" 'delete-forward-char
  "M-o" 'other-window
  "M-O" 'other-window-previous
  "C-M-O" 'other-window-prefix)

(with-eval-after-load 'magit
  ;; Magit overrides `bosskey-mode-map', so I need to override this here:
  (defkey magit-file-section-map "<C-return>" 'universal-transient))

;; For bindings that I do want to be overriden by minor/major modes, I use the
;; built-in `global-map'.
(defkey global-map
  ;; replace mappings
  [remap capitalize-word] 'capitalize-dwim
  [remap downcase-word]   'downcase-dwim
  [remap upcase-word]     'upcase-dwim
  ;; Make shift-click extend the region.
  [S-down-mouse-1] 'ignore
  [S-mouse-1] 'mouse-save-then-kill
  ;; Use M-drag-mouse-1 to create rectangle regions.
  [M-down-mouse-1] #'mouse-drag-region-rectangle ; down
  [M-drag-mouse-1] #'ignore                      ; drag
  [M-mouse-1]      #'mouse-set-point)            ; up

(defkey help-map "s" 'describe-symbol-at-point)

;; Package Operations

(define-prefix-command 'pkg-ops-map nil "Packages")

(defkey pkg-ops-map
  "h" '("describe" . describe-package)
  "a" '("autoremove" . package-autoremove)
  "d" '("delete" . package-delete)
  "i" '("install" . package-install)
  "s" '("selected" . package-install-selected-packages)
  "r" '("refresh" . package-refresh-contents)
  "l" '("list" . list-packages))

(defkey global-map"C-c p" 'pkg-ops-map)


;; MacOS-like bindings

(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

(defkey global-map
  "s-z" 'undo-only
  "s-Z" 'undo-redo
  "s-x" 'kill-region
  "s-c" 'kill-ring-save
  "s-v" 'yank
  "s-<backspace>" 'backward-kill-line
  "s-[" 'previous-buffer
  "s-]" 'next-buffer
  "s-{" 'indent-rigidly-left-to-tab-stop
  "s-}" 'indent-rigidly-right-to-tab-stop
  "s-/" 'comment-line
  "s-<up>" 'beginning-of-buffer
  "s-<down>" 'end-of-buffer
  "s-s" 'save-buffer
  "M-s-s" 'save-some-buffers
  "s-m" 'iconify-frame
  "s-," 'find-user-init-file
  "s-q" 'save-buffers-kill-emacs
  "s-." 'keyboard-quit)

;; I spend a lot of time in the terminal, and I spent many years working in
;; vim, both of which use readline bindings. I miss them when I don't have
;; them.
(defkey bosskey-mode-map
  "s-u" 'universal-argument
  "C-u" 'backward-kill-line
  "C-w" 'backward-kill-word
  "s-x" 'kill-region)


;;; Themes

;; I will admit that the below is... overkill. But what it does is (at least)
;; conceptually simple. It allows you to define your preferred dark and light
;; themes, and then provides functions for loading them, toggling them, and
;; matching to MacOS's system appearance.

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
  (if custom-enabled-themes
    (mapcar 'disable-theme custom-enabled-themes)))

(defun load-theme-cleanly (theme)
  "Disable active themes, then load theme."
  (interactive
   (list (intern
          (completing-read "Load Theme: "
                           (mapcar 'symbol-name (custom-available-themes))))))
  (disable-current-themes)
  (load-theme theme t))

(defun macos-dark-p ()
  "Return t if macOS appearance is dark."
  (interactive)
  (string= (shell-command-to-string "defaults read -g AppleInterfaceStyle")
           "Dark\n"))

(defun macos-toggle-system-appearance nil
  "Toggle macOS's system appearance between dark and light modes."
  (interactive)
  (shell-command-to-string "osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'"))

(defun load-theme-dark nil
  "Loads the dark theme."
  (disable-current-themes)
  (load-theme dark-theme t)
  (setq current-theme-color 'dark))

(defun load-theme-light nil
  "Loads the light theme."
  (disable-current-themes)
  (load-theme light-theme t)
  (setq current-theme-color 'light))

(defun toggle-theme-color nil
  "Toggle between `light-theme' and `dark-theme'."
  (interactive)
  (if (eq current-theme-color 'light)
      (load-theme-dark)
    (load-theme-light)))

(defun load-theme-color (color)
  "Load users preferred theme, based on ARG or macOS appearance.
Disables all current themes, then:
- if COLOR is \"light\" or \"dark\", load the `light-theme' or `dark-theme'.
- if COLOR is \"system\" check macOS's appearance state and match it with
  either the light or dark theme."
  (interactive
   (list (completing-read "Load Theme Color: " '("dark" "light" "system"))))
  (cond ((string= color "light")
         (load-theme-light))
        ((string= color "dark")
         (load-theme-dark))
        ((string= color "system")
         (if (macos-dark-p)
             (load-theme-dark)
           (load-theme-light)))))

;; Modus Themes options...

;; Use the local version instead of the built-in one
(load-package 'modus-themes
  :local-dir "modus-themes")

(custom-set-variables
 '(modus-themes-mixed-fonts t)
 '(modus-themes-italic-constructs t)
 '(modus-themes-links '(neutral-underline))
 '(modus-themes-diffs '(desaturated))
 '(modus-themes-org-agenda '((header-block . (variable-pitch 1.6))
                             (header-date . (bold-today))
                             (scheduled . rainbow))))

(custom-set-variables
 '(modus-themes-vivendi-color-overrides '((bg-main     . "#24242d")
                                          (bg-inactive . "#2f2f3b")
                                          (bg-hl-line  . "#2f2f3b"))))

;; I want different syntax options for operandi and vivendi and there doesn't
;; seem to be a built in way to do that. So I've created some custom
;; functions/advice for that. NOTE that any options you set in one of the
;; themes need to be reset to nil in the other theme.

(defun customize-modus-vivendi nil
  (custom-set-variables
   '(modus-themes-mode-line '(borderless accented))
   '(modus-themes-syntax '(yellow-comments faint))))

(defun customize-modus-operandi nil
  (custom-set-variables
   '(modus-themes-mode-line '(3d))
   '(modus-themes-syntax nil)))

(advice-add 'load-theme-dark :before 'customize-modus-vivendi)
(advice-add 'load-theme-light :before 'customize-modus-operandi)

;; Actual theme loading...

(setq light-theme 'modus-operandi)
(setq dark-theme  'modus-vivendi)
(setq default-theme-color 'light)

;; On startup I want to match the system
(load-theme-color 'system)

(custom-set-variables
 '(cursor-type 'box)
 '(cursor-in-non-selected-windows 'hollow)
 '(blink-cursor-blinks 10)
 '(blink-cursor-interval 0.35)
 '(blink-cursor-delay 0.5))


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
                                                        variable-pitch-adjust-height))))
    (progn
      (face-remap-remove-relative variable-pitch-remapping)
      (face-remap-remove-relative fixed-pitch-remapping))))

(add-hook 'buffer-face-mode-hook (lambda () (variable-pitch-adjust-mode 'toggle)))

(defvar-local buffer-remap-faces-default-cookie nil
  "Cookie used for `buffer-remap-faces--set'.")
(defvar-local buffer-remap-faces-fixed-pitch-cookie nil
  "Cookie used for `buffer-remap-faces--set'.")
(defvar-local buffer-remap-faces-variable-pitch-cookie nil
  "Cookie used for `buffer-remap-faces--set'.")

(defun buffer-remap-faces--clear nil
  "In the current buffer, remove all buffer-remap-faces--set cookies."
  (interactive)
  (face-remap-remove-relative buffer-remap-faces-default-cookie)
  (face-remap-remove-relative buffer-remap-faces-fixed-pitch-cookie)
  (face-remap-remove-relative buffer-remap-faces-variable-pitch-cookie))

(defun buffer-remap-faces--set (font-settings)
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
    (setq buffer-remap-faces-default-cookie
          (face-remap-add-relative 'default
                                   :family mono
                                   :height mono-height))
    (setq buffer-remap-faces-variable-pitch-cookie
          (face-remap-add-relative 'fixed-pitch
                                   :family mono))
    (setq buffer-remap-faces-fixed-pitch-cookie
          (face-remap-add-relative 'variable-pitch
                                   :family vari))
    (force-window-update (current-buffer))))

(define-minor-mode buffer-remap-faces
  "Minor mode to set buffer-local fonts."
  :lighter " BufferFaces"
  :init-value nil
  :global nil
  (if buffer-remap-faces
      (call-interactively 'buffer-remap-faces--set)
    (buffer-remap-faces--clear)))

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
        (Hack . ( :mono "Hack"
                  :vari "Inter"
                  :mode "SF Compact Text"
                  :line nil
                  :mono-height 120
                  :mode-height 140
                  :vari-height 130))
        (IBM . ( :mono "IBM Plex Mono"
                 :vari "IBM Plex Serif"
                 :mode "IBM Plex Sans"
                 :line nil
                 :mono-height 120
                 :mode-height 140
                 :vari-height 130))
        (iAWriter . ( :mono "iA Writer Duospace"
                      :vari "iA Writer Duospace"
                      :mode "IBM Plex Sans"
                      :line nil
                      :mono-height 120
                      :mode-height 120
                      :vari-height 120))
        (Go . ( :mono "Go Mono"
                :vari "Go"
                :mode "Go"
                :line nil
                :mono-height 120
                :mode-height 130
                :vari-height 130))
        (Pragmata . ( :mono "PragmataPro"
                      :vari "Fira Sans"
                      :mode "SF Compact Text"
                      :line nil
                      :mono-height 130
                      :mode-height 140
                      :vari-height 140))
        (Cascadia . ( :mono "Cascadia Code"
                      :vari "Fira Sans"
                      :mode "Fira Sans"
                      :line nil
                      :mono-height 120
                      :vari-height 130
                      :mode-height 130))))

;; On startup, use this set of fonts:
(set-custom-fonts "Hack")


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

(define-minor-mode buffer-position
  "Minor mode to display position of point in mode-line."
  :init-value nil
  :global nil)

(defun mode-line-buffer-modified-status ()
  "Return a string indicating buffer modification status."
  (if (and (buffer-modified-p)
           (buffer-file-name))
      (propertize "  -MD-"
                  'help-echo "Buffer is modified.")))

(defun mode-line-buffer-read-only-status ()
  "Return a string indicating buffer read-only status."
  (if buffer-read-only
      (propertize "  -RO-"
                  'help-echo "Buffer is read-only!")))

(defun mode-line-buffer-confirm-kill-status ()
  "Return a string indicating `buffer-confirm-kill' status."
  (if (and buffer-confirm-kill
           (buffer-modified-p))
      (propertize "  -CK-"
                  'help-echo "You must confirm killing this buffer.")))

(defun mode-line-buffer-line-spacing-status ()
  "Return a string indicating the buffer's `line-spacing' value."
  (if line-spacing
      (propertize
       (concat "  LS+" (number-to-string line-spacing))
       'help-echo "Buffer's line-spacing has been modified.")))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here. Check the docstrings for more
;; information.
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(setq-default mode-line-format
              '((:eval (mode-line-buffer-modified-status))
                "   " mode-line-buffer-identification
                (:eval (mode-line-buffer-read-only-status))
                (:eval (mode-line-buffer-confirm-kill-status))
                (:eval (if (eq buffer-position t) mode-line-position " "))
                "   " mode-line-modes
                (:eval (mode-line-buffer-line-spacing-status))
                " " (vc-mode vc-mode)
                mode-line-misc-info))


;;; Buffer Line-Spacing

;; In a manner similar to `text-scale-adjust'.

(defun set-buffer-line-spacing (&optional spacing)
  "Set the buffer's line-spacing to SPACING.
If SPACING is nil, set line-spacing to nil."
  (interactive "P")
  (setq-local line-spacing (if (integerp spacing)
                               spacing
                             nil)))

(defun buffer-line-spacing-increase ()
  "Increase the buffer's line-spacing by 1."
  (interactive)
  (setq-local line-spacing (+ (or line-spacing 0) 1)))

(defun buffer-line-spacing-decrease ()
  "Decrease the buffer's line-spacing by 1."
  (interactive)
  (if (or (null line-spacing)
          (<= line-spacing 0))
      (message "Line-spacing is nil")
    (setq-local line-spacing (- line-spacing 1))))

(defkey global-map
  "C-c C-=" 'buffer-line-spacing-increase
  "C-c C-+" 'buffer-line-spacing-increase
  "C-c C--" 'buffer-line-spacing-decrease
  "C-c C-0" 'set-buffer-line-spacing)

(defvar line-spacing-repeat-map
  (let ((map (make-sparse-keymap)))
    (defkey map
      "=" 'buffer-line-spacing-increase
      "+" 'buffer-line-spacing-increase
      "-" 'buffer-line-spacing-decrease
      "0" 'set-buffer-line-spacing
      "C-=" 'buffer-line-spacing-increase
      "C-+" 'buffer-line-spacing-increase
      "C--" 'buffer-line-spacing-decrease
      "C-0" 'set-buffer-line-spacing)
    map)
  "Keymap to repeat buffer line-spacing commands. Used in `repeat-mode'.")
(put 'buffer-line-spacing-increase 'repeat-map 'line-spacing-repeat-map)
(put 'buffer-line-spacing-decrease 'repeat-map 'line-spacing-repeat-map)
(put 'set-buffer-line-spacing 'repeat-map 'line-spacing-repeat-map)


;;; The Mark

(custom-set-variables
 '(set-mark-command-repeat-pop t)
 '(mark-even-if-inactive nil)
 '(delete-selection-mode t)
 '(mark-ring-max 512))

;; https://github.com/oantolin/emacs-config/blob/4eb8a2a4be518f5d528318773062ad9b9296ab56/my-lisp/text-extras.el#L38
(defun mark-line (&optional arg allow-extend)
  "Mark ARG lines starting with the current one. If ARG is negative,
mark -ARG lines ending with the current one.

Interactively (or if ALLOW-EXTEND is non-nil), if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next ARG lines after the ones already marked."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (and transient-mark-mode mark-active)))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (forward-line arg)
         (point)))
    (forward-line arg)
    (unless (= (preceding-char) 10)
      (setq arg (1- arg)))
    (push-mark nil t t)
    (forward-line (- arg))))

(defalias 'mark-sentence 'mark-end-of-sentence)

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

(defun exchange-point-and-mark-dwim ()
  "Respect region active/inactive and swap point and mark.
If a region is active, then leave it activated and swap point and mark.
If no region is active, then just swap point and mark."
  (interactive)
  (if (use-region-p)
      (exchange-point-and-mark)
    (exchange-point-and-mark)
    (deactivate-mark nil)))

(defun activate-the-mark (&optional arg)
  "Interactive wrapper for `activate-mark'."
  (interactive)
  (activate-mark arg))

(defun deactivate-the-mark (&optional arg)
  "Interactive wrapper for `deactivate-mark'."
  (interactive)
  (deactivate-mark arg))

(defun toggle-the-mark nil
  "Toggles active state of mark.
Does not pass arguments to underlying functions."
  (interactive)
  (if mark-active (deactivate-the-mark) (activate-the-mark)))

;; For some reason these two commands don't act like any other marking
;; commands, they don't activate the mark. I suspect that whoever wrote these
;; commands did so before `transient-mark-mode' was a thing.
(advice-add 'mark-beginning-of-buffer :after 'activate-mark)
(advice-add 'mark-end-of-buffer :after 'activate-mark)

;; I find the default mark-setting bindings to be difficult to remember. Who
;; the heck can remember all these esoteric bindings? Much better to make
;; these a simple transient dispatcher and give it a nice binding.
(transient-define-prefix set-mark-transient ()
  "Transient dispatcher for marking commands."
  :transient-suffix 'transient--do-stay
  [["Navigate:"
    ("c" "Consult Mark" consult-mark :transient nil)
    ("," "Back" pop-to-mark-command)
    ("." "Forward" unpop-to-mark-command)]
   ["Set/Extend Mark To..."
    ("b" "Whole Buffer" mark-whole-buffer)
    ("<" "Beginning of Buffer" mark-beginning-of-buffer)
    (">" "End of Buffer" mark-end-of-buffer)]
   [("w" "Word" mark-word)
    ("l" "Line" mark-line)
    ("p" "Paragraph" mark-paragraph)
    ("s" "Sentence" mark-sentence)
    ("P" "Page" mark-page)]
   [("S" "Sexp" mark-sexp)
    ("D" "Defun" mark-defun)]
   [("SPC" "Activate Mark" activate-the-mark)
    ("RET" "Exit" transient-quit-all)]])
(defkey bosskey-mode-map
  "C-z" 'set-mark-transient
  "C-M-h" 'mark-line
  "C-x C-x" 'exchange-point-and-mark-dwim)

(load-package 'visible-mark
  :eval
  (setq visible-mark-faces `(highlight))
  (global-visible-mark-mode 1))


;;; Windows

;; Settings for how Emacs displays windows, splits, etc.

(custom-set-variables
 '(winner-mode t)
 '(split-window-keep-point nil)
 '(even-window-sizes nil)
 '(switch-to-buffer-obey-display-actions t)
 '(help-window-select t))

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

(add-to-list 'display-buffer-alist
             '("\\*Pp Macroexpand Output.*"
               (display-buffer-below-selected)
               (window-height . fit-window-to-buffer)
               (window-parameters . ((select . nil))))
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
    ("m" "Maximize" maximize-window)]])

(load-package 'windmove
  :eval
  (setq windmove-create-window t)
  (defkey global-map
    "S-<up>" #'windmove-up
    "S-<right>" #'windmove-right
    "S-<down>" #'windmove-down
    "S-<left>" #'windmove-left
    "M-S-<up>" #'windmove-swap-states-up
    "M-S-<right>" #'windmove-swap-states-right
    "M-S-<down>" #'windmove-swap-states-down
    "M-S-<left>" #'windmove-swap-states-left))


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


;;; Packages

(load-package 'visual-regexp
  :eval
  (defkey global-map [remap query-replace] 'vr/query-replace)
  :after-load
  (with-eval-after-load 'visual-regexp-steroids
    (custom-set-variables
     '(vr/engine 'pcre2el))))

(load-package 'vundo
  :local-dir "vundo"
  :autoload vundo)

(load-package 'marginalia
  :eval
  (marginalia-mode 1))

(load-package 'embark
  :after-load
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

  (defkey minibuffer-local-map
    "C-," 'embark-become)
  (defkey embark-url-map
    "r" 'safari-read-later)
  (defkey embark-file-map
    "O" 'crux-open-with
    "j" 'dired-jump))

(load-package 'consult
  :after-load
  (consult-customize consult-line
                     consult-outline
                     consult-imenu
                     consult-mark
                     :preview-key 'any
                     consult-buffer
                     consult-buffer-other-window
                     :preview-key nil)

  (custom-set-variables
   '(consult-find-command "fd --color=never --full-path ARG OPTS"))

  :eval
  (defkey global-map
    [remap imenu] 'consult-imenu
    [remap yank-pop] 'consult-yank-pop
    [remap repeat-complex-command] #'consult-complex-command)

  (defvar mac-find-initial-string nil
    "String to pass to `consult-line'.")

  (defun set-mac-find-initial-string ()
    "Sets `consult-initial-string' to the region or symbol at point."
    (interactive)
    (let ((thing
           (if (region-active-p)
               (buffer-substring (region-beginning) (region-end))
             (symbol-name (symbol-at-point)))))
      (setq mac-find-initial-string thing)
      (message (format "%S" thing))))

  (defun consult-line-mac-find-initial nil
    (interactive)
    (consult-line mac-find-initial-string))

  (defkey global-map
    "s-e" 'set-mac-find-initial-string
    "s-g" 'consult-line-mac-find-initial))

(load-package 'lin
  :local-dir "lin"
  :require
  :after-load
  (lin-add-to-many-modes))

(load-package 'delight
  :eval
  (with-eval-after-load 'flyspell (delight 'flyspell-mode " Spell" "flyspell"))
  (delight 'outline-minor-mode " Out" "outline")
  (delight 'eldoc-mode nil "eldoc")
  (delight 'emacs-lisp-mode "Elisp" "elisp-mode")
  (delight 'auto-fill-function " Fill" "simple"))

(load-package 'fountain-mode
  :after-load
  (custom-set-variables
   '(fountain-add-continued-dialog nil)
   '(fountain-highlight-elements (quote (section-heading)))))

(load-package 'markdown-mode
  :eval
  ;; It seems everyone wants me to use the extension 'mdown' for
  ;; markdown documents, which for some reason I hate. I have no idea
  ;; why. I prefer 'text'. I probably got the idea here:
  ;; https://daringfireball.net/linked/2014/01/08/markdown-extension
  (add-to-list 'auto-mode-alist
               '("\\.text" . markdown-mode)))

(load-package 'olivetti
  :after-load
  (custom-set-variables
   '(olivetti-body-width 72)))

;; (load-package 'oblique
;;   :local-dir "oblique-strategies"
;;   :autoload oblique-strategy
;;   :eval
;;   (setq initial-scratch-message (concat
;;                                  ";; Welcome to Emacs!\n;; This is the scratch buffer, for unsaved text and Lisp evaluation.\n"
;;                                  ";; Oblique Strategy: " (oblique-strategy) "\n\n")))

(load-package 'expand-region
  :eval
  (defkey global-map "s-r" 'er/expand-region))

(load-package 'sdcv-mode
  :local-dir "emacs-sdcv"
  :autoload sdcv-search)


;;; Libraries

(load-package 'info
  :after-load
  (defun my-info-copy-current-node-name (arg)
    ;; https://depp.brause.cc/dotemacs/
    "Copy the lispy form of the current node.
With a prefix argument, copy the link to the online manual instead."
    (interactive "P")
    (let* ((manual (file-name-sans-extension
                    (file-name-nondirectory Info-current-file)))
           (node Info-current-node)
           (link (if (not arg)
                     (format "(info \"(%s) %s\")" manual node)
                   ;; NOTE this will only work with emacs-related nodes...
                   (format "https://www.gnu.org/software/emacs/manual/html_node/%s/%s.html"
                           manual (if (string= node "Top")
                                      "index"
                                    (replace-regexp-in-string " " "-" node))))))
      (kill-new link)
      (message link)))

  (defkey Info-mode-map "c" 'my-info-copy-current-node-name))

(load-package 'flyspell
  :eval
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "/usr/local/bin/aspell"))

(load-package 'dired
  :after-load
  (setq dired-use-ls-dired nil)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (defkey dired-mode-map
    "O" 'crux-open-with
    "C-/" 'dired-undo))

(load-package 'prog-mode
  :eval
  (defun prog-mode-hook-config nil
    (setq-local show-trailing-whitespace t)
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode))
  (add-hook 'prog-mode-hook 'prog-mode-hook-config))

(load-package 'outline
  :eval
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (setq outline-minor-mode-cycle t)
  :after-load
  (defkey outline-minor-mode-cycle-map
    "TAB"   nil ;; too often conflicts with tab-complete
    "M-TAB" 'outline-cycle)
  (defkey outline-minor-mode-map
    "<backtab>" 'outline-cycle-buffer))

(load-package 'calendar
  :after-load
  (require 'solar)
  (setq calendar-latitude 34.157
        calendar-longitude -118.324))

(load-package 'time
  :after-load
  (setq world-clock-time-format "%Z%t%R%t%F"
        world-clock-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Chicago" "Chicago")
          ("America/Montreal" "Montreal")
          ("Europe/London" "London"))))

(load-package 'isearch
  :after-load
  (setq isearch-lazy-count t)
  (setq isearch-repeat-on-direction-change t)
  (defun isearch-exit-at-start ()
    "Exit search at the beginning of the current match."
    (when (and isearch-forward
               (number-or-marker-p isearch-other-end)
               (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end)))
  (add-hook 'isearch-mode-end-hook 'isearch-exit-at-start))

(load-package 'ibuffer
  :after-load
  (setq ibuffer-display-summary nil))

(load-package 'remember
  :eval
  (custom-set-variables
   '(remember-data-file (concat org-directory "remember-notes"))
   '(remember-notes-initial-major-mode 'fundamental-mode)
   '(remember-notes-auto-save-visited-file-name t))
  (defun remember-dwim ()
    "If the region is active, capture with region, otherwise just capture."
    (interactive)
    (if (use-region-p)
        (let ((current-prefix-arg 4)) (call-interactively 'remember))
      (remember))))

(load-package 'hippie-exp
  :eval
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-expand-line
          try-expand-list
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))


;;; Elfeed

(with-eval-after-load 'elfeed

  (custom-set-variables
   '(elfeed-use-curl t)
   '(elfeed-db-directory (concat user-emacs-directory "elfeed/"))
   '(elfeed-enclosure-default-dir user-downloads-directory))

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
    "Visit the current entry, or region entries, using `browse-url-macos-background'."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (mapc (lambda (entry)
              (browse-url-macos-background (elfeed-entry-link entry))
              (elfeed-untag entry 'unread)
              (elfeed-search-update-entry entry))
            entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))

  (defun elfeed-show-visit-background ()
    "Visit the current entry in your browser using `browse-url-macos-background'."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (browse-url-macos-background link))))

  (defun elfeed-ytdl-download ()
    "Jump to next link (always entry link) and call `ytdl-download'."
    (interactive)
    (shr-next-link)
    (ytdl-download))

  (defun elfeed-search-safari-read-later ()
    "Save the current entry, or region entries, to Safari's Reading List."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (mapc (lambda (entry)
              (safari-read-later (elfeed-entry-link entry))
              (elfeed-untag entry 'unread)
              (elfeed-search-update-entry entry))
            entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))

  (defun elfeed-show-safari-read-later ()
    "Save the current elfeed entry to Safari's Reading List."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (safari-read-later link))))

  (defkey elfeed-search-mode-map
    "b"   'elfeed-search-browse-url-background
    "*"   'elfeed-search-tag--star
    "8"   'elfeed-search-untag--star
    "s-D" 'elfeed-search-safari-read-later
    "o"   'delete-other-windows
    "E"   'elfeed-search:emacs
    "O"   'elfeed-search:other
    "S"   'elfeed-search:star)

  (defkey elfeed-show-mode-map
    "r"   'elfeed-show-tag--read
    "u"   'elfeed-show-tag--unread
    "*"   'elfeed-show-tag--star
    "8"   'elfeed-show-tag--unstar
    "s-D" 'elfeed-show-safari-read-later
    "b"   'elfeed-show-visit-background
    "o"   'delete-other-windows
    "d"   'elfeed-ytdl-download)

  (add-hook 'elfeed-show-mode-hook 'variable-pitch-adjust-mode)

  ) ; End elfeed


;;; EWW

(setq shr-max-image-proportion 0.5)
(setq shr-width 80)
(setq shr-bullet "• ")
;;(setq browse-url-browser-function 'eww-browse-url) ; Use EWW as Emacs's browser

(setq eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/pdf\\)")

(defun eww-mode-setup ()
  "Apply some customization to fonts in eww-mode."
  (text-scale-increase 1)
  (setq-local line-spacing 2))

(add-hook 'eww-mode-hook 'eww-mode-setup)

(with-eval-after-load 'eww
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

  (transient-define-prefix eww-mode-help-transient ()
    "Transient for EWW"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["EWW"
     ["Actions"
      ("G" "Browse" eww)
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
  ) ; end "EWW"


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
   '(org-adapt-indentation nil)
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
  (add-to-list 'org-agenda-files "~/home/writing/kindred/compendium.org")

  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))

  (setq org-todo-keywords
        '((sequence "TODO" "DELG" "LATER" "|" "DONE" "MOVED" "CANCELED")))

  (setq org-todo-keyword-faces
        '(("DELG" . org-scheduled-previously)
          ("LATER" . org-scheduled-previously)))

  (transient-define-prefix org-todo-transient ()
    [["Org TODO Status"
      ("t" "TODO"     (lambda () (interactive) (org-todo "TODO")))
      ("g" "DELG"     (lambda () (interactive) (org-todo "DELG")))
      ("l" "LATER"    (lambda () (interactive) (org-todo "LATER")))
      ("d" "DONE"     (lambda () (interactive) (org-todo "DONE")))
      ("c" "CANCELED" (lambda () (interactive) (org-todo "CANCELED")))
      ("m" "MOVED"    (lambda () (interactive) (org-todo "MOVED")))
      ]])

  (defkey org-mode-map "C-c C-t" 'org-todo-transient)

  (setq org-capture-templates
        `(("i" "Personal Inbox" entry
           (file+headline ,(concat org-directory "life.org") "Inbox")
           "* %?\n\n" :empty-lines 1)
          ("l" "Personal Log Entry" entry
           (file+olp+datetree ,(concat org-directory "logbook.org"))
           "* %?\n%T\n\n" :empty-lines 1 :tree-type month )
          ("e" "Emacs Config" entry
           (file+headline ,(concat org-directory "emacs.org") "Emacs Config")
           "* TODO %?" :empty-lines 1)))

  (defun org-toggle-checkbox-presence ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-toggle-checkbox)))

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

  (defkey org-agenda-mode-map
    "k" 'org-capture
    "K" 'org-agenda-capture
    "S" 'org-agenda-schedule
    "D" 'org-agenda-deadline
    "C-/" 'org-agenda-undo)

  (transient-define-prefix org-agenda-todo-transient ()
  [["Org TODO Status"
    ("t" "TODO"     (lambda () (interactive) (org-agenda-todo "TODO")))
    ("g" "DELG"     (lambda () (interactive) (org-agenda-todo "DELG")))
    ("l" "LATER"    (lambda () (interactive) (org-agenda-todo "LATER")))
    ("d" "DONE"     (lambda () (interactive) (org-agenda-todo "DONE")))
    ("c" "CANCELED" (lambda () (interactive) (org-agenda-todo "CANCELED")))
    ("m" "MOVED"    (lambda () (interactive) (org-agenda-todo "MOVED")))
    ]])

  (defkey org-agenda-mode-map "t" 'org-agenda-todo-transient)

  ) ; End Org Agenda


;;;; Calendar <--> Org Integration

;; You can jump to org's agenda from the calendar, and capture from any date
;; in the agenda, why now capture from the calendar as well?

(with-eval-after-load 'calendar
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

  (defkey calendar-mode-map "k" 'org-calendar-capture))


;;; Transients

(autoload 'universal-transient "transient" nil t)

(with-eval-after-load 'transient

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
      ("a" "AutoFill" auto-fill-mode)
      ("h" "Line Highlight" hl-line-mode)
      ("g" "Magit Status" magit-status)
      ("l" "List Buffers" bs-show)
      ("k" "Kill Buffer" kill-buffer-dwim)
      ("K" "Kill Buffer & Window" kill-buffer-and-window)]
     ["Org"
      ("o a" "Org Agenda" org-agenda)
      ("o k" "Org Capture" org-capture)
      ("o s" "Store Link" org-store-link)
      ("o i" "Insert Link" org-insert-link)
      "Consult"
      ("c o" "Outline" consult-outline)
      ("c g" "Grep" consult-grep)]
     ["Other"
      ("f" "Set Fonts" set-custom-fonts)
      ("t" "Toggle Dark/Light Theme" toggle-theme-color :transient t)
      ("T" "Toggle macOS Apperance" macos-toggle-system-appearance :transient t)
      ("D" "Date/Time mode-line" toggle-date-time-battery)
      ("C" "Calendar" calendar)
      ("W" "World Clock" world-clock)
      ("d" "Define Word" sdcv-search)
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

  ) ; end transient

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


;;; Quick Help

;; Inspired by "On-demand help panels for obscure topics" here:
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs

(defmacro quick-help (name buffer text)
  "Macro for creating callable functions that display help.
Where NAME is name of function, BUFFER is name of buffer, and TEXT is displayed."
  (declare (indent defun))
  `(progn
     (defun ,name nil
       ,buffer
       (interactive)
       (let ((qh-buff (concat "*Quick Help: " ,buffer "*"))
             (qh-text ,text))
         (get-buffer-create qh-buff)
         (with-current-buffer qh-buff
           (insert qh-text)
           (goto-char (point-min))
           (not-modified)
           (read-only-mode)
           (local-set-key (kbd "C-g") (lambda () (interactive) (other-window -1)))
           (local-set-key (kbd "q") 'kill-buffer-and-window))
         (pop-to-buffer qh-buff '((display-buffer-below-selected)
                                  (window-parameters . ((no-other-window . nil)))
                                  (window-height . fit-window-to-buffer)))
         (message "C-g - Previous Window, q - Remove Window")))))

(quick-help qh--wheather
  "Weather Whether Wether"
  "The climate is made up of \"WEATHER\";
WHETHER it is nice out depends on whether it is raining or not.
A WETHER is just a castrated sheep.")

(quick-help qh--lying
  "Lying"
  "\
Lie (recline)   lay   lain  lying
Lay (put down)  laid  laid  laying
Lie (false)     lied  lied  lying   lies")

(quick-help qh--NATO-alphabet
  "NATO ALPHABET"
  "\
A - Alpha                  N - November
B - Bravo                  O - Oscar
C - Charlie                P - Papa
D - Delta                  Q - Quebec
E - Echo                   R - Romeo
F - Foxtrot                S - Sierra
G - Golf                   T - Tango
H - Hotel                  U - Uniform
I - India                  V - Victor
J - Juliet                 W - Whiskey
K - Kilo                   X - X-ray
L - Lima                   Y - Yankee
M - Mike                   Z - Zulu")

(define-prefix-command 'quick-help-prompt nil "Quick Help")

(defkey quick-help-prompt
  "w" '("Weather" . qh--wheather)
  "l" '("Lay" . qh--lying)
  "n" '("Nato" . qh--NATO-alphabet))

(defkey global-map "C-c h" 'quick-help-prompt)

;;; End of init.el
