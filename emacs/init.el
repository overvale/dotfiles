;; init.el --- Oliver Taylor's Emacs Config -*- lexical-binding: t -*-

;; Copyright © Oliver Taylor
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
;; https://github.com/raxod502/radian
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; https://github.com/casouri/lunarymacs
;; https://github.com/oantolin/emacs-config


;;; Settings

;; I can never seem to actually suppress the Emacs startup echo area message,
;; despite following the documentation carefully, so just redefine the
;; function instead.
(defun display-startup-echo-area-message ()
  (message "Welcome to Emacs!"))

;; Save all interactive customization to a temp file, which is never loaded.
;; This means interactive customization is session-local. Only this init file persists sessions.
(setq custom-file (make-temp-file "emacs-custom-"))

(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

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
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(context-menu-mode 1)
 '(column-number-mode 1)
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
 '(split-width-threshold 120)
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

;;;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-archive-priorities '(("gnu" . 30)
                                   ("nongnu" . 25)
                                   ("melpa-stable" . 20)
                                   ("melpa" . 10)))

(setq package-selected-packages
      '(consult
        ctrlf
        delight
        ef-themes
        embark
        embark-consult
        exec-path-from-shell
        lua-mode
        magit
        markdown-mode
        marginalia
        modus-themes
        move-text
        no-littering
        olivetti
        orderless
        orgalist
        request
        standard-themes
        titlecase
        vertico
        visual-regexp
        visual-regexp-steroids
        vundo))

;;;; Load-Package Macro

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
Available keywords are (MUST USE ONE):

  :require       If present, require the package.
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


;;;; Defkey

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

;;;; Override Global Mode

;; The key binding technique below is taken from the bind-key package. It
;; places all the bindings I don't want overridden into a minor mode which is
;; inserted into the `emulation-mode-map-alists', so only very few things can
;; override them.

(defvar override-global-map (make-keymap)
  "Keymap for `override-global-mode'.")

(define-minor-mode override-global-mode
  "Minor mode for my personal keybindings, which override others.
The only purpose of this minor mode is to override global keybindings.
Keybindings you define here will take precedence."
  :init-value t
  :global t
  :keymap override-global-map)

(add-to-list 'emulation-mode-map-alists
             `((override-global-mode . ,override-global-map)))


;;;; Critical Packages

;; These packages effect how Emacs starts-up, and how this config is loaded,
;; so they need to be declared early on.

(load-package 'transient
  ;; The best tutorial I've found for transients is here:
  ;; https://github.com/positron-solutions/transient-showcase
  :autoload transient-define-prefix
  :after-load
  (setq transient-detect-key-conflicts t
        transient-force-fixed-pitch t
        transient-show-popup t))

(load-package 'exec-path-from-shell
  :require
  :eval
  (exec-path-from-shell-initialize))

(load-package 'no-littering
  :require
  :eval
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


;;;; Environment Variables

(setq local-package-dir (concat user-home-dir "opt/"))

(setq org-directory (concat user-home-dir "org/"))

(load (concat user-home-dir "src/lisp/private.el"))

(add-to-list 'load-path (concat user-home-dir "dot/emacs/lisp/"))


;;; Functions

;; Some of these functions I wrote myself, many of them I copied (and perhaps
;; modified) from other people's configs.

(defun find-user-init-file ()
  "Find the user-init-file."
  (interactive)
  (find-file user-init-file))

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

(defun transpose-windows ()
  "Swap the buffers shown in current and next window."
  (interactive)
  (let ((this-buffer (window-buffer))
        (next-window (next-window nil :no-minibuf nil)))
    (set-window-buffer nil (window-buffer next-window))
    (set-window-buffer next-window this-buffer)
    (select-window next-window)))

(defun other-window-previous nil
  "Select previous window."
  (interactive)
  (other-window -1))

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

(defun join-line-next nil
  "Join this line to next."
  (interactive)
  (save-excursion
    (delete-indentation t)))

(defun dumb-down-punctuation (beg end)
  "Replace smart punctuation in buffer or region with ascii equivalents."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'")
                            ("\x2013" . "--")
                            ("\x2014" . "---"))
                          nil beg end))

(defun compose-mail-macos (&optional to subject body)
  "Compose a message using macOS's default mail program."
  (interactive)
  (start-process "Open Mail Message" nil "open"
                 (concat "mailto:"
                         (when to to) "?"
                         (when subject (concat "&subject=" (url-hexify-string subject)))
                         (when body (concat "&body=" (url-hexify-string body))))))

(defun compose-mail-macos-multiple (list subject body)
  "ADDRESSES should be a list of email addresses in the form of:
(setq mail-list '(\"Mail 1 <mail1@example.com>\"
                  \"Mail 2 <mail2@example.com>\"
                  \"Mail 3 <mail3@example.com>\"))"
  (while list
    ;; car = first item, cdr = remaining items
    (let ((to (car list)))
      (compose-mail-macos to subject body)
      (setq list (cdr list)))))

(defun send-buffer-mail-macos ()
  "Mail buffer using `compose-mail-macos'."
  (interactive)
  (compose-mail-macos nil (buffer-name) (buffer-string)))


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


;;; Minibuffer

(custom-set-variables
 '(read-file-name-completion-ignore-case t)
 '(read-buffer-completion-ignore-case t)
 '(completion-ignore-case t)
 '(completions-format 'one-column)
 '(completions-detailed t)
 '(completion-show-help nil)
 '(completion-cycle-threshold nil)
 '(enable-recursive-minibuffers t)
 '(minibuffer-follows-selected-frame nil)
 '(savehist-mode t)
 '(minibuffer-eldef-shorten-default t)
 '(minibuffer-depth-indicate-mode t)
 '(file-name-shadow-mode 1)
 '(minibuffer-depth-indicate-mode 1)
 '(minibuffer-electric-default-mode 1))

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
    "M-v" 'switch-to-minibuffer
    "z" #'completion-kill-buffer
    [remap keyboard-quit] #'delete-completion-window
    [remap quit-window] #'completion-quit
    "n" 'next-completion
    "p" 'previous-completion))


;;; Key Bindings

(defkey key-translation-map "ESC" (kbd "C-g"))

(defkey override-global-map
  "<C-return>" 'general-dispatch
  "M-j" 'join-line-next
  "C-." 'embark-act
  "s-b" 'consult-buffer
  "s-B" 'consult-buffer-other-window
  "s-f" 'consult-line
  "s-F" 'consult-line-multi
  "s-o" 'find-file
  "s-O" 'find-file-other-window
  "s-j" 'dired-jump
  "s-w" 'window-dispatch
  "M-/" 'completion-at-point
  "M-\\" 'cycle-spacing
  "M-z" 'zap-up-to-char
  "M-Q" 'unfill-dwim
  "M-o" 'other-window
  "M-O" 'other-window-previous)

(transient-define-prefix general-dispatch ()
  "General-purpose transient."
  ["Dispatch Transient"
   [("a" "AutoFill" auto-fill-mode)
    ("v" "Wrap Lines" visual-line-mode)
    ("h" "Line Highlight" hl-line-mode)
    ("l" "List Buffers" bs-show)
    ("k" "Kill Buffer" kill-buffer-dwim)
    ("K" "Kill Buffer & Window" kill-buffer-and-window)]
   [("s o" "*scratch-org*" scratch-buffer-org)
    ("s m" "*scratch-markdown*" scratch-buffer-markdown)
    ("O" "Org Mode" org-mode)
    ("g" "Magit Status" magit-status)]
   [("o" "Outpost..." outpost-dispatch)
    ("d" "Define Word" sdcv-search)
    ("p" "Package Ops..." package-dispatch)
    ("m" "Keyboard Macro..." kbd-macro-dispatch)
    ("c" "Calendar" calendar)
    ("w" "World Clock" world-clock)]
   [("f" "Set Fonts" set-custom-fonts)
    ("F" "Set Buffer Fonts" buffer-remap-faces-mode)
    ("t" "Toggle Dark/Light Theme" modus-themes-toggle :transient t)
    ("D" "Date/Time mode-line" toggle-date-time-battery)]])

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

(with-eval-after-load 'bs-mode
  (defkey bs-mode-map "i" 'ibuffer))

(transient-define-prefix kbd-macro-dispatch ()
  "Transient for keyboard macros."
  [["Keyboard Macros"
    ("s" "Start" start-kbd-macro)
    ("e" "End" end-kbd-macro)
    ("c" "Call" call-last-kbd-macro)
    ("r" "Region Lines" apply-macro-to-region-lines)]])

(transient-define-prefix package-dispatch ()
  "Transient for package operations."
  [["Packages"
    ("h" "Describe" describe-package)
    ("a" "Autoremove" package-autoremove)
    ("d" "Delete" package-delete)
    ("i" "Install" package-install)
    ("s" "Selected" package-install-selected-packages)
    ("r" "Refresh" package-refresh-contents)
    ("l" "List" list-packages)]])


;; MacOS-like bindings

(defkey global-map
  "s-z" 'undo-only
  "s-Z" 'undo-redo
  "s-x" 'kill-region
  "s-c" 'kill-ring-save
  "s-v" 'yank
  "s-<backspace>" 'backward-kill-line
  "s-a" 'mark-whole-buffer
  "s-n" 'new-buffer
  "s-N" 'make-frame-command
  "s-[" 'previous-buffer
  "s-]" 'next-buffer
  "s-/" 'comment-line
  "s-e" 'isearch-forward-symbol-at-point
  "s-<up>" 'beginning-of-buffer
  "s-<down>" 'end-of-buffer
  "s-s" 'save-buffer
  "M-s-s" 'save-some-buffers
  "s-m" 'iconify-frame
  "s-," 'find-user-init-file
  "s-q" 'save-buffers-kill-emacs
  "s-." 'keyboard-quit
  "M-8" (lambda () (interactive) (insert "•")))


;;; Theme & Cursor

(custom-set-variables
 '(cursor-type 'box)
 '(cursor-in-non-selected-windows 'hollow)
 '(blink-cursor-blinks 0)
 '(blink-cursor-interval 0.5)
 '(blink-cursor-delay 0.2))

(setq custom-safe-themes t)

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

(load-package 'modus-themes
  ;; Run (modus-themes-list-colors-current) to see color options.
  :require
  :eval
  (defun modus-themes-color (face)
    "Return the value of a modus-themes face.
This function is missing in modus-themes 4.0 for some reason."
    (car (alist-get face (modus-themes--current-theme-palette))))

  (defun load-modus-theme-match-system nil
    "Load `modus-operandi' or `modus-vivendi' matching the system theme."
    (interactive)
    (if (macos-dark-p)
        (load-theme-cleanly 'modus-vivendi)
      (load-theme-cleanly 'modus-operandi)))

  (setq modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-prompts '(bold)
        modus-themes-org-blocks 'gray-background
        modus-themes-headings
        '( (0 . (variable-pitch bold 1.4))
           (1 . (variable-pitch bold 1.3))
           (2 . (variable-pitch bold 1.2))
           (3 . (variable-pitch bold 1.1))
           (t . (1))))

  (setq modus-themes-common-palette-overrides
        '((underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)))

  (setq modus-operandi-palette-overrides
        '((bg-mode-line-active bg-blue-intense)
          (fg-mode-line-active fg-main)
          (bg-region bg-lavender)
          (fg-region fg-main)
          (comment red)
          (string green-cooler)))

  (setq modus-vivendi-palette-overrides
        '((bg-main bg-dim)
          (cursor blue-intense)
          (bg-mode-line-active bg-cyan-subtle)
          (fg-mode-line-active fg-main)
          (bg-region bg-lavender)
          (fg-region fg-main)
          (comment yellow-cooler)
          (string green-faint)))

  (load-modus-theme-match-system)

  (add-hook 'mac-effective-appearance-change-hook 'load-modus-theme-match-system))


;;; Fonts

;; This makes it so text-scale adjustments operate exactly one point size at a
;; time. The original value is 1.2, which sometimes increases/decreases by
;; more than a single point size. For example: (* 1.2 12) = 14.39
(setq text-scale-mode-step 1.09)

(load-package 'custom-fonts
  :require
  :after-load
  (setq custom-fonts-alist
        '((Apple . ( :mono "SF Mono"
                     :vari "New York"
                     :mode "SF Compact Text"
                     :line nil
                     :mono-height 120
                     :mode-height 130
                     :vari-height 140))
          (Berkeley . ( :mono "Berkeley Mono"
                        :vari "Go"
                        :mode "SF Compact Text"
                        :line nil
                        :mono-height 120
                        :mode-height 130
                        :vari-height 130))
          (Dossier . ( :mono "Dossier"
                       :vari "New York Small"
                       :mode "SF Compact Text"
                       :line 1
                       :mono-height 110
                       :mode-height 130
                       :vari-height 140))
          (IBM . ( :mono "IBM Plex Mono"
                   :vari "IBM Plex Serif"
                   :mode "IBM Plex Sans"
                   :line nil
                   :mono-height 120
                   :mode-height 130
                   :vari-height 130))
          (Pragmata . ( :mono "PragmataPro"
                        :vari "SF Pro Text"
                        :mode "SF Compact Text"
                        :line nil
                        :mono-height 130
                        :mode-height 130
                        :vari-height 120))
          (Triplicate . ( :mono "Triplicate T4"
                          :vari "Triplicate T4p"
                          :mode "SF Compact Text"
                          :line nil
                          :mono-height 130
                          :mode-height 130
                          :vari-height 130))
          ))

  (set-custom-fonts "Berkeley"))

;; Enable rendering SF symbols on macOS.
(set-fontset-font t nil "SF Pro Text" nil 'append)


;;; Mode-Line

(setq display-time-default-load-average nil)
(setq display-time-format "  %F  %R")
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
  (if (and (buffer-modified-p) ;; modified
           (buffer-file-name)) ;; file-visiting buffer
      (propertize " 􀑇"
                  'help-echo "Buffer is modified.")))

(defun mode-line-buffer-read-only-status ()
  "Return a string indicating buffer read-only status."
  (if buffer-read-only
      (propertize " 􀎠 "
                  'help-echo "Buffer is read-only!")))

(defun mode-line-buffer-confirm-kill-status ()
  "Return a string indicating `buffer-confirm-kill' status."
  (if buffer-confirm-kill
      (propertize " 􀉻 "
                  'help-echo "You must confirm killing this buffer.")))

(defun mode-line-buffer-line-spacing-status ()
  "Return a string indicating the buffer's `line-spacing' value."
  (if line-spacing
      (propertize (concat " 􀆏" (number-to-string line-spacing) " ")
                  'help-echo "Buffer's line-spacing has been modified.")))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here. Check the docstrings for more
;; information.
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(setq-default mode-line-format
              '((:eval (mode-line-buffer-modified-status))
                "  " mode-line-buffer-identification "  "
                (:eval (mode-line-buffer-read-only-status))
                (:eval (mode-line-buffer-confirm-kill-status))
                (:eval (if (eq buffer-position t) (list "  􀋵 " mode-line-position "  ") " "))
                mode-line-modes
                (:eval (mode-line-buffer-line-spacing-status))
                mode-line-misc-info))


;;; Mac-style Tabs

;; I use the Emacs Mac-Port, and the below code makes it so Emacs uses
;; mac-style tabs, with mac-style shortcuts. I like this much better than
;; `tab-bar-mode'.

(defun mac-new-tab ()
  "Create new Mac-style tab."
  (interactive)
  (let ((mac-frame-tabbing t))
    (make-frame-command)))

(defkey override-global-map
  "s-t" 'mac-new-tab
  "s-}" (lambda () (interactive (mac-next-tab 1)))
  "s-{" (lambda () (interactive (mac-previous-tab 1)))
  "s-T" 'mac-toggle-tab-bar
  "s-|" 'mac-toggle-tab-group-overview)


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

(defkey override-global-map
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
(transient-define-prefix mark-dispatch ()
  "Transient dispatcher for marking commands."
  :transient-suffix 'transient--do-stay
  [["Navigate:"
    ("c" "Consult Mark" consult-mark :transient nil)
    ("," "Back" pop-to-mark-command)
    ("." "Forward" unpop-to-mark-command)]
   ["Set/Extend Mark To..."
    ("b" "Whole Buffer" mark-whole-buffer)
    ("<" "Beginning of Buffer" mark-beginning-of-buffer)
    (">" "End of Buffer" mark-end-of-buffer)
    ("S" "Sexp" mark-sexp)
    ("D" "Defun" mark-defun)]
   [""
    ("w" "Word" mark-word)
    ("l" "Line" mark-line)
    ("p" "Paragraph" mark-paragraph)
    ("s" "Sentence" mark-sentence)
    ("P" "Page" mark-page)]
   ["Other:"
    ("SPC" "Activate Mark" activate-the-mark)
    ("RET" "Exit" transient-quit-all)]])

(defkey override-global-map
  "C-z" 'mark-dispatch
  "C-M-h" 'mark-line
  "C-x C-x" 'exchange-point-and-mark-dwim)


;;; Windows

;; Settings for how Emacs displays windows, splits, etc.

(custom-set-variables
 '(winner-mode t)
 '(split-window-keep-point nil)
 '(even-window-sizes nil)
 '(switch-to-buffer-obey-display-actions t)
 '(help-window-select t))

;; The order of the below items matter. The first one that matches is applied.
;; That's why all these add-to-list items have the APPEND flag.
(add-to-list 'display-buffer-alist
             '("\\*Calendar.*"
               (display-buffer-below-selected)
               (window-parameters . ((no-other-window . nil)))
               (window-height . fit-window-to-buffer))
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

(transient-define-prefix window-dispatch ()
  "Most commonly used window commands."
  [["Splits"
    ("h" "Split Horizontal" split-window-below-select)
    ("v" "Split Vertical"   split-window-right-select)
    ("b" "Balance"    balance-windows)
    ("f" "Fit"        fit-window-to-buffer)
    ("+" "Toggle H/V Split" toggle-window-split)
    ("t" "Transpose Windows" transpose-windows)]
   ["Window"
    ("d" "Dedicate Window" dedicated-mode)
    ("k" "Kill" delete-window)
    ("s-w" "Kill Frame" delete-frame)
    ("K" "Kill Buffer & Window" kill-buffer-and-window)
    ("o" "Kill Others"  delete-other-windows)
    ("m" "Maximize" maximize-window)]
   [""
    ("c" "Clone Indirect" clone-indirect-buffer)
    ("T" "Tear Off" tear-off-window)]])


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

(defun toggle-buffer-kill-protection nil
  "Sets `buffer-confirm-kill' and `buffer-offer-save' to t."
  (interactive)
  (if buffer-confirm-kill
      (progn
        (setq-local buffer-confirm-kill nil
                    buffer-offer-save nil)
        (message "'buffer-confirm-kill' set to 'nil'"))
    (progn
      (setq-local buffer-confirm-kill t
                  buffer-offer-save t)
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
        (gfm-mode) ; GitHub-Flavored Markdown
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
    (text-mode)
    (setq-local buffer-offer-save t)
    (setq-local buffer-confirm-kill t)))


;;; Web Search

;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el

(defun web-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro web-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "web-%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (web-search ,search-engine-url ,search-engine-prompt)))

(web-search-engine "google"
                   "http://www.google.com/search?q="
                   "Google: ")

(web-search-engine "youtube"
                   "http://www.youtube.com/results?search_query="
                   "Search YouTube: ")

(web-search-engine "github"
                   "https://github.com/search?q="
                   "Search GitHub: ")

(web-search-engine "pinboard"
                   "https://pinboard.in/search/u:Oliver?query="
                   "Pinboard: ")

(web-search-engine "wikipedia"
                   "http://en.wikipedia.org/wiki/Special:Search?go=Go&search="
                   "Search Wikipedia: ")

(web-search-engine "github-elisp"
                   "https://github.com/search?l=Emacs+Lisp&type=Code&q="
                   "Github Elisp: ")

(web-search-engine "imdb"
                   "https://www.imdb.com/find/?q="
                   "IMDB: ")

(web-search-engine "linkedin"
                   "https://www.linkedin.com/search/results/all/?keywords="
                   "LinkedIn: ")


;;; Quick Help

;; Inspired by "On-demand help panels for obscure topics" here:
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs

(defvar quick-help-alist nil
  "An alist of functions used by `quick-help'.")

(defun quick-help nil
  "Create quick help buffer for TOPIC with TEXT."
  (interactive)
  (let* ((topic (completing-read "Help For: " quick-help-alist))
         (help (alist-get (intern topic) quick-help-alist))
         (qh-buffer "*Quick Help*"))
    (with-current-buffer (get-buffer-create qh-buffer)
      (buffer-disable-undo)
      (setf (buffer-string) help)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (toggle-truncate-lines 1)
      (let ((view-read-only nil))
        (read-only-mode 1))
      (local-set-key (kbd "C-g") (lambda () (interactive) (other-window -1)))
      (local-set-key (kbd "q") 'kill-buffer-and-window))
    (pop-to-buffer qh-buffer '((display-buffer-below-selected)
                               (window-parameters . ((no-other-window . nil)))
                               (window-height . fit-window-to-buffer)))
    (message "C-g - Previous Window, q - Remove Window")))

(add-to-list 'quick-help-alist '(weather . "Weather Whether Wether

The climate is made up of \"WEATHER\";
WHETHER it is nice out depends on whether it is raining or not.
A WETHER is just a castrated sheep.") t)


(add-to-list 'quick-help-alist '(lying . "Lying

Lie (recline)   lay   lain  lying
Lay (put down)  laid  laid  laying
Lie (false)     lied  lied  lying   lies") t)

(add-to-list 'quick-help-alist '(NATO-alphabet . "NATO ALPHABET

A - Alpha        N - November
B - Bravo        O - Oscar
C - Charlie      P - Papa
D - Delta        Q - Quebec
E - Echo         R - Romeo
F - Foxtrot      S - Sierra
G - Golf         T - Tango
H - Hotel        U - Uniform
I - India        V - Victor
J - Juliet       W - Whiskey
K - Kilo         X - X-ray
L - Lima         Y - Yankee
M - Mike         Z - Zulu") t)

(add-to-list 'quick-help-alist '(info-mode . "Info Mode

p   - previous node       n - next node
[   - previous node       ] - next node
^   - up node             d - go to top
r   - forward history     l - back history
s/S - search              i - search index
m   - pick menu           f - follow reference
g   - goto node") t)

(add-to-list 'quick-help-alist '(dired-mode . "Dired Mode

f       open    b       back    o       open in new window
D       delete  C       copy    R       rename
m       mark    u       unmark  t       toggle mark
s-c     copy    s-v     paste   M-s-v   move
S-s-.   toggle invisible        (       toggle more info") t)

(defkey global-map "C-c h" 'quick-help)


;;; Text-Plus-Mode

(defvar-local text-plus-mode nil)

(defun text-plus-mode ()
  "Toggle orgalist-mode, orgtbl-mode, and visual-line-mode."
  (interactive)
  (if text-plus-mode
      (progn (setq text-plus-mode nil)
             (orgalist-mode -1)
             (orgtbl-mode -1)
             (visual-line-mode -1)
             (message "text-plus-mode deactivated"))
    (progn (setq text-plus-mode t)
           (orgalist-mode 1)
           (orgtbl-mode 1)
           (visual-line-mode 1)
           (message "text-plus-mode activated"))))



;;; Snippets

(defvar snippet-alist nil
  "An alist of snippets.")

(defun insert-snippet (snippet)
  "Prompt user to insert snippet from `snippet-alist'."
  (interactive
   (list (completing-read
          "Insert Snippet: "
          (mapcar #'car snippet-alist))))
  (let* ((snip (intern snippet))
         (string (alist-get snip snippet-alist)))
    (insert string)))

(add-to-list 'snippet-alist '(html-list . "\
<ul>
  <li></li>
</ul>"))

(add-to-list 'snippet-alist '(html-li . "<li></li>"))

(add-to-list 'snippet-alist '(defun . "\
(defun function ()
  (interactive)
  )"))

(add-to-list 'snippet-alist '(load-package . "\
(let ((package 'x))
  (require package)
  (add-to-list 'load-path (concat local-package-dir \"\"))
  (autoload 'x package nil nil)
  (with-eval-after-load package
    (progn
      ))
  (progn
    ))"))

(add-to-list 'snippet-alist '(comment-line . "# ----------------------------------------------------------"))

(add-to-list 'snippet-alist '(transient . "\
(transient-define-prefix foo ()
    \"bar\"
    [[\"baz\"
      (\"x\" \"foo\" bar)]
     ])"))


;;; Packages

(load-package 'orderless
  :require
  :eval
  (custom-set-variables
   '(completion-styles '(substring orderless basic))))

(load-package 'vertico
  :require
  :eval
  (vertico-mode 1)
  (vertico-multiform-mode 1)

  (setq vertico-multiform-categories
        '((consult-grep buffer)))

  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (consult-outline buffer indexed)
          (consult-line-multi buffer)))

  (defkey vertico-map
    "'"   #'vertico-quick-jump
    "M-v" #'vertico-multiform-vertical
    "M-g" #'vertico-multiform-grid
    "M-f" #'vertico-multiform-flat
    "M-r" #'vertico-multiform-reverse
    "M-u" #'vertico-multiform-unobtrusive)

  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(load-package 'ctrlf
  :eval
  (setq ctrlf-go-to-end-of-match nil
        ctrlf-highlight-current-line nil
        ctrlf-show-match-count-at-eol nil)
  (ctrlf-mode 1))

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
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  (defkey global-map
    [remap imenu] 'consult-imenu
    [remap yank-pop] 'consult-yank-pop
    [remap repeat-complex-command] #'consult-complex-command))

(load-package 'magit
  :after-load
  ;; Magit overrides `override-global-map', so I need to override this here:
  (defkey magit-file-section-map
    "<C-return>" 'universal-transient))

(load-package 'visual-regexp
  :eval
  (defkey global-map [remap query-replace] 'vr/query-replace)
  :after-load
  (with-eval-after-load 'visual-regexp-steroids
    (custom-set-variables
     '(vr/engine 'pcre2el))))

(load-package 'vundo
  :autoload vundo
  :after-load
  (custom-set-variables
   '(vundo-glyph-alist vundo-unicode-symbols)))

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
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)))

(load-package 'olivetti
  :after-load
  (custom-set-variables
   '(olivetti-body-width 82)))

(load-package 'sdcv-mode
  :local-dir "emacs-sdcv"
  :autoload sdcv-search
  :eval
  (defalias 'describe-word 'sdcv-search))

(load-package 'move-text
  :require
  :eval
  (move-text-default-bindings))


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

  (defun dired-finder nil
    "Open the current dired directory in Finder."
    (interactive)
    (shell-command (concat "open " dired-directory)))

  (defkey dired-mode-map
    "." 'dired-finder
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
  :after-load
  (setq outline-minor-mode-cycle t)
  (defkey outline-minor-mode-cycle-map
    "TAB"   nil ;; too often conflicts with tab-complete
    "M-TAB" 'outline-cycle)
  (defkey outline-minor-mode-map
    "<backtab>" 'outline-cycle-buffer))

(load-package 'calendar
  :after-load
  (require 'solar)
  (setq calendar-latitude 34.157
        calendar-longitude -118.324)

  (defun year-calendar (&optional year)
    "Generate a one year calendar that can be scrolled by year in each direction.
This is a modification of:  http://homepage3.nifty.com/oatu/emacs/calendar.html
See also: https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months"
    (interactive)
    (require 'calendar)
    (let* ((current-year (number-to-string (nth 5 (decode-time (current-time)))))
           (month 0)
           (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
      (switch-to-buffer (get-buffer-create calendar-buffer))
      (when (not (eq major-mode 'calendar-mode))
        (calendar-mode))
      (setq displayed-month month)
      (setq displayed-year year)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; horizontal rows
      (dotimes (j 4)
        ;; vertical columns
        (dotimes (i 3)
          (calendar-generate-month
           (setq month (+ month 1))
           year
           ;; indentation / spacing between months
           (+ 5 (* 25 i))))
        (goto-char (point-max))
        (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
        (widen)
        (goto-char (point-max))
        (narrow-to-region (point-max) (point-max)))
      (widen)
      (goto-char (point-min))
      (setq buffer-read-only t)))

  (defvar calendar-copy-as-kill-format "%a, %d %b"
    "Format string for formatting calendar dates with `format-time-string'.")

  (defun calendar-copy-as-kill ()
    "Copy date at point as kill if region is not active."
    (interactive)
    (if (use-region-p)
        (call-interactively #'kill-ring-save)
      (let ((date (calendar-cursor-to-date)))
        (when date
          (setq date (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
          (kill-new (format-time-string calendar-copy-as-kill-format date))
          (message "Date saved to kill-ring.")))))

  (defkey calendar-mode-map
    "w"   'calendar-copy-as-kill
    "s-c" 'calendar-copy-as-kill))

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

  (defun switch-to-consult-line ()
    "Switch from isearch to consult-line."
    (interactive)
    (isearch-exit)
    (consult-line isearch-string))

  (defun isearch-exit-at-start ()
    "Exit search at the beginning of the current match."
    (unless (or isearch-mode-end-hook-quit
                (bound-and-true-p isearch-suspended)
                (not isearch-forward)
                (not isearch-other-end)
                (and (boundp 'avy-command)
                     (eq avy-command 'avy-isearch)))
      (goto-char isearch-other-end)))

  (defkey isearch-mode-map
    "M-l" 'switch-to-consult-line)

  (add-hook 'isearch-mode-end-hook 'isearch-exit-at-start))

(load-package 'ibuffer
  :after-load
  (setq ibuffer-display-summary nil))

(load-package 'browse-url
  :eval
  (setq browse-url-mailto-function 'browse-url-generic
        browse-url-generic-program "open")

  (defun browse-url-macos-background (url)
    "Open URL with macOS `open'."
    (interactive)
    (start-process "open url"
                   nil "open" "--background" url)
    (message "URL opened in background.")))

(load-package 'rect
  :after-load
  (defkey rectangle-mark-mode-map
    "t" 'string-rectangle
    "o" 'open-rectangle
    "c" 'clear-rectangle
    "n" 'rectangle-number-lines
    "x" 'rectangle-exchange-point-and-mark
    "w" 'copy-rectangle-as-kill
    "y" 'yank-rectangle
    "k" 'kill-rectangle))


;;; Org


(defkey override-global-map
  "s-k" 'org-capture)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(defun org-search nil
  "Go to any headline in your refile targets."
  (interactive)
  (org-refile '(4)))

(with-eval-after-load 'org
  (custom-set-variables
   '(org-ellipsis " 􀠩")
   '(org-list-allow-alphabetical t)
   '(org-log-done 'time)
   '(org-log-into-drawer t)
   '(org-special-ctrl-a/e t)
   '(org-special-ctrl-k t)
   '(org-return-follows-link t)
   '(org-catch-invisible-edits 'show-and-error)
   '(org-refile-use-outline-path 'file)
   '(org-outline-path-complete-in-steps nil)
   '(org-goto-interface 'outline-path-completion)
   '(org-goto-max-level 10)
   '(org-refile-targets '((org-agenda-files :maxlevel . 5)))
   '(org-startup-with-inline-images t)
   '(org-image-actual-width '(600))
   '(org-hide-emphasis-markers nil)
   '(org-hide-leading-stars nil)
   '(org-adapt-indentation nil)
   '(org-insert-heading-respect-content t)
   '(org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+")))
   '(org-list-indent-offset 2)
   ;; src blocks
   '(org-link-elisp-confirm-function nil)
   '(org-src-fontify-natively t)
   '(org-fontify-quote-and-verse-blocks t)
   '(org-src-tab-acts-natively t)
   '(org-edit-src-content-indentation 0))

  (transient-define-prefix org-todo-transient ()
    [["Org TODO Status"
      ("t" "TODO"     (lambda () (interactive) (org-todo "TODO")))
      ("w" "WAIT"     (lambda () (interactive) (org-todo "DELG")))
      ("l" "LATER"    (lambda () (interactive) (org-todo "LATER")))
      ("d" "DONE"     (lambda () (interactive) (org-todo "DONE")))
      ("c" "CANCELED" (lambda () (interactive) (org-todo "CANCELED")))
      ("m" "MOVED"    (lambda () (interactive) (org-todo "MOVED")))
      ]])

  (defkey org-mode-map "C-c C-t" 'org-todo-transient)

  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))

  (setq org-todo-keywords
        '((sequence "TODO" "WAIT" "LATER" "|" "DONE" "MOVED" "CANCELED")))

  (setq org-todo-keyword-faces
        '(("WAIT" . org-scheduled-previously)
          ("LATER" . org-scheduled-previously))))


(with-eval-after-load 'org-agenda
  (custom-set-variables
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

  (add-hook 'org-agenda-mode-hook 'hl-line-mode)

  (transient-define-prefix org-agenda-todo-transient ()
    [["Org TODO Status"
      ("t" "TODO"     (lambda () (interactive) (org-agenda-todo "TODO")))
      ("w" "WAIT"     (lambda () (interactive) (org-agenda-todo "DELG")))
      ("l" "LATER"    (lambda () (interactive) (org-agenda-todo "LATER")))
      ("d" "DONE"     (lambda () (interactive) (org-agenda-todo "DONE")))
      ("c" "CANCELED" (lambda () (interactive) (org-agenda-todo "CANCELED")))
      ("m" "MOVED"    (lambda () (interactive) (org-agenda-todo "MOVED")))
      ]])

  (defkey org-agenda-mode-map "t" 'org-agenda-todo-transient)

  (setq org-agenda-custom-commands
        '(("1" "Agenda + TODO"
           ((agenda 'week)
            (todo "TODO|WAIT"
                  ((org-agenda-sorting-strategy '(todo-state-up priority-down))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                   (org-agenda-overriding-header "Active, not scheduled, Tasks: ")))))))

  (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))

  (defkey org-agenda-mode-map
    "k" 'org-capture
    "K" 'org-agenda-capture
    "S" 'org-agenda-schedule
    "D" 'org-agenda-deadline))

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
	           (org-get-cursor-date)))
          (delete-window)
          (call-interactively 'org-capture)))))

  (defkey calendar-mode-map "k" 'org-calendar-capture))


;;; End of init.el
