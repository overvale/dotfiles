;;; disabled.el -*- lexical-binding: t -*-


;; This file contains lisp that I'm not using but don't want to get rid of.


;;; Misc

(defun find-emacs-dotfiles ()
  "Find lisp files in your Emacs dotfiles directory, pass to completing-read."
  (interactive)
  (find-file (completing-read "Find Elisp Dotfile: "
                              (directory-files-recursively oht-dotfiles "\.el$"))))


;;; Mac Style Tabs

(defun oht-mac-new-tab ()
  "Create new Mac-style tab

macOS follows this convention: command-N creates a new window
and command-T creates a new tab in the same window. The Mac Port
version of Emacs has functions and variables that makes following
this convention possible.

This function works by setting the new-frame behaviour to use
tabs, creating a new frame (thus, tab), then changing the setting
back to system default."
  (interactive)
  (setq mac-frame-tabbing t)
  (make-frame-command)
  (setq mac-frame-tabbing 'automatic))

(defun oht-mac-show-tab-bar ()
  "Show the tab bar, part of the Mac Port"
  (interactive)
  (mac-set-frame-tab-group-property nil :tab-bar-visible-p t))

(defun oht-mac-hide-tab-bar ()
  "Hide the tab bar, part of the Mac Port"
  (interactive)
  (mac-set-frame-tab-group-property nil :tab-bar-visible-p nil))

(defun oht-mac-show-tab-bar-overview ()
  "Show the tab bar overview, part of the Mac Port"
  (interactive)
  (mac-set-frame-tab-group-property nil :overview-visible-p t))


;;; Dispatch

(defun oht-dispatch ()
  "Pass function names to completing-read for calling interactively.

This works by reading a list of functions to call interactively.
For example you might want to do something like:

(setq oht-dispatch-functions
      '(remember-notes
        elfeed
        org-agenda
        list-bookmarks
        mu4e
        eww
        oht-dispatch-downloads
        oht-dispatch-NPR-news
        oht-dispatch-CNN-news
        oht-dispatch-google-news))"
  (interactive)
  (call-interactively
   (intern (completing-read "Call Function: " oht-dispatch-functions))))


;;; EWW Bookmarks

;; The below code allows you to create standard emacs bookmarks in eww-mode.
;; It does this by customizing the `bookmark-make-record-function' variable to
;; a custom function.
;;
;; Adapted from:
;; 1. https://www.emacswiki.org/emacs/bookmark%2B-1.el
;; 2. https://github.com/TotalView/dotemacs/blob/master/.emacs.d/elpa/w3m-20140330.1933/bookmark-w3m.el

;; This function creates a properly formatted bookmark entry. It names a
;; 'handler' that's used when visiting the bookmark, defined below.
(defun oht-eww-bookmark-make-record ()
  "Make a bookmark record for the current eww buffer"
  `(,(plist-get eww-data :title)
    ((location . ,(eww-current-url))
     (handler . oht-eww-bookmark-handler)
     (defaults . (,(plist-get eww-data :title))))))

;; This function simply tells Emacs to use the custom function when using the
;; bookmarking system.
(defun oht-eww-set-bookmark-handler ()
  "This tells Emacs which function to use to create bookmarks."
  (interactive)
  (set (make-local-variable 'bookmark-make-record-function)
       #'oht-eww-bookmark-make-record))

(defun oht-eww-bookmark-handler (record)
  "Jump to an eww bookmarked location using EWW."
  (eww (bookmark-prop-get record 'location)))

;; Finally, add a hook to set the make-record-function
(add-hook 'eww-mode-hook 'oht-eww-set-bookmark-handler)


;;; Embark as Completion Framework

;; The following will allow you to use an embark live collection as your
;; completion framework.

;; Automatically show candidates after typing
(add-hook 'minibuffer-setup-hook 'embark-collect-completions-after-input)

(setq embark-collect-initial-view-alist
      '((file . list)
        (buffer . list)
        (symbol . list)
        (t . list)))

(defun switch-to-completions-or-other-window ()
  "Switch to the completions window, if it exists, or another window."
  (interactive)
  (if (get-buffer-window "*Embark Collect Completions*")
      (select-window (get-buffer-window "*Embark Collect Completions*"))
    (if (get-buffer-window "*Completions*")
        (progn
          (select-window (get-buffer-window "*Completions*"))
          (when (bobp) (next-completion 1)))
      (other-window 1))))

(defun switch-to-minibuffer ()
  "Focus the active minibuffer.
Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

(define-keys completion-list-mode-map
  (kbd "n") 'next-completion
  (kbd "p") 'previous-completion
  (kbd "M-o") 'switch-to-minibuffer)

(define-keys minibuffer-local-completion-map
  (kbd "M-o") 'switch-to-completions-or-other-window
  (kbd "C-n") 'switch-to-completions-or-other-window)

(defun embark-minibuffer-completion-help (_start _end)
  "Embark alternative to minibuffer-completion-help.
This means you hit TAB to trigger the completions list.
Source: https://old.reddit.com/r/emacs/comments/nhat3z/modifying_the_current_default_minibuffer/gz5tdeg/"
  (unless embark-collect-linked-buffer
    (embark-collect-completions)))
(advice-add 'minibuffer-completion-help
            :override #'embark-minibuffer-completion-help)

;; resize Embark Collect buffer to fit contents
(add-hook 'embark-collect-post-revert-hook
          (defun resize-embark-collect-window (&rest _)
            (when (memq embark-collect--kind '(:live :completions))
              (fit-window-to-buffer (get-buffer-window)
                                    (floor (frame-height) 2) 1))))

(defun exit-with-top-completion ()
  "Exit minibuffer with top completion candidate."
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (unless (test-completion content
                             minibuffer-completion-table
                             minibuffer-completion-predicate)
      (when-let ((completions (completion-all-sorted-completions)))
        (delete-minibuffer-contents)
        (insert
         (concat
          (substring content 0 (or (cdr (last completions)) 0))
          (car completions)))))
    (exit-minibuffer)))

(define-key minibuffer-local-completion-map          (kbd "<return>") 'exit-with-top-completion)
(define-key minibuffer-local-must-match-map          (kbd "<return>") 'exit-with-top-completion)
(define-key minibuffer-local-filename-completion-map (kbd "<return>") 'exit-with-top-completion)

;; Embark by default uses embark-minibuffer-candidates which does not sort the
;; completion candidates at all, this means that exit-with-top-completion
;; won't always pick the first one listed! If you want to ensure
;; exit-with-top-completion picks the first completion in the embark collect
;; buffer, you should configure Embark to use
;; embark-sorted-minibuffer-candidates instead. This can be done as follows:
(setq embark-candidate-collectors
      (cl-substitute 'embark-sorted-minibuffer-candidates
                     'embark-minibuffer-candidates
                     embark-candidate-collectors))


;;; PDFs

;; TODO: remove this

;; To get started, use homebrew to install a couple things:
;; $ brew install poppler automake
;; INITIALIZATION --- un-comment this on first-run
;; (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")

(use-package pdf-tools
  ;; To get Emacs to open PDFs in pdf-view-mode you need to add the extension
  ;; to the `auto-mode-alist', which pdf-tools suggests you do by calling the
  ;; function `pdf-tools-install'. But doing it this way loads the entire
  ;; pdf-tools package simply to make that alist association. Much easier to
  ;; set it manually (this code is taken directly from the pdf-tools source).
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations nil)
  (pdf-annot-list-format '((page . 3) (type . 24) (contents . 200)))
  ;; Required for retina scaling to work
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  :config
  ;; pdf-tools uses the `pdf-tools-install' function to set up hooks and
  ;; various things to ensure everything works as it should. PDFs will open
  ;; just fine without this, but not all features will be available.
  (pdf-tools-install)
  (defun oht-pdf-annot-fonts ()
    (hl-line-mode 1)
    (pdf-annot-list-follow-minor-mode))
  :hook (pdf-annot-list-mode-hook . oht-pdf-annot-fonts)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward)
              ("A" .   pdf-annot-add-highlight-markup-annotation)
              ("L" .   pdf-annot-list-annotations)
              ("O" .   pdf-occur)
              ("G" .   pdf-view-goto-page)
              ("<" .   pdf-view-first-page)
              (">" .   pdf-view-last-page)))

(use-package pdf-tools-org
  ;; This package seems pretty out of date, modifying the code as suggested here:
  ;; https://github.com/machc/pdf-tools-org/issues/7
  ;; seems to fix export
  :straight (:host github :repo "machc/pdf-tools-org" :branch "master")
  :commands pdf-tools-org-export-to-org)

;; The bad news is that pdf-tools might be entering a period of being
;; unmaintained. The github issue tracker is full of people commenting that
;; the maintainer cannot be reached. [2021-04-21]


;;; Mail

(setq mail-user-agent 'mu4e-user-agent)

(use-package message
  :straight nil
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (message-cite-style 'message-cite-style-thunderbird)
  (message-cite-function 'message-cite-original)
  (message-kill-buffer-on-exit t)
  (message-citation-line-format "On %d %b %Y at %R, %f wrote:\n")
  (message-citation-line-function 'message-insert-formatted-citation-line))

(use-package mu4e
  :load-path "/usr/local/Cellar/mu/1.4.15/share/emacs/site-lisp/mu/mu4e"
  :commands mu4e
  :bind (:map mu4e-headers-mode-map
              ("G" . mu4e-update-mail-and-index))
  :custom
  (mu4e-attachments-dir user-downloads-directory)
  (mu4e-update-interval (* 5 60))
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-format-flowed nil)
  (mu4e-confirm-quit nil)
  (mu4e-headers-date-format "%Y-%m-%d")
  (mu4e-headers-include-related nil)
  (mu4e-headers-skip-duplicates t)
  (mu4e-headers-time-format "%H:%M")
  (mu4e-headers-visible-lines 20)
  (mu4e-use-fancy-chars nil)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-headers-fields
   '((:human-date . 12)
     (:flags . 6)
     (:from . 22)
     (:thread-subject)))
  :config
  (load (concat oht-ingenuity-dir "mu4e.el"))
  (defun jcs-view-in-eww (msg)
    (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)
  (add-hook 'mu4e-compose-mode-hook
            (defun oht-mu4e-compose-settings ()
              "My settings for message composition."
              (auto-fill-mode -1)
              (visual-line-mode t)))
  (add-hook 'mu4e-view-mode-hook
            (defun oht-mu4e-view-settings ()
              "My settings for message composition."
              (facedancer-vadjust-mode 1)
              )))

(use-package smtpmail
  :straight nil
  :custom
  (auth-sources '((concat oht-ingenuity-dir "authinfo")))
  (smtpmail-stream-type 'starttls)
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587))


;;; Use-Package Autoremove

;; I want my init file to define all the packages I use, install them, and
;; remove them when no longer referenced. This is not how Package behaves by
;; default. Below is code that makes Package do this.
;;
;; When starting-up, package.el loads all packages in the variable
;; 'package-load-list', which defaults to 'all'. That means Package loads all
;; installed packages at startup -- even if those packages are not in your
;; init file. Package includes an autoremove function, but that function looks
;; at the variable `package-selected-packages' for the canonical list of
;; packages (not your init file) and `use-package' does not update that
;; variable.
;;
;; Below is a few things that creates a list of packages 'ensured' by
;; use-package and a function to autoremove anything not in that list.
;; This is taken from here:
;; https://github.com/jwiegley/use-package/issues/870#issuecomment-771881305
;;
;; Keep in mind, however, that you need to manually call
;; `use-package-autoremove' to actually remove packages. Straight allows you
;; to prevent the loading of any package not in a use-package declaration,
;; which is not possible when using Package since all installed packages are
;; loaded when `package-initialize' is called.

(defvar use-package-selected-packages '(use-package)
  "Packages pulled in by use-package.")

(defun use-package-autoremove ()
  "Autoremove packages not used by use-package."
  (interactive)
  (let ((package-selected-packages use-package-selected-packages))
    (package-autoremove)))

(define-advice use-package-handler/:ensure (:around (fn name-symbol keyword args rest state) select)
  (let ((items (funcall fn name-symbol keyword args rest state)))
    (dolist (ensure args items)
      (let ((package
             (or (and (eq ensure t) (use-package-as-symbol name-symbol))
                 ensure)))
        (when package
          (when (consp package)
            (setq package (car package)))
          (push `(add-to-list 'use-package-selected-packages ',package) items))))))

;; Automatically remove undeclared packages, after loading this file. Packages
;; will remain loaded until restart.
(add-hook 'after-init--hook 'use-package-autoremove)


;;; Replacing Package with Straight

;; Normally, packages are initialized prior to loading a user's init file.
;; If you're using straight instead of package this is an unnecessary step,
;; and you can prevent Emacs from doing this by placing this in early-init.el:
(setq package-enable-at-startup nil)

;; Ensure straight is installed. This is boilerplate from the straight documentation.
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

;; Ensure use-package is installed.
(straight-use-package 'use-package)

;; Install all declared packages. Can be overridden with `:straight nil'.
(setq straight-use-package-by-default t)


;;; Composition mode

(define-minor-mode composition-mode
  "A tiny minor-mode to toggle some settings I like when writing.
This is really just a wrapper around some extant features I toggle on/off
when I'm writing. I've wrapped them in a minor mode to make it easy to
toggle them on/off. It also allows me to define a lighter for the
mode-line."
  :init-value nil
  :lighter " Comp"
  (if composition-mode
      (progn
        (visual-line-mode t)
        (setq-local line-spacing 2)
        (olivetti-mode t)
        (text-scale-increase 1)
        (variable-pitch-mode 1))
    (progn
      (visual-line-mode -1)
      (setq-local line-spacing 0)
      (olivetti-mode -1)
      (text-scale-increase 0)
      (variable-pitch-mode -1)
      ;; This shouldn't be needed, but is:
      (toggle-truncate-lines 1))))


;;; Lisp Alternative to Use-Package

(add-to-list package-selected-packages 'olivetti)

(custom-set-variables
 '(olivetti-body-width 84))

(autoload 'olivetti-mode "olivetti")

(eval-after-load 'olivetti
  '(blackout 'olivetti-mode " Olvti"))


;;; Misc Transients

(transient-define-prefix oht-transient-tabs ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["General -> Tabs"
   [("t" "Tab Bar Mode" tab-bar-mode)
    ("n" "New" tab-bar-new-tab)
    ("k" "Kill" tab-bar-close-tab)
    ("z" "Undo Kill" tab-bar-undo-close-tab)
    ("]" "Next" tab-bar-switch-to-next-tab)
    ("[" "Previous" tab-bar-switch-to-prev-tab)]])

(transient-define-prefix oht-transient-help ()
  "Transient for Helpful commands"
  ["General -> Helpful Commands"
   [("p" "At Point" helpful-at-point)]
   [("c" "Callable" helpful-callable)
    ("f" "Function" helpful-function)
    ("C" "Command" helpful-command)
    ("v" "Variable" helpful-variable)
    ("s" "Symbol" helpful-symbol)
    ("M" "Macro" helpful-macro)
    ("k" "Key" helpful-key)
    ("m" "Mode" helpful-mode)]
   [("u" "Update" helpful-update)
    ("V" "Visit Reference" helpful-visit-reference)
    ("K" "Kill Helpful Buffers" helpful-kill-buffers)]])


;;; Mouse Window Splits

;; s-click to split windows at that exact spot
(global-set-key [s-mouse-1] 'mouse-split-window-horizontally)
(global-set-key [S-s-mouse-1] 'mouse-split-window-vertically)

;; Delete a window with M-s--click
(global-set-key [M-s-mouse-1] 'mouse-delete-window)


;;; Macros

(defmacro use-package-select (name &rest args)
  "Like `use-package', but adding package to package-selected-packages.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(progn
     (add-to-list 'package-selected-packages ',name)
     (use-package ,name
       ,@args)))

(defmacro use-blackout (feature mode &optional replacement)
  "Like `blackout', but adding `with-eval-after-load'.
FEATURE is name of lisp feature, MODE and REPLACEMENT are as in `blackout'."
  (declare (indent defun))
  `(with-eval-after-load ',feature
     (blackout ',mode ,replacement)))

;; https://github.com/amno1/emacs-init-generator/blob/main/generator.org
(defmacro define-keys (mapname &rest body)
  `(dolist (def '(,@body))
     (define-key ,mapname
       (if (vectorp (car def))
           (car def)
         (read-kbd-macro (car def)))
       (cdr def))))

(defmacro global-set-keys (&rest body)
  `(dolist (def '(,@body))
     (global-set-key
       (if (vectorp (car def))
           (car def)
         (read-kbd-macro (car def)))
       (cdr def))))


;;; Facedancer

(custom-set-variables
 '(facedancer-monospace-family "SF Mono")
 '(facedancer-monospace-height 12)
 '(facedancer-variable-family  "SF Pro Text")
 '(facedancer-variable-height  13)
 '(facedancer-mode-line-family "SF Compact Text"))

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


;;; facedancer-mode

;; There are a number of built-in functions for dealing with setting
;; per-buffer fonts, but all of them are built on buffer-face-mode, which
;; works by remapping ONLY the default face to a new value. If you'd like to
;; remap specific faces (for example the variable-pitch face)
;; buffer-face-mode won't cut it. The below approach applies the exact same
;; approach as buffer-face-mode but allows you to target individual faces.

(define-minor-mode facedancer-mode
  "Local minor mode for setting custom fonts per buffer.
To use, create a function which sets the variables locally, then
call that function with a hook, like so:

    (defun my/custom-elfeed-fonts ()
      (setq-local facedancer-monospace-family \"Iosevka\"
                  facedancer-variable-family  \"Inter\")
      (facedancer-mode 'toggle))

    (add-hook 'elfeed-show-mode 'my/custom-elfeed-fonts)"
  :init-value nil
  :lighter " FaceD"
  (if facedancer-mode
      (progn
        (setq-local facedancer-default-remapping
                    (face-remap-add-relative 'default
                                             :family facedancer-monospace-family
                                             :weight facedancer-monospace-weight
                                             :width  facedancer-monospace-width
                                             :height (* 10 facedancer-monospace-height)))
        (setq-local facedancer-fixed-pitch-remapping
                    (face-remap-add-relative 'fixed-pitch
                                             :family facedancer-monospace-family
                                             :weight facedancer-monospace-weight
                                             :width  facedancer-monospace-width
                                             :height (/ (float facedancer-monospace-height)
                                                        (float facedancer-variable-height))))
        (setq-local facedancer-variable-pitch-remapping
                    (face-remap-add-relative 'variable-pitch
                                             :family facedancer-variable-family
                                             :weight facedancer-variable-weight
                                             :width  facedancer-variable-width
                                             :height (/ (float facedancer-variable-height)
                                                        (float facedancer-monospace-height))))
        (force-window-update (current-buffer)))
    (progn
      (face-remap-remove-relative facedancer-default-remapping)
      (face-remap-remove-relative facedancer-fixed-pitch-remapping)
      (face-remap-remove-relative facedancer-variable-pitch-remapping)
      (force-window-update (current-buffer)))))

(defun my/go-fonts ()
  ;; To make this work, now that I've defined all these variables using defcustom,
  ;; you have to make each variable buffer-local. That's why this is disabled.
  ;; https://stackoverflow.com/questions/21917049/how-can-i-set-buffer-local-value-for-a-variable-defined-by-defcustom
  (interactive)
  (setq-local facedancer-monospace-family "Go Mono")
  (facedancer-mode 'toggle))


;;; Transient Keymaps

(defun test/easy-nav ()
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map [?p] 'previous-line)
     (define-key map [?n] 'next-line)
     (message "Easy Nav!")
     map) t))

(defun move-text-transiently ()
    (interactive)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (message "move-text-up/down")
       (define-key map (kbd "p") 'move-text-up)
       (define-key map (kbd "n") 'move-text-down)

(defvar transpose-keymap (make-keymap)
  "Keymap for transposing lines with move-text")

(defun transpose-keymap-eldoc-function ()
  (eldoc-message "Transpose Lines"))

(defun transpose-keymap--activate ()
  (interactive)
  (message "Transpose Lines Activated")
  (add-function :before-until (local 'eldoc-documentation-function)
                #'transpose-keymap-eldoc-function)
  (set-transient-map transpose-keymap t 'transpose-keymap--deactivate))

(defun transpose-keymap--deactivate ()
  (interactive)
  (message "Transpose Lines Deactivated")
  (remove-function (local 'eldoc-documentation-function)
                   #'transpose-keymap-eldoc-function))

(global-set-key (kbd "C-x C-t") 'transpose-keymap--activate)
(define-key transpose-keymap "p" 'move-text-up)
(define-key transpose-keymap "n" 'move-text-down)
       map) t))


;;; Eldoc Message

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



;;; EWW

(setq shr-max-image-proportion 0.5)
(setq shr-width 80)
(setq shr-bullet "• ")
(setq browse-url-browser-function 'eww-browse-url) ; Use EWW as Emacs's browser

(use-package eww
  :custom
  (eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/pdf\\)")
  :init
  (defun eww-mode-setup ()
    "Apply some customization to fonts in eww-mode."
    (facedancer-vadjust-mode)
    (text-scale-increase 1)
    (setq-local line-spacing 2))
  :commands (eww)
  :hook (eww-mode-hook . eww-mode-setup)
  :config
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

  ) ; End "use-package eww"

(with-eval-after-load 'eww
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
      ("M-p" "Previous Bookmark" eww-previous-bookmark)]]))


;;; Elfeed

(add-hook 'elfeed-search-mode-hook 'disable-selected-minor-mode)
(add-hook 'elfeed-show-mode-hook   'disable-selected-minor-mode)
(add-hook 'elfeed-show-mode-hook   'facedancer-vadjust-mode)

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

  (define-keys elfeed-search-mode-map
    "b" 'elfeed-search-browse-url-background
    "*" 'elfeed-search-tag--star
    "8" 'elfeed-search-untag--star
    "o" 'delete-other-windows
    "E" 'elfeed-search:emacs
    "O" 'elfeed-search:other
    "S" 'elfeed-search:star)

  (define-keys elfeed-show-mode-map
    "r" 'elfeed-show-tag--read
    "u" 'elfeed-show-tag--unread
    "*" 'elfeed-show-tag--star
    "8" 'elfeed-show-tag--unstar
    "b" 'elfeed-show-visit-background
    "o" 'delete-other-windows
    "d" 'ytdl-download)

  ) ; End elfeed


;;; iBuffer

(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Org"   (or (mode . org-mode)
                      (mode . org-agenda-mode)))
         ("Dired" (mode . dired-mode))
         ("ELisp" (mode . emacs-lisp-mode))
         ("Help"  (or (name . "\*Help\*")
                      (name . "\*Apropos\*")
                      (name . "\*Info\*"))))))

(defun ibuffer-setup ()
  (ibuffer-switch-to-saved-filter-groups "default"))

(add-hook 'ibuffer-mode-hook 'ibuffer-setup)


;;; Pulse

(defun pulse-line ()
  "Interactive function to pulse the current line."
  (interactive)
  (pulse-momentary-highlight-one-line (point)))

(defadvice other-window (after other-window-pulse activate) (pulse-line))
(defadvice delete-window (after delete-window-pulse activate) (pulse-line))
(defadvice recenter-top-bottom (after recenter-top-bottom-pulse activate) (pulse-line))

(defun ct/yank-pulse-advice (orig-fn &rest args)
  "Pulse line when yanking"
  ;; From https://christiantietze.de/posts/2020/12/emacs-pulse-highlight-yanked-text/
  (let (begin end)
    (setq begin (point))
    (apply orig-fn args)
    (setq end (point))
    (pulse-momentary-highlight-region begin end)))

(advice-add 'yank :around #'ct/yank-pulse-advice)


;;; Remember Mode

(custom-set-variables
 '(remember-data-file (concat oht-orgfiles "remember-notes"))
 '(remember-notes-initial-major-mode 'fundamental-mode)
 '(remember-notes-auto-save-visited-file-name t))

(defun remember-dwim ()
  "If the region is active, capture with region, otherwise just capture."
  (interactive)
  (if (use-region-p)
      (let ((current-prefix-arg 4)) (call-interactively 'remember))
    (remember)))


;;; Outline

(elisp-group globalize-outline-minor-mode
  "outline provides major and minor modes for collapsing sections
of a buffer into an outline-like format. Let's turn that minor
mode into a global minor mode and enable it."
  (define-globalized-minor-mode global-outline-minor-mode
    outline-minor-mode outline-minor-mode)
  (global-outline-minor-mode +1))


;;; Org

;; Org Agenda pop-up window

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
  (select-window (split-window-below))
  (oht-org-agenda-today)
  (fit-window-to-buffer)
  (use-local-map (copy-keymap org-agenda-mode-map))
  (local-set-key (kbd "x") 'oht-org-agenda-exit-delete-window)
  (local-set-key (kbd "q") 'delete-window))


;;; disabled.el ends here
