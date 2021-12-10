;;; disabled.el -*- lexical-binding: t -*-


;; This file contains lisp that I'm not using but don't want to get rid of.


;;; Misc

;; FIXME: Stole this technique from https://svn.red-bean.com/repos/kfogel/trunk/.emacs
(mapcar (lambda (k) (define-key global-map (car k) (car (cdr k))))
        (list (list (kbd "s-1") 'switch-to-buffer)
              (list (kbd "s-2") pkg-ops-map)))

(defun find-emacs-dotfiles ()
  "Find lisp files in your Emacs dotfiles directory, pass to completing-read."
  (interactive)
  (find-file (completing-read "Find Elisp Dotfile: "
                              (directory-files-recursively oht-dotfiles "\.el$"))))

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

(defun browse-url-macos-background (url)
  "Open URL with macOS `open'."
  (interactive)
  (start-process "open url"
                 nil "open" "--background" url))


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

(defun backward-kill-line nil
  "Kill backward to the start of line."
  (interactive)
  (kill-line 0))

(defun pop-to-buffer-same-mode (&rest modes)
  "Pop to a buffer with a mode among MODES, or the current one if not given."
  ;; https://jao.io/blog/2021-09-08-high-signal-to-noise-emacs-command.html
  (interactive)
  (let* ((modes (or modes (list major-mode)))
         (pred (lambda (b)
                 (let ((b (get-buffer (if (consp b) (car b) b))))
                   (member (buffer-local-value 'major-mode b) modes)))))
    (pop-to-buffer (read-buffer "Buffer: " nil t pred))))


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


;;; Minibuffer

;; This is what I used, and was quite happy with, until mct.el came along.

(defun select-minibuffer ()
  "Select the active minibuffer."
  (interactive)
  (when-let ((mini (active-minibuffer-window)))
    (select-window mini)))

(defun switch-to-completions-bottom ()
  (interactive)
  (switch-to-completions)
  (move-to-window-line -1))

(let ((minibuffer minibuffer-local-completion-map)
      (list completion-list-mode-map))
  (define-key minibuffer (kbd "C-n") 'switch-to-completions)
  (define-key minibuffer (kbd "C-p") 'switch-to-completions-bottom)
  (define-key list [remap other-window] 'select-minibuffer)
  (define-key list (kbd "n") 'next-completion)
  (define-key list (kbd "p") 'previous-completion))

(prog1 "completion buffer size"
  ;; This is taken from the live-completions README. It limits the height of
  ;; the *Completions* buffer to 1/3 of the frame height.
  (defvar old-max-height-function temp-buffer-max-height)

  (defun max-completions-height (buffer)
    (if (string= (buffer-name buffer) "*Completions*")
        (/ (frame-height) 3) ; 1/3 of the frame-height
      (funcall old-max-height-function temp-buffer-max-height)))

  (setq temp-buffer-max-height #'max-completions-height)
  (temp-buffer-resize-mode))

(local-package 'live-completions "live-completions"
  (require 'live-completions)

  (setq live-completions-columns 'single
        live-completions-sort-order 'cycle)

  (custom-set-faces
   '(live-completions-forceable-candidate ((t (:inherit 'modus-themes-special-mild)))))

  (let ((minibuffer minibuffer-local-completion-map))
    (define-key minibuffer (kbd "TAB") 'minibuffer-force-complete)
    (define-key minibuffer (kbd "<return>") 'minibuffer-force-complete-and-exit))

  (add-hook 'completion-list-mode-hook 'hl-line-mode)

  (live-completions-mode 1))


;;; Prot Minibuffer Adaptation

;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-minibuffer.el

;; This section only does 1 thing: it makes it so that C-n and C-p cycles
;; through the list of completion candidates and the minibuffer.

(defun prot-common-number-negative (n)
  "Make N negative."
  (if (and (numberp n) (> n 0))
      (* -1 n)
    (error "%s is not a valid positive number" n)))

(defun prot-minibuffer-focus-minibuffer ()
  "Focus the active minibuffer."
  (interactive)
  (when-let ((mini (active-minibuffer-window)))
    (select-window mini)))

(defun prot-minibuffer-switch-to-completions-top ()
  "Switch to the top of the completions' buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (switch-to-completions)
  (goto-char (point-min))
  (next-completion 1))

(defun prot-minibuffer-switch-to-completions-bottom ()
  "Switch to the bottom of the completions' buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (switch-to-completions)
  (goto-char (point-max))
  (next-completion -1)
  (goto-char (point-at-bol))
  (recenter
   (- -1
      (min (max 0 scroll-margin)
           (truncate (/ (window-body-height) 4.0))))
   t))

(defun prot-minibuffer-next-completion-or-mini (&optional arg)
  "Move to the next completion or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction it switches to the
minibuffer."
  (interactive "p")
  (if (or (eobp)
          (eq (point-max)
              (save-excursion (forward-line 1) (point))))
      (prot-minibuffer-focus-minibuffer)
    (next-completion (or arg 1)))
  (setq this-command 'next-line))

(defun prot-minibuffer-previous-completion-or-mini (&optional arg)
  "Move to the next completion or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction it switches to the
minibuffer."
  (interactive "p")
  (let ((num (prot-common-number-negative arg)))
    (if (or (bobp)
            (and (save-excursion ; NOTE 2021-07-23: This `and' is for Emacs28 group titles
                   (next-completion -1)
                   (eq (line-number-at-pos) 1))
                 (not
                  (save-excursion
                    (next-completion -1)
                    (get-text-property (point) 'completion--string))))
            (eq (point) (1+ (point-min)))) ; see hack in `prot-minibuffer--clean-completions'
        (prot-minibuffer-focus-minibuffer)
      (next-completion (or num 1)))))

(let ((map minibuffer-local-completion-map))
  (define-key map (kbd "C-n") #'prot-minibuffer-switch-to-completions-top)
  (define-key map (kbd "<down>") #'prot-minibuffer-switch-to-completions-top)
  (define-key map (kbd "C-p") #'prot-minibuffer-switch-to-completions-bottom)
  (define-key map (kbd "<up>") #'prot-minibuffer-switch-to-completions-bottom))

(let ((map completion-list-mode-map))
  (define-key map (kbd "C-n") #'prot-minibuffer-next-completion-or-mini)
  (define-key map (kbd "<down>") #'prot-minibuffer-next-completion-or-mini)
  (define-key map (kbd "C-p") #'prot-minibuffer-previous-completion-or-mini)
  (define-key map (kbd "<up>") #'prot-minibuffer-previous-completion-or-mini))


;;; Embark as Completion Framework

;; The following will allow you to use an embark live collection as your
;; completion framework. I will freely admit that this is ridiculous. I mean,
;; who in their right mind would do all this when they could just use
;; `vertico'? But all of the below allows me to do two important things:
;; completions are not shown at all until I request them with <TAB>, and upon
;; doing that I get a live-updating window (importantly) above the minibuffer.

;; Make the list one line per item:
(setq embark-collect-initial-view-alist '((t . list)))

;; Sort embark candidates
(setq embark-candidate-collectors
      (cl-substitute 'embark-sorted-minibuffer-candidates
                     'embark-minibuffer-candidates
                     embark-candidate-collectors))

(defun switch-to-embark-completions-maybe ()
  "Switch to the completions window, if it exists, or another window."
  (interactive)
  (if (get-buffer-window "*Embark Collect Completions*")
      (select-window (get-buffer-window "*Embark Collect Completions*"))
    (if (get-buffer-window "*Completions*")
        (progn
          (select-window (get-buffer-window "*Completions*"))
          (when (bobp) (next-completion 1)))
      (message "No completions window"))))

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

(defun fit-window-to-buffer-max-40% (&optional window)
  "Resize current window to fit buffer or 40% of the frame height."
  ;; https://github.com/oantolin/emacs-config/blob/5e33f9ec97fdd9d70d2b6204cc754399eaa69662/my-lisp/window-extras.el
  (fit-window-to-buffer
   (or window
       (let ((win (selected-window)))
         (if (window-minibuffer-p win) minibuffer-scroll-window win)))
   (floor (* 0.4 (frame-height))) 1))

(defun quit-window-exit-minibuffer nil
  "Quit the window and then exit the minibuffer.
Designed to be bound in `embark-collect-mode-map' to cleanly exit
the completions window and connected minibuffer."
  (interactive)
  (quit-window)
  (switch-to-minibuffer)
  (minibuffer-keyboard-quit))

(defun embark-minibuffer-completion-help (_start _end)
  "Embark alternative to minibuffer-completion-help.
This means you hit TAB to trigger the completions list.
Source: https://old.reddit.com/r/emacs/comments/nhat3z/modifying_the_current_default_minibuffer/gz5tdeg/"
  (unless embark-collect-linked-buffer
    (embark-collect-completions)))

(defun exit-with-top-completion ()
  "Exit minibuffer with top completion candidate."
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (unless (let (completion-ignore-case)
              (test-completion content
                               minibuffer-completion-table
                               minibuffer-completion-predicate))
      (when-let ((completions (completion-all-sorted-completions)))
        (delete-minibuffer-contents)
        (insert
         (concat
          (substring content 0 (or (cdr (last completions)) 0))
          (car completions)))))
    (exit-minibuffer)))

(defun apply-completion-bindings (map)
  "A common set of bindings to use in completion buffers."
  (define-key map (kbd "n") 'next-completion)
  (define-key map (kbd "p") 'previous-completion)
  (define-key map (kbd "M-v") 'switch-to-minibuffer))

(apply-completion-bindings completion-list-mode-map)

(let ((map embark-collect-mode-map))
  (apply-completion-bindings map)
  (define-key map (kbd "q") 'quit-window-exit-minibuffer)
  (define-key map (kbd "C-g") 'quit-window-exit-minibuffer)
  (define-key map (kbd ".") 'embark-act))

(let ((map minibuffer-local-completion-map))
  (define-key map (kbd "M-v") 'switch-to-embark-completions-maybe)
  (define-key map (kbd "C-n") 'switch-to-embark-completions-maybe)
  (define-key map (kbd "C-p") 'switch-to-embark-completions-maybe))

(define-key minibuffer-local-completion-map (kbd "RET") 'exit-with-top-completion)
(define-key minibuffer-local-must-match-map (kbd "RET") 'exit-with-top-completion)
(define-key minibuffer-local-filename-completion-map (kbd "RET") 'exit-with-top-completion)

;; Show completion candidates after typing <tab>:
(advice-add 'minibuffer-completion-help
            :override #'embark-minibuffer-completion-help)

;; Automatically show candidates after typing
;; (add-hook 'minibuffer-setup-hook 'embark-collect-completions-after-input)

;; resize Embark Collect buffer to fit contents
(add-hook 'embark-collect-post-revert-hook 'fit-window-to-buffer-max-40%)

;; Highlight line in Embark Collect Completions window:
(add-hook 'embark-collect-mode-hook
          (lambda () (interactive)
            (hl-line-mode 1)))


;;; Embark Exit With Top Candidate

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

(defmacro define-key-lambda (map key &rest body)
  `(define-key ,map ,key (lambda () (interactive) ,@body)))

(defmacro define-funkey (map key name &rest body)
  `(define-key ,map ,key (defun ,name nil (interactive) ,@body)))

(define-key-lambda global-map (kbd "s-1")
  (message "works"))

(define-funkey global-map (kbd "s-1") my:test2
  (message "test2"))


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


;;; variable-pitch-text

(defvar variable-pitch-text-height 1.1
  "The height of the `variable-pitch-text' face, must be relative.")

(defun set-variable-pitch-text-height (height)
  "Sets `variable-pitch-text-height' by converting HEIGHT.
Converts an absolute HEIGHT (140) to a relative value (1.1) based
on the default face."
  (setq variable-pitch-text-height (/ height
                                      (float (face-attribute 'default :height)))))

(defface variable-pitch-text
  `((t :inherit variable-pitch
       :height ,(symbol-value 'variable-pitch-text-height)))
  "The proportional face used for longer texts.
This is like the `variable-pitch' face, but is slightly bigger."
  :group 'basic-faces)

(defun variable-pitch-text-mode (&optional arg)
  "Swaps the default face for the variable-pitch-text face.
An interface to `buffer-face-mode' which uses the `variable-pitch-text' face.
Besides the choice of face, it is the same as `buffer-face-mode'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (autoload 'buffer-face-mode-invoke "face-remap")
  (buffer-face-mode-invoke 'variable-pitch-text (or arg t)
			   (called-interactively-p 'interactive)))




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


;;; iSearch

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

(elpa-package 'isearch-mb
  nil
  (isearch-mb-mode)
  (add-to-list 'isearch-mb--after-exit #'occur))


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

(defun echo-area-tooltips ()
  "Show tooltips in the echo area automatically for current buffer."
  (setq-local help-at-pt-display-when-idle t
              help-at-pt-timer-delay 0)
  (help-at-pt-cancel-timer)
  (help-at-pt-set-timer))

(add-hook 'org-mode-hook #'echo-area-tooltips)

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

;;; Startup Time Messages

;; Preamble

;; It is useful to know the impact of your init file on Emacs startup time so
;; you can avoid introducing slowdowns. There are many ways to do it, but this
;; is very simple and does the trick for me.

(defvar before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")

(message "Loading Emacs, pre-init...done (%.3fs)"
         (float-time (time-subtract before-user-init-time
                                    before-init-time)))

(message "Loading %s..." user-init-file)

;; Wrap-up

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

;;; Mode Line

;; Match mode-line border to mode-line background. Huge mode-line anyone?
(set-face-attribute 'mode-line nil :box `(:line-width 3 :color ,(face-attribute 'mode-line :background)))
(set-face-attribute 'mode-line-inactive nil :box `(:line-width 3 :color ,(face-attribute 'mode-line-inactive :background)))

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


;;; Dired Transients

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

(elpa-package 'selected
  ;; A keymap which activates when the region is active.
  ;; One of the more sublime packages I've stumbled across, `selected' creates a
  ;; keymap that is active any time (and only when) the region is active.
  ;; Wonderful for quickly acting on the active region.
  ;; When combined with my navigation keymap the two act like a very lightweight
  ;; vim emulation, but in an entirely emacs-y way.
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

  (defun elfeed-show-visit-background ()
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (message "Sent to browser: %s" link)
        (browse-url-macos-background link))))

  (defun elfeed-ytdl-download ()
    "Jump to next link (always entry link) and call `ytdl-download'."
    (interactive)
    (shr-next-link)
    (ytdl-download))

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
    (define-key map (kbd "d") 'elfeed-ytdl-download))

  ) ; End elfeed


;;; YTDL

(elpa-package 'ytdl
  (setq ytdl-download-folder user-downloads-directory)
  (setq ytdl-media-player "open")
  (setq ytdl-always-query-default-filename 'yes-confirm))


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


;;; disabled.el ends here
