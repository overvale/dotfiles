;;; disabled.el -*- lexical-binding: t -*-


;; This file contains lisp that I'm not using but don't want to get rid of.


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

;;; Vundo


(use-package vundo
  :straight (:type git :host github :repo "casouri/vundo" :branch "master")
  :commands vundo
  ;; The below is back-ported from Emacs 28, once you upgrade you can safely remove this:
  :config (load (concat oht-dotfiles "lisp/undo-backport.el")))

;; https://old.reddit.com/r/emacs/comments/j0fj7d/what_other_undoxx_packages_exist_besides_undotree/g6tndgw/

;; This code is backported from Emacs 28

(defun undo-redo (&optional arg)
  "Undo the last ARG undos."
  (interactive "*p")
  (cond
   ((not (undo--last-change-was-undo-p buffer-undo-list))
    (user-error "No undo to undo"))
   (t
    (let* ((ul buffer-undo-list)
           (new-ul
            (let ((undo-in-progress t))
              (while (and (consp ul) (eq (car ul) nil))
                (setq ul (cdr ul)))
              (primitive-undo arg ul)))
           (new-pul (undo--last-change-was-undo-p new-ul)))
      (message "Redo%s" (if undo-in-region " in region" ""))
      (setq this-command 'undo)
      (setq pending-undo-list new-pul)
      (setq buffer-undo-list new-ul)))))

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

In Transient Mark mode when the mark is active, undo changes only within
the current region.  Similarly, when not in Transient Mark mode, just \\[universal-argument]
as an argument limits undo to changes within the current region."
  (interactive "*P")
  ;; Make last-command indicate for the next command that this was an undo.
  ;; That way, another undo will undo more.
  ;; If we get to the end of the undo history and get an error,
  ;; another undo command will find the undo history empty
  ;; and will get another error.  To begin undoing the undos,
  ;; you must type some other command.
  (let* ((modified (buffer-modified-p))
     ;; For an indirect buffer, look in the base buffer for the
     ;; auto-save data.
     (base-buffer (or (buffer-base-buffer) (current-buffer)))
     (recent-save (with-current-buffer base-buffer
            (recent-auto-save-p)))
         ;; Allow certain commands to inhibit an immediately following
         ;; undo-in-region.
         (inhibit-region (and (symbolp last-command)
                              (get last-command 'undo-inhibit-region)))
     message)
    ;; If we get an error in undo-start,
    ;; the next command should not be a "consecutive undo".
    ;; So set `this-command' to something other than `undo'.
    (setq this-command 'undo-start)

    (unless (and (eq last-command 'undo)
         (or (eq pending-undo-list t)
             ;; If something (a timer or filter?) changed the buffer
             ;; since the previous command, don't continue the undo seq.
             (undo--last-change-was-undo-p buffer-undo-list)))
      (setq undo-in-region
        (and (or (region-active-p) (and arg (not (numberp arg))))
                 (not inhibit-region)))
      (if undo-in-region
      (undo-start (region-beginning) (region-end))
    (undo-start))
      ;; get rid of initial undo boundary
      (undo-more 1))
    ;; If we got this far, the next command should be a consecutive undo.
    (setq this-command 'undo)
    ;; Check to see whether we're hitting a redo record, and if
    ;; so, ask the user whether she wants to skip the redo/undo pair.
    (let ((equiv (gethash pending-undo-list undo-equiv-table)))
      (or (eq (selected-window) (minibuffer-window))
      (setq message (format "%s%s"
                                (if (or undo-no-redo (not equiv))
                                    "Undo" "Redo")
                                (if undo-in-region " in region" ""))))
      (when (and (consp equiv) undo-no-redo)
    ;; The equiv entry might point to another redo record if we have done
    ;; undo-redo-undo-redo-... so skip to the very last equiv.
    (while (let ((next (gethash equiv undo-equiv-table)))
         (if next (setq equiv next))))
    (setq pending-undo-list equiv)))
    (undo-more
     (if (numberp arg)
     (prefix-numeric-value arg)
       1))
    ;; Record the fact that the just-generated undo records come from an
    ;; undo operation--that is, they are redo records.
    ;; In the ordinary case (not within a region), map the redo
    ;; record to the following undos.
    ;; I don't know how to do that in the undo-in-region case.
    (let ((list buffer-undo-list))
      ;; Strip any leading undo boundaries there might be, like we do
      ;; above when checking.
      (while (eq (car list) nil)
    (setq list (cdr list)))
      (puthash list
               ;; Prevent identity mapping.  This can happen if
               ;; consecutive nils are erroneously in undo list.
               (if (or undo-in-region (eq list pending-undo-list))
                   t
                 pending-undo-list)
           undo-equiv-table))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    (let ((tail buffer-undo-list)
      (prev nil))
      (while (car tail)
    (when (integerp (car tail))
      (let ((pos (car tail)))
        (if prev
        (setcdr prev (cdr tail))
          (setq buffer-undo-list (cdr tail)))
        (setq tail (cdr tail))
        (while (car tail)
          (if (eq pos (car tail))
          (if prev
              (setcdr prev (cdr tail))
            (setq buffer-undo-list (cdr tail)))
        (setq prev tail))
          (setq tail (cdr tail)))
        (setq tail nil)))
    (setq prev tail tail (cdr tail))))
    ;; Record what the current undo list says,
    ;; so the next command can tell if the buffer was modified in between.
    (and modified (not (buffer-modified-p))
     (with-current-buffer base-buffer
       (delete-auto-save-file-if-necessary recent-save)))
    ;; Display a message announcing success.
    (if message
        (message "%s" message))))

(defun undo--last-change-was-undo-p (undo-list)
  (while (and (consp undo-list) (eq (car undo-list) nil))
    (setq undo-list (cdr undo-list)))
  (gethash undo-list undo-equiv-table))

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

(defun resize-embark-collect-window (&rest _)
  (when (memq embark-collect--kind '(:live :completions))
    (fit-window-to-buffer (get-buffer-window)
                          (floor (frame-height) 2) 1)))

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(add-hook 'embark-collect-post-revert-hook 'resize-embark-collect-window)
(add-hook 'minibuffer-setup-hook 'embark-collect-completions-after-input)
(add-hook 'embark-collect-mode-hook 'hl-line-mode)
(define-key minibuffer-local-map (kbd "C-p") 'embark-switch-to-collect-completions)
(define-key embark-collect-mode-map (kbd "C-n") 'switch-to-minibuffer-window)


;;;; PDFs

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
