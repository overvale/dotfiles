;;; oht-mac.el -*- lexical-binding: t -*-


;;; Functions

(defun oht-mac-kill-line ()
  "Kill to end of line. This custom function is needed because
binding c-k to kill-line doesn't work due to kill-line being
remapped, so the remapped value is always executed. But calling a
custom function obviates this and allows kill-line to be called
directly. Nil is required."
  (interactive)
  (kill-line nil)
  )

(defun oht-mac-find-settings ()
  "Quickly open init.el"
  (interactive)
  (find-file "~/home/dot/emacs/init.el"))

(defun oht-mac-find-scratch ()
  "Quickly open the scratch buffer"
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (previous-buffer)
    (switch-to-buffer "*scratch*")))

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
  (setq mac-frame-tabbing 'automatic)
  )

(defun oht-mac-show-tab-bar ()
  "Show the tab bar, part of the Mac Port"
  (interactive)
  (mac-set-frame-tab-group-property nil :tab-bar-visible-p t)
  )
(defun oht-mac-hide-tab-bar ()
  "Hide the tab bar, part of the Mac Port"
  (interactive)
  (mac-set-frame-tab-group-property nil :tab-bar-visible-p nil)
  )
(defun oht-mac-show-tab-bar-overview ()
  "Show the tab bar overview, part of the Mac Port"
  (interactive)
  (mac-set-frame-tab-group-property nil :overview-visible-p t)
  )

(defun oht-mac-kill-visual-line-backward ()
  "Kill from the point to beginning of visual line"
  (interactive)
  (set-mark-command nil)
  (beginning-of-visual-line)
  (kill-region (region-beginning) (region-end))
  )

(defun oht-mac-mark-whole-line ()
  "Mark the entirety of the current line."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(defun oht-mac-expand-to-beginning-of-visual-line ()
  "Set mark and move to beginning of visual line"
  (interactive)
  (set-mark-command nil)
  (beginning-of-visual-line)
  )
(defun oht-mac-expand-to-end-of-visual-line ()
  "Set mark and move to end of visual line"
  (interactive)
  (set-mark-command nil)
  (end-of-visual-line)
  )

(defun oht-mac-open-line-below (arg)
  "Open a new indented line below the current one."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun oht-mac-open-line-above (arg)
  "Open a new indented line above the current one."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun oht-mac-kill-line-backward ()
  "Kill from the point to beginning of whole line"
  (interactive)
  (kill-line 0))


(provide 'oht-mac)
