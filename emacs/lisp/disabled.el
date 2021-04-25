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
