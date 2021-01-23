;;; oht-eww.el --- Additional config for the eww browser -*- lexical-binding: t -*-

;; The below code allows you to create standard emacs bookmarks in eww-mode.
;; It does this by customizing the `bookmark-make-record-function' variable to
;; a custom function.
;;
;; Adapted from:
;; 1. https://www.emacswiki.org/emacs/bookmark%2B-1.el
;; 2. https://github.com/TotalView/dotemacs/blob/master/.emacs.d/elpa/w3m-20140330.1933/bookmark-w3m.el


;; First, make sure you can get the Title of the page, this will be the name
;; of our bookmark.
(defun oht-eww-current-title ()
  "Returns the title of the current eww buffer"
  (plist-get eww-data :title))

;; This function creates a properly formatted bookmark entry. It defines a
;; 'handler' that's used when visiting the bookmark, defined below.
(defun oht-eww-bookmark-make-record ()
  "Return a bookmark record for the current eww buffer."
  (interactive)
  `(,(oht-eww-current-title)
    (location . ,(eww-current-url))
    (handler . oht-eww-bookmark-handler)))

;; This function simply tells Emacs to use the custom function when using the
;; bookmarking system.
(defun oht-eww-set-bookmark-handler ()
  "This tells Emacs which function to use to create bookmarks."
  (interactive)
  (set (make-local-variable 'bookmark-make-record-function)
       #'oht-eww-bookmark-make-record))

;; This is the handler for when you visit a bookmark.
(defun oht-eww-bookmark-handler (record)
  "Jump to an eww bookmarked location."
  (eww (bookmark-prop-get record 'location)))

;; Finally, add a hook to set the make-record-function
(add-hook 'eww-mode-hook 'oht-eww-set-bookmark-handler)


(provide 'oht-eww)
