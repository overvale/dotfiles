;;; oht-eww.el --- Additional config for the eww browser -*- lexical-binding: t -*-


;;; Display

(defun oht-eww-fonts ()
  "Apply some customization to fonts in eww-mode."
  (text-scale-increase 2)
  (setq-local line-spacing 3)
  )

(add-hook 'eww-mode-hook 'oht-eww-fonts)


;;; Bookmarking

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

;; Note that eww's default behavior us to bury the buffer when you press q.
;; When you do that, then visit a new url, something about the eww-data :title
;; system doesn't update to the new title. But if you kill the eww buffer
;; instead, it works. So I've bound k to kill-this-buffer in eww-mode.

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
