;;; oht-eww.el --- Additional config for the eww browser -*- lexical-binding: t -*-


;;; Display

(defun oht-eww-fonts ()
  "Apply some customization to fonts in eww-mode."
  (larger-variable-pitch-mode)
  (text-scale-increase 1)
  (setq-local line-spacing 1)
  )

(add-hook 'eww-mode-hook 'oht-eww-fonts)


;; Creates a toggle for showing images
(make-variable-buffer-local
 (defvar eww-inhibit-images-status nil
   "EWW Inhibit Images Status"))

(defun eww-inhibit-images-toggle ()
  (interactive)
  (setq eww-inhibit-images-status (not eww-inhibit-images-status))
  (if eww-inhibit-images-status
      (progn
	(setq-local shr-inhibit-images t)
	(eww-reload t))
    (progn
	(setq-local shr-inhibit-images nil)
	(eww-reload t))))


;;; Bookmarking

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

;; Finally, add a hook to set the make-record-function
(add-hook 'eww-mode-hook 'oht-eww-set-bookmark-handler)


(provide 'oht-eww)
