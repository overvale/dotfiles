;;; oht-dispatch.el --- -*- lexical-binding: t -*-


;; This lets you pass arbitrary functions to `completing-read' and call the
;; selected candidate.


(defun oht-dispatch-read-later ()
  "Open my Read Later directory."
  (interactive)
  (find-file "~/home/files/read later"))

(defun oht-dispatch-NPR-news ()
  "Open text.npr.org"
  (interactive)
  (browse-url "https://text.npr.org"))

(defun oht-dispatch-CNN-news ()
  "Open lite.cnn.com"
  (interactive)
  (browse-url "https://lite.cnn.com/en"))

(defun oht-dispatch-google-news ()
  "Open 68k.news"
  (interactive)
  (browse-url "http://68k.news/"))


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
		oht-dispatch-read-later
		oht-dispatch-NPR-news
		oht-dispatch-CNN-news
        oht-dispatch-google-news))"
  (interactive)
  (call-interactively
   (intern (completing-read "Call Function: " oht-dispatch-functions))))


(provide 'oht-dispatch)
