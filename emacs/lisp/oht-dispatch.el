;;; oht-dispatch.el --- -*- lexical-binding: t -*-

;; This lets you pass arbitrary functions to `completing-read' and call the
;; selected candidate.

;; (setq oht-dispatch-functions
;;       '(remember-notes
;; 	elfeed
;; 	org-agenda
;; 	list-bookmarks
;; 	hackernews
;; 	mu4e
;; 	prot-eww-browse-dwim
;; 	oht-dispatch-read-later
;; 	oht-dispatch-NPR-news
;; 	oht-dispatch-CNN-news
;; 	))


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
  "Pass function names to completing-read for calling interactively."
  (interactive)
  (call-interactively
   (intern (completing-read "Call Function: " oht-dispatch-functions))))

(provide 'oht-dispatch)
