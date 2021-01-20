;;; oht-dispatch.el --- -*- lexical-binding: t -*-

;; This lets you pass arbitrary functions to `completing-read' and call the
;; selected candidate.

;; First, define some functions for things (not in emacs) that I do on the
;; computer all the time.
(defun oht-dispatch-hacker-news ()
  "Open news.ycombinator.com in browser."
       (interactive)
       (start-process "open HN" nil "open" "https://news.ycombinator.com"))
(defun oht-dispatch-mail-personal ()
  "Open Mail.app"
       (interactive)
       (start-process "open mail" nil "open" "-a" "Mail"))
(defun oht-dispatch-mail-work ()
  "Open Work Mail."
       (interactive)
       (start-process "open work email" nil "open" "-a" "Mimestream"))
(defun oht-dispatch-slack ()
  "Open Slack in browser."
       (interactive)
       (start-process "open slack" nil "open" "https://ievfx.slack.com"))
(defun oht-dispatch-read-later ()
  "Open my Read Later directory."
       (interactive)
       (find-file "~/Documents/read later"))

;; Next, create a function to pass a list of functions to `completing-read'.
(defun oht-dispatch ()
  "Pass function names to completing-read for calling interactively."
  (interactive)
  (call-interactively
   (intern (completing-read "Choose one: " oht-dispatch-functions))))

(provide 'oht-dispatch)
