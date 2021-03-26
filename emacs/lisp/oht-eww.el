;;; oht-eww.el --- Additional config for the eww browser -*- lexical-binding: t -*-


;;; Display

(defun oht-eww-fonts ()
  "Apply some customization to fonts in eww-mode."
  (larger-variable-pitch-mode)
  (text-scale-increase 1)
  (setq-local line-spacing 2)
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


;;; Open URL in background buffer

(defun oht-eww-open-in-new-buffer-bury ()
  "Open URL in a new buried buffer"
  (interactive)
  (eww-open-in-new-buffer)
  (quit-window)
  (message "Browsing in buried buffer"))


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


;;; Prot Extensions

;; The following are taken from https://protesilaos.com/dotemacs

;;;###autoload
(defun prot-eww-browse-dwim (url &optional arg)
  "Visit a URL, maybe from `eww-prompt-history', with completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new eww buffer.

If URL does not look like a valid link, run a web query using
`eww-search-prefix'.

When called from an eww buffer, provide the current link as
initial input."
  (interactive
   (list
    (completing-read "Run EWW on: " eww-prompt-history
                     nil nil (plist-get eww-data :url) 'eww-prompt-history)
    current-prefix-arg))
  (eww url (if arg 4 nil)))

;;;###autoload
(defun prot-eww-visit-bookmark (&optional arg)
  "Visit bookmarked URL.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive "P")
  (eww-read-bookmarks t)
  (let ((list (gensym)))
    (dolist (bookmark eww-bookmarks)
      (push (plist-get bookmark :url) list))
    (eww (completing-read "Visit EWW bookmark: " list)
         (if arg 4 nil))))

(autoload 'View-quit "view")

;;;###autoload
(defun prot-eww-find-feed ()
  "Produce Occur buffer with RSS/Atom links from XML source."
  (interactive)
  (eww-view-source)
  (occur "\\(rss\\|atom\\)\\+xml.*href=[\"']\\(.*?\\)[\"']" "\\2")
  (View-quit))

(defvar prot-eww--wikipedia-hist '()
  "Input history for `prot-eww-search-wikipedia'.")

;;;###autoload
(defun prot-eww-search-wikipedia (string &optional arg)
  "Search Wikipedia page matching STRING.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive
   (list (read-string "Search Wikipedia: " nil 'prot-eww--wikipedia-hist)
         current-prefix-arg))
  (eww
   (format "https://en.m.wikipedia.org/w/index.php?search=%s" string)
   (if arg 4 nil))
  (add-to-history 'prot-eww--wikipedia-hist string))

;;;###autoload
(defun prot-eww-bookmark-page (title)
  "Add eww bookmark named with TITLE."
  (interactive
   (list
    (read-string "Set bookmark title: " (plist-get eww-data :title))))
  (plist-put eww-data :title title)
  (eww-add-bookmark))

(defun prot-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

(add-hook 'eww-after-render-hook #'prot-eww--rename-buffer)
(advice-add 'eww-back-url :after #'prot-eww--rename-buffer)
(advice-add 'eww-forward-url :after #'prot-eww--rename-buffer)


(provide 'oht-eww)
