;; elfeed-extras.el --- -*- lexical-binding: t -*-

;; Why doesn't this exist in show mode?
(defalias 'elfeed-show-tag--unread (elfeed-expose #'elfeed-show-tag 'unread)
  "Mark the current entry unread.")
(defalias 'elfeed-show-tag--read (elfeed-expose #'elfeed-show-untag 'unread)
  "Mark the current entry read.")

;; Stars in search mode
(defalias 'elfeed-search-tag--star (elfeed-expose #'elfeed-search-tag-all 'star)
  "Add the 'star' tag to all selected entries")
(defalias 'elfeed-search-untag--star (elfeed-expose #'elfeed-search-untag-all 'star)
  "Remove the 'star' tag to all selected entries")

;; Stars in show mode
(defalias 'elfeed-show-tag--star (elfeed-expose #'elfeed-show-tag 'star)
  "Add the 'star' tag to current entry")
(defalias 'elfeed-show-tag--unstar (elfeed-expose #'elfeed-show-untag 'star)
  "Remove the 'star' tag to current entry")

(defun elfeed-search-browse-url-background ()
  "Visit the current entry, or region entries, using `browse-url-macos-background'."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (mapc (lambda (entry)
            (browse-url-macos-background (elfeed-entry-link entry))
            (elfeed-untag entry 'unread)
            (elfeed-search-update-entry entry))
          entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(defun elfeed-show-visit-background ()
  "Visit the current entry in your browser using `browse-url-macos-background'."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (browse-url-macos-background link))))

(defun elfeed-ytdl-download ()
  "Jump to next link (always entry link) and call `ytdl-download'."
  (interactive)
  (shr-next-link)
  (ytdl-download))

(defun elfeed-search-safari-read-later ()
  "Save the current entry, or region entries, to Safari's Reading List."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (mapc (lambda (entry)
            (safari-read-later (elfeed-entry-link entry))
            (elfeed-untag entry 'unread)
            (elfeed-search-update-entry entry))
          entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(defun elfeed-show-safari-read-later ()
  "Save the current elfeed entry to Safari's Reading List."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (safari-read-later link))))

(provide 'elfeed-extras)
