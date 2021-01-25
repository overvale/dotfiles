;;; oht-elfeed-post.el --- Loaded after Elfeed -*- lexical-binding: t -*-


;;; Display

(defun oht-elfeed-show-fonts ()
  "Apply some customization to fonts in elfeed-show-mode."
  ;; (text-scale-increase 1)
  (setq-local line-spacing 2)
  )


;;; Functions

(defun bjm/elfeed-show-visit-gui ()
  "Wrapper for elfeed-show-visit to use gui browser instead of eww"
  (interactive)
  (let ((browse-url-generic-program "/usr/bin/open"))
    (elfeed-show-visit t)))

;; Elfeed doesn't have a built-in way of flagging or marking items for later,
;; but it does have tags, which you can use for this. The below is a simple
;; alias for adding and removing the 'star' tag. Keep in mind that his only
;; works in the search-mode
(defalias 'elfeed-search-toggle--star
  (elfeed-expose #'elfeed-search-toggle-all 'star)
  "Add the 'star' tag to current entry")

;; Even though there are bindings for marking items as 'read' and 'unread' in
;; the search-mode, there are no such built-in bindings for show-mode. So we
;; add them here.
(defalias 'elfeed-show-tag--unread
  (elfeed-expose #'elfeed-show-tag 'unread)
  "Mark the current entry unread.")
(defalias 'elfeed-show-tag--read
  (elfeed-expose #'elfeed-show-untag 'unread)
  "Mark the current entry read.")


;; Pinboard integration

(defun hrs/elfeed-current-entry ()
  "Return the elfeed entry, in both show and search modes."
  (cond ((eq major-mode 'elfeed-show-mode)
         elfeed-show-entry)
        ((eq major-mode 'elfeed-search-mode)
         (elfeed-search-selected t))))

(defun hrs/elfeed-pinboard-current-entry ()
  "Save the current elfeed entry to pinboard."
  (interactive)
  (let ((url (elfeed-entry-link (hrs/elfeed-current-entry)))
        (title (elfeed-entry-title (hrs/elfeed-current-entry))))
    (pinboard-auth)
    (pinboard-not-too-soon :pinboard-save
			   (pinboard-save url title "" "" t nil))))


(provide 'oht-elfeed-post)
