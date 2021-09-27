;;; outline-cycle-backport.el --- Backport of Emacs 28 functions -*- lexical-binding: t -*-

;; This is a backport of outline-cycle functions from Emacs 28

(defun outline--cycle-state ()
  "Return the cycle state of current heading.
Return either 'hide-all, 'headings-only, or 'show-all."
  (save-excursion
    (let (start end ov-list heading-end)
      (outline-back-to-heading)
      (setq start (point))
      (outline-end-of-heading)
      (setq heading-end (point))
      (outline-end-of-subtree)
      (setq end (point))
      (setq ov-list
            (seq-filter
             (lambda (o)
               (and (eq (overlay-get o 'invisible) 'outline)
                    (save-excursion
                      (goto-char (overlay-start o))
                      (outline-on-heading-p t))))
             (overlays-in start end)))
      (cond ((null ov-list) 'show-all)
            ((and (or (= end (point-max)
                         (1+ (overlay-end (car ov-list))))
                      (= (overlay-end (car ov-list)) end))
                  (= (overlay-start (car ov-list)) heading-end))
             'hide-all)
            (t 'headings-only)))))

(defun outline-has-subheading-p ()
  "Return t if this heading has subheadings, nil otherwise."
  (save-excursion
    (outline-back-to-heading)
    (< (save-excursion (outline-next-heading) (point))
       (save-excursion (outline-end-of-subtree) (point)))))

(defun outline-cycle ()
  "Cycle between `hide all', `headings only' and `show all'.
`Hide all' means hide all subheadings and their bodies.
`Headings only' means show sub headings but not their bodies.
`Show all' means show all subheadings and their bodies."
  (interactive)
  (condition-case nil
      (pcase (outline--cycle-state)
        ('hide-all
         (if (outline-has-subheading-p)
             (progn (outline-show-children)
                    (message "Only headings"))
           (outline-show-subtree)
           (message "Show all")))
        ('headings-only
         (outline-show-subtree)
         (message "Show all"))
        ('show-all
         (outline-hide-subtree)
         (message "Hide all")))
    (outline-before-first-heading nil)))

(defvar-local outline--cycle-buffer-state 'show-all
  "Internal variable used for tracking buffer cycle state.")

(defun outline-cycle-buffer ()
  "Cycle the whole buffer like in `outline-cycle'."
  (interactive)
  (let (has-top-level)
    (save-excursion
      (goto-char (point-min))
      (while (not (or has-top-level (eobp)))
        (when (outline-on-heading-p t)
          (when (= (funcall outline-level) 1)
            (setq has-top-level t)))
        (outline-next-heading)))
    (cond
     ((and (eq outline--cycle-buffer-state 'show-all)
           has-top-level)
      (outline-hide-sublevels 1)
      (setq outline--cycle-buffer-state 'top-level)
      (message "Top level headings"))
     ((or (eq outline--cycle-buffer-state 'show-all)
          (eq outline--cycle-buffer-state 'top-level))
      (outline-show-all)
      (outline-hide-region-body (point-min) (point-max))
      (setq outline--cycle-buffer-state 'all-heading)
      (message "All headings"))
     (t
      (outline-show-all)
      (setq outline--cycle-buffer-state 'show-all)
      (message "Show all")))))

(provide 'outline-cycle-backport)
