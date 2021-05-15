;;; org-extras.el -*- lexical-binding: t -*-

;;; General Settings

(setq org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-adapt-indentation nil
      org-catch-invisible-edits 'show-and-error
      org-outline-path-complete-in-steps nil)

(setq org-refile-targets
      ;; this sets "refile targets" to any headline, level 1-3, in you agenda files.
      '((org-agenda-files :maxlevel . 3)))

;; Look & Feel
(setq org-hide-emphasis-markers t
      org-fontify-quote-and-verse-blocks t
      org-ellipsis "..."
      org-insert-heading-respect-content t)

;; Agenda
(setq org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-start-with-log-mode t)

;; Source Code Blocks
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

;; Lists may be labelled with letters.
(custom-set-variables
 '(org-list-allow-alphabetical t))

;; This sets the sequence of plain list bullets
;; The syntax is confusing and I don't understand it,
;; but I like the results.
(setq org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+")))

;; Increase sub-item indentation by this amount
;; the default is 2 so the below means 2+2 = 4 (spaces)
(setq org-list-indent-offset 2)

;;; Agenda Files & Views

(setq org-agenda-files '("~/home/org/"))

(when (string= (system-name) "shadowfax.local")
  (add-to-list 'org-agenda-files "~/home/writing/kindred/compendium.org"))

(setq org-agenda-custom-commands
      '(("1" "TODAY: Today's Agenda + Priority Tasks"
         ((agenda "d" ((org-agenda-span 'day)))
          (todo "TODO"
                ((org-agenda-sorting-strategy '(todo-state-up))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))))
        ("0" "COMPLETE: Week Agenda + All Tasks"
         ((agenda "w" ((org-agenda-span 'week)))
          (todo "TODO|LATER"
                ((org-agenda-sorting-strategy '(todo-state-up))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))
                )))))

;;;; Functions

;; I've created separate functions to call the org agenda commands,
;; this allows you to bypass the org agenda prompt and go directly to
;; your preferred agenda view.

(defun oht-org-agenda-today ()
  "Call custom agenda command"
  (interactive)
  (org-agenda nil "1"))

(defun oht-org-agenda-complete ()
  "Call custom agenda command"
  (interactive)
  (org-agenda nil "0"))

(defun oht-org-agenda-agenda ()
  "Call custom agenda command"
  (interactive)
  (org-agenda nil "a"))

(defun oht-org-agenda-todos ()
  "Call custom agenda command"
  (interactive)
  (org-agenda nil "t"))

;;;; Settings

(setq org-agenda-use-time-grid nil)

;; Agenda Sorting
(setq org-agenda-sorting-strategy
      '(((agenda habit-down time-up priority-down category-up)
         (todo category-up priority-down)
         (tags priority-down category-keep)
         (search category-keep))))

;; These variables make the global TODO list skip certain entries:
;;    org-agenda-todo-ignore-with-date,
;;    org-agenda-todo-ignore-timestamp,
;;    org-agenda-todo-ignore-scheduled,
;;    org-agenda-todo-ignore-deadlines
(setq org-agenda-todo-ignore-scheduled 'all
      org-agenda-todo-ignore-deadlines 'near
      org-deadline-warning-days 5)

;; If this option is set, the same options will also apply for the tags-todo
;; search, which is the general tags/property matcher restricted to unfinished
;; TODO entries only.
(setq org-agenda-tags-todo-honor-ignore-options t)

;; If you'd like to hide completed tasks from the agenda, even if they're
;; scheduled or have a deadline, here are variables for that.
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t)

;;; Keywords

(setq org-todo-keywords
      '((sequence "TODO(t)" "LATER(l)" "|" "DONE(d)" "CANCELED(c)")))

;;;; Functions

;; Functions for directly setting todo status. I'm doing this here because the
;; pop-ups provided `org-agenda-todo' and `org-todo' are horrifically broken,
;; and I prefer to use a transient commands instead.
(defun org-todo-set-todo ()
  (interactive)
  (org-todo "TODO"))
(defun org-agenda-todo-set-todo ()
  (interactive)
  (org-agenda-todo "TODO"))

(defun org-todo-set-later ()
  (interactive)
  (org-todo "LATER"))
(defun org-agenda-todo-set-later ()
  (interactive)
  (org-agenda-todo "LATER"))

(defun org-todo-set-done ()
  (interactive)
  (org-todo "DONE"))
(defun org-agenda-todo-set-done ()
  (interactive)
  (org-agenda-todo "DONE"))

(defun org-agenda-todo-set-canceled ()
  (interactive)
  (org-agenda-todo "CANCELED"))
(defun org-todo-set-canceled ()
  (interactive)
  (org-todo "CANCELED"))

;;;; Settings

(custom-set-variables
 '(org-enforce-todo-dependencies t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-log-done 'time)
 '(org-log-into-drawer t))

;;; Capture Templates

(setq org-capture-templates
      '(
        ("p" "Personal")
        ("pi" "Personal Inbox" entry
         (file+headline "~/home/org/life.org" "Inbox")
         "* %?\n\n" :empty-lines 1)
        ("pl" "Personal Log Entry" entry
         (file+olp+datetree "~/home/org/logbook.org")
         "* %?\n%T\n\n" :empty-lines 1 :tree-type month )
        ("pp" ".plan" entry
         (file "~/home/org/logbook.org")
         (file "~/home/dot/emacs/capture-templates/plan.org"))

        ("i" "Ingenuity")
        ("ii" "Ingenuity Inbox" entry
         (file+headline "~/home/org/ingenuity.org" "Inbox")
         "* %?\n\n" :empty-lines 1)
        ("il" "Ingenuity Log Entry" entry
         (file "~/home/org/ingenuity_logbook.org")
         "* %? %T\n\n" :empty-lines 1)
        ("ic" "Ingenuity Cold Call" entry
         (file "~/home/org/ingenuity_logbook.org")
         (file "~/home/dot/emacs/capture-templates/cold-call.org"))
        ("if" "Ingenuity Mail Follow Up" entry
         (file+headline "~/home/org/ingenuity.org" "Mail")
         "* TODO %a\n\n  %i" :empty-lines 1)

        ("s" "Scanline")
        ("sl" "Scanline Log Entry" entry
         (file+olp+datetree "~/home/org/scanline_logbook.org")
         "* %^{prompt}\n%T\n\n%?" :empty-lines 1 :tree-type week )

        ("e" "Emacs Config" entry
         (file+headline "~/home/org/emacs.org" "Emacs Config")
         "* TODO %?" :empty-lines 1)))

;;;; Functions

(defun add-newline-at-end-if-none ()
  "Add a newline at the end of the buffer if there isn't any."
  (save-excursion
    (save-restriction
      (goto-char (1- (point-max)))
      (if (not (looking-at "\n\n"))
          (progn
            (goto-char (point-max))
            (insert "\n"))))))

(add-hook 'org-capture-before-finalize-hook 'add-newline-at-end-if-none)

(defun oht/org-insert-date-today ()
  "Insert today's date using standard org formatting."
  (interactive)
  (org-insert-time-stamp (current-time)))

(defun oht/org-insert-date-today-inactive ()
  "Inserts today's date in org inactive format."
  (interactive)
  (insert (format-time-string "\[%Y-%m-%d %a\]")))


;;; Narrow/Widen

(defun narrow-or-widen-dwim (p)
  ;; https://github.com/oantolin/emacs-config/blob/master/my-lisp/narrow-extras.el
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.
With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	    ((and (bound-and-true-p org-src-mode) (not p))
	     (org-edit-src-exit))
	    ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (or (ignore-errors (org-edit-src-code))
             (ignore-errors (org-narrow-to-block))
             (org-narrow-to-subtree)))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
	    ((derived-mode-p 'tex-mode)
	     (TeX-narrow-to-group))
        (t (narrow-to-defun))))


(provide 'org-extras)
