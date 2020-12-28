;;; -*- lexical-binding: t -*-

;;; Agenda Settings

;; This defines which files you want included in your agenda/TODO views.
(setq org-agenda-files
      '("~/Documents/org-files/"
	"~/Documents/writing/kindred/compendium.org"
	))

;; Each type of agenda view can be independently customized. For more info see the documentation for the variable =org-agenda-sorting-strategy=.

;; Agenda Sorting
(setq org-agenda-sorting-strategy
      '(
	((agenda habit-down time-up priority-down category-up)
	 (todo category-up priority-down)
	 (tags priority-down category-keep)
	 (search category-keep))))

;; And here we have some custom commands for the agenda view.

;; You have to wait until org-agenda loads because org itself
;; doesn't know what 'org-agenda-mode-map' is.
(eval-after-load "org-agenda"
'(progn
	(define-key org-agenda-mode-map
		"S" 'org-agenda-schedule)
		))

;;; Settings

;; do not indent text below a headline
(setq org-adapt-indentation nil)

;; I don't like not seeing the stars, since those are markup
(setq org-hide-leading-stars nil)

;; This prevents editing inside folded sections
(setq org-catch-invisible-edits 'show-and-error)

;; this sets "refile targets" to any headline, level 1-3, in you agenda files.
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 3)))
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Make C-a, C-e, and C-k smarter with regard to headline tags.
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)

;; Setup org-goto to send headlines to completion-read
(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
(setq org-outline-path-complete-in-steps nil)

;;; Look & Feel

;; by default, hide org-markup
;; I have a toggle for this defined in functions
(setq org-hide-emphasis-markers t)

;; Style quote and verse blocks
(setq org-fontify-quote-and-verse-blocks t)

;; Character to display at the end of a folded headline
;;(setq org-ellipsis " ⬎")

;; this tells org to use the current window for agenda
;; rather than creating a split
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-restore-windows-after-quit t)

;; If you try to insert a heading in the middle of an entry, don't
;; split it in half, but instead insert the new heading after the
;; end of the current entry.
(setq org-insert-heading-respect-content t)

;; When creating an agenda, make sure hl-line-mode is active
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;;; Source Code Blocks

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)

;;; Lists

;; Lists may be labelled with letters.
(setq org-list-allow-alphabetical t)

;; This sets the sequence of plain list bullets
;; The syntax is confusing and I don't understand it,
;; but I like the results.
(setq org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+")))

;; Increase sub-item indentation by this amount
;; the default is 2 so the below means 2+2 = 4 (spaces)
(setq org-list-indent-offset 2)

;;; Custom Agendas

;; This defines custom agendas. There's a very good tutorial on how to set
;; these up at
;; [[https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html][The
;; Chronicle]].

(setq org-agenda-custom-commands
      '(
        ("1" "TODAY: Today's Agenda + Priority Tasks"
         ((agenda "d" ((org-agenda-span 'day)))
          (todo "TODO"
                ((org-agenda-sorting-strategy '(todo-state-up))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))
                )))
	("0" "COMPLETE: Week Agenda + All Tasks"
         ((agenda "w" ((org-agenda-span 'week)))
          (todo "TODO|LATER"
                ((org-agenda-sorting-strategy '(todo-state-up))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))
                 )))
	))

;; The variables
;;    org-agenda-todo-ignore-with-date,
;;    org-agenda-todo-ignore-timestamp,
;;    org-agenda-todo-ignore-scheduled,
;;    org-agenda-todo-ignore-deadlines
;; make the global TODO list skip certain entries
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-agenda-todo-ignore-deadlines 'near)

;; If this option is set, the same options will also apply for the tags-todo
;; search, which is the general tags/property matcher restricted to unfinished
;; TODO entries only.
(setq org-agenda-tags-todo-honor-ignore-options t)

;; If you'd like to hide completed tasks from the agenda, even if they're
;; scheduled or have a deadline, here are variables for that.
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;;; Keywords

(setq org-todo-keywords
      '((sequence "TODO(t)" "LATER(l)" "|" "DONE(d)" "CANCELED(c)")
        ))

;; Ensure that a task can’t be marked as done if it contains
;; unfinished subtasks or checklist items. This is handy for
;; organizing "blocking" tasks hierarchically.
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;; This adds 'COMPLETED: DATE' when you move something to a DONE state
(setq org-log-done 'time)
;; And record those in a LOGBOOK drawer
(setq org-log-into-drawer t)

;;; Capture Templates

(setq org-capture-templates
     '(("o" "Online" entry
	(file+headline "~/Documents/org-files/watch_read.org" "Online")
	"* Read %?\n\n")
       ("p" "Personal Inbox" entry
        (file+headline "~/Documents/org-files/inbox.org" "Personal")
        "* %?\n\n")
       ("P" "Personal Log Entry" entry
        (file "~/Documents/org-files/logbook.org")
        "* %?\n%t\n\n")
       ("i" "Ingenuity Inbox" entry
        (file+headline "~/Documents/org-files/inbox.org" "Ingenuity")
        "* %?\n\n")
       ("I" "Ingenuity Log Entry" entry
        (file "~/Documents/org-files/ingenuity_logbook.org")
        "* %? %t\n\n")
       ("c" "Ingenuity Cold Call" entry
	(file "~/Documents/org-files/ingenuity_logbook.org")
	(file "~/dot/emacs/capture-templates/cold-call.org"))
       ("@" ".plan" entry
	(file "~/Documents/org-files/logbook.org")
	(file "~/dot/emacs/capture-templates/plan.org"))
       ))

;; Ensure Capture Templates End With Newline

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

;;; org imenu

;; =imenu= normally indexes only two levels - since I run deeply nested documents, go up to six levels.

(setq org-imenu-depth 6)

;; When a document is folded and the user searches and finds with imenu, the body of the folded header is revealed, so that the search result can actually be seen. This differs from how =org-goto= works in 2 ways:

;; - imenu does not display nested headlines, you have to drill down into each.
;; - =org-goto= only jumps to the headline, it doesn't expand it. But it is nested.

(defun ok-imenu-show-entry ()
  "Reveal content of header."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry)
    (org-reveal t))
   ((bound-and-true-p outline-minor-mode)
    (outline-show-entry))))

(add-hook 'imenu-after-jump-hook 'ok-imenu-show-entry)
