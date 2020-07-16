;;; Org Settings
;;  --------------------------------------------------------

;; This adds [COMPLETED: DATE] and logs status changes
(setq org-log-done 'time)

;; This prevents editing inside folded sections
(setq org-catch-invisible-edits 'show-and-error)

;; This sets the sequence of plain list bullets
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

;; Increase sub-item indentation by this amount
;; the default is 2 so the below means 2+2 = 4 (spaces)
(setq org-list-indent-offset 2)

;; this tells org to use the current window for agenda
;; rather than creating a split
(setq org-agenda-window-setup 'current-window)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "WAIT(w)" "|" "CANCELLED(c)")))

(setq org-agenda-custom-commands '(

	("d" "Do Now - Not scheduled, #A, TODO"
         ((agenda "d" ((org-agenda-span 'day)))
          (tags "+PRIORITY={A}/TODO"
                ((org-agenda-overriding-header "Tasks you should do NOW:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                 ))))

	("c" "Complete - Agenda and ALL todos"
         ((agenda "")
          (todo "TODO|WAIT"
           ((org-agenda-overriding-header "Global list of TODO items of type: ALL (non-scheduled)")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
            ))
          ))

        ))

(setq org-agenda-files
      (quote ("~/Documents/org-files/")))

(setq org-refile-targets
      '((org-agenda-files :maxlevel . 3)))
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-capture-templates
      '(("p" "Personal Inbox" entry
	 (file+headline "~/Documents/org-files/refile.org" "Personal")
	 "* %?\n\n")
	("P" "Personal Log Entry" entry
	 (file "~/Documents/org-files/logbook.org")
	 "* %?\n%t\n\n")
	("i" "Ingenuity Inbox" entry
	 (file+headline "~/Documents/org-files/refile.org" "Ingenuity")
	 "* %?\n\n")
	("I" "Ingenuity Log Entry" entry
	 (file "~/Documents/org-files/ing_log.org")
	 "* %^{Log type|Meeting: |Call: } %? %t\n\n")
	))
