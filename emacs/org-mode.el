;; -*- lexical-binding: t -*-

;;;; Settings

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

;;;; Look & Feel

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

;;;; Source Code Blocks

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)

;;;; Lists

;; Lists may be labelled with letters.
(setq org-list-allow-alphabetical t)

;; This sets the sequence of plain list bullets
;; The syntax is confusing and I don't understand it,
;; but I like the results.
(setq org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+")))

;; Increase sub-item indentation by this amount
;; the default is 2 so the below means 2+2 = 4 (spaces)
(setq org-list-indent-offset 2)

;;;; Custom Agendas

;; This defines custom agendas. There's a very good tutorial on how to set these up at [[https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html][The Chronicle]].

(setq org-agenda-custom-commands
      '(
        ("1" "TODAY: Today's Agenda + Tasks"
         ((agenda "d" ((org-agenda-span 'day)))
          (todo "TODAY|TODO|LATER"
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
(setq org-agenda-todo-ignore-scheduled 'future)

;; If this option is set, the same options will also apply for the tags-todo
;; search, which is the general tags/property matcher restricted to unfinished
;; TODO entries only.
(setq org-agenda-tags-todo-honor-ignore-options t)

;; If you'd like to hide completed tasks from the agenda, even if they're
;; scheduled or have a deadline, here are variables for that.
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;;;; Keywords

(setq org-todo-keywords
      '((sequence "TODAY(T)" "TODO(t)" "LATER(l)" "|" "DONE(d)" "CANCELED(c)")
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

;;;; Capture Templates

(setq org-capture-templates
      '(("d" "Daily Focus" entry
	 (file "~/Documents/org-files/logbook.org")
	 (file "~/dot/emacs/capture-templates/daily-focus.org"))
	("p" "Personal Inbox" entry
         (file+headline "~/Documents/org-files/refile.org" "Personal")
         "* %?\n\n")
        ("P" "Personal Log Entry" entry
         (file "~/Documents/org-files/logbook.org")
         "* %?\n%t\n\n")
        ("i" "Ingenuity Inbox" entry
         (file+headline "~/Documents/org-files/refile.org" "Ingenuity")
         "* %?\n\n")
        ("I" "Ingenuity Log Entry" entry
         (file "~/Documents/org-files/ingenuity_logbook.org")
         "* %? %t\n\n")
	("c" "Ingenuity Cold Call" entry
	 (file "~/Documents/org-files/logbook.org")
	 (file "~/dot/emacs/capture-templates/cold-call.org"))
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

;;;; Agenda Settings

;; This defines which files you want included in your agenda/TODO views.
(setq org-agenda-files
      '("~/Documents/org-files/"
	"~/Documents/writing/kindred/compendium.org"
	))

;; Each type of agenda view can be independently customized. For more info see the documentation for the variable =org-agenda-sorting-strategy=.

;; This is currently disabled because I'm fine with the default behavior. I'm leaving it here as documentation.

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

;;;; Org hydra

(defhydra hydra-org (:color pink :hint nil)
  "
Org                    Links                 Outline
 _q_ quit              _i_ insert            _<_ previous
 _o_ edit              _n_ next              _>_ next
 ^^                    _p_ previous          _a_ all
 ^^                    _s_ store             _g_ go
 ^^                    ^^                    _v_ overview
"
  ("q" nil)
  ("<" org-backward-element)
  (">" org-forward-element)
  ("a" outline-show-all)
  ("g" org-goto :color blue)
  ("i" org-insert-link :color blue)
  ("n" org-next-link)
  ("o" org-edit-special :color blue)
  ("p" org-previous-link)
  ("s" org-store-link)
  ("v" org-overview))

;;;; Org-Agenda Hydra

;; This is beautiful. It is taken from [[https://oremacs.com/2016/04/04/hydra-doc-syntax/][abo-abo]] (creator of hydra). It creates view toggles and displays the status of those toggles.

;; You have to wait until org-agenda loads because org itself
;; doesn't know what 'org-agenda-mode-map' is.
(eval-after-load "org-agenda"
'(progn
	(define-key org-agenda-mode-map
		"v" 'hydra-org-agenda-view/body)
		))

(defun org-agenda-cts ()
  (let ((args (get-text-property
               (min (1- (point-max)) (point))
               'org-last-args)))
    (nth 2 args)))
(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
_w_: ?w? week       _[_: inactive      _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?    _r_: report=?r?
_m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
_y_: ?y? year       _q_: quit          _L__l__c_: ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view
   (if (eq 'day (org-agenda-cts))
       "[x]" "[ ]"))
  ("w" org-agenda-week-view
   (if (eq 'week (org-agenda-cts))
           "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view
       (if (eq 'fortnight (org-agenda-cts))
           "[x]" "[ ]"))
  ("m" org-agenda-month-view
       (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view
       (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode
       (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode
       (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode
       (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode
       (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid
       (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary
       (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("["
   (let ((org-agenda-include-inactive-timestamps t))
     (org-agenda-check-type t 'timeline 'agenda)
     (org-agenda-redo)))
  ("q" (message "Abort") :exit t))

;;;; org imenu

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
