;;; Org

(bind-key* "s-k" 'org-capture)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(defun org-search nil
  "Go to any headline in your refile targets."
  (interactive)
  (org-refile '(4)))

(with-eval-after-load 'org
  (custom-set-variables
   '(org-ellipsis " 􀠩")
   '(org-list-allow-alphabetical t)
   '(org-log-done 'time)
   '(org-log-into-drawer t)
   '(org-special-ctrl-a/e t)
   '(org-special-ctrl-k t)
   '(org-return-follows-link t)
   '(org-catch-invisible-edits 'show-and-error)
   '(org-refile-use-outline-path 'file)
   '(org-outline-path-complete-in-steps nil)
   '(org-goto-interface 'outline-path-completion)
   '(org-goto-max-level 10)
   '(org-refile-targets '((org-agenda-files :maxlevel . 5)))
   '(org-startup-with-inline-images t)
   '(org-image-actual-width '(600))
   '(org-hide-emphasis-markers nil)
   '(org-hide-leading-stars nil)
   '(org-adapt-indentation nil)
   '(org-insert-heading-respect-content t)
   '(org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+")))
   '(org-list-indent-offset 2)
   ;; src blocks
   '(org-link-elisp-confirm-function nil)
   '(org-src-fontify-natively t)
   '(org-fontify-quote-and-verse-blocks t)
   '(org-src-tab-acts-natively t)
   '(org-edit-src-content-indentation 0))

  (transient-define-prefix org-todo-transient ()
    [["Org TODO Status"
      ("t" "TODO"     (lambda () (interactive) (org-todo "TODO")))
      ("w" "WAIT"     (lambda () (interactive) (org-todo "DELG")))
      ("l" "LATER"    (lambda () (interactive) (org-todo "LATER")))
      ("d" "DONE"     (lambda () (interactive) (org-todo "DONE")))
      ("c" "CANCELED" (lambda () (interactive) (org-todo "CANCELED")))
      ("m" "MOVED"    (lambda () (interactive) (org-todo "MOVED")))
      ]])

  (defkey org-mode-map "C-c C-t" 'org-todo-transient)

  (add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("f" . "src fountain"))

  (setq org-todo-keywords
        '((sequence "TODO" "WAIT" "LATER" "|" "DONE" "MOVED" "CANCELED")))

  (setq org-todo-keyword-faces
        '(("WAIT" . org-scheduled-previously)
          ("LATER" . org-scheduled-previously))))


(with-eval-after-load 'org-agenda
  (custom-set-variables
   '(org-agenda-window-setup 'current-window)
   '(org-agenda-restore-windows-after-quit t)
   '(org-agenda-span 'day)
   '(org-agenda-start-with-log-mode nil)
   '(org-agenda-log-mode-items '(closed clock state))
   '(org-agenda-use-time-grid nil)
   '(org-deadline-warning-days 7)
   '(org-agenda-todo-ignore-scheduled nil)
   '(org-agenda-todo-ignore-deadlines nil)
   '(org-agenda-skip-deadline-if-done t)
   '(org-agenda-skip-scheduled-if-done t)
   '(org-agenda-skip-deadline-prewarning-if-scheduled t))

  (add-hook 'org-agenda-mode-hook 'hl-line-mode)

  (transient-define-prefix org-agenda-todo-transient ()
    [["Org TODO Status"
      ("t" "TODO"     (lambda () (interactive) (org-agenda-todo "TODO")))
      ("w" "WAIT"     (lambda () (interactive) (org-agenda-todo "DELG")))
      ("l" "LATER"    (lambda () (interactive) (org-agenda-todo "LATER")))
      ("d" "DONE"     (lambda () (interactive) (org-agenda-todo "DONE")))
      ("c" "CANCELED" (lambda () (interactive) (org-agenda-todo "CANCELED")))
      ("m" "MOVED"    (lambda () (interactive) (org-agenda-todo "MOVED")))
      ]])

  (defkey org-agenda-mode-map "t" 'org-agenda-todo-transient)

  (setq org-agenda-custom-commands
        '(("1" "Agenda + TODO"
           ((agenda 'week)
            (todo "TODO|WAIT"
                  ((org-agenda-sorting-strategy '(todo-state-up priority-down))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                   (org-agenda-overriding-header "Active, not scheduled, Tasks: ")))))))

  (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))

  (defkey org-agenda-mode-map
    "k" 'org-capture
    "K" 'org-agenda-capture
    "S" 'org-agenda-schedule
    "D" 'org-agenda-deadline))

(with-eval-after-load 'calendar
  (defun org-calendar-capture (&optional with-time)
    "Call `org-capture' with the date at point.
With a `C-1' prefix, use the HH:MM value at point, if any, or the
current HH:MM time."
    (interactive "P")
    (if (not (eq major-mode 'calendar-mode))
        (user-error "You cannot do this outside of calendar buffers")
      (progn
        (require 'org)
        (let ((org-overriding-default-time
	           (org-get-cursor-date)))
          (delete-window)
          (call-interactively 'org-capture)))))

  (defkey calendar-mode-map "k" 'org-calendar-capture))

(provide 'overvale-org)
