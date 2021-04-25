;;; undo-backport.el -*- lexical-binding: t -*-

;; https://old.reddit.com/r/emacs/comments/j0fj7d/what_other_undoxx_packages_exist_besides_undotree/g6tndgw/

;; This code is backported from Emacs 28

(defun undo-redo (&optional arg)
  "Undo the last ARG undos."
  (interactive "*p")
  (cond
   ((not (undo--last-change-was-undo-p buffer-undo-list))
    (user-error "No undo to undo"))
   (t
    (let* ((ul buffer-undo-list)
           (new-ul
            (let ((undo-in-progress t))
              (while (and (consp ul) (eq (car ul) nil))
                (setq ul (cdr ul)))
              (primitive-undo arg ul)))
           (new-pul (undo--last-change-was-undo-p new-ul)))
      (message "Redo%s" (if undo-in-region " in region" ""))
      (setq this-command 'undo)
      (setq pending-undo-list new-pul)
      (setq buffer-undo-list new-ul)))))

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

In Transient Mark mode when the mark is active, undo changes only within
the current region.  Similarly, when not in Transient Mark mode, just \\[universal-argument]
as an argument limits undo to changes within the current region."
  (interactive "*P")
  ;; Make last-command indicate for the next command that this was an undo.
  ;; That way, another undo will undo more.
  ;; If we get to the end of the undo history and get an error,
  ;; another undo command will find the undo history empty
  ;; and will get another error.  To begin undoing the undos,
  ;; you must type some other command.
  (let* ((modified (buffer-modified-p))
     ;; For an indirect buffer, look in the base buffer for the
     ;; auto-save data.
     (base-buffer (or (buffer-base-buffer) (current-buffer)))
     (recent-save (with-current-buffer base-buffer
            (recent-auto-save-p)))
         ;; Allow certain commands to inhibit an immediately following
         ;; undo-in-region.
         (inhibit-region (and (symbolp last-command)
                              (get last-command 'undo-inhibit-region)))
     message)
    ;; If we get an error in undo-start,
    ;; the next command should not be a "consecutive undo".
    ;; So set `this-command' to something other than `undo'.
    (setq this-command 'undo-start)

    (unless (and (eq last-command 'undo)
         (or (eq pending-undo-list t)
             ;; If something (a timer or filter?) changed the buffer
             ;; since the previous command, don't continue the undo seq.
             (undo--last-change-was-undo-p buffer-undo-list)))
      (setq undo-in-region
        (and (or (region-active-p) (and arg (not (numberp arg))))
                 (not inhibit-region)))
      (if undo-in-region
      (undo-start (region-beginning) (region-end))
    (undo-start))
      ;; get rid of initial undo boundary
      (undo-more 1))
    ;; If we got this far, the next command should be a consecutive undo.
    (setq this-command 'undo)
    ;; Check to see whether we're hitting a redo record, and if
    ;; so, ask the user whether she wants to skip the redo/undo pair.
    (let ((equiv (gethash pending-undo-list undo-equiv-table)))
      (or (eq (selected-window) (minibuffer-window))
      (setq message (format "%s%s"
                                (if (or undo-no-redo (not equiv))
                                    "Undo" "Redo")
                                (if undo-in-region " in region" ""))))
      (when (and (consp equiv) undo-no-redo)
    ;; The equiv entry might point to another redo record if we have done
    ;; undo-redo-undo-redo-... so skip to the very last equiv.
    (while (let ((next (gethash equiv undo-equiv-table)))
         (if next (setq equiv next))))
    (setq pending-undo-list equiv)))
    (undo-more
     (if (numberp arg)
     (prefix-numeric-value arg)
       1))
    ;; Record the fact that the just-generated undo records come from an
    ;; undo operation--that is, they are redo records.
    ;; In the ordinary case (not within a region), map the redo
    ;; record to the following undos.
    ;; I don't know how to do that in the undo-in-region case.
    (let ((list buffer-undo-list))
      ;; Strip any leading undo boundaries there might be, like we do
      ;; above when checking.
      (while (eq (car list) nil)
    (setq list (cdr list)))
      (puthash list
               ;; Prevent identity mapping.  This can happen if
               ;; consecutive nils are erroneously in undo list.
               (if (or undo-in-region (eq list pending-undo-list))
                   t
                 pending-undo-list)
           undo-equiv-table))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    (let ((tail buffer-undo-list)
      (prev nil))
      (while (car tail)
    (when (integerp (car tail))
      (let ((pos (car tail)))
        (if prev
        (setcdr prev (cdr tail))
          (setq buffer-undo-list (cdr tail)))
        (setq tail (cdr tail))
        (while (car tail)
          (if (eq pos (car tail))
          (if prev
              (setcdr prev (cdr tail))
            (setq buffer-undo-list (cdr tail)))
        (setq prev tail))
          (setq tail (cdr tail)))
        (setq tail nil)))
    (setq prev tail tail (cdr tail))))
    ;; Record what the current undo list says,
    ;; so the next command can tell if the buffer was modified in between.
    (and modified (not (buffer-modified-p))
     (with-current-buffer base-buffer
       (delete-auto-save-file-if-necessary recent-save)))
    ;; Display a message announcing success.
    (if message
        (message "%s" message))))

(defun undo--last-change-was-undo-p (undo-list)
  (while (and (consp undo-list) (eq (car undo-list) nil))
    (setq undo-list (cdr undo-list)))
  (gethash undo-list undo-equiv-table))

(provide 'undo-backport)
