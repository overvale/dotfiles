;;; Buffer Line-Spacing

;; In a manner similar to `text-scale-adjust'.

;; Make this a mode so it can have a lighter like text-scale-adjust.

(defun set-buffer-line-spacing (&optional spacing)
  "Set the buffer's line-spacing to SPACING.
If SPACING is nil, or not supplied, set line-spacing to nil."
  (interactive "P")
  (setq-local line-spacing (if (integerp spacing)
                               spacing
                             nil)))

(defun buffer-line-spacing-increase ()
  "Increase the buffer's line-spacing by 1."
  (interactive)
  (setq-local line-spacing (+ (or line-spacing 0) 1)))

(defun buffer-line-spacing-decrease ()
  "Decrease the buffer's line-spacing by 1."
  (interactive)
  (if (or (null line-spacing)
          (<= line-spacing 0))
      (message "Line-spacing is nil")
    (setq-local line-spacing (- line-spacing 1))))

(keymap-global-set "C-c C-=" 'buffer-line-spacing-increase)
(keymap-global-set "C-c C-+" 'buffer-line-spacing-increase)
(keymap-global-set "C-c C--" 'buffer-line-spacing-decrease)
(keymap-global-set "C-c C-0" 'set-buffer-line-spacing)

(defvar line-spacing-repeat-map
  (let ((map (make-sparse-keymap)))
    (bind-keys
     :map map
      ("="   . buffer-line-spacing-increase)
      ("+"   . buffer-line-spacing-increase)
      ("-"   . buffer-line-spacing-decrease)
      ("0"   . set-buffer-line-spacing)
      ("C-=" . buffer-line-spacing-increase)
      ("C-+" . buffer-line-spacing-increase)
      ("C--" . buffer-line-spacing-decrease)
      ("C-0" . set-buffer-line-spacing))
    map)
  "Keymap to repeat buffer line-spacing commands. Used in `repeat-mode'.")
(put 'buffer-line-spacing-increase 'repeat-map 'line-spacing-repeat-map)
(put 'buffer-line-spacing-decrease 'repeat-map 'line-spacing-repeat-map)
(put 'set-buffer-line-spacing 'repeat-map 'line-spacing-repeat-map)


;;; The Mark


;; https://github.com/oantolin/emacs-config/blob/4eb8a2a4be518f5d528318773062ad9b9296ab56/my-lisp/text-extras.el#L38
(defun mark-line (&optional arg allow-extend)
  "Mark ARG lines starting with the current one. If ARG is negative,
mark -ARG lines ending with the current one.

Interactively (or if ALLOW-EXTEND is non-nil), if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next ARG lines after the ones already marked."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (and transient-mark-mode mark-active)))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (forward-line arg)
         (point)))
    (forward-line arg)
    (unless (= (preceding-char) 10)
      (setq arg (1- arg)))
    (push-mark nil t t)
    (forward-line (- arg))))

(defalias 'mark-sentence 'mark-end-of-sentence)

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  ;; https://stackoverflow.com/a/14539202
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun exchange-point-and-mark-dwim ()
  "Respect region active/inactive and swap point and mark.
If a region is active, then leave it activated and swap point and mark.
If no region is active, then just swap point and mark."
  (interactive)
  (if (use-region-p)
      (exchange-point-and-mark)
    (exchange-point-and-mark)
    (deactivate-mark nil)))

(defun activate-the-mark (&optional arg)
  "Interactive wrapper for `activate-mark'."
  (interactive)
  (activate-mark arg))

(defun deactivate-the-mark (&optional arg)
  "Interactive wrapper for `deactivate-mark'."
  (interactive)
  (deactivate-mark arg))

(defun toggle-the-mark nil
  "Toggles active state of mark.
Does not pass arguments to underlying functions."
  (interactive)
  (if mark-active (deactivate-the-mark) (activate-the-mark)))

;; For some reason these two commands don't act like any other marking
;; commands, they don't activate the mark. I suspect that whoever wrote these
;; commands did so before `transient-mark-mode' was a thing.
(advice-add 'mark-beginning-of-buffer :after 'activate-mark)
(advice-add 'mark-end-of-buffer :after 'activate-mark)


;;; Confirm Killing Modified Buffers

;; Emacs asks for confirmation when killing modified file-visiting buffers,
;; but does not request confirmation for regular buffers.
;;
;; The option `buffer-offer-save' tells Emacs to prompt to you save modified
;; regular buffers when EXITING Emacs, but no such option exists for KILLING
;; regular buffers (as described in the docstring for `buffer-offer-save').
;;
;; The below creates a buffer-local variable and function for
;; `kill-buffer-query-functions' that provides this functionality.

(defvar-local buffer-confirm-kill nil
  "Non-nil means confirm killing buffer when modified.
Variable is checked by `buffer-confirm-kill-p'.")

(defun buffer-confirm-kill-p ()
  "Return nil if buffer is modified and `buffer-confirm-kill' is t.
This function is designed to be called from `kill-buffer-query-functions'."
  (if (and (buffer-modified-p)
           buffer-confirm-kill)
      (yes-or-no-p
       (format "Buffer %S is modified; kill anyway? " (buffer-name)))
    t))

(add-hook 'kill-buffer-query-functions #'buffer-confirm-kill-p)

(defun toggle-buffer-kill-protection nil
  "Sets `buffer-confirm-kill' and `buffer-offer-save' to t."
  (interactive)
  (if buffer-confirm-kill
      (progn
        (setq-local buffer-confirm-kill nil
                    buffer-offer-save nil)
        (message "'buffer-confirm-kill' set to 'nil'"))
    (progn
      (setq-local buffer-confirm-kill t
                  buffer-offer-save t)
      (message "'buffer-confirm-kill' set to 't'"))))


;;; Scratch Buffers

;; It is sometimes useful to quickly create a scratch buffer in markdown- or
;; org-mode (for editing text you're going to paste into a reddit
;; comment/post, for example).

(defvar scratch-markdown-initial-message "<!-- Scratch Buffer for Markdown Mode -->\n\n"
  "Message to be inserted in markdown scratch buffer.")

(defvar scratch-markdown-buffer "*scratch-markdown*"
  "Name of markdown scratch buffer.")

(defun scratch-buffer-markdown ()
  "Create a *scratch* buffer in Markdown Mode and switch to it."
  (interactive)
  (let ((buf scratch-markdown-buffer))
    (if (get-buffer buf)
        (switch-to-buffer buf)
      (progn
        (switch-to-buffer buf)
        (gfm-mode) ; GitHub-Flavored Markdown
        (with-current-buffer buf
          (setq-local buffer-confirm-kill t)
          (setq-local buffer-offer-save t)
          (insert scratch-markdown-initial-message)
          (not-modified))))))

(defvar scratch-org-initial-message "# Scratch Buffer for Org Mode\n\n"
  "Message to be inserted in org scratch buffer.")

(defvar scratch-org-buffer "*scratch-org*"
  "Name of org-mode scratch buffer.")

(defun scratch-buffer-org ()
  "Create a *scratch* buffer in Org Mode and switch to it."
  (interactive)
  (let ((buf scratch-org-buffer))
    (if (get-buffer buf)
        (switch-to-buffer buf)
      (progn
        (switch-to-buffer buf)
        (org-mode)
        (with-current-buffer buf
          (setq-local buffer-confirm-kill t)
          (setq-local buffer-offer-save t)
          (insert scratch-org-initial-message)
          (not-modified))))))

;; Aside from creating markdown and org-mode buffers, sometimes it is nice to
;; just have a quick place to work with text.
(defun new-buffer (name)
  "Create a new buffer, prompting for NAME."
  (interactive
   (list (read-string
          "Create buffer (default \"untitled\"): "
          nil nil "untitled")))
  (let ((buffer (generate-new-buffer name)))
    (switch-to-buffer buffer)
    (text-mode)
    (setq-local buffer-offer-save t)
    (setq-local buffer-confirm-kill t)))


(provide 'overvale-tools)
