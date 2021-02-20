;;; oht-embark.el --- Customization for the Embark Package -*- lexical-binding: t -*-

;; This setup for Embark is admittedly a little complex at first glance. It is
;; designed to integrate Embark with Selectrum. Additionally, the functions
;; `use-embark-completions' and `use-selectrum-completions' provide everything
;; needed to switch between using Selectrum and Embark for completions.

;; Show which-key help when you call Embark
(setq embark-action-indicator
      (lambda (map)
	(which-key--show-keymap "Embark" map nil nil 'no-paging)
	#'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)

;; Resize Completions Buffer
(add-hook 'embark-collect-post-revert-hook
	  (defun resize-embark-collect-window (&rest _)
	    (when (memq embark-collect--kind '(:live :completions))
	      (fit-window-to-buffer (get-buffer-window)
				    (floor (frame-height) 2) 1))))

;; Pause Selectrum while using embark-collect-live
(add-hook 'embark-collect-mode-hook
	  (defun pause-selectrum ()
	    (when (eq embark-collect--kind :live)
	      (with-selected-window (active-minibuffer-window)
		(shrink-window selectrum-num-candidates-displayed)
		(setq-local selectrum-num-candidates-displayed 0)))))

(defun use-embark-completions ()
  "Use Embark for completions.

These hooks make it so that the embark-live-collect buffer is
automatically displayed after you type something. Keep in mind that this
separate from listing all the potential completions when you press TAB.

That means pressing TAB when there are multiple possible
candidates (or is empty) will result in BOTH the
embark-live-collect and *completions* buffers being shown. When
there is only one candidate, however, TAB will complete."
  (interactive)
  (selectrum-mode -1)
  (add-hook 'minibuffer-setup-hook 'embark-collect-completions-after-input)
  (add-hook 'embark-post-action-hook 'embark-collect--update-linked)
  (add-hook 'embark-collect-mode-hook 'hl-line-mode)
  )

(defun use-selectrum-completions ()
  "Use Selectrum for completions.

This simply removes the hooked added by the function `use-embark-completions'."
  (interactive)
  (selectrum-mode 1)
  (remove-hook 'minibuffer-setup-hook 'embark-collect-completions-after-input)
  (remove-hook 'embark-post-action-hook 'embark-collect--update-linked)
  (remove-hook 'embark-collect-mode-hook 'hl-line-mode)
  )

(provide 'oht-embark)
