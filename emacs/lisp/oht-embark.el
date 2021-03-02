;;; oht-embark.el --- Customization for the Embark Package -*- lexical-binding: t -*-

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

(provide 'oht-embark)
