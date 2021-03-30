;;; oht-transient-mark.el --- Mark-related customizations -*- lexical-binding: t -*-

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode

(defun exchange-point-and-mark-dwim ()
  "If a region is active, then leave it activated and swap point and mark.
If no region is active, then stay active and swap."
  (interactive)
  (if (use-region-p)
      (exchange-point-and-mark)
    (exchange-point-and-mark)
    (deactivate-mark nil)))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(provide 'oht-transient-mark)
