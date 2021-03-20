;;; oht-composition.el --- Composition Minor Mode -*- lexical-binding: t -*-

;; This is really just a wrapper around some extant features I toggle on/off
;; when I'm writing. I've wrapped them in a minor mode to make it easy to
;; toggle them on/off. It also allows me to define a lighter for the
;; mode-line.


;; First define a buffer-local variable so we can do the toggle
(make-variable-buffer-local
 (defvar composition-mode-status nil
   "Composition Mode Status"))

;; This function actually toggles the mode, by reading the variable.
;;;###autoload
(defun composition-mode-toggle ()
  (setq composition-mode-status (not composition-mode-status))
  (if composition-mode-status
      (progn
	(visual-line-mode t)
	(setq-local line-spacing 2)
	(olivetti-mode t)
	(text-scale-increase 1)
	(variable-pitch-mode 1)
	(larger-variable-fonts-mode 1)
       )
    (progn
      (visual-line-mode -1)
      (setq-local line-spacing 0)
      (olivetti-mode -1)
      (text-scale-increase 0)
      (variable-pitch-mode -1)
      (larger-variable-fonts-mode -1)
      ;; This shouldn't be needed, but is:
      (toggle-truncate-lines 1)
     ))
  )

;;;###autoload
(define-minor-mode composition-mode
  "A tiny minor-mode to toggle some settings I like when writing."
 :init-value nil
 :lighter " Comp"
 (composition-mode-toggle)
 (make-local-variable 'composition-mode-status)
 )

(provide 'oht-composition)
