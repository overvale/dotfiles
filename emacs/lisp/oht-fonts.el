;;; oht-fonts.el -*- lexical-binding: t -*-


;;; Functions

;;;###autoload
(defun oht-fonts-line-spacing (&optional arg)
  "Buffer local, set the line spacing. Takes an argument, 0 by default"
  (interactive "P")
  (setq-local line-spacing arg))


;;; Variables

(defvar oht-fonts-monospace "Monaco"
  "Monospace font to be used for the default and fixed-pitch faces.")

(defvar oht-fonts-variable "Georgia"
  "Variable font to to used for the variable-pitch face.")

(defvar oht-fonts-monospace-size 12
  "Font size, as an integer, to be used for the default and fixed-pitch sizes.

This value will be multiplied by 10, so 12 will become 120. This is to comply
with Emacs' set-face-attribute requirements.")

(defvar oht-fonts-variable-size 14
  "Font size, as an integer, to be used for the variable-pitch size.

This value will be used to determine a relative (float) size based on the default
size. So if your default size is 12 and your variable size is 14 the derived
size will be 1.16.")


;;; Set fonts and their sizes

(set-face-attribute 'default nil
		    :family oht-fonts-monospace
		    :height (* oht-fonts-monospace-size 10))
(set-face-attribute 'fixed-pitch nil
		    :family oht-fonts-monospace
		    :height 1.0 )
(set-face-attribute 'variable-pitch nil
		    :family oht-fonts-variable :height 1.0)


;;; Larger Variable Pitch Mode

;; A minor mode to scale the variable-pitch face up to the height defined in
;; ‘oht-fonts-variable-size’ and the fixed-pitch face down to the height
;; defined in ‘oht-fonts-monospace-size’. This mode needs to be enabled
;; wherever you want to adjust face sizes, perhaps with a hook.

(make-variable-buffer-local
 (defvar larger-variable-pitch-mode-status nil
   "Status of the larger-variable-pitch-mode"))

(make-variable-buffer-local
 (defvar variable-pitch-remapping nil
   "variable-pitch remapping cookie for larger-variable-pitch-mode."))

(make-variable-buffer-local
 (defvar fixed-pitch-remapping nil
   "fixed-pitch remapping cookie for larger-variable-pitch-mode"))

;;;###autoload
(defun larger-variable-pitch-mode-toggle ()
  (setq larger-variable-pitch-mode-status (not larger-variable-pitch-mode-status))
  (if larger-variable-pitch-mode-status
      (progn
	(setq variable-pitch-remapping
	      (face-remap-add-relative 'variable-pitch
				       :height (/ (float oht-fonts-variable-size)
						  (float oht-fonts-monospace-size))))
	(setq fixed-pitch-remapping 
	      (face-remap-add-relative 'fixed-pitch
				       :height (/ (float oht-fonts-monospace-size)
						  (float oht-fonts-variable-size))))
	(force-window-update (current-buffer)))
    (progn
      (face-remap-remove-relative variable-pitch-remapping)
      (face-remap-remove-relative fixed-pitch-remapping))))

(define-minor-mode larger-variable-pitch-mode
  "Minor mode to scale the variable- and fixed-pitch faces up and down."
  :init-value nil
  :lighter " V+"
  (larger-variable-pitch-mode-toggle))



(provide 'oht-fonts)
