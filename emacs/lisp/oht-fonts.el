;;; oht-fonts.el -*- lexical-binding: t -*-


;;; Functions

(defun oht-fonts-line-spacing (&optional arg)
  "Sets line spacing for the current buffer. Takes an argument, 0 by default"
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
  "Font point size, as an integer, to be used for the variable-pitch size.

This value will be used to determine a relative (float) size based on the size
of oht-fonts-monospace. So if your monospace size is 12 and your variable size
is 14 the derived size will be 1.16.")

(defvar oht-fonts-monospace-weight 'normal
  "Weight of both the default and fixed-pitch faces.")

(defvar oht-fonts-variable-weight 'normal
  "Weight of the variable-pitch face.")

;;; Set fonts and their sizes

(defun oht-fonts-set ()
  "Function for setting the default, variable-, and fixed-pitch faces.

All three faces will be set to exactly the same size, with the variable-
and fixed-pitch faces as relative (float) sizes. This allows
text-scale-adjust to work correctly."
  (interactive)
  (set-face-attribute 'default nil
					  :family oht-fonts-monospace
					  :weight oht-fonts-monospace-weight
					  :height (* oht-fonts-monospace-size 10))
  (set-face-attribute 'fixed-pitch nil
					  :family oht-fonts-monospace
					  :weight oht-fonts-monospace-weight
					  :height 1.0)
  (set-face-attribute 'variable-pitch nil
					  :family oht-fonts-variable
					  :weight oht-fonts-variable-weight
					  :height 1.0))


;;; Larger Variable Pitch Mode

(define-minor-mode larger-variable-pitch-mode
  "Minor mode to optically adjust the variable-pitch face size.

A minor mode to scale (in the current buffer) the variable-pitch
face up to the height defined by ‘oht-fonts-variable-size’ and
the fixed-pitch face down to the height defined by
‘oht-fonts-monospace-size’. More info about this implementation
see the elisp info manual under 'Face Remapping'."
  :init-value nil
  :lighter " V+"
  (if larger-variable-pitch-mode
      (progn
		(setq-local variable-pitch-remapping
					(face-remap-add-relative 'variable-pitch
											 :height (/ (float oht-fonts-variable-size)
														(float oht-fonts-monospace-size))))
		(setq-local fixed-pitch-remapping
					(face-remap-add-relative 'fixed-pitch
											 :height (/ (float oht-fonts-monospace-size)
														(float oht-fonts-variable-size))))
		(force-window-update (current-buffer)))
    (progn
      (face-remap-remove-relative variable-pitch-remapping)
      (face-remap-remove-relative fixed-pitch-remapping))))

(defun oht-fonts-buffer-face-setup ()
  "If buffer-face-mode is active, activate larger-variable-pitch-mode, otherwise disable it."
  (if buffer-face-mode
      (larger-variable-pitch-mode 1)
    (larger-variable-pitch-mode -1)))

;; Make this mode the default whenever entering buffer-face-mode
(add-hook 'buffer-face-mode-hook 'oht-fonts-buffer-face-setup)


;; Custom Buffer Fonts Mode

(define-minor-mode oht-fonts-buffer-font-mode
  "Minor mode for setting custom fonts per buffer."
  :init-value nil
  :lighter " BufFont"
  (if oht-fonts-buffer-font-mode
	  (progn
		(setq-local oht-fonts-default-remapping
					(face-remap-add-relative 'default
											 :family oht-fonts-monospace
											 :weight oht-fonts-monospace-weight
											 :height (/ (float oht-fonts-monospace-size)
														(float oht-fonts-variable-size))))
		(setq-local oht-fonts-fixed-pitch-remapping
					(face-remap-add-relative 'fixed-pitch
											 :family oht-fonts-monospace
											 :weight oht-fonts-monospace-weight
											 :height (/ (float oht-fonts-monospace-size)
														(float oht-fonts-variable-size))))
		(setq-local oht-fonts-variable-pitch-remapping
					(face-remap-add-relative 'variable-pitch
											 :family oht-fonts-variable
											 :weight oht-fonts-variable-weight
											 :height (/ (float oht-fonts-variable-size)
														(float oht-fonts-monospace-size))))
		(force-window-update (current-buffer)))
  (progn
    (face-remap-remove-relative oht-fonts-default-remapping)
    (face-remap-remove-relative oht-fonts-variable-pitch-remapping)
    (face-remap-remove-relative oht-fonts-fixed-pitch-remapping)
	(force-window-update (current-buffer)))))

(defun oht-fonts-programming ()
  (interactive)
  (setq-local oht-fonts-monospace "Iosevka")
  (setq-local oht-fonts-variable  "IBM Plex Sans")
  (oht-fonts-buffer-font-mode 'toggle))


(provide 'oht-fonts)
