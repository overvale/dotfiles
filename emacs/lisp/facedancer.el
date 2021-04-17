;;; facedancer.el --- Tools for adjusting faces -*- lexical-binding: t -*-

;; Copyright (C) 2021 Oliver Taylor

;; Author: Oliver Taylor
;; URL: 
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package's goal is to make customizing Emacs' fonts easy.
;; facedancer-mode allows you to set per-buffer fonts.
;; facedancer-vadjust-mode allows optical adjustment of the variable-pitch height.


;;; Variables

(defvar facedancer-monospace "Courier"
  "Monospace font to be used for the default and fixed-pitch faces.")

(defvar facedancer-variable "Georgia"
  "Variable font to to used for the variable-pitch face.")

(defvar facedancer-monospace-size 12
  "Font size, as an integer, to be used for the default and fixed-pitch sizes.

This value will be multiplied by 10, so 12 will become 120. This is to comply
with Emacs' set-face-attribute requirements.")

(defvar facedancer-variable-size 14
  "Font point size, as an integer, to be used for the variable-pitch size.

This value will be used to determine a relative (float) size based on the size
of facedancer-monospace. So if your monospace size is 12 and your variable size
is 14 the derived size will be 1.16.")

(defvar facedancer-monospace-weight 'normal
  "Weight of both the default and fixed-pitch faces.")

(defvar facedancer-variable-weight 'normal
  "Weight of the variable-pitch face.")

(defvar facedancer-monospace-width 'normal
  "Width of both the default and fixed-pitch faces.")

(defvar facedancer-variable-width 'normal
  "Width of the variable-pitch face.")


;;; Functions

(defun facedancer-line-spacing (&optional arg)
  "Interactive wrapper around line-spacing var. Takes an argument, 0 by default."
  (interactive "P")
  (setq-local line-spacing arg))

(defun facedancer-font-set ()
  "Function for setting the default, variable-, and fixed-pitch faces.

All three faces will be set to exactly the same size, with the variable-
and fixed-pitch faces as relative (float) sizes. This allows
text-scale-adjust to work correctly."
  (interactive)
  (set-face-attribute 'default nil
					  :family facedancer-monospace
					  :weight facedancer-monospace-weight
					  :width  facedancer-monospace-width
					  :height (* facedancer-monospace-size 10))
  (set-face-attribute 'fixed-pitch nil
					  :family facedancer-monospace
					  :weight facedancer-monospace-weight
					  :width  facedancer-monospace-width
					  :height 1.0)
  (set-face-attribute 'variable-pitch nil
					  :family facedancer-variable
					  :weight facedancer-variable-weight
					  :width  facedancer-variable-width
					  :height 1.0))


;; Facedancer Mode

(define-minor-mode facedancer-mode
  "Minor mode for setting custom fonts per buffer.

Accepts any of the following variables, all are optional.

VARIABLE                      DEFAULT VALUE
---------------------------   -------------
facedancer-monospace          Monaco
facedancer-variable           Georgia
facedancer-monospace-size     12
facedancer-variable-size      14
facedancer-monospace-weight   normal
facedancer-variable-weight    normal
facedancer-monospace-width    normal
facedancer-variable-width     normal

To use them, create a function which sets the variables locally,
then call that function with a hook, like so:

    (defun my/custom-elfeed-fonts ()
      (setq-local facedancer-monospace \"Iosevka\")
      (setq-local facedancer-variable  \"IBM Plex Sans\")
      (facedancer-mode 'toggle))
    
    (add-hook 'elfeed-show-mode 'my/custom-elfeed-fonts)
"
  :init-value nil
  :lighter " FaceD"
  (if facedancer-mode
	  (progn
		(setq-local facedancer-default-remapping
					(face-remap-add-relative 'default
											 :family facedancer-monospace
											 :weight facedancer-monospace-weight
											 :width  facedancer-monospace-width
											 :height (/ (float facedancer-monospace-size)
														(float facedancer-variable-size))))
		(setq-local facedancer-fixed-pitch-remapping
					(face-remap-add-relative 'fixed-pitch
											 :family facedancer-monospace
											 :weight facedancer-monospace-weight
											 :width  facedancer-monospace-width
											 :height (/ (float facedancer-monospace-size)
														(float facedancer-variable-size))))
		(setq-local facedancer-variable-pitch-remapping
					(face-remap-add-relative 'variable-pitch
											 :family facedancer-variable
											 :weight facedancer-variable-weight
											 :width  facedancer-variable-width
											 :height (/ (float facedancer-variable-size)
														(float facedancer-monospace-size))))
		(force-window-update (current-buffer)))
	(progn
      (face-remap-remove-relative facedancer-default-remapping)
      (face-remap-remove-relative facedancer-fixed-pitch-remapping)
      (face-remap-remove-relative facedancer-variable-pitch-remapping)
	  (force-window-update (current-buffer)))))


;;; Facedancer Variable Adjust Mode

(define-minor-mode facedancer-vadjust-mode
  "Minor mode to adjust the variable-pitch face size buffer-locally.

A minor mode to scale (in the current buffer) the variable-pitch
face up to the height defined by ‘facedancer-variable-size’ and
the fixed-pitch face down to the height defined by
‘facedancer-monospace-size’."
  :init-value nil
  :lighter " V+"
  (if facedancer-vadjust-mode
      (progn
		(setq-local variable-pitch-remapping
					(face-remap-add-relative 'variable-pitch
											 :height (/ (float facedancer-variable-size)
														(float facedancer-monospace-size))))
		(setq-local fixed-pitch-remapping
					(face-remap-add-relative 'fixed-pitch
											 :height (/ (float facedancer-monospace-size)
														(float facedancer-variable-size))))
		(force-window-update (current-buffer)))
	(progn
      (face-remap-remove-relative variable-pitch-remapping)
      (face-remap-remove-relative fixed-pitch-remapping)
	  (force-window-update (current-buffer)))))

(defun facedancer-buffer-face-setup ()
  "If buffer-face-mode is active, activate facedancer-vadjust-mode, otherwise disable it."
  (if buffer-face-mode
      (facedancer-vadjust-mode 1)
    (facedancer-vadjust-mode -1)))

;; Add a hook which enables facedancer-vadjust-mode when buffer-face-mode
;; activates.
(add-hook 'buffer-face-mode-hook 'facedancer-buffer-face-setup)


(provide 'facedancer)
;;; facedancer.el ends here
