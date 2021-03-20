;;; oht-fonts.el -*- lexical-binding: t -*-

;; My Font Settings

;; When using `text-scale-incraese', this sets each 'step' to about one point size.
(setq text-scale-mode-step 1.08)

(defun oht/set-line-spacing (&optional arg)
  "Buffer local, set the line spacing. Takes an argument, 0 by default"
  (interactive "P")
  (setq-local line-spacing arg)
  )

;; Keep in mind that when in variable-pitch-mode the fixed-pitch
;; size is based not on the default size but the variable-pitch
;; size. For this reason I want the height of both the
;; variable-pitch and fixed-pitch fonts to always be 1.0.

;; you can find more faces to customize with M-x list-faces-display

;; default and fixed are usually set to the same thing
(set-face-attribute 'default nil
		    :family "IBM Plex Mono" :height 120)
(set-face-attribute 'fixed-pitch nil
		    :family "IBM Plex Mono" :height 1.0)

(set-face-attribute 'variable-pitch nil
		    :family "IBM Plex Sans" :height 1.0)

;; Enlarge mode-line font
(set-face-attribute 'mode-line nil :height 1.1)
(set-face-attribute 'mode-line-inactive nil :height 1.1)

(setq-default line-spacing nil)



;; RESEARCH AREA
;; ----------------------------------------------

;; I'm experimenting with setting buffer-local fonts.
;; The below code works most of the time but is very fragile,
;; chalk it up to me not knowing how the code actually works.

;; The limitation I'm trying to get around is this:
;; Most variable-width fonts are optically smaller than monospaced ones.
;; So you want to scale the variable-width font, but doing that screws up the
;; fixed-pitch font when you're not in variable-width-mode.

;; To get around this I'm toying with setting buffer-local fonts on mode-hooks
;; or functions so I can control it.

;; 1. Set the default, variable- and fixed-width fonts.
;; 2. Create a variable-pitch-mode hook which sets a new fixed-width size.
;; 3. Create an exit hook to reset it correctly.

(make-variable-buffer-local
 (defvar oht-enlarge-variable-status nil
   "Enlarge Variable Status"))

(defun oht-fonts-enlarge-variable-toggle ()
  "Toggles increase size of variable-width fonts, buffer locally."
  (interactive)
  (setq oht-enlarge-variable-status (not oht-enlarge-variable-status))
  (if oht-enlarge-variable-status
      (progn
	(make-local-variable 'oht-font-variable-cookie)
	(make-local-variable 'oht-font-fixed-cookie)
	(setq-local oht-font-variable-cookie
		    (face-remap-add-relative
		     'variable-pitch :height 1.1))
	(setq-local oht-font-fixed-cookie
		    (face-remap-add-relative
		     'fixed-pitch :height 0.92))
	)
    (progn
      (face-remap-remove-relative oht-font-variable-cookie)
      (face-remap-remove-relative oht-font-fixed-cookie))))

(define-minor-mode larger-variable-fonts-mode
  "A tiny minor-mode to enlarge variable-pitch fonts."
 :init-value nil
 :lighter " ƒ"
 (oht-fonts-enlarge-variable-toggle)
 (make-local-variable 'oht-enlarge-variable-status)
 )


;; You can set a font for an individual frame with `set-frame-font'
;; You can experiment, in the Mac Port, with `mac-font-panel-mode'
;; There's also (buffer-face-set :family "Fira Mono" :height 130)


(provide 'oht-fonts)
