;;; Apperance
;;  --------------------------------------------------------

(set-default 'cursor-type 'bar)

;; Setup functions for my favorite fonts
(defun font-iosevka ()
  (set-face-attribute 'default nil
		      :family "Iosevka" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "Iosevka Sparkle" :height 140 :weight 'normal))

(defun font-fira ()
  (set-face-attribute 'default nil
		      :family "Fira Mono" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "Fira Sans" :height 150 :weight 'normal))

(defun font-roboto ()
  (set-face-attribute 'default nil
		      :family "Roboto Mono" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "Roboto Sans" :height 150 :weight 'normal))

(defun font-sf ()
  (set-face-attribute 'default nil
		      :family "SF Mono" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "New York Medium" :height 160 :weight 'normal))

(defun font-ibm ()
  (set-face-attribute 'default nil
		      :family "IBM Plex Mono" :height 130 :weight 'normal)
  (set-face-attribute 'variable-pitch nil
		      :family "IBM Plex Serif" :height 150 :weight 'normal))

;; Modus theme customizations
(setq modus-operandi-theme-bold-constructs t
      modus-vivendi-theme-bold-constructs t
      modus-operandi-theme-slanted-constructs t
      modus-vivendi-theme-slanted-constructs t)

(load-theme 'modus-operandi t)
(font-ibm)
