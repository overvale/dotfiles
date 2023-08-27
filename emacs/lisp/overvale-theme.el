;;; Fonts & Theme


;;;; Fonts

;; This makes it so text-scale adjustments operate exactly one point size at a
;; time. The original value is 1.2, which sometimes increases/decreases by
;; more than a single point size. For example: (* 1.2 120) = 144.
;; Whereas (* 1.084 120) = 130.08.
(setq text-scale-mode-step 1.084)

(custom-set-faces
 '(default ((t :family "SF Mono" :height 120)))
 '(fixed-pitch ((t :family "SF Mono")))
 '(mode-line-active ((t :family "SF Compact Text")))
 '(mode-line-inactive ((t :family "SF Compact Text")))
 '(variable-pitch ((t :family "SF Pro Text"))))

;; Enable rendering SF symbols on macOS.
(set-fontset-font t nil "SF Pro Text" nil 'append)


;;;; Theme

(setq custom-safe-themes t)

(defun disable-current-themes nil
  "Disables all currently enabled themes."
  (interactive)
  (if custom-enabled-themes
      (mapcar 'disable-theme custom-enabled-themes)))

(defun load-theme-cleanly (theme)
  "Disable active themes, then load theme."
  (interactive
   (list (intern
          (completing-read "Load Theme: "
                           (mapcar 'symbol-name (custom-available-themes))))))
  (disable-current-themes)
  (load-theme theme t))

(defun macos-dark-p ()
  "Return t if macOS appearance is dark."
  (interactive)
  (string= (shell-command-to-string "defaults read -g AppleInterfaceStyle")
           "Dark\n"))

(load-theme 'standard-light)


;;;; Cursor

(custom-set-variables
 '(cursor-type 'box)
 '(cursor-in-non-selected-windows nil)
 '(blink-cursor-blinks 0)
 '(blink-cursor-interval 0.5)
 '(blink-cursor-delay 0.2))


(provide 'overvale-theme)
