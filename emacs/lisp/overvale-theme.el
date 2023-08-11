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

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-prompts '(bold)
        modus-themes-org-blocks 'gray-background
        modus-themes-headings
        '( (0 . (variable-pitch bold 1.4))
           (1 . (variable-pitch bold 1.3))
           (2 . (variable-pitch bold 1.2))
           (3 . (variable-pitch bold 1.1))
           (t . (1))))

  (setq modus-themes-common-palette-overrides
        '((underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)))

  (setq modus-operandi-palette-overrides
        '((comment red)
          ;; (bg-mode-line-active bg-hl-line)
          (border-mode-line-active border-mode-line-inactive)
          (bg-mode-line-inactive bg-dim)
          (bg-region bg-lavender)
          (fg-region fg-main)
          (cursor "#2f7bff")
          (fringe bg-main)
          (string green-cooler)))

  (setq modus-vivendi-palette-overrides
        '((bg-main bg-dim)
          (cursor blue-intense)
          (bg-mode-line-active bg-cyan-subtle)
          (fg-mode-line-active fg-main)
          (bg-region bg-lavender)
          (fg-region fg-main)
          (comment yellow-cooler)
          (string green-faint)))

  (defun load-modus-theme-match-system nil
    "Load `modus-operandi' or `modus-vivendi' matching the system theme."
    (interactive)
    (if (macos-dark-p)
        (load-theme-cleanly 'modus-vivendi)
      (load-theme-cleanly 'modus-operandi)))

  (load-modus-theme-match-system)

  (add-hook 'mac-effective-appearance-change-hook 'load-modus-theme-match-system))


;;;; Cursor

(custom-set-variables
 '(cursor-type 'bar)
 '(cursor-in-non-selected-windows nil)
 '(blink-cursor-blinks 0)
 '(blink-cursor-interval 0.5)
 '(blink-cursor-delay 0.2))


(provide 'overvale-theme)
