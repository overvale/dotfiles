;; Packages
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; Set some defaults
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq ispell-program-name "/usr/local/bin/ispell")
(global-visual-line-mode t)
(set-default 'cursor-type 'bar)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;(require 'evil)
;;(evil-mode 1)

;; Startup in a useful way
(setq inhibit-startup-screen +1)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; Per mode settings
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook
  (lambda ()
	(org-indent-mode t)
	)
  t)

;; Font settings
(set-face-attribute 'default nil
                    :family "SF Mono" :height 120 :weight 'normal)
(set-face-attribute 'variable-pitch nil
		    :family "Merriweather" :height 120 :weight 'normal)
;; switch to non-monospace with 'variable-pitch-mode'

;; Org Mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
