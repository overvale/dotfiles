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

(require 'evil)
(evil-mode 1)

;; Per mode settings
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook
  (lambda ()
	(org-indent-mode t)
	)
  t)

;; Font settings
(set-face-attribute 'default nil
                    :family "Fira Mono" :height 120 :weight 'normal)
(set-face-attribute 'variable-pitch nil
		    :family "Fira Sans" :height 120 :weight 'normal)
;; switch to non-monospace with 'variable-pitch-mode'

;; Org Mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
