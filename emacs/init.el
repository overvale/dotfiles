;; Packages
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; Set some defaults
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq ispell-program-name "/usr/local/bin/ispell")
(global-visual-line-mode t)
(set-default 'cursor-type 'bar)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Per mode settings
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook
  (lambda ()
	(org-indent-mode t)
	)
  t)

;; Font settings
(set-face-attribute 'default nil
                    :family "Essential PragmataPro" :height 130 :weight 'normal)
;; Theme
(load-theme 'plan9 t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (plan9)))
 '(custom-safe-themes
   (quote
    ("1867e0e8f67310ded7db2ac1d70b80c2b75f2c2bf9baca5ee2d5360236f54d3c" "e21d7b25ca2bf460e5b78d5aaf43dcc0b38d955218755df59c4ffd0419983af1" default)))
 '(fci-rule-color "#f8fce8")
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
