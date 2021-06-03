;;; early-init.el --- -*- lexical-binding: t -*-

;; Documentation: (info "(emacs) Early Init File")

;; Unless 'package-enable-at-startup' is set to 'nil' Emacs will automatically
;; "activate" all installed packages after early-init.el but before init.el.
;; To prevent an installed package from loading at startup, customize the
;; `package-load-list' like this:
;; (setq package-load-list '((org-journal nil) all))

;; Set these settings before the GUI frame is created.

(custom-set-variables
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(menu-bar-mode t))

(setq default-frame-alist
      (append (list
               ;; pixels
               '(internal-border-width . 1) ; pixels
               ;; characters
               '(width . 80) '(height . 50))))
