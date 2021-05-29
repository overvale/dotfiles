;;; early-init.el --- -*- lexical-binding: t -*-

;; Documentation: (info "(emacs) Early Init File")

;; Unless 'package-enable-at-startup' is set to 'nil' Emacs will automatically
;; load all installed packages after early-init.el but before init.el To
;; prevent an installed package from loading at startup, customize the
;; `package-load-list' like this:
;; (setq package-load-list '((org-journal nil) all))

;; Set these settings before the GUI frame is created
(custom-set-variables
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(menu-bar-mode t))

;; Frame default parameters
(setq default-frame-alist
   (append (list
          '(width . 80) ; width (in characters)
          '(height . 50) ; height (in characters)
          '(internal-border-width . 0) ; border (in pixels)
      )))
