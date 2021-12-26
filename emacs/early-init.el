;;; early-init.el --- -*- lexical-binding: t -*-

;; Documentation: (info "(emacs) Early Init File")
;;                (info "(emacs) Package Installation")

;; Unless 'package-enable-at-startup' is set to 'nil' Emacs will automatically
;; "activate" all installed packages after early-init.el but before init.el.
;; To prevent an installed package from loading at startup, customize the
;; `package-load-list' like this:
;; (setq package-load-list '((org-journal nil) all))

;; Set these settings before the GUI frame is created.

(setq tool-bar-mode nil
      menu-bar-mode t)

(scroll-bar-mode 0)

;; Switch off garbage collection (will be switched on later).
;; This is step 1 of 2. Step 2 is in init.
;; Taken from Doom Emacs.
(setq gc-cons-threshold most-positive-fixnum)
