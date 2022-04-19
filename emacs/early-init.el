;;; early-init.el --- -*- lexical-binding: t -*-

;; Documentation: (info "(emacs) Early Init File")
;;                (info "(emacs) Package Installation")

;; Unless 'package-enable-at-startup' is set to 'nil' Emacs will automatically
;; "activate" all installed packages after early-init.el but before init.el.

;; Prevent these built-in packages from being activated. Instead I use
;; versions that I manually pull from git repos.
(setq package-load-list '((modus-themes nil)
                          ;; make all others available:
                          all))

;; Set these settings before the GUI frame is created.
(setq tool-bar-mode nil
      menu-bar-mode t)

(scroll-bar-mode 0)

;; Switch off garbage collection (will be switched on later).
;; This is step 1 of 2. Step 2 is in init.
;; Taken from Doom Emacs.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-quickstart t)

 (setq initial-frame-alist
       '((width . 90)
         (height . 55)))

;; end early-init.el


