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

(setq tool-bar-mode nil
      menu-bar-mode t)

 (setq initial-frame-alist
       '((width . 90)
         (height . 55)))

;; end early-init.el


