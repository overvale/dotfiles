;;; early-init.el --- -*- lexical-binding: t -*-

;; Documentation: (info "(emacs) Early Init File")
;;                (info "(emacs) Package Installation")

;; Unless 'package-enable-at-startup' is set to 'nil' Emacs will automatically
;; "activate" all installed packages after early-init.el but before init.el.

(setq tool-bar-mode nil
      menu-bar-mode t)

 (setq initial-frame-alist
       '((width . 90)
         (height . 55)))

;; The scratch buffer is created before `user-init-file' is evaluated, so if
;; you want `find-file' to start somewhere other than ~ it needs to be here in
;; your early-init file.
(defvar user-home-dir "~/home/")
(setq default-directory user-home-dir)

;; end early-init.el


