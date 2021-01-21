;;; early-init.el --- -*- lexical-binding: t -*-

;; Most of this is stolen from doom-emacs/early-init.el

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(fset #'package--ensure-init-file #'ignore)  ; DEPRECATED Removed in 28

;; I'm not using Doom, but I'm using `straight' for packages, so I can do the
;; same.

;; This file is loaded before the GUI is initialized (see docs). If these
;; settings are applied AFTER the GUI is initialized then you might actually
;; see all these settings happen (window moving around / flashing).

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Frame default parameters
(setq default-frame-alist
   (append (list
          '(width . 80) ; width (in characters)
          '(height . 50) ; height (in characters)
          '(internal-border-width . 0) ; border (in pixels)
	  )))

