;;; early-init.el --- -*- lexical-binding: t -*-

;; Stolen from doom-emacs/early-init.el
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Do not initialize packages prior to evaluating the user's init file
(setq package-enable-at-startup nil)

;; Precompute package autoloads to speed up startup.
(setq package-quickstart t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; This file is loaded before the GUI is initialized (see docs). If these
;; settings are applied AFTER the GUI is initialized then you might actually
;; see all these settings happen (window moving around / flashing).
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
