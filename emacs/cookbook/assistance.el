;;; assistance.el --- A mode with a pop-up help buffer -*- lexical-binding: t -*-

;; Copyright (C) 2021 Oliver Taylor

;; Author: Oliver Taylor
;; Homepage: https://github.com/olivertaylor/dotfiles


;;; Commentary:

;; This code is an experiment to test how a minor mode can display a
;; help-style buffer to display its keybindings, similar to the way the hydra
;; and transient packages display "hints" windows. This mode does the same
;; thing but via a buffer that can be toggled on and off.


;;; Minor mode

(defvar assist-me-keymap (make-keymap)
  "Keymap for assist-me-mode")

(define-minor-mode assist-me-mode
  "Minor mode for assisting with bindings."
  :init-value nil
  :lighter " Assist"
  :keymap assist-me-keymap)

;; ensures other minor modes don't override it
(add-to-list 'emulation-mode-map-alists
             `((assist-me-mode . ,assist-me-keymap)))

(define-key assist-me-keymap (kbd "n") 'next-line)
(define-key assist-me-keymap (kbd "p") 'previous-line)
(define-key assist-me-keymap (kbd "a") 'beginning-of-line)
(define-key assist-me-keymap (kbd "e") 'end-of-line)
(define-key assist-me-keymap (kbd "h") 'toggle-assist-me-buffer)
(define-key assist-me-keymap (kbd "q") 'assist-me-mode)


;;; The Help buffer

(defvar assist-help-buffer "*Assistance Help*"
  "Name of the assistance help buffer")

(defvar assist-help-text nil
  "Contents of the assistance help buffer")

(setq assist-help-text "* Assistance Mode Cheat Sheet *
n - Next Line       a - Beginning of Line
p - Previous Line   e - End of Line

h - Toggle Help")

;; automatically display help buffer
(add-hook 'assist-me-mode-hook 'toggle-assist-me-buffer)

(defun assist-help-buffer--toggle ()
  "Toggles the display of the *Assistance Help* buffer."
  (interactive)
  (if (get-buffer-window assist-help-buffer)
      (assist-help-buffer--kill)
    (assist-help-buffer--make)))

(defun assist-help-buffer--make ()
  "Creates a help buffer with text from assist-help-text"
  (interactive)
  (with-temp-buffer-window assist-help-buffer nil nil
    (with-current-buffer assist-help-buffer
      (insert assist-help-text)
      (use-local-map (copy-keymap help-mode-map))))
  (fit-window-to-buffer (get-buffer-window assist-help-buffer)))

(defun assist-help-buffer--kill ()
  "If it exists, kills the assist me help buffer"
  (interactive)
  (if (get-buffer-window assist-help-buffer)
      (quit-window t (get-buffer-window assist-help-buffer))
    (message "No help window to kill")))

;; assistance.el ends here
