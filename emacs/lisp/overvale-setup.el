;;; Setup

;; I can never seem to actually suppress the Emacs startup echo area message,
;; despite following the documentation carefully, so just redefine the
;; function instead.
(defun display-startup-echo-area-message ()
  (message "Welcome to Emacs!"))

;; Save all interactive customization to a temp file, which is never loaded.
;; This means interactive customization is session-local. Only this init file persists sessions.
(setq custom-file (make-temp-file "emacs-custom-"))

(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

;; For most of my "settings" I use `custom-set-variables', which does a bunch of neat stuff:
;;
;; 1. It calls a variable's "setter" function, if it has one. A perfect
;;    example of why this is needed is `truncate-lines', which simply will not
;;    work unless set this way.
;;
;; 2. It can activate modes as well as set variables, which makes it tidy.
;;
;; 3. It takes care of setting the default for buffer-local variables correctly.
;;
;; https://with-emacs.com/posts/tutorials/almost-all-you-need-to-know-about-variables/#_user_options
;; https://old.reddit.com/r/emacs/comments/exnxha/withemacs_almost_all_you_need_to_know_about/fgadihl/

(custom-set-variables
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(ring-bell-function 'ignore)
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(context-menu-mode 1)
 '(column-number-mode 1)
 '(scroll-step 1)
 '(scroll-margin 1)
 '(set-language-environment "UTF-8")
 '(frame-title-format '("%b"))
 '(uniquify-buffer-name-style 'forward)
 '(vc-follow-symlinks t)
 '(find-file-visit-truename t)
 '(create-lockfiles nil)
 '(make-backup-files nil)
 '(bookmark-save-flag 1)
 '(bookmark-menu-confirm-deletion t)
 '(describe-bindings-outline t)
 '(truncate-lines t)
 '(set-mark-command-repeat-pop t)
 '(mark-even-if-inactive nil)
 '(delete-selection-mode t)
 '(mark-ring-max 512)
 '(save-interprogram-paste-before-kill t)
 '(kill-do-not-save-duplicates t)
 '(kill-ring-max 512)
 '(sentence-end-double-space nil)
 '(tab-always-indent 'complete)
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(fill-column 78)
 '(split-width-threshold 120)
 '(winner-mode t)
 '(split-window-keep-point nil)
 '(even-window-sizes nil)
 '(switch-to-buffer-obey-display-actions t)
 '(help-window-select t)
 '(transient-detect-key-conflicts t)
 '(transient-force-fixed-pitch t)
 '(transient-show-popup t)
 '(delete-by-moving-to-trash t)
 '(trash-directory "~/.Trash"))

;; You can delay activating these modes a moment to save a very tiny bit of
;; startup time.
(defun after-init-hook-setup nil
  (global-auto-revert-mode 1)
  (repeat-mode 1)
  (save-place-mode 1)
  (recentf-mode 1))

(add-hook 'after-init-hook 'after-init-hook-setup)

(provide 'overvale-setup)
