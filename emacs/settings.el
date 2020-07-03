;;; Sane defaults from: https://github.com/rougier/elegant-emacs
;;; --------------------------------------------------------

;; garbage collection
(setq gc-cons-threshold (* 100 1024 1024))

;; startup preferences
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; if you visit 2 files named 'makefile' this will name them:
;; foo/makefile & bar/makefile
;; Emacs defualt is makefile<foo/foo> & makefile<bar/bar>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;; General Settings
;;; --------------------------------------------------------

(tool-bar-mode 0)
(tooltip-mode  0)
(scroll-bar-mode 0)
(set-default 'cursor-type 'bar)
(global-visual-line-mode t)                     ; word-wrap
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-list-command "list")
(add-to-list 'load-path "~/.emacs.d/colors/")

(which-key-mode)
(global-undo-tree-mode)
(show-paren-mode)

;; counsel settings
(counsel-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")


;;; Keybindings
;;; --------------------------------------------------------

;; remap C-RET to C-c for faster prefixed keybindings
(global-set-key
 (kbd "C-<return>")
 (lookup-key global-map
	     (kbd "C-c")))

;; replace search with swiper
(global-set-key (kbd "C-s") 'swiper)

;; upgrade to hippe-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Quick Shortcuts
(global-set-key (kbd "C-c o") 'other-window)
(global-set-key (kbd "C-c b") 'counsel-switch-buffer)
(global-set-key (kbd "C-c f") 'counsel-fzf)
(global-set-key (kbd "C-c l") 'swiper-all)
(global-set-key (kbd "C-c x") 'counsel-M-x)

;; quickly open my settings (this) file
(defun settings-edit-file ()
  (interactive)
  (find-file "~/dot/emacs/settings.el"))

;; quickly kill current buffer
(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

;; Quickly switch to scratch buffer with Cmd+0:
(global-set-key
 (kbd "s-0")
 (lambda ()
   (interactive)
   (if (string= (buffer-name) "*scratch*")
       (previous-buffer)
     (switch-to-buffer "*scratch*"))))

;; Move Lines
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)
(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))
(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)


;;; Mac Shortcuts
;;; --------------------------------------------------------

;; Command to super
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)

;; Reft-option to meta for commands
(setq mac-option-modifier 'meta)
(setq mac-left-option-modifier 'meta)

;; Right-option to option, for special characters: ¡™£¢∞§¶•ªº
(setq mac-right-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)

;; Mac-like save, undo, cut, copy, paste, etc.
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-w") 'custom/kill-this-buffer)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)


;;; Apperance
;;; --------------------------------------------------------

;; Font settings
;; switch to non-monospace with 'variable-pitch-mode'
(set-face-attribute 'default nil
                    :family "SF Mono" :height 130 :weight 'light)
(set-face-attribute 'variable-pitch nil
		    :family "New York Medium" :height 140 :weight 'light)

(load-theme 'gruvbox)


;;; Org Mode
;;; --------------------------------------------------------

;; When org-mode, turn on variable-pitch and spelling
(add-hook 'org-mode-hook
  (lambda ()
    (variable-pitch-mode t)
    (flyspell-mode t)
	)
  t)

(global-set-key "\C-ca" 'org-agenda)

(setq org-todo-keywords
  '((sequence "TODO" "DELG" "|" "OMIT" "DONE")))

(setq org-agenda-files
      (quote (
	      "~/Documents/life.org"
	      "~/Documents/life_log.org"
	      "~/Documents/ingenuity/ingenuity.org"
	      "~/Documents/ingenuity/ing_log.org"
	      )))
