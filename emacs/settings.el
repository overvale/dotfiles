;; Sane defaults from: https://github.com/rougier/elegant-emacs
;; --------------------------------------------------------

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

;; this turns off the use of tabs when Emacs formats text
;; (setq-default indent-tabs-mode nil)

;; disable pop-wins
;; (setq pop-up-windows nil)

;; General Settings
;; --------------------------------------------------------

(tool-bar-mode 0)
(tooltip-mode  0)
(scroll-bar-mode 0)

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-list-command "list")
(set-default 'cursor-type 'bar)
(global-visual-line-mode t) ;; word-wrap

(add-to-list 'load-path "~/.emacs.d/colors/")

(which-key-mode)
(global-undo-tree-mode)
(counsel-mode)

;; Keybindings
;; --------------------------------------------------------

(defun settings-edit-file ()
  (interactive)
  (find-file "~/dot/emacs/settings.el"))


;; quickly kill current buffer
(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)


;; Quickly switch to scratch buffer with Cmd+0:
(global-set-key (kbd "s-0") (lambda ()
                              (interactive)
                              (if (string= (buffer-name) "*scratch*")
                                  (previous-buffer)
                                (switch-to-buffer "*scratch*"))))

(global-set-key (kbd "s-<right>") 'next-buffer)
(global-set-key (kbd "s-<left>") 'previous-buffer)

;; counsel settings
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper)

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

;; quick shortcuts
(global-set-key (kbd "C-c o") 'other-window)
(global-set-key (kbd "C-c b") 'counsel-switch-buffer)
(global-set-key (kbd "C-c f") 'counsel-fzf)
(global-set-key (kbd "C-c l") 'swiper-all)
(global-set-key (kbd "C-c SPC") 'counsel-M-x)
;; (global-set-key (kbd "") ')

;; Move Lines
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

;; Shortcuts
;; --------------------------------------------------------

;; Set command to super
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)

;; Set option to meta
(setq mac-option-modifier 'meta)
(setq mac-left-option-modifier 'meta)

;; Set right option to option
(setq mac-right-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)

;; Mac save, undo, cut, copy, paste
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-w") 'custom/kill-this-buffer)

;; Apperance
;; --------------------------------------------------------

;; Font settings
;; switch to non-monospace with 'variable-pitch-mode'
(set-face-attribute 'default nil
                    :family "SF Mono" :height 130 :weight 'normal)
(set-face-attribute 'variable-pitch nil
		    :family "New York Medium" :height 140 :weight 'normal)

(load-theme 'gruvbox)

;; Org Mode
;; --------------------------------------------------------

;; When activating org-mode, turn on indent and variable-pitch
(add-hook 'org-mode-hook
  (lambda ()
    ;;(org-indent-mode t)
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
