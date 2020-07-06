;;; General Settings
;;; --------------------------------------------------------

;; garbage collection
(setq gc-cons-threshold (* 100 1024 1024))

;; startup preferences
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;;; Mac-Like Settings
;; Command to super
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)
;; Left-option to meta for commands
(setq mac-option-modifier 'meta)
(setq mac-left-option-modifier 'meta)
;; Right-option to option, for special characters: ¡™£¢∞§¶•ªº
(setq mac-right-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)

;; if you visit 2 files named 'makefile' this will name them:
;; foo/makefile & bar/makefile
;; Emacs defualt is makefile<foo/foo> & makefile<bar/bar>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq visible-bell 1)
(tool-bar-mode 0)
(set-default 'cursor-type 'bar)
(global-visual-line-mode t) ; word-wrap
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-list-command "list")
(add-to-list 'load-path "~/.emacs.d/colors/")

(which-key-mode t)
(global-undo-tree-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(counsel-mode t)
(require 'expand-region)
(cua-selection-mode t)
(global-auto-revert-mode t)


;;; Apperance
;;; --------------------------------------------------------

;; Font settings
;; switch to non-monospace with 'variable-pitch-mode'
(set-face-attribute 'default nil
                    :family "IBM Plex Mono" :height 130 :weight 'normal)
(set-face-attribute 'variable-pitch nil
		    :family "IBM Plex Serif" :height 160 :weight 'normal)

(load-theme 'gruvbox-light-hard t)


;;; Mode Settings
;;; --------------------------------------------------------

;; counsel settings
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
;(setq ivy-initial-inputs-alist nil) ;removes ^ from initial input

;; When markdown-mode, turn on variable-pitch and spelling
(add-hook 'markdown-mode-hook
  (lambda ()
    (variable-pitch-mode t)
    (flyspell-mode t)
	)
  t)

;; When org-mode, turn on variable-pitch and spelling
(add-hook 'org-mode-hook
  (lambda ()
    (variable-pitch-mode t)
    (flyspell-mode t)
	)
  t)

(setq org-todo-keywords
  '((sequence "TODO" "DELG" "|" "OMIT" "DONE")))

(setq org-agenda-files
      (quote (
	      "~/Documents/life.org"
	      "~/Documents/life_log.org"
	      "~/Documents/ingenuity/ingenuity.org"
	      "~/Documents/ingenuity/ing_log.org"
	      )))



;;; Functions
;;; --------------------------------------------------------

;; quickly open my settings (this) file
(defun settings-edit-file ()
  (interactive)
  (find-file "~/dot/emacs/settings.el"))

;; quickly kill current buffer
(defun custom/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; Quickly switch to scratch buffer with Cmd+0:
(global-set-key (kbd "s-0")
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

(defun mark-whole-line ()
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(defun toggle-window-split ()
  "Toggle between vertical and horizontal split."
  ;; Source: https://www.emacswiki.org/emacs/ToggleWindowSplit.
  ;; Author: Jeff Dwork
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun open-in-bbedit ()
  "Open current file or dir in BBEdit.
Adapted from:
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) (expand-file-name default-directory ) )))
    (message "path is %s" $path)
    (string-equal system-type "darwin")
      (shell-command (format "open -a BBEdit \"%s\"" $path))))

;;; Hydra
;;; --------------------------------------------------------

(global-set-key (kbd "s-j")
		(defhydra hydra-vim (:color red)
		  "VIM POWER"
		  ("h" backward-char "left")
		  ("l" forward-char "right")
		  ("j" next-line "next")
		  ("k" previous-line "previous")
		  ("b" backward-word "previous word")
		  ("e" forward-word "next word")
		  ("0" beginning-of-visual-line "start of line")
		  ("$" end-of-visual-line "end of line")
		  ("/" swiper-isearch "search forward")
		  ("?" swiper-isearch-backward "search backward")
		  ("{" backward-paragraph "back paragraph")
		  ("}" forward-paragraph "forward paragraph")
		  ("(" backward-sentence "back sentence")
		  (")" forward-sentence "forward sentence")
		  ("q" nil "cancel" :color blue)))


;;; Keybindings
;;; --------------------------------------------------------

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "s-/") 'comment-line)

(global-set-key (kbd "s-k o") 'other-window)
(global-set-key (kbd "s-k v") 'split-window-right)
(global-set-key (kbd "s-k s") 'split-window-below)
(global-set-key (kbd "s-k 1") 'delete-other-windows)
(global-set-key (kbd "s-k 0") 'delete-window)
(global-set-key (kbd "s-k r") 'toggle-window-split)

(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "s-p") 'counsel-M-x)
(global-set-key (kbd "s-a") 'org-agenda)
(global-set-key (kbd "s-b") 'counsel-switch-buffer)
(global-set-key (kbd "s-o") 'counsel-find-file)
(global-set-key (kbd "M-s-o") 'counsel-buffer-or-recentf)
(global-set-key (kbd "s-e") 'er/expand-region)
(global-set-key (kbd "s-m") 'magit-status)
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "M-s-f") 'swiper-all)
(global-set-key (kbd "C-M-s-f") 'counsel-ag)

;; Standard Mac Shortcuts
;; https://support.apple.com/en-us/HT201236
(global-set-key (kbd "s-,") 'settings-edit-file)
(global-set-key (kbd "s-n") 'make-frame-command)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-w") 'custom/kill-this-buffer)
(global-set-key (kbd "s-<up>") (kbd "M-<"))
(global-set-key (kbd "s-<down>") (kbd "M->"))
(global-set-key (kbd "s-<left>") (kbd "C-a"))
(global-set-key (kbd "s-<right>") (kbd "C-e"))
(global-set-key (kbd "s-l") 'mark-whole-line)
(global-set-key (kbd "s-M-l") 'mark-paragraph)
(global-set-key (kbd "s-]") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-[") 'indent-rigidly-left-to-tab-stop)

(global-set-key (kbd "s-;") 'flyspell-buffer)
(global-set-key (kbd "s-'") 'flyspell-goto-next-error)
(global-set-key (kbd "s-\"") 'flyspell-correct-word-before-point)
