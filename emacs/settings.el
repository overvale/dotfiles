;; Packages
;; --------------------------------------------------------------------------------

(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("melpa"        . "http://melpa.org/packages/")))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'evil
                          'magit
                          'spacemacs-theme
                          'exec-path-from-shell
			  'markdown-mode
			  'counsel
			  'ace-window
			  'yasnippet
)

;; Settings
;; --------------------------------------------------------------------------------

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-list-command "list")
(global-visual-line-mode t)
(set-default 'cursor-type 'bar)

(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Startup options
(setq inhibit-startup-screen +1)
;;(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time

;; EVIL
(require 'evil)
(global-set-key (kbd "C-c e") 'evil-mode)

;; Using the system $PATH
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Appearance
;; --------------------------------------------------------------------------------

;; Add the themes dir to the load path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; Font settings
(set-face-attribute 'default nil
                    :family "Fira Mono" :height 140 :weight 'normal)
(set-face-attribute 'variable-pitch nil
		    :family "Fira Sans" :height 140 :weight 'normal)
;; switch to non-monospace with 'variable-pitch-mode'

(load-theme 'spacemacs-light t)

;; Org Mode
;; --------------------------------------------------------------------------------

(setq org-agenda-files (list "~/Dropbox/life.org"
			     "~/Dropbox_Ingenuity/operations/ingenuity.org"
			     ))

(setq org-todo-keywords
      '((sequence "TODO" "DELIGATED" "DONE")))


(add-hook 'org-mode-hook
  (lambda ()
    (org-indent-mode t)
    (variable-pitch-mode t)
	)
  t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; IVY
;; --------------------------------------------------------------------------------

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key (kbd "M-p") 'ace-window)


;; Yasnippets
;; --------------------------------------------------------------------------------

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
