;; Packages
;; --------------------------------------------------------------------------------
;; Set up packages sources
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("melpa"        . "https://melpa.org/packages/")))

;; Defines a function that will be used to install packages
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

;; Activate installed packages
(package-initialize)

;; On startup, if these packages aren't detected, install them
(ensure-package-installed 'evil
                          'magit
                          'helm
			  'spacemacs-theme
)

;; Settings
;; --------------------------------------------------------------------------------

(require 'evil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Helm Settings
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

;; Set some basic defaults
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq ispell-program-name "/usr/local/bin/ispell")
(global-visual-line-mode t)
(set-default 'cursor-type 'bar)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Startup options
(setq inhibit-startup-screen +1)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

(add-hook 'text-mode-hook 'turn-on-flyspell)

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
			     "~/Dropbox/ingenuity/ingenuity.org"
			     "~/code/notes/projects.org"
			     ))

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




