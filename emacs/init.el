(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ;;("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ))

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package
(eval-when-compile
  (require 'use-package))

(use-package vc
  :config
  (setq vc-follow-symlinks t)) ; Because my dotfiles are managed that way

;; If the el file is newer than the org file, load that instead of using
;; org-babel, which loads org, which takes a few seconds. This is done to speed
;; startup time.
;;(let* ((conf "~/dot/emacs/emacs-init")
;;       (el (concat conf ".el"))
;;       (org (concat conf ".org")))
;;  (when (or (not (file-exists-p el))
;;            (file-newer-than-file-p org el))
;;    (org-babel-load-file org))
;;      (load-file el))


;; Load org and load your config file
(org-babel-load-file (expand-file-name "~/dot/emacs/emacs-init.org"))

;; ------------------------------------------------------------------
