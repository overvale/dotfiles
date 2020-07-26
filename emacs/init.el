;;; PACKAGES
;; setup package system
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ;;("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ))
(package-initialize)

(require 'org)
(setq vc-follow-symlinks t)
(org-babel-load-file (expand-file-name "~/dot/emacs/emacs-init.org"))

;; init.el ends here
