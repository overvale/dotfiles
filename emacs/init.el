;; This loads my settings file, leaving the init.el file for interactive customizations

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-library "~/.emacs.d/settings.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7d0de67184bb67d00a45150f050183e8ef8f6277e66ef2da11dbb54b15274cba" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8b30636c9a903a9fa38c7dcf779da0724a37959967b6e4c714fdc3b3fe0b8653" "0153cc571e0e2ebdb93a1702a2a77c81ef6d8d252b64d6b53e85502f66e66a8c" default)))
 '(org-agenda-files (quote ("~/Dropbox_Ingenuity/operations/ingenuity.org")) t)
 '(package-selected-packages
   (quote
    (yasnippet ace-window doom-thmese markdown-mode exec-path-from-shell spacemacs-theme magit evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
