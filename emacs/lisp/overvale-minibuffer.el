;;; Minibuffer

;; I use vertico for completions, but it is useful to set some useful defaults
;; as a fallback.

(custom-set-variables
 '(read-file-name-completion-ignore-case t)
 '(read-buffer-completion-ignore-case t)
 '(completion-ignore-case t)
 '(completions-format 'one-column)
 '(completions-detailed t)
 '(completion-show-help nil)
 '(completion-cycle-threshold nil)
 '(enable-recursive-minibuffers t)
 '(minibuffer-follows-selected-frame nil)
 '(savehist-mode t)
 '(minibuffer-eldef-shorten-default t)
 '(minibuffer-depth-indicate-mode t)
 '(file-name-shadow-mode 1)
 '(minibuffer-depth-indicate-mode 1)
 '(minibuffer-electric-default-mode 1))

(defun switch-to-completions-bottom ()
  "Switch to the *Completions* buffer, at the bottom."
  (interactive)
  (switch-to-completions)
  (move-to-window-line -1))

(bind-keys
 :map minibuffer-local-completion-map
 ("C-n" . switch-to-completions)
 ("C-p" . switch-to-completions-bottom)
 :map completion-list-mode-map
 ([remap other-window] . switch-to-minibuffer)
 ("M-v" . switch-to-minibuffer)
 ("n" . next-completion)
 ("p" . previous-completion))


(provide 'overvale-minibuffer)
