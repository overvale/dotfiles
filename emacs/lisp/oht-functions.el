;; -*- lexical-binding: t; -*-

(defun oht/kill-buffer-window ()
  "Kill current buffer and window, but not the last window."
  (interactive)
  (kill-buffer (current-buffer))
  (if (not (one-window-p))
          (delete-window))
  )

(defun oht/rotate-window-split ()
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

(defun oht/open-in-bbedit ()
  "Open current file or dir in BBEdit.
Adapted from:
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) (expand-file-name default-directory ) )))
    (message "path is %s" $path)
    (string-equal system-type "darwin")
    (shell-command (format "open -a BBEdit \"%s\"" $path))))


(defun oht/kill-region-or-char ()
  "If there's a region, kill it, if not, kill the next character."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-forward-char 1 nil)))

(defun sanemacs/backward-kill-word ()
  "Kill word backward, without copying to clipboard."
  (interactive "*")
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(defun oht/toggle-line-numbers ()
  "Toggles display of line numbers. Applies to all buffers."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode)))

(defun oht/toggle-whitespace ()
  "Toggles display of indentation and space characters."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))


(defun oht/join-line-next ()
  (interactive)
  (join-line -1))

(defun oht/pipe-region (start end command)
  "Run shell-command-on-region interactivly replacing the region in place"
  (interactive (let (string)
                 (unless (mark)
                   (error "The mark is not set now, so there is no region"))
                 (setq string (read-from-minibuffer "Shell command on region: "
                                                    nil nil nil
                                                    'shell-command-history))
                 (list (region-beginning) (region-end)
                       string)))
  (shell-command-on-region start end command t t))

(defun oht/split-below ()
"Split horizontally and switch to that new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun oht/split-beside ()
"Split vertically and switch to that new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun oht/forward-word-beginning ()
  "Go to the end of the next word."
  (interactive)
  (forward-word 2)
  (backward-word))


(defun oht/org-insert-date-today ()
  "Insert today's date using standard org formatting."
  (interactive)
  (org-insert-time-stamp (current-time))
  )
(defun oht/switch-to-new-buffer ()
  "Create a new buffer named 'Untitled'."
  (interactive)
  (switch-to-buffer "Untitled")
  )
(defun oht/find-scratch ()
  "Switch to the *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*")
  )

(defun all-occur (rexp)
  "Search all buffers for REXP."
  (interactive "MSearch open buffers for regex: ")
  (multi-occur (buffer-list) rexp))

(defun pulse-line (&rest _)
  (interactive)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

;; From https://christiantietze.de/posts/2020/12/emacs-pulse-highlight-yanked-text/
(defun ct/yank-pulse-advice (orig-fn &rest args)
  "Pulse line when yanking"
  ;; Define the variables first
  (let (begin end)
    ;; Initialize `begin` to the current point before pasting
    (setq begin (point))
    ;; Forward to the decorated function (i.e. `yank`)
    (apply orig-fn args)
    ;; Initialize `end` to the current point after pasting
    (setq end (point))
    ;; Pulse to highlight!
    (pulse-momentary-highlight-region begin end)))
(advice-add 'yank :around #'ct/yank-pulse-advice)

(defun oht/other-window ()
  "Wrapper around other-window & pulse line."
  (interactive)
  (other-window 1)
  (pulse-line)
  )

(defun oht/delete-window ()
  "Wrapper around delete-window & pulse line."
  (interactive)
  (delete-window)
  (pulse-line)
  )

(defun oht/recenter-top-bottom ()
  "Wrapper around recenter-top-bottom & pulse line."
  (interactive)
  (recenter-top-bottom)
  (pulse-line)
  )

(defun occur-dwim ()
  "Call `occur' with a sane default.
Taken from oremacs.com. This will offer as the default candidate:
the current region (if it's active), or the current symbol."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(provide 'oht-functions)
