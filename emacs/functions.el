;; -*- lexical-binding: t; -*-

(defun oht/writing-mode ()
  "Enable variable-pitch, flyspell, and increased line-spacing and margins."
  (interactive)
  (variable-pitch-mode t)
  (flyspell-mode t)
  (setq-local line-spacing 0.15)
  ;; define width of buffer margins
  (setq-local left-margin-width 1)
  (setq-local right-margin-width 1)
  ;;(set-window-buffer nil (current-buffer)) ; Use them now.
  )

(defun oht/fix-variable-org-indent ()
  "Fix for org-indent not hiding markup in org-indent-mode.
from: https://maxjmartin.com/Emacs%20Dotfile.html"
  (interactive)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  )

(defun oht/find-settings ()
  "Quickly open emacs-init.org"
  (interactive)
  (find-file "~/dot/emacs/emacs-init.org"))

(defun oht/counsel-find-org ()
  "Quickly open ~/Documents/org-files/"
  (interactive)
  (counsel-find-file "~/Documents/org-files/"))

(defun oht/kill-this-buffer ()
  "Quickly kill current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun oht/find-scratch ()
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (previous-buffer)
    (switch-to-buffer "*scratch*")))

;; Move Lines
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)
(defun oht/move-line-up ()
  "Move the current line up by 1 line"
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))
(defun oht/move-line-down ()
  "More the current line down by 1 line"
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun oht/mark-whole-line ()
  "Mark the entirety of the current line."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(defun oht/toggle-window-split ()
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

(defun oht/expand-to-beginning-of-visual-line ()
  "Set mark and move to beginning of visual line"
  (interactive)
  (set-mark-command nil)
  (beginning-of-visual-line)
  )
(defun oht/expand-to-end-of-visual-line ()
  "Set mark and move to end of visual line"
  (interactive)
  (set-mark-command nil)
  (end-of-visual-line)
  )

(defun oht/kill-line ()
  "Kill to end of line. This custom function is needed because binding c-k to kill-line doesn't work due to kill-line being remapped, so the remapped value is always executed. But calling a custom function obviates this and allows kill-line to be called directly. Nil is required."
  (interactive)
  (kill-line nil)
  )

(defun oht/kill-line-backward ()
  "Kill from the point to beginning of whole line"
  (interactive)
  (kill-line 0))

(defun oht/kill-visual-line-backward ()
  "Kill from the point to beginning of visual line"
  (interactive)
  (set-mark-command nil)
  (beginning-of-visual-line)
  (kill-region (region-beginning) (region-end))
  )

(defun oht/kill-region-or-char ()
  "If there's a region, kill it, if not, kill the next character."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-forward-char 1 nil)))

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

(defun oht/open-line-below (arg)
  "Open a new indented line below the current one."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun oht/open-line-above (arg)
  "Open a new indented line above the current one."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun oht/join-line-next ()
  (interactive)
  (join-line -1))

(defun oht/org-hide-emphasis-markers ()
  "Toggle whether or not the emphasis markers ~, =, *, _ are displayed"
  (interactive)
  (if (bound-and-true-p org-hide-emphasis-markers)
      (setq-local org-hide-emphasis-markers nil)
    (setq-local org-hide-emphasis-markers t))
  (font-lock-fontify-buffer)
  )

(defun oht/shell-command-on-region-replace (start end command)
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
(defun recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(defun yank-pop+ (&optional arg)
 "Call `yank-pop' with ARG when appropriate, or offer completion."
 (interactive "*P")
 (if arg (yank-pop arg)
   (let* ((old-last-command last-command)
          (selectrum-should-sort-p nil)
          (enable-recursive-minibuffers t)
          (text (completing-read
                 "Yank: "
                 (cl-remove-duplicates
                  kill-ring :test #'string= :from-end t)
                 nil t nil nil))
          ;; Find `text' in `kill-ring'.
          (pos (cl-position text kill-ring :test #'string=))
          ;; Translate relative to `kill-ring-yank-pointer'.
          (n (+ pos (length kill-ring-yank-pointer))))
     (unless (string= text (current-kill n t))
       (error "Could not setup for `current-kill'"))
     ;; Restore `last-command' over Selectrum commands.
     (setq last-command old-last-command)
     ;; Delegate to `yank-pop' if appropriate or just insert.
     (if (eq last-command 'yank)
         (yank-pop n) (insert-for-yank text)))))

(defun selectrum-switch-buffer+ ()
  (interactive)
  (let* ((selectrum-should-sort-p nil)
         (candidates
          (let* ((cb (window-buffer
                      (minibuffer-selected-window)))
                 (bf (or (buffer-file-name cb) "")))
            (lambda (input)
              (let* ((buffers (mapcar #'buffer-name
                                      (cl-delete-if
                                       (lambda (buf)
                                         (eq buf cb))
                                       (buffer-list))))
                     (files (cl-delete-if (lambda (f) (string= f bf))
                                          (copy-sequence recentf-list)))
                     (candidates ()))
                (cond ((string-prefix-p " " input)
                       (setq input (substring input 1))
                       (setq candidates
                             (cl-delete-if-not
                              (lambda (name)
                                (string-prefix-p " " name))
                              buffers)))
                      ((string-prefix-p "b " input)
                       (setq input (substring input 2))
                       (setq candidates
                             (cl-delete-if
                              (lambda (name)
                                (string-prefix-p " " name))
                              buffers)))
                      ((string-prefix-p "f " input)
                       (setq input (substring input 2))
                       (setq candidates files))
                      (t
                       (setq candidates
                             (append
                              (cl-delete-if
                               (lambda (name)
                                 (string-prefix-p " " name))
                               buffers)
                              files))))
                `((candidates . ,candidates)
                  (input . ,input))))))
         (cand (selectrum-read "Switch to: " candidates)))
    (cond ((member cand recentf-list)
           (find-file cand))
          (t
           (switch-to-buffer cand)))))

(defun oht/new-tab ()
  "macOS follows this convention: command-N creates a new window and command-T creates a new tab in the same window. The Mac Port version of Emacs has functions and variables that makes following this convention possible.

This function works by setting the new-frame behaviour to use tabs, creating a new frame (thus, tab), then changing the setting back to system default."
  (interactive)
  (setq mac-frame-tabbing t)
  (make-frame-command)
  (setq mac-frame-tabbing 'automatic)
  )

(defun oht/show-tab-bar ()
  (interactive)
  (mac-set-frame-tab-group-property nil :tab-bar-visible-p t)
  )
(defun oht/hide-tab-bar ()
  (interactive)
  (mac-set-frame-tab-group-property nil :tab-bar-visible-p nil)
  )
(defun oht/show-tab-bar-overview ()
  (interactive)
  (mac-set-frame-tab-group-property nil :overview-visible-p t)
  )

(defun oht/next-frame ()
  (interactive)
  (select-frame-set-input-focus (next-frame)))
(defun oht/previous-frame ()
  (interactive)
  (select-frame-set-input-focus (previous-frame)))

