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
  "Switch to open buffer or recent file. Narrow to hidden buffer with { } prefix, to files with {f } prefix and to buffers with {b } prefix."
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

(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode.  The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let\* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(defun oht/org-insert-date-today ()
  (interactive)
  (org-insert-time-stamp (current-time))
  )

(defvar selectrum-outline-history nil "History of chosen headings for `selectrum-outline'.")
(defun selectrum-outline ()
  "Jump to a heading.  Regexps are pre-defined.  Obeys narrowing."
  (interactive)
  (let ((selectrum-should-sort-p nil)) ; Headings should stay in order of appearance.
    ;; Just use Org's built-in features when applicable.
    (if (eq major-mode 'org-mode)
        (let ((org-outline-path-complete-in-steps)
              (org-goto-interface 'outline-path-completion))
          (org-goto))

      ;; Otherwise, have to find and format headings manually.
      (let* ((heading-regexp
              (cl-case major-mode
                ;; Groups: (1) level determinant, (2) heading text.
                ;; The top level is 0, for a zero-length determinant.
                (emacs-lisp-mode
                 "^;;;\\(?1:;*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
                (markdown-mode
                 "^#\\(?1:#*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
                (python-mode
                 "^##\\(?1:\\**\\|#*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
                (t
                 (user-error "selectrum-outline: No headings defined for %s."
                             major-mode))))

             ;; Get the basic information of each heading in the accessible
             ;; portion of the buffer.
             (buffer-contents (split-string (buffer-string) "\n"))
             (headings
              (cl-loop for txt in buffer-contents
                       for num from 1 to (1- (length buffer-contents))
                       ;; Only get the heading lines.
                       when (string-match heading-regexp txt)
                       ;; Heading text, Outline level, Line number
                       collect (list (match-string-no-properties 2 txt)
                                     (length (match-string-no-properties 1 txt))
                                     num)))

             ;; Create the prefix headings ("H1", "H1/h2", etc.)
             (formatted-headings
              (cl-loop
               ;; Get current line, used to find closest heading.
               ;; We're not moving point, so `save-excursion' isn't needed
               ;; anywhere.
               with current-line-number = (line-number-at-pos (point))
               ;; Create a line-number format.
               ;; There are a few reasons to include the line number at the
               ;; beginning of the candidate:
               ;; 1. Emacs won't show duplicate candidates, so we need a way to
               ;     distinguish headings with the same full path
               ;     (such as 2 candidates of "a/b/c").
               ;; 2. In cases where headings are duplicated, it adds context.
               ;; 3. `completing-read' won't return candidates with their
               ;;    text properties,and we need a way to associate a heading
               ;;    with its line number.
               ;;
               ;; #3 could be avoided by using `selectrum-read', and makes for
               ;; a slightly more comfortable experience, but then the command
               ;; wouldn't work in Icomplete and others.
               with number-format = (format "L%%0%dd: "
                                            (length (number-to-string
                                                     (length buffer-contents))))
               ;; Variables for keeping track of heading "path".
               with backwards-prefix-list = ()
               with prev-heading-level = 0
               for prev-heading-text  = nil then heading-text
               for (heading-text heading-level line-num) in headings
               do (cond
                   ;; If we've moved to a greater level (further down the tree),
                   ;; add the previous heading to the heading prefix list so
                   ;; that we can prepend it to the current heading when
                   ;; formatting.
                   ((> heading-level prev-heading-level)
                    (setq backwards-prefix-list (cons prev-heading-text
                                                      backwards-prefix-list)
                          prev-heading-level heading-level))
                   ;; Otherwise, if we've moved to a lower level (higher up the
                   ;; tree), and need to remove the most recently added prefix
                   ;; from the list (i.e., go from '(c b a) back to '(b a)).
                   ((< heading-level prev-heading-level)
                    (setq backwards-prefix-list (last backwards-prefix-list
                                                      heading-level)
                          prev-heading-level heading-level))
                   ;; Otherwise, do nothing.
                   (t nil))
               collect (cons (concat
                              (format number-format line-num)
                              (string-join (reverse backwards-prefix-list)
                                           "/")
                              (and backwards-prefix-list "/")
                              heading-text)
                             ;; Find distance from the heading line to the
                             ;; current line.  Positive for lines after, and
                             ;; negative for lines before.
                             (- line-num current-line-number))))

             ;; Get default candidate.
             (default-candidate
               (cl-loop for prev-heading = "" then heading
                        for (heading . dist) in formatted-headings
                        ;; Once we've gone down past the current line, return
                        ;; the previous heading (the one found before going
                        ;; past).
                        when (> dist 0)
                        return prev-heading))

             ;; Get the desired heading.
             (chosen-heading (completing-read "Jump to heading: "
                                              formatted-headings
                                              nil t nil
                                              'selectrum-outline-history
                                              default-candidate))
             ;; Stop at the ":". It is followed by one " ".
             (line-number-prefix (seq-take-while (lambda (char)
                                                   (not (char-equal ?: char)))
                                                 chosen-heading))
             ;; Get the line number for that heading, skipping the "L" in
             ;; line-number-prefix.
             (chosen-line-number
              (string-to-number (substring line-number-prefix 1)))
             ;; Get the current line number to determine the travel distance.
             (current-line-number (line-number-at-pos (point))))

        ;; After selection, manually edit history to remove line numbers.
        (setcar selectrum-outline-history
                (substring chosen-heading
                           ;; Want after line-prefix followed by ": ".
                           (+ (length line-number-prefix) 2)))
        ;; Now do the actual movement, but first push mark.
        (push-mark (point) t)
        ;; Using `goto-line' isn't recommended for non-interactive use.
        (forward-line (- chosen-line-number current-line-number))
        (beginning-of-line-text 1)))))

(defcustom selectrum-marks-highlight-face 'highlight
  "The face used to highlight the mark (shown as \"|\") in `selectrum-marks'."
  :type 'face
  :group 'selectrum)

(defvar selectrum--marks-history ()
  "History for the command `selectrum-marks'.
This is probably not so useful, since marks can move with text.")

;;;###autoload
(defun selectrum-marks ()
  "Jump to a marker in `mark-ring', signified by a highlighted \"|\" (the vertical bar character).
Currently truncates line if longer than window body width."
  (interactive)
  (if (null (marker-position (mark-marker)))
      ;; If the first marker is not placed (though it probably exists),
      ;; assume that no valid marks exist.
      (user-error "selectrum-marks: No marks currently exist.")
    (let* ((selectrum-should-sort-p nil)
           (formatted-candidates
            (save-excursion
              (cl-loop with window-width = (window-body-width (minibuffer-window))
                       for marker in (cons (mark-marker)
                                           ;; Some markers have the same position,
                                           ;; so we skip them.
                                           (cl-remove-duplicates
                                            mark-ring
                                            :test (lambda (m1 m2)
                                                    (= (marker-position m1)
                                                       (marker-position m2)))))
                       ;; Since we need to go to the marker's position anyway,
                       ;; we get and go to the position in one step.
                       ;; Since `mark-ring' is buffer local, we assume that
                       ;; all markers in it have a valid position.
                       for pos          = (goto-char (marker-position marker))
                       for line-beg-pos = (line-beginning-position)
                       ;; Get where we'll show the marker in the candidate.
                       ;; NOTE: At some point, we'll want to make sure this
                       ;; is actually visible for long lines.
                       for str-pos      = (- pos line-beg-pos)
                       ;; Get the marker's context.
                       for line-string  = (buffer-substring
                                           line-beg-pos (line-end-position))
                       ;; Display the marker in the candidate.
                       for highlighted-candidate = (concat (substring line-string 0 str-pos)
                                                           (propertize
                                                            "|"
                                                            'face selectrum-marks-highlight-face)
                                                           (substring line-string str-pos))

                       ;; Create the final formatting of each candidate.
                       ;; Need to do formatting at end to make sure things are properly aligned.
                       collect pos                   into marker-positions
                       collect highlighted-candidate into highlighted-candidates

                       for      line-number =    (line-number-at-pos pos t)
                       collect  line-number into line-numbers
                       maximize line-number into max-line-number

                       collect  str-pos into column-numbers
                       maximize str-pos into max-col-number

                       finally return
                       (cl-loop with form = (concat "%0"   (number-to-string (length (number-to-string max-line-number)))
                                                    "d,%0" (number-to-string (length (number-to-string max-col-number)))
                                                    "d: %s")
                                for marker-pos in marker-positions
                                for line-num   in line-numbers
                                for col-num    in column-numbers
                                for cand       in highlighted-candidates
                                for str        =  (format form line-num col-num cand)
                                collect (cons (if (> (length str) window-width)
                                                  (concat (substring str 0 (- window-width 10)) "...")
                                                str)
                                              marker-pos)))))
           ;; Get the desired marker from the user.
           (chosen-cand (completing-read "Go to marker: " formatted-candidates nil
                                         t nil selectrum--marks-history)))
      ;; Go to the chosen marker.
      (goto-char (cdr (assoc chosen-cand formatted-candidates))))))
