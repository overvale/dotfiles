;;; Mode-Line

(define-minor-mode buffer-position
  "Minor mode to display position of point in mode-line."
  :init-value nil
  :global nil)

(defun mode-line-buffer-modified-status ()
  "Return a string indicating buffer modification status."
  (if (and (buffer-modified-p) ;; modified
           (buffer-file-name)) ;; file-visiting buffer
      (propertize" 􀜞"
                  'help-echo "Buffer is modified.")))

(defun mode-line-buffer-read-only-status ()
  "Return a string indicating buffer read-only status."
  (if buffer-read-only
      (propertize " 􀎠 "
                  'help-echo "Buffer is read-only!")))

(defun mode-line-buffer-confirm-kill-status ()
  "Return a string indicating `buffer-confirm-kill' status."
  (if buffer-confirm-kill
      (propertize " 􀉻 "
                  'help-echo "You must confirm killing this buffer.")))

(defun mode-line-buffer-line-spacing-status ()
  "Return a string indicating the buffer's `line-spacing' value."
  (if line-spacing
      (propertize (concat " 􀆏" (number-to-string line-spacing) " ")
                  'help-echo "Buffer's line-spacing has been modified.")))

(defun modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator)))

(defun modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun modeline-major-mode-help-echo ()
  "Return `help-echo' value for `prot-modeline-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(setq-default mode-line-format
              '((:eval (mode-line-buffer-modified-status))
                ;; buffer name
                ;; TODO: This should only display an icon if the buffer is file-visiting
                "  􀈷 %b  "
                ;; Major Mode
                "    "
                (:eval
                 `(,(modeline-major-mode-indicator)
                   " "
                   ,(modeline-major-mode-name)
                   ))
                ;; buffer status
                (:eval (mode-line-buffer-read-only-status))
                ;; (:eval (mode-line-buffer-confirm-kill-status))
                "    "
                ;; Modes
                (:eval (when (mode-line-window-selected-p)
                           mode-line-modes))
                ;; Cursor position in buffer
                (:eval (when (mode-line-window-selected-p)
                         (when (eq buffer-position t)
                           (list "  􀋵 " mode-line-position "  "))))
                ;; Line-height spacing
                (:eval (when (mode-line-window-selected-p)
                         (mode-line-buffer-line-spacing-status)))))

(provide 'overvale-modeline)
