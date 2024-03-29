;;; Key Bindings

(bind-key "ESC" (kbd "C-g") key-translation-map )

(bind-keys*
 ("s-d" . general-dispatch)
 ("M-j" . join-line-next)
 ("s-b" . switch-to-buffer)
 ("s-B" . switch-to-buffer-other-window)
 ("s-o" . find-file)
 ("s-O" . find-file-other-window)
 ("s-j" . dired-jump)
 ("s-w" . window-dispatch)
 ("M-/" . completion-at-point)
 ("M-z" . zap-up-to-char)
 ("C-M-h" . mark-line)
 ("C-x C-x" . exchange-point-and-mark-dwim)
 ("M-Q" . unfill-dwim)
 ("M-o" . other-window)
 ("M-O" . other-window-previous))

(bind-keys
 ([remap capitalize-word] . capitalize-dwim)
 ([remap downcase-word]   . downcase-dwim)
 ([remap upcase-word]     . upcase-dwim)
 ;; Make shift-click extend the region.
 ([S-down-mouse-1] . ignore)
 ([S-mouse-1] . mouse-save-then-kill)
 ;; Use M-drag-mouse-1 to create rectangle regions.
 ([M-down-mouse-1] . mouse-drag-region-rectangle) ; down
 ([M-drag-mouse-1] . ignore)                      ; drag
 ([M-mouse-1]      . mouse-set-point))            ; up


;;; MacOS-like bindings

(bind-keys
 ("s-z" . undo-only)
 ("s-Z" . undo-redo)
 ("s-x" . kill-region)
 ("s-c" . kill-ring-save)
 ("s-v" . yank)
 ("s-<backspace>" . backward-kill-line)
 ("s-a" . mark-whole-buffer)
 ("s-n" . new-buffer)
 ("s-N" . make-frame-command)
 ("s-[" . previous-buffer)
 ("s-]" . next-buffer)
 ("s-+" . text-scale-adjust)
 ("s-/" . comment-line)
 ("s-<up>" . beginning-of-buffer)
 ("s-<down>" . end-of-buffer)
 ("s-<right>" . end-of-visual-line)
 ("s-<left>" . beginning-of-visual-line)
 ("s-s" . save-buffer)
 ("M-s-s" . save-some-buffers)
 ("s-m" . iconify-frame)
 ("s-," . find-user-init-file)
 ("s-q" . save-buffers-kill-emacs)
 ("s-." . keyboard-quit)
 ("s-M-t" . toggle-tool-bar-mode-from-frame)
 ("s-t" . mac-new-tab)
 ("s-}" . (lambda () (interactive (mac-next-tab 1))))
 ("s-{" . (lambda () (interactive (mac-previous-tab 1))))
 ("s-T" . mac-toggle-tab-bar)
 ("s-|" . mac-toggle-tab-group-overview)
 ("M-8" . (lambda () (interactive) (insert "•"))))

(keymap-set global-map "s-g" 'isearch-forward)
(keymap-set isearch-mode-map "s-g" 'isearch-repeat-forward)
(keymap-set isearch-mode-map "s-G" 'isearch-repeat-backward)


;;; Transients

(transient-define-prefix general-dispatch ()
  "General-purpose transient."
  ["Dispatch Transient"
   [("a" "AutoFill" auto-fill-mode)
    ("v" "Wrap Lines" visual-line-mode)
    ("V" "Variable Width" variable-pitch-mode)
    ("h" "Line Highlight" hl-line-mode)
    ("b" "List Buffers" bs-show)
    ("r" "Rename Visited File" rename-visited-file)
    ("k" "Kill Buffer" kill-buffer-dwim)
    ("K" "Kill Buffer & Window" kill-buffer-and-window)]
   [
    ("s o" "*scratch-org*" scratch-buffer-org)
    ("s m" "*scratch-markdown*" scratch-buffer-markdown)
    ("g" "Magit Status" magit-status)
    ]
   [("p" "Package Ops..." package-dispatch)
    ("m" "Keyboard Macro..." kbd-macro-dispatch)
    ("c" "Calendar" calendar)
    ("w" "World Clock" world-clock)]])

(transient-define-prefix kbd-macro-dispatch ()
  "Transient for keyboard macros."
  [["Keyboard Macros"
    ("s" "Start" start-kbd-macro)
    ("e" "End" end-kbd-macro)
    ("c" "Call" call-last-kbd-macro)
    ("r" "Region Lines" apply-macro-to-region-lines)]])

(transient-define-prefix package-dispatch ()
  "Transient for package operations."
  [["Packages"
    ("h" "Describe" describe-package)
    ("a" "Autoremove" package-autoremove)
    ("d" "Delete" package-delete)
    ("i" "Install" package-install)
    ("s" "Selected" package-install-selected-packages)
    ("r" "Refresh" package-refresh-contents)
    ("l" "List" list-packages)]])

(transient-define-prefix window-dispatch ()
  "Most commonly used window commands."
  [["Splits"
    ("h" "Split Horizontal" split-window-below)
    ("v" "Split Vertical"   split-window-right)
    ("b" "Balance"    balance-windows)
    ("f" "Fit"        fit-window-to-buffer)
    ("+" "Toggle H/V Split" toggle-window-split)
    ("t" "Transpose Windows" transpose-windows)]
   ["Window"
    ("k" "Kill" delete-window)
    ("s-w" "Kill Frame" delete-frame)
    ("K" "Kill Buffer & Window" kill-buffer-and-window)
    ("o" "Kill Others"  delete-other-windows)
    ("m" "Maximize" maximize-window)
    ("c" "Clone Indirect" clone-indirect-buffer)
    ("T" "Tear Off" tear-off-window)]])


(provide 'overvale-bindings)
