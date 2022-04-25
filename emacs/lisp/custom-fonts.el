;; custom-fonts.el ---  -*- lexical-binding: t -*-

;; The below code provides the following features:

;; 1. A way to define sets of fonts and font sizes, which you can load as the
;;    default for all frames or per-buffer.
;; 2. A mode for scaling only the variable-pitch face up to the exact size of
;;    your choosing so that mixed-font buffers look great.

(defvar custom-fonts-alist nil
  "An alist of font properties used by `set-custom-fonts'.
The alist should be formatted thus:

(setq custom-fonts-alist
      '((Apple . ( :mono \"SF Mono\"
                   :vari \"New York\"
                   :mode \"SF Compact Text\"
                   :line nil
                   :mono-height 120
                   :mode-height 130
                   :vari-height 140))))")

(defun set-custom-fonts (font-settings)
  "Prompt user and set fonts according to selection from `custom-fonts-alist'.
FONT-SETTINGS should be a string."
  (interactive
   (list (completing-read
          "Select default font pairings: "
          (mapcar #'car custom-fonts-alist))))
  (let* ((fonts (intern font-settings))
         (properties (alist-get fonts custom-fonts-alist))
         (mono (plist-get properties :mono))
         (vari (plist-get properties :vari))
         (mode (plist-get properties :mode))
         (line (plist-get properties :line))
         (mono-height (plist-get properties :mono-height))
         (mode-height (plist-get properties :mode-height))
         (vari-height (plist-get properties :vari-height)))
    (setq line-spacing line
          variable-pitch-adjust-height vari-height)
    (custom-set-faces
     `(default ((t :family ,mono :height ,mono-height)))
     `(fixed-pitch ((t :family ,mono)))
     `(variable-pitch ((t :family ,vari)))
     `(mode-line ((t :family ,mode :height ,mode-height)))
     `(mode-line-inactive ((t :family ,mode :height ,mode-height))))))

(defvar variable-pitch-adjust-height nil
  "Used by `variable-pitch-adjust-mode' to determine the
variable-pitch scaling amount in that mode.")

(define-minor-mode variable-pitch-adjust-mode
  "Minor mode to adjust only the variable-pitch face height buffer-locally.
Scales the variable-pitch height up to the height defined by
‘variable-pitch-adjust-height’ and the fixed-pitch face down to
match the default face height. Thus, in mixed-font settings you
can scale the variable-pitch height independently of the
fixed-pitch and default face heights."
  :init-value nil
  :lighter " V+"
  (if variable-pitch-adjust-mode
      (progn
        (setq-local variable-pitch-remapping
                    (face-remap-add-relative 'variable-pitch
                                             :height (/ variable-pitch-adjust-height
                                                        (float (face-attribute 'default :height)))))
        (setq-local fixed-pitch-remapping
                    (face-remap-add-relative 'fixed-pitch
                                             :height (/ (float (face-attribute 'default :height))
                                                        variable-pitch-adjust-height))))
    (progn
      (face-remap-remove-relative variable-pitch-remapping)
      (face-remap-remove-relative fixed-pitch-remapping))))

(add-hook 'buffer-face-mode-hook (lambda () (variable-pitch-adjust-mode 'toggle)))

(defvar-local buffer-remap-faces-default-cookie nil
  "Cookie used for `buffer-remap-faces--set'.")
(defvar-local buffer-remap-faces-fixed-pitch-cookie nil
  "Cookie used for `buffer-remap-faces--set'.")
(defvar-local buffer-remap-faces-variable-pitch-cookie nil
  "Cookie used for `buffer-remap-faces--set'.")

(defun buffer-remap-faces--clear nil
  "In the current buffer, remove all buffer-remap-faces--set cookies."
  (interactive)
  (face-remap-remove-relative buffer-remap-faces-default-cookie)
  (face-remap-remove-relative buffer-remap-faces-fixed-pitch-cookie)
  (face-remap-remove-relative buffer-remap-faces-variable-pitch-cookie))

(defun buffer-remap-faces--set (font-settings)
  "Prompt user and set fonts for the current buffer according to selection from `custom-fonts-alist'.
It should probably be a mode instead."
  (interactive
   (list (completing-read
          "Select font pairings for Buffer: "
          (mapcar #'car custom-fonts-alist))))
  (let* ((fonts (intern font-settings))
         (properties (alist-get fonts custom-fonts-alist))
         (mono (plist-get properties :mono))
         (vari (plist-get properties :vari))
         (mono-height (plist-get properties :mono-height)))
    (buffer-remap-faces--clear)
    (setq buffer-remap-faces-default-cookie
          (face-remap-add-relative 'default
                                   :family mono
                                   :height mono-height))
    (setq buffer-remap-faces-variable-pitch-cookie
          (face-remap-add-relative 'fixed-pitch
                                   :family mono))
    (setq buffer-remap-faces-fixed-pitch-cookie
          (face-remap-add-relative 'variable-pitch
                                   :family vari))
    (force-window-update (current-buffer))))

(define-minor-mode buffer-remap-faces-mode
  "Minor mode to set buffer-local fonts."
  :lighter " BufferFaces"
  :init-value nil
  :global nil
  (if buffer-remap-faces-mode
      (call-interactively 'buffer-remap-faces--set)
    (buffer-remap-faces--clear)))

(provide 'custom-fonts)
