;; theme-loading.el ---  -*- lexical-binding: t -*-

;; I will admit that the below is... overkill. But what it does is (at least)
;; conceptually simple. It allows you to define your preferred dark and light
;; themes, and then provides functions for loading them, toggling them, and
;; matching to MacOS's system appearance.

(defvar light-theme nil
  "Preferred light-theme.")

(defvar dark-theme nil
  "Preferred dark-theme.")

(defvar default-theme-color 'light
  "Default theme to load, accepts 'light and 'dark.")

(defvar current-theme-color default-theme-color
  "Is the current theme color light or dark?")

(defun disable-current-themes nil
  "Disables all currently enabled themes."
  (interactive)
  (if custom-enabled-themes
    (mapcar 'disable-theme custom-enabled-themes)))

(defun load-theme-cleanly (theme)
  "Disable active themes, then load theme."
  (interactive
   (list (intern
          (completing-read "Load Theme: "
                           (mapcar 'symbol-name (custom-available-themes))))))
  (disable-current-themes)
  (load-theme theme t))

(defun macos-dark-p ()
  "Return t if macOS appearance is dark."
  (interactive)
  (string= (shell-command-to-string "defaults read -g AppleInterfaceStyle")
           "Dark\n"))

(defun macos-toggle-system-appearance nil
  "Toggle macOS's system appearance between dark and light modes."
  (interactive)
  (shell-command-to-string "osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'"))

(defun load-theme-dark nil
  "Loads the dark theme."
  (disable-current-themes)
  (load-theme dark-theme t)
  (setq current-theme-color 'dark))

(defun load-theme-light nil
  "Loads the light theme."
  (disable-current-themes)
  (load-theme light-theme t)
  (setq current-theme-color 'light))

(defun toggle-theme-color nil
  "Toggle between `light-theme' and `dark-theme'."
  (interactive)
  (if (eq current-theme-color 'light)
      (load-theme-dark)
    (load-theme-light)))

(defun load-theme-color (color)
  "Load users preferred theme, based on ARG or macOS appearance.
Disables all current themes, then:
- if COLOR is \"light\" or \"dark\", load the `light-theme' or `dark-theme'.
- if COLOR is \"system\" check macOS's appearance state and match it with
  either the light or dark theme."
  (interactive
   (list (completing-read "Load Theme Color: " '("dark" "light" "system"))))
  (cond ((string= color "light")
         (load-theme-light))
        ((string= color "dark")
         (load-theme-dark))
        ((string= color "system")
         (if (macos-dark-p)
             (load-theme-dark)
           (load-theme-light)))))

(provide 'theme-loading)
