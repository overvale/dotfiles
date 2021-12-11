;; quick-help.el  -*- lexical-binding: t -*-

;; This idea was stolen from the beautiful config here:
;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; This macro creates functions that display help text in a pop-up
;; buffer.

(defmacro quick-help (name buffer text)
  "Macro for creating callable functions that display help.
Where NAME is name of function, BUFFER is name of buffer, and TEXT is displayed."
  (declare (indent defun))
  `(progn
     (defun ,name nil
       ,buffer
       (interactive)
       (let ((qh-buff (concat "*Quick Help: " ,buffer "*"))
             (qh-text ,text))
         (get-buffer-create qh-buff)
         (with-current-buffer qh-buff
           (insert qh-text)
           (goto-char (point-min))
           (not-modified)
           (read-only-mode)
           (special-mode)
           (local-set-key (kbd "C-g") (lambda () (interactive) (other-window -1)))
           (local-set-key (kbd "q") 'kill-buffer-and-window))
         (pop-to-buffer qh-buff '((display-buffer-below-selected)
                                  (window-parameters . ((no-other-window . nil)))
                                  (window-height . fit-window-to-buffer)))
         (message "C-g - Previous Window, q - Remove Window")))))

(quick-help qh--it-hotline
  "IT Hotline"
  "IT HOTLINE: 855-555-5555")

(quick-help qh--departments
  "Departments"
  "\
| Department | Manager     | Extension | Time Zone |
|------------+-------------+-----------+-----------|
| Sales      | Dave F      |        16 | LA        |
| IT         | Sydney R    |       198 | NY        |
| Support    | Ellie T     |        29 | DEN       |
| Shipping   | Shaun D     |       345 | ATL       |
| Recieving  | Brian C     |       876 | NY        |
| Marketing  | Elizabeth W |        12 | LA        |
| Coffee     | Donna F     |        34 | NY        |")

(quick-help qh--wheather
  "Weather Whether Wether"
  "\
The climate is made up of “WEATHER”;
WHETHER it is nice out depends on whether it is raining or not.
A WETHER is just a castrated sheep.")

(define-prefix-command 'quick-help-prompt nil "Quick Help")

(let ((map quick-help-prompt))
  (define-key map "i" '("IT Hotline" . qh--it-hotline))
  (define-key map "d" '("Departments" . qh--departments))
  (define-key map "w" '("Weather Whether Wether" . qh--wheather)))

(global-set-key (kbd "C-c h") 'quick-help-prompt)

;; end quick-help.el
