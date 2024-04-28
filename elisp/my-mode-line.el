;;; my-mode-line.el --- My mode line

;;; Commentary:

;; My minimalist mode line.

;;; Code:

(require 'time)

(defvar my/mode-line-selected-window (frame-selected-window))

(defun my/mode-line-set-selected-window (&rest _args)
  "Set current selected window."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq my/mode-line-selected-window (frame-selected-window))
    (force-mode-line-update)))

(defun my/mode-line-selected-active-p ()
  "Is current mode line selected."
  (eq my/mode-line-selected-window (selected-window)))

(defvar my/mode-line-format
  '("%e" mode-line-front-space
    mode-line-buffer-identification
    (:eval
     (when (my/mode-line-selected-active-p)
       (list (propertize
              " " 'display
              `(space
                :align-to
                (- right
                   ,(+ (length battery-mode-line-string)
                       (length display-time-string)))))
             'battery-mode-line-string
             'display-time-string)))))

;;;###autoload
(defun my/mode-line-init ()
  "Custom minimalist mode line with right aligned time and battery."
  (setq-default mode-line-format my/mode-line-format)
  ;; Display time in mode line.
  (setq display-time-default-load-average nil)
  (setq display-time-string-forms
        '((propertize (format-time-string "%F %H:%M" now) 'face 'bold)))
  (display-time-mode t)
  ;; Display battery in mode line.
  (display-battery-mode 1))

(provide 'my-mode-line)
;;; my-mode-line.el ends here
