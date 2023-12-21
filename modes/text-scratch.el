;;; text-scratch.el --- *scratch* but for text -*- lexical-binding: t -*-
;;; Commentary:

;; Temporary text buffer that copies contents to clip board and clears
;; buffer on C-C C-C.

;; The main idea is this makes it easy to switch to Emacs write some
;; text with all the Emacs convenience and copy paste into another
;; program e.g: browser, slack etc.

;;; Code:

;; The 'text-scratch/kill-buffer-content-and-add-to-kill-ring function
;; code was generated using:

;;   kmacro-start-macro
;;   kmacro-end-macro
;;   kmacro-name-last-macro
;;   insert-kbd-macro

;; In pure elisp it would be (kill-region (point-min) (point-max))
(defalias 'text-scratch/kill-buffer-content-and-add-to-kill-ring
  (kmacro "M-< C-SPC M-> C-w"))

;;;###autoload
(define-minor-mode text-scratch-mode
  "Toggle mode."
  :init-value nil
  :lighter " TXSC"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")
                (lambda () (interactive)
                  (text-scratch/kill-buffer-content-and-add-to-kill-ring)))
    map))

(defun text-scratch/get-buffer-create ()
  "Return the *text* buffer, creating a new one if needed."
  (or (get-buffer "*text*")
      (let ((scratch (get-buffer-create "*text*")))
        ;; Don't touch the buffer contents or mode unless we know that
        ;; we just created it.
        (with-current-buffer scratch
          (text-mode)
          (text-scratch-mode))
        scratch)))

(defun text-scratch/buffer ()
  "Switch to the *text* buffer.
If the buffer doesn't exist, create it first."
  (interactive)
  (pop-to-buffer-same-window (text-scratch/get-buffer-create)))

(provide 'text-scratch)
;;; text-scratch.el ends here
