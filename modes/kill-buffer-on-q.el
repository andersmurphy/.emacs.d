;;; kill-buffer-on-q.el --- q kills buffer  -*- lexical-binding: t -*-
;;; Commentary:
;; Minor mode for killing current buffer on q
;;; Code:

;;;###autoload
(define-minor-mode kill-buffer-on-q
  "Toggle mode."
  :init-value nil
  :lighter " kbq"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda () (interactive)
                                (kill-buffer (current-buffer))))
    map))

(provide 'kill-buffer-on-q)
;;; kill-buffer-on-q.el ends here
