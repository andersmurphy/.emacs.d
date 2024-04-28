;;; speak-region.el --- Text to speech

;;; Commentary:

;;; Code:

(defvar buffer-name "*Speak Region*")

(add-to-list
 'display-buffer-alist
 `(,buffer-name display-buffer-no-window (nil)))

;;;###autoload
(defun speak-region ()
  "Convert text in region to audio."
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning)
                                              (region-end))))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (start-process
     "say"
     (generate-new-buffer buffer-name)
     "say"
     (shell-quote-argument text))))

(provide 'speak-region)
;;; speak-region.el ends here
