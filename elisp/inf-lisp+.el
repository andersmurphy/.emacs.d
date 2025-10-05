;;; inf-lisp+.el --- inf-lisp improvements  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;To turn on debugger on error: M-x toggle-debug-on-error
(require 'inf-lisp)
(require 'parseedn)

(defmacro inf-lisp+/when-repl-running (&rest forms)
  "Evaluate FORMS if REPL is running. Otherwise show error message."
  `(if (get-buffer "*inferior-lisp*")
       (progn
         ,@forms)
     (message "REPL needs to be running for this command to work!")))

(defun inf-lisp+/show-repl ()
  "Show running REPL in buffer that is not the current buffer."
  (interactive)
  (when (get-buffer "*inferior-lisp*")
    (unless (string= (buffer-name) "*inferior-lisp*")
      (display-buffer "*inferior-lisp*" t))
    (other-window 1)
    (comint-show-maximum-output)
    (other-window 1)))

(defun inf-lisp+/eval (command &optional hide)
  "Evaluate elisp representation of COMMAND or string.
HIDE prevents the repl from being shown."
  (inf-lisp+/when-repl-running
   (thread-first
     (if (stringp command)
         command
       (parseedn-print-str command))
     lisp-eval-string)
   (unless hide
     (inf-lisp+/show-repl))))

(defun inf-lisp+/get-last-sexp ()
  "Get last sexp as STRING."
  (interactive)
  (buffer-substring (save-excursion (backward-sexp) (point)) (point)))

(defun inf-lisp+/eval-last-sexp (&optional hide)
  "Evaluate previous sexp. HIDE prevents the repl from being shown."
  (interactive)
  (inf-lisp+/eval (inf-lisp+/get-last-sexp) hide))

(defun inf-lisp+/eval-buffer ()
  "Evaluate entire buffer in REPL."
  (interactive)
  (inf-lisp+/when-repl-running
   (lisp-eval-region (point-min) (point-max))
   (inf-lisp+/show-repl)))

(defun inf-lisp+/do-on-first-prompt (thunk)
  "Evaluate THUNK on first REPL prompt."
  (let ((sym  (gensym)))
    (defalias sym (lambda (output)
                    (when (string-match "^[^=>]*[=>] *" output)
                      (and thunk (funcall thunk))
                      (remove-hook 'comint-output-filter-functions
                                   sym))))
    (add-hook 'comint-output-filter-functions
              sym)))

(defun inf-lisp+/open-repl (&optional thunk)
  "Open REPL in window that is not the current buffer.
If there is only the current buffer split window right. Optionally THUNK
can be specified to invoke the relevant repl. If no thunk is provided
fall back to `inferior-lisp-program'."
  (interactive)
  (when (one-window-p)
    (split-window-sensibly))
  (display-buffer-use-some-window (current-buffer) nil)
  (other-window 1)
  (previous-buffer)
  (if (string= (buffer-name) "*inferior-lisp*")
      (previous-buffer)
    (progn
      (next-buffer)
      (or (and thunk (funcall thunk))
          (inferior-lisp inferior-lisp-program))
      (comint-show-maximum-output)))
  (other-window 1))

(provide 'inf-lisp+)
;;; inf-lisp+.el ends here
