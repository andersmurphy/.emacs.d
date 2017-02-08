;;; kraken.el --- A modal and composable text editing mode.

;; Author: Anders Murphy <andersmurphy@gmail.com>
;; Maintainer: Anders Murphy <andersmurphy@gmail.com>

;;; Commentary:

;;; Code:

(require 'hydra)

(defcustom kraken-leader-function 'show-no-leader-function-set-message
  "Optional leader function that can be bound to space."
  :group 'kraken
  :type 'function)

(defhydra kraken-change-mode (:color blue
                              :body-pre (insert "f")
                              :timeout 0.4
                              :idle 0.5)
  ("d" (progn
         (delete-char -1)
         (kraken/body))))

(global-set-key (kbd "f") 'kraken-change-mode/body)

(defhydra kraken
  (:foreign-keys warn
   :columns 3
   :body-pre   (progn
                 (set-cursor-color "#EEAD0E")
                 (setq-default cursor-type 'box)
                 (highlight-next-character))
   :post  (progn
            (set-cursor-color "#66CD00")
            (setq-default cursor-type 'bar)))
  "NORMAL"
  ("h" highlight-previous-character "move left")
  ("j" highlight-below-character "move down")
  ("k" highlight-above-character "move up")
  ("l" highlight-next-character "move right")
  ("a" highlight-until-beginning-of-line "beginning of line")
  ("e" highlight-until-end-of-line "end of line")
  ("g" avy-goto-word-1 "avy goto word")
  ("o" open-and-insert "open and insert" :exit t)
  ("p" yank "paste")
  ("y" copy-region-as-kill "yank")
  ("c" delete-region "change" :exit t)
  ("d" delete-region "delete")
  ("f" indent-region "format")
  ("u" deactivate-mark-then-undo "undo")
  ("i" (deactivate-mark t) "insert" :exit t)
  ("\"" (surround-with "\"" "\"") "double quotes")
  ("'" (surround-with "'" "'") "single quotes")
  ("(" (surround-with "(" ")")  "parentheses")
  ("[" (surround-with "[" "]") "braces")
  ("{" (surround-with "{" "}") "brackets")
  ("<SPC>" (funcall kraken-leader-function) "leader" :exit t))

(defun open-and-insert ()
  (interactive)
  (call-interactively 'end-of-line)
  (call-interactively 'newline-and-indent))

(defun highlight-until-end-of-line ()
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'end-of-line))

(defun highlight-until-beginning-of-line ()
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'beginning-of-line))

(defun highlight-previous-character ()
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'backward-char))

(defun highlight-next-character ()
  (interactive)
  (when (not (eobp))
      (call-interactively 'forward-char)
      (call-interactively 'forward-char)
      (call-interactively 'highlight-previous-character)))

(defun highlight-above-character ()
  (interactive)
  (call-interactively 'previous-line)
  (call-interactively 'forward-char)
  (call-interactively 'highlight-previous-character))

(defun highlight-below-character ()
  (interactive)
  (call-interactively 'next-line)
  (call-interactively 'forward-char)
  (call-interactively 'highlight-previous-character))

(defun surround-with (opening closing)
  (interactive)
  (if (region-active-p)
      (insert-pair 1 opening closing)
    (insert (concat opening closing))
    (backward-char)))

(defun deactivate-mark-then-undo ()
  (interactive)
  (deactivate-mark t)
  (call-interactively 'undo-tree-undo))

(defun delete-region ()
  (interactive)
  (call-interactively 'kill-region)
  (call-interactively 'forward-char)
  (call-interactively 'highlight-previous-character))

(defun show-no-leader-function-set-message ()
  "Message to be shown when the kraken-leader-function variable hasn't been set."
  (message "You haven't set a leader function."))

(provide 'kraken)

;;; kraken.el ends here
