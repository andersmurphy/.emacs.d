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

(defhydra kraken-change-state (:color blue
                              :body-pre (insert "f")
                              :timeout 0.4
                              :idle 0.5)
  ("d" (progn
         (delete-char -1)
         (kraken/body))))

(global-set-key (kbd "f") 'kraken-change-state/body)

(defhydra kraken
  (:foreign-keys warn
   :columns 3
   :body-pre   (progn
                 (set-cursor-color "#EEAD0E")
                 (setq-default cursor-type 'box)
                 (region-next-character))
   :post  (progn
            (set-cursor-color "#66CD00")
            (setq-default cursor-type 'bar)))
  "NORMAL"
  ("h" region-previous-character "move left")
  ("j" region-below-character "move down")
  ("k" region-above-character "move up")
  ("l" region-next-character "move right")
  ("a" region-until-beginning-of-line "beginning of line")
  ("e" region-until-end-of-line "end of line")
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
  "Opens a new line and enters insert state."
  (interactive)
  (call-interactively 'end-of-line)
  (call-interactively 'newline-and-indent))

(defun region-until-end-of-line ()
  "Create a region from the current point to the end of the current line."
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'end-of-line))

(defun region-until-beginning-of-line ()
  "Create a region from the current point to the start of the current line."
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'beginning-of-line))

(defun region-previous-character ()
  "Create a region on the character behind the current point.
Does nothing if the point is at the beginning of the buffer"
  (interactive)
  (when (not (bobp))
    (call-interactively 'set-mark-command)
    (call-interactively 'backward-char)))

(defun region-next-character ()
  "Create a region on the character in front of the current point.
Does nothing if the point is at the end of the buffer."
  (interactive)
  (when (not (eobp))
    (call-interactively 'forward-char)
    (call-interactively 'forward-char)
    (call-interactively 'region-previous-character)))

(defun region-above-character ()
  "Create a region on the character above and in front of the current point."
  (interactive)
  (call-interactively 'previous-line)
  (call-interactively 'forward-char)
  (call-interactively 'region-previous-character))

(defun region-below-character ()
  "Create a region on the character below and in front of the current point."
  (interactive)
  (call-interactively 'next-line)
  (call-interactively 'forward-char)
  (call-interactively 'region-previous-character))

(defun surround-with (opening closing)
  "Surround the current region with the OPENING and CLOSING strings."
  (interactive)
  (if (region-active-p)
      (insert-pair 1 opening closing)
    (insert (concat opening closing))
    (backward-char)))

(defun deactivate-mark-then-undo ()
  "Deactivate the mark and then perform and undo action."
  (interactive)
  (deactivate-mark t)
  (call-interactively 'undo-tree-undo))

(defun delete-region ()
  "Kill the region and then create a region on the next character."
  (interactive)
  (call-interactively 'kill-region)
  (call-interactively 'forward-char)
  (call-interactively 'region-previous-character))

(defun show-no-leader-function-set-message ()
  "Message to be shown when the kraken-leader-function variable hasn't been set."
  (message "You haven't set a leader function."))

(provide 'kraken)

;;; kraken.el ends here
