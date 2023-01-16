;;; topiary.el --- My riff on structural editing -*- lexical-binding: t -*-
;;; Commentary:

;; Built using the lisp.el commands that come with Emacs for minimal
;; dependencies.

;; Highlights the area that commands will operate on for faster visual
;; feedback.

;; Tries to minimise the number of keybindings by doing what you mean
;; rather than what you say based on current context. Allowing you to
;; do more with less.

;;; Code:

(require 'subr-x)
(require 'org)
(require 'org-inlinetask)
(require 'sgml-mode)

(defun topiary/end-of-buffer-p ()
  "Return t if point at end of buffer."
  (= (point) (point-max)))

(defun topiary/beginning-of-buffer-p ()
  "Return t if point at beginning of buffer."
  (= (point) (point-min)))

(defun topiary/in-string-p ()
  "Return t if point in string."
  (and (not (topiary/beginning-of-buffer-p))
       (not (topiary/end-of-buffer-p))
       (nth 3 (syntax-ppss))))

(defun topiary/in-empty-string-p ()
  "Return t if point in empty string."
  (and (nth 3 (syntax-ppss))
       (= (nth 8 (syntax-ppss)) (- (point) 1))
       (= (char-before) (char-after) ?\")))

(defun topiary/in-comment-p ()
  "Return t if point in non nestable comment line."
  (nth 4 (syntax-ppss)))

(defun topiary/beginning-of-comment-p ()
  "Return t if point at comment start."
  (and
   (member (char-before) (string-to-list "\n "))
   (looking-at-p comment-start)))

(defun topiary/on-comment-line-p ()
  "Return t if point on non nestable comment line."
  (or (topiary/in-comment-p)
      (save-excursion
        (skip-chars-forward (concat "^" comment-start  "\n"))
        (when (not (= (point-max) (point)))
          (forward-char))
        (topiary/in-comment-p))))

(defun topiary/in-empty-pair-p ()
  "Return t if point in empty pair."
  (and (member (char-before) (string-to-list "{[("))
       (member (char-after) (string-to-list "}])"))))

(defun topiary/in-empty-line-p ()
  "Return t if point in empty line."
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defun topiary/after-empty-pair-p ()
  "Return t if point is directly after empty pair."
  (member (ignore-errors
            (buffer-substring-no-properties (- (point) 2) (point)))
          (list "()" "{}" "[]")))

(defun topiary/before-empty-pair-p ()
  "Return t if point is directly before empty pair."
  (member (ignore-errors
            (buffer-substring-no-properties (point)  (+ (point) 2)))
          (list "()" "{}" "[]")))

(defun topiary/after-empty-string-p ()
  "Return t if point is directly after string."
  (equal (ignore-errors
           (buffer-substring-no-properties (- (point) 2) (point)))
         "\"\""))

(defun topiary/before-empty-string-p ()
  "Return t if point is directly before string."
  (equal (ignore-errors
           (buffer-substring-no-properties (point) (+ (point) 2)))
         "\"\""))

(defmacro topiary/if-in-string (first-form &rest forms)
  "If in string do FIRST-FORM otherwise do FORMS."
  `(lambda ()
     (interactive)
     (if (topiary/in-string-p)
         ,first-form
       ,@forms)))

;;;###autoload
(define-minor-mode topiary-mode
  "Toggle topiary mode."
  :init-value nil
  :lighter " TP"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-a") 'topiary/back-to-indentation-or-beginning)
            (define-key map (kbd "C-w") 'topiary/kill)
            (define-key map (kbd "C-t") 'topiary/transpose)
            (define-key map (kbd "C-f") 'topiary/forward)
            (define-key map (kbd "C-b") 'topiary/backward)
            (define-key map (kbd "C-n") 'topiary/next)
            (define-key map (kbd "C-p") 'topiary/previous)
            (define-key map (kbd "M-f") 'topiary/bounds-forward)
            (define-key map (kbd "M-b") 'topiary/bounds-backward)
            (define-key map (kbd "C-M-k") 'topiary/kill-sexp)
            (define-key map (kbd "M-k")   'topiary/kill-sexp)
            (define-key map (kbd "C-M-h") 'backward-sexp)
            (define-key map (kbd "C-k") 'topiary/kill-line)
            (define-key map (kbd "C-d") 'topiary/delete-forward)
            (define-key map (kbd "C-y") 'topiary/yank)
            (global-set-key (kbd "C-v") 'topiary/yank)
            (define-key map (kbd "DEL") 'topiary/delete-backward)
            (define-key map (kbd "C-)") 'topiary/forward-slurp)
            (define-key map (kbd "(")  (topiary/if-in-string
                                        (insert "(")
                                        (topiary/wrap-with-parens)))
            (define-key map (kbd "[")  (topiary/if-in-string
                                        (insert "[")
                                        (topiary/wrap-with-brackets)))
            (define-key map (kbd "{")  (topiary/if-in-string
                                        (insert "{")
                                        (topiary/wrap-with-braces)))
            (define-key map (kbd "SPC")  'topiary/space)
            (define-key map (kbd "RET")  'topiary/newline)
            (define-key map (kbd ")")  (topiary/if-in-string (insert ")")))
            (define-key map (kbd "]")  (topiary/if-in-string (insert "]")))
            (define-key map (kbd "}")  (topiary/if-in-string (insert "}")))
            (define-key map (kbd ";")  'topiary/insert-semicolon)
            (define-key map (kbd "'")  'topiary/insert-quote)
            (define-key map (kbd "\"") 'topiary/insert-double-quote)
            (define-key map (kbd "\\") (lambda () (interactive)
                                         (insert "\\")))
            (define-key map (kbd "\\") (lambda () (interactive)
                                         (insert "\\")))
            (define-key map (kbd "M-l") 'topiary/downcase)
            (define-key map (kbd "M-u") 'topiary/upcase)
            (define-key map (kbd "C-\\") 'topiary/indent-region)
            (define-key map (kbd "C-M-\\") 'topiary/indent-region)
            (define-key map (kbd "M-;") 'topiary/comment-region)
            map)
  (if topiary-mode
      (progn
        (add-hook 'post-command-hook #'topiary/hl-bounds-overlay)
        (add-hook 'post-self-insert-hook 'topiary/post-self-insert))
    (remove-hook 'post-command-hook #'topiary/hl-bounds-overlay)
    (remove-hook 'post-self-insert-hook 'topiary/post-self-insert)))

(defun topiary/supported-mode-p ()
  "Return t if current mode is supported topiary."
  (member major-mode '(clojure-mode
                       clojurec-mode
                       clojurescript-mode
                       emacs-lisp-mode
                       lisp-interaction-mode)))

(defun topiary/back-to-indentation-or-beginning ()
  "Go to first character in line.
If already at first character go to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-visual-line)
    (progn (beginning-of-visual-line)
           (back-to-indentation))))

(defun topiary/insert-pair (pair)
  "Insert PAIR."
  (insert pair)
  (backward-char 1))

(defun topiary/insert-semicolon ()
  "Don't insert if it will break AST."
  (interactive)
  (when (or (not (topiary/supported-mode-p))
            (topiary/in-string-p)
            (topiary/in-comment-p)
            (= (point) (line-end-position)))
    (insert ";")))

(defun topiary/strict-insert ()
  "If to level and the last entered character is not a valid insert delete it.

\(defn foo [] 3)
n|

In the above example the n would be deleted. Handles comments."
  (let ((ppss (syntax-ppss)))
    (when (and (= 0 (car ppss))
               (not (nth 4 ppss))
               (not (member (char-before) (string-to-list "{[(\"\n "))))
      (delete-char -1))))

(defun topiary/post-self-insert ()
  "Prevent insert breaking top level AST."
  (when (topiary/supported-mode-p)
    (topiary/strict-insert)))

(defun topiary/bounds-of-last-sexp ()
  "Get bounds of last sexp."
  (when (and (not (topiary/in-string-p))
             (member (char-before) (string-to-list "}])\"")))
    (cons (save-excursion (backward-sexp) (point)) (point))))

(defun topiary/hungry-delete-backward ()
  "Delete the preceding character or all preceding whitespace."
  (interactive)
  (let ((initial-point (point)))
    (skip-chars-backward "\n ")
    (cond ((= initial-point (point)) (delete-char -1))
          ((eq last-command 'kill-region) (kill-region initial-point (point)))
          (t (delete-region initial-point (point))))))

(defun topiary/hungry-delete-forward ()
  "Delete the following character or all preceding whitespace."
  (interactive)
  (let ((initial-point (point)))
    (skip-chars-forward "\n ")
    (cond ((= initial-point (point)) (delete-char 1))
          ((eq last-command 'kill-region)
           (kill-region initial-point (point)))
          (t (delete-region (point) initial-point)))))

(defun topiary/newline ()
  "If after point open line else newline."
  (interactive)
  (if (or (member (char-after) (string-to-list "\n "))
          (topiary/end-of-buffer-p))
      (newline-and-indent)
    (progn
      (save-excursion
        (skip-chars-backward " ")
        (skip-chars-backward "\n " (- (point) 1))
        (newline-and-indent))
      (skip-chars-backward " ")
      (skip-chars-backward "\n " (- (point) 1)))))
(defun topiary/bounds-of-space-before-opening-paren ()
  "Get bounds of space character after cursor if opening char is before cursor."
  (when (or (and (not (topiary/in-string-p))
                 (member (char-before) (string-to-list "{[("))
                 (member (char-after) (string-to-list "\n ")))
            (and (topiary/in-string-p)
                 (= (char-before) ?\")
                 (member (char-after) (string-to-list "\n "))))
    (cons (point) (+ (point) 1))))

(defun topiary/bounds-of-org-subtree ()
  "Return the bound of current org subtree."
  (when (and (string-equal major-mode "org-mode")
             (not (topiary/end-of-buffer-p))
             (= (char-after) ?\*))
    (save-excursion
      (let ((beg (point)))
        (org-end-of-subtree t t)
        ;; Include the end of an inlinetask
        (when (and (featurep 'org-inlinetask)
                   (looking-at-p (concat (org-inlinetask-outline-regexp)
                                         "END[ \t]*$")))
          (end-of-line))
        (cons beg (point))))))

(defun topiary/bounds-of-punctuation-forward ()
  "Get bounds of - / character if after cursor."
  (when (and (member (char-after) (string-to-list "=_/?!#>,.@'<"))
             (not (member (char-after (+ (point) 1))
                          (string-to-list "[\"({"))))
    (cons (point) (+ (point) 1))))

(defun topiary/bounds-of-punctuation-backward ()
  "Get bounds of - / character if  before cursor."
  (when (or (not (or (member (char-after) (string-to-list "\n ]})"))
                     (member (char-before) (string-to-list "\n ({["))))
            (topiary/in-string-p)
            (topiary/in-comment-p))
    (when  (member (char-before) (string-to-list "=_-/?!>,.<"))
      (cons (point) (- (point) 1)))))

(defun topiary/bounds-of-space-forward ()
  "Get bounds of - / character if  before cursor."
  (when (member (char-after) (string-to-list " "))
    (save-excursion
      (let* ((initial-point (point))
             (next-non-space-point (progn (skip-chars-forward " ") (point)))
             (characters-between-point (- next-non-space-point initial-point))
             (character-after-next-point (char-after)))
        (cond ((or (member character-after-next-point (string-to-list ")}]\n"))
                   (and (= (char-before) ?\")
                        (topiary/in-string-p)))
               (cons initial-point next-non-space-point))
              ((> characters-between-point 1)
               (cons initial-point (- next-non-space-point 1))))))))

(defun topiary/bounds-of-active-region ()
  "Get bounds of active region if region is active."
  (when (region-active-p)
    (cons (region-beginning) (region-end))))

(defun topiary/bounds-of-empty-pair ()
  "Get bounds of empty pair () {} []."
  (when (topiary/in-empty-pair-p)
    (cons (- (point) 1) (+ (point) 1))))

(defun topiary/bounds-of-single-bracket-in-string ()
  "Get bounds of single bracket in string."
  (when (and (topiary/in-string-p)
             (member (char-before) (string-to-list "}]){[(")))
    (cons (point) (- (point) 1))))

(defun topiary/bounds-of-empty-string ()
  "Get bounds of empty string."
  (when (topiary/in-empty-string-p)
    (cons (- (point) 1) (+ (point) 1))))

(defun topiary/bounds-of-escaped-double-quote-in-string ()
  "Get bounds of escaped double quote string."
  (when (topiary/in-string-p)
    (cond ((and (equal (char-after) ?\")
                (equal (char-before) ?\\))
           (cons (- (point) 1) (+ (point) 1)))
          ((and (equal (char-before) ?\")
                (equal (char-before (- (point) 1)) ?\\))
           (cons (- (point) 2) (point)))
          ((and (equal (char-after) ?\\)
                (equal (char-after (+ (point) 1)) ?\"))
           (cons (point) (+ (point) 2))))))

(defun topiary/bounds-of-strings-at-point (strings)
  "Return bounds of string if string at point match any STRINGS."
  (save-excursion
    (skip-chars-backward "[:graph:]")
    (when (thread-last
            (seq-map (lambda (string) (concat string "[\s\n]" )) strings)
            (seq-some #'looking-at))
      (cons (point) (progn (skip-chars-forward "[:graph:]") (point))))))

(defun topiary/bounds-of-html-tag-forward ()
  "Return bounds of html tag at point forward."
  (when (and (or (provided-mode-derived-p major-mode 'sgml-mode)
                 (provided-mode-derived-p major-mode 'js-mode))
             (= (char-after) ?<))
    (save-excursion
      (cons (point) (progn (sgml-skip-tag-forward 1) (point))))))

(defun topiary/bounds-of-html-tag-backward ()
  "Return bounds of html tag at point backward."
  (when (and (or (provided-mode-derived-p major-mode 'sgml-mode)
                 (provided-mode-derived-p major-mode 'js-mode))
             (= (char-before) ?>))
    (save-excursion
      (cons (point) (progn (sgml-skip-tag-backward 1) (point))))))

(defun topiary/bounds-of-sexp-at-point ()
  "Like (bounds-of-thing-at-point \\'sexp) but return nil in strings."
  (when (not (topiary/in-string-p))
    (bounds-of-thing-at-point 'sexp)))

(defun topiary/bounds-of-word-at-point ()
  "Like (bounds-of-thing-at-point \\'word) when in string or comment.
When not in string or comment only return bounds of word when not at
edge of word. eg: foo|d would return the bounds of \\'food\\'. But food| would
 return nil."
  (when (or (not (or (topiary/end-of-buffer-p)
                     (topiary/beginning-of-buffer-p)
                     (member (char-after)  (string-to-list "\n ]})"))
                     (member (char-before) (string-to-list "\n ({["))))
            (topiary/in-string-p)
            (topiary/in-comment-p))
    (bounds-of-thing-at-point 'word)))

(defun topiary/bounds-of-char-in-comment ()
  "Return bounds char in comment."
  (when (topiary/in-comment-p)
    (cons (point) (- (point) 1))))

(defun topiary/bounds-of-comment ()
  "Return bounds of comment."
  (when (topiary/beginning-of-comment-p)
    (cons (point)
          (save-excursion
            (end-of-line)
            (point)))))

(defun topiary/compute-bounds ()
  "Get compute topiary bounds."
  (or (topiary/bounds-of-active-region)
      (topiary/bounds-of-html-tag-forward)
      (topiary/bounds-of-html-tag-backward)
      (topiary/bounds-of-punctuation-forward)
      (topiary/bounds-of-escaped-double-quote-in-string)
      (topiary/bounds-of-space-forward)
      (topiary/bounds-of-word-at-point)
      (topiary/bounds-of-comment)
      (topiary/bounds-of-strings-at-point '(";;" ";;;"))
      (topiary/bounds-of-punctuation-backward)
      (topiary/bounds-of-char-in-comment)
      (topiary/bounds-of-space-before-opening-paren)
      (topiary/bounds-of-single-bracket-in-string)
      (topiary/bounds-of-empty-string)
      (topiary/bounds-of-org-subtree)
      (topiary/bounds-of-sexp-at-point)
      (topiary/bounds-of-empty-pair)
      (topiary/bounds-of-last-sexp)))

(defvar topiary/hl-bounds-overlay nil
  "Overlay for highlighting bounds.")

(defun topiary/hl-bounds-make-overlay ()
  "Create overlay for bounds."
  (let ((overlay (make-overlay 1 1)))
    (overlay-put overlay 'face 'region)
    (setq topiary/hl-bounds-overlay overlay)
    overlay))

(defun topiary/hl-bounds-overlay ()
  "Highlight topiary bounds."
  (interactive)
  (ignore-errors
    (let ((bounds (topiary/compute-bounds))
          (overlay     (or topiary/hl-bounds-overlay
                           (topiary/hl-bounds-make-overlay))))
      (if bounds
          (move-overlay overlay
                        (car bounds)
                        (cdr bounds)
                        (current-buffer))
        (delete-overlay topiary/hl-bounds-overlay)))))

(defun topiary/bounds ()
  "Return current topiary bounds (doesn't compute new bounds)."
  (let* ((start (overlay-start topiary/hl-bounds-overlay))
         (end (overlay-end topiary/hl-bounds-overlay))
         (bounds (when (and start end) (cons start end))))
    bounds))

(defun topiary/kill ()
  "Kill topiary bounds. If neither hungry delete backward.
Delete rather then kill when in mini buffer."
  (interactive)
  (let* ((bounds (topiary/bounds))
         (bounds-directed (if (and bounds (> (point) (car bounds)))
                              (cons (cdr bounds) (car bounds))
                            bounds)))
    (if bounds
        (let ((beg (car bounds-directed))
              (end (cdr bounds-directed)))
          (if (minibufferp)
              (delete-region beg end)
            (kill-region beg end)))
      (topiary/hungry-delete-backward))))

(defun topiary/forward ()
  "Move forward skips over whitespace."
  (interactive)
  (let ((initial-point (point)))
    (skip-chars-forward "\n ")
    (when (= initial-point (point))
      (forward-char 1))))

(defun topiary/backward ()
  "Move backward skip over whitespace."
  (interactive)
  (let ((initial-point (point)))
    (skip-chars-backward "\n ")
    (when (= initial-point (point))
      (backward-char 1))))

(defun topiary/bounds-forward ()
  "Forward topiary highlighted bounds."
  (interactive)
  (let* ((bounds (topiary/bounds))
         (end-of-bounds (cdr bounds)))
    (cond ((and bounds
                (> end-of-bounds (point)))
           (goto-char end-of-bounds))
          (t (topiary/forward)))))

(defun topiary/bounds-backward ()
  "Backward topiary highlighted bounds."
  (interactive)
  (let* ((bounds (topiary/bounds))
         (beginning-of-bounds (car bounds)))
    (cond ((and bounds
                (= 0 (abs (- beginning-of-bounds (cdr bounds)))))
           (topiary/backward)
           (goto-char (car (topiary/bounds))))
          ((and bounds
                (< beginning-of-bounds (point)))
           (goto-char beginning-of-bounds))
          (t (topiary/backward)))))

(defun topiary/next ()
  "Like next line except sticks to first/last char. Skips empty lines."
  (interactive)
  (cond ((= (point) (save-excursion
                      (beginning-of-visual-line)
                      (skip-chars-forward " ")
                      (point)))
         (progn
           (call-interactively 'next-line)
           (while (topiary/in-empty-line-p)
             (call-interactively 'next-line))
           (beginning-of-visual-line)
           (skip-chars-forward " ")))
        ((= (point) (save-excursion
                      (end-of-visual-line)
                      (skip-chars-backward " ")
                      (point)))
         (progn
           (call-interactively 'next-line)
           (while (topiary/in-empty-line-p)
             (call-interactively 'next-line))
           (end-of-visual-line)
           (skip-chars-backward " ")))
        (t (call-interactively 'next-line))))

(defun topiary/previous ()
  "Like previous line except sticks to first/last char. Skips empty lines."
  (interactive)
  (cond ((= (point) (save-excursion
                      (beginning-of-visual-line)
                      (skip-chars-forward " ")
                      (point)))
         (progn
           (call-interactively 'previous-line)
           (while (topiary/in-empty-line-p)
             (call-interactively 'previous-line))
           (beginning-of-visual-line)
           (skip-chars-forward " ")))
        ((= (point) (save-excursion
                      (end-of-visual-line)
                      (skip-chars-backward " ") (point)))
         (progn
           (call-interactively 'previous-line)
           (while (topiary/in-empty-line-p)
             (call-interactively 'previous-line))
           (end-of-visual-line)
           (skip-chars-backward " ")))
        (t (call-interactively 'previous-line))))

(defun topiary/wrap-with (opening-string closing-string)
  "Wrap current symbol or sexp with OPENING-STRING CLOSING-STRING.
Cursor point stays on the same character despite potential point shift."
  (let ((pair (concat opening-string closing-string))
        (bounds (topiary/bounds)))
    (cond
     ((and (member (char-before) (string-to-list "({[") )
           (member (char-after) (string-to-list "\n ]})")))
      (topiary/insert-pair pair))
     (bounds
      (progn
        (save-excursion
          (goto-char (car bounds))
          (insert opening-string)
          (goto-char (+ (cdr bounds) 1))
          (insert closing-string))
        (when (> (+ (car bounds) 1) (point))
          (forward-char 1))))
     (t
      (topiary/insert-pair pair)))))

(defun topiary/wrap-with-parens ()
  "Wrap current symbol with parens."
  (interactive)
  (topiary/wrap-with "(" ")"))

(defun topiary/wrap-with-brackets ()
  "Wrap current symbol with parens."
  (interactive)
  (topiary/wrap-with "[" "]"))

(defun topiary/wrap-with-braces ()
  "Wrap current symbol with parens."
  (interactive)
  (topiary/wrap-with "{" "}"))

(defun topiary/wrap-with-double-quotes ()
  "Wrap current symbol with quotes."
  (interactive)
  (topiary/wrap-with "\"" "\""))

(defun topiary/unwrap ()
  "Unwrap the current expression. Works on ()[]{}\".

Examples:
  (foo bar baz)|     -> foo bar baz|
  (foo bar)| (baz)   -> foo bar| (baz)
  |(foo bar baz)     -> |foo bar baz
  |(foo bar) (baz)   -> |foo bar (baz)
  #|{foo bar baz}    -> |foo bar baz
  #|(foo %)          -> |foo %
  |\"foo\"           -> |foo
  #_|(foo bar baz)   -> |foo bar baz
  '|(foo bar baz)    -> |foo bar baz"
  (interactive)
  (save-excursion
    (when (and (equal (char-before (- (point) 1)) ?\#)
               (equal (char-after (- (point) 1)) ?\_))
      (delete-char -2))
    (let ((bounds (topiary/compute-bounds)))
      (goto-char (car bounds))
      (if (member (char-after) (string-to-list "#'"))
          (progn
            (delete-char 2)
            (goto-char (- (cdr bounds) 2)))
        (progn (delete-char 1)
               (goto-char (- (cdr bounds) 1))))
      (delete-char -1))))

(defun topiary/transpose ()
  "Move sexp left if point at beginning. Otherwise move right.
If end or beginning of outer sexp reached move point to other bound.

\(a| b c) -> (b a| c) -> (b c a|) -> (b c |a) -> (b |a c) -> (|a b c) -> (b| a c)"
  (interactive)
  (let ((bounds (topiary/bounds)))
    (cond ((member (char-after) (string-to-list ")]}")) (backward-sexp))
          ((= (point) (car bounds))
           (forward-sexp)
           (condition-case nil
               (progn
                 (transpose-sexps -1)
                 (backward-sexp))
             (error (forward-sexp))))
          (t (transpose-sexps 1)))))

(defun topiary/delete-escaped-double-quote ()
  "Delete escaped double quote."
  (let* ((bounds (topiary/bounds-of-escaped-double-quote-in-string))
         (beg (car bounds))
         (end (cdr bounds)))
    (delete-region beg end)))

(defun topiary/delete-forward ()
  "Delete forward char.
Doesn't delete delimiter unless empty, in which case it deletes both."
  (interactive)
  (unless (topiary/end-of-buffer-p)
    (if (topiary/supported-mode-p)
        (cond ((or (topiary/in-empty-pair-p) (topiary/in-empty-string-p))
               (if (equal (char-before (- (point) 1)) ?\#)
                   (delete-region (- (point) 2) (+ (point) 1))
                 (delete-region (- (point) 1) (+ (point) 1))))
              ((and (topiary/in-string-p)
                    (equal (char-after) ?\\)
                    (equal (char-after (+ (point) 1)) ?\"))
               (topiary/delete-escaped-double-quote))
              ((and (or (topiary/in-string-p) (topiary/in-comment-p))
                    (not (equal (char-after) ?\")))
               (topiary/hungry-delete-forward))
              ((or (topiary/before-empty-pair-p)
                   (topiary/before-empty-string-p))
               (delete-region  (point)  (+ (point) 2)))
              ((or (member (char-after) (string-to-list ")]}"))
                   (and (topiary/in-string-p)
                        (equal (char-after) ?\")))
               (save-excursion (forward-char) (topiary/unwrap)))
              ((member (char-after) (string-to-list "\"{[("))
               (forward-char))
              (t (topiary/hungry-delete-forward)))
      (topiary/hungry-delete-forward))))

(defun topiary/delete-backward ()
  "Delete backward char.
Doesn't delete delimiter unless empty, in which case it deletes both."
  (interactive)
  (unless (topiary/beginning-of-buffer-p)
    (if (topiary/supported-mode-p)
        (cond ((or (topiary/in-empty-pair-p) (topiary/in-empty-string-p))
               (if (equal (char-before (- (point) 1)) ?\#)
                   (delete-region (- (point) 2) (+ (point) 1))
                 (delete-region (- (point) 1) (+ (point) 1))))
              ((and (topiary/in-string-p)
                    (equal (char-before) ?\")
                    (equal (char-before (- (point) 1)) ?\\))
               (topiary/delete-escaped-double-quote))
              ((and (or (topiary/in-string-p) (topiary/in-comment-p))
                    (not (equal (char-before) ?\")))
               (topiary/hungry-delete-backward))
              ((or (topiary/after-empty-pair-p)
                   (topiary/after-empty-string-p))
               (delete-region  (- (point) 2) (point)))
              ((or (member (char-before) (string-to-list "{[("))
                   (and (topiary/in-string-p)
                        (equal (char-before) ?\")))
               (save-excursion (backward-char) (topiary/unwrap)))
              ((member (char-before) (string-to-list "\")]}"))
               (backward-char))
              (t (topiary/hungry-delete-backward)))
      (topiary/hungry-delete-backward))))

(defun topiary/yank ()
  "Like \\'yank\\'. But calling \\'yank\\' again will call \\'yank-pop\\'."
  (interactive)
  (if (member last-command '(yank yank-pop))
      (yank-pop)
    (yank)))

(defun topiary/kill-sexp ()
  "Kill sexp if in topiary supported mode otherwise. Kill sentence."
  (interactive)
  (if (topiary/supported-mode-p)
      (kill-sexp)
    (kill-sentence)))

(defun topiary/space ()
  "If previous char is space and next char is closing delimiter.
Hungry delete space and skip the delimiter (forward char).
Insert space automatically if following char is a closing
delimiter (after forward char)."
  (interactive)
  (if (not (and
            (topiary/supported-mode-p)
            (or (and (member (char-before)  (string-to-list " {[("))
                     (member (char-after) (string-to-list "]})")))
                (and (topiary/in-string-p)
                     (member (char-before) (string-to-list " "))
                     (member (char-after) (string-to-list "\""))))))
      (insert " ")
    (when (member (char-before) (string-to-list " "))
      (topiary/hungry-delete-backward))
    (forward-char)
    (when (member (char-after) (string-to-list "]})"))
      (insert " "))))

(defun topiary/forward-slurp ()
  "Slurp sexp forward. Return t if successful nil otherwise."
  (interactive)
  (save-excursion
    (up-list)
    (let ((close (char-before)))
      (delete-char -1)
      (prog1
          (ignore-errors
            (forward-sexp) t)
        (insert close)))))

(defun topiary/forward-slurp-all ()
  "Slurp all sexp forward until you get to a closing parenthesis."
  (interactive)
  (while (topiary/forward-slurp)))

(defun topiary/kill-line ()
  "Kill a line as if with `kill-line', but respecting delimiters."
  (interactive)
  (cond
   ((topiary/in-empty-line-p) (topiary/hungry-delete-forward))
   ((topiary/supported-mode-p)
    (let ((initial-point (point)))
      (cond ((save-excursion
               (skip-chars-forward "\s")
               (= (line-end-position) (point))) (skip-chars-forward "\s\n"))
            ((topiary/in-string-p) (skip-chars-forward "^\n\""))
            ((topiary/on-comment-line-p) (skip-chars-forward "^\n"))
            (t (progn (while (and
                              (ignore-errors (forward-sexp) t)
                              (> (line-end-position) (point))))
                      (skip-chars-forward "\s"))))
      (kill-region initial-point (point))))
   (t (kill-line))))

(defun topiary/insert-quote ()
  "Insert single quote if in supported mode or in string or in comment.
Otherwise wrap with double quotes."
  (interactive)
  (if (or (not (topiary/supported-mode-p))
          (topiary/in-string-p)
          (topiary/in-comment-p))
      (insert "'")
    (topiary/wrap-with-double-quotes)))

(defun topiary/insert-double-quote ()
  "Insert double quote if in supported mode or in comment.
In string insert escaped double quote.
Otherwise insert single quote."
  (interactive)
  (cond ((or (not (topiary/supported-mode-p))
             (topiary/in-comment-p))
         (insert "\""))
        ((topiary/in-string-p) (insert "\\\""))
        (t (insert "'"))))

(defun topiary/upcase ()
  "Upcase topiary bounds."
  (interactive)
  (let ((bounds (topiary/bounds)))
    (upcase-region (car bounds) (cdr bounds))))

(defun topiary/downcase ()
  "Downcase topiary bounds."
  (interactive)
  (let ((bounds (topiary/bounds)))
    (downcase-region (car bounds) (cdr bounds))))

(defun topiary/indent-region ()
  "Indent topiary bounds. If no bounds indent region.
Also cleans up whitespace."
  (interactive)
  (let ((bounds (topiary/bounds)))
    (if bounds
        (progn
          (whitespace-cleanup-region (car bounds) (cdr bounds))
          (indent-region (car bounds) (cdr bounds) nil))
      (progn
        (whitespace-cleanup-region (point-min) (point-max))
        (indent-region (point-min) (point-max) nil)))))

(defun topiary/compute-bounds-for-uncomment ()
  "Get compute topiary bounds for uncomment."
  (or (topiary/bounds-of-active-region)
      (let* ((start (point))
             (end (overlay-start show-paren--overlay))
             (bounds-directed (when (and start end)
                                (if (> start end)
                                    (cons end start)
                                  (cons start end))))
             (bounds+leading-comment
              (when bounds-directed
                (cons
                 (save-excursion
                   (goto-char (car bounds-directed))
                   (skip-chars-backward (concat "^" comment-start  "\n"))
                   (skip-chars-backward comment-start)
                   (point))
                 (cdr bounds-directed)))))
        bounds+leading-comment)
      (cons (save-excursion
              (skip-chars-backward (concat "^" comment-start  "\n"))
              (skip-chars-backward comment-start)
              (point))
            (point))))

(defun topiary/comment-region ()
  "Indent topiary bounds. If no bounds indent region.
Also cleans up whitespace."
  (interactive)
  (let ((bounds (topiary/bounds)))
    (cond ((topiary/on-comment-line-p)
           (let ((bounds (topiary/compute-bounds-for-uncomment)))
             (when bounds
               (uncomment-region (car bounds) (cdr bounds)))))
          (bounds
           (comment-region (car bounds) (cdr bounds))
           (skip-chars-forward (concat comment-start  " ")))
          ((= (point) (line-end-position))
           (insert (comment-padright comment-start (comment-add nil)))))))

(provide 'topiary)
;;; topiary.el ends here
