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
  (and (not (topiary/beginning-of-buffer-p))
       (not (topiary/end-of-buffer-p))
       (= (char-before) (char-after) ?\")))

(defun topiary/in-empty-pair-p ()
  "Return t if point in empty pair."
  (and (member (char-before) (string-to-list "{[("))
       (member (char-after) (string-to-list "}])"))))

(defun topiary/in-empty-line-p ()
  "Return t if point in empty line."
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defun topiary/after-empty-pair-p ()
  "Return t if point is directly after empty pair."
  (member (buffer-substring-no-properties (- (point) 2) (point))
          (list "()" "{}" "[]")))

(defun topiary/before-empty-pair-p ()
  "Return t if point is directly before empty pair."
  (member (buffer-substring-no-properties (point)  (+ (point) 2))
          (list "()" "{}" "[]")))

(defun topiary/after-empty-string-p ()
  "Return t if point is directly after string."
  (equal (buffer-substring-no-properties (- (point) 2) (point))
         "\"\""))

(defun topiary/before-empty-string-p ()
  "Return t if point is directly before string."
  (equal (buffer-substring-no-properties (point) (+ (point) 2))
         "\"\""))

(defmacro topiary/if-in-string (first-form &rest forms)
  "If in string do FIRST-FORM otherwise do FORMS."
  `(lambda ()
     (interactive)
     (if (in-string-p)
         ,first-form
       ,@forms)))

;;;###autoload
(define-minor-mode topiary-mode
  "Toggle topiary mode."
  :init-value nil
  :lighter " TP"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-a") 'topiary/back-to-indentation-or-beginning)
            (define-key map (kbd "C-w") 'topiary/smart-kill)
            (define-key map (kbd "C-t") 'topiary/smart-transpose)
            (define-key map (kbd "C-f") 'topiary/smart-forward)
            (define-key map (kbd "C-b") 'topiary/smart-backward)
            (define-key map (kbd "M-f") 'topiary/smart-sexp-forward)
            (define-key map (kbd "M-b") 'topiary/smart-sexp-backward)
            (define-key map (kbd "C-M-k") 'kill-sexp)
            (define-key map (kbd "C-M-h") 'backward-sexp)
            (define-key map (kbd "C-k") 'topiary/kill-line)
            (define-key map (kbd "C-d") 'topiary/delete-forward)
            (define-key map (kbd "DEL") 'topiary/delete-backward)
            (define-key map (kbd "C-)") 'topiary/forward-slurp)
            (define-key map (kbd "'")  (topiary/if-in-string
                                        (insert "'")
                                        (topiary/smart-quote)))
            (define-key map (kbd "(")  (topiary/if-in-string
                                        (insert "(")
                                        (topiary/smart-bracket)))
            (define-key map (kbd "[")  (topiary/if-in-string
                                        (insert "[")
                                        (topiary/wrap-with-brackets)))
            (define-key map (kbd "{")  (topiary/if-in-string
                                        (insert "{")
                                        (topiary/wrap-with-braces)))
            (define-key map (kbd "SPC")  'topiary/smart-space)
            (define-key map (kbd ")")  (topiary/if-in-string (insert ")")))
            (define-key map (kbd "]")  (topiary/if-in-string (insert "]")))
            (define-key map (kbd "}")  (topiary/if-in-string (insert "}")))
            (define-key map (kbd ";")  (topiary/if-in-string
                                        (insert ";")
                                        (topiary/insert-double-semicolon)))
            (define-key map (kbd "\"") (topiary/if-in-string
                                        (insert "\\\"")
                                        (insert "'")))
            (define-key map (kbd "\\") (lambda () (interactive) (insert "\\")))
            map)
  (if topiary-mode
      (progn
        (add-hook 'post-command-hook #'topiary/hl-current-kill-region-overlay)
        (add-hook 'post-self-insert-hook 'topiary/post-self-insert))
    (remove-hook 'post-command-hook #'topiary/hl-current-kill-region-overlay)
    (remove-hook 'post-self-insert-hook 'topiary/post-self-insert)))

(defun topiary/supported-mode-p ()
  "Return t if current mode is supported topiary."
  (member major-mode '(clojure-mode
                       clojurec-mode
                       clojurescript-mode
                       emacs-lisp-mode
                       lisp-interaction-mode)))

(defun topiary/back-to-indentation-or-beginning ()
  "Go to first character in line. If already at first character go to beginning of line."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun topiary/symbols-in-sexp ()
  "Return list of strings before point in sexp."
  (ignore-errors
    (thread-last
        (buffer-substring
         (save-excursion (backward-up-list) (point))
         (save-excursion (backward-up-list) (forward-sexp) (point)))
      (replace-regexp-in-string "{" "(")
      (replace-regexp-in-string "}" ")")
      (replace-regexp-in-string "#" "")
      read-from-string
      car)))

(defun topiary/symbols-in-outer-sexp ()
  "Return list of strings before point in outer sexp."
  (ignore-errors
    (thread-last
        (buffer-substring
         (save-excursion (backward-up-list) (backward-up-list) (point))
         (save-excursion (backward-up-list) (backward-up-list) (forward-sexp) (point)))
      read-from-string
      car
      (seq-remove #'vectorp))))

(defun topiary/insert-pair (pair)
  "Insert PAIR."
  (insert pair)
  (backward-char 1))

(defun topiary/wrap-with (opening-string closing-string)
  "Wrap current symbol or sexp with OPENING-STRING CLOSING-STRING.
Cursor point stays on the same character despite potential point shift."
  (let ((pair (concat opening-string closing-string))
        (bounds (or (bounds-of-thing-at-point 'sexp)
                    (and
                     (not (member (char-before) (string-to-list "#@")))
                     (bounds-of-thing-at-point 'symbol)))))
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

(defvar topiary/sb-depth-1-syms
  '(fn defn let defmacro if-let when-let binding assoc-in update-in
       get-in select-keys defmethod with-redefs :keys :strs loop
       when-some if-some letfn)
  "List of symbols that trigger smart bracket at paren depth 1.")

(defvar topiary/sb-depth-2-syms
  '(fn defn defmacro defmethod :require :import)
  "List of symbols that trigger smart bracket at paren depth 2.")

(defvar topiary/sb-always-bracket-syms
  '(:require :import)
  "List of symbols that's should always bracket.")

(defun topiary/sb-p (syms sexp)
  "Return t if SEXP satisfies smart bracket heuristic.
If the first item in the list is a member of the smart bracket SYMS list."
  (ignore-errors
    (thread-first (car sexp)
      (member syms))))

(defun topiary/no-vector-in-sexp (sexp)
  "Return non-nil if no vector in SEXP."
  (not (seq-some #'vectorp sexp)))

(defun topiary/smart-bracket-clojure ()
  "Contextually insert [] when typing ()."
  (interactive)
  (let ((sexp (topiary/symbols-in-sexp))
        (outer-sexp (topiary/symbols-in-outer-sexp)))
    (cond ((or (bounds-of-thing-at-point 'sexp)
               (bounds-of-thing-at-point 'symbol))
           (topiary/wrap-with-parens))
          ((or (topiary/sb-p topiary/sb-always-bracket-syms sexp)
               (and (topiary/sb-p topiary/sb-depth-1-syms sexp)
                    (topiary/no-vector-in-sexp sexp))
               (and
                (vectorp sexp)
                (topiary/sb-p topiary/sb-depth-2-syms outer-sexp)))
           (topiary/insert-pair "[]"))
          (t (topiary/insert-pair "()")))))

(defun topiary/smart-bracket-lisp ()
  "Wrap in parens if sexp otherwise insert parens."
  (interactive)
  (if (or (symbol-at-point) (sexp-at-point))
      (topiary/wrap-with-parens)
    (topiary/insert-pair "()")))

(defun topiary/smart-bracket ()
  "Select appropriate smart-bracket for LISP dialect."
  (interactive)
  (cond ((member major-mode '(clojure-mode clojurescript-mode clojurec-mode)) (topiary/smart-bracket-clojure))
        (t (topiary/smart-bracket-lisp))))

(defun topiary/smart-transpose ()
  "Move sexp left if point at beginning. Otherwise move right.
If end or beginning of outer sexp reached move point to other bound.

\(a| b c) -> (b a| c) -> (b c a|) -> (b c |a) -> (b |a c) -> (|a b c) -> (b| a c)"
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (cond ((not bounds)             (backward-sexp))
          ((= (point) (car bounds))
           (forward-sexp)
           (condition-case nil
               (progn
                 (transpose-sexps -1)
                 (backward-sexp))
             (error (forward-sexp))))
          (t (transpose-sexps 1)))))

(defun topiary/insert-double-semicolon ()
  "Insert two semicolons. Don't insert if it will break AST. Insert single semicolon if inside string."
  (interactive)
  (cond ((or (not (topiary/supported-mode-p))
             (topiary/in-string-p))
         (insert ";"))
        ((ignore-errors
           (or (= (save-excursion (forward-sexp) (point)) (line-end-position))
               (= (point) (line-end-position))))
         (insert ";; "))))

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
  (when (member (char-before) (string-to-list "}])\""))
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
          ((eq last-command 'kill-region) (kill-region (point) initial-point))
          (t (delete-region (point) initial-point)))))

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
  (when (member (char-after) (string-to-list "=_-/?!#>,.@'<"))
    (cons (point) (+ (point) 1))))

(defun topiary/bounds-of-punctuation-backward ()
  "Get bounds of - / character if  before cursor."
  (when  (member (char-before) (string-to-list "=_-/?!>,.<"))
    (cons (point) (- (point) 1))))

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

(defun topiary/smart-kill-bounds ()
  "Get current smart-kill bounds."
  (or (topiary/bounds-of-active-region)
      (topiary/bounds-of-html-tag-forward)
      (topiary/bounds-of-html-tag-backward)
      (topiary/bounds-of-punctuation-forward)
      (topiary/bounds-of-escaped-double-quote-in-string)
      (topiary/bounds-of-space-forward)
      (when (not (minibufferp))
        (bounds-of-thing-at-point 'word))
      (topiary/bounds-of-strings-at-point '(";;" ";;;"))
      (topiary/bounds-of-punctuation-backward)
      (topiary/bounds-of-space-before-opening-paren)
      (topiary/bounds-of-single-bracket-in-string)
      (topiary/bounds-of-empty-string)
      (topiary/bounds-of-org-subtree)
      (bounds-of-thing-at-point 'sexp)
      (topiary/bounds-of-empty-pair)
      (topiary/bounds-of-last-sexp)))

(defvar topiary/hl-current-kill-region-overlay nil
  "Overlay for highlighting current kill region.")

(defun topiary/hl-current-kill-region-make-overlay ()
  "Create overlay for current kill region."
  (let ((overlay (make-overlay 1 1)))
    (overlay-put overlay 'face 'region)
    (setq topiary/hl-current-kill-region-overlay overlay)
    overlay))

(defun topiary/hl-current-kill-region-overlay ()
  "Highlight current kill region."
  (interactive)
  (ignore-errors
    (let ((bounds (topiary/smart-kill-bounds))
          (overlay     (or topiary/hl-current-kill-region-overlay
                           (topiary/hl-current-kill-region-make-overlay))))
      (if bounds
          (move-overlay overlay
                        (car bounds)
                        (cdr bounds)
                        (current-buffer))
        (delete-overlay topiary/hl-current-kill-region-overlay)))))

(defun topiary/smart-kill ()
  "Kill current topiary overlay. If neither hungry delete backward.
Delete rather than kill when in mini buffer."
  (interactive)
  (let* ((start (overlay-start topiary/hl-current-kill-region-overlay))
         (end (overlay-end topiary/hl-current-kill-region-overlay))
         (bounds (when (and start end) (cons start end)))
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

(defun topiary/smart-forward ()
  "Move forward skips over whitespace."
  (interactive)
  (let ((initial-point (point)))
    (skip-chars-forward "\n ")
    (when (= initial-point (point))
      (forward-char 1))))

(defun topiary/smart-backward ()
  "Move backward skip over whitespace."
  (interactive)
  (let ((initial-point (point)))
    (skip-chars-backward "\n ")
    (when (= initial-point (point))
      (backward-char 1))))

(defun topiary/smart-sexp-forward ()
  "When in supported mode, forward sexp."
  (interactive)
  (when (topiary/supported-mode-p)
    (forward-sexp)))

(defun topiary/smart-sexp-backward ()
  "When in supported mode, backward sexp."
  (interactive)
  (when (topiary/supported-mode-p)
    (backward-sexp)))

(defun topiary/unwrap ()
  "Unwrap the current expression. Works on ()[]{}\".

Examples:
  (foo bar baz)|     -> foo bar baz|
  (foo bar)| (baz)   -> foo bar| (baz)
  |(foo bar baz)     -> |foo bar baz
  |(foo bar) (baz)   -> |foo bar (baz)
  #{|foo bar baz}    -> |foo bar baz
  #(|foo %)          -> |foo %"
  (interactive)
  (save-mark-and-excursion
    (condition-case nil
        (let ((bounds (topiary/smart-kill-bounds)))
          (goto-char (car bounds))
          (if (equal (char-after) ?\#)
              (progn (delete-char 2)
                     (goto-char (- (cdr bounds) 2)))
            (progn (delete-char 1)
                   (goto-char (- (cdr bounds) 1))))
          (delete-char -1))
      (error (message "Can't unwrap top level")))))

(defun topiary/delete-forward ()
  "Delete forward char doesn't delete delimiter unless empty, in which case it deletes both."
  (interactive)
  (unless (topiary/end-of-buffer-p)
    (if (topiary/supported-mode-p)
        (cond ((or (topiary/in-empty-pair-p) (topiary/in-empty-string-p))
               (delete-region (- (point) 1) (+ (point) 1)))
              ((or (topiary/before-empty-pair-p)
                   (topiary/before-empty-string-p))
               (delete-region  (point)  (+ (point) 2)))
              ((topiary/in-string-p)
               (topiary/hungry-delete-forward))
              ((member (char-after) (string-to-list ")]}"))
               (save-excursion (forward-char) (topiary/unwrap)))
              ((not (member (char-after) (string-to-list "\"{[(")))
               (topiary/hungry-delete-forward)))
      (topiary/hungry-delete-forward))))

(defun topiary/delete-backward ()
  "Delete backward char doesn't delete delimiter unless empty, in which case it deletes both."
  (interactive)
  (unless (topiary/beginning-of-buffer-p)
    (if (topiary/supported-mode-p)
        (cond ((or (topiary/in-empty-pair-p) (topiary/in-empty-string-p))
               (delete-region (- (point) 1) (+ (point) 1)))
              ((or (topiary/after-empty-pair-p)
                   (topiary/after-empty-string-p))
               (delete-region  (- (point) 2) (point)))
              ((topiary/in-string-p)
               (topiary/hungry-delete-backward))
              ((member (char-before) (string-to-list "{[("))
               (save-excursion (backward-char) (topiary/unwrap)))
              ((not (member (char-before) (string-to-list "\")]}")))
               (topiary/hungry-delete-backward)))
      (topiary/hungry-delete-backward))))

(defun topiary/smart-yank ()
  "Overwrite current smart kill region when yanking."
  (interactive)
  (unless (or (topiary/bounds-of-empty-string)
              (topiary/bounds-of-empty-pair)
              (topiary/bounds-of-punctuation-forward)
              (topiary/bounds-of-punctuation-backward))
    (let* ((bounds (topiary/smart-kill-bounds))
           (bounds-directed (if (and bounds (> (point) (car bounds)))
                                (cons (cdr bounds) (car bounds))
                              bounds)))
      (when bounds
        (let ((beg (car bounds-directed))
              (end (cdr bounds-directed)))
          (delete-region beg end)))))
  (yank))

(defun topiary/smart-quote ()
  "If previous and next character wrap in double quotes.
If previous character is a alphanumeric insert single quote.
If next character is a alphanumeric or an opening paren insert single quote.
Otherwise insert double quote."
  (interactive)
  (let ((b-char (if (char-before) (char-to-string (char-before)) ""))
        (a-char (if (char-before) (char-to-string (char-after)) "")))
    (cond
     ((and  (string-match "[[:alnum:]-_/:]" b-char)
            (string-match "[[:alnum:]-_/(?!:]" a-char))
      (topiary/wrap-with "\"" "\""))
     ((or   (string-match "[[:alnum:]]" b-char)
            (string-match "[[:alnum:](]" a-char))
      (insert "'"))
     (t (topiary/insert-pair "\"\"")))))

(defun topiary/smart-space ()
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
    (let ((line-number (count-lines 1 (point)))
          (initial-point (point)))
      (while (and
              (progn (skip-chars-forward "\n ")
                     (not (and (= (char-after) ?\") (topiary/in-string-p))))
              (ignore-errors (forward-sexp) t)
              (and (= line-number (count-lines 1 (point)))
                   (save-excursion
                     (skip-chars-forward "\n ")
                     (= line-number (count-lines 1 (point)))))))
      (kill-region initial-point (point))))
   (t (kill-line))))

(provide 'topiary)
;;; topiary.el ends here
