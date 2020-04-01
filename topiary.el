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

;; Missing functionality

;; (define-key map (kbd "C-k") 'sp-kill-sexp)
;; ("<C-[>" . sp-backward-slurp-sexp)
;; ("C-{" . sp-backward-barf-sexp)
;; ("C-}" . sp-forward-barf-sexp)
;; (define-key smartparens-mode-map (kbd "C-]") 'sp-forward-slurp-sexp)
;; (sp-backward-unwrap-sexp)
;;  Can't insert ;; inside sexp even though it wouldn't break sexp
;;  Should auto insert ;; in correct context
;;  Normal ;/: behaviour outside of lisps
;; If inserting ;; after ;; convert to  ;;;

(defmacro topiary/if-in-string (then-form else-form)
  "If in string do THEN-FORM otherwise do ELSE-FORM."
  `(lambda ()
     (interactive)
     (if (nth 3 (syntax-ppss))
         ,then-form
       ,else-form)))

;;;###autoload
(define-minor-mode topiary-mode
  "Toggle topiary mode."
  :init-value nil
  :lighter " TP"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-a") 'topiary/back-to-indentation-or-beginning)
            (define-key map (kbd "C-w") 'topiary/smart-kill)
            (define-key map (kbd "C-t") 'topiary/smart-transpose)
            (define-key map (kbd "C-f") 'topiary/skip-ws-forward-char)
            (define-key map (kbd "C-b") 'topiary/skip-ws-backward-char)
            (define-key map (kbd "C-M-k") 'kill-sexp)
            (define-key map (kbd "C-M-h") 'backward-sexp)
            (define-key map (kbd "C-k") 'sp-kill-hybrid-sexp)
            (define-key map (kbd "'")  (topiary/if-in-string
                                        (insert "\\\"")
                                        (topiary/smart-quote)))
            (define-key map (kbd "(")  (topiary/if-in-string
                                        (insert "(")
                                        (topiary/smart-bracket)))
            (define-key map (kbd ")")  (topiary/if-in-string
                                        (insert ")")
                                        (sp-backward-unwrap-sexp)))
            (define-key map (kbd "[")  (topiary/if-in-string
                                        (insert "[")
                                        (topiary/wrap-with-brackets)))
            (define-key map (kbd "{")  (topiary/if-in-string
                                        (insert "{")
                                        (topiary/wrap-with-braces)))
            (define-key map (kbd ";")  (topiary/if-in-string
                                        (insert ";")
                                        (topiary/insert-double-semicolon)))
            (define-key map (kbd "\"") (lambda () (interactive) (insert "'")))
            (define-key map (kbd "\\") (lambda () (interactive) (insert "\\")))
            map)
  (if topiary-mode
      (progn
        (add-hook 'post-command-hook #'topiary/hl-current-kill-region-overlay-hook)
        (add-hook 'post-self-insert-hook 'topiary/post-self-insert))
    (remove-hook 'post-command-hook #'topiary/hl-current-kill-region-overlay-hook)
    (remove-hook 'post-self-insert-hook 'topiary/post-self-insert)))

(defun topiary/back-to-indentation-or-beginning ()
  "Go to first character in line. If already at first character go to beginning of line."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun topiary/skip-ws-forward-char ()
  "Move cursor forward one character. Skips over whitespace."
  (interactive)
  (let ((initial-point (point)))
    (skip-chars-forward "\n ")
    (when (= initial-point (point))
      (forward-char 1))))

(defun topiary/skip-ws-backward-char ()
  "Move cursor backward one character. Skips over whitespace."
  (interactive)
  (let ((initial-point (point)))
    (skip-chars-backward "\n ")
    (when (= initial-point (point))
      (backward-char 1))))

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
           (member (char-after) (string-to-list "\n ")))
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
  (cond ((equal major-mode 'clojure-mode) (topiary/smart-bracket-clojure))
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
  (cond ((ignore-errors
           (or (= (save-excursion (forward-sexp) (point)) (line-end-position))
               (= (point) (line-end-position))))
         (insert ";; "))
        ((nth 3 (syntax-ppss)) (insert ";"))))

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
  (when (member major-mode '(clojure-mode emacs-lisp-mode))
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
    (if (= initial-point (point))
        (delete-char -1 t)
      (kill-region initial-point (point)))))

(defun topiary/bounds-of-space-before-opening-paren ()
  "Get bounds of space character after cursor if opening char is before cursor."
  (when (and (member (char-before) (string-to-list "{[("))
             (member (char-after) (string-to-list "\n ")))
    (cons (point) (+ (point) 1))))

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
        (cond ((member character-after-next-point (string-to-list ")}]\n"))
               (cons initial-point next-non-space-point))
              ((> characters-between-point 1)
               (cons initial-point (- next-non-space-point 1))))))))

(defun topiary/bounds-of-active-region ()
  "Get bounds of active region if region is active."
  (when (region-active-p)
    (cons (region-beginning) (region-end))))

(defun topiary/smart-kill-bounds ()
  "Get current smart-kill bounds."
  (or (topiary/bounds-of-active-region)
      (topiary/bounds-of-punctuation-forward)
      (topiary/bounds-of-space-forward)
      (bounds-of-thing-at-point 'word)
      (topiary/bounds-of-punctuation-backward)
      (topiary/bounds-of-space-before-opening-paren)
      (bounds-of-thing-at-point 'sexp)
      (topiary/bounds-of-last-sexp)))

(defmacro topiary/handle-ivy-if-loaded ()
  "When ivy is loaded handle smart-kill in ivy minibuffer."
  (when (require 'ivy nil 'noerror)
    '(when (and (minibufferp)
                (bound-and-true-p ivy-mode))
       (ivy-backward-delete-char))))

(defun topiary/smart-kill ()
  "Kill backward word or sexp. If neither hungry delete backward."
  (interactive)
  (let* ((bounds (topiary/smart-kill-bounds))
         (bounds-directed (if (and bounds (> (point) (car bounds)))
                              (cons (cdr bounds) (car bounds))
                            bounds)))
    (cond (bounds
           (condition-case nil
               (kill-region (car bounds-directed) (cdr bounds-directed))
             (error
              (topiary/handle-ivy-if-loaded))))
          (t (topiary/hungry-delete-backward)))))

(defvar topiary/hl-current-kill-region-overlay nil
  "Overlay for highlighting current kill region.")

(defun topiary/hl-current-kill-region-make-overlay ()
  "Create overlay for current kill region."
  (let ((overlay (make-overlay 1 1)))
    (overlay-put overlay 'face 'region)
    (setq topiary/hl-current-kill-region-overlay overlay)
    overlay))

(defun topiary/hl-current-kill-region-overlay-hook ()
  "Post-Command-Hook for highlighting current kill region."
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

(defun topiary/smart-quote ()
  "If previous and next character wrap in double quotes.
If previous character is a alphanumeric insert single quote.
If next character is a alphanumeric or an opening paren insert single quote.
Otherwise insert double quote."
  (interactive)
  (let ((b-char (char-to-string (char-before)))
        (a-char (char-to-string (char-after))))
    (cond
     ((and (string-match "[[:alnum:]-_/:]" b-char)
           (string-match "[[:alnum:]-_/(?!:]" a-char))  (topiary/wrap-with "\"" "\""))
     ((or  (string-match "[[:alnum:]]" b-char)
           (string-match "[[:alnum:](]" a-char))  (insert "'"))
     (t (topiary/insert-pair "\"\"")))))

(provide 'topiary)
;;; topiary.el ends here
