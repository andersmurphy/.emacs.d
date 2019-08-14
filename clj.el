;;; clj.el --- Clojure RELP powered development  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'edn)
(require 'dash)

(defun my/clj-symbol-at-point ()
  "Get Clojure symbol at point."
  (with-syntax-table clojure-mode-syntax-table
    (symbol-at-point)))

(defun my/clj-eval (command)
  "Evaluate elisp representation of COMMAND."
  (-> (edn-print-string command)
      lisp-eval-string))

(defun my/clj-get-current-namespace-symbol ()
  "Get symbol for current buffer namespace."
  (save-excursion
    (goto-char (point-min))
    (let ((ns-idx (re-search-forward clojure-namespace-name-regex nil t)))
      (when ns-idx
        (goto-char ns-idx)
        (my/clj-symbol-at-point)))))

(defun my/clj-format-with-ns (command)
  "Format edn COMMAND to use the current buffer namespace.
If buffer doesn't have namespace defaults to current namespace.
Handles both string and edn commands."
  (let ((ns (my/clj-get-current-namespace-symbol))
        (string (if (stringp command)
                    command
                  (edn-print-string command))))
    (format "(do
               (when-not (find-ns '%s) (require '%s))
               (binding [*ns* (or (find-ns '%s) *ns*)]
                 (eval '%s)))"
            ns ns ns string)))

(defun my/clj-eval-with-ns (command)
  "Evaluate COMMAND in the context of the current buffer namespace.
If buffer doesn't have namespace defaults to current namespace."
  (-> (my/clj-format-with-ns command)
      lisp-eval-string))

(defun my/clj-get-last-sexp ()
  "Get last sexp as STRING."
  (interactive)
  (buffer-substring (save-excursion (backward-sexp) (point)) (point)))

(defun my/clj-eval-last-sexp-with-ns ()
  "Evaluate previous sexp in the context of the current buffer namespace.
If buffer doesn't have namespace defaults to current namespace."
  (interactive)
  (my/when-repl-running
   (my/clj-eval-with-ns (my/clj-get-last-sexp))
   (my/show-repl)))

(defun my/->boolean (value)
  "Convert turthy/falsy VALUE to boolean."
  (if value t nil))

(defun my/inferior-lisp-program-heroku-p ()
  "Return non-nil if heroku REPL is running."
  (my/->boolean
   (string-match-p "heroku" inferior-lisp-program)))

(defun my/t->true-sym (value)
  "Return symbol true if VALUE is t."
  (if (and (booleanp value) value) 'true  value))

(defun my/configure-repl ()
  "Configure global repl settings."
  (my/clj-eval `(do
                 (when-not ,(my/t->true-sym
                             (my/inferior-lisp-program-heroku-p))
                           (require (quote [pjstadig.humane-test-output]))
                           (eval '(pjstadig.humane-test-output/activate!)))
                 (set! *print-length* 30)
                 (clojure.main/repl :print (fn [x]
                                               (newline)
                                               (clojure.pprint/pprint x))))))

(defun my/do-on-first-prompt (thunk)
  "Evaluate THUNK on first REPL prompt."
  (let ((sym  (gensym)))
    (defalias sym (lambda (output)
                    (when (string-match "^[^=>]*[=>] *" output)
                      (and thunk (funcall thunk))
                      (remove-hook 'comint-output-filter-functions
                                   sym))))
    (add-hook 'comint-output-filter-functions
              sym)))

(defun my/show-repl ()
  "Show running REPL in buffer that is not the current buffer."
  (interactive)
  (when (get-buffer "*inferior-lisp*")
    (unless (string= (buffer-name) "*inferior-lisp*")
      (display-buffer "*inferior-lisp*" t))
    (other-window 1)
    (comint-show-maximum-output)
    (other-window 1)))

(defun my/dir-contains-git-root-p (dirname)
  "Return non-nil if DIRNAME has /.git/config."
  (file-exists-p (concat dirname "/.git/config")))

(defun my/try-to-find-project-file (dirname &optional clj-lisp-prog)
  "Will try to find the correct project root project.clj/deps.edn file.
In the case of nested projects will find the nearest project.clj/deps.edn file.
Works up directories starting from the current files directory DIRNAME. Optionally CLJ-LISP-PROG can be specified."
  (cond
   ((file-exists-p (concat dirname "project.clj"))
    (list (concat dirname "project.clj")
          (or clj-lisp-prog "lein repl")))
   ((file-exists-p (concat dirname "deps.edn"))
    (list (concat dirname "deps.edn")
          "clojure"))
   ((or (my/dir-contains-git-root-p dirname)
        (string= "/" dirname))
    (list (buffer-file-name) "clojure"))
   (t (-> (directory-file-name dirname)
          file-name-directory
          (my/try-to-find-project-file clj-lisp-prog)))))

(defun my/try-to-open-clj-project-file (&optional clj-lisp-prog)
  "Will try to open the correct project root project.clj/deps.edn file.
In the case of nested projects will open the nearest project.clj/deps.edn file.
Optionally CLJ-LISP-PROG can be specified"
  (unless (get-buffer "*inferior-lisp*")
    (let ((file-and-prog (my/try-to-find-project-file
                          (file-name-directory (buffer-file-name))
                          clj-lisp-prog)))
      (find-file-existing (nth 0 file-and-prog))
      (setq inferior-lisp-program (nth 1 file-and-prog)))))

(defun my/clj-inferior-lisp ()
  "Run REPL. If REPL is not running do first prompt behaviour after launch."
  (interactive)
  (if (get-buffer "*inferior-lisp*")
      (inferior-lisp inferior-lisp-program)
    (progn (my/do-on-first-prompt 'my/configure-repl)
           (inferior-lisp inferior-lisp-program))))

(defun my/clj-open-repl (&optional clj-lisp-prog)
  "Open REPL in window that is not the current buffer. If there is only the current buffer split window right. Will try to find the project root and open the correct REPL type accordingly. Optionally CLJ-LISP-PROG can be specified."
  (interactive)
  (when (one-window-p)
    (split-window-right))
  (display-buffer-use-some-window (current-buffer) nil)
  (other-window 1)
  (previous-buffer)
  (if (string= (buffer-name) "*inferior-lisp*")
      (previous-buffer)
    (progn
      (next-buffer)
      (my/try-to-open-clj-project-file clj-lisp-prog)
      (my/clj-inferior-lisp)
      (comint-show-maximum-output)))
  (other-window 1))

(defun my/kill-inferior-lisp-buffer ()
  "Kill *unferior-lisp* buffer if running."
  (when (get-buffer "*inferior-lisp*")
    (kill-buffer "*inferior-lisp*")))

(defun my/start-repl (clj-lisp-prog)
  "Kill any running REPL and start new REPL for CLJ-LISP-PROG."
  (my/kill-inferior-lisp-buffer)
  (my/clj-open-repl clj-lisp-prog))

(defmacro my/when-repl-running (&rest forms)
  "Evaluate FORMS if REPL is running. Otherwise show error message."
  `(if (get-buffer "*inferior-lisp*")
       (progn
         ,@forms)
     (message "REPL needs to be running for this command to work!")))

(defun heroku-production-repl ()
  "Start heroku production REPL."
  (interactive)
  (my/start-repl "heroku run lein repl --remote production"))

(defun heroku-staging-repl ()
  "Start heroku staging REPL."
  (interactive)
  (my/start-repl "heroku run lein repl --remote staging"))

(defun my/clj-doc-for-symbol ()
  "Print doc for symbol at point."
  (interactive)
  (my/when-repl-running
   (my/clj-eval-with-ns
    `(clojure.repl/doc ,(my/clj-symbol-at-point)))
   (my/show-repl)))

(defun my/clj-source-for-symbol ()
  "Print source for symbol at point."
  (interactive)
  (my/when-repl-running
   (my/clj-eval-with-ns
    `(clojure.repl/source ,(my/clj-symbol-at-point)))
   (my/show-repl)))

(defun my/clj-javadoc-for-symbol ()
  "Open javadoc in browser for symbol at point."
  (interactive)
  (my/when-repl-running
   (my/clj-eval-with-ns
    `(clojure.java.javadoc/javadoc ,(my/clj-symbol-at-point)))
   (my/show-repl)))

(defun my/clj-load-current-ns ()
  "Load current buffer namespace."
  (interactive)
  (save-buffer)
  (my/when-repl-running
   (let ((sym (my/clj-get-current-namespace-symbol)))
     (my/clj-eval `(do (require ',sym :reload)
                       (in-ns ',sym))))
   (message "Namespace loaded.")))

(defun my/clj-eval-buffer ()
  "Evaluate entire buffer in REPL."
  (interactive)
  (my/when-repl-running
   (lisp-eval-region (point-min) (point-max))
   (my/show-repl)))

(defun my/clj-run-ns-tests ()
  "Run all unit-tests for namepsace. Reloads both namespace and test namespace.
Works from both namespace and test namespace"
  (interactive)
  (save-some-buffers t)
  (my/when-repl-running
   (if (not (my/inferior-lisp-program-heroku-p))
       (let* ((sym (my/clj-get-current-namespace-symbol))
              (sym-name (symbol-name sym))
              (ns (make-symbol
                   (if (string-suffix-p "test" sym-name)
                       (replace-regexp-in-string "-test" "" sym-name)
                     sym-name)))
              (test-ns (make-symbol
                        (if (string-suffix-p "test" sym-name)
                            sym-name
                          (concat sym-name "-test")))))
         (my/clj-eval `(do (require ',ns :reload)
                           (require ',test-ns :reload)
                           (binding
                            [clojure.test/*stack-trace-depth* 10]
                            (clojure.test/run-tests ',test-ns))))
         (my/show-repl))
     (message "Command disabled: You shouldn't run tests on Heroku!"))))

(defun my/clj-run-project-tests ()
  "Run all unit-tests for project. Reloads all test and test namespaces."
  (interactive)
  (save-some-buffers t)
  (my/when-repl-running
   (if (not (my/inferior-lisp-program-heroku-p))
       (progn
         (my/clj-eval `(do (require '[clojure.test])
                           (require '[clojure.tools.namespace.find])
                           (require '[clojure.tools.namespace.repl])
                           (clojure.tools.namespace.repl/set-refresh-dirs "src" "test")
                           (clojure.tools.namespace.repl/refresh)
                           (time
                            (binding
                             [clojure.test/*stack-trace-depth* 10]
                             (->> (map clojure.java.io/file (set ["test"]))
                                  (mapcat clojure.tools.namespace.find/find-namespaces-in-dir)
                                  (map (fn [ns] (require ns) ns))
                                  (apply clojure.test/run-tests))))))
         (my/show-repl))
     (message "Command disabled: You shouldn't run tests on Heroku!"))))

(defun my/clj-comment-form ()
  "Comment or uncomment current form using #_ reader macro."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (if bounds
        (progn (goto-char (car bounds))
               (search-backward "\"" (- (point) 1) t))
      (clojure-backward-logical-sexp))
    (if (search-forward "#_" (+ (point) 2) t)
        (delete-char -2)
      (insert "#_")
      (backward-char 2))))

(defun my/clj-apropos ()
  "Given a regular expression return a list of all definitions in all currently loaded namespaces that match."
  (interactive)
  (my/when-repl-running
   (my/clj-eval `(clojure.repl/apropos
                  (re-pattern ,(read-string "Apropos (regex):"))))
   (my/show-repl)))

(defun my/clj-find-doc ()
  "Given a regular expression print documentation for any vars whose documentation or name contain a match."
  (interactive)
  (my/when-repl-running
   (my/clj-eval `(clojure.repl/find-doc
                  (re-pattern ,(read-string "Find Doc (regex):"))))
   (my/show-repl)))

(defun my/clj-find-implementation-or-test (file-name)
  "Find coresponding test or implementation file for FILE-NAME."
  (unless file-name (error "The current buffer is not visiting a file"))
  (if (string-suffix-p "test" (file-name-sans-extension
                               (file-name-nondirectory file-name)))
      (replace-regexp-in-string
       "_test" "" (replace-regexp-in-string
                   "test/" "src/" file-name))
    (replace-regexp-in-string
     "src/" "test/" (replace-regexp-in-string ".clj" "_test.clj" file-name))))

(defun my/clj-toggle-between-implementation-and-test ()
  "Toggle between implementation and test files. Reuses current window."
  (interactive)
  (-> (buffer-file-name)
      my/clj-find-implementation-or-test
      find-file))

(defun my/check-first-item-string (list)
  "Return LIST if first item is a string."
  (when (stringp (car list))
    list))

(defun my/clj-run-command-read-edn-output (output-buffer command)
  (let ((proc (inferior-lisp-proc))
        (formatted-command (my/clj-format-with-ns command)))
    (save-excursion
      (set-buffer (get-buffer-create output-buffer))
      (erase-buffer)
      (comint-redirect-send-command-to-process
       formatted-command output-buffer proc nil t)
      (set-buffer (process-buffer proc))
      (while (null comint-redirect-completed)
        (accept-process-output nil 1))
      (set-buffer output-buffer)
      (-> (buffer-substring-no-properties (point-min) (point-max))
          string-trim
          read))))

(defun my/clj-get-file-source-path (file)
  "Return path to FILE source."
  (let ((source-dir (if (string-match-p "test" file) "test" "src")))
    (-> (split-string
         (buffer-file-name)
         "src\\|test")
        car
        (concat source-dir "/" file))))

(defun my/clj-jump (list)
  "Jump to line and column in file."
  (if list
      (let ((col  (nth 0 list))
            (file (nth 1 list))
            (line (nth 2 list)))
        (xref-push-marker-stack)
        (-> (my/clj-get-file-source-path file)
            find-file-existing)
        (goto-line line)
        (forward-char col))
    (message "Symbol definition not found!")))

(defun my/clj-jump-to-symbol ()
  "Jump to symbol definition. If symbol is defined in another file, open that file in a buffer and go to the definition line and column."
  (interactive)
  (my/when-repl-running
   (-> (my/clj-run-command-read-edn-output
        "*my/clj-jump*"
        `(-> (var ,(my/clj-symbol-at-point))
             meta
             (select-keys [:file :line :column])
             seq
             sort
             ((partial map second))))
       my/clj-jump)))

(defun my/clj-jump-back ()
  "Return to point before jump."
  (interactive)
  (xref-pop-marker-stack))

(defun my/clj-completions (prefix)
  "Completion function that passes PREFIX to function to compliment.
Uses the namepsace of the current buffer. If buffer doesn't have namespace
defaults to current namespace."
  (-> (my/clj-run-command-read-edn-output
       "*my/clj-completions*"
       `(do (require '[compliment.core])
            (compliment.core/completions
             ,prefix
             {:plain-candidates true})))
      my/check-first-item-string))

(defun my/clj-completion-backend (command &optional arg &rest ignored)
  "Completion backend powered by the clojure compliment completion library."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'my/clj-completion-backend))
    (prefix (and (eq major-mode 'clojure-mode)
                 (comint-check-proc "*inferior-lisp*")
                 (not (my/inferior-lisp-program-heroku-p))
                 (company-grab-symbol)))
    (candidates (my/clj-completions arg))
    (sorted t)))

(defvar my/clj-warn-on-reflection-state nil)
(defun my/clj-toggle-warn-on-reflection ()
  "Toggle warn on reflection."
  (interactive)
  (my/when-repl-running
   (if my/clj-warn-on-reflection-state
       (progn
         (my/clj-eval `(do (set! *warn-on-reflection* false)
                           {'*warn-on-reflection* false}))
         (setq my/clj-warn-on-reflection-state nil))
     (progn
       (my/clj-eval `(do (set! *warn-on-reflection* true)
                         {'*warn-on-reflection* true}))
       (setq my/clj-warn-on-reflection-state t)))))


(add-hook 'inferior-lisp-mode-hook
          (lambda()
            ;; Add font lock to clojure keywords in REPL
            (font-lock-add-keywords 'inferior-lisp-mode
                                    clojure-font-lock-keywords 'end)
            ;; Enable smartparen mode in REPL (none strict)
            (smartparens-mode)))

(defun my/clj-before-save ()
  "Vertically align the contents of the sexp around point on save."
  (when (eq major-mode 'clojure-mode)
    (clojure-align (point-min) (point-max))))

(add-hook 'before-save-hook 'my/clj-before-save)

(defun my/require-test-src-file ()
  "Used for generating test templates."
  (concat "["
          (->> (clojure-expected-ns)
               (replace-regexp-in-string "-test" ""))
          " :as "
          (->> (buffer-name)
               file-name-sans-extension
               (replace-regexp-in-string "_" "-")
               (replace-regexp-in-string "-test" ""))
          "]"))

(defun my/smart-square-brackets ()
  "Contextually insert [] when typing ()."
  (iteractive)
  (message (my/clj-get-last-sexp)))

(provide 'clj)
;;; clj.el ends here
