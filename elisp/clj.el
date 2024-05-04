;;; clj.el --- Clojure RELP powered development  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;To turn on debugger on error: M-x toggle-debug-on-error

(require 'parseedn)
(require 'inf-lisp)

(defmacro my/when-repl-running (&rest forms)
  "Evaluate FORMS if REPL is running. Otherwise show error message."
  `(if (get-buffer "*inferior-lisp*")
       (progn
         ,@forms)
     (message "REPL needs to be running for this command to work!")))

(defun my/clj-symbol-at-point ()
  "Get Clojure symbol at point."
  (with-syntax-table clojure-mode-syntax-table
    (symbol-at-point)))

(defun my/clj-eval (command)
  "Evaluate elisp representation of COMMAND or string."
  (thread-first
    (if (stringp command)
        command
      (parseedn-print-str command))
    lisp-eval-string))

(defun my/clj-get-current-namespace-symbol ()
  "Get symbol for current buffer namespace."
  (save-excursion
    (goto-char (point-min))
    (let ((ns-idx (re-search-forward clojure-namespace-name-regex nil t)))
      (when ns-idx
        (goto-char ns-idx)
        (my/clj-symbol-at-point)))))

(defun my/clj-get-last-sexp ()
  "Get last sexp as STRING."
  (interactive)
  (buffer-substring (save-excursion (backward-sexp) (point)) (point)))

(defun my/clj-format-with-ns (command)
  "Format edn COMMAND to use the current buffer namespace.
If buffer doesn't have namespace defaults to current namespace.
Handles both string and edn commands."
  (let ((ns (my/clj-get-current-namespace-symbol))
        (string (if (stringp command)
                    command
                  (parseedn-print-str command))))
    (if ns
        (format "(do
               (when-not (find-ns '%s) (require '%s))
               (binding [*ns* (or (find-ns '%s) *ns*)]
                 (eval '%s)))"
                ns ns ns string)
      (format "(eval '%s)" string))))

(defun my/clj-eval-with-ns (command)
  "Evaluate COMMAND in the context of the current buffer namespace.
If buffer doesn't have namespace defaults to current namespace."
  (thread-first
    (my/clj-format-with-ns command)
    lisp-eval-string))

(defun my/clj-eval-last-sexp ()
  "Evaluate previous sexp."
  (interactive)
  (cond
   ((member major-mode '(clojure-mode clojurec-mode))
    (my/when-repl-running
     (my/clj-eval-with-ns (my/clj-get-last-sexp))
     (my/show-repl)))
   ((eq major-mode 'clojurescript-mode)
    (my/when-repl-running
     (my/clj-eval (my/clj-get-last-sexp))
     (my/show-repl)))
   (t (message "Mode not supported."))))

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

(defun my/configure-clj-repl ()
  "Configure global repl settings."
  (my/clj-eval
   `(do
     (require (quote [pjstadig.humane-test-output]))
     (eval '(pjstadig.humane-test-output/activate!))
     (set! *warn-on-reflection* true)
     (set! *print-length* 30)
     (clojure.main/repl :print (fn [x]
                                   (newline)
                                   (clojure.pprint/pprint x)
                                   (newline))))))

(defun my/configure-bb-repl ()
  "Configure global repl settings."
  (my/clj-eval
   `(do
     (set! *warn-on-reflection* true)
     (set! *print-length* 30)
     (clojure.main/repl :print (fn [x]
                                   (newline)
                                   (clojure.pprint/pprint x)
                                   (newline))))))

(defun my/configure-heroku-repl ()
  "Configure global repl settings."
  (my/clj-eval
   `(do
     (set! *print-length* 30)
     (clojure.main/repl :print (fn [x]
                                   (newline)
                                   (clojure.pprint/pprint x)
                                   (newline))))))

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
In the case of nested projects will find the nearest
project.clj/deps.edn file. Works up directories starting from the
current files directory DIRNAME. Optionally CLJ-LISP-PROG can be specified."
  (cond
   ;; deps
   ((file-exists-p (concat dirname "deps.edn"))
    (list (concat dirname "deps.edn")
          (or clj-lisp-prog "clojure -M:user:dev -r")))
   ;; lein
   ((file-exists-p (concat dirname "project.clj"))
    (list (concat dirname "project.clj")
          (or clj-lisp-prog "lein repl")))
   ;; babashka
   ((file-exists-p (concat dirname "bb.edn"))
    (list (concat dirname "bb.edn")
          (or clj-lisp-prog "bb repl")))
   ;; Fall back to babashka if no project file
   ((or (my/dir-contains-git-root-p dirname)
        (string= "/" dirname))
    (list (buffer-file-name) "bb repl"))
   (t (thread-first
        (directory-file-name dirname)
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

(defun my/configure-cljs-repl ()
  "Configure global repl settings.")

(defun my/clj-inferior-lisp (&optional mode)
  "Run REPL. If REPL is not running do first prompt behaviour after launch.
MODE determines dispatch on dialect eg: clojure/clojurescript."
  (interactive)
  (if (get-buffer "*inferior-lisp*")
      (inferior-lisp inferior-lisp-program)
    (progn (my/do-on-first-prompt
            (lambda () (cond ((eq mode 'clojurescript-mode)
                              (my/configure-cljs-repl))
                             ((string-match-p "bb"
                                              inferior-lisp-program)
                              (my/configure-bb-repl))
                             ((string-match-p "heroku"
                                              inferior-lisp-program)
                              (my/configure-heroku-repl))
                             (t (my/configure-clj-repl)))))
           (inferior-lisp inferior-lisp-program))))

(defun my/clj-open-repl (&optional clj-lisp-prog)
  "Open REPL in window that is not the current buffer.
If there is only the current buffer split window right. Will try
to find the project root and open the correct REPL type
accordingly. Optionally CLJ-LISP-PROG can be specified."
  (interactive)
  (let ((mode major-mode))
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
        (my/clj-inferior-lisp mode)
        (comint-show-maximum-output)))
    (other-window 1)))

(defun my/clj-nrepl-connect ()
  "Connect to running nrpel instance."
  (interactive)
  (my/clj-open-repl
   (format
    "clojure -Sdeps '{:deps {reply/reply {:mvn/version \"0.5.1\"}}}' -M -m reply.main --attach %s"
    (read-string "nrepl port: " "9001"))))

(defun my/kill-inferior-lisp-buffer ()
  "Kill *inferior-lisp* buffer if running."
  (when (get-buffer "*inferior-lisp*")
    (kill-buffer "*inferior-lisp*")))

(defun my/start-repl (clj-lisp-prog)
  "Kill any running REPL and start new REPL for CLJ-LISP-PROG."
  (my/kill-inferior-lisp-buffer)
  (my/clj-open-repl clj-lisp-prog))

(defun my/try-to-find-git-root (dirname)
  "Will try and find the nearest root for project.
Works up directories starting from the current files directory DIRNAME."
  (cond
   ((or (my/dir-contains-git-root-p dirname)
        (string= "/" dirname))
    (directory-file-name dirname))
   (t (thread-first
        (directory-file-name dirname)
        file-name-directory
        my/try-to-find-git-root))))

(defun my/clj-doc-for-symbol ()
  "Print doc for symbol at point."
  (interactive)
  (my/when-repl-running
   (cond ((member major-mode '(clojure-mode clojurec-mode))
          (my/clj-eval-with-ns
           `(do
             (newline)
             (newline)
             (clojure.repl/doc ,(my/clj-symbol-at-point)))))
         ((eq major-mode 'clojurescript-mode)
          (my/clj-eval `(do
                         (newline)
                         (newline)
                         (cljs.repl/doc ,(my/clj-symbol-at-point))))))
   (my/show-repl)))

(defun my/clj-apropos ()
  "Given a regex return a list of defs in loaded namespaces that match."
  (interactive)
  (my/when-repl-running
   (cond
    ((member major-mode '(clojure-mode clojurec-mode))
     (my/clj-eval-with-ns
    `(clojure.repl/apropos
      (re-pattern ,(read-string "Apropos (regex):")))))
    ((eq major-mode 'clojurescript-mode)
     (my/clj-eval
      `(cljs.repl/apropos
             (re-pattern ,(read-string "Apropos (regex):"))))))
   (my/show-repl)))

(defun my/clj-sync-deps ()
  "Sync deps file. Not currently supported in clj."
  (interactive)
  (my/when-repl-running
   (cond
    ((member major-mode '(clojure-mode clojurec-mode))
     (my/clj-eval-with-ns
      `(clojure.repl.deps/sync-deps))))
   (my/show-repl)))

(defun my/clj-find-doc ()
  "Given a regex print doc for any vars whose doc or name contain a match."
  (interactive)
  (my/when-repl-running
   (cond
    ((member major-mode '(clojure-mode clojurec-mode))
     (my/clj-eval-with-ns
    `(clojure.repl/find-doc
      (re-pattern ,(read-string "Find Doc (regex):")))))
    ((eq major-mode 'clojurescript-mode)
     (my/clj-eval
      `(cljs.repl/find-doc
             (re-pattern ,(read-string "Find Doc (regex):"))))))
   (my/show-repl)))

(defun my/clj-source-for-symbol ()
  "Show source for symbol at point."
  (interactive)
  (my/when-repl-running
   (cond
    ((member major-mode '(clojure-mode clojurec-mode))
     (my/clj-eval-with-ns
      `(clojure.repl/source ,(my/clj-symbol-at-point))))
    ((eq major-mode 'clojurescript-mode)
     (my/clj-eval
      `(cljs.repl/source ,(my/clj-symbol-at-point)))))
   (my/show-repl)))

(defun my/clj-load-current-ns ()
  "Load current buffer namespace."
  (interactive)
  (save-buffer)
  (my/when-repl-running
   (let ((sym (my/clj-get-current-namespace-symbol)))
     ;; temp user ns prevents namespaces getting dirty
     ;; might not be needed
     (my/clj-eval
      (cond ((member major-mode '(clojure-mode clojurec-mode))
             `(do (in-ns 'user)
                  (when-not (find-ns ',sym)
                            ;; require can take :reload
                            ;; to force a reload of the
                            ;; on disk file
                            (require ',sym)
                            nil)
                  (in-ns ',sym)))
            ((eq major-mode 'clojurescript-mode)
             `(in-ns ',sym)))))
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
  (thread-first (buffer-file-name)
      my/clj-find-implementation-or-test
      find-file))

(defun my/clj-create-new-deps-project ()
  "Create a new deps.edn project."
  (interactive)
  (let* ((project-name-path (read-directory-name "Directory:"))
         ;; Program development is easier when everything
         ;; uses the same project namespace. Means
         ;; project renames etc are less of a pain.
         ;; (not true for library development).
         (namespace-name "server"))
    (make-directory project-name-path)
    (find-file (concat project-name-path "/deps.edn"))
    (insert "{:paths [\"src\"]
 :deps {org.clojure/clojure {:mvn/version \"1.12.0-alpha11\"}}
 :aliases {}}")
    (save-buffer)
    (make-directory (concat project-name-path "/src"))
    (make-directory (concat project-name-path "/src/" namespace-name))
    (find-file (concat project-name-path "/src/" namespace-name
                       "/core.clj"))
    (clojure-insert-ns-form)
    (save-buffer)
    (find-file (concat project-name-path "/deps.edn"))))

(add-hook 'inferior-lisp-mode-hook
          (lambda()
            ;; Use clojure syntax table if available
            (ignore-errors
              (set-syntax-table clojure-mode-syntax-table))))

(provide 'clj)
;;; clj.el ends here
