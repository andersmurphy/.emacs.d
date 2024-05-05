;;; init.el --- Initialization file for Emacs -*- lexical-binding:t -*-

;;; Commentary:

;; Emacs Startup File --- initialisation for Emacs

;; C-x C-e to evaluate current expression.
;; M-. to navigate to function source.
;; C-c C-d to navigate to function docs.
;; M-x consult-info-emacs

;;; Code:

;;; PACKAGE MANAGER
(progn ;;; Setup

  ;; To not increase Emacs startup time, check package modifications when
  ;; packages edited (with Emacs), instead of checking modifications at startup.
  (setq straight-check-for-modifications
        '(check-on-save find-when-checking))
  ;; Use default depth of 1 when cloning files with git to get
  ;; saves network bandwidth and disk space.
  (setq straight-vc-git-default-clone-depth 1)

  ;; Bootstrap straight.el package manager
  ;; https://github.com/raxod502/straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el"
                           user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Always load newest byte code
  (setq load-prefer-newer t)

  ;; Bootstrap use-package
  ;; use-package is used to configure the rest of the packages.
  (defvar straight-use-package-by-default)
  (setq straight-use-package-by-default t)

  ;; Lazy
  (setq use-package-always-defer t)

  ;; Profiling - M-x use-package-report
  ;; (setq use-package-compute-statistics t)

  ;; Forces Custom to save all customizations in a separate file
  (setq custom-file "~/.emacs.d/custom.el"))

;;; PATH - (needs to be called as the first use-package)
(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns x))
  :init
  (exec-path-from-shell-initialize))

;;; EMACS LISP LIBRARIES
(defmacro comment (&rest _)
  "Ignore BODY, yields nil."
  nil)
(use-package parseedn)  ;; edn parsing
(use-package pcre2el) ;; regex conversion

;;; GENERAL
(use-package emacs
  :straight nil
  :init
  :config
  ;; General settings
  (setq-default
   use-short-answers t
   confirm-kill-emacs 'y-or-n-p
   auto-save-default nil
   auto-save-list-file-prefix nil
   create-lockfiles nil
   history-length 500
   make-backup-files nil
   backup-inhibited t
   enable-recursive-minibuffers t
   max-mini-window-height 1
   ring-bell-function 'ignore
   use-dialog-box nil
   vc-follow-symlinks t
   large-file-warning-threshold 100000000
   recenter-positions '(top middle bottom)
   set-mark-command-repeat-pop 't
   history-delete-duplicates t
   echo-keystrokes 0.1
   set-message-functions '(inhibit-message set-minibuffer-message)
   inhibit-message-regexps '(".*recentf.*")
   mac-command-modifier 'control
   indent-tabs-mode nil
   tab-width 2
   tab-always-indent 'complete
   sentence-end-double-space nil)

  ;; force pin entry through emacs
  (setenv "GPG_AGENT_INFO" nil)
  (setq epg-pinentry-mode 'loopback
        epa-file-select-keys nil)

  ;; Use Utf-8 encoding.
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system        'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq-default locale-coding-system      'utf-8
                buffer-file-coding-system 'utf-8)

  ;; Display help in same window
  (add-to-list 'display-buffer-alist
               '("*Help*" display-buffer-same-window))

  ;; Add alias for insert-kbd-macro and call-last-kbd-macro that
  ;; matches kmacro naming of other commands.
  (defalias 'kmacro-insert-macro 'insert-kbd-macro)
  (defalias 'kmacro-call-last-macro 'call-last-kbd-macro)

  (defun my/init ()
    "Open init file (this file)."
    (interactive)
    (find-file "~/.emacs.d/init.el"))

  (defun my/update-emacs-packages ()
    "Update Emacs packages using straight."
    (interactive)
    (straight-check-all)
    (straight-pull-all)
    (straight-freeze-versions))

  (defun my/reload-init ()
    "Reload init."
    (interactive)
    (save-buffer)
    (load  "~/.emacs.d/init.el"))

  (defun my/zoom-in ()
    "Zoom in all buffers."
    (interactive)
    (set-face-attribute
     'default nil
     :height (+ (face-attribute 'default :height) 10))
    (when (eq major-mode 'nov-mode)
      (my/nov-rerender-without-losing-point))
    (when (eq major-mode 'eww-mode)
      (eww-reload t)))

  (defun my/zoom-out ()
    "Zoom out all buffers."
    (interactive)
    (set-face-attribute
     'default nil
     :height (- (face-attribute 'default :height) 10))
    (when (eq major-mode 'nov-mode)
      (my/nov-rerender-without-losing-point))
    (when (eq major-mode 'eww-mode)
      (eww-reload t)))

  (defun my/dedupe-kill (args)
    "Prevent sequential duplicate items in kill ring."
    (let ((string (car args))
          (replace (cdr args))
          (last (car-safe kill-ring)))
      (when (equal last string)
        (setq replace t))
      (list string replace)))
  (advice-add 'kill-new :filter-args #'my/dedupe-kill)

  (defun my/exchange-point-and-mark-no-region ()
    "Identical to \\[exchange-point-and-mark] but will not activate the region."
    (interactive)
    (exchange-point-and-mark)
    (deactivate-mark nil))

  ;; Unbind suspend-frame.
  ;; This would cause the cursor to disappear if you pressed C-x C-z
  ;; by mistake.
  (global-unset-key (kbd "C-x C-z"))
  ;; Unbind tmm-menubar as I never use it.
  (global-unset-key (kbd "M-`"))
  ;; Unbind scroll down as I never use it.
  ;; Scroll up is also unbound (C-v is bound to something else).
  (global-unset-key (kbd "M-v"))
  ;; Translations
  (define-key key-translation-map ":" ";")
  (define-key key-translation-map ";" ":")
  (define-key key-translation-map (kbd "RET") (kbd "C-m"))
  :bind ((:map global-map
               ("C-x f"   . find-file)
               ("C-x C-f" . find-file)
               ("C-z"     . undo)
               ("C-x d"   . dired)
               ("C-x C-d" . dired)
               ("M-c"     . org-capture)
               ("C-o"     . my/other-window)
               ("C-x o"   . my/other-window)
               ("C-x k"   . kill-this-buffer)
               ("C-x 2"   . (lambda () (interactive)
                              (split-window-sensibly)
                              (other-window 1)))
               ("C-x 3"   . (lambda () (interactive)
                              (split-window-sensibly)
                              (other-window 1)))
               ("C-x -"   . my/zoom-out)
               ("C-x C--" . my/zoom-out)
               ("C-x +"   . my/zoom-in)
               ("C-x C-+" . my/zoom-in)
               ;; C-M-\ is a bit awkward and C-\ (toggle input method)
               ;; is not something I use.
               ("C-\\"    . indent-region)
               ([remap exchange-point-and-mark] .
                my/exchange-point-and-mark-no-region))
         (:map minibuffer-local-map
               ("C-v" . topiary/yank)
               ("C-y" . topiary/yank)
               ("C-w" . topiary/kill)
               ("C-o" . my/other-window)))
  :hook (after-init . (lambda ()
                        ;; Enable visual line mode
                        (global-visual-line-mode)
                        ;; Enable auto revert
                        (global-auto-revert-mode))))
(use-package elisp-mode
  :straight nil
  :config
  (defun my/docs-for-elisp-symbol-at-point ()
    "Show docs for elisp symbol at point."
    (interactive)
    (describe-function (symbol-at-point)))

  ;; Use function name face on use package declarations
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(use-package\\)\\_>[   ']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (2 font-lock-function-name-face nil t))))

  :bind (:map emacs-lisp-mode-map
              ("C-c C-d" . my/docs-for-elisp-symbol-at-point)
              :map lisp-interaction-mode-map
              ("C-c C-d" . my/docs-for-elisp-symbol-at-point)))
(use-package startup
  :straight nil
  :init
  ;; Initial scratch message.
  (setq initial-scratch-message
        ";; C-x C-e to evaluate current expression.
;; M-. to navigate to function source.
;; C-c C-d to navigate to function docs.
;; M-x consult-info-emacs to search Emacs/Elisp manuals.
;; M-x shortdoc-display-group to get elisp cheat sheet by category.\n\n")
  ;; This delays flymake until after initialisation
  (setq initial-major-mode nil)
  :hook (after-init . (lambda () (with-current-buffer "*scratch*"
                                   (setq initial-major-mode
                                         'lisp-interaction-mode)
                                   (lisp-interaction-mode)))))
(use-package window
  :straight nil
  :config
  (setq split-width-threshold 100)

  (defun my/other-window ()
    "Switch to another window. If no other window exists create one.
     If in minibuffer close."
    (interactive)
    (if (window-minibuffer-p)
        (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
          (abort-recursive-edit))
      (progn
        (when (one-window-p)
          (split-window-sensibly))
        (other-window 1))))

  (defun my/reset-window-layout (_)
    "Reset window layout on width change. Triggers width threshold.
This can be used to make the window layout change based on frame size."
    (unless (or (= (window-old-pixel-width) (window-pixel-width))
                (one-window-p)
                (not (frame-size-changed-p)))
      (let* ((_ (other-window 1))
             (other-buff (buffer-name)))
        (delete-window)
        (split-window-sensibly)
        (other-window 1)
        (switch-to-buffer other-buff)
        (other-window 1))))

  ;;  Automatically change window layout on frame resize.
  (setq window-size-change-functions '(my/reset-window-layout))
  :hook (after-init . (lambda ()
                        ;; Sets the initial frame to fill the screen.
                        (toggle-frame-fullscreen)
                        (switch-to-buffer "*Messages*"))))
(use-package Info-mode
  :straight nil
  :init
  (defun my/info-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :height 1.2))
  :hook ((Info-mode . variable-pitch-mode)
         (Info-mode . my/info-font-setup)))
(use-package super-save
  :defer 1
  :init
  (setq save-silently t)
  (super-save-mode t))
(use-package bookmark
  :straight nil
  :config
  ;; Save Bookmarks on any change
  (setq bookmark-save-flag 1)
  ;; Store bookmarks in emacs-sync
  (setq bookmark-default-file "~/.emacs.d/emacs-sync/bookmarks"))
(use-package ls-lisp
  :demand t
  :straight nil
  :config
  ;; Switch to use ls-lisp makes ls platform agnostic
  ;; (need to test with TRAMP).
  (setq ls-lisp-use-insert-directory-program nil))
(use-package dired
  :straight nil
  :config
  ;; Directories first
  (setq dired-listing-switches "--group-directories-first -alh")
  ;; Ensures Dired file lists are refreshed when files are
  ;; created/deleted/renamed.
  ;; Also hides auto revert message.
  (setq dired-auto-revert-buffer t)
  ;; Bind return to alternate file, so that dired reuses same buffer.
  (put 'dired-find-alternate-file 'disabled nil)
  ;; WDired (writable dired) can be accessed by making the dired
  ;; buffer writable  with the binding C-x C-q. Any change you make to
  ;; the buffer will remain  unchanged until you commit them by typing
  ;; C-c C-c. To cancel the changes and revert to the original state
  ;; you can type C-c k.

  ;; The feature bellow force confirmation in the case of potential
  ;; overwrites :caused by rename.
  (setq wdired-confirm-overwrite t)
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file))
  ;; Dired hide details by default
  :hook ((dired-mode . dired-hide-details-mode)))
(use-package so-long
  :straight nil
  :defer 1
  :config
  (global-so-long-mode t))
(use-package kill-buffer-on-q
  ;; Convenience mode for killing buffer on q
  :straight nil
  :load-path "~/.emacs.d/elisp")

;;; PRIVACY
(use-package totp
  :straight nil
  ;; further reading
  ;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
  ;; https://www.masteringemacs.org/article/securely-generating-totp-tokens-emacs
  :defer 1
  :load-path "~/.emacs.d/elisp")
(use-package auth-source
  :straight nil
  :config
  (setq auth-sources (quote ("~/.emacs.d/emacs-sync/.authinfo.gpg"))))

;;; VISUAL
(use-package ligature
  :init
  (ligature-set-ligatures
   t
   '(;;; Disabled ligatures because they make things look weird
     ;; "www" "**" "***" "**/" ;; <- org headings
     ;; ";;"                   ;; <- triple comments ;;;
     ;;  "##" "###" "####"     ;; <- loading bars
     ;;  "----" "++" "+++"     ;; <- diffs/magit
     ;;; Enabled ligatures
     "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
     ":::" ":=" "!!" "!=" "!==" "-}" "-->" "->" "->>"
     "-<" "-<<" "-~" "#{" "#[" "#(" "#?" "#_"
     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" "/*" "/**"
     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
     "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))
(use-package my-theme
  ;; Note: the theme itself is actually loaded in early init
  :straight nil
  :config
  ;;; DYNAMIC THEME

  ;; Hook for after theme load.
  ;; Update the theme of these components on theme change.
  (defvar after-load-theme-hook nil
    "Hook run after a colour theme is loaded using `load-theme'.")

  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))

  (defun my/dim-parens ()
    "Make parenthesis less prominent by matching comment face."
    (font-lock-add-keywords
     nil `((,(rx (any "()")) . 'font-lock-dim-face))))

  (defun my/fade-characters ()
    "Make some characters less prominent."
    (font-lock-add-keywords
     nil `((,(rx (any "[]{}_&#%~@.,")) . 'font-lock-comment-face))))

  (defun my/color-important-words ()
    "Make important words more prominent."
    (font-lock-add-keywords
     nil
     '(("\\b[Ee]rrors?\\b\\|\\b[Ff]ailures?\\b\\|\\b[Ff]ail\\b\\|\\bERRORS?\\b\\|\\bFAILURES?\\b\\|\\bFAIL\\b"
        . 'error)
       ("\\b[Ww]arnings?\\b\\|\\bWARNINGS?\\b"
        . 'warning)
       ("\\b[Ss]uccesse?s?\\b\\|\\b[Pp]ass\\b\\|\\bSUCCESSE?S?\\b\\|\\bPASS\\b"
        . 'success))))

  ;; Messages buffer is started before init is run so we can't use a hook
  (with-current-buffer "*Messages*"
    (my/color-important-words))

  ;; hooks
  (add-hook 'messages-buffer-mode-hook 'my/color-important-words)
  (add-hook 'clojure-mode-hook 'my/dim-parens)
  (add-hook 'emacs-lisp-mode-hook 'my/dim-parens)
  (add-hook 'clojure-mode-hook 'my/fade-characters)
  (add-hook 'emacs-lisp-mode-hook 'my/fade-characters)
  (add-hook 'eshell-mode-hook 'my/color-important-words)
  (add-hook 'inferior-lisp-mode-hook 'my/color-important-words)
  (add-hook 'inferior-lisp-mode-hook 'ansi-color-for-comint-mode-filter)
  (add-hook 'shell-mode-hook 'my/color-important-words)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-filter)
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer))
(use-package my-mode-line
  :straight nil
  :load-path "~/.emacs.d/elisp"
  :hook (after-init . my/mode-line-init))

;;; META NAVIGATION
(use-package recentf
  :defer 1
  :straight nil
  :config
  (setq recentf-exclude
        '(".*\.gpg"
          ".*\.gz)"
          ".emacs.d/emms/history"
          ".emacs.d/emacs-sync/.*"))
  (setq recentf-max-saved-items 10)
  (recentf-mode t))
(use-package isearch
  :straight nil
  :config
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  (setq search-invisible 'open)

  (defun my/isearch-thing-at-point ()
    (interactive)
    (if (string-empty-p isearch-string)
        (let ((bounds (topiary/bounds)))
          (cond
           (bounds
            (when (< (car bounds) (point))
              (goto-char (car bounds)))
            ;; We don't want the region to be active when navigating
            ;; between matches.
            (when (region-active-p)
              (deactivate-mark t))
            (isearch-yank-string
             (buffer-substring-no-properties (car bounds) (cdr bounds))))
           (t
            (setq isearch-error "No thing at point")
            (isearch-push-state)
            (isearch-update))))
      (isearch-update)))

  (defun my/goto-match-end ()
    (when (and (not isearch-forward)
               isearch-other-end
               (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end)))

  (defun my/isearch-repeat-backward ()
    (interactive)
    (let ((previous-point (point)))
      (isearch-repeat-backward)
      (when (not (= (point) isearch-other-end))
        (goto-char isearch-other-end))
      (when (and (not isearch-error)
                 isearch-success
                 (= (point) previous-point))
        (my/isearch-repeat-backward))))

  ;; Make isearch wrap automatically if it doesn't find anything
  (defadvice isearch-search (after isearch-no-fail activate)
    (unless isearch-success
      (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
      (ad-activate 'isearch-search)
      (isearch-repeat (if isearch-forward 'forward))
      (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
      (ad-activate 'isearch-search)))

  :hook (('isearch-mode-end . my/goto-match-end))
  :bind
  (:map isearch-mode-map
        ("DEL" . isearch-del-char)
        ("TAB" . isearch-exit)
        ("C-i" . isearch-toggle-invisible)
        ("M-i" . isearch-toggle-invisible)
        ("C-w" . isearch-del-char)
        ("C-g" . isearch-cancel)
        ("C-n" . isearch-repeat-forward)
        ("C-p" . my/isearch-repeat-backward)
        ("C-s" . my/isearch-thing-at-point)
        ("C-r" . isearch-query-replace)
        ("C-v" . isearch-yank-kill)
        ("C-y" . isearch-yank-kill)))
(use-package vertico
  :ensure t
  :init
  (vertico-mode))
(use-package vertico-prescient
  :after vertico
  :demand t
  :config
  (vertico-prescient-mode t)
  (prescient-persist-mode t))
(use-package corfu-prescient
  :after corfu
  :demand t
  :config
  (corfu-prescient-mode t))
(use-package project
  :straight nil
  :init
  ;; Don't include submodule files in searches etc
  (setq project-vc-merge-submodules nil))
(use-package magit
  :config
  (setq magit-diff-highlight-indentation nil)
  (setq magit-diff-highlight-trailing nil)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-revision-insert-related-refs nil)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk 'all)
  (setq magit-diff-refine-ignore-whitespace t)
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (setq magit-process-find-password-functions
        '(magit-process-password-auth-source))
  (setq magit-process-finish-apply-ansi-colors t)
  ;; Allows q to be used to quit transient buffers
  (transient-bind-q-to-quit)

  (defun my/get-github-token ()
    (funcall
     (plist-get
      (car (auth-source-search :max 1 :host "github.com"))
      :secret)))

  (defun my/get-github-username ()
    (plist-get
     (car (auth-source-search :max 1 :host "github.com"))
     :user))

  (defun my/magit-spin-off-pull-request ()
    "Spin off last commit as a pull request."
    (interactive)
    (when (y-or-n-p "Spin off pull request?")
      (let* ((commit-name (magit-rev-format
                           "%s"
                           (or (magit-get-current-branch) "HEAD")))
             (branch-name (replace-regexp-in-string
                           "\\s-+" "-"
                           (downcase commit-name)))
             (from (car (last (magit-region-values 'commit))))
             (master-name (car (seq-filter (lambda (n) (or (equal n "master") (equal n "main")))
                                           (magit-list-branch-names)))))
        (magit--branch-spinoff branch-name from t)
        (with-environment-variables
            (("GITHUB_TOKEN" (my/get-github-token)))
          (magit-shell-command-topdir
           (concat
            "git push -u origin " branch-name
            ";gh pr create --fill --head " branch-name
            ";git checkout " master-name))))))

  (defun my/magit-create-private-github-remote ()
    "Create a private repo on github, add remote as origin
     and push local commits."
    (interactive)
    (with-environment-variables
        (("GITHUB_TOKEN" (my/get-github-token)))
      (magit-shell-command-topdir
       (concat
        "gh repo create " (my/get-github-username) "/"
        (file-name-nondirectory
         (directory-file-name default-directory))
        " --private --source=. --remote=origin --push"))))

  (defun my/current-pr-number ()
    (number-to-string (oref (forge-current-pullreq) number)))

  (defun my/forge-approve-pull-request ()
    "Approve current pull request."
    (interactive)
    (when-let ((pr-number (my/current-pr-number)))
      (with-environment-variables
          (("GITHUB_TOKEN" (my/get-github-token)))
        (magit-shell-command-topdir
         (concat "gh pr review " pr-number " --approve --body '🧞'")))))

  (defun my/magit-search-git-log-for-change ()
    "Search git log for current symbol or topiary region.

If region spans multiple lines does regex or of each trimmed line.
This effectively returns all changes to that set of lines. Or anything
in the file that matches one of those lines.

Lines containing common patterns that appear throughout the file can
lead to unrelated results. For example (interactive) in this file
would lead to a large number of unrelated results as it's a very
common occurrence.

If this becomes a problem these common lines could be filtered."
    (interactive)
    (let* ((bounds (topiary/compute-bounds))
           (region-str (or
                        (thing-at-point 'symbol t)
                        (buffer-substring (car bounds) (cdr bounds))))
           (pcre-regex (and region-str
                            (concat
                             "("
                             (mapconcat
                              (lambda (line)
                                (concat ".?" (string-trim line) ".?"))
                              (split-string
                               (rxt-elisp-to-pcre
                                (regexp-quote region-str)) "\n")
                              "|")
                             ")"))))
      (if-let ((file (magit-file-relative-name)))
          (magit-log-setup-buffer
           (list (or magit-buffer-refname
                     (magit-get-current-branch)
                     "HEAD"))
           (list  "--follow" (concat "-G " pcre-regex))
           (and file (list file))
           magit-log-buffer-file-locked)
        (user-error "Buffer isn't visiting a file"))))

  :bind (("C-x g" . magit-status))
  :hook ((after-save . magit-after-save-refresh-status)))
(use-package forge
  ;; For generating tokens see: https://github.com/settings/tokens
  :after magit
  :demand t)
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . bold)
          ("EXPLORE" . bold)))
  :hook (prog-mode . hl-todo-mode))
(use-package magit-todos
  :after magit
  :demand t
  :config
  (magit-todos-mode)
  (setq magit-todos-auto-group-items 15)
  (setq magit-todos-group-by '(magit-todos-item-keyword)))
(use-package browse-at-remote
  :init
  (defun my/git-url-for-region ()
    (interactive)
    (browse-at-remote-kill)
    (message "git url for region yanked!")))
(use-package org
  :straight nil
  :config
  ;; Org babel/source blocks
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-adapt-indentation nil)

  (defun my/org-todo-sort ()
    "Sort sections by TODO."
    (interactive)
    (ignore-errors (outline-up-heading 10))
    (org-sort-entries nil ?o)
    (org-cycle)
    (org-cycle))

  ;; Capture templates.
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "~/.emacs.d/emacs-sync/org/tasks.org" "Tasks")
           "* TODO %?"))))
(use-package consult
  :bind
  (("C-x b"   . my/consult-omni)
   ("C-x C-b" . my/consult-omni)
   ("C-x p"   . my/consult-omni)
   ("C-x r b" . consult-bookmark)
   ("C-M-s"   . my/consult-ripgrep)
   ("M-y"     . consult-yank-pop)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("C-h i"   . consult-info))
  :init
  (defun my/consult-project-with-root (root)
    "Return the project for a given project ROOT."
    (project--find-in-directory root))

  (defun my/consult-project-files (root)
    "Compute the project files given the ROOT."
    (let* ((project (my/consult-project-with-root root))
           (files (project-files project)))
      (mapcar (lambda (f) (file-relative-name f root)) files)))

  (defun my/consult-file (selected-root)
    "Create a view for selecting project files for the project at SELECTED-ROOT."
    (let ((candidate (consult--read
                      (my/consult-project-files selected-root)
                      :prompt "Project File: "
                      :sort t
                      :require-match t
                      :category 'file
                      :state (consult--file-preview)
                      :history 'file-name-history)))
      (consult--file-action (concat selected-root candidate))))

  (defun my/consult-find-with-concat-root (candidate)
    "Find-file concatenating root with CANDIDATE."
    (consult--file-action (concat (project-root (project-current)) candidate)))

  (defvar my/consult-source-file
    `(:name "Project File"
            :narrow    (?f . "File")
            :category  file
            :face      consult-file
            :history   file-name-history
            :enabled   ,#'project-current
            :action    ,#'my/consult-find-with-concat-root
            :items
            ,(lambda ()
               (my/consult-project-files (project-root (project-current))))))

  (defvar my/consult-source-project
    `(:name "Known Project"
            :narrow    (?p . "Project")
            :category  file
            :face      consult-file
            :history   file-name-history
            :annotate  ,(lambda (dir)
                          (format "Project: %s"
                                  (file-name-nondirectory (directory-file-name dir))))
            :action    ,#'my/consult-file
            :items     ,#'project-known-project-roots))

  (setq my/consult-omni-sources
        '(consult--source-buffer
          consult--source-recent-file
          consult--source-bookmark
          my/consult-source-file
          my/consult-source-project))

  (defun my/consult-omni ()
    (interactive)
    (consult-buffer my/consult-omni-sources))

  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl" ))

  (defun my/consult-ripgrep (&optional dir)
    "Search with `rg' for files in DIR with INITIAL input.
See `consult-grep' for details."
    (interactive "P")
    (let* ((bounds (topiary/bounds))
           (sym (buffer-substring (car bounds) (cdr bounds))))
      (consult--grep "Ripgrep" #'consult--ripgrep-make-builder dir
                     (and sym (regexp-quote (car (split-string sym "\n")))))))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; Disable preview, to enable set to 'any
  (setq consult-preview-key nil))
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ;; alternative for `describe-bindings'
   ("C-h b" . embark-bindings)
   ("C-h C-h" . embark-prefix-help-command))
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; More minimalist embark (using completion)
  (setq embark-prompter 'embark-completing-read-prompter)
  (setq embark-indicators
      '(embark-minimal-indicator  ; default is embark-mixed-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))
  :config
  ;; Configure embark-dwim actions
  ;; Don't want flymake at point as a target (would rather go to source)
  (delete 'embark-target-flymake-at-point embark-target-finders))

;;; TEXT FORMATTING
(use-package hideshow
  :straight nil
  :config
  (setq hs-hide-comments-when-hiding-all nil)

  (defun my/display-most-sever-flymake-error (ov)
    "Display most sever error in folded code block at top level."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((most-sever-error
              (car (sort (flymake--overlays :beg (overlay-start ov)
                                            :end (overlay-end ov))
                         (lambda (a b) (> (overlay-get a 'severity)
                                          (overlay-get b 'severity))))))
             (level (and most-sever-error
                         (overlay-get most-sever-error 'category)))
             (marker-string (concat "*" (format "%s" level) "*")))

        (if most-sever-error
            (let* ((error-face (overlay-get most-sever-error 'face))
                   (error-face (if (listp error-face)
                                   ;; handle eglot diagnostic sometimes
                                   ;; being returned as list
                                   (car error-face)
                                 error-face)))
              (overlay-put ov 'before-string
                           (propertize marker-string
                                       'display
                                       (list 'left-fringe
                                             'my/flymake-fringe-indicator
                                             error-face))))
          (overlay-put ov 'before-string nil)))))

  (setq hs-set-up-overlay 'my/display-most-sever-flymake-error)

  (defvar my/previous-flymake-errors nil)

  (defun my/toggle-defun-level-hiding ()
    "Toggle folded code at top level without losing cursor position."
    (interactive)
    (save-excursion
      ;; handle being at the end of a defun
      (when (and (char-before ?\))
                 (member (char-after) (string-to-list "\n ")))
        (backward-char 1))
      ;; Handles being inside a defun
      (unless (<= (nth 0 (syntax-ppss)) 0)
        (goto-char (car (nth 9 (syntax-ppss)))))
      (hs-toggle-hiding)))

  (defun my/refresh-folded-code-errors ()
    "Refresh folded code that contains errors to make them visible at the top level."
    (let ((current-flymake-errors (flymake--overlays)))
      (unless (equal current-flymake-errors my/previous-flymake-errors)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlay-get ov 'hs)
            (my/display-most-sever-flymake-error ov)))
        (setq my/previous-flymake-errors current-flymake-errors))))

  (defadvice flymake--handle-report (after refresh-folded-errors activate)
    (my/refresh-folded-code-errors))

  :hook (((emacs-lisp-mode clojure-mode)
          . (lambda ()
              (hs-minor-mode) (hs-hide-all))))

  :bind (:map hs-minor-mode-map
              ("TAB" . my/toggle-defun-level-hiding)
              ("<backtab>" . hs-hide-all)))
(use-package subword
  :straight nil
  :init
  (global-subword-mode))
(use-package topiary
  :straight nil
  :load-path "~/.emacs.d/elisp"
  :hook ((text-mode prog-mode comint-mode outline-mode Info-mode eshell-mode magit-blob-mode) . topiary-mode))
(use-package special-mode
  :straight nil
  :bind (:map special-mode-map
              ("C-w" . topiary/kill)))

;;; WRITING
(use-package text-scratch
  :straight nil
  :load-path "~/.emacs.d/elisp"
  :demand t)
(use-package abbrev
  :straight nil
  :init
  (setq-default abbrev-mode t)
  :config
  (defun my/wiki-style-misspellings->abbrev-table ()
    "Convert wiki style misspellings to abbrev table entries.
https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines"
    (interactive)
    ;; remove entries with more than one option
    (call-interactively 'mark-whole-buffer)
    (flush-lines ",")
    ;; convert entries to abbrev entries
    (while (not (eobp))
      (insert "(\"")
      (skip-chars-forward "^-")
      (insert "\" ")
      (delete-char 2)
      (insert "\"")
      (end-of-line)
      (insert "\" nil :count 1)")
      (forward-line 1))))

;;; LINTING
(use-package flyspell
  :defer 1
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB"))
  ;; Spellchek docs and comments in prog-mode but not strings
  (setq flyspell-prog-text-faces (delq 'font-lock-string-face
                                       flyspell-prog-text-faces))
  (setq ispell-personal-dictionary "~/.emacs.d/setup/dotfiles/.aspell.en.pws")
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))
(use-package flymake
  :straight nil
  :init
  (define-fringe-bitmap 'my/flymake-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b11111111
            #b11111111
            #b11111111
            #b11111111
            #b11111111
            #b11111111
            #b11111111
            #b11111111
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (setq flymake-error-bitmap '(my/flymake-fringe-indicator flymake-error))
  (setq flymake-warning-bitmap '(my/flymake-fringe-indicator flymake-warning))
  (setq flymake-note-bitmap '(my/flymake-fringe-indicator flymake-note))
  :config
  (custom-set-variables
   '(help-at-pt-timer-delay 0.1)
   '(help-at-pt-display-when-idle '(flymake-diagnostic)))
  :hook
  (emacs-lisp-mode . (lambda () (flymake-mode t))))

;;; COMPLETION
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-preview-current nil)
  (setq corfu-bar-width 0)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :init
  (global-corfu-mode)

  :config
  ;; Monkey patch to remove annotation/extra info noise
  (cl-defgeneric corfu--affixate (cands)
    "Annotate CANDS with annotation function."
    (setq cands (cl-loop for cand in cands collect (list cand "" "")))
    (let* ((dep (plist-get completion-extra-properties :company-deprecated))
           (mf (run-hook-with-args-until-success 'corfu-margin-formatters corfu--metadata)))
      (cl-loop for x in cands for (c . _) = x do
               (when mf
                 (setf (cadr x) (funcall mf c)))
               (when (and dep (funcall dep c))
                 (setcar x (setq c (substring c)))
                 (add-face-text-property 0 (length c) 'corfu-deprecated 'append c)))
      (cons mf cands)))

  corfu-map

  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("TAB" . corfu-complete)
              ([tab] . corfu-complete)
              ("RET" . corfu-complete)
              ("C-w" . topiary/kill)))
(use-package cape
  :config
  (setq cape-dabbrev-check-other-buffers "some")
  (defun my/ignore-keywords-unless-explicit (cand)
    (or (not (keywordp cand))
        (eq (char-after (car completion-in-region--data)) ?:)))

  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       (cape-capf-predicate
                        #'eglot-completion-at-point
                        #'my/ignore-keywords-unless-explicit)
                       #'cape-dabbrev))))
  (defun my/setup-elisp ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       (cape-capf-predicate
                        #'elisp-completion-at-point
                        #'my/ignore-keywords-unless-explicit)
                       #'cape-dabbrev))))
  :hook ((eglot-managed-mode . my/eglot-capf)
         (emacs-lisp-mode    . my/setup-elisp)))
(use-package dabbrev
  :straight nil
  :after cape
  :demand t
  :config
  ;; Add repl buffers to dabbrev suggestions
  (setq dabbrev-friend-buffer-function
        (lambda (other-buffer)
          (or (dabbrev--same-major-mode-p other-buffer)
              (and (member major-mode '(clojure-mode
                                        clojurec-mode
                                        clojurescript-mode
                                        inferior-lisp-mode))
                   (with-current-buffer other-buffer
                     (eq major-mode 'inferior-lisp-mode)))))))

;;; PROGRAMMING
;; LSP - Language Server Protocol
(use-package eglot
  :straight nil
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-sync-connect 0)
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :hoverProvider
          :signatureHelpProvider))

  :hook
  (((clojure-mode js-mode) . eglot-ensure)))
(use-package jarchive
  :after eglot
  :demand t
  :config
  (jarchive-mode))
;; SQL
(use-package sql
  :straight nil)
(use-package sql-indent
  :demand t
  :after sql)
;; Clojure
(use-package clojure-mode
  :config
  (setq clojure-align-forms-automatically t)
  (setq clojure-indent-style 'always-indent)

  ;;; FUNCTIONS

  (defun my/js-to-json ()
    "Convert topiary region js to json."
    (interactive)
    (let* ((bounds (topiary/compute-bounds))
           (node (when (executable-find "node")
                   (format "node -e 'console.log(JSON.stringify(%s, null, 2))'"
                           (thread-last
                             (buffer-substring-no-properties
                              (car bounds)
                              (cdr bounds))
                             (replace-regexp-in-string
                              "'" "\"")
                             (replace-regexp-in-string
                              (pcre-to-elisp ",([\n\r\s]*)}")
                              "\\1}"))))))
      (if node
          (shell-command-on-region
           (car bounds)
           (cdr bounds)
           node
           (current-buffer) t)
        (user-error "Could not find node install"))))

  (defun my/json-to-edn ()
    "Convert topiary region js/json to edn."
    (interactive)
    (let ((jet (when (executable-find "jet")
                 "jet --pretty --keywordize keyword --from json --to edn")))
      (if jet
          (progn
            (my/js-to-json)
            (let* ((bounds (topiary/compute-bounds)))
              (shell-command-on-region
               (car bounds)
               (cdr bounds)
               jet (current-buffer) t)))
        (user-error "Could not find jet installed")))))
(use-package clj
  :straight nil
  :after clojure-mode
  :load-path "~/.emacs.d/elisp"
  :bind (:map clojure-mode-map
              ("C-c C-a" . my/clj-apropos)
              ("C-c C-z" . my/clj-open-repl)
              ("C-c C-d" . my/clj-doc-for-symbol)
              ("C-c C-s" . my/clj-source-for-symbol)
              ("C-c C-f" . my/clj-find-doc)
              ("C-c C-l" . my/clj-load-current-ns)
              ("C-c C-b" . my/clj-eval-buffer)
              ("C-x C-e" . my/clj-eval-last-sexp)
              ("M-g t"   . my/clj-toggle-between-implementation-and-test)
              ("C-c C-t n"   . my/clj-run-ns-tests)
              ("C-c C-t p"   . my/clj-run-project-tests)
              ("C-c C-t C-n" . my/clj-run-ns-tests)
              ("C-c C-t C-p" . my/clj-run-project-tests)))
(use-package html-to-hiccup)
;; JavaScript
(use-package js
  :straight nil
  :config
  (setq js-indent-level 2))
;; Css
(use-package css-mode
  :straight nil
  :config
  (setq css-indent-offset 2))
;; Markdown
(use-package markdown-mode
  ;; requires multimarkdown if you want to use preview.
  :straight nil
  :init
  (defun my/md-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :height 1.2)
    (set-face-attribute 'markdown-code-face nil :height 0.9))
  :hook ((markdown-mode . variable-pitch-mode)
         (markdown-mode . my/md-font-setup)))

;;; MISC
(defun my/current-ip ()
  "Return current IP address."
  (interactive)
  (message (format-network-address (car (network-interface-info "en0")))))
(defun my/unix-timestanp-to-utc-time ()
  "Output selected unix timestamp in UTC format."
  (interactive)
  (let ((bounds (topiary/compute-bounds)))
    (message
     (format-time-string
      "%Y-%m-%d %a %H:%M:%S UTC"
      (seconds-to-time
       (string-to-number (buffer-substring-no-properties
                          (car bounds) (cdr bounds))))))))
(defun my/count-lines ()
  "Count lines in project. Filters by current buffer file extension.
i.e if the current buffer is a .clj file then it will count lines of .clj
files in the project. Respects gitignore."
  (interactive)
  (let ((default-directory (vc-root-dir))
        (extension (file-name-extension (buffer-name (current-buffer)))))
    (cond ((not default-directory)
           (message "Project does not have a git root."))
          ((not extension)
           (message "File does not have extension."))
          (t (async-shell-command
              (format "git ls-files '*.%s' | xargs wc -l | sort -n"
                      extension)
              (generate-new-buffer "*line-count*"))))))
(defun my/osx-open-in-finder ()
  "Open current file in finder."
  (interactive)
  (shell-command "open ."))
(defun my/keyboard-firmware-tool ()
  "Open keyboard firmware configuration tool."
  (interactive)
  (shell-command "open -a chrysalis"))
(defun my/start-postgresql ()
  "Start local postgresql database."
  (interactive)
  (async-shell-command
   "brew services start postgresql" (generate-new-buffer "*postgresql*")))
(defun my/what-face (pos)
  "Get face under at POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;; MEDIA
(use-package nov
  :defer t
  :init
  (defun my/nov-rerender-without-losing-point ()
    (let ((point (point)))
      (nov-render-document)
      (goto-char point)))
  (defun my/nov-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :height 1.2)
    (my/nov-rerender-without-losing-point))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (setq nov-text-width 65)
  :hook (nov-mode . my/nov-font-setup))
(use-package emms ;; M-x emms-play-directory
  ;; NOTE: emms tends to break now and then with byte compilation.
  ;; you can test different version by using the commit hash for
  ;; the commit you care about. Can be found here:
  ;;
  ;; https://github.com/emacsmirror/emms/commits/master/
  ;;
  ;; You'll then need to change the emms value in
  ;;
  ;; .emacs.d/straight/versions/default.el
  ;;
  ;; And delete:
  ;;
  ;; .emacs.d/straight/repos/emms.el
  ;;
  ;; Finally restart emacs to re-download the repo.
  ;;
  ;; Sometimes there can also be issues with the byte compilation caching
  ;; old files.
  :config
  (emms-minimalistic)
  (setq emms-player-list '(emms-player-mpv))
  (emms-mode-line-disable)
  (emms-playing-time-disable-display)
  (setq emms-repeat-playlist t)
  (defvar emms-source-file-default-directory)
  (setq emms-source-file-default-directory "~/Dropbox/music/")

  (defun my/emms-kill-mpv ()
    "Reset mpv when it's misbehaving."
    (interactive)
    (shell-command "killall mpv"))

  (defun my/emms-track-description (track)
    "Return a description of TRACK, that just includes the file name."
    (let ((artist (emms-track-get track 'info-artist))
          (title (emms-track-get track 'info-title)))
      (cond ((and artist title)
             (concat (format "%s" artist) " - " (format "%s" title)))
            (title title)
            ((eq (emms-track-type track) 'file)
             (with-temp-buffer
               (save-excursion
                 (insert (file-name-nondirectory
                          (directory-file-name (emms-track-name track)))))
               (ignore-error 'search-failed
                 (search-forward-regexp (rx "." (+ alnum) eol))
                 (delete-region (match-beginning 0) (match-end 0)))
               (buffer-string)))
            (t (emms-track-simple-description track)))))

  (setq emms-track-description-function 'my/emms-track-description)

  (defun my/radio3 ()
    "Radio 3. These links might break now and then. For latest links see:

https://gist.github.com/bpsib/67089b959e4fa898af69fea59ad74bc3"
    (interactive)
    (emms-play-streamlist
     "http://lstn.lv/bbc.m3u8?station=bbc_radio_three&bitrate=96000"))

  (defun my/radio4 ()
    "Radio 4. These links might break now and then. For latest links see:

https://gist.github.com/bpsib/67089b959e4fa898af69fea59ad74bc3"
    (interactive)
    (emms-play-streamlist
     "http://lstn.lv/bbc.m3u8?station=bbc_radio_fourfm&bitrate=96000")))
(use-package eww
  :straight nil
  :config

  (setq eww-bookmarks-directory "~/.emacs.d/emacs-sync/")
  ;; ignore html specified colours
  (setq shr-use-colors nil)
  ;; disable images
  (setq shr-inhibit-images t)
  ;; disable animations
  (setq shr-image-animate nil)
  ;; don't render screen reader hidden tags
  ;; reduces noise on some sites
  (setq shr-discard-aria-hidden t)
  (defun my/eww-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :height 1.1))
  :hook ((eww-mode . my/eww-font-setup)
         (eww-mode . variable-pitch-mode)
         (eww-after-render . eww-readable)))
(use-package speak-region
  :straight nil
  :load-path "~/.emacs.d/elisp"
  ;; Need to set this, not sure why auto load not working?
  :commands speak-region)

;;; LOAD PROJECT SPECIFIC COMMANDS
(when (file-directory-p "~/.emacs.d/emacs-sync")
  (load "~/.emacs.d/emacs-sync/project-specific-commands.el"))

;;; SET FLYMAKE LOAD PATH FOR ELISP
;; Needs to be done at the end of this file after
;; all elisp dependencies have been loaded.
;;
;; This might cause Flymake false positives when working
;; on separate Emacs lisp projects.
(setq elisp-flymake-byte-compile-load-path
      (append (list "./") load-path))

;;; DISABLE SOME LINT WARNINGS IN THIS FILE
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init.el ends here
