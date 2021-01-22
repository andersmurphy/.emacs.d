;;; init.el --- Initialization file for Emacs -*- lexical-binding:t -*-

;;; Commentary:

;; Emacs Startup File --- initialisation for Emacs

;; M-. to navigate to function source.
;; C-c C-d to navigate to function docs.
;; C-x C-e to evaluate current expression.
;; M-x elisp-index-search to search elisp manual.
;; M-x emacs-index-search to search Emacs manual.

;;; Code:
(progn ;;; Setup
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
  ;; Install use-package if it's not already installed.
  ;; use-package is used to configure the rest of the packages.
  (straight-use-package 'use-package)
  (defvar straight-use-package-by-default)
  (setq straight-use-package-by-default t)
  (require 'use-package)

  ;; Forces Custom to save all customizations in a seperate file
  (setq custom-file "~/.emacs.d/custom.el")

  ;; Prevents error if the custom.el file doesn't exist
  (load custom-file 'noerror))

;;; Emacs lisp Extension
(defmacro comment (&rest _)
  "Ignore BODY, yields nil."
  nil)
(use-package dash)
(use-package edn)
(use-package elisp-mode
  :straight nil
  :config
  (defun my/docs-for-elisp-symbol-at-point ()
    "Show docs for elisp symbol at point."
    (interactive)
    (describe-function (symbol-at-point)))

  :bind (:map emacs-lisp-mode-map
              ("C-c C-d" . my/docs-for-elisp-symbol-at-point)
              :map lisp-interaction-mode-map
              ("C-c C-d" . my/docs-for-elisp-symbol-at-point)))

;;; Controls
(progn ;; Defaults

  ;; Disable all arrow keys
  (global-unset-key (kbd "<left>"))
  (global-unset-key (kbd "<right>"))
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<down>"))
  (global-unset-key (kbd "<C-left>"))
  (global-unset-key (kbd "<C-right>"))
  (global-unset-key (kbd "<C-up>"))
  (global-unset-key (kbd "<C-down>"))
  (global-unset-key (kbd "<M-left>"))
  (global-unset-key (kbd "<M-right>"))
  (global-unset-key (kbd "<M-up>"))
  (global-unset-key (kbd "<M-down>"))

  ;; Bind cmd (super) key to control
  (setq mac-command-modifier 'control)

  ;; Swap ; and :
  (define-key key-translation-map (kbd ";") (kbd ":"))
  (define-key key-translation-map (kbd ":") (kbd ";"))

  ;; Swap C-m and C-j
  (define-key key-translation-map (kbd "C-j") (kbd "C-m"))
  (define-key key-translation-map (kbd "C-m") (kbd "C-j"))

  ;; Make return/enter key behave like C-m
  (define-key key-translation-map (kbd "RET") (kbd "C-m"))

  ;; Swap C-p and C-h
  (define-key key-translation-map (kbd "C-h") (kbd "C-p"))
  (define-key key-translation-map (kbd "C-p") (kbd "C-h"))

  ;; Swap M-p and M-h
  (define-key key-translation-map (kbd "M-h") (kbd "M-p"))
  (define-key key-translation-map (kbd "M-p") (kbd "M-h"))

  ;; Global key bindings
  (global-set-key (kbd "C-x f") 'find-file)
  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
  (global-set-key (kbd "C-j") 'newline)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-?") 'help-command)
  (global-set-key (kbd "C-x C-d") 'dired)
  (global-set-key (kbd "M-c") 'org-capture)
  (global-set-key (kbd "C-v") 'yank)
  (global-set-key (kbd "C-x o") 'my/other-window)
  (global-set-key (kbd "C-o") 'my/other-window)
  (global-set-key (kbd "C-x k") 'kill-this-buffer)
  (global-set-key (kbd "C-x 2")
                  (lambda () (interactive)
                    (split-window-below)
                    (other-window 1)))
  (global-set-key (kbd "C-x 3")
                  (lambda () (interactive)
                    (split-window-right)
                    (other-window 1)))
  (global-set-key (kbd "C-x -") 'my/zoom-out)
  (global-set-key (kbd "C-x +") 'my/zoom-in)

  ;; Minibuffer binding
  (define-key minibuffer-local-map (kbd "C-v") 'yank)
  (define-key minibuffer-local-map (kbd "C-w") 'topiary/smart-kill)
  (define-key minibuffer-local-map (kbd "C-o") 'my/other-window))
(defun my/keyboard-firmware-tool ()
  "Open keyboard firmware configuration tool in browser."
  (interactive)
  (browse-url "https://configure.ergodox-ez.com/planck-ez/layouts/ABGGZ/latest/0"))
(use-package disable-mouse
  :config
  (global-disable-mouse-mode))

;;; General
(progn ;; Defaults

  ;; Start emacs server
  ;; Open file in existing emacs window (rather than a new session)
  ;; when clicking on a file in finder.
  (server-start)

  ;; Turn off alarms completely.
  (setq ring-bell-function 'ignore)

  ;; Answering 'y' or 'n'.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Disable dialog boxes.
  (setq use-dialog-box nil)

  ;; Disable all backups.
  (setq-default  auto-save-default nil
                 create-lockfiles nil
                 history-length 500
                 make-backup-files nil)

  ;; Don't ask for confirmation when opening symlinked file.
  (setq vc-follow-symlinks t)

  ;; Warn when opening large files (bigger than 100MB).
  (setq large-file-warning-threshold 100000000)

  ;; Use Utf-8 encoding.
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system        'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq locale-coding-system   'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)

  ;; Display help in same window
  (add-to-list 'display-buffer-alist
               '("*Help*" display-buffer-same-window))

  ;; Ask for confirmation when closing emacs.
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; Show keystrokes ASAP
  (setq echo-keystrokes 0.1)

  ;; Initial scratch message.
  (setq initial-scratch-message "")

  ;; Makes recenter go to top first.
  (setq recenter-positions '(top middle bottom))

  ;; Keep hitting C-Space after initial C-u C-Space to pop marks.
  (setq set-mark-command-repeat-pop 't)

  ;; Auto update buffer when it is changed by an external source
  (global-auto-revert-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Enable emoji
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))
(defun my/init ()
  "Open init file (this file)."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun my/update-emacs-packages ()
  "Update Emacs packages using straight."
  (interactive)
  (straight-pull-all)
  (straight-freeze-versions))
(defun my/reload-init ()
  "Reload init."
  (interactive)
  (save-buffer)
  (load  "~/.emacs.d/init.el"))
(defun my/other-window ()
  "Switch to another window. If no other window exists create one."
  (interactive)
  (when (one-window-p)
    (split-window-right))
  (other-window 1))
(defun my/toggle-window-layout-vertical-horizontal ()
  "Switch from a vertical layout to a horizontal layout and vice versa."
  (interactive)
  (let* ((current-window-x (car (window-edges)))
         (_ (other-window 1))
         (other-buff (buffer-name))
         (other-window-x (car (window-edges))))
    (delete-window)
    (if (= current-window-x other-window-x)
        (split-window-right)
      (split-window-below))
    (other-window 1)
    (switch-to-buffer other-buff)
    (other-window 1)))
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))
(use-package super-save
  :config
  (super-save-mode t)
  (setq save-silently t))
(use-package bookmark
  :straight nil
  :config
  ;; Save Bookmarks on any change
  (setq bookmark-save-flag 1)

  ;; Store bookmarks in emacs-sync
  (setq bookmark-default-file "~/.emacs.d/emacs-sync/bookmarks"))
(use-package dired
  :straight nil
  :config
  ;; Ensures Dired file lists are refreshed when files are created/deleted/renamed.
  ;; Also hides auto revert message.
  (setq dired-auto-revert-buffer t)
  ;; Dired hide details by default
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  ;; Bind return to alternate file, so that dired reuses same buffer.
  (put 'dired-find-alternate-file 'disabled nil)
  ;; WDired (writable dired) can be accessed by making the dired buffer writable
  ;; with the binding C-x C-q. Any change you make to the buffer will remain
  ;; unchanged until you commit them by typing C-c C-c. To cancel the changes
  ;; and revert to the original state you can type C-c k.

  ;; The feature bellow force confirmation in the case of potential overwrites
  ;; :caused by rename.
  (setq wdired-confirm-overwrite t)
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("h" . dired-previous-line)
              ("p" . describe-mode)))
(use-package ansi-color
  :config
  (progn
    (defun my/ansi-colorize-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))
    (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)))
(use-package eshell
  :straight nil
  :init
  ;; Eshell starts out defining its map as nil and then only sets it to a keymap
  ;; locally later so :bind won't work
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "M-h") 'eshell-previous-matching-input-from-input))))
(use-package so-long
  :straight nil
  :config
  (global-so-long-mode t))

;;; Visual
(progn ;; Defaults

  ;; Hide menu bar.
  (menu-bar-mode -1)

  ;; Hide scroll and tool bar when not in terminal mode.
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tool-bar-mode -1))

  ;; Disables splash screen.
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t)

  ;; Title bar matches theme.
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist
               '(ns-appearance . dark))

  ;; Remove title bar icon and file name.
  (defvar ns-use-proxy-icon)
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil)

  ;; Sets the initial frame to fill the screen.
  (add-hook 'after-init-hook 'toggle-frame-fullscreen)

  ;; Sets the initial frame to be flush with the top left corner of the screen.
  (add-to-list 'initial-frame-alist '(left . 0))
  (add-to-list 'initial-frame-alist '(top . 0))

  ;; Cursor only appears in current buffer.
  (setq-default cursor-in-non-selected-windows nil)

  ;; Unbind suspend-frame.
  ;; This would cause the cursor to disappear if you pressed C-x C-z by mistake.
  (global-unset-key (kbd "C-x C-z"))

  ;; Sets font and font size.
  (set-frame-font "Menlo 14"))
(progn ;; Dynamic theme changes

  ;; To find out the name of the face you want to customise:
  ;; M-x cutomize-face and then search through the list of faces.

  ;; Hook for after theme load.
  ;; Update the theme of these components on theme change.
  (defvar after-load-theme-hook nil
    "Hook run after a colour theme is loaded using `load-theme'.")
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))

  (defun my/dim-parens ()
    "Make parenthesis less prominent by matching comment face."
    (font-lock-add-keywords nil
                            '(("(\\|)" . 'font-lock-comment-face))))

  (add-hook 'clojure-mode-hook 'my/dim-parens)
  (add-hook 'emacs-lisp-mode-hook 'my/dim-parens))
(defun my/zoom-in ()
  "Zoom in all buffers."
  (interactive)
  (set-face-attribute
   'default nil
   :height (+ (face-attribute 'default :height) 10)))
(defun my/zoom-out ()
  "Zoom out all buffers."
  (interactive)
  (set-face-attribute
   'default nil
   :height (- (face-attribute 'default :height) 10)))
(load "~/.emacs.d/my-theme.el")
(use-package my-theme
  ;; Theme changes are made to these packages
  ;; so they need to be loaded before the theme.
  :straight nil
  :config
  (defun my/disable-all-themes ()
    "Disable all active themes."
    (dolist (i custom-enabled-themes)
      (disable-theme i)))

  (my/disable-all-themes)
  (load-theme 'my t))
(progn ;; Mode Line
  ;; Functions for determining if mode line is active.
  (defvar my/mode-line-selected-window (frame-selected-window))

  (defun my/mode-line-set-selected-window (&rest _args)
    (when (not (minibuffer-window-active-p (frame-selected-window)))
      (setq my/mode-line-selected-window (frame-selected-window))
      (force-mode-line-update)))

  (defun my/mode-line-selected-active-p ()
    (eq my/mode-line-selected-window (selected-window)))

  (add-hook 'window-configuration-change-hook #'my/mode-line-set-selected-window)
  (advice-add 'handle-switch-frame :after #'my/mode-line-set-selected-window)
  (advice-add 'select-window :after #'my/mode-line-set-selected-window)

  ;; Custom minimalist mode line with right aligned time
  (setq-default mode-line-end-spaces
                (list (propertize " " 'display '(space :align-to (- right 16)))
                      'display-time-string))

  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  mode-line-buffer-identification
                  (:eval (when (my/mode-line-selected-active-p)
                           mode-line-end-spaces))))

  ;; Display time in mode line.
  (defvar display-time-default-load-average)
  (setq display-time-default-load-average nil)
  (defvar display-time-string-forms)
  (setq display-time-string-forms
        '((propertize (format-time-string "%F %H:%M" now) 'face 'bold)))
  (display-time-mode t))

;;; Meta Navigation
(defun my/osx-open-in-finder ()
  "Open current file in finder."
  (interactive)
  (shell-command "open ."))
(defun my/jump-to-file-in-project-at-point ()
  "Try to find file at point in project and go to line."
  (interactive)
  (let* ((path (thing-at-point 'filename))
         (path-without-line-number (replace-regexp-in-string
                                    ":.*" "" path))
         (line-num (nth 1 (split-string path ":"))))
    (defun my/insert-current-thing ()
      (insert path-without-line-number)
      (remove-hook 'minibuffer-setup-hook 'my/insert-current-thing))
    (add-hook 'minibuffer-setup-hook 'my/insert-current-thing)
    (my/other-window)
    (xref-push-marker-stack)
    (project-find-file)
    (when line-num
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-num))))))
(use-package recentf
  :straight nil
  :config
  (recentf-mode t)
  (defun my/open-recent-files ()
    (thread-last
        (append (seq-take recentf-list 10)
                '("~/.emacs.d/init.el" "~/.emacs.d/emacs-sync/org/tasks.org"))
      (delete-dups)
      (seq-remove (apply-partially 'string-suffix-p ".emacs.d/emms/history"))
      (seq-reverse)
      (mapcar #'find-file)))
  :hook
  (after-init . my/open-recent-files))
(use-package isearch
  :straight nil
  :config
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  :bind
  (:map isearch-mode-map
        ("DEL" . isearch-del-char)
        ("TAB" . isearch-exit)
        ("C-w" . isearch-del-char)
        ("C-g" . isearch-cancel)
        ("C-n" . isearch-repeat-forward)
        ("C-p" . isearch-repeat-backward)
        ("C-s" . isearch-forward-symbol-at-point)
        ("C-v" . isearch-yank-kill)
        ("C-r" . isearch-query-replace)))
(use-package selectrum
  :config
  (selectrum-mode t))
(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode t)
  (prescient-persist-mode t))
(use-package project
  :straight nil
  :after eglot
  :bind
  ("C-x p" . project-find-file)
  ("C-h" . project-find-file)
  ("C-M-s" . project-find-regexp))
(use-package magit
  :config
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk 'all)
  (setq magit-diff-refine-ignore-whitespace t)
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

  (defun my/magit-spin-off-pull-request ()
    "Spin off last commit as a pull request."
    (interactive)
    (let ((branch (magit-read-string-ns "Spin off branch"))
          (from (car (last (magit-region-values 'commit)))))
      (magit--branch-spinoff branch from t)
      (run-hooks 'magit-credential-hook)
      (magit-run-git "push" "-u" "origin" branch)
      (magit-branch-checkout "master")
      (forge-create-pullreq (concat "origin/" branch) "origin/master")))

  (defun my/magit-search-git-log-for-change ()
    "Search git log. Default to symbol or sexp at point."
    (interactive)
    (let* ((sym (or (thing-at-point 'symbol) (thing-at-point 'sexp)))
           (regex (read-regexp
                   "Search git change"
                   (and sym (concat "[[{(\s\n]"
                                    (regexp-quote sym)
                                    "[]})\s\n]")))))
      (if-let ((file (magit-file-relative-name)))
          (magit-log-setup-buffer
           (list (or magit-buffer-refname
                     (magit-get-current-branch)
                     "HEAD"))
           (list  "--follow" (concat "-G" regex))
           (and file (list file))
           magit-log-buffer-file-locked)
        (user-error "Buffer isn't visiting a file"))))

  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("h" . magit-section-backward)
         :map magit-log-mode-map
         ("h" . magit-section-backward)
         :map magit-diff-mode-map
         ("h" . magit-section-backward))
  :hook (after-save . magit-after-save-refresh-status))
(use-package forge
  :config
  (defun my/set-github-forge-token ()
    "For generating tokens see: https://github.com/settings/tokens"
    (interactive)
    (find-file "~/.authinfo")
    (insert (concat "machine api.github.com login andersmurphy^forge password "
                    (read-string "Enter token:")
                    "\n"))
    (save-buffer))
  :after magit)
(use-package org
  :straight nil
  :config
  ;; Org babel/source blocks
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t)

  ;; Auto tangle .org files in script folder
  (defun my/tangle-scripts ()
    (when-let ((file-name (buffer-file-name)))
      (when (string-match "^.*?/\.emacs\.d/setup/scripts/.*\.org$" file-name)
        (org-babel-tangle-file file-name))))

  (add-hook 'after-save-hook #'my/tangle-scripts)

  ;; Sort sections by TODO.
  (defun my/org-todo-sort ()
    (interactive)
    (ignore-errors (outline-up-heading 10))
    (org-sort-entries nil ?o)
    (org-cycle)
    (org-cycle))


  ;; Capture templates.
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "~/.emacs.d/emacs-sync/org/tasks.org" "Tasks")
           "* TODO %?")
          ("f" "Food Journal" entry
           (file+datetree "~/.emacs.d/emacs-sync/org/food.org")
           "* %?"))))

;;; Text Formatting
(progn ;; Defaults

  ;; Enable visual line mode
  (global-visual-line-mode)

  ;; Only use spaces
  (setq-default indent-tabs-mode nil)

  ;; Set tab width.
  (setq-default tab-width 2)

  ;; Tab will contextually indent or complete.
  (setq tab-always-indent 'complete)

  ;; Sentence should end with only a full stop
  (setq sentence-end-double-space nil)

  (defun my/dedupe-kill (args)
    "Prevent sequential duplicate items in kill ring."
    (let ((string (car args))
          (replace (cdr args))
          (last (car-safe kill-ring)))
      (when (equal last string)
        (setq replace t))
      (list string replace)))
  (advice-add 'kill-new :filter-args #'my/dedupe-kill))
(use-package whitespace
  :straight nil
  :init
  (add-hook 'before-save-hook #'whitespace-cleanup))
(use-package aggressive-indent
  :hook ((emacs-lisp-mode clojure-mode) . aggressive-indent-mode))
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
            (overlay-put ov 'before-string
                         (propertize marker-string
                                     'display
                                     (list 'left-fringe
                                           'my/flymake-fringe-indicator
                                           (overlay-get most-sever-error 'face))))
          (overlay-put ov 'before-string nil))
        (overlay-put ov 'display "..."))))

  (setq hs-set-up-overlay 'my/display-most-sever-flymake-error)

  (defvar my/previous-flymake-errors nil)

  (defun my/toggle-defun-level-hiding ()
    "Toggle folded code at top level without losing cursor position."
    (interactive)
    (save-excursion
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

  :hook (((emacs-lisp-mode clojure-mode) . (lambda ()
                                             (hs-minor-mode) (hs-hide-all))))
  :bind (:map hs-minor-mode-map
              ("TAB" . my/toggle-defun-level-hiding)
              ("<backtab>" . (lambda ()
                               (interactive)
                               (save-excursion
                                 (hs-hide-all))))))
(load "~/.emacs.d/modes/topiary.el")
(use-package topiary

  :straight nil
  :init
  :hook ((text-mode prog-mode comint-mode outline-mode) . topiary-mode))

;;; Linting
(use-package flyspell
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB"))
  ;; Spellchek docs and comments in prog-mode but not strings
  (setq flyspell-prog-text-faces (delq 'font-lock-string-face
                                       flyspell-prog-text-faces))
  (setq ispell-personal-dictionary "~/.emacs.d/setup/dotfiles/.aspell.en.pws")
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))
(use-package flymake :straight nil
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

;;; Completion & Templates
(use-package company
  :init
  (setq company-idle-delay 0.2)
  (global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("TAB" . company-complete-selection)
              ("C-w" . topiary/smart-kill)))
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))
(use-package autoinsert
  :init
  (setq auto-insert-query nil
        auto-insert-alist nil
        auto-insert-directory (locate-user-emacs-file "templates"))
  :config
  (auto-insert-mode 1)
  (defun my/autoinsert-yas-expand ()
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (define-auto-insert "\\.clj$" ["default_clj.clj" my/autoinsert-yas-expand])
  (define-auto-insert "\\test.clj$" ["default_test_clj.clj" my/autoinsert-yas-expand])
  (define-auto-insert "\\.cljs$" ["default_cljs.cljs" my/autoinsert-yas-expand])
  (define-auto-insert "project.clj$" ["default_project.clj" my/autoinsert-yas-expand])
  (define-auto-insert "deps.edn$" ["default_deps.edn" my/autoinsert-yas-expand])
  (define-auto-insert ".gitignore" ["default.gitignore" my/autoinsert-yas-expand]))

;;; Programming
(progn ;; Defaults
  ;; Show paren mode
  (show-paren-mode 1)
  (defvar show-paren-delay)
  (setq show-paren-delay 0))
;; LSP - Language Server Protocol
(use-package eglot
  :demand t
  :config
  (setq eglot-sync-connect 0)
  (setq eglot-ignored-server-capabilites
        '(:documentHighlightProvider :hoverProvider))
  (add-to-list 'eglot-server-programs
               '(clojure-mode . ("bash" "-c" "clojure-lsp")))
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  :hook
  (((clojure-mode js-mode) . eglot-ensure)))
;; Lisp
(use-package inf-lisp
  :bind (:map inferior-lisp-mode-map
              ("M-h" . comint-previous-input)
              ("M-." . my/jump-to-file-in-project-at-point)
              ("M-," . xref-pop-marker-stack)))
;; SQL
(defun my/start-postgresql ()
  "Start local postgresql database."
  (interactive)
  (async-shell-command "pg_ctl -D /usr/local/var/postgresql@10 start" (generate-new-buffer "*postgresql*")))
(use-package sql
  :config
  (defun my/sql-find-up-or-down (file-name)
    (unless file-name (error "The current buffer is not visiting a file"))
    (if (string-suffix-p ".up" (file-name-sans-extension (file-name-nondirectory file-name)))
        (replace-regexp-in-string "\\.up\\." ".down." file-name)
      (replace-regexp-in-string "\\.down\\." ".up." file-name)))

  (defun my/sql-toggle-up-down ()
    (interactive)
    (-> (buffer-file-name)
        my/sql-find-up-or-down
        find-file))
  :bind (:map sql-mode-map
              ("M-g t" . my/sql-toggle-up-down)
              :map sql-interactive-mode-map
              ("M-h" . comint-previous-input)))
;; Clojure
(load "~/.emacs.d/modes/clj.el")
(use-package clj :straight nil)
(use-package clojure-mode
  :bind (:map clojure-mode-map
              ("C-c C-a" . my/clj-apropos)
              ("C-c C-z" . my/clj-open-repl)
              ("C-c C-d" . my/clj-doc-for-symbol)
              ("C-c C-s" . my/clj-source-for-symbol)
              ("C-c C-j" . my/clj-javadoc-for-symbol)
              ("C-c C-f" . my/clj-find-doc)
              ("C-c C-l" . my/clj-load-current-ns)
              ("C-c C-b" . my/clj-eval-buffer)
              ("M-g t"   . my/clj-toggle-between-implementation-and-test)
              ("C-c C-t n" . my/clj-run-ns-tests)
              ("C-c C-t C-n" . my/clj-run-ns-tests)
              ("C-c C-t p" . my/clj-run-project-tests)
              ("C-c C-t C-p" . my/clj-run-project-tests)
              ("C-x C-e" . my/clj-eval-last-sexp-with-ns)
              ("M-;" . my/clj-comment-form)))
;; HTTP
(use-package restclient
  :defer t
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode))
(defun my/current-ip ()
  "Return current IP address."
  (interactive)
  (message (format-network-address (car (network-interface-info "en0")))))
;; JavaScript
(use-package js
  :straight nil
  :config
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2))
;; iOS Simulator
(defvar my/rn-ios-uninstall-app-last-value nil)
(defun my/rn-ios-uninstall-app (app-bundle-identifier)
  "Uninstall app from simulator with APP-BUNDLE-IDENTIFIER."
  (interactive (list (read-string "App bundle identifier: " nil
                                  (list my/rn-ios-uninstall-app-last-value))))
  (setq my/rn-ios-uninstall-app-last-value app-bundle-identifier)
  (shell-command (concat "xcrun simctl uninstall booted " app-bundle-identifier)))

;;; Media
(use-package nov
  :defer t
  :init
  (defun my/nov-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :height 1.3)
    (nov-render-document))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (setq nov-text-width 80)
  :hook (nov-mode . my/nov-font-setup))
(use-package emms
  :defer t
  :config
  (emms-standard)
  (emms-default-players)
  (emms-mode-line-disable)
  (emms-playing-time-disable-display)
  (setq emms-repeat-playlist t)
  (defvar emms-source-file-default-directory)
  (setq emms-source-file-default-directory "~/Dropbox/music/"))
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (markdown-toggle-fontify-code-blocks-natively)

  (defun my/clj-open-md-block-repl ()
    (interactive)
    (setq inferior-lisp-program
          "clojure -Sdeps {:deps{compliment{:mvn/version\"0.3.8\"}}}")
    (my/clj-open-repl))
  :bind (:map markdown-mode-map
              ("C-c C-a" . my/clj-apropos)
              ("C-c C-z" . my/clj-open-md-block-repl)
              ("C-c C-d" . my/clj-doc-for-symbol)
              ("C-c C-s" . my/clj-source-for-symbol)
              ("C-c C-f" . my/clj-find-doc)
              ("C-x C-e" . my/clj-eval-last-sexp-with-ns)))
(use-package eww
  :straight nil
  :config
  (setq eww-bookmarks-directory "~/.emacs.d/emacs-sync/")
  ;; use default font/text size
  (setq shr-use-fonts nil)
  ;; ignore html specified colours
  (setq shr-use-colors nil)
  ;; disable images
  (setq shr-inhibit-images t)
  ;; disable animations
  (setq shr-image-animate nil)
  ;; don't render screen reader hidden tags
  ;; reduces noise on some sites
  (setq shr-discard-aria-hidden t))

;;; Load Project Specific Commands
(load "~/.emacs.d/emacs-sync/project-specific-commands.el")

;;; Set Flymake load path for elisp
;; Needs to be done at the end of this file after
;; all elisp dependencies have been loaded.
;;
;; This might cause Flymake false positives when working
;; on separate Emacs lisp projects.
(setq elisp-flymake-byte-compile-load-path
      (append (list "./") load-path))

;;; init.el ends here
