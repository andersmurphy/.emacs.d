;;; init.el --- Initialization file for Emacs -*- lexical-binding:t -*-

;;; Commentary:

;; Emacs Startup File --- initialisation for Emacs

;; M-. to navigate to function source.
;; C-c C-d to navigate to function docs.
;; C-x C-e to evaluate current expression.
;; M-x elisp-index-search to search elisp manual.
;; M-x emacs-index-search to search Emacs manual.

;;; Code:

;;; PACKAGE MANAGER
(progn ;;; Setup

  ;; To not increase Emacs startup time, check package modifications when
  ;; packages edited (with Emacs), instead of checking modifications at startup.
  (setq straight-check-for-modifications '(check-on-save find-when-checking))
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

;; Needs to be called as soon as possible for native compilation
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;;; EMACS LISP EXTENSION
(defmacro comment (&rest _)
  "Ignore BODY, yields nil."
  nil)
(use-package dash) ;; functional helpers
(use-package edn)  ;; edn parsing
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
(use-package pcre2el) ;; regex conversion

;;; CONTROLS
(progn ;; Defaults

  ;; Bind cmd (super) key to control
  (setq mac-command-modifier 'control)

  ;; Swap ; and :
  (define-key key-translation-map (kbd ";") (kbd ":"))
  (define-key key-translation-map (kbd ":") (kbd ";"))

  ;; Make return/enter key behave like C-m
  (define-key key-translation-map (kbd "RET") (kbd "C-m"))

  ;; Global key bindings
  (global-set-key (kbd "C-x f") 'find-file)
  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-x C-d") 'dired)
  (global-set-key (kbd "M-c") 'org-capture)
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
  (global-set-key (kbd "C-x C--") 'my/zoom-out)
  (global-set-key (kbd "C-x +") 'my/zoom-in)
  (global-set-key (kbd "C-x C-+") 'my/zoom-in)
  (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
  ;; Unbind tmm-menubar as I never use it.
  (global-unset-key (kbd "M-`"))
  ;; Unbind scroll down as I never use it.
  ;; Scroll up is also unbound (C-v is bound to something else).
  (global-unset-key (kbd "M-v"))

  ;; Minibuffer binding
  (define-key minibuffer-local-map (kbd "C-v") 'topiary/yank)
  (define-key minibuffer-local-map (kbd "C-w") 'topiary/kill)
  (define-key minibuffer-local-map (kbd "C-o") 'my/other-window))
(defun my/keyboard-firmware-tool ()
  "Open keyboard firmware configuration tool."
  (interactive)
  (shell-command "open -a chrysalis"))

;;; GENERAL
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
  (straight-check-all)
  (straight-pull-all)
  (straight-freeze-versions))
(defun my/reload-init ()
  "Reload init."
  (interactive)
  (save-buffer)
  (load  "~/.emacs.d/init.el"))
(setq split-width-threshold 80)
(defun my/other-window ()
  "Switch to another window. If no other window exists create one."
  (interactive)
  (when (one-window-p)
    (split-window-sensibly))
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
(use-package Info-mode
  :straight nil
  :init
  (defun my/info-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :height 1.2))
  :hook ((Info-mode . variable-pitch-mode)
         (Info-mode . my/info-font-setup)))
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
(use-package so-long
  :straight nil
  :config
  (global-so-long-mode t))
(load "~/.emacs.d/modes/kill-buffer-on-q.el")
(use-package kill-buffer-on-q
  ;; Convenience mode for killing buffer on q
  :straight nil)

;;; VISUAL
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

  ;; Sets font size.
  (set-face-attribute 'default nil :height 150))
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
                            '(("(\\|)" . 'font-lock-comment-face)
                              )))

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

  (add-hook 'clojure-mode-hook 'my/dim-parens)
  (add-hook 'emacs-lisp-mode-hook 'my/dim-parens)
  (add-hook 'eshell-mode-hook 'my/color-important-words)
  (add-hook 'inferior-lisp-mode-hook 'my/color-important-words)
  (add-hook 'inferior-lisp-mode-hook 'ansi-color-for-comint-mode-filter)
  (add-hook 'shell-mode-hook 'my/color-important-words)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-filter)
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer))
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
(defun my/what-face (pos)
  "Get face under at POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
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

  (defvar my/active-theme)
  (setq my/active-theme my/dark-theme)

  (defun my/toggle-dark-light-theme ()
    "Toggle theme between dark and light."
    (interactive)
    (setq my/active-theme
          (if (eq my/active-theme my/dark-theme)
              my/light-theme
            my/dark-theme))
    (my/set-theme-faces my/active-theme)
    (enable-theme 'my))

  (my/disable-all-themes)
  (my/set-theme-faces my/active-theme)
  (enable-theme 'my))
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

;;; META NAVIGATION
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

  (defun my/replace-in-buffer ()
    (interactive)
    (save-excursion
      (replace-string
       isearch-string
       (read-string (concat "Replace " isearch-string " with: ")
                    isearch-string)
       nil
       (point-min)
       (point-max))))

  (defun my/isearch-forward-thing-at-point ()
    (interactive)
    (let ((bounds (topiary/bounds)))
      (cond
       (bounds
        (when (< (car bounds) (point))
          (goto-char (car bounds)))
        (isearch-yank-string
         (buffer-substring-no-properties (car bounds) (cdr bounds))))
       (t
        (setq isearch-error "No thing at point")
        (isearch-push-state)
        (isearch-update)))))

  (defun my/replace-thing-at-point-in-buffer ()
    (interactive)
    (let* ((bounds (topiary/bounds))
           (string
            (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (save-excursion
        (replace-string
         string
         (read-string (concat "Replace " string " with: ")
                      string)
         nil
         (point-min)
         (point-max)))))

  :bind
  ("C-s" . isearch-forward)
  ("C-r" . my/replace-thing-at-point-in-buffer)
  (:map isearch-mode-map
        ("DEL" . isearch-del-char)
        ("TAB" . isearch-exit)
        ("C-w" . isearch-del-char)
        ("C-g" . isearch-cancel)
        ("C-n" . isearch-repeat-forward)
        ("C-p" . isearch-repeat-backward)
        ("C-s" . my/isearch-forward-thing-at-point)
        ("C-v" . isearch-yank-kill)
        ("C-r" . my/replace-in-buffer)))
(use-package selectrum
  :config
  (selectrum-mode t))
(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode t)
  (prescient-persist-mode t))
(use-package company-prescient
  :config
  (company-prescient-mode t))
(use-package project
  :straight nil
  :after eglot
  :config
  ;; Monkey patch project--read-regexp to use sexp rather than symbol
  (defun project--read-regexp ()
    (let ((sym (thing-at-point 'sexp t)))
      (read-regexp "Find regexp" (and sym (regexp-quote sym))
                   project-regexp-history-variable)))
  :bind
  ("C-x p" . project-find-file)
  ("C-M-s" . project-find-regexp))
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
  ;; Allows q to be used to quit transient buffers
  (transient-bind-q-to-quit)

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
             (from (car (last (magit-region-values 'commit)))))
        (magit--branch-spinoff branch-name from t)
        (run-hooks 'magit-credential-hook)
        (magit-run-git "push" "-u" "origin" branch-name)
        (magit-branch-checkout "master")
        (forge-create-pullreq (concat "origin/" branch-name) "origin/master"))))

  (defun my/magit-search-git-log-for-change ()
    "Search git log for current symbol or topiary region.

If region spans multiple lines does regex or of each trimmed line.
This effectively returns all changes to that set of lines. Or anything
in the file that matches that one of those lines.

Lines containing common patterns that appear throughout the file can
lead to unrelated results. For example '(interactive)' in this file
would lead to a large number of unrelated results as it's a very
common occurrence.

If this becomes a problem these common lines could be filtered."
    (interactive)
    (let* ((bounds (topiary/bounds))
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
        org-src-tab-acts-natively t
        org-adapt-indentation nil)

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
           "* TODO %?"))))

;;; TEXT FORMATTING
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
  :hook ((text-mode prog-mode comint-mode outline-mode info-mode) . topiary-mode))
(use-package special-mode
  :straight nil
  :bind (:map special-mode-map
              ("C-w" . topiary/kill)))

;;; LINTING
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

;;; COMPLETION
(use-package company
  :init
  (setq company-idle-delay 0.2)
  (setq company-tooltip-offset-display nil)
  (setq company-format-margin-function nil)
  (global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("TAB" . company-complete)
              ([tab] . company-complete)
              ("C-w" . topiary/kill)))

;;; PROGRAMMING
;; LSP - Language Server Protocol
(use-package eglot
  :demand t
  :config
  (setq eglot-sync-connect 0)
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :hoverProvider
          :signatureHelpProvider))
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojurescript-mode) . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  :hook
  (((clojure-mode js-mode) . eglot-ensure)))
;; Lisp
(use-package inf-lisp
  :bind (:map inferior-lisp-mode-map
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
  :bind (:map sql-mode-map ("M-g t" . my/sql-toggle-up-down)))
;; Clojure
(load "~/.emacs.d/modes/clj.el")
(use-package clj :straight nil)
(use-package clojure-mode
  :config

  (defconst clojure-font-lock-keywords
    ;; Define our own font lock keywords.
    ;; Monochrome theme means we only care about declarations.
    ;; By simplifying font locking we get better performance in large files.
    (eval-when-compile
      `(;; def..., fn and ns
        (,(concat
           ;; Declaration
           "(\\(def[^ \r\n\t]*\\>\\|ns\\|fn\\)"
           ;; Any whitespace
           "[ \r\n\t]*"
           ;; Possibly type or metadata
           "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
           "\\(" clojure--sym-regexp "\\)?")
         (2 font-lock-function-name-face nil t))
        ;; Highlights namespace part of function/keyword
        (,(concat "\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
         (1 font-lock-type-face)))))

  :bind (:map clojure-mode-map
              ("C-c C-a" . my/clj-apropos)
              ("C-c C-z" . my/clj-open-repl)
              ("C-c C-d" . my/clj-doc-for-symbol)
              ("C-c C-s" . my/clj-source-for-symbol)
              ("C-c C-f" . my/clj-find-doc)
              ("C-c C-l" . my/clj-load-current-ns)
              ("C-c C-b" . my/clj-eval-buffer)
              ("C-x C-e" . my/clj-eval-last-sexp)
              ("M-;"     . clojure-toggle-ignore)
              ("M-g t"   . my/clj-toggle-between-implementation-and-test)
              ("C-c C-t n"   . my/clj-run-ns-tests)
              ("C-c C-t p"   . my/clj-run-project-tests)
              ("C-c C-t C-n" . my/clj-run-ns-tests)
              ("C-c C-t C-p" . my/clj-run-project-tests)))
;; HTTP
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
;; Css
(use-package css-mode
  :straight nil
  :config
  (setq css-indent-offset 2))
;; Markdown
(use-package markdown-mode
  :straight nil
  :init
  (defun my/md-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :height 1.2)
    (set-face-attribute 'markdown-code-face nil :height 0.9))
  :hook ((markdown-mode . variable-pitch-mode)
         (markdown-mode . my/md-font-setup)))

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
                             :height 1.3)
    (my/nov-rerender-without-losing-point))
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
  (setq emms-source-file-default-directory "~/Dropbox/music/")
  (defun my/emms-kill-mpv ()
    "Reset mpv when it's misbehaving."
    (interactive)
    (shell-command "killall mpv")))
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
    (interactive)
    (face-remap-add-relative 'variable-pitch
                             :height 1.2))
  :hook ((eww-mode . my/eww-font-setup)
         (eww-mode . variable-pitch-mode)
         (eww-after-render . eww-readable)))

;;; LOAD PROJECT SPECIFIC COMMANDS
(when (file-directory-p "~/.emacs.d/emacs-sync")
  (load "~/.emacs.d/emacs-sync/project-specific-commands.el"))

;;; Set Flymake load path for elisp
;; Needs to be done at the end of this file after
;; all elisp dependencies have been loaded.
;;
;; This might cause Flymake false positives when working
;; on separate Emacs lisp projects.
(setq elisp-flymake-byte-compile-load-path
      (append (list "./") load-path))

;;; init.el ends here
