;;; early-init.el --- Early init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;; Defer garbage collection until post startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Switch to a smaller threshold once startup complete.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16MB
                  gc-cons-percentage 0.1)))

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We handle our own initialization with
;; straight/use-package so we can disable these.
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

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

;; Fringe
(fringe-mode 16)

;; Theme
(load "~/.emacs.d/elisp/my-theme.el")
(enable-theme 'my)

;; Sets font.
(set-face-attribute 'default nil :height 150 :family "Fira Code")

;; Temporarily disable modeline
(setq mode-line-format nil)

;;; DISABLE SOME LINT WARNINGS IN THIS FILE
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; early-init.el ends here
