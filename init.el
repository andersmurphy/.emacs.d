;;; init.el --- Anders' Emacs configuration

;;; Commentary:

;; This is a minimal init.el set up, almost all configuration is
;; in config.org.

;;; Code:

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB).
(setq gc-cons-threshold 50000000)

;; Set up package
(require 'package)

;; Enable Elpa and Melpa
(add-to-list 'package-archives
     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Update the package metadata if the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Only use org to load the config if it has changed or doesn't exist
(let ((org-file (concat user-emacs-directory "config.org"))
      (el-file  (concat user-emacs-directory "config.el")))
  (when (or (not (file-exists-p el-file))
            (file-newer-than-file-p org-file el-file))
    (org-babel-load-file org-file))
  (load-file el-file))

;; Forces Custom to save all customizations in a seperate file
(setq custom-file "~/.emacs.d/custom.el")
;; Prevents error if the custom.el file doesn't exist
(load custom-file 'noerror)

;;; init.el ends here
