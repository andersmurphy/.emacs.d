;; Removes menu scroll and tool bar
(menu-bar-mode -1)

(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; Set up package and adds ELPA and MELPA
(require 'package)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))

;; Forces Custom to save all customizations in a seperate file
(setq custom-file "~/.emacs.d/custom.el")
;; Prevents error if the custom.el file doesn't exist
(load custom-file 'noerror)
