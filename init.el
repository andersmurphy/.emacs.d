;; Adds MELPA package repository. This must come before package-initialize
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Added by Package.el. This must come before configurations of
;; installed packages.
(package-initialize)

;; Sets theme
(load-theme 'zenburn t)

;; Removes menu scroll and tool bar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Ivy completion configuration
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(ivy-mode 1)

;; Magit configuration
(global-set-key (kbd "C-x g") 'magit-status)

;; Disables start up messages
(setq inhibit-startup-message t
inhibit-startup-echo-area-message t)  

;; Enables auto indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit ivy parinfer zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
