;;; my-theme.el --- My theme

;;; Commentary:

;; To find out the name of the face you want to customise:
;; M-x cutomize-face and then search through the list of faces.
;; M-x describe-text-properties to see faces of text at point.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(deftheme my
  "Created 2020-07-20.")

(defun my/rgb->hex (r g b)
  "Convert R G B to hex."
  (concat "#" (substring ;; drop first character
               (format "%x" ;; formats as hex
                       ;; ash is arithmetic shift
                     (+ (ash 1 24) (ash r 16) (ash g 8) b))
             1)))

(defun my/hsl->hex (H S L)
  "Convert H S L to hex."
  (apply 'color-rgb-to-hex
         (color-hsl-to-rgb
          (/ (float H) 360)
          (/ (float S) 100)
          (/ (float L) 100))))

(defun my/hex->rgb (color)
  "Convert hex COLOR to RGB."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (* (/ x div) 255)))

(defun my/blend-color (color1 color2 alpha)
  "Blend COLOR1 and COLOR2 together by a coefficient ALPHA."
  (if (and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
      (apply (lambda (r g b) (format "#%02x%02x%02x" r g b))
             (cl-loop for it    in (my/hex->rgb color1)
                      for other in (my/hex->rgb color2)
                      collect (+ (* alpha it) (* other (- 1 alpha)))))
    color1))

(defun my/darken-color (color alpha)
  "Darken a hex COLOR by a coefficient ALPHA between 0 and 1."
  (if (listp color)
      (cl-loop for c in color collect (my/darken-color c alpha))
    (my/blend-color color "#000000" (- 1 alpha))))

(defun my/lighten-color (color alpha)
  "Lighten a hex COLOR by a coefficient ALPHA between 0 and 1."
  (if (listp color)
      (cl-loop for c in color collect (my/lighten-color c alpha))
    (my/blend-color color "#FFFFFF" (- 1 alpha))))

(defun my/is-light-color-p (color)
  "Return t if COLOR is light.
Uses HSP: http://alienryderflex.com/hsp.html"
  (thread-last
      (cl-mapcar (lambda (a b) (* a a b))
                 (my/hex->rgb color)
                 '(0.299 0.587 0.114))
    (apply  #'+)
    sqrt
    (< 127.5)))

(defvar my/dark-theme)
(defvar my/light-theme)
(let ((bg       "#323437")
      (fg       "#d1d0c5")
      (red      "#ea6962")
      (green    "#89b482")
      (yellow   "#d8a657")
      (blue     "#7daea3")
      (purple   "#d3869b"))
  (setq my/dark-theme
        `((bg      . ,bg)
          (fg      . ,fg)
          (red     . ,red)
          (green   . ,green)
          (yellow  . ,yellow)
          (blue    . ,blue)
          (purple  . ,purple)))
  (setq my/light-theme
        `((fg      . ,bg)
          (bg      . ,fg)
          (red     . ,(my/darken-color red 0.2))
          (green   . ,(my/darken-color green 0.2))
          (yellow  . ,(my/darken-color yellow 0.2))
          (blue    . ,(my/darken-color blue 0.2))
          (purple  . ,(my/darken-color purple 0.2)))))

(defun my/set-theme-faces (theme-colors)
  "Set theme faces to THEME-COLORS."
  (let* ((fg      (assoc-default  'fg     theme-colors))
         (bg      (assoc-default  'bg     theme-colors))
         (red     (assoc-default  'red    theme-colors))
         (red2    (if (my/is-light-color-p bg)
                      (my/lighten-color red 0.3)
                    (my/darken-color red 0.3)))
         (red3    (if (my/is-light-color-p bg)
                      (my/lighten-color red 0.6)
                    (my/darken-color red 0.6)))
         (green   (assoc-default  'green  theme-colors))
         (green2 (if (my/is-light-color-p bg)
                     (my/lighten-color green 0.3)
                   (my/darken-color green 0.3)))
         (green3 (if (my/is-light-color-p bg)
                     (my/lighten-color green 0.6)
                   (my/darken-color green 0.6)))
         (yellow  (assoc-default  'yellow theme-colors))
         (blue    (assoc-default  'blue   theme-colors))
         (purple  (assoc-default  'purple   theme-colors))
         (bg-region (if (my/is-light-color-p bg)
                        (my/darken-color bg 0.1)
                      (my/lighten-color bg 0.1)))
         (bg-button (if (my/is-light-color-p bg)
                        (my/darken-color bg-region 0.1)
                      (my/lighten-color bg-region 0.1)))
         (fg-comment (if (my/is-light-color-p bg)
                         (my/lighten-color fg 0.3)
                       (my/darken-color fg 0.3)))
         (fg-error   red)
         (fg-warning yellow)
         (fg-success green)
         (bg-inactive (if (my/is-light-color-p bg)
                          (my/darken-color bg 0.05)
                        (my/lighten-color bg 0.05)))
         (fg-highlight bg)
         (bg-highlight purple)
         (fg-match     purple)
         (h2-height  (lambda (_) (+ (face-attribute 'default :height) 10))))
    (custom-theme-set-faces
     'my
     `(highlight ((t (:foreground ,fg-highlight :background ,bg-highlight :distant-foreground ,fg-highlight))))
     `(default ((t (:foreground ,fg :background ,bg))))
     `(region ((t (:background ,bg-region :distant-foreground ,fg))))
     `(show-paren-match ((t (:foreground ,fg :background ,bg-region :weight bold))))
     `(show-paren-mismatch ((t (:foreground ,fg-error :background ,bg-region :weight bold))))
     `(cursor                       ((t (:foreground ,bg :background ,bg-highlight))))
     `(isearch      ((t (:foreground ,fg-highlight :background ,bg-highlight))))
     `(isearch-fail ((t (:foreground ,fg :background ,red))))
     `(lazy-highlight ((t (:foreground ,fg-match :background nil :weight bold))))
     `(match ((t (:foreground ,fg-match :background nil :weight bold))))
     `(minibuffer-prompt            ((t (:foreground ,fg :weight bold))))
     `(font-lock-function-name-face ((t (:foreground ,fg :weight bold))))
     `(font-lock-constant-face      ((t (:foreground ,nil))))
     `(font-lock-builtin-face       ((t (:foreground ,nil))))
     `(font-lock-string-face        ((t (:foreground ,nil))))
     `(font-lock-comment-face       ((t (:foreground ,fg-comment))))
     `(font-lock-doc-face           ((t (:foreground ,fg-comment))))
     `(font-lock-keyword-face       ((t (:foreground nil))))
     `(font-lock-variable-name-face ((t (:foreground ,fg :weight bold))))
     `(font-lock-type-face          ((t (:foreground ,fg :weight bold))))
     `(error ((t (:foreground ,fg-error :weight bold))))
     `(warning ((t (:foreground ,fg-warning :weight bold))))
     `(success ((t (:foreground ,fg-success :weight bold))))
     `(fringe                       ((t (:background nil))))
     `(link                         ((t (:foreground ,blue))))
     `(mode-line ((t (:foreground ,fg :background ,bg-region :weight bold :box (:line-width 4 :color ,bg-region) :overline nil :underline nil))))
     `(mode-line-inactive ((t (:foreground ,fg-comment :background ,bg-inactive :weight bold :box (:line-width 4 :color ,bg-inactive) :overline nil :underline nil))))
     `(mode-line-buffer-id ((t nil)))
     `(flymake-error ((t (:underline (:style line :color ,fg-error) :foreground ,fg-error :weight bold))) )
     `(flymake-warning ((t (:underline (:style line :color ,fg-warning) :foreground ,fg-warning :weight bold))) )
     `(flymake-note
       ((t (:underline (:style line :color ,fg-success) :foreground ,fg-success :weight bold))))
     `(flyspell-incorrect          ((t (:underline (:style line :color ,fg-error)))))
     `(flyspell-duplicate          ((t (:underline (:style line :color ,fg-warning)))))
     `(vertical-border             ((t (:foreground ,bg-inactive))))
     `(header-line ((t (:background ,bg-region :foreground ,fg :weight bold))))
     `(custom-state ((t (:foreground ,green))))

     ;; buttons and fields
     `(custom-button ((t (:weight bold
                                  :background ,bg-button
                                  :foreground ,fg
                                  :box (:line-width 2 :style released-button)))))
     `(help-key-binding ((t (:weight bold
                                     :background ,bg-button
                                     :foreground ,fg
                                     :box (:line-width 2 :style released-button)))))
     `(widget-field ((t (:background ,bg-button
                                     :box (:line-width 2 :style pressed-button)))))
     `(eww-form-submit ((t (:weight bold
                                    :background ,bg-button
                                    :foreground ,fg
                                    :box (:line-width 2 :style released-button)))))
     `(eww-form-text ((t (:background ,bg-button
                                      :box (:line-width 2 :style pressed-button)))))

     ;; xref
     `(xref-file-header ((t (:foreground ,fg :weight bold :background ,bg-region :extend t))))
     `(xref-match ((t (:foreground ,fg-match :background nil :weight bold))))

     ;; comint buffer
     `(comint-highlight-prompt ((t (:foreground ,fg :weight bold :height ,h2-height))))

     ;; eshell
     `(eshell-prompt ((t (:foreground ,fg :weight bold :height ,h2-height))))
     `(eshell-ls-directory ((t (:foreground ,blue :weight bold))))

     ;; emms
     `(emms-playlist-track-face    ((t (:foreground ,fg :background ,bg))))
     `(emms-playlist-selected-face ((t (:foreground ,fg :background ,bg :weight bold))))

     ;; diff/magit
     `(diff-added                           ((t (:background ,green3))))
     `(diff-removed                         ((t (:background ,red3))))
     `(diff-refine-added                    ((t (:background ,green2))))
     `(diff-refine-removed                  ((t (:background ,red2))))
     `(magit-diff-added-highlight           ((t (:background ,green3))))
     `(magit-diff-added                     ((t (:background ,green3))))
     `(magit-diff-removed-highlight         ((t (:background ,red3))))
     `(magit-diff-removed                   ((t (:background ,red3))))
     `(magit-diffstat-added                 ((t (:foreground ,green))))
     `(magit-diffstat-removed               ((t (:foreground ,red))))
     `(font-lock-warning-face               ((t (:foreground ,red :weight bold))))
     `(magit-diff-context-highlight         ((t (:background ,bg :foreground ,fg))))
     `(magit-diff-context                   ((t (:background ,bg :foreground ,fg))))
     `(magit-diff-section-heading-highlight ((t (:background ,bg :foreground ,fg))))
     `(magit-diff-section-heading           ((t (:background ,bg :foreground ,fg))))
     `(magit-section-highlight              ((t (:background ,bg :foreground nil))))
     `(magit-section-heading                ((t (:background ,bg :foreground ,fg :weight bold :height ,h2-height))))
     `(magit-diff-hunk-heading-highlight    ((t (:background ,bg-region :foreground ,fg))))
     `(magit-diff-hunk-heading              ((t (:background ,bg-region :foreground ,fg))))
     `(magit-branch-local  ((t (:foreground ,blue))))
     `(magit-branch-remote ((t (:foreground ,green))))
     `(magit-hash ((t (:foreground ,fg-comment))))
     `(magit-log-author ((t (:foreground ,fg-comment))))
     `(magit-log-date ((t (:foreground ,fg-comment))))

     ;; corfu
     `(corfu-default           ((t (:background ,bg-region))))
     `(corfu-current ((t (:foreground ,fg-highlight :background ,bg-highlight :distant-foreground ,fg-highlight))))
     `(corfu-bar ((t (:background ,bg-region))))
     `(corfu-deprecated ((t (:foreground ,fg-comment :strike-through t))))

     ;; prescient
     `(prescient-primary-highlight ((t (:foreground ,fg-match :background nil :weight bold))))

     ;; org
     `(org-level-1 ((t (:foreground ,fg :background ,bg :height 1.5 :weight bold))))
     `(org-level-2 ((t (:foreground ,fg :background ,bg :height 1.2 :weight normal))))
     `(org-level-3 ((t (:foreground ,fg :background ,bg :height 1.0 :weight bold))))
     `(org-level-4 ((t (:foreground ,fg :background ,bg :height 1.0 :weight normal))))
     ;; eww
     `(eww-valid-certificate ((t (:foreground ,fg-success))))
     `(eww-invalid-certificate ((t (:foreground ,fg-error))))
     ;; eglot
     `(eglot-diagnostic-tag-unnecessary-face ((t (:foreground ,fg-comment))))
     )))

(provide-theme 'my)
;;; my-theme.el ends here
