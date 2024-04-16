;;; my-theme.el --- My theme

;;; Commentary:

;; To find out the name of the face you want to customise:
;; M-x cutomize-face and then search through the list of faces.
;; M-x describe-text-properties to see faces of text at point.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'color)

(deftheme my
  "Created 2020-07-20.")

(defun my/hsl->hex (H S L)
  "Convert H S L to hex."
  (apply 'color-rgb-to-hex
         (color-hsl-to-rgb
          (/ (float H) 360)
          (/ (float S) 100)
          (/ (float L) 100))))

(defface font-lock-dim-face
  '()
  "Basic face for highlighting.")

;; 350 180 160 220 are fun.
(defun my/random-hue ()
  "Select a random hue between 0 and 360 with step of 10."
  (let ((step 10))
    (* (random (/ 360 step)) step)))

(defvar my/hue (my/random-hue))
(defun my/gen-dark-theme ()
  "Generate a dark theme."
  (let ((hue my/hue))
    `((bg          . ,(my/hsl->hex hue  11  18))
      (bg-inactive . ,(my/hsl->hex hue  13  20))
      (bg-region   . ,(my/hsl->hex hue  12  28))
      (fg          . ,(my/hsl->hex hue  14  71))
      (fg-comment  . ,(my/hsl->hex hue   9  55))
      (fg-dim      . ,(my/hsl->hex hue  10  40))
      (blue        . ,(my/hsl->hex hue 100  66))
      (cya n        . ,(my/hsl->hex 207  82  65))
      (green       . ,(my/hsl->hex  95  38  62))
      (red         . ,(my/hsl->hex 355  65  65))
      (yellow      . ,(my/hsl->hex  39  67  69))
      (purple      . ,(my/hsl->hex 286  60  67)))))
(defvar my/dark-theme (my/gen-dark-theme))

(defun my/gen-light-theme ()
  "Generate a light theme."
  (let ((hue my/hue))
    `((bg          . ,(my/hsl->hex hue  11  (- 110 18)))
      (bg-inactive . ,(my/hsl->hex hue  13  (- 110 20)))
      (bg-region   . ,(my/hsl->hex hue  12  (- 110 28)))
      (fg          . ,(my/hsl->hex hue  14  (- 100 71)))
      (fg-comment  . ,(my/hsl->hex hue   9  (- 100 55)))
      (fg-dim      . ,(my/hsl->hex hue  10  (- 100 40)))
      (blue        . ,(my/hsl->hex hue 100  (- 100 66)))
      (cyan        . ,(my/hsl->hex 221  87  60))
      (green       . ,(my/hsl->hex 119  34  47))
      (red         . ,(my/hsl->hex   5  74  59))
      (yellow      . ,(my/hsl->hex  41  99  38))
      (purple      . ,(my/hsl->hex 301  63  40)))))
(defvar my/light-theme (my/gen-light-theme))

(defun my/set-theme-faces (theme-colors)
  "Set theme faces to THEME-COLORS."
  (let* ((fg          (assoc-default  'fg          theme-colors))
         (bg          (assoc-default  'bg          theme-colors))
         (red         (assoc-default  'red         theme-colors))
         (green       (assoc-default  'green       theme-colors))
         (yellow      (assoc-default  'yellow      theme-colors))
         (blue        (assoc-default  'blue        theme-colors))
         (cyan        (assoc-default  'cyan        theme-colors))
         (purple      (assoc-default  'purple      theme-colors))
         (bg-region   (assoc-default  'bg-region   theme-colors))
         (bg-inactive (assoc-default  'bg-inactive theme-colors))
         (fg-comment  (assoc-default  'fg-comment  theme-colors))
         (fg-dim      (assoc-default  'fg-dim      theme-colors))
         (bg-button   `(:weight bold
                               :background ,bg-region
                               :foreground ,fg
                               :box (:line-width 2 :style released-button)))
         (fg-error   red)
         (fg-warning yellow)
         (fg-success green)
         (fg-match     blue)
         (h2-height  (lambda (_) (+ (face-attribute 'default :height) 10))))
    (custom-theme-set-faces
     'my
     `(highlight ((t (:background ,bg-region))))
     `(default ((t (:foreground ,fg :background ,bg))))
     `(region ((t (:background ,bg-region))))
     `(show-paren-match ((t (:foreground ,fg-match :weight bold))))
     `(show-paren-mismatch ((t (:foreground ,fg-error :background ,bg-region :weight bold))))
     `(cursor                       ((t (:foreground ,bg :background ,fg-match))))
     `(isearch      ((t (:background ,bg-region))))
     `(isearch-fail ((t (:foreground ,fg :background ,red))))
     `(lazy-highlight ((t (:foreground ,fg-match :background unspecified :weight bold))))
     `(match ((t (:foreground ,fg-match :background unspecified :weight bold))))
     `(completions-common-part
       ((t (:foreground ,fg-match :background unspecified :weight bold))))
     `(minibuffer-prompt            ((t (:foreground ,fg :weight bold))))
     `(font-lock-function-name-face ((t (:foreground ,fg :weight bold))))
     `(font-lock-constant-face      ((t (:foreground unspecified))))
     `(font-lock-builtin-face       ((t (:foreground unspecified))))
     `(font-lock-string-face
       ((t (:foreground unspecified :italic t))))
     `(font-lock-comment-face       ((t (:foreground ,fg-comment))))
     `(font-lock-doc-face           ((t (:foreground ,fg-comment))))
     `(font-lock-dim-face ;; this is a custom face for dimming parens
       ((t (:foreground ,fg-dim))))
     `(font-lock-keyword-face       ((t (:foreground unspecified))))
     `(font-lock-variable-name-face ((t (:foreground ,fg :weight bold))))
     `(font-lock-type-face          ((t (:foreground ,fg :weight bold))))
     `(error ((t (:foreground ,fg-error :weight bold))))
     `(warning ((t (:foreground ,fg-warning :weight bold))))
     `(success ((t (:foreground ,fg-success :weight bold))))
     `(fringe                       ((t (:background unspecified))))
     `(link                         ((t (:foreground ,blue))))
     `(mode-line ((t (:foreground ,fg :background ,bg-region :weight bold :box (:line-width 4 :color ,bg-region) :overline nil :underline nil))))
     `(mode-line-inactive ((t (:foreground ,fg-comment :background ,bg-inactive :weight bold :box (:line-width 4 :color ,bg-inactive) :overline nil :underline nil))))
     `(mode-line-buffer-id ((t nil)))
     `(flymake-error ((t (:underline (:style line :color ,fg-error) :foreground ,fg-error :weight bold))) )
     `(flymake-warning ((t (:underline (:style line :color ,fg-warning) :foreground ,fg-warning :weight bold))) )
     `(flymake-note
       ((t (:underline (:style line :color ,fg-comment) :foreground ,fg-comment :weight bold))))
     `(flyspell-incorrect          ((t (:underline (:style line :color ,fg-error)))))
     `(flyspell-duplicate          ((t (:underline (:style line :color ,fg-warning)))))
     `(vertical-border             ((t (:foreground ,bg-inactive))))
     `(header-line ((t (:background ,bg-region :foreground ,fg :weight bold))))
     `(custom-state ((t (:foreground ,green))))

     ;; buttons and fields
     `(custom-button ((t ,bg-button)))
     `(widget-field ((t (:background ,bg-region  :box (:line-width 2 :style pressed-button)))))
     `(eww-form-submit ((t ,bg-button)))
     `(eww-form-text ((t (:background ,bg-region :box (:line-width 2 :style pressed-button)))))

     ;; key
     `(help-key-binding ((t (:weight bold))))
     `(transient-key ((t (:weight bold))))

     ;; xref
     `(xref-file-header ((t (:foreground ,fg :weight bold :background ,bg-region :extend t))))
     `(xref-match ((t (:foreground ,fg-match :background unspecified :weight bold))))

     ;; comint buffer
     `(comint-highlight-prompt ((t (:foreground ,fg :weight bold :height ,h2-height))))

     ;; eshell
     `(eshell-prompt ((t (:foreground ,fg :weight bold :height ,h2-height))))
     `(eshell-ls-directory ((t (:foreground ,blue :weight bold))))

     ;; ansi
     `(ansi-color-yellow ((t (:foreground ,yellow :background ,yellow))))
     `(ansi-color-green ((t (:foreground ,green :background ,green))))
     `(ansi-color-blue ((t (:foreground ,blue :background ,blue))))
     `(ansi-color-cyan ((t (:foreground ,cyan :background ,cyan))))
     `(ansi-color-red ((t (:foreground ,red :background ,red))))
     `(ansi-color-magenta ((t (:foreground ,purple :background ,purple))))

     ;; emms
     `(emms-playlist-track-face    ((t (:foreground ,fg :background ,bg))))
     `(emms-playlist-selected-face ((t (:foreground ,fg :background ,bg :weight bold))))

     ;; diff/magit
     `(diff-added                           ((t (:foreground ,green))))
     `(diff-removed                         ((t (:foreground ,red))))
     `(diff-refine-added
       ((t (:foreground ,green :weight bold :underline (:style line :color ,green)))))
     `(diff-refine-removed
       ((t (:foreground ,red :weight bold :underline (:style line :color ,red)))))
     `(magit-diff-added-highlight           ((t (:foreground ,green))))
     `(magit-diff-added                     ((t (:foreground ,green))))
     `(magit-diff-removed-highlight         ((t (:foreground ,red))))
     `(magit-diff-removed                   ((t (:foreground ,red))))
     `(magit-diffstat-added                 ((t (:foreground ,green))))
     `(magit-diffstat-removed               ((t (:foreground ,red))))
     `(font-lock-warning-face               ((t (:foreground ,red :weight bold))))
     `(magit-diff-context-highlight         ((t (:background ,bg :foreground ,fg))))
     `(magit-diff-context                   ((t (:background ,bg :foreground ,fg))))
     `(magit-diff-section-heading-highlight ((t (:background ,bg :foreground ,fg))))
     `(magit-diff-section-heading           ((t (:background ,bg :foreground ,fg))))
     `(magit-section-highlight              ((t (:background ,bg :foreground unspecified))))
     `(magit-section-heading
       ((t (:background ,bg :foreground ,fg :weight bold :height ,h2-height))))
     `(magit-diff-hunk-heading-highlight    ((t (:background ,bg-region :foreground ,fg))))
     `(magit-diff-hunk-heading              ((t (:background ,bg-region :foreground ,fg))))
     `(magit-branch-remote ((t (:foreground ,fg-match :weight bold))))
     `(magit-branch-local ((t (:foreground ,fg-match :weight bold))))
     `(magit-hash ((t (:foreground ,fg-comment))))
     `(magit-tag ((t (:foreground ,fg-warning))))
     `(magit-log-author ((t (:foreground ,fg-comment))))
     `(magit-log-date ((t (:foreground ,fg-comment))))
     `(magit-section-heading-selection ((t (:foreground ,fg-match))))
     `(magit-process-ng ((t (:foreground ,red :weight bold))))
     `(magit-process-ok ((t (:foreground ,green :weight bold))))

     ;; transient
     `(transient-key-exit   ((t (:inherit transient-key))))
     `(transient-key-stay   ((t (:inherit transient-key))))
     `(transient-key-return ((t (:inherit transient-key))))
     `(transient-key-noop   ((t (:inherit transient-key
                                          :foreground ,fg-comment))))
     `(transient-delimiter         ((t (:foreground ,fg-comment))))
     `(transient-inapt-suffix      ((t (:foreground ,fg-comment))))
     `(transient-unreachable       ((t (:foreground ,fg-comment))))
     `(transient-inactive-value    ((t (:foreground ,fg-comment))))
     `(transient-inactive-argument ((t (:foreground ,fg-comment))))

     ;; forge
     `(forge-pullreq-merged  ((t (:foreground ,fg-comment))))
     `(forge-pullreq-open  ((t (:foreground ,fg))))
     `(forge-notification-unread ((t (:weight bold))))
     `(forge-dimmed ((t (:foreground ,fg-comment))))
     `(forge-issue-completed ((t (:foreground ,fg-comment))))

     ;; vertico
     `(vertico-current ((t (:background ,bg-region))))

     ;; corfu
     `(corfu-default           ((t (:background ,bg))))
     `(corfu-current ((t (:background ,bg-region))))
     `(corfu-bar ((t (:background ,bg))))
     `(corfu-border ((t (:background ,bg))))
     `(corfu-deprecated ((t (:foreground ,fg-comment :strike-through t))))

     ;; prescient
     `(prescient-primary-highlight ((t (:foreground ,fg-match :background unspecified :weight bold))))

     ;; org
     `(org-level-1 ((t (:foreground ,fg :background ,bg :height 1.5 :weight bold))))
     `(org-level-2 ((t (:foreground ,fg :background ,bg :height 1.2 :weight normal))))
     `(org-level-3 ((t (:foreground ,fg :background ,bg :height 1.0 :weight bold))))
     `(org-level-4 ((t (:foreground ,fg :background ,bg :height 1.0 :weight normal))))
     ;; eww
     `(eww-valid-certificate ((t (:foreground ,fg-success))))
     `(eww-invalid-certificate ((t (:foreground ,fg-error))))
     ;; eglot
     `(eglot-diagnostic-tag-unnecessary-face ((t (:foreground ,fg-comment)))))))

(provide-theme 'my)
;;; my-theme.el ends here
