;;; my-theme.el --- My theme

;;; Commentary:

;;; Code:

;; To find out the name of the face you want to customise:
;; M-x cutomize-face and then search through the list of faces.
(deftheme my
  "Created 2020-07-20.")

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

(let* ((bg "#f2f2f2")
       (fg "#222222")
       (bg-region (if (my/is-light-color-p bg)
                      (my/darken-color bg 0.1)
                    (my/lighten-color bg 0.1)))
       (fg-comment (if (my/is-light-color-p bg)
                       (my/lighten-color fg 0.4)
                     (my/darken-color fg 0.4)))
       (fg-error (face-foreground 'error))
       (fg-warning (face-foreground 'warning))
       (fg-success (face-foreground 'success))
       (bg-inactive (if (my/is-light-color-p bg)
                        (my/darken-color bg 0.05)
                      (my/lighten-color bg 0.05)))
       (fg-highlight (face-foreground 'highlight))
       (bg-highlight (face-background 'highlight))
       (magit-add "#e9ffe9")
       (magit-remove "#ffecec"))

  (custom-theme-set-faces
   'my
   `(default ((t (:foreground ,fg :background ,bg))))
   `(region ((t (:background ,bg-region))))
   `(show-paren-match ((t (:foreground ,fg :background ,bg-region :weight bold))))
   `(show-paren-mismatch ((t (:foreground ,fg-error :background ,bg-region :weight bold))))
   `(cursor ((t (:foreground ,fg))))
   `(isearch ((t (:foreground ,fg-highlight :background ,bg-highlight))))
   `(lazy-highlight ((t (:foreground ,fg-highlight :background ,bg-highlight ))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
   `(eshell-prompt ((t (:foreground ,fg :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-builtin-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,fg))))
   `(font-lock-comment-face ((t (:foreground ,fg-comment))))
   `(font-lock-doc-face ((t (:foreground ,fg-comment))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-type-face ((t (:foreground ,fg :weight bold))))
   `(js2-function-param ((t (:foreground ,fg))))
   `(fringe ((t (:background nil))))
   `(mode-line ((t (:foreground ,fg :background ,bg-region :weight bold :box (:line-width 4 :color ,bg-region) :overline nil :underline nil))))
   `(mode-line-inactive ((t (:foreground ,fg-comment :background ,bg-inactive :weight bold :box (:line-width 4 :color ,bg-inactive) :overline nil :underline nil))))
   `(mode-line-buffer-id ((t nil)))
   `(xref-file-header ((t (:foreground ,fg :weight bold))))
   `(flymake-error ((t (:underline (:style line :color ,fg-error) :foreground ,fg-error :weight bold))) )
   `(flymake-warning ((t (:underline (:style line :color ,fg-warning) :foreground ,fg-warning :weight bold))) )
   `(flymake-note
     ((t (:underline (:style line :color ,fg-success) :foreground ,fg-success :weight bold))) )
   `(flyspell-incorrect ((t (:underline (:style line :color ,fg-error)))) )
   `(flyspell-duplicate ((t (:underline (:style line :color ,fg-warning)))) )
   `(vertical-border ((t (:foreground ,bg-inactive))))
   `(emms-playlist-track-face ((t (:foreground ,fg :background ,bg))))
   `(emms-playlist-selected-face ((t (:foreground ,fg :background ,bg :weight bold))))
   ;; diff
   `(diff-added ((t (:background "#e9ffe9"))))
   `(diff-removed ((t (:background "#ffecec"))))
   `(diff-refine-added ((t (:background "#a4f4a3"))))
   `(diff-refine-removed ((t (:background "#f9cbca"))))
   `(magit-diff-added-highlight ((t (:background "#e9ffe9"))))
   `(magit-diff-added ((t (:background "#e9ffe9"))))
   `(magit-diff-removed-highlight ((t (:background "#ffecec"))))
   `(magit-diff-removed ((t (:background "#ffecec"))))
   ;; org
   `(org-level-1 ((t (:foreground ,fg :background ,bg :height 1.5 :weight bold))))
   `(org-level-2 ((t (:foreground ,fg :background ,bg :height 1.2 :weight normal))))
   `(org-level-3 ((t (:foreground ,fg :background ,bg :height 1.0 :weight bold))))
   `(org-level-4 ((t (:foreground ,fg :background ,bg :height 1.0 :weight normal))))))

(provide-theme 'my)
;;; my-theme.el ends here
