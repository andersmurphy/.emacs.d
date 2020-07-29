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
    (color1)))

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
       (fg-comment (if (my/is-light-color-p fg)
                       (my/darken-color fg 0.4)
                     (my/lighten-color fg 0.4)))
       (fg-error (face-foreground 'error))
       (bg-inactive (if (my/is-light-color-p bg)
                        (my/darken-color bg 0.05)
                      (my/lighten-color bg 0.05))))

  (custom-theme-set-faces
   'my
   `(default ((t (:foreground ,fg :background ,bg))))
   `(region ((t (:background ,bg-region))))
   `(show-paren-match ((t (:foreground ,fg :background ,bg-region :weight bold))))
   `(show-paren-match ((t (:foreground ,fg-error :background ,bg-region :weight bold))))
   `(cursor ((t (:foreground ,fg))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
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
   `(vertical-border ((t (:foreground ,bg-inactive))))
   `(emms-playlist-track-face ((t (:foreground ,fg :background ,bg))))
   `(emms-playlist-selected-face ((t (:foreground ,fg :background ,bg :weight bold))))))


(provide-theme 'my)
