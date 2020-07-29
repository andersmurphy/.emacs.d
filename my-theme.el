(deftheme my
  "Created 2020-07-20.")

(let ((bg "#f2f2f2")
      (fg "#222222"))
  (custom-theme-set-faces
   'my
   `(default                     ((t (:foreground ,fg :background ,bg))))
   `(cursor                      ((t (:foreground ,fg :weight bold))))
   `(minibuffer-prompt           ((t (:foreground ,fg))))
   `(fringe                      ((t (:background nil))))
   `(emms-playlist-track-face    ((t (:foreground ,fg :background ,bg))))
   `(emms-playlist-selected-face ((t (:foreground ,fg :background ,bg :weight bold))))))

(provide-theme 'my)
