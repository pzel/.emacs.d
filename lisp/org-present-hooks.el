(defun org-present-pzel-start-hook ()
  (interactive)
  (setq cursor-type 'bar)
  (setq global-font-height 232)
  (set-cursor-color "Wheat")
  (fringe-mode '(0 . 0))
  (set-frame-parameter nil 'internal-border-width 40)
  (set-window-margins (get-buffer-window) 3)
  (set-face-attribute 'default nil :height global-font-height)
  (hide-global-modeline)
  (org-present-big)
  (org-display-inline-images))

(defun org-present-pzel-stop-hook ()
  (interactive)
  (setq global-font-height 132)
  (setq cursor-type 'box)
  (set-cursor-color "black")
  (set-frame-parameter nil 'internal-border-width 0)
  (set-window-margins (get-buffer-window) 0)
  (set-face-attribute 'default nil :height global-font-height)
  (show-global-modeline)
  (org-remove-inline-images))

(defun hide-global-modeline ()
  (interactive)
  (setq mode-line-format nil))

(defun show-global-modeline ()
  (interactive)
  (setq mode-line-format original-mode-line-format))

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda () (org-present-pzel-start-hook)))
     (add-hook 'org-present-mode-quit-hook
               (lambda () (org-present-pzel-stop-hook)))))
