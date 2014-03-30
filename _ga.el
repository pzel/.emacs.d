;; ========== LOADING/PATHS=============
(add-to-list 'load-path (expand-file-name "/etc/emacs/lisp/"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/hasktags-emacs")
(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(load "hasktags")
(require 'haskell-cabal)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; ======== FILE EXTENSIONS =============
(mapcar (lambda (pair)
    (add-to-list 'auto-mode-alist pair))
        '(("Gemfile$"  . ruby-mode)
          ("\\.rake$"  . ruby-mode)
          ("Rakefile$" . ruby-mode)
          ("\\.json$"  . js-mode)
          ("\\.cabal$" . haskell-cabal-mode)))


;; ========= TABS/INDENTATION ===========
(require 'smart-tab)
(setq-default tab-width 2)
(setq-default js-indent-level 2)   ; javascript
(setq-default css-indent-offset 2) ; css

; spaces for tabbing
(setq-default indent-tabs-mode nil)

; always show that horrible trailing whitespace
(setq-default show-trailing-whitespace t)

; Use acutal tabs in Makefiles
(add-hook 'makefile-mode-hook
  (lambda()
    (setq indent-tabs-mode t)
    (setq tab-width 8)))


;; ========== BUFFERS/DIRED =============
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(put 'dired-find-alternate-file 'disabled nil)
(kill-buffer "*scratch*")


;; ========== LOOK AND FEEL =============
(setq inhibit-startup-message t)
(global-font-lock-mode 1)
(setq-default show-trailing-whitespace 't)
(setq scroll-step 1)
(show-paren-mode t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(transient-mark-mode 1)
(delete-selection-mode 1)
(menu-bar-mode nil)
(set-language-environment "UTF-8")


;; ========== INPUT/CONTROL ============
(fset 'yes-or-no-p 'y-or-n-p)
;(global-set-key (kbd "C-c C-r") 'revert-buffer)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(put 'upcase-region 'disabled nil)

;; ========= Kszymek mode ========
(require 'ido)
(setq ido-enable-flex-matching nil)
(setq kszymek-mode nil)
(defun toggle-kszymek-mode ()
  (interactive)
  (setq kszymek-mode (not kszymek-mode))
  (if kszymek-mode
      (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
      (global-set-key (kbd "C-x C-b") 'list-buffers))
  (progn (ido-mode kszymek-mode)
   (message "Kszymex mode %s" kszymek-mode)))
(global-set-key (kbd "C-x t") 'toggle-kszymek-mode)



;; === Clear trailing whitespace
(defun toggle-noisy-whitespace ()
  (interactive)
  (if show-trailing-whitespace
      (progn
        (setq show-trailing-whitespace nil)
        (message "Noisy whitespace disabled"))
    (progn
      (setq show-trailing-whitespace t)
      (message "Noisy whitespace enabled"))))

(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c e") 'toggle-noisy-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; === Easy commenting/uncommenting
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; ========= Xterm mouse tracking (linux only
(require 'mouse)
(setq mouse-mode nil)
(setq mouse-sel-mode nil)
(defun toggle-mouse-mode ()
  (interactive)
  (setq mouse-mode (not mouse-mode))
  (if mouse-mode
    (progn (xterm-mouse-mode t)
     (defun track-mouse (e))
     (message "Kszymex mouse mode on"))
    (progn (xterm-mouse-mode 0)
     (message "Kszymex mouse mode off"))))

(global-set-key (kbd "C-x m") 'toggle-mouse-mode)


;; ========= Other functions ===
(defun terminal ()
  (interactive)
  (ansi-term "/bin/bash" "ansi-term")
  (toggle-noisy-whitespace))
(global-set-key (kbd "M-RET") 'terminal)
