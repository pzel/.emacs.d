;; ========== EMACS SYSTEM OPTIONS =============
;;; emacs load path
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp/") load-path))

;; ========== Interactively Do Things =============
(require 'ido)
(require 'highlight-parentheses)
(highlight-parentheses-mode)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; ========== RVM ===========================
(require `rvm)
(rvm-use-default) ;; use rvm’s default ruby for the current Emacs session

;; ========== EXTRA LANGUAGE MODES =========
; haml mode
(require 'haml-mode)

;; ========== TABBING, ETC  ==============
(require 'smart-tab)
; spaces for tabbing
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(setq-default tab-width 2)

;; =========== GUI OPTIONS ===============
(if (fboundp 'tool-bar-mode) (tool-bar-mode nil))
(if (fboundp 'menu-bar-mode) (menu-bar-mode nil))
(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode 'right)
(set-face-attribute 'default nil :font "dejavu sans mono 11")

;; =========== UNIQUE BUFFER NAMES ==========
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ========== FONTS, LOOK-N-FEEL =========
(set-face-attribute 'default nil :height 105)

;; ========== INPUT/CONTROL ====================
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<C-tab>") 'other-window)

;; ========== BACKUP/AUTOSAVE FILES  ==========
(setq make-backup-files t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
  ; Disable autosave #foo.bar# files
  ; (auto-save-mode nil)
(setq auto-save-default nil)

;; ========= SESSION SAVING ==================
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/desktops"))
(setq desktop-dirname "~/.emacs.d/desktops")
(setq desktop-base-file-name "emacs-desktop")


;; ========= CUSTOMIZED THROUGH GUI ==========
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
