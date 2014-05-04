;; PATHS
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/ess"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/rhtml/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/color-themes/"))

;; LOOK-N-FEEL ;;
(ffap-bindings)
(setq-default inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(global-font-lock-mode 0)
(show-paren-mode 1)
(column-number-mode t)
(size-indication-mode t)
(transient-mark-mode 1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(delete-selection-mode 1)
(set-language-environment "UTF-8")

(if (eq (symbol-value 'window-system) 'x)
    (progn
      (setq-default scroll-bar-mode-explicit t)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (setq-default mouse-autoselect-window t) ;focus-follows-mouse
      ;; (set-face-attribute 'default nil :font "droid sans mono" :height 78)))
      (set-face-attribute 'default nil :font "monaco" :height 88)
      ;; (set-face-attribute 'default nil :font "Courier 10 pitch" :height 92)
      ;; (set-face-attribute 'default nil :font "consolas" :height 88)
      ))

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)

;; INPUT & CONTROL
(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)

;; Unicode shortcuts with M-p
(load "macrons.el")

;; (require 'ibus)
;; (add-hook 'after-init-hook 'ibus-mode-on)
;; (setq ibus-agent-file-name "/usr/local/bin/ibus-el-agent")
;; (ibus-define-common-key ?\C-\s nil)
;; (ibus-define-common-key ?\C-/ nil)
;; (global-set-key (kbd "C-c C-i") 'ibus-toggle)

;; Marmalade packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; SPELLING
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

(setq scheme-program-name "csi -:c")

;; RUBY ETC.
(require 'rhtml-mode)
(setq-default ruby-deep-indent-paren nil)
(setq-default ruby-deep-indent-arglist nil)


;; ERLANG
(setq load-path (cons  "/home/p/.erlangs/17.0/lib/tools-2.6.14/emacs/" load-path))
(setq erlang-root-dir "/home/p/.erlangs/17.0/")
(setq erlang-man-root-dir "/home/p/.erlangs/17.0/man")
(setq exec-path (cons "/home/p/.erlangs/17.0/bin" exec-path))

(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(defun inf-ctl-g ()
  (interactive)
  (comint-send-string (current-buffer) (make-string 1 ?\C-g)))
(add-hook 'erlang-shell-mode-hook
  (lambda () (define-key erlang-shell-mode-map (kbd "C-c g") 'inf-ctl-g)))

(require 'erlang-start)
(setq-default erlang-indent-level 4)
(setq-default erlang-electric-commands '())

;; Sane regular expressions
(require 're-builder)
(setq reb-re-syntax 'string)

;; Haskell mode
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; LANGUAGE MODES
(mapcar (lambda (pair)
          (add-to-list 'auto-mode-alist pair))
        '(("\\.sxml$" . scheme-mode)
          ("Gemfile$" . ruby-mode)
          ("Rakefile$" . ruby-mode)
          (".erb$" . rhtml-mode)
          (".scss$" . css-mode)
          (".scss.erb$" . css-mode)
          (".f$" . fundamental-mode)
          (".spec$" . erlang-mode)
          ("^rebar.config$" . erlang-mode)
          (".app.src$" . erlang-mode)
          (".pde$" . java-mode)
          ("\\.m$" . octave-mode)))

;; TEXT FORMATTING ;;
;; (require 'smart-tab)
(setq-default show-trailing-whitespace nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 2)
(setq-default css-indent-offset 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)

;; Clean trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use acutal tabs in Makefiles; show whitespace
(add-hook 'makefile-mode-hook
          (lambda()
            (setq indent-tabs-mode t)
            (setq tab-width 8)))

;; BUFFERS ;;
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)
(put 'dired-find-alternate-file 'disabled nil)


;; BACKUP/AUTOSAVE
(setq-default make-backup-files nil)
(setq-default backup-inhibited t)
(setq-default auto-save-default nil)

;; share clipboard with X
;;(setq-default x-select-enable-primary t)
(require 'xclip)
(turn-on-xclip)

;; start emacs server
(load "server")
(unless (server-running-p) (server-start))


;; keybindings
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x C-p"))
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-c f") 'ffap-other-window)
(global-set-key [mouse-2] 'ffap-at-mouse)
(global-set-key (kbd "M-`") 'other-window)
(global-set-key (kbd "M-RET")
                (lambda() (interactive) (shell-buf "*shell*")))
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

;; ;; Abbrevs
;; (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
;; (setq save-abbrevs t)
;; (quietly-read-abbrev-file)

;; M-x shell tweaks
(setq-default comint-scroll-show-maximum-output 1)
(setq-default comint-input-ignoredups t)
(setenv "EDITOR" "emacsclient")
(setenv "NODE_NO_READLINE" "1")
(setenv "PAGER" "cat")

(defun shell-buf (name)
  (interactive)
  (shell name)
  (setq comint-scroll-show-maximum-output nil))
