;; PATHS
(require 'cl-lib)
(add-to-list 'Info-default-directory-list "~/.emacs.d/_info/")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/erlang-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/xclip-1.3/"))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;("elpa" . "https://elpa.gnu.org/packages/")
                         ))
(package-initialize)

;; Authinfo
;; (setq auth-sources '("~/.authinfo.gpg"))

;; My custom globals
(defvar global-font-height 150) ;; use 140 on low-res screen
(defvar original-mode-line-format mode-line-format)

;; LOOK-N-FEEL ;;
(ffap-bindings)
(setq-default inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
;(global-font-lock-mode 1)
(set-face-underline-p 'underline nil)
(column-number-mode t)
(size-indication-mode t)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(delete-selection-mode 1)
(set-language-environment "UTF-8")
(setq-default fill-column 79)
(setq-default Buffer-menu-name-width 35)
(setq-default Buffer-menu-mode-width 10)
(setq-default Buffer-menu-size-width 10)
(setq-default display-buffer-alist
              '(("*shell-?*" (display-buffer-reuse-window
                              display-buffer-same-window))))

(cond
 ((eq window-system 'nil)
  (progn
    (require 'xclip)
    (require 'mouse)
    ;;(color-theme-initialize)
    ;;(color-theme-retro-orange) ;; set orange
    (global-font-lock-mode 0)    ;; disable dynamic highlighting
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
    (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
    (defvar global-shell-location "/bin/bash")))
  ((eq (symbol-value 'window-system) 'x)
   (progn
     (defvar global-font-face "Iosevka Term")
     ;(defvar global-font-face "Fira Mono")
     (defvar global-shell-location "/bin/bash")
     (setq-default scroll-bar-mode-explicit t)
     (scroll-bar-mode -1)
     (tool-bar-mode -1)
     ;; these two lines make touchpad scrolling usable
     (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
     (setq mouse-wheel-progressive-speed nil)
     (setq-default mouse-autoselect-window t)
     (set-face-background 'trailing-whitespace "IndianRed1")
     (set-face-attribute 'fixed-pitch nil
                         :font global-font-face
                         :height global-font-height)
     (set-face-attribute 'default nil
                         :font global-font-face
                         :height global-font-height)
     (set-frame-size (selected-frame) 100 25)
     (fringe-mode '(1 . 1))
     (color-theme-initialize)
     (load-theme 'commentary t)
     (setq-default os-open-command "xdg-open")))
  ((eq window-system 'ns)
   (progn
     (defvar global-font-face "Iosevka Term")
     (defvar global-shell-location "/opt/pkg/bin/bash")
     (setq-default scroll-bar-mode-explicit t)
     (scroll-bar-mode -1)
     (tool-bar-mode -1)
     (setq-default mouse-autoselect-window t)
     (set-face-background 'trailing-whitespace "IndianRed1")
     (set-face-attribute 'default nil :font global-font-face :height global-font-height)
     (set-frame-size (selected-frame) 100 25)
     (fringe-mode '(1 . 1))
     (color-theme-initialize)
     (load-theme 'commentary t)
     (setq shell-command-switch "-lc")
     (setq-default os-open-command "open"))))

;; INPUT & CONTROL
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)

;; ;; W3M Browser
;; (setq w3m-coding-system 'utf-8
;;       w3m-file-coding-system 'utf-8
;;       w3m-file-name-coding-system 'utf-8
;;       w3m-input-coding-system 'utf-8
;;       w3m-output-coding-system 'utf-8
;;       w3m-terminal-coding-system 'utf-8
;;       w3m-user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Safari/537.36")
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; (global-set-key "\C-xm" 'browse-url-at-point)

;; ;; https://gist.github.com/venmos-zz/6190816
;; (setq w3m-search-default-engine "DuckDuck Go")
;; (setq w3m-search-engine-alist '())
;; (add-to-list 'w3m-search-engine-alist
;;   	'("DuckDuck Go" "http://www.duckduckgo.com/html?q=%s"))

;; (defadvice w3m-search (after change-default activate)
;;   (let ((engine (nth 1 minibuffer-history)))
;;     (when (assoc engine w3m-search-engine-alist)
;;       (setq w3m-search-default-engine engine))))

;; WEB MODE
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)

;; ELM
(setq-default elm-indent-offset 2)

;; PROJECTILE
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)

;; ERLANG/ELIXIR
(require 'erlang-start)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(add-hook 'elixir-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(setq-default erlang-indent-level 4)
(setq-default erlang-electric-commands '())
(require 'slim-erlang)

;; PONY
(add-hook
  'ponylang-mode-hook
  (lambda ()
    (set-variable 'indent-tabs-mode nil)
    (set-variable 'tab-width 2)))

;; PYTHON
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)

;; Sane regular expressions
(require 're-builder)
(setq reb-re-syntax 'string)

;; FLYCHECK
;(add-hook 'after-init-hook #'global-flycheck-mode)

;; Haskell mode
;;(Add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(defun enable-old-haskell-indent () (haskell-indent-mode))
(add-hook 'haskell-mode-hook 'enable-old-haskell-indent)
(setq-default haskell-indent-offset 2)

;; EasyPG: GPG support (decrypt in buffer; save encrypted)
(require 'epa-file)
(load "armor-mode")
(epa-file-enable)
(setq epg-gpg-program "/usr/bin/gpg2")
(setq epa-file-name-regexp "\\.\\(gpg\\|\\asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'")
(epa-file-name-regexp-update)
(setq epa-file-select-keys nil)

;; ORG MODE
(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq org-startup-truncated nil)
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o l") 'org-store-link)
(global-set-key (kbd "M-o a") 'org-agenda)
(global-set-key (kbd "M-o c") 'org-capture)
(global-set-key (kbd "M-o b") 'org-iswitchb)
(global-set-key (kbd "M-o p") 'org-present)

;; ORG-PRESENT
(autoload 'org-present "org-present" nil t)
(load-file (expand-file-name "~/.emacs.d/lisp/org-present-hooks.el"))

;; ISPELL
(setq ispell-program-name "/usr/bin/hunspell")

;; MODE BINDINGS
(mapcar (lambda (pair)
	  (add-to-list 'auto-mode-alist pair))
	'(
	  (".spec$" . erlang-mode)
	  ("rebar.config$" . erlang-mode)
	  ("reltool.config$" . erlang-mode)
	  (".app.src$" . erlang-mode)
	  (".pde$" . java-mode)
	  ("\\.sxml$" . scheme-mode)
	  (".sld$" . scheme-mode)
	  (".lfe$" . scheme-mode)
	  ("\\.org\\.gpg$" . org-mode)
	  ("\\.org\\.gpg\\.asc$" . org-mode)
	  ("Gemfile$" . ruby-mode)
	  ("Rakefile$" . ruby-mode)
	  (".md$" . markdown-mode)
	  ("\\.html?\\'" . web-mode)
	  ("\\.css?\\'" . web-mode)
	  (".pug$" . javascript-mode)
    ))


;; TEXT FORMATTING ;;
(electric-indent-mode 0)
(setq-default electric-indent-mode 0)
(setq-default show-trailing-whitespace t)
(show-paren-mode 1)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 2)
(setq-default css-indent-offset 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)

;; Use acutal tabs in Makefiles; show whitespace
(add-hook 'makefile-mode-hook
	  (lambda()
	    (setq-default indent-tabs-mode t)
	    (setq-default tab-width 8)))

;; No emacs poo files
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(setq-default backup-inhibited t)
(setq-default auto-save-default nil)

;; Reloading minor modes
(defun active-minor-modes ()
  (interactive)
  (cl-remove-if-not
   (lambda(x) (and x))
   (mapcar (lambda(m) (and (boundp m) (symbol-value m) m)) minor-mode-list)))


;; M-x shell tweaks
(setq explicit-shell-file-name global-shell-location)
(setq-default comint-scroll-show-maximum-output 1)
(setq-default comint-input-ignoredups t)
(setq-default comint-eol-on-send nil)
(setq-default comint-use-prompt-regexp t)
(setq-default comint-prompt-regexp "")
(add-hook 'comint-mode-hook
	  (lambda ()
	    (define-key shell-mode-map (kbd "RET") 'shell-eol-and-insert)
	    (global-unset-key (kbd "C-x C-x"))))

(setenv "NODE_NO_READLINE" "1")
(setenv "EDITOR" "emx")
(setenv "PAGER" "cat")

(defun shell-eol-and-insert ()
  (interactive)
  (move-end-of-line nil)
  (comint-send-input))

(defun server-start-here ()
  (interactive)
  (ignore-errors (server-force-delete))
  (server-start)
  "Server started here")

(server-start-here)

(defun shell-run (name)
  (interactive)
  (shell name)
  (setq show-trailing-whitespace nil))

(defun shell1 () (interactive) (shell-run "*shell-1*"))
(defun shell2 () (interactive) (shell-run "*shell-2*"))
(defun shell3 () (interactive) (shell-run "*shell-3*"))

(defun clear-buffer-permenantly ()
  "clear whole buffer, contents are not added to the kill ring"
  (interactive)
  (delete-region (point-min) (point-max)))

;; (setq browse-url-browser-function 'browse-url-generic
;;         engine/browser-function 'browse-url-generic
;;         browse-url-generic-program "/home/p/.local/bin/open_in_current_browser")

(defun dump-url (url &rest ignore)
  "Dump URL using w3m."
  (interactive "sURL: ")
  (shell-command (concat "w3m " url))
  (pop-to-buffer "*Shell Command Output*")
  (setq truncate-lines t))

(defun refresh-buffer ()
  "Reload file-local variables"
  (interactive)
  (let ((v major-mode)
        (mm (active-minor-modes)))
    (normal-mode)
    (funcall v)
    (mapcar 'funcall mm)))

(defun global-font-size-bigger ()
  (interactive)
  (setq global-font-height (+ global-font-height 10))
  (set-face-attribute 'default nil :font global-font-face :height global-font-height)
  (set-face-attribute 'fixed-pitch nil :font global-font-face :height global-font-height))

(defun global-font-size-smaller ()
  (interactive)
  (setq global-font-height (- global-font-height 10))
  (set-face-attribute 'default nil :font global-font-face :height global-font-height)
  (set-face-attribute 'fixed-pitch nil :font global-font-face :height global-font-height))

;; keybindings
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "C-x C-p"))
(global-unset-key (kbd "C-x C-r"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-z"))

(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "<f1>") 'top-level)
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<f5>") 'refresh-buffer)
(global-set-key (kbd "C-+") 'global-font-size-bigger)
(global-set-key (kbd "C--") 'global-font-size-smaller)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-k") 'clear-buffer-permenantly)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-o C-o") 'other-window)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x C-m") 'compile)
(global-set-key (kbd "C-x C-r") 'ffap-other-window)
(global-set-key (kbd "C-x E") (kbd "C-u 1 C-x C-e")) ;; eval-into-buffer
(global-set-key (kbd "M-1") 'shell1)
(global-set-key (kbd "M-2") 'shell2)
(global-set-key (kbd "M-3") 'shell3)
(global-set-key (kbd "M-RET") 'shell1)
(global-set-key (kbd "M-`") 'other-window)

;; vulnerability: http://lists.gnu.org/archive/html/emacs-devel/2017-09/msg00211.html
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("08fd3e64e02db1cf1b3dc79296df1e77e104f208ef897dc0c1b4e0112e1b50de" "5d139820639cd941c60033dcdd462bf5fffa76da549e6bdf1d83945803d30f01" "630a574f8383a372b2dd51d92176ac255029067ebefb760f7dba5cdb7f8be30c" "cd95da9e526850b3df2d1b58410d586386bfc0182a2aaca3f33d6cd8548c091a" "3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" "dba244449b15bdc6a3236f45cec7c2cb03de0f5cf5709a01158a278da86cb69b" "9c22be8846bce5d64c803b1f7f4051f0675ba7c0eb492e03a17bb765b0a35d82" "50bfaa1e09c73a6832a4178812ca76ec673ba94f022bdea885dc679d4f472580" "6eaebdc2426b0edfff9fd9a7610f2fe7ddc70e01ceb869aaaf88b5ebe326a0cd" default)))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (ag typescript-mode jedi flycheck-pony pony-mode ponylang-mode pdf-tools eww-lnum w3 restclient sql-indent terraform-mode web-mode-edit-element web-mode tuareg sml-mode elfeed w3m graphviz-dot-mode elm-mode roguel-ike twittering-mode fuel elixir-mode fsharp-mode floobits lua-mode thrift protobuf-mode yaml-mode projectile org-present org-pomodoro ocp-indent markdown-mode ledger-mode haskell-mode grizzl flx-ido evil-vimish-fold ddskk color-theme)))
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
     (web-mode-engines-alist quote
			     (("django" . "\\.html\\'")))))))

(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:weight semi-light :family "Fira Sans")))))
