;; PATHS
;; Local lisp stuff
(require 'subr-x)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
;(require 'k-mode)

;; Packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package magit :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package dumb-jump :ensure t
  :init
  (dumb-jump-mode t))

(use-package commentary-theme :ensure t)

(use-package yaml-mode :ensure t
  :init
  (add-hook 'yaml-mode-hook
            (lambda () (whitespace-mode 0))))

(use-package visual-fill-column :ensure t)

(use-package web-mode :ensure t
  :init
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2))

(use-package projectile :ensure t
  :init
  (setq projectile-tags-file-name "tags")
  :config
  (projectile-global-mode))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package graphviz-dot-mode :ensure t
  :init
  (add-hook 'graphviz-dot-mode-hook
            (lambda ()
              (local-unset-key (kbd "RET")
              (local-unset-key (kbd ";"))))))

(use-package rainbow-delimiters :ensure t)

(use-package column-enforce-mode :ensure t
  :init
  (setq column-enforce-column 80))

(use-package which-key :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package erlang :ensure t
  :init
  (setq erlang-indent-level 2)
  (setq erlang-electric-commands '())
  (add-hook 'erlang-mode-hook '(lambda()
                                 (setq indent-tabs-mode nil)
                                 (local-set-key (kbd "C-c n") 'display-line-numbers-mode)
                                 (column-enforce-mode 1)
                                 (display-line-numbers-mode 1))))

(use-package elm-mode :ensure t
  :init
  (add-hook 'elm-mode-hook '(lambda() (display-line-numbers-mode 1))))

(use-package elixir-mode :ensure t
  :init
  (add-hook 'before-save-hook
            '(lambda() (if (eq major-mode 'elixir-mode) (whitespace-cleanup))))
  (add-hook 'elixir-mode-hook
            '(lambda()
               (local-set-key (kbd "C-c n") 'display-line-numbers-mode)
               (column-enforce-mode 1)
               (display-line-numbers-mode nil))))

;; My custom globals
(defvar pzel-font-height 120) ;; use 140 on low-res screen
(defvar original-mode-line-format mode-line-format)

;; erc: hide noise in channel
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; LOOK-N-FEEL
(ffap-bindings)
(setq-default inhibit-startup-message t)
(setq initial-scratch-message "")
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(setq large-file-warning-threshold (* 24 10000000))
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
(setq-default uniquify-buffer-name-style 'forward)

(cond
 ((eq window-system 'nil)  ;; TERMINAL
  (progn
    (require 'mouse)
    (global-font-lock-mode 0)
    (xterm-mouse-mode t)
    (xclip-mode t)
    (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
    (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
    (defvar global-shell-location "/bin/bash")))
  ((eq (symbol-value 'window-system) 'x)    ;; XORG
   (progn
     (defvar pzel-font-face "Fira Mono")
     (defvar pzel-variable-font-face "Fira Sans Light")
     (defvar global-shell-location "/bin/bash")
     (setq-default scroll-bar-mode-explicit t)
     (scroll-bar-mode -1)
     (tool-bar-mode -1)
     (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
     (setq mouse-wheel-progressive-speed nil)
     (setq-default mouse-autoselect-window t)
     (set-face-background 'trailing-whitespace "IndianRed1")
     (set-face-background 'default "floral white")
     (set-face-attribute 'fixed-pitch nil
                         :font pzel-font-face
                         :height pzel-font-height)
     (set-face-attribute 'default nil
                         :font pzel-font-face
                         :height pzel-font-height)
     (set-face-attribute 'variable-pitch nil
                         :font pzel-variable-font-face
                         :height pzel-font-height)
     (set-frame-size (selected-frame) 100 25)
     (fringe-mode '(1 . 1))
     (load-theme 'commentary t)
     (setq-default os-open-command "xdg-open"))))

;; INDENTS
(setq-default elm-indent-offset 2)

;; Elfeed
(defun pzel-refresh-elfeed-feeds ()
  (interactive)
  (setq elfeed-feeds
        (with-temp-buffer
          (insert-file-contents "~/.newsboat/urls")
          (split-string (buffer-string) "\n" t))))
(pzel-refresh-elfeed-feeds)

;; EWW as default browser
(setq shr-inhibit-images t)
(setq browse-url-browser-function 'eww-browse-url)
(add-hook 'eww-mode-hook
          #'disable-trailing-whitespace)

;; Electric buffer list
(add-hook 'electric-buffer-menu-mode-hook
          #'disable-trailing-whitespace)

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))


(add-hook 'java-mode-hook
          '(lambda()
             (local-set-key (kbd "C-c n") 'display-line-numbers-mode)
             (display-line-numbers-mode 1)))

;; Disable C-c C-c in python mode
(add-hook 'python-mode-hook
          '(lambda () (local-unset-key (kbd "C-c C-c"))))

;; Visual-wrap lines in text mode (and org mode)
;; with visual-fill-column, this is wrapped to `fill-column`
(add-hook 'text-mode-hook #'visual-line-mode)

;; Disable dangling space hilight in term mode
(defun disable-trailing-whitespace()
    (setq show-trailing-whitespace nil))

(add-hook 'term-mode-hook #'disable-trailing-whitespace)
(add-hook 'ansi-term-mode-hook #'disable-trailing-whitespace)
(add-hook 'elpher-mode-hook
          (lambda() (disable-trailing-whitespace) (local-unset-key (kbd "q"))))

;; Sane regular expressions
(require 're-builder)
(setq reb-re-syntax 'string)

;; EasyPG: GPG support (decrypt in buffer; save encrypted)
(require 'epa-file)
(setq epa-pinentry-mode 'loopback)
(load "armor-mode")
(epa-file-enable)
(setq epg-gpg-program "/usr/bin/gpg2")
(setq epa-file-name-regexp "\\.\\(gpg\\|\\asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'")
(epa-file-name-regexp-update)
(setq epa-file-select-keys nil)

;; ORG MODE
(defun org-home () (interactive) (find-file-at-point "~/notes.org/index.org.gpg"))
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
(global-set-key (kbd "<f8>") 'org-home)

;; ORG-PRESENT
(autoload 'org-present "org-present" nil t)
(load-file (expand-file-name "~/.emacs.d/lisp/org-present-hooks.el"))

;; IDO
(setq ido-enable-flex-matching t)

;; ISPELL
(setq ispell-program-name "/usr/bin/hunspell")
(setq ispell-dictionary "en_US")
(setq ispell-hunspell-dict-paths-alist
      '(("polish" "/usr/share/hunspell/pl_PL.aff")
        ("pl_PL" "/usr/share/hunspell/pl_PL.aff")
        ("en_US" "/usr/share/hunspell/en_US.aff")))

;; MODE BINDINGS
(mapcar (lambda (pair)
    (add-to-list 'auto-mode-alist pair))
  '(("\\.org\\.gpg$" . org-mode)
    ("\\.org\\.gpg\\.asc$" . org-mode)
    ("Gemfile$" . ruby-mode)
    ("Rakefile$" . ruby-mode)
    ("\\.html?\\'" . web-mode)
    ("\\.html.eex\\'" . web-mode)
    ("\\.css?\\'" . web-mode)))

;; TEXT FORMATTING ;;
(setq-default bidi-display-reordering nil)
(electric-indent-mode nil)
(setq-default electric-indent-mode nil)
(setq-default show-trailing-whitespace t)
(show-paren-mode 1)
(setq-default show-paren-delay 0)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 2)
(setq-default css-indent-offset 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)

;; Use acutal tabs in Makefiles
(add-hook 'makefile-mode-hook
    (lambda()
      (setq indent-tabs-mode t)
      (setq tab-width 8)))

;; No emacs poo files
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(setq-default backup-inhibited t)
(setq-default auto-save-default nil)

;; Debug on error
; (setq debug-on-error nil)

;; Reloading minor modes
(defun active-minor-modes ()
  (interactive)
  (cl-remove-if-not
   (lambda(x) (and x))
   (mapcar (lambda(m) (and (boundp m) (symbol-value m) m)) minor-mode-list)))

;; M-x shell tweaks
(setq-default sh-basic-offset 2)
(setq explicit-shell-file-name global-shell-location)
(setq-default comint-scroll-show-maximum-output 1)
(setq-default comint-input-ignoredups t)
(setq-default comint-eol-on-send nil)
(setq-default comint-use-prompt-regexp t)
(setq-default comint-prompt-regexp "")
(add-hook 'comint-mode-hook
    (lambda ()
      (define-key shell-mode-map (kbd "RET")
        'shell-eol-and-insert)
      (global-unset-key (kbd "C-x C-x"))))

;; move cursor to the end when switching to shell
(add-hook 'buffer-list-update-hook
          (lambda()
            (if (string-match "*shell-" (buffer-name (current-buffer)))
                (goto-char (point-max))
              t)))

(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
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
  (font-lock-mode 0)
  (setq show-trailing-whitespace nil))

(defun shell1 () (interactive) (shell-run "*shell-1*"))
(defun shell2 () (interactive) (shell-run "*shell-2*"))
(defun shell3 () (interactive) (shell-run "*shell-3*"))
(defun clear-buffer-permenantly ()
  "clear whole buffer, contents are not added to the kill ring"
  (interactive)
  (delete-region (point-min) (point-max)))

(defun refresh-buffer ()
  "Reload file-local variables"
  (interactive)
  (let ((v major-mode)
        (mm (active-minor-modes)))
    (normal-mode)
    (funcall v)
    (mapcar 'funcall mm)
    (revert-buffer nil t)))

(defun pzel--font-resize ()
  (mapcar
   (lambda (face-font)
     (set-face-attribute (car face-font) nil
                         :font (cdr face-font)
                         :height pzel-font-height))
   `((default . ,pzel-font-face)
     (fixed-pitch . ,pzel-font-face)
     (variable-pitch . ,pzel-variable-font-face))))

(defun pzel-font-size-bigger ()
  (interactive)
  (setq pzel-font-height (+ pzel-font-height 10))
  (pzel--font-resize))

(defun pzel-font-size-smaller ()
  (interactive)
  (setq pzel-font-height (- pzel-font-height 10))
  (pzel--font-resize))

(defun insert-current-datetime ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M")))

;; Keybindings
;; UNMAP UNWANTED KEYBINDINGS
(progn
  (mapcar (lambda(key) (global-unset-key (kbd key)))
          '("<f1>" "<f2>" "<f3>"
            "C-o" "C-r" "C-r" "C-s" "C-t" "C-j"
            "C-x C-b" "C-x C-n" "C-x C-p" "C-x C-r" "C-x C-z"
            "C-x m" "C-z"
            "M-`" "<C-down-mouse-1>"
            ))
  ;; Set custom keybindings
  (mapcar (lambda(key-bind) (global-set-key (kbd (car key-bind))
                                            (cdr key-bind)))
          `(
            ("<f1>" . other-window)
            ("<f2>" . save-buffer)
            ("<f3>" . projectile-find-file)
            ("<f5>" . refresh-buffer)
            ("<f6>" . electric-buffer-list)
            ("<f7>" . ispell-buffer)
            ("C-+" . pzel-font-size-bigger)
            ("C--" . pzel-font-size-smaller)
            ("C-c C-c" . comment-or-uncomment-region)
            ("C-c C-k" . clear-buffer-permenantly)
            ("C-c w" . delete-trailing-whitespace)
            ("C-c v" . visual-line-mode)
            ("C-j" . newline)
            ("C-o C-o" . other-window)
            ("C-r" . isearch-backward-regexp)
            ("C-s" . isearch-forward-regexp)
            ("C-x C-b" . electric-buffer-list)
            ("C-x C-m" . compile)
            ("C-x C-r" . ffap-other-window)
            ("C-x E" . ,(kbd "C-u 1 C-x C-e"))
            ("M-`" . other-window)
            ("M-1" . shell1) ("M-2" . shell2) ("M-3" . shell3)
            ("M-RET" . shell1)
            ("M-g" . goto-line)
            ("M-o d" . insert-current-datetime))))

;; vulnerability: http://lists.gnu.org/archive/html/emacs-devel/2017-09/msg00211.html
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(elfeed elpher magit spark yaml-mode xclip which-key web-mode w3m visual-fill-column use-package tuareg roguel-ike rainbow-delimiters projectile ponylang-mode nginx-mode markdown-mode lua-mode inverse-acme-theme haskell-mode haskell-emacs green-screen-theme green-phosphor-theme green-is-the-new-black-theme grayscale-theme graphviz-dot-mode go-mode flycheck evil erlang elm-mode elixir-mode dumb-jump constant-theme commentary-theme column-enforce-mode abyss-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
