;; PATHS
;; Local lisp stuff
(require 'subr-x)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

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

;; Evil mode
(add-to-list 'load-path "~/src/evil")
(require 'evil)
;; Disable undo-tree
;; (require undo-tree-mode)(require 'undo-tree)
(progn (evil-mode 1) (evil-mode -1))
(global-set-key (kbd "<f4>") 'evil-mode)
;; Make _ part of a word, like vim
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?= "w")
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
(define-key evil-motion-state-map (kbd "C-p") 'projectile-find-file)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
;(define-key evil-normal-state-map (kbd "b") 'projectile-switch-to-buffer)

(use-package commentary-theme
  :ensure t)

(use-package yaml-mode
  :ensure t
  :init
  (add-hook 'yaml-mode-hook
            (lambda () (whitespace-mode 0))))

(use-package markdown-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2))

;; My custom globals
(defvar global-font-height 120) ;; use 140 on low-res screen
(defvar original-mode-line-format mode-line-format)

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

(cond
 ((eq window-system 'nil)
  (progn
    (require 'mouse)
    (global-font-lock-mode 0)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
    (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
    (defvar global-shell-location "/bin/bash")))
  ((eq (symbol-value 'window-system) 'x)
   (progn
     (defvar global-font-face "Go Mono")
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
     (load-theme 'commentary t)
     (setq shell-command-switch "-lc")
     (setq-default os-open-command "open"))))


;; INDENTS
(setq-default elm-indent-offset 2)

;; PROJECTILE-CONTROLLED MODES
(use-package projectile
  :ensure t
  :init
  (setq projectile-tags-file-name "tags")
  :config
  (projectile-global-mode))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package graphviz-dot-mode
  :ensure t
  :init
  (add-hook 'graphviz-dot-mode-hook
            (lambda ()
              (local-unset-key (kbd "RET")
              (local-unset-key (kbd ";"))))))


(use-package rainbow-delimiters
  :ensure t)

(use-package column-enforce-mode
  :ensure t
  :init
  (setq column-enforce-column 80))

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package erlang
  :ensure t
  :init
  (setq erlang-indent-level 2)
  (setq erlang-electric-commands '())
  (add-hook 'erlang-mode-hook '(lambda()
                                 (setq indent-tabs-mode nil)
                                 (local-set-key (kbd "C-c n") 'display-line-numbers-mode)
                                 (column-enforce-mode 1)
                                 (display-line-numbers-mode 1))))

(use-package elm-mode
  :ensure t
  :init
  (add-hook 'elm-mode-hook
            '(lambda()
               (display-line-numbers-mode 1))))


(use-package elixir-mode
  :ensure t
  :init
  (add-hook 'before-save-hook
            '(lambda() (whitespace-cleanup)))
  (add-hook 'elixir-mode-hook
            '(lambda()
               (local-set-key (kbd "C-c n") 'display-line-numbers-mode)
               (column-enforce-mode 1)
               (display-line-numbers-mode 1))))

;; Disable C-c C-c in python mode
(add-hook 'python-mode-hook
          '(lambda () (local-unset-key (kbd "C-c C-c"))))

;; Disable dangling space hilight in term mode
(defun disable-trailing-whitespace()
    (setq show-trailing-whitespace nil))

(add-hook 'term-mode-hook
          #'disable-trailing-whitespace)
(add-hook 'ansi-term-mode-hook
          #'disable-trailing-whitespace)

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
;    (".md$" . markdown-mode)
    ("\\.html?\\'" . web-mode)
    ("\\.html.eex\\'" . web-mode)
    ("\\.css?\\'" . web-mode)
    (".pug$" . javascript-mode)
    ))

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
;;      (whitespace-mode 1)
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
            ;; ("<up>" . scroll-down-command)
            ;; ("<down>" . scroll-up-command)
            ("C-+" . global-font-size-bigger)
            ("C--" . global-font-size-smaller)
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
            ("M-o d" . insert-current-datetime)
            )))

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
    ("ae3a3bed17b28585ce84266893fa3a4ef0d7d721451c887df5ef3e24a9efef8c" "08a89acffece58825e75479333109e01438650d27661b29212e6560070b156cf" "ba913d12adb68e9dadf1f43e6afa8e46c4822bb96a289d5bf1204344064f041e" "8150ded55351553f9d143c58338ebbc582611adc8a51946ca467bd6fa35a1075" "39546362fed4d5201b2b386dc21f21439497c9eec5fee323d953b3e230e4083e" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "08fd3e64e02db1cf1b3dc79296df1e77e104f208ef897dc0c1b4e0112e1b50de" "5d139820639cd941c60033dcdd462bf5fffa76da549e6bdf1d83945803d30f01" "630a574f8383a372b2dd51d92176ac255029067ebefb760f7dba5cdb7f8be30c" "cd95da9e526850b3df2d1b58410d586386bfc0182a2aaca3f33d6cd8548c091a" "3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" "dba244449b15bdc6a3236f45cec7c2cb03de0f5cf5709a01158a278da86cb69b" "9c22be8846bce5d64c803b1f7f4051f0675ba7c0eb492e03a17bb765b0a35d82" "50bfaa1e09c73a6832a4178812ca76ec673ba94f022bdea885dc679d4f472580" "6eaebdc2426b0edfff9fd9a7610f2fe7ddc70e01ceb869aaaf88b5ebe326a0cd" default)))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (flycheck-golangci-lint toml-mode evil flycheck go-mode haskell-emacs js-mode green-phosphor-theme green-screen-theme green-is-the-new-black-theme constant-theme grayscale-theme inverse-acme-theme abyss-theme tuareg xclip j-mode forth-mode color-theme-initialize w3m rainbow-delimiters python-mode erlang which-key ripgrep pinentry sqlformat rust-mode nginx-mode typit typing-game use-package commentary-theme package-lint ag flycheck-pony ponylang-mode pdf-tools eww-lnum w3 restclient sql-indent web-mode-edit-element web-mode graphviz-dot-mode elm-mode roguel-ike twittering-mode fuel elixir-mode fsharp-mode floobits lua-mode thrift protobuf-mode yaml-mode projectile org-present org-pomodoro ocp-indent markdown-mode ledger-mode haskell-mode grizzl flx-ido evil-vimish-fold ddskk)))
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
 )
