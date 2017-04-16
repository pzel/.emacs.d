;; PATHS
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/erlang-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/xclip-1.3/"))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Authinfo
(setq auth-sources '("~/.authinfo.gpg"))


;; My custom globals
(defvar global-font-height 132)
(defvar global-font-face "Iosevka Term") ;Iosevka Term")

(global-set-key (kbd "<f4>") 'evil-mode)
;; Prevent evil mode from opening undo-tree
(progn
  (evil-mode 1)
  (global-undo-tree-mode 0)
  (evil-mode 0))


;; TRAMP
;;(require 'tramp)
;; (setq tramp-default-method "ssh")

;; LOOK-N-FEEL ;;
(ffap-bindings)
(setq-default inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(global-font-lock-mode 1)
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

(if (eq (symbol-value 'window-system) nil)
      (progn
        (require 'xclip)
        (require 'mouse)
        (xterm-mouse-mode t)
        (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
        (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
        (defun track-mouse (e)))
  (progn
    (setq-default scroll-bar-mode-explicit t)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (setq-default mouse-autoselect-window t) ;focus-follows-mouse
    (set-face-background 'trailing-whitespace "IndianRed1")
    (set-face-attribute 'default nil :font global-font-face :height global-font-height)
    (set-frame-size (selected-frame) 100 25)
    (fringe-mode '(1 . 1))
    (color-theme-initialize)
    (load-library "~/.emacs.d/lisp/minimal-light-theme")
    (setq-default os-open-command "xdg-open")))

;; INPUT & CONTROL
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)

;; Unicode shortcuts with M-p
;;(load "macrons.el")

;; JAPANESE INPUT
;;(set-fontset-font "fontset-default"
;;                  'japanese-jisx0208
;;                  (font-spec :family "Meiryo"));; :size 24))
;;(global-set-key (kbd "C-x C-j") 'skk-mode)

;; W3M Browser
(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;; WEB MODE
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)

;; PROJECTILE
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)

;;ERLANG
;; (add-to-list 'load-path "~/.erlangs/default/lib/wrangler-1.2.0/elisp")
;; (condition-case nil (require 'wrangler) (error nil))

;; (setq load-path (cons "~/.erlangs/default/lib/tools-2.7.2/emacs" load-path))
;; (setq erlang-root-dir "~/.erlangs/default")
;; (setq exec-path (cons "~/.erlangs/default/bin" exec-path))

(require 'erlang-start)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(setq-default erlang-indent-level 4)
(setq-default erlang-electric-commands '())
(require 'slim-erlang)

;; FACTOR
(require 'factor-mode)
(setq fuel-listener-factor-binary "/opt/factor/factor")
(setq fuel-listener-factor-image  "/opt/factor/factor.image")

;; Sane regular expressions
(require 're-builder)
(setq reb-re-syntax 'string)

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
(setq org-startup-truncated nil)
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o l") 'org-store-link)
(global-set-key (kbd "M-o a") 'org-agenda)
(global-set-key (kbd "M-o c") 'org-capture)
(global-set-key (kbd "M-o b") 'org-iswitchb)
(global-set-key (kbd "M-o p") 'org-present)

;; ORG-PRESENT
(autoload 'org-present "org-present" nil t)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-display-inline-images)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-remove-inline-images)))))
;; ISPELL
(setq ispell-program-name "/usr/bin/hunspell")

;; LANGUAGE MODES
(setq utop-command "opam config exec -- utop -emacs")

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

	  (".md$" . text-mode)
	  (".tab$" . text-mode)

    ("\\.html?\\'" . web-mode)
    ("\\.css?\\'" . web-mode)
    ))


;; TEXT FORMATTING ;;
;; (require 'smart-tab)
(electric-indent-mode 0)
(setq-default electric-indent-mode 0)
(setq-default show-trailing-whitespace nil)
(show-paren-mode 1)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 2)
(setq-default css-indent-offset 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)

;; Show trailing whitespace in the following modes:
(mapcar (lambda (mode)
          (add-hook mode (lambda ()
                           (setq show-trailing-whitespace 1))))
        '(erlang-mode-hook haskell-mode-hook makefile-mode-hook))

;; Clean trailing whitespace before saving
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use acutal tabs in Makefiles; show whitespace
(add-hook 'makefile-mode-hook
	  (lambda()
	    (setq-default indent-tabs-mode t)
	    (setq-default tab-width 8)))

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

;; (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
;; (setq save-abbrevs t)
;; (quietly-read-abbrev-file)

;; M-x shell tweaks
(setq-default comint-scroll-show-maximum-output 1)
(setq-default comint-input-ignoredups t)
(add-hook 
 'comint-mode-hook 
 (lambda () (global-unset-key (kbd "C-x C-x"))))
(setenv "NODE_NO_READLINE" "1")
(setenv "EDITOR" "emx")
(setenv "PAGER" "cat")

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

(defun shell1 ()
  (interactive)
  (shell-run "*shell-1*"))

(defun shell2 ()
  (interactive)
  (shell-run "*shell-2*"))

(defun shell3 ()
  (interactive)
  (shell-run "*shell-3*"))

(defun clear-buffer-permenantly ()
  "clear whole buffer, contents are not added to the kill ring"
  (interactive)
  (delete-region (point-min) (point-max)))

(defun open-url-at-point (fun)
  (letrec ((fname (thing-at-point 'filename))
           (clean-fname
            (replace-regexp-in-string "\\.\\.\\." "" fname))
           (prepended-fname
            (replace-regexp-in-string "^/" "file:///" clean-fname)))
    (funcall fun (format "%s" prepended-fname))))

(defun browse-url-at-point ()
  (interactive)
  (open-url-at-point
   (lambda(name)
     (message name)
     (let ((process-connection-type nil))
     (start-process "" nil "exo-open" name)))))

(defun ffap-at-mouse-other-window (e)
  (interactive "e")
  (let ((guess
         ;; Maybe less surprising without the save-excursion?
         (save-excursion
           (mouse-set-point e)
           (ffap-guesser))))
    (cond
     (guess
      (set-buffer (ffap-event-buffer e))
      (ffap-highlight)
      (unwind-protect
          (progn
            (sit-for 0)			; display
            (message "Finding `%s'" guess)
            (find-file-other-window guess)
            guess)			; success: return non-nil
        (ffap-highlight t)))
     ((interactive-p)
      (if ffap-at-mouse-fallback
          (call-interactively ffap-at-mouse-fallback)
        (message "No file or url found at mouse click.")
        nil))				; no fallback, return nil
     ;; failure: return nil
     )))

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
  (set-face-attribute 'default nil :font global-font-face :height global-font-height))

(defun global-font-size-smaller ()
  (interactive)
  (setq global-font-height (- global-font-height 10))
  (set-face-attribute 'default nil :font global-font-face :height global-font-height))


;; keybindings
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-x C-b"))
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
(global-set-key (kbd "C-c U") 'w3m-dump-at-point)
(global-set-key (kbd "C-c m") 'erlang-man-module)
(global-set-key (kbd "C-c u") 'browse-url-at-point)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-o C-o") 'other-window)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x C-m") 'compile)
(global-set-key (kbd "C-x C-r") 'ffap-other-window)
(global-set-key (kbd "C-x |") 'toggle-window-split)
(global-set-key (kbd "M-1") 'shell1)
(global-set-key (kbd "M-2") 'shell2)
(global-set-key (kbd "M-3") 'shell3)
(global-set-key (kbd "M-RET") 'shell1)
(global-set-key (kbd "M-`") 'other-window)
(global-set-key [mouse-3] 'ffap-at-mouse-other-window)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5d139820639cd941c60033dcdd462bf5fffa76da549e6bdf1d83945803d30f01" "630a574f8383a372b2dd51d92176ac255029067ebefb760f7dba5cdb7f8be30c" "cd95da9e526850b3df2d1b58410d586386bfc0182a2aaca3f33d6cd8548c091a" "3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" "dba244449b15bdc6a3236f45cec7c2cb03de0f5cf5709a01158a278da86cb69b" "9c22be8846bce5d64c803b1f7f4051f0675ba7c0eb492e03a17bb765b0a35d82" "50bfaa1e09c73a6832a4178812ca76ec673ba94f022bdea885dc679d4f472580" "6eaebdc2426b0edfff9fd9a7610f2fe7ddc70e01ceb869aaaf88b5ebe326a0cd" default)))
 '(package-selected-packages
   (quote
    (roguel-ike w3m twittering-mode fuel utop elixir-mode fsharp-mode floobits lua-mode thrift protobuf-mode yaml-mode web-mode tuareg projectile org-present org-pomodoro ocp-indent merlin markdown-mode ledger-mode haskell-mode grizzl flx-ido evil-vimish-fold ddskk color-theme)))
 '(safe-local-variable-values
   (quote
    ((web-mode-engines-alist quote
                             (("django" . "\\.html\\'")))))))

(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
