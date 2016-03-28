;; PATHS
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/xclip-1.3/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/erlang-mode/"))
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/color-themes/"))

;; Marmalade packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; LINUX/MAC OS X specific
(cond
 ((string-equal system-type "darwin")
  (progn
    (require 'pbcopy)
    (turn-on-pbcopy)
    (setq-default os-open-command "open")
  ))
 ((and (equal (system-name) "kos")
       (string-equal system-type "gnu/linux"))
  (progn
    ;;(require 'xclip)
    ;;(turn-on-xclip)
    (setq-default os-open-command "xdg-open"))))

;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")

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

(cond
 ((eq (symbol-value 'window-system) 'x)
    (progn
      (setq-default scroll-bar-mode-explicit t)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (setq-default mouse-autoselect-window t) ;focus-follows-mouse
      (set-face-background 'trailing-whitespace "IndianRed1")
;;    (set-face-attribute 'default nil :font "PerfectDos" :height 120)
;;    (set-face-attribute 'default nil :font "DejaVu Sans Mono Book" :height 105)
;;    (set-face-attribute 'default nil :font "Noto Sans Mono CJK JP" :height 102)
    (set-face-attribute 'default nil :font "Iosevka CC" :height 132)
;;    (set-face-attribute 'default nil :font "-*-fixed-medium-r-*-*-14-*-*-*-*-*-iso8859-*")
;;    (set-face-attribute 'default nil :font "8x13bold")
    (set-frame-size (selected-frame) 130 31)
    (fringe-mode '(1 . 1))
    (color-theme-initialize)
    (load-file "~/.emacs.d/lisp/minimal-light-theme.el")
    'xorg-detected
    ))
 ((eq (symbol-value 'window-system) 'ns)
  (progn
    (setq-default scroll-bar-mode-explicit t)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode 1)
    (setq-default mouse-autoselect-window t) ;focus-follows-mouse
    (set-face-attribute 'default nil :font "Menlo" :height 140)
    (set-face-background 'trailing-whitespace "IndianRed1")
    (setq ring-bell-function #'ignore)

    ;; light green-on-dark green
    ;; (add-to-list 'default-frame-alist '(background-color . "#0A4B08"))
    ;; (add-to-list 'default-frame-alist '(foreground-color . "green3"))

    (setq ispell-program-name "/usr/local/bin/ispell")
    (setq ispell-list-command "list")
    (fringe-mode '(1 . 1))
    'mac-os-detected
    ))
 (t ;; We're running in a terminal
  (progn
    (require 'mouse)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
    (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
    (defun track-mouse (e))
    'terminal-detected)))

;; INPUT & CONTROL
(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(windmove-default-keybindings)

;; Unicode shortcuts with M-p
;;(load "macrons.el")


;;ERLANG
(setq load-path (cons  "~/erlang/lib/tools-2.6.14/emacs/" load-path))
(setq erlang-root-dir "~/erlang/")
(setq erlang-man-root-dir "~/erlang/man")
(setq exec-path (append (list "/usr/local/bin" "~/erlang/bin") exec-path))
(add-to-list 'load-path "~/erlang/lib/wrangler-1.1.01/elisp")

(condition-case nil (require 'wrangler) (error nil))

(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(defun inf-ctl-g ()
  (interactive)
  (comint-send-string (current-buffer) (make-string 1 ?\C-g)))
(add-hook 'erlang-shell-mode-hook
          (lambda () (define-key erlang-shell-mode-map (kbd "C-c g") 'inf-ctl-g)))
(require 'erlang-start)
(setq-default erlang-indent-level 2)
(setq-default erlang-indent-level 4)
(setq-default erlang-electric-commands '())

(require 'slim-erlang)

;; Sane regular expressions
(require 're-builder)
(setq reb-re-syntax 'string)

;; Haskell mode
;;(Add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq-default haskell-indent-offset 2)

;; GEISER
(setq-default geiser-active-implementations '(guile))

;; LANGUAGE MODES
(mapcar (lambda (pair)
	  (add-to-list 'auto-mode-alist pair))
	'(("\\.sxml$" . scheme-mode)
	  (".sld$" . scheme-mode)
	  (".lfe$" . scheme-mode)
	  (".spec$" . erlang-mode)
	  ("rebar.config$" . erlang-mode)
	  ("reltool.config$" . erlang-mode)
	  (".app.src$" . erlang-mode)
	  ("Gemfile$" . ruby-mode)
	  ("Rakefile$" . ruby-mode)
	  (".md$" . text-mode)
	  (".tab$" . text-mode)
	  (".pde$" . java-mode)
	  (".f$" . fundamental-mode)
	  ("\\.m$" . octave-mode)))

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

;; BUFFERS ;;
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)
(put 'dired-find-alternate-file 'disabled nil)


(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(setq-default backup-inhibited t)
(setq-default auto-save-default nil)

(load "server")
(unless (server-running-p) (server-start))

;; ;; Abbrevs
;; (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
;; (setq save-abbrevs t)
;; (quietly-read-abbrev-file)

;; M-x shell tweaks
(setq-default comint-scroll-show-maximum-output 1)
(setq-default comint-input-ignoredups t)
(setenv "NODE_NO_READLINE" "1")
(setenv "EDITOR" "ema")
(setenv "PAGER" "cat")

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
     (start-process "" nil "/usr/local/bin/open_in_current_browser" name)))))

(defun w3m-dump-at-point ()
  (interactive)
  (open-url-at-point 'dump-url))

(defun search-all-buffers (expr)
  (interactive "sSearch all buffers for: ")
  (multi-occur-in-matching-buffers ".*" expr))

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

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(setq browse-url-browser-function 'w3m-browse-url)

(defun dump-url (url &rest ignore)
  "Dump URL using w3m."
  (interactive "sURL: ")
  (shell-command (concat "w3m " url))
  (pop-to-buffer "*Shell Command Output*")
  (setq truncate-lines t))

;; keybindings
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x C-p"))
(global-unset-key (kbd "C-x C-r"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-o"))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x C-r") 'ffap-other-window)
(global-set-key [mouse-3] 'ffap-at-mouse-other-window)
(global-set-key (kbd "M-`") 'other-window)
(global-set-key (kbd "C-o C-o") 'other-window)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "M-RET") 'shell1)
(global-set-key (kbd "M-1") 'shell1)
(global-set-key (kbd "M-2") 'shell2)
(global-set-key (kbd "M-3") 'shell3)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c s") 'search-all-buffers)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c C-k") 'clear-buffer-permenantly)
(global-set-key (kbd "C-c m") 'erlang-man-module)
(global-set-key (kbd "C-c u") 'browse-url-at-point)
(global-set-key (kbd "C-c U") 'w3m-dump-at-point)
(global-set-key (kbd "C-x |") 'toggle-window-split)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5d139820639cd941c60033dcdd462bf5fffa76da549e6bdf1d83945803d30f01" "630a574f8383a372b2dd51d92176ac255029067ebefb760f7dba5cdb7f8be30c" "cd95da9e526850b3df2d1b58410d586386bfc0182a2aaca3f33d6cd8548c091a" "3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" "dba244449b15bdc6a3236f45cec7c2cb03de0f5cf5709a01158a278da86cb69b" "9c22be8846bce5d64c803b1f7f4051f0675ba7c0eb492e03a17bb765b0a35d82" "50bfaa1e09c73a6832a4178812ca76ec673ba94f022bdea885dc679d4f472580" "6eaebdc2426b0edfff9fd9a7610f2fe7ddc70e01ceb869aaaf88b5ebe326a0cd" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
