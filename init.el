;; PATHS
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
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
    (require 'xclip)
    (turn-on-xclip)
    (setq-default os-open-command "xdg-open"))))

;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")

;; LOOK-N-FEEL ;;
(ffap-bindings)
(setq-default inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(global-font-lock-mode 0)
(column-number-mode t)
(size-indication-mode t)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(delete-selection-mode 1)
(set-language-environment "UTF-8")
(setq-default fill-column 78)



(cond
 ((eq (symbol-value 'window-system) 'x)
    (progn
      (setq-default scroll-bar-mode-explicit t)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (setq-default mouse-autoselect-window t) ;focus-follows-mouse
      (set-face-background 'trailing-whitespace "IndianRed1")
;;    (set-face-attribute 'default nil :font "LucidaTypewriter" :height 88)
    (set-face-attribute 'default nil :font "DejaVu Sans Mono Book" :height 92)
;;    (set-face-attribute 'default nil :font "-*-fixed-medium-r-*-*-14-*-*-*-*-*-iso8859-*")
;;    (set-face-attribute 'default nil :font "6x13")
      (fringe-mode '(1 . 1))
      'xorg-detected
      ))
 ((eq (symbol-value 'window-system) 'ns)
  (progn
    (setq-default scroll-bar-mode-explicit t)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode 1)
    (setq-default mouse-autoselect-window t) ;focus-follows-mouse
    (set-face-attribute 'default nil :font "Menlo" :height 120)
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

(condition-case nil
    (require 'wrangler)
  (error nil))

(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(defun inf-ctl-g ()
  (interactive)
  (comint-send-string (current-buffer) (make-string 1 ?\C-g)))
(add-hook 'erlang-shell-mode-hook
          (lambda () (define-key erlang-shell-mode-map (kbd "C-c g") 'inf-ctl-g)))
(require 'erlang-start)
(setq-default erlang-indent-level 4)
(setq-default erlang-electric-commands '())

(require 'slim-erlang)

;; Sane regular expressions
(require 're-builder)
(setq reb-re-syntax 'string)

;; Haskell mode
;;(Add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(setq-default haskell-indent-offset 2)

;; Text mode
(add-hook 'text-mode-hook
	  (lambda()
	    (remove-hook 'before-save-hook
			 'delete-trailing-whitespace nil 'locally-removed)))

;; LANGUAGE MODES
(mapcar (lambda (pair)
	  (add-to-list 'auto-mode-alist pair))
	'(("\\.sxml$" . scheme-mode)
	  (".spec$" . erlang-mode)
	  ("rebar.config$" . erlang-mode)
	  ("reltool.config$" . erlang-mode)
	  (".app.src$" . erlang-mode)
	  ("Gemfile$" . ruby-mode)
	  ("Rakefile$" . ruby-mode)
	  (".md$" . text-mode)
	  (".f$" . fundamental-mode)
    (".tab$" . text-mode)
	  (".pde$" . java-mode)
	  ("\\.m$" . octave-mode)))

;; TEXT FORMATTING ;;
;; (require 'smart-tab)
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(defun internally-browse-url-at-point ()
  (interactive)
  (open-url-at-point 'browse-url))

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
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x C-r") 'ffap-other-window)
(global-set-key [mouse-3] 'ffap-at-mouse-other-window)
(global-set-key (kbd "M-`") 'other-window)
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
(global-set-key (kbd "C-c u") 'internally-browse-url-at-point)
(global-set-key (kbd "C-c U") 'w3m-dump-at-point)
(global-set-key (kbd "C-x |") 'toggle-window-split)
