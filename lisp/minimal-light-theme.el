;;; minimal-light-theme.el --- A minimal theme based on xterm-256 color set

;; This file is not part of GNU Emacs.

;; Copyright (C) 2013 Amirreza Ghaderi
;; Authors: Amirreza Ghaderi <amirreza.blog@gmail.com>
;; Version: 0.9
;; Keywords: syntax, highlight, theme, minimal, xterm
;; URL: 
;; Compatibility: GNU Emacs 24.x

;;; Commentary:

;; Im not a big fan of syntax highlighting! to me, syntax highlighting is not 
;; only reduce code readability, but its also a distraction! of course many
;; programmers may not agree with this opinion, but for those who do,
;; 'minimal-light' is a VERY minimalistic color theme which use xterm-256 color set
;; so it has a unified look in both GUI and in Terminal. there is also a 
;; 'dark' version of this color theme (minimal-dark). 

;;; License:
;; Use of this source code is governed by the 'Revised BSD License'
;; which can be found in the LICENSE file.

;;; Code:

(deftheme minimal-light
  "A minimal theme based on xterm-256 color set [light version]")

(let ((black016  "#000000")   ;; black
      (white231  "#ffffff")   ;; white
      (yellow230 "#ffffdf")   ;; background
      (gray243   "#767676")   ;; text, code
      (gray249   "#b2b2b2")   ;; comments
      (gray255   "#eeeeee")   ;; fringe
      (red160    "#d70000")   ;; match
      (blue066   "#5f8787")   ;; strings
      (gray253   "#dadada")   ;; line numbers
      (yellow187 "#dfdfaf"))  ;; current line


  ;; Set faces
  (custom-theme-set-faces
   'minimal-light
   
   `(default ((t (:foreground ,black016 :background ,yellow230))))
   `(cursor  ((t (:background ,black016))))
   
   ;; Highlighting faces
   `(fringe    ((t (:background ,gray255))))   
   `(highlight ((t (:background ,white231))))
   `(region    ((t (:background ,yellow187))))
   
   ;; Font lock faces
   ;;   `(font-lock-string-face        ((t (:foreground ,blue066))))
   `(font-lock-string-face        ((t (:foreground ,black016))))
   `(font-lock-comment-face       ((t (:foreground ,gray243))))
   `(font-lock-constant-face      ((t (:foreground ,black016))))
   `(font-lock-function-name-face ((t (:foreground ,black016 :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,black016))))
   `(font-lock-builtin-face       ((t (:foreground ,black016))))
   `(font-lock-keyword-face       ((t (:foreground ,black016))))
   `(font-lock-type-face          ((t (:foreground ,black016))))

   ;;parens
   `(show-paren-mismatch   ((t (:foreground ,blue066 :background ,gray255 :weight bold))))
   `(show-paren-match      ((t (:foreground ,red160  :background ,gray255 :weight bold))))
	
   ;; line numbers, current line, mode-line
   ;`(hl-line-face ((t (:background ,red160 :weight bold))))
   `(hl-line      ((t (:background ,yellow187))))  ;;current line
   `(linum        ((t (:background ,gray253))))    ;;line numbers
   ;;   `(mode-line    ((t (:foreground ,yellow230 :background ,gray243 :box nil))))
   
   ;; web-mode
   `(web-mode-html-tag-face          ((t (:foreground ,gray243 :weight bold))))
   `(web-mode-html-attr-name-face    ((t (:foreground ,gray243 ))))
   `(web-mode-css-property-name-face ((t (:foreground ,gray243 :weight bold))))
   `(web-mode-keyword-face           ((t (:foreground ,gray243 :weight bold))))
   `(web-mode-builtin-face           ((t (:foreground ,gray243 :weight bold))))
   
   )

  ;; Set variables
  (custom-theme-set-variables
   'minimal-light 
   ;'(other variables)
   )
)

(provide-theme 'minimal-light)


;; Local Variables:
;; no-byte-compile: t
;; End:    

;;; minimal-light-theme.el ends here

