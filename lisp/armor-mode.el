;; Minor mode for ASCII-armored gpg-encrypted files
(define-minor-mode auto-encryption-armored-mode
  "Save files in encrypted, ASCII-armored format"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Encrypted,Armored"
  ;; The minor mode bindings.
  nil
  (if (symbol-value auto-encryption-armored-mode)
      (set (make-local-variable 'epa-armor) t)
    (kill-local-variable 'epa-armor))
  )
