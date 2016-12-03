;; Minor mode for ASCII-armored gpg-encrypted files
(define-minor-mode auto-encryption-armored-mode
  "Save files in encrypted, ASCII-armored format"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Armor"
  ;; The minor mode bindings.
  nil
  (if (symbol-value auto-encryption-armored-mode)
      (set (make-local-variable 'epa-armor) t)
    (kill-local-variable 'epa-armor)))

(add-hook 'find-file-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) 
                           "asc")
              (auto-encryption-armored-mode 1))))

(provide auto-encryption-armored-mode)
