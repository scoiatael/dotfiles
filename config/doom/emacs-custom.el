(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" default))
 '(ignored-local-variable-values
   '((eval ignore-errors (require 'whitespace) (whitespace-mode 1))
     (whitespace-style face indentation)
     (eval progn (c-set-offset 'case-label '0) (c-set-offset 'innamespace '0)
      (c-set-offset 'inline-open '0))))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(rubocop-autocorrect-command "rubocop -A --format emacs")
 '(safe-local-variable-values
   '((+format-with . rubocop) (+format-with . rubocop-format-current-file-silent)
     (+format-with quote rubocop-format-current-file-silent)
     (+format-with . "eslint") (engine . jinja2)
     (flycheck-disabled-checkers emacs-lisp-checkdoc))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
