(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(ignored-local-variable-values
   '((eval ignore-errors
      (require 'whitespace)
      (whitespace-mode 1))
     (whitespace-style face indentation)
     (eval progn
      (c-set-offset 'case-label '0)
      (c-set-offset 'innamespace '0)
      (c-set-offset 'inline-open '0))))
 '(safe-local-variable-values '((flycheck-disabled-checkers emacs-lisp-checkdoc))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
