;;; +private|spellchecking.el -*- lexical-binding: t; -*-

(use-package! jinx
  :config (add-hook 'emacs-startup-hook #'global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide '+private|spellchecking)
