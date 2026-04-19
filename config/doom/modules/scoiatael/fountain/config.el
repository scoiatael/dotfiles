;;; config.el --- Fountain mode for screenwriting    -*- lexical-binding: t; -*-

(use-package! fountain-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode)))
