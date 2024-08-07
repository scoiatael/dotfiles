;;; ../../dotfiles/config/doom/gleam.el -*- lexical-binding: t; -*-

(use-package gleam-ts-mode
  :config (add-to-list 'auto-mode-alist '("\\.gleam\\'" . gleam-ts-mode)))

(after! apheleia (push '(gleam-format . ("gleam format" filepath)) apheleia-formatters))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide '+private|gleam)
