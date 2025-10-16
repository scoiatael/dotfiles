;;; scoiatael/just/config.el -*- lexical-binding: t; -*-

(use-package! just-mode
  :config (add-to-list 'auto-mode-alist '("/\\(?:\\.\\)?[jJ][uU][sS][tT][fF][iI][lL][eE]\\'" . just-mode)))

(use-package! justl
  :config
  (setq justl-shell 'vterm)
  (advice-add 'justl :around #'envrc-propagate-environment)
  (map! :leader ";" #'justl)
  (map! :map #'justl-mode-map
        :n "x" 'justl-exec-recipe
        :n "X" 'justl-exec-shell))

