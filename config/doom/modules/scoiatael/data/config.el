;;; modules/scoiatael/data/config.el -*- lexical-binding: t; -*-

(use-package! ob-graphql)

(use-package! river-mode
  :config
  (add-hook! #'river-mode #'display-line-numbers-mode)
  (add-to-list 'auto-mode-alist '("\\.alloy\\'" . river-mode)))

(use-package! prisma-mode)