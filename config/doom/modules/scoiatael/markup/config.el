;;; modules/scoiatael/markup/config.el -*- lexical-binding: t; -*-

(add-hook! #'haml-mode
  (setq-local tab-width 2))

(after! markdown-mode
  (add-hook! #'markdown-mode-hook #'scoiatael/visualize-trailing-whitespace))