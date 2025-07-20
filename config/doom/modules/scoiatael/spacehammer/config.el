;;; modules/scoiatael/spacehammer/config.el -*- lexical-binding: t; -*-

(use-package! fennel-mode
  :config (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))

(add-to-list 'load-path (expand-file-name "~/.hammerspoon/spacehammer"))
(load "spacehammer.el")