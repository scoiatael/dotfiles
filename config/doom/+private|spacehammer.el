;;; +private|spacehammer.el -*- lexical-binding: t; -*-

(use-package! fennel-mode
  :config (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))

(add-to-list              'load-path (expand-file-name "~/.hammerspoon/"))
(load "spacehammer.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide '+private|spacehammer)
