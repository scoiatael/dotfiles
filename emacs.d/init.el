;; load packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'cl)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(let ((default-directory "~/.emacs.d/vendor"))
  (normal-top-level-add-subdirs-to-load-path))

(mapc 'load (directory-files "~/.emacs.d/custom" t "^[0-9]+.*\.el$"))

(global-aggressive-indent-mode 1)
(dired-async-mode 1)

(require 'ruby-electric)
(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
(setq-default tab-width 2)

(show-paren-mode 1)
(electric-pair-mode 1)

(provide 'init)
;;; init.el ends here
