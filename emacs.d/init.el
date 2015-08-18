;; Load Packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'cl)

(let ((default-directory "~/.emacs.d/vendor"))
  (normal-top-level-add-subdirs-to-load-path))

(mapc 'load (directory-files "~/.emacs.d/custom" t "^[0-9]+.*\.el$"))

(global-aggressive-indent-mode 1)
(dired-async-mode 1)

(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

(show-paren-mode 1)

(provide 'init)
;;; init.el ends here
