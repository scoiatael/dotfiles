;; load packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'cl-lib)
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
(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

(add-to-list 'post-command-hook 'whitespace-cleanup)
(add-to-list 'before-save-hook 'whitespace-cleanup)

(setq tab-width 2)
(setq standard-indent 2)
(setq js-indent-level 2)
(setq indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(provide 'init)
;;; init.el ends here
