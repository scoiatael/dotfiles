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

(setq-default comment-multi-line t)

(let ((default-directory "~/.emacs.d/vendor"))
  (normal-top-level-add-subdirs-to-load-path))

(mapc 'load (directory-files "~/.emacs.d/custom" t "^[0-9]+.*\.el$"))

(global-aggressive-indent-mode 1)
(dired-async-mode 1)
(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

(add-to-list 'post-command-hook 'whitespace-cleanup)
(add-to-list 'before-save-hook 'whitespace-cleanup)

(setq tab-width 2)
(setq-default tab-width 2)
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-auto-revert-mode 1)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

(add-hook 'haml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)))

(provide 'init)
;;; init.el ends here
