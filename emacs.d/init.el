;; Load Packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'cl)

(mapc 'load (directory-files "~/.emacs.d/custom" t "^[0-9]+.*\.el$"))

(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
(require 'powerline)
(powerline-evil-theme)
(setq powerline-arrow-shape 'curve) ;; best for small fonts
