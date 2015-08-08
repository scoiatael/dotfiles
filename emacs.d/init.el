;; Load Packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'cl)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(scroll-bar-mode 1)
(tool-bar-mode -1)

(require 'evil)
(evil-mode 1)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
				    (face-foreground 'mode-line))))
    (add-hook 'post-command-hook
    (lambda ()
	(let ((color (cond ((minibufferp) default-color)
			((evil-insert-state-p) '("#e80000" . "#ffffff"))
			((evil-emacs-state-p)  '("#444488" . "#ffffff"))
			((buffer-modified-p)   '("#006fa0" . "#ffffff"))
			(t default-color))))
	(set-face-background 'mode-line (car color))
	(set-face-foreground 'mode-line (cdr color))))))
