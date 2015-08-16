;; Load Packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'cl)

(require 'neotree)

(let ((default-directory "~/.emacs.d/vendor"))
  (normal-top-level-add-subdirs-to-load-path))

(mapc 'load (directory-files "~/.emacs.d/custom" t "^[0-9]+.*\.el$"))

(require 'recentf)
(setq recentf-auto-cleanup 'never
      recentf-exclude '("[/\\]\\.elpa/"
			"[/\\]\\.ido\\.last\\'"
			"[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads\\.el\\'"
			"[/\\]archive-contents\\'"
			"[/\\]\\.loaddefs\\.el\\'"
			"url/cookies")
      recentf-save-file (expand-file-name ".recentf" "~/.emacs.d/")
      ;; save 100 most recent files
      recentf-max-saved-items 100)

(require 'fiplr)
(setq fiplr-ignored-globs '((directories (".git" ".svn" ".cask" "third_party"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(define-key evil-normal-state-map (kbd ";") 'fiplr-find-file)

(global-aggressive-indent-mode 1)
(dired-async-mode 1)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

(define-key evil-normal-state-map (kbd "<C-SPC>") 'helm-mini)

(require 'fuzzy)
(require 'auto-complete)
(setq ac-auto-show-menu 0.0)
(setq ac-use-fuzzy t)
(setq ac-comphist-file "/tmp/ac-comphist.dat")
(ac-config-default)

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)

(require 'hlinum)
(hlinum-activate)
(setq linum-format "%4d ")
(setq-default left-fringe-width  20)
(global-linum-mode 1)

(provide 'init)
;;; init.el ends here
