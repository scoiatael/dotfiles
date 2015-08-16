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

(setq recentf-auto-cleanup 'never
      recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads\\.el\\'" "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies")
      recentf-save-file (expand-file-name ".recentf" "/tmp/l")
      ;; save 100 most recent files
      recentf-max-saved-items 100)

(setq fiplr-ignored-globs '((directories (".git" ".svn" ".cask" "third_party"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(define-key evil-normal-state-map (kbd ";") 'fiplr-find-file)

(global-aggressive-indent-mode 1)
(dired-async-mode 1)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

(define-key evil-normal-state-map (kbd "<C-SPC>") 'helm-mini)
