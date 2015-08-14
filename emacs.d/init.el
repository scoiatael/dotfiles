;; Load Packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'cl)

(require 'neotree)

(mapc 'load (directory-files "~/.emacs.d/custom" t "^[0-9]+.*\.el$"))

(setq recentf-auto-cleanup 'never
      recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads\\.el\\'" "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies")
      recentf-save-file (expand-file-name ".recentf" "/tmp/l")
      ;; save 100 most recent files
      recentf-max-saved-items 100)


(require 'helm-config)
(helm-mode 1)

(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map (kbd ";") 'fiplr-find-file)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
