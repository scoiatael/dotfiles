;;; modules/scoiatael/editor/config.el -*- lexical-binding: t; -*-

(setq
 display-line-numbers-type 'relative
 doom-localleader-key ","
 comint-prompt-read-only nil)

;; https://emacs.stackexchange.com/a/221
(define-key input-decode-map [?\C-i] [C-i])

(after! vterm
  (advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args)))))

(after! projectile
  (setq projectile-require-project-root t
        projectile-enable-idle-timer 90)
  (add-hook 'projectile-idle-timer-hook #'projectile-invalidate-cache))

(after! minimap
  (add-hook 'org-capture-mode-hook (lambda () (minimap-mode 0))))

(after! dired
  (define-key dired-mode-map (kbd "\\") #'ranger-toggle-dotfiles))