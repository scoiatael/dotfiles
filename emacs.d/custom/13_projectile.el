(require 'projectile)

(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)

(define-key evil-normal-state-map (kbd ";") 'projectile-find-file)
