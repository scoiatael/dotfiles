(require 'helm-config)
(helm-mode 1)

(setq helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t)

(define-key evil-normal-state-map (kbd "<C-SPC>") 'helm-mini)
