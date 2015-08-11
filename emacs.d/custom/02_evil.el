(require 'evil)
(require 'evil-leader)

(global-evil-leader-mode)
(evil-mode 1)

(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "e" 'neotree-toggle)

(add-hook 'neotree-mode-hook
  (lambda ()
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
