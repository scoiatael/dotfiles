(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)
