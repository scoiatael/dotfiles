;;; modules/scoiatael/ai/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (gptel-make-kagi "Kagi"
    :key (lambda () (password-store-get "kagi-api-token")))

  (setq
   gptel-model 'claude-3-7-sonnet-20250219
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key (lambda () (password-store-get "anthropic-com-api-token"))))

  (map! :localleader
        "g g" #'gptel
        "g r" #'gptel-rewrite
        "g s" #'gptel-send
        "g m" #'gptel-menu))
