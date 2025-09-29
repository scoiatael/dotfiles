;;; modules/scoiatael/ai/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :when (modulep! +gptel)
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

(use-package! macher
  :when (modulep! +gptel +macher)
  :custom
  ;; The org UI has structured navigation and nice content folding.
  (macher-action-buffer-ui 'default)

  :config
  (after! gptel
    (macher-install))
  ;; Adjust buffer positioning to taste.
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . bottom)))
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher-patch:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . right)))
  )

(use-package! claude-code-ide
  :when (modulep! +claude)
  ;; :bind '("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config (claude-code-ide-emacs-tools-setup))

(use-package! chatgpt-shell
  :when (modulep! +shell)
  :custom
  (chatgpt-shell-anthropic-key (lambda () (password-store-get "anthropic-com-api-token"))))
