;;; modules/scoiatael/llm/config.el -*- lexical-binding: t; -*-

(after! gptel
  (gptel-make-kagi "Kagi"
    :key (lambda () (password-store-get "kagi-api-token")))

  (setq
   gptel-model 'claude-sonnet-4-6
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key (lambda () (password-store-get "anthropic-com-api-token"))))

  (map! :localleader
        "g n" #'scoiatael/gptel-send-to-new-buffer
        "g s" #'gptel-send
        "g g" #'gptel
        "g r" #'gptel-rewrite
        "g m" #'gptel-menu)

  ;; https://github.com/emacs-evil/evil-collection/issues/642
  (map! :map gptel-context-buffer-mode-map
        :n "q" #'gptel-context-quit
        :n "n" #'gptel-context-next
        :n "p" #'gptel-context-previous
        :n "d" #'gptel-context-flag-deletion
        :n "RET" #'gptel-context-visit) )

(use-package! llm-tool-collection
  :when (modulep! +tools)
  :config
  (after! gptel
    (setq gptel-tools (mapcar (apply-partially #'apply #'gptel-make-tool)
                              (mapcar #'symbol-value (-filter (lambda (tool)
                                                                (not (member (symbol-name tool)
                                                                             '("llm-tc/create-file" "llm-tc/create-directory"))))
                                                              llm-tool-collection--all-tools))))))

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

(use-package! amp
  :when (modulep! +amp))
