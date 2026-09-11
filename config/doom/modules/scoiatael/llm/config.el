;;; modules/scoiatael/llm/config.el -*- lexical-binding: t; -*-

(after! gptel
  (gptel-make-kagi "Kagi"
    :key (lambda () (password-store-get "kagi-api-token")))

  (setq
   gptel-model 'claude-sonnet-4-6
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key (lambda () (password-store-get "anthropic-com-api-token"))))

  ;; https://github.com/emacs-evil/evil-collection/issues/642
  (map! :map gptel-context-buffer-mode-map
        :n "q" #'gptel-context-quit
        :n "n" #'gptel-context-next
        :n "p" #'gptel-context-previous
        :n "d" #'gptel-context-flag-deletion
        :n "RET" #'gptel-context-visit) )

;; Read-only companion agent over ACP, sandboxed by nono. Backed by either
;; maki or claude-agent-acp -- see `scoiatael/companion-backend'. The launchers
;; live in modules/aspects/companion.nix, and config/nono/{maki,claude}-companion.json
;; is what actually decides what either of them is allowed to touch.
(use-package! agent-shell
  :when (modulep! +agent)
  :init
  (setq agent-shell-permission-responder-function
        #'scoiatael/agent-shell-companion-permission-responder)
  :config
  (add-to-list 'agent-shell-agent-configs
               #'scoiatael/agent-shell-maki-companion-config)
  ;; A dedicated side window, treemacs style: it survives `delete-other-windows'
  ;; and keeps its width, so the companion stays visible next to the code.
  (setq agent-shell-display-action
        '((display-buffer-in-side-window)
          (side . left)
          (slot . 0)
          (window-width . 0.4)
          (window-parameters . ((no-delete-other-windows . t)))))
  (map! :localleader
        "g c" #'scoiatael/agent-shell-maki-companion
        "g v" #'scoiatael/agent-shell-companion-review))
