(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                `(elixir-mode
                  ,(rx "do")  ; Block start
                  ,(rx "end") ; Block end
                  ,(rx "#")  ; Comment start
                  nil
                  nil)))

(eval-after-load "alchemist" '(diminish 'alchemist-mode "🅐"))
(eval-after-load "alchemist-phoenix" '(diminish 'alchemist-phoenix-mode "㋭"))

;; https://github.com/syl20bnr/spacemacs/issues/9756
(eval-after-load "alchemist"
  '(setq spacemacs-default-jump-handlers
         (remove 'evil-goto-definition spacemacs-default-jump-handlers)))

;; elixir-mode hook to format file after save
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'after-save-hook 'elixir-mix-format-current-file)))
