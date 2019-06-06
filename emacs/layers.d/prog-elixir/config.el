(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                `(elixir-mode
                  ,(rx "do")  ; Block start
                  ,(rx "end") ; Block end
                  ,(rx "#")  ; Comment start
                  nil
                  nil)))

;; elixir-mode hook to format file after save
(add-hook 'elixir-mode-hook
          (lambda ()
            (require 'eglot)
            (add-to-list 'eglot-server-programs `(elixir-mode "~/Documents/elixir/elixir-ls/release/language_server.sh"))
            (eglot-ensure)
            (add-hook 'after-save-hook 'elixir-mix-format-current-file)))

(with-eval-after-load 'elixir-mode
  (spacemacs/declare-prefix-for-mode 'elixir-mode
    "mt" "tests" "testing related functionality")
  (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
    "tb" 'exunit-verify-all
    "ta" 'exunit-verify
    "tk" 'exunit-rerun
    "tt" 'exunit-verify-single))
