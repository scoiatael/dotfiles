;;; modules/scoiatael/lsp/config.el -*- lexical-binding: t; -*-

(use-package! eldoc-box
  :config
  (map! :leader "h h" #'eldoc-box-help-at-point)
  (after! eglot
    (define-key eglot-mode-map (kbd "C-c I") #'eldoc-box-eglot-help-at-point)))

(after! eglot
  (add-to-list 'eglot-server-programs `((js-mode typescript-mode typescript-ts-mode typescript-tsx-mode rjsx-mode jtsx-typescript-mode) . ,(eglot-alternatives '(("deno" "lsp") ("typescript-language-server" "--stdio")))))
  (setq eglot-events-buffer-config '(:size 2000000 :format full))

  (setq lsp-nix-nixd-server-path "nixd"
        lsp-nix-nixd-formatting-command [ "nixfmt" ]
        lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
        lsp-nix-nixd-nixos-options-expr (concat "(builtins.getFlake \"" (getenv "HOME") "/dotfiles\").nixosConfigurations.LsFramework.options")
        lsp-nix-nixd-home-manager-options-expr (concat "(builtins.getFlake \"" (getenv "HOME") "/dotfiles\").homeConfigurations.\"lukaszczaplinski@LsFramework\".options"))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t)))
