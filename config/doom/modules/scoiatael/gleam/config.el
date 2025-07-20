;;; modules/scoiatael/gleam/config.el -*- lexical-binding: t; -*-

(use-package! gleam-ts-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.gleam\\'" . gleam-ts-mode))
  (add-hook 'gleam-ts-mode-hook
            #'eglot-ensure)
  (add-hook 'gleam-ts-mode-hook
            (lambda () (add-hook 'before-save-hook 'eglot-format-buffer  nil t)))
  (add-hook 'gleam-mode-hook
            (lambda () (add-hook 'before-save-hook 'gleam-format nil t))))

(after! apheleia 
  (push '(gleam-format . ("gleam format" filepath)) apheleia-formatters))

(after! projectile
  (projectile-register-project-type 'gleam '("gleam.toml")
                                    :project-file "gleam.toml"
				                    :compile "gleam build"
				                    :test "gleam test"
				                    :run "gleam run"
                                    :src-dir "src/"
                                    :test-dir "test/"
				                    :test-suffix "_test"))

(after! eglot
  (add-to-list 'eglot-server-programs
               '((gleam-mode gleam-ts-mode) . ("gleam" "lsp"))))
