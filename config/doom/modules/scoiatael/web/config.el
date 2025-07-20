;;; modules/scoiatael/web/config.el -*- lexical-binding: t; -*-

;; Tree-sitter and combobulate setup
(when (version<= "29" emacs-version)
  (use-package treesit
    :preface
    (defun mp-setup-install-grammars ()
      "Install Tree-sitter grammars if they are absent."
      (interactive)
      (dolist (grammar
               ;; Note the version numbers. These are the versions that
               ;; are known to work with Combobulate *and* Emacs.
               '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                 (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
                 (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                 (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                 (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
                 (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                 (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
                 (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
                 (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                 (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                 (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
        (add-to-list 'treesit-language-source-alist grammar)
        ;; Only install `grammar' if we don't already have it
        ;; installed. However, if you want to *update* a grammar then
        ;; this obviously prevents that from happening.
        (unless (treesit-language-available-p (car grammar))
          (treesit-install-language-grammar (car grammar)))))

    :config
    (mp-setup-install-grammars)
    (use-package combobulate
      :custom
      ;; You can customize Combobulate's key prefix here.
      ;; Note that you may have to restart Emacs for this to take effect!
      (combobulate-key-prefix "C-s")
      :hook ((prog-mode . combobulate-mode)))))

;; Reason and ReScript
(use-package reason-mode
  :init
  (require 'reason-mode)
  (require 'merlin)
  (add-hook 'reason-mode-hook (lambda ()
                                (add-hook 'before-save-hook 'refmt-before-save)
                                (merlin-mode)))
  :config
  (cl-remove 'reason-mode auto-mode-alist :test 'equal :key 'cdr)
  (add-to-list 'auto-mode-alist '("\\.rei?\\'" . reason-mode)))

(use-package rescript-mode
  :init
  (require 'compile)
  :config
  (after! apheleia
    (setf (alist-get 'rescript apheleia-formatters)
          '("rescript" "format" "-stdin" ".res" ))
    (setf (alist-get 'rescript-mode apheleia-mode-alist)
          '(rescript)))
  (after! eglot
    (add-to-list 'eglot-server-programs
                 '(rescript-mode . ("rescript-language-server" "--stdio"))))
  :hook ((rescript-mode . (lambda () (electric-indent-local-mode -1)))))

;; Web mode configuration
(after! web-mode
  (add-to-list 'web-mode-engines-alist '("jinja2" . "\\.jinja2?\\'"))
  (setq-default web-mode-enable-engine-detection 't))