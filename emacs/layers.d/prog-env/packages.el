(defconst prog-env-packages
  '(direnv
    eglot
    company-lsp
    lsp-ui))

(defun prog-env/init-direnv ()
    (direnv-update-environment)
    (add-hook 'prog-mode-hook 'direnv-mode))

(defun prog-env/init-eglot ()
    (require 'eglot))

(defun prog-env/init-company-lsp ()
  (require 'company-lsp)
  (push 'company-lsp company-backends))

(defun prog-env/init-lsp-ui ()
  (require 'lsp-ui)
  (add-hook 'eglot-mode-hook 'lsp-ui-mode))
