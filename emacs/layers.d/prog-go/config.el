(add-hook 'go-mode-hook
          (lambda ()
            (require 'eglot)
            (add-to-list 'eglot-server-programs `(go-mode "gopls"))
            (eglot-ensure)))
