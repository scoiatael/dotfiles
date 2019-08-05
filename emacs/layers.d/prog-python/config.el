
 (add-hook 'elixir-mode-hook
           (lambda ()
             (require 'eglot)
              (add-to-list 'eglot-server-programs
                 `(python-mode . ("pyls" "-v" "--tcp" "--host"
                                  "localhost" "--port" :autoport)))
             (eglot-ensure)))
