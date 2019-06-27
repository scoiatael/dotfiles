(defconst prog-env-packages
  '(direnv
    eglot))

(defun prog-env/init-direnv ()
    (direnv-update-environment)
    (add-hook 'prog-mode-hook 'direnv-mode))

(defun prog-env/init-eglot ()
    (require 'eglot))
