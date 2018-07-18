(defconst prog-env-packages
  '(direnv))

(defun prog-env/init-direnv ()
    (direnv-update-environment)
    (add-hook 'prog-mode-hook 'direnv-mode))
