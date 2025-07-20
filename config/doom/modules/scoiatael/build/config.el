;;; modules/scoiatael/build/config.el -*- lexical-binding: t; -*-

(use-package! justl
  :config
  (setq justl-shell 'vterm)
  (advice-add 'justl :around #'envrc-propagate-environment)
  (map! :leader ";" #'justl)
  (map! :map #'justl-mode-map
        :n "x" 'justl-exec-recipe
        :n "X" 'justl-exec-shell))

(use-package! apheleia
  :config
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist)
        '(rufo))
  (setf (alist-get 'ruby-mode apheleia-mode-alist)
        '(rufo))
  (setf (alist-get 'prisma apheleia-formatters)
        '("~/dotfiles/bin/prisma-formatter" filepath))
  (setf (alist-get 'prisma-mode apheleia-mode-alist)
        '(prisma))
  (setf (alist-get 'tofu apheleia-formatters)
        '("tofu" "fmt" "-"))
  (setf (alist-get 'terraform-mode apheleia-mode-alist)
        'tofu))

(after! format
  (advice-add 'format-all-buffer :around #'envrc-propagate-environment)
  (advice-add 'format-all-buffer--from-hook :around #'envrc-propagate-environment)
  (advice-add '+format-buffer-h :around #'envrc-propagate-environment))