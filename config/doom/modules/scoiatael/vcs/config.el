;;; modules/scoiatael/vcs/config.el -*- lexical-binding: t; -*-

(after! magit
  (add-hook 'find-file-hook #'jujutsu-commit-setup-check-buffer)
  (defconst jujutsu-commit-filename-regexp "\\.jjdescription\\'")

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude jujutsu-commit-filename-regexp))

  (add-to-list 'with-editor-file-name-history-exclude jujutsu-commit-filename-regexp)

  (defun jujutsu-commit-setup-check-buffer ()
    (when (and buffer-file-name
               (string-match-p jujutsu-commit-filename-regexp buffer-file-name))
      (git-commit-setup))))