;;; ~/dotfiles/emacs/doom.d/autoload/file-path.el -*- lexical-binding: t; -*-

;;;autoload
(defun scoiatael/yank-file-location ()
  (interactive)
  (let ((file-name (file-relative-name buffer-file-name (projectile-project-root)))
        (line-num (line-number-at-pos)))
    (message (kill-new (format "%s:%d" file-name line-num)))))
