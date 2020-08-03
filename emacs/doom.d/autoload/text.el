;;; ~/dotfiles/emacs/doom.d/autoload/text.el -*- lexical-binding: t; -*-

(defun scoiatael/yank (s)
  (message (kill-new s)))

;;;###autoload
(defun scoiatael/yank-file-location ()
  (interactive)
  (let ((file-name (file-relative-name buffer-file-name (projectile-project-root)))
        (line-num (line-number-at-pos)))
    (scoiatael/yank (format "%s:%d" file-name line-num))))

;;;###autoload
(defun scoiatael/yank-current-date ()
  (interactive)
  (scoiatael/yank (shell-command-to-string "date +%Y-%m-%d | xargs printf %s")))
