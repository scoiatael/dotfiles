;;; ~/dotfiles/emacs/doom.d/autoload/text.el -*- lexical-binding: t; -*-

(defun scoiatael/yank (s)
  (message (kill-new s)))


;;;###autoload
(defun scoiatael/file-relative-name ()
  (file-relative-name buffer-file-name (projectile-project-root)))

;;;###autoload
(defun scoiatael/yank-file-location ()
  (interactive)
  (scoiatael/yank (format "%s:%d" (scoiatal/file-relative-name) (line-number-at-pos))))

;;;###autoload
(defun scoiatael/yank-current-date ()
  (interactive)
  (scoiatael/yank (shell-command-to-string "date +%Y-%m-%d | xargs printf %s")))
