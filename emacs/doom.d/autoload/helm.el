;;; ~/.config/doom/autoload/helm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun scoiatael/helm-rg-project ()
  "Start helm-rg with empty suggestion list"
  (interactive)
  (helm-projectile-rg ""))
