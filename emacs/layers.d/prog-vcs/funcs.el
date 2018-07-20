(defun fullscreen-magit-status ()
  (interactive)
  (magit-status)
  (delete-other-windows))
