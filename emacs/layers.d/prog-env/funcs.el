(defun slurp-line-backwards ()
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (backward-kill-word 1)
    (delete-horizontal-space)))
