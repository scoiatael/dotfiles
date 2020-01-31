;;; ~/.config/doom/autoload/puppet.el -*- lexical-binding: t; -*-

;;;###autoload
(defun scoiatael/puppet-align-parameters ()
  "Align params of form Type $var = default, on $ and ="
  (interactive)
  (save-excursion
    (backward-up-list)
    (let ((beg (point))
          (regexp "\\(\\s-*\\)[\$=]"))
      (forward-list)
      (align-regexp beg (point) regexp 1 1 t))))
