;;; ~/.config/doom/autoload/puppet.el -*- lexical-binding: t; -*-


;;;###autoload
(defun scoiatael/puppet-default-namespace ()
  "Use relative file name to infer namespace for element"
  (s-replace "/" "::" (file-name-directory (scoiatael/file-relative-name))))

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
