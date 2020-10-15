;;; ~/.config/doom/autoload/puppet.el -*- lexical-binding: t; -*-


;;;###autoload
(defun scoiatael/puppet-default-namespace ()
  "Use relative file name to infer namespace for element"
  (let* ((dir (file-name-directory (scoiatael/file-relative-name)))
        (dir-no-modules (s-replace "modules/" "" dir))
        (dir-no-manifests (s-replace "manifests/" "" dir-no-modules)))
    (s-replace "/" "::" dir-no-manifests)))

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
