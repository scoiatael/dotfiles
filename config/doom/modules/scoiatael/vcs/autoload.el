;;; git.el --- Useful autoloads for git              -*- lexical-binding: t; -*-

;;;###autoload
(defun git-switch-previous-branch ()
  "Switch to the previous git branch in the current Magit repository."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (magit-run-git "switch" "-")))
