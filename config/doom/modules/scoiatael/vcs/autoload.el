;;; git.el --- Useful autoloads for git              -*- lexical-binding: t; -*-

;;;###autoload
(defun scoiatael/git-switch-previous-branch ()
  "Switch to the previous git branch in the current Magit repository."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (magit-run-git "switch" "-")))

;;;###autoload
(defun scoiatael/git-pull-main ()
  "Switch to main branch and pull"
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (magit-run-git "switch" (cdr (magit--get-default-branch)))
    (magit-run-git "pull")))
