;;; ~/.config/doom/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun scoiatael/set-org-todo-keywords ()
  (setq org-todo-keywords '((sequence "TODO" "HOLD" "DONE"))))

;;;###autoload
(defun scoiatael/github-url-to-package-spec (url)
  "Convert a GitHub URL to a `package!' declaration for doom-emacs.
Takes a URL like 'https://github.com/jdormit/ob-graphql.git' and
returns '(package! ob-graphql :recipe (:host github :repo \"jdormit/ob-graphql\"))'"
  (interactive "sGitHub URL: ")
  (let* ((url-without-prefix (replace-regexp-in-string "https://github.com/" "" url))
         (url-without-suffix (replace-regexp-in-string "\.git$" "" url-without-prefix))
         (repo-parts (split-string url-without-suffix "/"))
         (user (nth 0 repo-parts))
         (repo (nth 1 repo-parts))
         (package-spec (format "(package! %s :recipe (:host github :repo \"%s/%s\"))\n(use-package! %s :init :config)" repo user repo repo)))
    (when (called-interactively-p 'any)
      (insert package-spec))
    package-spec))
