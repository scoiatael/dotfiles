;;; ~/.config/doom/autoload/nix.el -*- lexical-binding: t; -*-

;;;###autoload
(defun scoiatael/github-url-to-nix-flake-input (url)
  "Convert a GitHub URL to nix flake input form.
Takes a URL like 'git@github.com:WootingKb/just-flake.git' or
'https://github.com/WootingKb/just-flake.git' and returns
'just-flake.url = \"github:WootingKb/just-flake\"'"
  (interactive "sGitHub URL: ")
  (let* ((cleaned-url (replace-regexp-in-string "\.git$" "" url))
         (repo-parts (cond
                      ;; SSH format (git@github.com:user/repo)
                      ((string-match "git@github.com:\\([^/]+\\)/\\(.+\\)" cleaned-url)
                       (list (match-string 1 cleaned-url) (match-string 2 cleaned-url)))
                      ;; HTTPS format (https://github.com/user/repo)
                      ((string-match "https://github.com/\\([^/]+\\)/\\(.+\\)" cleaned-url)
                       (list (match-string 1 cleaned-url) (match-string 2 cleaned-url)))
                      (t (error "Unrecognized GitHub URL format"))))
         (user (nth 0 repo-parts))
         (repo (nth 1 repo-parts))
         (flake-input (format "%s.url = \"github:%s/%s\";" repo user repo)))
    (when (called-interactively-p 'any)
      (insert flake-input))
    flake-input))
