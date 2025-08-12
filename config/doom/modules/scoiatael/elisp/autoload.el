;;; ../../dotfiles/config/doom/autoload/elisp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my-elisp-config-flymake-byte-compile (report-fn &rest _args)
  "A Flymake backend for elisp byte compilation.
Spawn an Emacs process that byte-compiles a file representing the
current buffer state and calls REPORT-FN when done."
  (when elisp-flymake--byte-compile-process
    (when (process-live-p elisp-flymake--byte-compile-process)
      (kill-process elisp-flymake--byte-compile-process)))
  (let ((temp-file (make-temp-file "elisp-flymake-byte-compile"))
        (source-buffer (current-buffer))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'nomessage))
    (let* ((output-buffer (generate-new-buffer " *elisp-flymake-byte-compile*")))
      (setq
       elisp-flymake--byte-compile-process
       (make-process
        :name "elisp-flymake-byte-compile"
        :buffer output-buffer
        :command `(,(expand-file-name invocation-name invocation-directory)
                   "--batch"
                   "--eval" "(setq byte-compile-warnings '(not unresolved docstrings))"
                   "-l" "~/.emacs.doom/lisp/doom.el"
                   "-l" "~/.emacs.doom/lisp/doom-start.el"
                   "-l" "~/.emacs.local/init.el"
                   ;; "--eval" "(setq load-prefer-newer t)" ; for testing
                   ,@(mapcan (lambda (path) (list "-L" path))
                             elisp-flymake-byte-compile-load-path)
                   "-f" "elisp-flymake--batch-compile-for-flymake"
                   ,temp-file)
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (unless (process-live-p proc)
            (unwind-protect
                (cond
                 ((not (and (buffer-live-p source-buffer)
                            (eq proc (with-current-buffer source-buffer
                                       elisp-flymake--byte-compile-process))))
                  (flymake-log :warning
                               "byte-compile process %s obsolete" proc))
                 ((zerop (process-exit-status proc))
                  (elisp-flymake--byte-compile-done report-fn
                                                    source-buffer
                                                    output-buffer))
                 (t
                  (funcall report-fn
                           :panic
                           :explanation
                           (format "byte-compile process %s died" proc))))
              (ignore-errors (delete-file temp-file))
              (kill-buffer output-buffer))))
        :stderr " *stderr of elisp-flymake-byte-compile*"
        :noquery t)))))


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


;;;###autoload
(defun scoiatael/extract-package-name-from-line (line)
  "Extract package name from a package! declaration line."
  (let ((match (s-match (rx "(package! "
                            (group (one-or-more (not " ")))
                            (zero-or-more anychar)
                            ")")
                        line)))
    (when match (car (cdr match)))))

(ert-deftest extracting-package-name ()
  "Tests that scoiatael/extract-package-name-from-line works"
  (should (equal (scoiatael/extract-package-name-from-line "(package! macher)") "macher"))
  (should (equal (scoiatael/extract-package-name-from-line "(package! gptel :pin \"00bcdf0551f97e0b496614a6dcebd5fdeda4751b\")") "gptel"))
  (should (equal (scoiatael/extract-package-name-from-line "(package! macher :recipe (:host github :repo \"kmontag/macher\"))") "macher")))

;;;###autoload
(defun scoiatael/get-package-current-commit (package-name)
  "Get the current commit hash for PACKAGE-NAME from straight.el."
  (let* ((package (intern-soft package-name))
         (local-repo (doom-package-recipe-repo package))
         (repo-dir (straight--repos-dir local-repo)))
    (when (file-directory-p repo-dir)
      (let ((default-directory repo-dir))
        (string-trim (shell-command-to-string "git rev-parse HEAD"))))))

;;;###autoload
(defun scoiatael/pin-package-to-current-version ()
  "Pin the package! declaration on current line to its currently installed version.
Works with package declarations like:
  (package! macher :recipe (:host github :repo \"kmontag/macher\"))
  (package! gptel)
  (package! nov :pin \"old-hash\")
Replaces or adds :pin with current git hash from straight.el."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((line (thing-at-point 'line t))
           (package-name (scoiatael/extract-package-name-from-line line))
           (current-commit (when package-name
                             (scoiatael/get-package-current-commit package-name))))
      (if (and package-name current-commit)
          (let ((new-line
                 (cond
                  ;; Already has :pin, replace it
                  ((string-match ":pin\\s-+\"[^\"]*\"" line)
                   (replace-regexp-in-string ":pin\\s-+\"[^\"]*\""
                                             (format ":pin \"%s\"" current-commit)
                                             line))
                  ;; Has other parameters, add :pin before closing paren
                  ((string-match ")\\s-*$" line)
                   (replace-regexp-in-string ")\\s-*$"
                                             (format " :pin \"%s\")" current-commit)
                                             line))
                  ;; Simple case, just package name
                  ((string-match "(package!\\s-+\\([^)]+\\))" line)
                   (replace-regexp-in-string "(package!\\s-+\\([^)]+\\))"
                                             (format "(package! %s :pin \"%s\")"
                                                     package-name current-commit)
                                             line)))))
            (beginning-of-line)
            (kill-line)
            (insert (string-trim new-line))
            (message "Pinned %s to %s" package-name (substring current-commit 0 7)))
        (message "Could not extract package name or find current commit for line: %s" (string-trim line))))))
