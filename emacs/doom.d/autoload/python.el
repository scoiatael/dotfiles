;;; ~/.config/doom/autoload/python.el -*- lexical-binding: t; -*-

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
;;;###autoload
(defun scoiatael/python-remove-unused-imports()
  "Use Autoflake to remove unused imports with
   autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))


(defun scoiatael/newline-and-indent-noninteractive ()
  (newline-and-indent)
  (indent-according-to-mode))

;;;###autoload
(defun scoiatael/python-insert-pry ()
  "Insert Pythonic pry comment (for fast debug)"
  (interactive)
  (save-excursion
      (end-of-line)
      (scoiatael/newline-and-indent-noninteractive)
      (insert "# fmt: off")
      (scoiatael/newline-and-indent-noninteractive)
      (insert "import pry; pry()")
      (scoiatael/newline-and-indent-noninteractive)
      (insert "# fmt: on")))


;;;###autoload
(defun scoiatael/maybe-activate-virtualenv ()
  "Activate virtualenv if inside one"
  (interactive)
  (message "Working on %s" (direnv--directory))
  (direnv-update-environment)
  (if-let ((virtualenv (getenv "VIRTUAL_ENV")))
      (progn
        (message "Found virtualenv: %s" virtualenv)
        (setq-local python-shell-interpreter (format "%s/bin/python" virtualenv))
        (setq-local lsp-python-ms-python-executable-cmd  (format "%s/bin/python" virtualenv))
        (pythonic-activate virtualenv)
        (force-mode-line-update))
  (message "No virtualenv found")))
