;;; modules/scoiatael/elisp/config.el -*- lexical-binding: t; -*-

(add-hook! #'emacs-lisp-mode
  (when (doom-real-buffer-p (current-buffer))
    (when (seq-find (lambda (dir) (file-in-directory-p (buffer-file-name) dir))
                    '("~/dotfiles" "~/.config" "~/.doom.d" "~/.emacs.d/lisp" "~/.emacs.d/modules"))
      (setq flymake-diagnostic-functions '(my-elisp-config-flymake-byte-compile)))
    (flymake-mode))

  (cl-callf append elisp-flymake-byte-compile-load-path load-path))

(after! smartparens
  (dolist (char '("f" "r"))
    (sp-local-pair '(python-mode) (concat char "'") "'")
    (sp-local-pair '(python-mode) (concat char "\"") "\""))
  (sp-local-pair '(python-mode) "\"\"\"" "\"\"\""))