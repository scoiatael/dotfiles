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

(map! :map #'emacs-lisp-mode-map
      :localleader
      "g h" #'scoiatael/github-url-to-package-spec
      "." #'scoiatael/pin-package-to-current-version)

(use-package dwim-shell-command
  :ensure t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-shell-command] . dwim-shell-command))
  :config
  (require 'dwim-shell-commands)
  (setq  dwim-shell-commands-git-clone-dirs '("~/Documents/" "~/Desktop" "~/Downloads")))

