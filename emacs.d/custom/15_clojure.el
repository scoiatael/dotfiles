(require 'clj-refactor)
(require 'clojure-mode)
(setq evil-lispy-modified-operators nil)
(require 'evil-lispy)

(defun scoiatael/clj-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (evil-lispy-mode 1)
					;(lispy-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(defun scoiatael/emacs-lisp-mode-hook ()
  (evil-lispy-mode t))

(add-hook 'clojure-mode-hook #'scoiatael/clj-mode-hook)

(add-hook 'emacs-lisp-mode-hook #'scoiatael/emacs-lisp-mode-hook)
