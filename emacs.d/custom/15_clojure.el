(require 'clj-refactor)
(require 'clojure-mode)

(defun scoiatael/clj-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (evil-lispy-mode 1)
  ;(lispy-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'scoiatael/clj-mode-hook)

(require 'evil-lispy)
