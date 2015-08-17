(require 'clj-refactor)

(defun scoiatael/clj-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (lispy-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))
