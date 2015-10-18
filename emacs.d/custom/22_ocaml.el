;;; package --- Summary
;;; Commentary:

;;; Code:

(let ((opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1)))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp")))

(require 'merlin)

(defun my-tuareg-mode-hook ()
  "Ocaml-mode customizations."

  (setq-default merlin-use-auto-complete-mode 'easy)
  (setq-default merlin-command 'opam)

  (merlin-mode t))


(add-hook 'tuareg-mode 'my-tuareg-mode-hook)

(provide '22_ocaml)
;;; 22_ocaml ends here
