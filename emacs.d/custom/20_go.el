;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'evil)

(defun my-go-mode-hook ()
  "Go-mode customizations."

  (add-to-list 'load-path
               (concat (getenv "GOPATH")
                       "/src/github.com/dougm/goflymake"))

  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                                        ; Godef jump key binding
  (define-key evil-normal-state-map "gF" 'godef-jump))


(add-hook 'go-mode-hook 'my-go-mode-hook)

(provide '20_go)
;;; 20_go ends here
