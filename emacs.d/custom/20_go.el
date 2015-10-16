;; go get -u github.com/dougm/goflymake
;; go get -u -v github.com/nsf/gocode
;; go get golang.org/x/tools/cmd/goimports

(defun my-go-mode-hook ()
  (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))
  (require 'go-flycheck)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                                        ; Godef jump key binding
  (define-key evil-normal-state-map "gF" 'godef-jump))


(add-hook 'go-mode-hook 'my-go-mode-hook)
