(defun my-go-mode-hook ()
	(add-hook 'before-save-hook 'gofmt-before-save)
	(local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
																				; Godef jump key binding
	(local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode-hook 'my-go-mode-hook)
