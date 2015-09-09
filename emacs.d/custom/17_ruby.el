(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; disable aggresive indent
(add-hook 'ruby-mode-hook 'aggressive-indent-mode)

(defun my-ruby-mode-hook ()
  (push 'ac-source-yasnippet ac-sources))
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(provide '17_ruby)
;;; 17_ruby.el ends here
