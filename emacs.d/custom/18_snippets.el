(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/yasmate/snippets"
	"~/.emacs.d/snippets/yasnippet-snippets"
	"~/.emacs.d/snippets/custom"
	))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
