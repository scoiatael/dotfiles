(evil-add-command-properties #'xref-find-definitions :jump t)

(evil-define-key '(normal visual motion) go-mode-map
  "gd" 'xref-find-definitions
  "gD" 'xref-find-definitions-other-window)
