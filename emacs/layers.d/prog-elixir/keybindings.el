(evil-add-command-properties #'xref-find-definitions :jump t)

(evil-define-key '(normal visual motion) elixir-mode-map
  "gS" 'elixir-remove-do-sugar
  "gd" 'xref-find-definitions
  "gD" 'xref-find-definitions-other-window)
