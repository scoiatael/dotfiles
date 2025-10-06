;;; modules/scoiatael/ruby/config.el -*- lexical-binding: t; -*-

(after! eglot
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) "ruby-lsp")))
