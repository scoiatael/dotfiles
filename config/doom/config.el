;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Configuration has been migrated to private modules in modules/scoiatael/
;; Each module contains related functionality:
;;
;; - appearance: themes, fonts, dashboard, org-modern styling
;; - completion: company settings
;; - editor: line numbers, scrolling, navigation, projectile
;; - system: user info, frame parameters, macOS settings, custom files
;; - elisp: emacs-lisp mode configuration, smartparens
;; - web: treesitter, combobulate, reason, rescript, web-mode
;; - markup: markdown, haml configuration
;; - data: graphql, prisma, river-mode (alloy)
;; - devops: terraform/infrastructure tooling
;; - ai: gptel configuration and AI integrations
;; - build: justl, apheleia, formatters
;; - vcs: git, magit, jujutsu configuration
;; - lsp: eglot, eldoc-box, language server settings
;; - org: org-mode, capture, roam, agenda configuration
;; - keybinds: all custom key mappings
;; - modeline: custom modeline with keycast and spaceline
;; - spellcheck: jinx spell checking configuration  
;; - email: notmuch email client settings
;;
;; Optional modules (commented out in init.el):
;; - llm: ellama/ollama integration
;; - gleam: gleam language support
;; - spacehammer: hammerspoon/fennel integration
;;
;; To enable optional modules, uncomment them in init.el and run 'doom sync'

;; Load any remaining custom configurations that don't fit in modules
(let ((custom-config-file (expand-file-name "./custom.el" (dir!))))
  (when (file-exists-p custom-config-file)
    (load-file custom-config-file)))

;; Used by customization system
(setq custom-file "~/dotfiles/config/doom/emacs-custom.el")
(load custom-file)