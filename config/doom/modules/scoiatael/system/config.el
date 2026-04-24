;;; modules/scoiatael/system/config.el -*- lexical-binding: t; -*-

(setq
 user-full-name "Lukasz Czaplinski"
 git-commit-summary-max-length 120
 gcmh-high-cons-threshold (* 1024 1024 1024)) ; 1GiB

;; Org capture frame parameters
(setf (alist-get 'width +org-capture-frame-parameters) 180)
(setf (alist-get 'height +org-capture-frame-parameters) 20)

;; macOS specific settings
(when (featurep :system 'macos) (setq mac-right-option-modifier nil))

;; NOTE: I'd really like it but it clashes with :ui workspaces
;; https://github.com/doomemacs/doomemacs/issues/6205
;; (setq uniquify-buffer-name-style 'forward)

;; Used by customization system
(setq custom-file "~/dotfiles/config/doom/emacs-custom.el")
(load custom-file)
