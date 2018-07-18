;; Override SHELL in order for direnv to work better with Fish.
(setq exec-path-from-shell-shell-name "bash")
(setq shell-file-name (replace-regexp-in-string "\n$" ""  (shell-command-to-string "which bash")))
(setenv "shell" "bash")

;; I use symlinks extensively for my dotfiles. I don't want Spacemacs to complain each time I want to open .spacemacs
(setq vc-follow-symlinks t)

;; Autocomplete tweaks
;; see http://auto-complete.org/doc/manual.html#configuration
(setq ac-auto-show-menu 0.1)
(setq ac-delay 0.1)
(setq ac-use-fuzzy t)

;; Use spaces instead of tabs, width of tab is 2 spaces
(setq tab-width 2)
(setq tab-width 2)
(setq standard-indent 2)
(setq standard-indent 2)
(setq indent-tabs-mode nil)
(setq indent-tabs-mode nil)
