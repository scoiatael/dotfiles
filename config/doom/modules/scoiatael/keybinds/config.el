;;; modules/scoiatael/keybinds/config.el -*- lexical-binding: t; -*-

;; Insert mode keybinds
(map! :i ", SPC" (cmd! (insert ", ")))
(map! :i ", RET" (cmd! (insert ",\n")))
(map! :i ", f" (cmd! (call-interactively #'evil-snipe-f)))
(map! :i ", F" (cmd! (call-interactively #'evil-snipe-F)))
(map! :i ", ;" (cmd! (save-excursion (up-list) (insert ";") )))
(map! :i ", ESC" (cmd! (insert ",") (evil-escape)))
(map! :i "<C-i>" #'up-list)

;; Normal mode keybinds
(map! :n "]p" #'evil-unimpaired-paste-below
      :n "[p" #'evil-unimpaired-paste-above
      :n "!" #'+default/yank-pop
      :n "\\" #'+vterm/toggle
      :v "[6" #'base64-decode-region
      :v "]6" #'base64-encode-region
      :v "v" #'er/expand-region)

(map! :n "M-<up>" #'evil-window-up
      :n "M-<right>"  #'evil-window-right
      :n "M-<left>"  #'evil-window-left
      :n "M-<down>"  #'evil-window-down)

;; Leader keybinds
(map! :leader
      "-" #'dirvish
      "\\" #'lsp-ui-doc-show
      ">" #'spacemacs/alternate-buffer
      "R" #'org-roam-node-find
      "A" #'org-todo-list
      "D" #'deadgrep
      "/"   #'+default/search-project
      "i u" #'list-unicode-display
      "i d" #'scoiatael/yank-current-date
      "w 2" #'split-window-below
      "w 3" #'split-window-right
      "f d" #'dired-jump
      "f Y" #'scoiatael/yank-file-location
      "g -" #'git-switch-previous-branch
      "s c" #'evil-ex-nohighlight
      "s e" #'iedit-mode
      "r R" (cmd! (find-file (projectile-rails-expand-root "README.md"))))

(map! :leader
      "w <left>" #'evil-window-left
      "w <right>" #'evil-window-right
      "w <up>" #'evil-window-up
      "w <down>" #'evil-window-down)

(map! :leader
      :prefix "k"
      "c" (cmd! (chmod buffer-file-name (read-file-modes "chmod:" buffer-file-name)))
      "d" #'server-edit
      "e" #'ediff-buffers
      "g" #'epa-encrypt-file
      "y" #'scoiatael/json-to-yaml)

(map! :leader
      :prefix "j"
      "l" #'avy-goto-word-1
      "n" #'avy-goto-line)

;; Mode-specific keybinds
(map! :map magit-unmerged-section-map
      :n :desc "Next conflict" "]s" #'smerge-next
      :n :desc "Prev conflict" "[s" #'smerge-prev
      :n :desc "Keep current" "C" #'smerge-keep-current
      :n :desc "Keep all" "A" #'smerge-keep-all)

(map! :map git-commit-mode-map
      :n :desc "Commit" "]c" #'with-editor-finish
      :n :desc "Cancel" "]k" #'with-editor-cancel)

(map! :after org-mode
      :map org-mode-map
      :desc "dwim" "C-M-x" #'+org/dwim-at-point
      :desc "toggle folds" "TAB" #'org-cycle)

(map! :map puppet-mode-map
      :after puppet-mode
      :localleader
      :desc "Align block" "b" #'puppet-align-block
      :desc "Align class params" "p" #'scoiatael/puppet-align-parameters
      :desc "Toggle string quotes" "'" #'puppet-toggle-string-quotes)

(map! :map direnv-envrc-mode-map
      :after direnv
      :localleader
      :desc "Allow envrc" "a" #'direnv-allow)

(map! :map eglot-mode-map
      :localleader
      :desc "Quickfix" "a" #'eglot-code-action-quickfix)

(map! :map org-roam-mode-map
      :leader
      "r i" #'org-roam-insert-immediate)
