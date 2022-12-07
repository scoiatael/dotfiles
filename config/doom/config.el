(setq-default
 doom-localleader-key ","
 ;; .emacs.d/ is RO thanks to home-manager :)
 doom-local-dir (file-truename "~/.emacs.local/")
 user-full-name "Lukasz Czaplinski"
 doom-font (font-spec :family "Iosevka" :size 13)
 doom-theme 'doom-one
 display-line-numbers-type t
 comint-prompt-read-only nil
 package-native-compile t
 gcmh-high-cons-threshold (* 512 1024 1024) ; 0.5GiB
 ns-right-alternate-modifier 'none
 lsp-use-plists t
 mac-right-option-modifier nil)

(setq-default python-shell-interpreter "python")

(setq-default ivy-use-selectable-prompt t
      ivy-use-virtual-buffers t
      enable-recursive-minibuffers t)

(setq-hook!
    '(typescript-mode-hook)
  +format-with-lsp nil)

(setq-default
 magit-repository-directories '(("~/Documents" . 2))
 magit-inhibit-save-previous-winconf t
 magit-save-repository-buffers nil)

(setq-default lsp-rust-server 'rust-analyzer)
(setq-default rustic-analyzer-command (concat doom-etc-dir "lsp/rust-analyzer"))

(setq-default web-mode-enable-engine-detection 't)

(setq-default +snippets-dir (file-truename "~/dotfiles/emacs-snippets"))

(after! git-gutter
  (setq-default
   git-gutter:deleted-sign "˗"
   git-gutter:added-sign "·"
   git-gutter:modified-sign "˃"))

(scoiatael/defer
 (add-hook 'prog-mode-hook #'turn-on-visual-line-mode)

 (setenv "SSH_AUTH_SOCK"
         (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket | xargs echo -n"))

 (add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("poetry.lock\\'" . conf-mode))

 (advice-add 'message :around #'scoiatael/suppress-math-support-messages)

 (add-hook! #'gfm-mode #'scoiatael/visualize-trailing-whitespace)

 ;; https://github.com/doomemacs/doomemacs/issues/3900#issuecomment-769556912
 (advice-add 'format-all-buffer :around #'envrc-propagate-environment)
 (advice-add 'format-all-buffer--from-hook :around #'envrc-propagate-environment)
 (advice-add '+format-buffer-h :around #'envrc-propagate-environment)

 (advice-add 'scoiatael/maybe-activate-virtualenv :around #'envrc-propagate-environment)

 (after! python
   (add-hook! python-mode
              #'scoiatael/maybe-activate-virtualenv
              #'evil-normal-state
              #'lispyville-mode)))

(after! eshell
  (require 'em-smart)
  (setq-default eshell-where-to-jump 'begin
                eshell-review-quick-commands nil
                eshell-smart-space-goes-to-end t)
  (add-to-list 'eshell-modules-list 'eshell-smart))

;; https://github.com/hlissner/doom-emacs/issues/3327#issuecomment-710543885
(after! 'smartparens
  (dolist (char '("f" "r"))
    (sp-local-pair '(python-mode) (concat char "'") "'")
    (sp-local-pair '(python-mode) (concat char "\"") "\""))
  (sp-local-pair '(python-mode) "\"\"\"" "\"\"\""))

(setq-default
   org-directory "~/Dropbox/org/"
   org-archive-location (concat org-directory "archive/%s::")
   org-ellipsis " ▼ "
   org-clock-persist t
   org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷"))

(after! org
  (add-to-list 'org-modules 'org-habit t)
  (add-hook! #'org-load
    (undo-tree-mode)
    (scoiatael/set-org-todo-keywords)))

(after! 'org-roam
  (remove-hook #'org-roam-find-file-hook
    #'+org-roam-open-with-buffer-maybe-h))

(after! 'poetry
  (remove-hook #'python-mode-hook #'poetry-tracking-mode))

;; TODO: upsert project space in tmux instead of cd
;; (after! projectile
;;   (add-hook #'projectile-after-switch-project-hook #'+tmux/cd-to-project))

(after! 'dap-mode
  (dap-register-debug-template
   "Python :: pytest focus"
   (list :type "python"
         :args "-m focus"
         :cwd nil
         :program nil
         :module "pytest"
         :request "launch"
         :name "Python :: pytest focus")))

(after! 'yasnippet
  (add-hook! 'snippet-mode
             (ws-butler-mode -1)
             (whitespace-newline-mode)))

(after! 'projectile
  (pushnew! projectile-project-root-files ".envrc"))

(after! 'lsp-mode
  (pushnew! lsp-file-watch-ignored-directories "[/\\\\]\\.direnv\\'" "[/\\\\]db\\'" "[/\\\\]tmp\\'" "[/\\\\]dist\\'"))

;; (map!
;;  :map python-pytest-mode-map
;;  "q" #'bury-buffer
;;  "G" #'+popup/raise)

(map! "M-'" #'+term/toggle
      "C-c s" #'ssh)


(map! :n "[p" (cmd! (newline) (evil-paste-after))
      :n "]p" (cmd! (save-excursion (line-move -1) (newline) (evil-paste-after))))

(map! :v
      "v" #'er/expand-region)

(map! :leader
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
      "s c" #'evil-ex-nohighlight
      "s e" #'iedit-mode
      "r R" (cmd! (counsel-projectile-find-file "README.md")))

(map! :leader
      "w <left>" #'evil-window-left
      "w <right>" #'evil-window-right
      "w <up>" #'evil-window-up
      "w <down>" #'evil-window-down)

(map! :leader
      :prefix "k"
      "d" #'server-edit
      "e" #'ediff-buffers
      "g" #'epa-encrypt-file
      "y" #'scoiatael/json-to-yaml)

(map! :leader
      :prefix "j"
      "l" #'avy-goto-word-1
      "n" #'avy-goto-line)

(map! :map magit-unmerged-section-map
      :leader
      :prefix "S"
      :desc "Next conflict" "{" #'smerge-next
      :desc "Prev conflict" "}" #'smerge-prev
      :desc "Keep current" "c" #'smerge-keep-current
      :desc "Keep all" "a" #'smerge-keep-all)

(map! :after org-mode
      :map org-mode-map
      "Q" #'org-roam-buffer-toggle
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

(map! :after swiper
      :map swiper-map
      "ESC" #'keyboard-quit
      "C-j" #'ivy-next-line
      "C-k" #'ivy-previous-line)

(map! :after python
      :map python-mode-map
      :localleader
      :prefix "d"
      :desc "Start debugging" "d" #'dap-debug
      :desc "Debugger hydra" "h" #'dap-hydra
      :desc "Debugger REPL" "r" #'dap-ui-repl
      :desc "Stop debugger" "q" #'dap-ui-delete-session
      :desc "Disconnect all debuggers" "Q" #'dap-delete-all-sessions
      :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle
      :desc "Show locals" "l" #'dap-ui-locals
      :prefix "i"
      :desc "Remove obsolete imports" "d" #'scoiatael/python-remove-unused-imports)

(map! :map org-roam-mode-map
      :leader
      "r i" #'org-roam-insert-immediate)

(map! :map #'clojure-mode-map
      :localleader
      "r a r" #'cljr-add-require-to-ns)

(use-package! origami
  :config (progn
            (origami-mode 1)
            (global-origami-mode)
            (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)))

(use-package! evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package! elvish-mode
  :mode ("\\.elv" . elvish-mode))

(use-package! tree-sitter)
(use-package! tree-sitter-langs)
(use-package! combobulate
  ;; Ensure `combobulate-mode` is activated when you launch a mode it supports
  :hook ((python-mode . combobulate-mode)
         ;; (js-mode . combobulate-mode)))
         (typescript-mode . combobulate-mode)))

;; Enable LSP for Puppet if puppet-editor-services is installed
(defvar scoiatael/puppet-editor-services nil "Set this to path to puppet-editor-services/puppet-languageserver")

(defun scoiatael/puppet-lsp-connection ()
  (when scoiatael/puppet-editor-services
    `(,scoiatael/puppet-editor-services
      "-c"
      "--stdio"
      "--debug=/tmp/emacs_puppet-editor-services.log"
      "--timeout=0")))

(use-package! puppet-mode
  :config
  (when (featurep! :tools lsp)
    (require 'lsp)
    (add-hook 'puppet-mode-hook #'lsp)
    (add-to-list 'lsp-language-id-configuration '(puppet-mode . "puppet"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection 'scoiatael/puppet-lsp-connection)
                      :activation-fn (lsp-activate-on "puppet")
                      :major-modes '(puppet-mode)
                      :server-id 'puppet-languageserver))))

(use-package! ob-http)
(use-package! jq-mode)

(use-package! coffee-mode)

(use-package! ox-slack)

(use-package! list-unicode-display)

(use-package! vlang-mode)

(use-package! nix-update
  :config
  (add-hook 'nix-mode-hook
            (lambda ()
              (local-set-key (kbd "C-. u") 'nix-update-fetch))))

;; (use-package! meson-mode
;;   :config
;;   (add-hook 'meson-mode-hook 'company-mode))

(use-package! ligature
  :config (global-ligature-mode t))

(load! (expand-file-name "packages/evil-colemak-dh.el" doom-private-dir))

(let ((custom-config-file (expand-file-name "./custom.el" (dir!))))
  (when (file-exists-p custom-config-file)
    (load-file custom-config-file)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
