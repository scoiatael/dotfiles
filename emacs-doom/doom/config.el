(setq doom-localleader-key ","
      user-full-name "Lukasz Czaplinski"
      doom-font (font-spec :family "Iosevka" :size 12)
      doom-theme 'doom-one
      display-line-numbers-type t
      comint-prompt-read-only nil
      package-native-compile t
      ns-right-alternate-modifier 'none
      mac-right-option-modifier nil)

(setq python-shell-interpreter "python")

(setq ivy-use-selectable-prompt t
      ivy-use-virtual-buffers t
      enable-recursive-minibuffers t )

(setq org-directory "~/org/"
      org-archive-location (concat org-directory "archive/%s::")
      org-ellipsis " ▼ "
      org-clock-persist t
      org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷"))

(setq org-capture-templates
      '(("t" "todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t)
        ("n" "note" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t)
        ("p" "Templates for projects")
        ("pt" "Project-local todo" entry
         (file+headline +org-capture-project-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t)
        ("pn" "Project-local notes" entry
         (file+headline +org-capture-project-notes-file "Inbox")
         "* %U %?\n%i\n%a" :prepend t)
        ("pc" "Project-local changelog" entry
         (file+headline +org-capture-project-changelog-file "Unreleased")
         "* %U %?\n%i\n%a" :prepend t)
        ("o" "Centralized templates for projects")
        ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
        ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
        ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))

(setq magit-repository-directories '(("~/Documents" . 2))
      magit-inhibit-save-previous-winconf t
      magit-save-repository-buffers nil)

(setq lsp-rust-server 'rust-analyzer)
(setq rustic-analyzer-command (concat doom-etc-dir "lsp/rust-analyzer" ))

(setq web-mode-enable-engine-detection 't)

(setq! +snippets-dir "~/dotfiles/emacs/snippets")

(scoiatael/defer
 (add-hook 'prog-mode-hook #'turn-on-visual-line-mode)

 (setenv "SSH_AUTH_SOCK"
         (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket | xargs echo -n"))

 (add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
 (advice-add 'message :around #'scoiatael/suppress-math-support-messages)

 (add-hook! #'gfm-mode #'scoiatael/visualize-trailing-whitespace)

 (add-hook #'python-mode-hook #'scoiatael/maybe-activate-virtualenv)
 (add-hook #'python-mode-hook #'evil-normal-state))

(after! lsp-mode
  (setq lsp-enable-file-watchers nil)
  (dolist (dir '("[/\\\\]\\.direnv"))
    (push dir lsp-file-watch-ignored-directories))
  (add-to-list 'lsp-language-id-configuration '(puppet-mode . "puppet")))

(after! org
  (add-to-list 'org-modules 'org-habit t)
  (add-hook! #'org-load
    (scoiatael/set-org-todo-keywords)))

(after! poetry
  (remove-hook #'python-mode-hook #'poetry-tracking-mode))

(after! projectile
  (add-hook #'projectile-after-switch-project-hook #'+tmux/cd-to-project))

(after! dap-mode
  (dap-register-debug-template
   "Python :: pytest focus"
   (list :type "python"
         :args "-m focus"
         :cwd nil
         :program nil
         :module "pytest"
         :request "launch"
         :name "Python :: pytest focus")))

(after! projectile
  (pushnew! projectile-project-root-files ".envrc"))

(map!
 :map python-pytest-mode-map
 "q" #'bury-buffer
 "G" #'+popup/raise)

(map! "M-'" #'+eshell/toggle)

(map! :leader
      ">" #'spacemacs/alternate-buffer
      "R" #'org-roam-find-file
      "A" #'org-todo-list
      "D" #'deadgrep
      "SPC" #'counsel-M-x
      "/"   #'+default/search-project
      "i u" #'counsel-unicode-char
      "i d" #'scoiatael/yank-current-date
      "c x" #'lsp-treemacs-errors-list
      "w 2" #'split-window-below
      "w 3" #'split-window-right
      "a k" #'counsel-yank-pop
      "f d" #'dired-jump
      "f Y" #'scoiatael/yank-file-location
      "s c" #'evil-ex-nohighlight
      "s e" #'iedit-mode
      "r R" (cmd! (counsel-projectile-find-file "README.md")))

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

(use-package ion-mode
  :mode (("\\.ion\\'" . ion-mode) ("/ion/initrc\\'" . ion-mode))
  :config (add-to-list 'company-backends #'company-ion))

;; (use-package! org-superstar
;;   :config (add-hook! #'org-mode #'org-superstar-mode))

(use-package! dap-mode
  :after lsp-mode
  :init (setq dap-breakpoints-file (concat doom-etc-dir "dap-breakpoints")
              dap-utils-extension-path (concat doom-etc-dir "dap-extension/"))
  :config (progn
            (dap-mode 1)
            (dap-ui-mode 1)
            (dap-tooltip-mode 1)
            (tooltip-mode 1)
            (require 'dap-python)
            (require 'dapui)
            (add-hook 'dap-stopped-hook
                      (lambda (_arg) (call-interactively #'dap-ui-repl)))))


;; Add custom path for puppet-languageserver via
;; (after! lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("..some_dir../puppet-editor-services/puppet-languageserver" "--stdio"))
;;                     :activation-fn (lsp-activate-on "puppet")
;;                     :server-id 'puppet-languageserver)))

(use-package! emamux
  :config
  (map! :leader
        (:prefix ("v" . "tmux pane")
         :desc "Open new window in cd" :nv "n" #'emamux:new-window
         :desc "Send command" :nv "c" #'emamux:send-command)))

(let ((custom-config-file (expand-file-name "./custom.el" (dir!))))
  (when (file-exists-p custom-config-file)
    (load-file custom-config-file)))
