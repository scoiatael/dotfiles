(use-package origami
  :config
  (origami-mode 1)
  :after
  (global-origami-mode)
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(setq
 doom-localleader-key ","
 user-full-name "Lukasz Czaplinski"
 doom-font (font-spec :family "Iosevka" :size 12)
 doom-theme 'doom-one
 display-line-numbers-type t)

(add-hook 'prog-mode-hook #'turn-on-visual-line-mode)

(setq ns-right-alternate-modifier 'none)
(setq mac-right-option-modifier nil)

(map!
 "M-'" #'+eshell/toggle)

(map! :leader
      ">" #'spacemacs/alternate-buffer
      "R" #'org-roam-find-file
      "A" #'org-todo-list
      "SPC" #'counsel-M-x
      "/"   #'+default/search-project
      "i u" #'counsel-unicode-char
      "i d" #'scoiatael/yank-current-date
      "j l" #'avy-goto-word-1
      "c x" #'lsp-treemacs-errors-list
      "f d" #'dired-jump
      "s c" #'evil-ex-nohighlight
      "w 2" #'split-window-below
      "w 3" #'split-window-right
      "a k" #'counsel-yank-pop
      "f Y" #'scoiatael/yank-file-location
      "s e" #'iedit-mode
      "S n" #'smerge-next
      "S p" #'smerge-prev
      "S c" #'smerge-keep-current
      )

;; Group helpful misc Emacs commands under "SPC-k"
(map! :leader
      :prefix "k"
      "d" #'server-edit
      "e" #'ediff-files
      "g" #'epa-encrypt-file
      )

(use-package! evil-surround
  :config
    (global-evil-surround-mode 1))

(after! swiper
  (define-key swiper-map (kbd "ESC") #'keyboard-quit)
  (define-key swiper-map (kbd "C-j") #'ivy-next-line)
  (define-key swiper-map (kbd "C-k") #'ivy-previous-line))

(setq ivy-use-selectable-prompt t
      ivy-use-virtual-buffers t
      enable-recursive-minibuffers t )

(setq org-directory "~/org/"
      org-archive-location (concat org-directory "archive/%s::")
      org-ellipsis " ▼ "
      org-clock-persist t
      org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷"))

(use-package! org-superstar
  :config
  (add-hook! #'org-mode (org-superstar-mode 1)))

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


(add-hook! #'org-load
           (scoiatael/set-org-todo-keywords))

(after! org
  (add-to-list 'org-modules 'org-habit t))

(map! :after org-mode
      :map org-mode-map
      :desc "dwim" "C-M-x" #'+org/dwim-at-point
      :desc "toggle folds" "TAB" #'org-cycle)

(setq magit-repository-directories '(("~/Documents" . 2))
      magit-inhibit-save-previous-winconf t
      magit-save-repository-buffers nil)

(map!
 :map puppet-mode-map
 :after puppet-mode
 :localleader
 :desc "Align block" "b" #'puppet-align-block
 :desc "Align class params" "p" #'scoiatael/puppet-align-parameters
 :desc "Toggle string quotes" "'" #'puppet-toggle-string-quotes)

(map!
 :map direnv-envrc-mode-map
 :after direnv
 :localleader
 :desc "Allow envrc" "a" #'direnv-allow)

(use-package! dap-mode
  :after lsp-mode
  :preface
  (setq dap-breakpoints-file (concat doom-etc-dir "dap-breakpoints")
        dap-utils-extension-path (concat doom-etc-dir "dap-extension/"))
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (require 'dap-python)
  (require 'dapui)
  (add-hook 'dap-stopped-hook
            (lambda (_arg) (call-interactively #'dap-ui-repl)))
  )

(setq
 python-shell-interpreter "python"
 lsp-python-ms-python-executable-cmd "python")
(add-hook #'python-mode-hook #'scoiatael/maybe-activate-virtualenv)

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

(setq lsp-rust-server 'rust-analyzer)
(setq rustic-analyzer-command (concat doom-etc-dir "lsp/rust-analyzer" ))

(setq web-mode-enable-engine-detection 't)

(add-hook! #'gfm-mode #'scoiatael/visualize-trailing-whitespace)

(defun scoiatael/visualize-trailing-whitespace ()
  "Visualize trailingwhitespace in current buffer"
  (interactive)
  (setq whitespace-style '(face trailing))
  (whitespace-turn-on))

(setq! +snippets-dir "~/dotfiles/emacs/snippets")

(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-completion-system 'ivy)

(map!
 :map #'org-roam-mode-map
 :leader
 "r i" #'org-roam-insert-immediate)


(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))

(load-file (expand-file-name "./custom.el" (dir!)))
