;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

(setq doom-localleader-key ",")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Lukasz Czaplinski"
      user-mail-address "lczaplinski@opera.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(map!
 "M-'" #'+eshell/toggle
 "M-j" #'next-buffer
 "M-k" #'previous-buffer)

(defun scoiatael/insert-current-date () (interactive)
    (insert (shell-command-to-string "date +%Y-%m-%d | xargs printf %s")))

(map! :leader
      "SPC" #'helm-M-x
      "d"   #'scoiatael/insert-current-date
      "e"   #'ediff-files
      "c x" #'lsp-treemacs-errors-list
      "f d" #'dired-jump
      "/"   #'scoiatael/helm-rg-project
      "s c" #'evil-ex-nohighlight
      "w 2" #'split-window-below
      "w 3" #'split-window-right
      "a k" #'helm-show-kill-ring)

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

(use-package! evil-surround
  :config
    (global-evil-surround-mode 1))

(after! swiper
  (define-key swiper-map (kbd "ESC") #'keyboard-quit)
  (define-key swiper-map (kbd "C-j") #'ivy-next-line)
  (define-key swiper-map (kbd "C-k") #'ivy-previous-line))

;;; :lang org
(setq org-directory "~/iCloud/org/"
      org-archive-location (concat org-directory "archive/%s::")
      org-ellipsis " ▼ "
      org-clock-persist t
      org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷"))

(add-hook! 'org-load-hook
           #'scoiatael/set-org-todo-keywords)

(after! org
  (add-to-list 'org-modules 'org-habit t))

(map! :after org-mode
      :map org-mode-map
      :desc "dwim" "C-M-x" #'+org/dwim-at-point
      :desc "toggle folds" "TAB" #'org-cycle)

(setq magit-repository-directories '(("~/Documents" . 2))
      magit-save-repository-buffers nil)


(add-hook #'python-mode-hook #'scoiatael/maybe-activate-virtualenv)

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

(setq safe-local-variable-values
      '((flycheck-puppet-lint-executable . "/Users/opera_user/Documents/puppet/.direnv/ruby/bin/puppet-lint")
        (flycheck-puppet-parser-executable . "/Users/opera_user/Documents/puppet/.direnv/ruby/bin/puppet")))

(setq desktop-restore-eager 3)

(add-hook 'prog-mode-hook #'turn-on-visual-line-mode)

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

(setq ns-right-alternate-modifier 'none)
(setq mac-right-option-modifier nil)

(setq python-shell-interpreter "python")
(setq lsp-python-ms-python-executable-cmd "python")

(setq lsp-rust-server 'rust-analyzer)
(setq rustic-analyzer-command (concat doom-etc-dir "lsp/rust-analyzer" ))

(load-file (expand-file-name "./custom.el" (dir!)))
