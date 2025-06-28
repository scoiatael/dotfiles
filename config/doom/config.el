;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq
 doom-localleader-key ","
 display-line-numbers-type 'relative
 user-full-name "Lukasz Czaplinski"
 ;; https://github.com/doomemacs/themes/tree/screenshots
 doom-theme 'doom-vibrant
 doom-font (font-spec :family "Victor Mono" :size 12)
 doom-symbol-font (font-spec :family "Symbols Nerd Font Mono")
 comint-prompt-read-only nil
 git-commit-summary-max-length 120
 gcmh-high-cons-threshold (* 1024 1024 1024) ; 1GiB
 +doom-dashboard-functions '(doom-dashboard-widget-banner doom-dashboard-widget-loaded doom-dashboard-widget-shortmenu)
 fancy-splash-image "~/dotfiles/config/doom/cacochan.png"
 +doom-dashboard-menu-sections
 '(("Open todo list"
    :icon (nerd-icons-octicon "nf-oct-check" :face 'doom-dashboard-menu-title)
    :action scoiatael/switch-to-agenda-workspace)
   ("Open project"
    :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
    :action projectile-switch-project)
   ("Jump to bookmark"
    :icon (nerd-icons-octicon "nf-oct-bookmark" :face 'doom-dashboard-menu-title)
    :action bookmark-jump)))

(setf (alist-get 'width +org-capture-frame-parameters) 180)
(setf (alist-get 'height +org-capture-frame-parameters) 20)

;; (set-frame-parameter nil 'alpha-background 70)
;; (add-to-list 'default-frame-alist '(alpha-background . 70))

(after! company
  (setq company-minimum-prefix-length 5
        company-idle-delay 0.5))

(when (featurep :system 'macos) (setq mac-right-option-modifier nil))

(add-hook! #'emacs-lisp-mode
  (when (doom-real-buffer-p (current-buffer))
    (when (seq-find (lambda (dir) (file-in-directory-p (buffer-file-name) dir))
                    '("~/dotfiles" "~/.config" "~/.doom.d" "~/.emacs.d/lisp" "~/.emacs.d/modules"))
      (setq flymake-diagnostic-functions '(my-elisp-config-flymake-byte-compile)))
    (flymake-mode))

  (cl-callf append elisp-flymake-byte-compile-load-path load-path))

(add-hook! #'haml-mode
  (setq-local tab-width 2))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
(when (version<= "29" emacs-version)
  (use-package treesit
    :preface
    (defun mp-setup-install-grammars ()
      "Install Tree-sitter grammars if they are absent."
      (interactive)
      (dolist (grammar
               ;; Note the version numbers. These are the versions that
               ;; are known to work with Combobulate *and* Emacs.
               '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                 (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
                 (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                 (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                 (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
                 (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                 (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
                 (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
                 (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                 (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                 (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
        (add-to-list 'treesit-language-source-alist grammar)
        ;; Only install `grammar' if we don't already have it
        ;; installed. However, if you want to *update* a grammar then
        ;; this obviously prevents that from happening.
        (unless (treesit-language-available-p (car grammar))
          (treesit-install-language-grammar (car grammar)))))

    ;; Optional. Combobulate works in both xxxx-ts-modes and
    ;; non-ts-modes.

    ;; You can remap major modes with `major-mode-remap-alist'. Note
    ;; that this does *not* extend to hooks! Make sure you migrate them
    ;; also
    ;; (dolist (mapping
    ;;          '((python-mode . python-ts-mode)
    ;;            (css-mode . css-ts-mode)
    ;;            (typescript-mode . typescript-ts-mode)
    ;;            (js2-mode . js-ts-mode)
    ;;            (bash-mode . bash-ts-mode)
    ;;            (conf-toml-mode . toml-ts-mode)
    ;;            (go-mode . go-ts-mode)
    ;;            (css-mode . css-ts-mode)
    ;;            (json-mode . json-ts-mode)
    ;;            (js-json-mode . json-ts-mode)))
    ;;   (add-to-list 'major-mode-remap-alist mapping))
    :config
    (mp-setup-install-grammars)
    ;; Do not forget to customize Combobulate to your liking:
    ;;
    ;;  M-x customize-group RET combobulate RET
    ;;
    (use-package combobulate
      :custom
      ;; You can customize Combobulate's key prefix here.
      ;; Note that you may have to restart Emacs for this to take effect!
      (combobulate-key-prefix "C-s")
      :hook ((prog-mode . combobulate-mode))
      ;; Amend this to the directory where you keep Combobulate's source
      ;; code.
      ))
  )

(use-package! eldoc-box
  :config
  (map! :leader "h h" #'eldoc-box-help-at-point)
  (after! eglot
    (define-key eglot-mode-map (kbd "C-c I") #'eldoc-box-eglot-help-at-point)))

(use-package! ob-graphql)
(use-package! ob-async)

(use-package! org-modern :config
              :init
              (add-hook 'org-mode-hook #'org-modern-mode)
              (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
              :config
              (set-face-attribute 'org-modern-symbol nil :family "Symbols Nerd Font Mono")
              (setq org-modern-todo-faces '(("INBOX" :background "snow" :foreground "black")
				            ("BACKLOG" :background "peach puff" :foreground "black")
				            ("TODO" :background "dark salmon" :foreground "black")
				            ("IN-PROGRESS" :background  "salmon" :foreground "black")
				            ("WAITING" :background "rosy brown" :foreground "black")
				            ("DONE" :background "dark green" :foreground "white")
				            ("CANCELED" :background "dark khaki" :foreground "black")
				            ("META" :background "thistle" :foreground "black")))
              (set-face-attribute 'org-modern-label nil :inherit nil)
              (setq org-modern-priority-faces '((?A :background "IndianRed1" :foreground "white")
				                (?B :background "IndianRed3" :foreground "white")
				                (?C :background "IndianRed4" :foreground "white"))))

;; https://github.com/psibi/justl.el?tab=readme-ov-file#installation
(use-package! justl
  :config
  (setq justl-shell 'vterm)
  (advice-add 'justl :around #'envrc-propagate-environment)
  (map! :leader ";" #'justl)
  (map! :map #'justl-mode-map
        :n "x" 'justl-exec-recipe
        :n "X" 'justl-exec-shell))

;; https://github.com/doomemacs/doomemacs/issues/7438
(use-package! apheleia
  :config
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist)
        '(rufo))
  (setf (alist-get 'ruby-mode apheleia-mode-alist)
        '(rufo)))

(use-package! river-mode
  :config
  (add-hook! #'river-mode #'display-line-numbers-mode)
  (add-to-list 'auto-mode-alist '("\\.alloy\\'" . river-mode)))

(use-package! gptel
  :config
  ;; (setq
  ;;  gptel-model 'mistral:latest
  ;;  gptel-backend (gptel-make-ollama "Ollama"
  ;;                :host "localhost:11434"
  ;;                :stream t
  ;;                :models '(mistral:latest)))

  (gptel-make-openai "TogetherAI"         ;Any name you want
    :host "api.together.xyz"
    :key (lambda () (password-store-get "together-ai-api-token"))                   ;can be a function that returns the key
    :stream t
    :models '(;; has many more, check together.ai
              deepseek-ai/DeepSeek-R1
              deepseek-ai/DeepSeek-V3
              mistralai/Mixtral-8x7B-Instruct-v0.1
              codellama/CodeLlama-13b-Instruct-hf
              codellama/CodeLlama-34b-Instruct-hf))
  (gptel-make-kagi "Kagi"
    :key (lambda () (password-store-get "kagi-api-token")))

  (setq
   gptel-model 'claude-3-7-sonnet-20250219
   gptel-directives '((assistant . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
                      (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
                      (default . "You are a large language model and a careful programmer living inside Emacs editor. Provide code and only code as output without any additional text, prompt or note. Look at last line for additional requests from user.")
                      (writing . "You are a large language model and a writing assistant. Respond concisely.")
                      (chat . "You are a large language model and a conversation partner. Respond concisely."))
   gptel-backend (gptel-make-anthropic "Claude"          ;Any name you want
                   :stream t                             ;Streaming responses
                   :key (lambda () (password-store-get "anthropic-com-api-token"))))

  (map! :localleader
        "g g" #'gptel
        "g r" #'gptel-rewrite
        "g s" #'gptel-send
        "g m" #'gptel-menu))

(use-package! prisma-mode)

;; (use-package kagi
;;   :ensure t
;;   :custom
;;   ;; or use a function, e.g. with the password-store package:
;;   (kagi-api-token (lambda () (password-store-get "kagi-api-token")))

;;   ;; Universal Summarizer settings
;;   (kagi-summarizer-engine "cecil")
;;   (kagi-summarizer-default-language "EN")
;;   (kagi-summarizer-cache t)
;;   :config
;;   (define-kagi-fastgpt-prompt kagi-write
;;                               "Write code that solves following problem: %s"
;;                               "Code"))

;; (use-package jtsx
;;   :ensure t
;;   :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
;;          ("\\.tsx\\'" . jtsx-tsx-mode)
;;          ("\\.ts\\'" . jtsx-typescript-mode))
;;   :commands jtsx-install-treesit-language
;;   :hook ((jtsx-jsx-mode . hs-minor-mode)
;;          (jtsx-tsx-mode . hs-minor-mode)
;;          (jtsx-typescript-mode . hs-minor-mode)
;;          (jtsx-jsx-mode . eglot-ensure)
;;          (jtsx-tsx-mode . eglot-ensure)
;;          (jtsx-typescript-mode . eglot-ensure) )
;;   ;; :custom
;;   ;; Optional customizations
;;   ;; (js-indent-level 2)
;;   ;; (typescript-ts-mode-indent-offset 2)
;;   ;; (jtsx-switch-indent-offset 0)
;;   ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
;;   ;; (jtsx-jsx-element-move-allow-step-out t)
;;   ;; (jtsx-enable-jsx-electric-closing-element t)
;;   ;; (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
;;   ;; (jtsx-enable-jsx-element-tags-auto-sync nil)
;;   ;; (jtsx-enable-all-syntax-highlighting-features t)
;;   :config
;;   (defun jtsx-bind-keys-to-mode-map (mode-map)
;;     "Bind keys to MODE-MAP."
;;     (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
;;     (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
;;     (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
;;     (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
;;     (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
;;     (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
;;     (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
;;     (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
;;     (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
;;     (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
;;     (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
;;     (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
;;     (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node)
;;     (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
;;     (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
;;     (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))

;;   (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
;;     (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

;;   (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
;;     (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

;;   (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
;;   (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package reason-mode
  :init
  ;; (defun shell-cmd (cmd)
  ;;   "Returns the stdout output of a shell command or nil if the command returned
  ;;  an error"
  ;;   (car (ignore-errors (apply 'process-lines (split-string cmd)))))

  ;; (defun reason-cmd-where (cmd)
  ;;   (let ((where (shell-cmd cmd)))
  ;;     (if (not (string-equal "unknown flag ----where" where))
  ;;         where)))

  ;; (let* ((refmt-bin (or (reason-cmd-where "refmt ----where")
  ;;                       (shell-cmd "which refmt")
  ;;                       (shell-cmd "which bsrefmt")))
  ;;        (merlin-bin (or (reason-cmd-where "ocamlmerlin ----where")
  ;;                        (shell-cmd "which ocamlmerlin")))
  ;;        (merlin-base-dir (when merlin-bin
  ;;                           (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
  ;;   ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  ;;   (when merlin-bin
  ;;     (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
  ;;     (setq merlin-command merlin-bin))

  ;;   (when refmt-bin
  ;;     (setq refmt-command refmt-bin)))

  (require 'reason-mode)
  (require 'merlin)
  (add-hook 'reason-mode-hook (lambda ()
                                (add-hook 'before-save-hook 'refmt-before-save)
                                (merlin-mode)))

  :config
  (cl-remove 'reason-mode auto-mode-alist :test 'equal :key 'cdr)
  (add-to-list 'auto-mode-alist '("\\.rei?\\'" . reason-mode)))

(use-package rescript-mode
  :init
  (require 'compile)
  :config
  (setf (alist-get 'rescript apheleia-formatters)
        '("rescript" "format" "-stdin" ".res" ))
  (setf (alist-get 'rescript-mode apheleia-mode-alist)
        '(rescript))
  (after! eglot
    (add-to-list 'eglot-server-programs
                 '(rescript-mode . ("rescript-language-server" "--stdio"))))
  :hook ((rescript-mode . (lambda () (electric-indent-local-mode -1)))))

(use-package! org-reverse-datetree
  :config
  (map! :map org-mode-map :localleader
        "d" #'org-reverse-datetree-goto-date-in-file
        "a" #'org-reverse-datetree-archive-subtree))

(require-relative-list '(+private|modeline
                         +private|spellchecking))
;; (require '+private|llm)
;; (require '+private|spacehammer)
;; (require '+private|gleam)

;; - `after!' for running code after a package has loaded
;; https://github.com/hlissner/doom-emacs/issues/3327#issuecomment-710543885
(after! markdown-mode
  (add-hook! #'markdown-mode-hook #'scoiatael/visualize-trailing-whitespace))

(after! smartparens
  (dolist (char '("f" "r"))
    (sp-local-pair '(python-mode) (concat char "'") "'")
    (sp-local-pair '(python-mode) (concat char "\"") "\""))
  (sp-local-pair '(python-mode) "\"\"\"" "\"\"\""))

(after! notmuch
  ;; https://github.com/doomemacs/doomemacs/issues/4585
  (set-popup-rule! "^\\*notmuch-hello" :ignore t)
  ;; https://protesilaos.com/emacs/dotemacs#h:2c9a555f-d3ec-4d0e-bad6-6c26b576a49a
  (setq notmuch-show-logo t
        notmuch-column-control 1.0
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-thousands-separator ""
        notmuch-hello-sections '(notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)
        notmuch-show-all-tags-list t))

(after! format
  (advice-add 'format-all-buffer :around #'envrc-propagate-environment)
  (advice-add 'format-all-buffer--from-hook :around #'envrc-propagate-environment)
  (advice-add '+format-buffer-h :around #'envrc-propagate-environment))

(after! org
  (require 'ob-clojure)
  (require 'ob-async)
  (setq
   org-babel-clojure-backend 'babashka
   org-archive-location (concat org-directory "/archive/%s::")
   org-ellipsis " ▼ "
   org-clock-persist t
   org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")
   org-capture-templates
   '(("t" "todo" entry (file+function "todo.org" org-reverse-datetree-goto-date-in-file)
      "* [ ] %?\n%i\n%a"
      :prepend t)
     ("d" "deadline" entry (file+function "todo.org" org-reverse-datetree-goto-date-in-file)
      "* [ ] %?\nDEADLINE: <%(org-read-date)>\n\n%i\n%a"
      :prepend t)
     ("s" "schedule" entry (file+function "todo.org" org-reverse-datetree-goto-date-in-file)
      "* [ ] %?\nSCHEDULED: <%(org-read-date)>\n\n%i\n%a"
      :prepend t)
     ("c" "check out later" entry (file+headline "todo.org" "Check out later")
      "* [ ] %?\n%i\n%a"
      :prepend t)
     ("l" "ledger" plain (file "ledger/personal.gpg")
      "%(+beancount/clone-transaction)")) ))

(after! org-roam
  (setq
   ;; Use human readable dates for dailies titles
   org-roam-dailies-capture-templates
   `(("d" "default" plain ""
      :target (file+head "%<%Y-%m-%d>.org" ,(format "%%[%s/template/journal.org]" org-roam-directory))))))

(after! web-mode
  (add-to-list 'web-mode-engines-alist '("jinja2" . "\\.jinja2?\\'"))
  (setq-default web-mode-enable-engine-detection 't))

(after! vterm
  (advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args)))))

(after! projectile
  (setq projectile-require-project-root t
        projectile-enable-idle-timer 90)
  (add-hook 'projectile-idle-timer-hook #'projectile-invalidate-cache))

(after! eglot
  (add-to-list 'eglot-server-programs `((js-mode typescript-mode typescript-ts-mode typescript-tsx-mode rjsx-mode jtsx-typescript-mode) . ,(eglot-alternatives '(("deno" "lsp") ("vtsls" "--stdio")))))
  (setq eglot-events-buffer-config '(:size 2000000 :format full))
  ;; (setq eglot-strict-mode '(disallow-non-standard-keys enforce-required-keys))

  (setenv "NIXD_FLAGS"  "--inlay-hints=false")
  (setq lsp-nix-nixd-server-path "nixd"
        lsp-nix-nixd-formatting-command [ "nixfmt" ]
        lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
        lsp-nix-nixd-nixos-options-expr (concat "(builtins.getFlake \"" (getenv "HOME") "/dotfiles\").nixosConfigurations.LsFramework.options")
        lsp-nix-nixd-home-manager-options-expr (concat "(builtins.getFlake \"" (getenv "HOME") "/dotfiles\").homeConfigurations.\"lukaszczaplinski@LsFramework\".options"))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t)))

(after! minimap
  (add-hook 'org-capture-mode-hook (lambda () (minimap-mode 0))))

(after! magit
  (add-hook 'find-file-hook #'jujutsu-commit-setup-check-buffer)
  (defconst jujutsu-commit-filename-regexp "\\.jjdescription\\'")

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude jujutsu-commit-filename-regexp))

  (add-to-list 'with-editor-file-name-history-exclude jujutsu-commit-filename-regexp)

  (defun jujutsu-commit-setup-check-buffer ()
    (when (and buffer-file-name
               (string-match-p jujutsu-commit-filename-regexp buffer-file-name))
      (git-commit-setup))))

(after! dired
  (define-key dired-mode-map (kbd "\\") #'ranger-toggle-dotfiles))

(after! apheleia
  (setf (alist-get 'prisma apheleia-formatters)
        '("~/dotfiles/bin/prisma-formatter" filepath))
  (setf (alist-get 'prisma-mode apheleia-mode-alist)
        '(prisma))
  (setf (alist-get 'tofu apheleia-formatters)
        '("tofu" "fmt" "-"))
  (setf (alist-get 'terraform-mode apheleia-mode-alist)
        'tofu))

;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
(map! :i ", SPC" (cmd! (insert ", ")))
(map! :i ", RET" (cmd! (insert ",\n")))
(map! :i ", f" (cmd! (call-interactively #'evil-snipe-f)))
(map! :i ", F" (cmd! (call-interactively #'evil-snipe-F)))
(map! :i ", ;" (cmd! (save-excursion (up-list) (insert ";") )))
(map! :i ", ESC" (cmd! (insert ",") (evil-escape)))

;; https://emacs.stackexchange.com/a/221
(define-key input-decode-map [?\C-i] [C-i])
(map! :i "<C-i>" #'up-list)

(map! :n "]p" #'evil-unimpaired-paste-below
      :n "[p" #'evil-unimpaired-paste-above
      :n ";" #'projectile-find-file
      :n "\\" #'+vterm/toggle
      :v "[6" #'base64-decode-region
      :v "]6" #'base64-encode-region
      :v "v" #'er/expand-region)

;; (map! :n "M-<up>" (cmd!  (transpose-lines 1)(forward-line -2))
;;       :n "M-<down>" (cmd! (forward-line 1) (transpose-lines 1) (forward-line -1)) )

(map! :n "M-<up>" #'evil-window-up
      :n "M-<right>"  #'evil-window-right
      :n "M-<left>"  #'evil-window-left
      :n "M-<down>"  #'evil-window-down)

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
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(let ((custom-config-file (expand-file-name "./custom.el" (dir!))))
  (when (file-exists-p custom-config-file)
    (load-file custom-config-file)))

;; Used by customization system
(setq custom-file "~/dotfiles/config/doom/emacs-custom.el")
(load custom-file)
