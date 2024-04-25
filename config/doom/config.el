;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; (when noninteractive
;;   (add-to-list 'doom-env-whitelist "^SSH_"))

(setq
 doom-localleader-key ","
 display-line-numbers-type 'relative
 user-full-name "Lukasz Czaplinski"
 doom-theme 'doom-one
 doom-font (font-spec :family "VictorMono Nerd Font Mono" :size 12)
 comint-prompt-read-only nil
 git-commit-summary-max-length 120
 gcmh-high-cons-threshold (* 1024 1024 1024)) ; 1GiB

(set-frame-parameter nil 'alpha-background 70)
(add-to-list 'default-frame-alist '(alpha-background . 70))

(after! company
  (setq company-minimum-prefix-length 5
        company-idle-delay 0.5))

(when IS-MAC (setq
              ns-right-alternate-modifier 'none
              mac-right-option-modifier nil))

(add-to-list #'doom-symbol-fallback-font-families "Iosevka")
(add-hook! #'emacs-lisp-mode
  (add-to-list #'flycheck-disabled-checkers 'emacs-lisp-checkdoc))

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
(when EMACS29+
  (use-package! combobulate
    ;; Ensure `combobulate-mode` is activated when you launch a mode it supports
    :hook ((python-mode . combobulate-mode)
           (typescript-mode . combobulate-mode))))

;; https://github.com/doomemacs/doomemacs/issues/7438
(use-package! apheleia)

;; https://gitlab.inria.fr/jwintz/doom.d/-/blob/develop/config.el
(add-to-list              'load-path doom-private-dir)
(add-to-list 'custom-theme-load-path doom-private-dir)
(require '+private|modeline)
(require '+private|spellchecking)
(require '+private|llm)

(after! jinx
  (setq jinx-languages "en cs pl"))

;; - `after!' for running code after a package has loaded
;; https://github.com/hlissner/doom-emacs/issues/3327#issuecomment-710543885
(after! smartparens
  (dolist (char '("f" "r"))
    (sp-local-pair '(python-mode) (concat char "'") "'")
    (sp-local-pair '(python-mode) (concat char "\"") "\""))
  (sp-local-pair '(python-mode) "\"\"\"" "\"\"\""))

(after! format
  (advice-add 'format-all-buffer :around #'envrc-propagate-environment)
  (advice-add 'format-all-buffer--from-hook :around #'envrc-propagate-environment)
  (advice-add '+format-buffer-h :around #'envrc-propagate-environment))

(after! org
  (setq
   org-archive-location (concat org-directory "archive/%s::")
   org-ellipsis " ▼ "
   org-clock-persist t
   org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")))

(after! web-mode
  (add-to-list 'web-mode-engines-alist '("jinja2" . "\\.jinja2?\\'"))
  (setq-default web-mode-enable-engine-detection 't))

(after! vterm
  (advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args))))
  (define-key vterm-mode-map (kbd "M-'") #'+vterm/toggle))

(after! projectile
  (add-to-list #'projectile-project-root-files ".envrc")
  (add-to-list #'projectile-project-root-files-bottom-up ".envrc")
  (add-to-list #'projectile-project-root-files-top-down-recurring ".envrc"))

(after! eglot
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t)))

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

;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
(map! "C-`" #'+vterm/toggle)

(map! :n "]p" #'evil-unimpaired-paste-below
      :n "[p" #'evil-unimpaired-paste-above
      :n ";" #'projectile-find-file
      :n "\\" #'+vterm/toggle
      :v "[6" #'base64-decode-region
      :v "]6" #'base64-encode-region
      :v "v" #'er/expand-region)

(map! :leader
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
      "r R" (cmd! (find-file "README.md")))

(map! :leader
      "w <left>" #'evil-window-left
      "w <right>" #'evil-window-right
      "w <up>" #'evil-window-up
      "w <down>" #'evil-window-down)

(map! :leader
      :prefix "k"
      "c" #'chmod
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
