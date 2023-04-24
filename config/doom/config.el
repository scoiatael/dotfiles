;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(when noninteractive
  (add-to-list 'doom-env-whitelist "^SSH_"))

(setq
 doom-localleader-key ","
 user-full-name "Lukasz Czaplinski"
 doom-font "JetBrainsMono Nerd Font-9"
 doom-theme 'doom-one
 display-line-numbers-type t
 comint-prompt-read-only nil
 gcmh-high-cons-threshold (* 1024 1024 1024)) ; 1GiB

(when IS-MAC (setq
              ns-right-alternate-modifier 'none
              mac-right-option-modifier nil))

(add-to-list 'doom-symbol-fallback-font-families "Iosevka")


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
           ;; (js-mode . combobulate-mode)))
           (typescript-mode . combobulate-mode))))

(use-package! eldoc-box
  :hook ((eglot-managed-mode-hook . eldoc-box-hover-mode)))

;; https://gitlab.inria.fr/jwintz/doom.d/-/blob/develop/config.el
(add-to-list              'load-path doom-private-dir)
(add-to-list 'custom-theme-load-path doom-private-dir)
(require '+private|modeline)


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

;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
(map! "M-'" #'+vterm/toggle)

(map! :n "]p" #'evil-unimpaired-paste-below
      :n "[p" #'evil-unimpaired-paste-above)

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
      "r R" (cmd! (find-file "README.md")))

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
