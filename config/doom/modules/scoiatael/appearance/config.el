;;; modules/scoiatael/appearance/config.el -*- lexical-binding: t; -*-

;; Theme and visual appearance settings
(setq
 doom-theme 'doom-vibrant
 doom-font (font-spec :family "Victor Mono" :size 12)
 doom-symbol-font (font-spec :family "Symbols Nerd Font Mono")
 fancy-splash-image "~/dotfiles/config/doom/cacochan.png"
 +doom-dashboard-functions '(doom-dashboard-widget-banner doom-dashboard-widget-loaded doom-dashboard-widget-shortmenu)
 +doom-dashboard-menu-sections
 '(("Open todo list"
    :icon (nerd-icons-octicon "nf-oct-check" :face 'doom-dashboard-menu-title)
    :action scoiatael/switch-to-agenda-workspace)
   ("Open nixpkgs"
    :icon (nerd-icons-devicon "nf-dev-nixos" :face 'doom-dashboard-menu-title)
    :action scoiatael/switch-to-nixpkgs-workspace )
   ("Open home-manager"
    :icon (nerd-icons-mdicon "nf-md-home_assistant" :face 'doom-dashboard-menu-title)
    :action scoiatael/switch-to-home-manager-workspace )
   ("Open nix-darwin"
    :icon (nerd-icons-devicon "nf-dev-apple" :face 'doom-dashboard-menu-title)
    :action scoiatael/switch-to-nix-darwin-workspace )
   ("Open project"
    :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
    :action projectile-switch-project)
   ("Jump to bookmark"
    :icon (nerd-icons-octicon "nf-oct-bookmark" :face 'doom-dashboard-menu-title)
    :action bookmark-jump)))

(map! :mode #'+doom-dashboard-mode
      :n "q" #'+workspace/kill
      :n "1" #'scoiatael/switch-to-agenda-workspace
      :n "2" #'scoiatael/switch-to-nixpkgs-workspace
      :n "3" #'scoiatael/switch-to-home-manager-workspace
      :n "4" #'scoiatael/switch-to-nix-darwin-workspace
      :n ";" #'projectile-switch-project)

;; Transparency (commented out)
;; (set-frame-parameter nil 'alpha-background 70)
;; (add-to-list 'default-frame-alist '(alpha-background . 70))

(use-package! org-modern
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  :config
  (setq org-modern-todo-faces '(("INBOX" :background "snow" :foreground "black")
                                ("BACKLOG" :background "peach puff" :foreground "black")
                                ("TODO" :background "dark salmon" :foreground "black")
                                ("IN-PROGRESS" :background  "salmon" :foreground "black")
                                ("WAITING" :background "rosy brown" :foreground "black")
                                ("DONE" :background "dark green" :foreground "white")
                                ("CANCELED" :background "dark khaki" :foreground "black")
                                ("META" :background "thistle" :foreground "black")))
  (set-face-attribute 'org-modern-symbol nil :family "Symbols Nerd Font Mono")
  (set-face-attribute 'org-modern-label nil :inherit nil)
  (setq org-modern-priority-faces '((?A :background "IndianRed1" :foreground "white")
                                    (?B :background "IndianRed3" :foreground "white")
                                    (?C :background "IndianRed4" :foreground "white"))))

(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))
