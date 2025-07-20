;;; modules/scoiatael/modeline/config.el -*- lexical-binding: t; -*-

(use-package! keycast
  :commands keycast-mode)

(use-package! doom-modeline
  :config (when (display-graphic-p)
            (require 'cl-lib)
            (require 'moody)
            (require 'devdocs)

            (setq doom-modeline-bar-width 5)
            (setq x-underline-at-descent-line t)

            (require 'spaceline-config)

            (setq-default
             powerline-height 24
             powerline-default-separator 'butt
             spaceline-flycheck-bullet "V %s"
             spaceline-separator-dir-left '(right . right)
             spaceline-separator-dir-right '(left . left))

            (spaceline-spacemacs-theme)
            (spaceline-toggle-persp-name-off)
            (spaceline-toggle-window-number-off)
            (spaceline-toggle-workspace-number-off)
            (spaceline-toggle-minor-modes-off)

;;;; Segments, me own

            (defvar-local +private|modeline--vcs
              '(:eval
                (let ((active (doom-modeline--active)))
                  (let ((icon doom-modeline--vcs-icon)
                        (text doom-modeline--vcs-text))
                    (moody-ribbon
                     (concat
                      doom-modeline-spc
                      (if icon
                          (propertize
                           (concat
                            (if active
                                icon
                              (doom-modeline-propertize-icon icon 'mode-line-inactive))
                            doom-modeline-vspc)
                           'mouse-face 'mode-line-highlight
                           'help-echo (get-text-property 1 'help-echo vc-mode)
                           'local-map (get-text-property 1 'local-map vc-mode)))
                      (if (not text)
                          (if active
                              "Unversioned"
                            (propertize "Unversioned" 'face 'mode-line-inactive))
                        (if active
                            text
                          (propertize text 'face 'mode-line-inactive)))

                      doom-modeline-spc
                      (if active
                          "  "))
                     nil 'down)))))

            (defvar-local +private|modeline--keycast-key
              '(:eval
                (when-let ((casting keycast-mode)
                           (active (doom-modeline--active))
                           (key (ignore-errors
                                  (key-description keycast--this-command-keys))))
                  (moody-tab
                   (concat
                    doom-modeline-spc
                    (propertize key 'face 'doom-modeline-evil-visual-state)
                    doom-modeline-spc)
                   nil 'up))))

            (defvar-local +private|modeline--keycast-cmd
              '(:eval
                (when-let ((casting keycast-mode)
                           (active (doom-modeline--active))
                           (cmd keycast--this-command)
                           (rep keycast--command-repetitions))
                  (propertize
                   (concat
                    doom-modeline-spc
                    (format "%s" cmd)
                    (if (> rep 0)
                        (format " x%s" (1+ rep))
                      "")
                    doom-modeline-spc)
                   'face 'doom-modeline-evil-visual-state))))

            (defvar-local +private|modeline--indic
              '(:eval
                (when (doom-modeline--active)
                  (concat
                   doom-modeline-spc
                   (if doom-modeline--objed-active
                       (doom-modeline-icon 'faicon "circle" "" ""
                                           :face '+private|faces-objed-mode :height 0.7 :v-adjust 0.0)
                     (doom-modeline-icon 'faicon "circle" "" ""
                                         :face '+private|faces-emacs-mode :height 0.7 :v-adjust 0.0))
                   doom-modeline-spc))))

            (defvar-local +private|modeline--objed
              '(:eval
                (when-let ((active (doom-modeline--active)))
                  (moody-tab
                   (concat
                    doom-modeline-spc
                    (if doom-modeline--objed-active
                        (propertize (format "%s(%s) "
                                            (symbol-name objed--object)
                                            (char-to-string (aref (symbol-name objed--obj-state) 0)))
                                    'face '+private|faces-objed-mode
                                    'help-echo (format "Objed object: %s (%s)"
                                                       (symbol-name objed--object)
                                                       (symbol-name objed--obj-state)))
                      (propertize (format "%s " "Normal")
                                  'face '+private|faces-emacs-mode))
                    doom-modeline-spc)
                   nil 'up))))

            (defvar-local +private|modeline--doc-path
              '(:eval
                (let-alist (car devdocs--stack)
                  (moody-tab
                   (concat
                    doom-modeline-spc
                    (propertize
                     (concat (devdocs--doc-title .doc)
                             (and .type devdocs-separator) .type
                             (and .name devdocs-separator) .name)
                     'face 'mode-line-buffer-id)
                    doom-modeline-spc
                    )
                   nil 'down))))

            (defvar-local +private|modeline--doc-mode
              '(:eval
                (moody-ribbon
                 (concat
                  doom-modeline-spc
                  (doom-modeline-icon 'faicon "book" "" "" :height 1.0 :v-adjust 0.0)
                  doom-modeline-spc
                  mode-name
                  doom-modeline-spc)
                 nil 'down)))

            (defvar-local +private|modeline--modal-guide
              '(:eval
                (moody-ribbon
                 (concat
                  doom-modeline-spc
                  (doom-modeline-icon 'faicon "book" "" "" :height 1.0 :v-adjust 0.0)
                  doom-modeline-spc
                  "Modal editing hints"
                  doom-modeline-spc)
                 nil 'down)))

            (put '+private|modeline--buffer      'risky-local-variable t)
            (put '+private|modeline--vcs         'risky-local-variable t)
            (put '+private|modeline--keycast-key 'risky-local-variable t)
            (put '+private|modeline--keycast-cmd 'risky-local-variable t)
            (put '+private|modeline--indic       'risky-local-variable t)
            (put '+private|modeline--objed       'risky-local-variable t)
            (put '+private|modeline--doc-path    'risky-local-variable t)
            (put '+private|modeline--doc-mode    'risky-local-variable t)
            (put '+private|modeline--modal-guide 'risky-local-variable t)

            (doom-modeline-def-segment +private|modeline-buffer       +private|modeline--buffer)
            (doom-modeline-def-segment +private|modeline-vcs          +private|modeline--vcs)
            (doom-modeline-def-segment +private|modeline-keycast-key  +private|modeline--keycast-key)
            (doom-modeline-def-segment +private|modeline-keycast-cmd  +private|modeline--keycast-cmd)
            (doom-modeline-def-segment +private|modeline-indic        +private|modeline--indic)
            (doom-modeline-def-segment +private|modeline-objed        +private|modeline--objed)
            (doom-modeline-def-segment +private|modeline-doc-path     +private|modeline--doc-path)
            (doom-modeline-def-segment +private|modeline-doc-mode     +private|modeline--doc-mode)
            (doom-modeline-def-segment +private|modeline-modal-guide  +private|modeline--modal-guide)

            (doom-modeline-def-modeline 'main
                                        '(hud " " matches
                                          +private|modeline-buffer buffer-position selection-info
                                          " " +private|modeline-keycast-key +private|modeline-keycast-cmd)
                                        '(misc-info
                                          +private|modeline-indic
                                          +private|modeline-objed
                                          major-mode process
                                          +private|modeline-vcs))

            (doom-modeline-def-modeline 'documentation
                                        '(hud " " matches
                                          +private|modeline-doc-path)
                                        '(misc-info
                                          +private|modeline-doc-mode))

            (doom-modeline-def-modeline 'guide
                                        '(hud " ")
                                        '(misc-info +private|modeline-modal-guide))

            (defun +private|modeline/set-guide-modeline ()
              ""
              (doom-modeline-set-modeline 'guide)
              (setq-local header-line-format mode-line-format))

            (defun +private/adjust-modeline ()
              (when (string-match "private" (format! "%s" doom-theme))
                (set-face-attribute 'header-line nil :family "Operator Mono" :weight 'normal)
                (set-face-attribute 'mode-line nil :family "Operator Mono" :weight 'bold :height 0.9)
                (set-face-attribute 'mode-line-inactive nil :family "Operator Mono" :weight 'bold :height 0.9)))

            (add-hook! 'doom-load-theme-hook
              (+private/adjust-modeline))

            (+private/adjust-modeline))

  (add-to-list '+doom-dashboard-functions
               (lambda () (setq header-line-format nil)))
  (add-to-list '+doom-dashboard-functions
               (lambda () (setq mode-line-format nil))))