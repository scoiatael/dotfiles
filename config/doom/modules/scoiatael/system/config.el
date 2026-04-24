;;; modules/scoiatael/system/config.el -*- lexical-binding: t; -*-

(setq
 user-full-name "Lukasz Czaplinski"
 git-commit-summary-max-length 120
 gcmh-high-cons-threshold (* 1024 1024 1024)) ; 1GiB

;; Org capture frame parameters
(setf (alist-get 'width +org-capture-frame-parameters) 180)
(setf (alist-get 'height +org-capture-frame-parameters) 20)

;; macOS specific settings
(when (featurep :system 'macos) (setq mac-right-option-modifier nil))

;; NOTE: I'd really like it but it clashes with :ui workspaces
;; https://github.com/doomemacs/doomemacs/issues/6205
;; (setq uniquify-buffer-name-style 'forward)

(use-package! breadcrumb
  :config
  (breadcrumb-mode)
  (setq breadcrumb-project-crumb-separator " > ")
  (map! :leader ";" #'breadcrumb-jump)
  (advice-add #'breadcrumb--format-project-node :around
              (lambda (og p more &rest r)
                "Icon For File"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-icon-for-file string)
                              " " string)
                    (concat (nerd-icons-faicon
                             "nf-fa-folder_open"
                             :face 'breadcrumb-project-crumbs-face)
                            " "
                            string)))))

  (advice-add #'breadcrumb--project-crumbs-1 :filter-return
              (lambda (return)
                "Icon for Parent Node"
                (if (listp return)
                    (setf (car return)
                          (concat
                           " "
                           (nerd-icons-faicon
                            "nf-fa-rocket"
                            :face 'breadcrumb-project-base-face)
                           " "
                           (car return))))
                return))

  (advice-add #'breadcrumb--format-ipath-node :around
              (lambda (og p more &rest r)
                "Icon for items"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-codicon
                               "nf-cod-symbol_field"
                               :face 'breadcrumb-imenu-leaf-face)
                              " " string)
                    (cond ((string= string "Packages")
                           (concat (nerd-icons-codicon "nf-cod-package" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Requires")
                           (concat (nerd-icons-codicon "nf-cod-file_submodule" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((or (string= string "Variable") (string= string "Variables"))
                           (concat (nerd-icons-codicon "nf-cod-symbol_variable" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Function")
                           (concat (nerd-icons-mdicon "nf-md-function_variant" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          (t string)))))))


;; Used by customization system
(setq custom-file "~/dotfiles/config/doom/emacs-custom.el")
(load custom-file)
