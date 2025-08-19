;;; scoiatael/narrow/config.el -*- lexical-binding: t; -*-

;; Enable narrow-to-page and narrow-to-defun
(map! :leader
      :prefix "n"
      :desc "Narrow to defun"    "d" #'narrow-to-defun
      :desc "Widen"              "w" #'scoiatael/widen)

(map! :localleader
      :desc "Narrow to region"    "r" #'narrow-to-region
      :desc "Narrow dwim"    "n" #'scoiatael/narrow-or-widen-dwim)

(after! org
  (map! :leader
        :map org-mode-map
        :prefix "n"
        :desc "Narrow to subtree"   "s" #'org-narrow-to-subtree
        :desc "Narrow to block"     "b" #'org-narrow-to-block
        :desc "Narrow to element"   "e" #'org-narrow-to-element))

;; Define narrow-mode keymap
(defvar narrow-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for narrow-mode.")

;; Enable narrow-to-page and narrow-to-defun
(map! :localleader
      :mode :normal
      :map narrow-mode-map
      :desc "Widen"              "q" #'scoiaael/widen)

;; Define narrow-mode minor mode
(define-minor-mode narrow-mode
  "Minor mode active when buffer is narrowed."
  :lighter " Narrow"
  :keymap narrow-mode-map
  :global nil)

;; Advice to enable narrow-mode after narrowing
(defun narrow-mode--activate-advice (&rest _)
  "Enable narrow-mode after narrowing."
  (narrow-mode 1))

(defun narrow-mode--deactivate-advice (&rest _)
  "Disable narrow-mode after widening."
  (narrow-mode -1))

;; Add advice to narrowing and widening functions
(advice-add 'narrow-to-defun :after #'narrow-mode--activate-advice)
(advice-add 'narrow-to-region :after #'narrow-mode--activate-advice)
(advice-add 'org-narrow-to-subtree :after #'narrow-mode--activate-advice)
(advice-add 'org-narrow-to-block :after #'narrow-mode--activate-advice)
(advice-add 'org-narrow-to-element :after #'narrow-mode--activate-advice)
(advice-add 'widen :after #'narrow-mode--deactivate-advice)
