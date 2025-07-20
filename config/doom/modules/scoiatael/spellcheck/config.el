;;; modules/scoiatael/spellcheck/config.el -*- lexical-binding: t; -*-

(use-package! jinx
  :hook ((emacs-startup . global-jinx-mode))
  :config
  (setq jinx-languages "en_GB pl_PL"
        jinx-delay 1.0))

(map!
 :n  "z="   #'jinx-correct
 :m  "[s"   #'jinx-next
 :m  "]s"   #'jinx-previous)

(after! vertico-multiform ;; if using vertico
  (add-to-list 'vertico-multiform-categories
               '(jinx (vertico-grid-annotate . 25)))
  (vertico-multiform-mode 1))