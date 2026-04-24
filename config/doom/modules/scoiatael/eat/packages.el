;;; modules/scoiatael/eat/packages.el -*- lexical-binding: t; -*-

(package! eat :recipe (:type git
                       :host codeberg
                       :repo "akib/emacs-eat"
                       :files ("*.el" ("term" "term/*.el") "*.texi"
                               "*.ti" ("terminfo/e" "terminfo/e/*")
                               ("terminfo/65" "terminfo/65/*")
                               ("integration" "integration/*")
                               (:exclude ".dir-locals.el" "*-tests.el"))))
