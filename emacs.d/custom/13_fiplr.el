(require 'fiplr)
(setq fiplr-ignored-globs '((directories (".git" ".svn" ".cask" "third_party"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(define-key evil-normal-state-map (kbd ";") 'fiplr-find-file)
