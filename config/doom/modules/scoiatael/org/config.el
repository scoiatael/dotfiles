;;; modules/scoiatael/org/config.el -*- lexical-binding: t; -*-

(use-package! ob-async)

(use-package! org-reverse-datetree
  :config
  (map! :map org-mode-map :localleader
        "D" #'org-reverse-datetree-goto-date-in-file
        "a" #'org-archive-subtree-default))

(after! org
  (require 'ob-clojure)
  (require 'ob-async)
  (map! :map org-mode-map :localleader
        "t" #'org-todo)
  (setq
   org-babel-clojure-backend 'babashka
   org-archive-location (concat org-directory "/archive/%s::")
   org-ellipsis " ¼ "
   org-clock-persist t
   org-superstar-headline-bullets-list '("0" "1" "2" "3" "4" "5" "6" "7" "7" "7" "7")
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
      "%(+beancount/clone-transaction)"))))

(after! org-roam
  (setq
   ;; Use human readable dates for dailies titles
   org-roam-dailies-capture-templates
   `(("d" "default" plain ""
      :target (file+head "%<%Y-%m-%d>.org" ,(format "%%[%s/template/journal.org]" org-roam-directory))))))
