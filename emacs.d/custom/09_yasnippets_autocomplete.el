;;; package --- Summary
;;; Sets up autocomplete with custom yasnipptes

;;; Commentary:
;;; Depends on submodules of this repository
;;; Yasnippet must be required before autocomplete

;;; Code:

(require 'fuzzy)
(require 'yasnippet)
(require 'auto-complete-config)
(require 'go-autocomplete)
(require 'semantic)

(yas-global-mode 1)
(semantic-mode 1)

(setq-default ac-auto-show-menu 0.0)
(setq-default ac-use-fuzzy t)

(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/yasmate/snippets"
        "~/.emacs.d/snippets/yasnippet-snippets"
        "~/.emacs.d/snippets/custom"
        ))

(provide '09_yasnippets_autocomplete)
;;; 09_yasnippets_autocomplete ends here
