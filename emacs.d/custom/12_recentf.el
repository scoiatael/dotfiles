(require 'recentf)
(setq recentf-auto-cleanup 'never
      recentf-exclude '("[/\\]\\.elpa/"
			"[/\\]\\.ido\\.last\\'"
			"[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads\\.el\\'"
			"[/\\]archive-contents\\'"
			"[/\\]\\.loaddefs\\.el\\'"
			"url/cookies")
      recentf-save-file (expand-file-name ".recentf" "~/.emacs.d/")
      ;; save 100 most recent files
      recentf-max-saved-items 100)
