;;; ~/dotfiles/emacs/doom.d/autoload/text.el -*- lexical-binding: t; -*-

(defun scoiatael/yank (s)
  (message (kill-new s)))

;;;###autoload
(defun scoiatael/file-relative-name ()
  (file-relative-name buffer-file-name (projectile-project-root)))

;;;###autoload
(defun scoiatael/yank-file-location ()
  (interactive)
  (scoiatael/yank (format "%s:%d" (scoiatael/file-relative-name) (line-number-at-pos))))

;;;###autoload
(defun scoiatael/blog/cover-img ()
  (interactive)
  (scoiatael/yank (format "![](/img/%s.jpg)" (file-name-sans-extension (file-relative-name buffer-file-name)))))

;;;###autoload
(defun scoiatael/yank-current-date ()
  (interactive)
  (scoiatael/yank (shell-command-to-string "date +%Y-%m-%d | xargs printf %s")))

;;;###autoload
(defun scoiatael/json-to-yaml (start end)
  (interactive (list (region-beginning) (region-end)))
  (shell-command-on-region start end "ruby -e 'require \"yaml\"; require \"json\"; puts YAML.dump(JSON.parse($stdin.read))'" :replace t))

;;;###autoload
(defun to-snake-case (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((camel-case-str (buffer-substring start end)))
        (delete-region start end)
        (insert (s-snake-case camel-case-str)))
    (message "No region selected")))

;;;###autoload
(defun to-camel-case (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((str (buffer-substring start end)))
        (delete-region start end)
        (insert (s-upper-camel-case str)))
    (message "No region selected")))

;; https://github.com/zmaas/evil-unimpaired/blob/master/evil-unimpaired.el#L67-L75
;;;###autoload
(defun evil-unimpaired-paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1 evil-this-register))

;;;###autoload
(defun evil-unimpaired-paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1 evil-this-register))
