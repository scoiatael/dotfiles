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
