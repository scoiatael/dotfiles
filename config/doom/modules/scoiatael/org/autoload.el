
;; Define new minor mode that will fontify org roam nodes in current file
;; i.e. comments containing [[id:dbe828b1-9291-4249-8cb0-b74bac8aade5][gron]]
;; and rebind ENTER to visit them using scoiatael/org-roam-open-id-at-point

(defface scoiatael/org-roam-node-face
  '((t :foreground "#4682B4" :underline t))
  "Face for org-roam nodes in comments.")

(defvar scoiatael/org-roam-node-regexp
  "\\[\\[id:[a-f0-9\\-]+\\]\\[\\([^]]+\\)\\]\\]"
  "Regexp to match org-roam nodes.")

(defvar scoiatael/org-roam-fontify-mode-map (make-sparse-keymap)
  "Keymap for org-roam-fontify-mode.")

(evil-define-key 'normal scoiatael/org-roam-fontify-mode-map
  (kbd "<return>")  (lambda ()
                      (interactive)
                      (unless (scoiatael/org-roam-open-id-at-point)
                        (scoiatael/org-roam-fontify-mode 'toggle)
                        (execute-kbd-macro (this-command-keys))
                        (scoiatael/org-roam-fontify-mode 'toggle))))

;;;###autoload
(define-minor-mode scoiatael/org-roam-fontify-mode
  "Minor mode to fontify org-roam nodes and bind ENTER to visit them."
  :lighter " OrgRoam"
  :keymap scoiatael/org-roam-fontify-mode-map
  (if scoiatael/org-roam-fontify-mode
      (font-lock-add-keywords nil
                              `((,scoiatael/org-roam-node-regexp 0 'scoiatael/org-roam-node-face t)))
    (font-lock-remove-keywords nil
                               `((,scoiatael/org-roam-node-regexp 0 'scoiatael/org-roam-node-face t))))
  (font-lock-flush))


;;;###autoload
(defun scoiatael/replace-pipes-with-todo (start end)
  "Replace first occurrence of |text| with ** TODO on each line in region."
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (beginning-of-line)
          (when (re-search-forward "|[^|]+|" (line-end-position) t)
            (replace-match "** TODO"))
          (forward-line 1)
          (setq end (+ end (- (length "** TODO") (length (match-string 0)))))))
    (message "No region selected")))

;;;###autoload
(defun scoiatael/export-subtree-to-markdown-buffer ()
  "Export current org subtree to markdown in a new buffer."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (let* ((subtree-heading (org-get-heading t t t t))
           (org-export-with-toc nil)
           (buffer-name (format "*Markdown Export: %s*" (or subtree-heading "Subtree")))
           (markdown-content (org-export-as 'md nil nil t)))
      (widen)
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert markdown-content)
        (markdown-mode)
        (goto-char (point-min))
        (save-excursion
          (while (re-search-forward "^[[:space:]]*\n" nil t)
            (replace-match "")))
        (markdown-mode)
        (display-buffer (current-buffer))))))
