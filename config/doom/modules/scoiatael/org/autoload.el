
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

