;;; scoiatael/org/autoload.el -*- lexical-binding: t; -*-

;; Open roam node at cursor even outside org-roam major mode
;; e.g. in nix-mode when cursor is on       ugrep # [[id:a92dcd13-5bb1-4dc7-b47c-0cef5b9affe5]|[ugrep]]
;; open org-roam note a92dcd13-5bb1-4dc7-b47c-0cef5b9affe5
;;;###autoload
(defun scoiatael/org-roam-open-id-at-point ()
  "Open org-roam node by extracting ID from link at point."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (if (string-match "\\[\\[id:\\([^]]+\\)\\]" line)
        (let ((id (match-string 1 line)))
          (save-excursion
            (org-roam-node-visit (org-roam-node-from-id id)))
          't)
      nil)))


;;;###autoload
(defun scoiatael/insert (char)
  (interactive)
  (if (eq major-mode #'vterm-mode)
      (vterm-insert char)
    (insert char)))
