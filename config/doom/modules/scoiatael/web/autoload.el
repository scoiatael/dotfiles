;;; scoiatael/web/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun scoiatael/convert-tailwind-theme-def ()
  "Convert Tailwind theme JSON to CSS custom properties."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\"\\([-a-z0-9]+\\)\": \"\\(.+\\)\",?" end t)
        (replace-match "--color-\\1: \\2;")))))
