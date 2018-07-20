(defgroup prog-elixir
  nil
  "Customizations for my custom layer"
  :prefix "prog-elixir")

(defcustom prog-elixir/mix-format-after-save
  nil
  "Whether to format buffer after save"
  :type 'boolean
  :group 'prog-elixir
  )

(defun elixir-mix-format-current-file ()
  (interactive)
  (if prog-elixir/mix-format-after-save
      (elixir-format)))

(defun elixir-remove-do-sugar ()
  (interactive)
  (save-excursion
    (elixir-beginning-of-defun)
    (goto-char (line-end-position))
    (backward-kill-word 1)
    (delete-horizontal-space)
    (insert ", do:")
    (forward-line 1)
    (delete-indentation)
    (forward-line 1)
    (kill-whole-line)))
