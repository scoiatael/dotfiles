;;; scoiatael/llm/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun scoiatael/gptel-send-to-new-buffer (&optional arg)
  "Send the current region or buffer to a new gptel session buffer.

This is a modification of `gptel-send' that creates a new gptel session
buffer and writes the response there instead of in-place.
This works better with tools than the original gptel-send,
 which was more intended towards LLM replying with code suggestions in-line.

With prefix ARG, prompt for a gptel directive."
  (interactive "P")
  (let* ((query (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-substring-no-properties (point-min) (point-max))))
         (system-message (when arg
                           (read-string "System message: " gptel--system-message)))
         ;; Generate a unique buffer name
         (buffer-name (format "*gptel: %s*"
                              (format-time-string "%Y-%m-%d %H:%M:%S")))
         ;; Convert major-mode to source block language
         (src-lang (let ((mode-name (symbol-name major-mode)))
                     (if (string-suffix-p "-mode" mode-name)
                         (substring mode-name 0 (- (length mode-name) 5))
                       mode-name)))
         ;; Create new gptel buffer
         (gptel-buffer (get-buffer-create buffer-name)))

    ;; Set up the buffer as a gptel buffer
    (with-current-buffer gptel-buffer
      (org-mode)
      (gptel-mode)
      (when system-message
        (setq gptel--system-message system-message))
      ;; Format initial query as proper org mode text
      (insert "* Code Fix Request\n\n")
      (insert "Please review and fix the code according to the instructions in the region below:\n\n")
      ;; Insert the query as an org source block
      (insert (format "#+begin_src %s\n" src-lang))
      (insert query)
      (insert "\n#+end_src\n\n")
      (gptel-send))

    ;; Display the new buffer
    (pop-to-buffer gptel-buffer)))
