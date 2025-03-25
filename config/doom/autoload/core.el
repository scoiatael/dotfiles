;;; ~/dotfiles/emacs/doom.d/autoload/core.el -*- lexical-binding: t; -*-

;;;###autoload
(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

;;;###autoload
(defun scoiatael/visualize-trailing-whitespace ()
  "Visualize trailingwhitespace in current buffer"
  (interactive)
  (require 'whitespace)
  (setq whitespace-style '(face trailing))
  (whitespace-turn-on))

;;;###autoload
(defun scoiatael/suppress-math-support-messages (old-fun format &rest args)
  (if (string= format "markdown-mode math support enabled")
      (ignore)
    (apply old-fun format args)))

(defvar scoiatael/+format-on-save-enabled-modes nil)

;;;###autoload
(defun scoiatael/toggle-format-on-save ()
  (interactive)
  (let ((oldvalue scoiatael/+format-on-save-enabled-modes))
    (setq scoiatael/+format-on-save-enabled-modes +format-on-save-enabled-modes)
    (setq +format-on-save-enabled-modes oldvalue)))

;;;###autoload
(defmacro scoiatael/defer (&rest body) `(run-at-time 1 nil (lambda () ,(cons 'progn body))))

;;;###autoload
(defun scoiatael/add-project ()
  (interactive)
  (f-touch ".project")
  (projectile-add-known-project default-directory))


;;;###autoload
(defun scoiatael/find-project ()
  (interactive)
  (+workspace/new)
  (find-file "~/Documents/Git"))

(defun parse-file-link (file-link)
  "Parse a file link of the format \"file:///path/to/file:line:column\".
Returns a list containing the file path, line number, and column number.
Returns nil if the input string is not in the expected format."
  (let ((regexp "^file://\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)$"))
    (if (string-match regexp file-link)
        (list (match-string 1 file-link)
              (string-to-number (match-string 2 file-link))
              (string-to-number (match-string 3 file-link)))
      nil)))

;;;###autoload
(defun scoiatael/open-file-url (file)
  (interactive "sfilename: \n")
  (if-let ((match (parse-file-link file)))
      (cl-destructuring-bind (file line column) match
        (find-file file)
        (goto-line line)
        (move-to-column column))))
