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

(defvar-local scoiatael/+format-on-save-disabled-modes nil)

;;;###autoload
(defun scoiatael/toggle-format-on-save ()
  (interactive)
  (let ((oldvalue scoiatael/+format-on-save-disabled-modes))
    (if oldvalue
        ;; Return to previous values
        (progn
          (setq scoiatael/+format-on-save-disabled-modes nil)
          (setq +format-on-save-disabled-modes oldvalue))
      ;; Add current mode to disabled modes
      (progn
        (setq scoiatael/+format-on-save-disabled-modes +format-on-save-disabled-modes)
        (pushnew! +format-on-save-disabled-modes major-mode)))))

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
  "Parse a file link of format \"file:///path/to/file:line:column\"."
  (let ((match (s-match (rx (? "file://")
                            (group (+ (not ":")))
                            (? ":" (group (+ (not ":")))
                               (? ":" (group (+ (not ":")))))) file-link)))
    (when-let ((groups (cdr match)))
      (pcase (length groups)
        (3 (list :file (nth 0 groups)
                 :line (string-to-number (nth 1 groups))
                 :col (string-to-number (nth 2 groups))))
        (2 (list :file (nth 0 groups)
                 :line (string-to-number (nth 1 groups))
                 :col nil))
        (1 (list :file (nth 0 groups)
                 :line nil
                 :col nil))))))

(defun find-file-existing (file)
  (find-file  (if (file-exists-p file) file
                ;; else
                (expand-file-name file (projectile-project-root)))))

;;;###autoload
(defun scoiatael/open-file-url (file)
  (interactive "sfilename: \n")
  (if-let ((match (parse-file-link file)))
      (cl-destructuring-bind (&key file line col) match
        (find-file-existing file)
        (when line
          (goto-line line))
        (when col
          (move-to-column col))
        file)
    ;; if everything else fails
    (find-file-existing file)
    file))

;;;###autoload
(defun scoiatael/switch-to-agenda-workspace ()
  "Switch to workspace named AGENDA or create it if it doesn't exist."
  (interactive)
  (let ((workspace-name "AGENDA"))
    (unless (eq (+workspace-current-name) +workspaces-main)
      (unless (+workspace-exists-p workspace-name)
        (+workspace-new workspace-name))
      (+workspace-switch workspace-name))
    (find-file-existing "~/org/todo.org")))

;;;###autoload
(defun scoiatael/switch-to-nixpkgs-workspace ()
  "Switch to workspace named NIXPKGS or create it if it doesn't exist."
  (interactive)
  (let ((workspace-name "NIXPKGS"))
    (unless (eq (+workspace-current-name) +workspaces-main)
      (unless (+workspace-exists-p workspace-name)
        (+workspace-new workspace-name))
      (+workspace-switch workspace-name))
    (dired (car (last (split-string (getenv "NIX_PATH") "="))))))

