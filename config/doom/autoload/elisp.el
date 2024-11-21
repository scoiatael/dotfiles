;;; ../../dotfiles/config/doom/autoload/elisp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my-elisp-config-flymake-byte-compile (report-fn &rest _args)
  "A Flymake backend for elisp byte compilation.
Spawn an Emacs process that byte-compiles a file representing the
current buffer state and calls REPORT-FN when done."
  (when elisp-flymake--byte-compile-process
    (when (process-live-p elisp-flymake--byte-compile-process)
      (kill-process elisp-flymake--byte-compile-process)))
  (let ((temp-file (make-temp-file "elisp-flymake-byte-compile"))
        (source-buffer (current-buffer))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'nomessage))
    (let* ((output-buffer (generate-new-buffer " *elisp-flymake-byte-compile*")))
      (setq
       elisp-flymake--byte-compile-process
       (make-process
        :name "elisp-flymake-byte-compile"
        :buffer output-buffer
        :command `(,(expand-file-name invocation-name invocation-directory)
                   "--batch"
                   "--eval" "(setq byte-compile-warnings '(not unresolved docstrings))"
                   "-l" "~/.emacs.doom/lisp/doom.el"
                   "-l" "~/.emacs.doom/lisp/doom-start.el"
                   ;; "--eval" "(setq load-prefer-newer t)" ; for testing
                   ,@(mapcan (lambda (path) (list "-L" path))
                             elisp-flymake-byte-compile-load-path)
                   "-f" "elisp-flymake--batch-compile-for-flymake"
                   ,temp-file)
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (unless (process-live-p proc)
            (unwind-protect
                (cond
                 ((not (and (buffer-live-p source-buffer)
                            (eq proc (with-current-buffer source-buffer
                                       elisp-flymake--byte-compile-process))))
                  (flymake-log :warning
                               "byte-compile process %s obsolete" proc))
                 ((zerop (process-exit-status proc))
                  (elisp-flymake--byte-compile-done report-fn
                                                    source-buffer
                                                    output-buffer))
                 (t
                  (funcall report-fn
                           :panic
                           :explanation
                           (format "byte-compile process %s died" proc))))
              (ignore-errors (delete-file temp-file))
              (kill-buffer output-buffer))))
        :stderr " *stderr of elisp-flymake-byte-compile*"
        :noquery t)))))
