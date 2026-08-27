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
         (file-buffer (or (buffer-file-name) (buffer-name)))
         (system-message (when arg
                           (read-string "System message: " gptel--system-message)))
         ;; Generate a unique buffer name
         (new-buffer-name (format "*gptel: %s*"
                                  (format-time-string "%Y-%m-%d %H:%M:%S")))
         ;; Convert major-mode to source block language
         (src-lang (let ((mode-name (symbol-name major-mode)))
                     (if (string-suffix-p "-mode" mode-name)
                         (substring mode-name 0 (- (length mode-name) 5))
                       mode-name)))
         (root (project-root (project-current)))
         (agents-md (expand-file-name "AGENTS.md" root))
         (agents-md-exists (file-readable-p agents-md))
         ;; Create new gptel buffer
         (gptel-buffer (get-buffer-create new-buffer-name)))

    ;; Set up the buffer as a gptel buffer
    (with-current-buffer gptel-buffer
      (org-mode)
      (gptel-mode)
      (when system-message
        (setq gptel--system-message system-message))
      ;; Format initial query as proper org mode text
      (insert "* Code Fix Request\n\n")
      (insert (read-string  "prompt:" "Please review and fix the code according to the instructions in the region below:") "\n")
      (insert (format "Root: %s\n\n" root))
      (insert (format "Current buffer: %s\n\n" file-buffer))
      (when agents-md-exists
        (insert (format "AGENTS.md: %s\n\n" agents-md)))
      ;; Insert the query as an org source block
      (insert (format "#+begin_src %s\n" src-lang))
      (insert query)
      (insert "\n#+end_src\n\n")
      (gptel-send))

    ;; Display the new buffer
    (pop-to-buffer gptel-buffer)))

;;; Companion agent (maki over ACP, sandboxed by nono)

(defvar scoiatael/maki-companion-acp-command '("maki-companion")
  "Command starting the read-only maki ACP server.

Defined in `modules/aspects/maki.nix'.  It resolves the API key outside
the sandbox, points XDG_CONFIG_HOME at the companion's own maki config
\(which is what drops the write tools), points TMPDIR at the one writable
scratch directory, and execs `maki acp' under the `maki-companion' nono
profile.")

(defvar scoiatael/agent-shell-companion-denied-tool-kinds '("edit" "delete" "move")
  "ACP tool-call kinds the companion is never allowed to run.

Everything else is auto-approved, `execute' included: the point of the
companion is that it can run checks and linters unattended, and the nono
profile -- not a modal dialog -- is what stops it changing anything.")

;;;###autoload
(defun scoiatael/agent-shell-maki-companion-config ()
  "Return the agent-shell configuration for the maki companion agent."
  (agent-shell-make-agent-config
   :identifier 'maki-companion
   :mode-line-name "Maki companion"
   :buffer-name "Maki companion"
   :shell-prompt "Companion> "
   :shell-prompt-regexp "Companion> "
   :client-maker
   (lambda (buffer)
     (agent-shell--make-acp-client
      :command (car scoiatael/maki-companion-acp-command)
      :command-params (cdr scoiatael/maki-companion-acp-command)
      :environment-variables (agent-shell-make-environment-variables
                              :inherit-env t)
      :context-buffer buffer))
   :install-instructions
   "Provided by the `maki' aspect. Run a home-manager switch to install it."))

(defun scoiatael/agent-shell-companion-buffer ()
  "Return this project's companion shell buffer, starting one if needed.

Deliberately not `agent-shell--dwim' or `agent-shell-shell-buffer': both
end up in `agent-shell--shell-buffer', whose last resort is prompting for
an agent, and `--dwim' ignores its :config unless :new-shell is set."
  (or (seq-find (lambda (buffer)
                  (eq 'maki-companion
                      (map-elt (agent-shell-get-config buffer) :identifier)))
                (agent-shell-project-buffers))
      (agent-shell-start :config (scoiatael/agent-shell-maki-companion-config))))

;;;###autoload
(defun scoiatael/agent-shell-maki-companion ()
  "Start, or switch to, the read-only maki companion shell."
  (interactive)
  (let ((buffer (scoiatael/agent-shell-companion-buffer)))
    (if-let* ((window (get-buffer-window buffer)))
        (select-window window)
      (select-window (display-buffer buffer agent-shell-display-action)))))

;;;###autoload
(defun scoiatael/agent-shell-companion-permission-responder (permission)
  "Answer PERMISSION without prompting, in companion shells only.

Returns nil elsewhere, so other agents keep the interactive dialog."
  (when (eq 'maki-companion
            (map-elt (agent-shell-get-config (current-buffer)) :identifier))
    (let* ((kind (map-elt (map-elt permission :tool-call) :kind))
           (wanted (if (member kind scoiatael/agent-shell-companion-denied-tool-kinds)
                       "reject_once"
                     "allow_once")))
      (when-let* ((choice (seq-find (lambda (option)
                                      (equal (map-elt option :kind) wanted))
                                    (map-elt permission :options))))
        (funcall (map-elt permission :respond) (map-elt choice :option-id))
        t))))

(defvar scoiatael/agent-shell-companion-review-prompt
  "Review this for anything worth changing. Skip praise and summary."
  "Default request sent by `scoiatael/agent-shell-companion-review'.")

;;;###autoload
(defun scoiatael/agent-shell-companion-review (&optional arg)
  "Ask the companion agent about the region, or the whole buffer.

Sends a `file:line-line' reference rather than the text itself: the agent
can read the file, the transcript stays short, and the reference keeps
pointing at the right place as the buffer changes.

With prefix ARG, prompt for the question instead of using
`scoiatael/agent-shell-companion-review-prompt'."
  (interactive "P")
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (let* ((root (if-let* ((project (project-current)))
                   (project-root project)
                 default-directory))
         (target (if (use-region-p)
                     (format "%s:%d-%d"
                             (file-relative-name (buffer-file-name) root)
                             (line-number-at-pos (region-beginning))
                             (line-number-at-pos (max (region-beginning)
                                                      (1- (region-end)))))
                   (file-relative-name (buffer-file-name) root)))
         (question (if arg
                       (read-string (format "Ask about %s: " target)
                                    scoiatael/agent-shell-companion-review-prompt)
                     scoiatael/agent-shell-companion-review-prompt)))
    (deactivate-mark)
    (agent-shell-insert :text (format "%s\n\n%s" question target)
                        :submit t
                        :shell-buffer (scoiatael/agent-shell-companion-buffer))))
