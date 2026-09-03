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

;;; Companion agent (read-only ACP agent, sandboxed by nono)

(defcustom scoiatael/companion-backend 'maki
  "Which ACP agent backs the companion shell.

`maki' runs `maki acp'; `claude' runs claude-agent-acp.  Both are
sandboxed the same way and answer to the same commands, so this is a
free choice -- see `modules/aspects/companion.nix'.  Existing shells keep
the backend they started with; `agent-shell-restart' picks up a change."
  :type '(choice (const maki) (const claude))
  :group 'agent-shell)

(defvar scoiatael/maki-companion-acp-command '("maki-companion")
  "Command starting the read-only maki ACP server.

Defined in `modules/aspects/companion.nix'.  It resolves the API key
outside the sandbox, points XDG_CONFIG_HOME at the companion's own maki
config \(which is what drops the write tools), points TMPDIR at the one
writable scratch directory, and execs `maki acp' under the
`maki-companion' nono profile.")

(defvar scoiatael/claude-companion-acp-command '("claude-companion")
  "Command starting the read-only claude-agent-acp server.

The claude-agent-acp twin of `scoiatael/maki-companion-acp-command',
also from `modules/aspects/companion.nix'.  Points CLAUDE_CONFIG_DIR at
the companion's own config -- settings.json denying the write tools, the
companion prompt as CLAUDE.md -- and runs under the `claude-companion'
nono profile.")

(defvar scoiatael/claude-companion-disallowed-tools
  ["Write" "Edit" "NotebookEdit"]
  "Tools claude-agent-acp is started without.

Sent as `_meta.claudeCode.options.disallowedTools', which the adapter
merges into the Claude Agent SDK options, so the model never sees them.
This is the counterpart of disabling maki's write/edit plugins; the
`permissions.deny' list in the companion settings.json is the second
layer, and the nono profile the third.

A vector rather than a list: it has to reach the agent as a JSON array,
and a list of strings would be ambiguous with an alist.")

(defun scoiatael/claude-companion-session-meta ()
  "Return the `_meta' claude-agent-acp gets with session-creating requests."
  `((claudeCode
     . ((options
         . ((disallowedTools . ,scoiatael/claude-companion-disallowed-tools)
            ;; Recent Claude models default `thinking.display' to "omitted",
            ;; which streams signature-only blocks with no visible text.  The
            ;; same request agent-shell's own Claude config makes.
            (thinking . ((type . "adaptive")
                         (display . "summarized")))))))))

(defvar scoiatael/agent-shell-companion-denied-tool-kinds '("edit" "delete" "move")
  "ACP tool-call kinds the companion is never allowed to run.

Everything else is auto-approved, `execute' included: the point of the
companion is that it can run checks and linters unattended, and the nono
profile -- not a modal dialog -- is what stops it changing anything.")

(defun scoiatael/agent-shell-companion-client-maker (command)
  "Return a client maker running COMMAND, a list of program and arguments."
  (lambda (buffer)
    (agent-shell--make-acp-client
     :command (car command)
     :command-params (cdr command)
     :environment-variables (agent-shell-make-environment-variables
                             :inherit-env t)
     :context-buffer buffer)))

;;;###autoload
(defun scoiatael/agent-shell-maki-companion-config ()
  "Return the agent-shell configuration for the companion agent.

Which backend it runs is `scoiatael/companion-backend'.  The
`:identifier' is the same either way, so the buffer lookup in
`scoiatael/agent-shell-companion-buffer' and the auto-approval in
`scoiatael/agent-shell-companion-permission-responder' need not care."
  (apply
   #'agent-shell-make-agent-config
   :identifier 'maki-companion
   :shell-prompt "Companion> "
   :shell-prompt-regexp "Companion> "
   :install-instructions
   "Provided by the `companion' aspect. Run a home-manager switch to install it."
   (pcase scoiatael/companion-backend
     ('maki
      (list :mode-line-name "Maki companion"
            :buffer-name "Maki companion"
            :client-maker (scoiatael/agent-shell-companion-client-maker
                           scoiatael/maki-companion-acp-command)))
     ('claude
      (list :mode-line-name "Claude companion"
            :buffer-name "Claude companion"
            :client-maker (scoiatael/agent-shell-companion-client-maker
                           scoiatael/claude-companion-acp-command)
            ;; MCP servers deliberately absent: both backends get them from
            ;; nix, so `programs.mcp.servers' stays the single registry.
            :session-meta (scoiatael/claude-companion-session-meta)))
     (backend (user-error "Unknown `scoiatael/companion-backend': %s" backend)))))

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
  "Start, or switch to, the read-only companion shell."
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
