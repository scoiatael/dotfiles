;;; amp.el --- Amp IDE integration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Keegan Carruthers-Smith

;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Author: Amp <amp@ampcode.com>
;; Version: 0.0.2
;; Package-Requires: ((emacs "28.1") (websocket "1.12"))
;; Keywords: amp, ai, agent, assistant
;; URL: https://github.com/keegancsmith/amp.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This package provides integration between Emacs and the Amp CLI.
;; It allows Amp to see the file you currently have open, your cursor
;; position, your text selection, and diagnostics.
;;
;; Based on the amp.nvim plugin architecture.

;;; Code:

(require 'json)
(require 'websocket)
(require 'cl-lib)

;;; Configuration

(defgroup amp nil
  "Amp IDE integration for Emacs."
  :group 'tools
  :prefix "amp-")

(defcustom amp-log-level 'info
  "Logging level for Amp plugin.
One of: trace, debug, info, warn, error."
  :type '(choice (const :tag "Trace" trace)
                 (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warn" warn)
                 (const :tag "Error" error))
  :group 'amp)

;;; State

(defvar amp--server nil
  "The WebSocket server instance.")

(defvar amp--port nil
  "The port the server is running on.")

(defvar amp--auth-token nil
  "Authentication token for the server.")

(defvar amp--clients nil
  "List of connected clients.")

(defvar amp--connected nil
  "Whether any clients are connected.")

(defvar amp--latest-selection nil
  "The latest selection that was broadcast.")

(defvar amp--latest-visible-files nil
  "The latest visible files that were broadcast.")

(defvar amp--selection-timer nil
  "Timer for debounced selection updates.")

(defvar amp--event-listeners (make-hash-table :test 'equal)
  "Event listeners by event name.")

;;; Logging

(defvar amp--log-buffer "*amp-log*"
  "Buffer name for Amp log messages.")

(defvar amp--last-error nil
  "Most recent error (cons of timestamp, context, and message).")

(defun amp--log (level context message &rest args)
  "Log a message at LEVEL from CONTEXT with MESSAGE and ARGS."
  (let* ((levels '((trace . 0) (debug . 1) (info . 2) (warn . 3) (error . 4)))
         (current-level (or (cdr (assq amp-log-level levels)) 2))
         (msg-level (or (cdr (assq level levels)) 2)))
    (when (>= msg-level current-level)
      (let ((formatted-msg (apply #'format message args)))
        (with-current-buffer (get-buffer-create amp--log-buffer)
          (goto-char (point-max))
          (insert (format "[%s] [%s] %s: %s\n"
                          (format-time-string "%H:%M:%S")
                          (upcase (symbol-name level))
                          context
                          formatted-msg)))
        (when (eq level 'error)
          (setq amp--last-error (list (float-time) context formatted-msg)))
        (when (memq level '(warn error))
          (message "[Amp %s] %s" (upcase (symbol-name level)) formatted-msg))))))

;;; Utility Functions

(defun amp--get-data-home ()
  "Get the data directory for Amp, following the same logic as amp.nvim."
  (or (getenv "AMP_DATA_HOME")
      (let ((system-type-name (symbol-name system-type)))
        (cond
         ((string-match "windows" system-type-name)
          (expand-file-name "~/.local/share"))
         ((string-match "darwin" system-type-name)
          (expand-file-name "~/.local/share"))
         (t
          (or (getenv "XDG_DATA_HOME")
              (expand-file-name "~/.local/share")))))))

(defun amp--lock-dir ()
  "Get the lock directory path."
  (expand-file-name "amp/ide" (amp--get-data-home)))

(defun amp--generate-auth-token ()
  "Generate a random authentication token."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (token ""))
    (dotimes (_ 32)
      (setq token (concat token (string (aref chars (random (length chars)))))))
    token))

(defun amp--create-lockfile (port auth-token)
  "Create a lockfile for PORT with AUTH-TOKEN."
  (let* ((lock-dir (amp--lock-dir))
         (lockfile-path (expand-file-name (format "%d.json" port) lock-dir))
         (lock-data (json-encode
                     `((port . ,port)
                       (authToken . ,auth-token)
                       (pid . ,(emacs-pid))
                       (workspaceFolders . ["/"])
                       (ideName . ,(format "Emacs %s" emacs-version))))))
    (unless (file-directory-p lock-dir)
      (make-directory lock-dir t))
    (set-file-modes lock-dir #o700)
    (with-temp-file lockfile-path
      (insert lock-data))
    (set-file-modes lockfile-path #o600)
    lockfile-path))

(defun amp--remove-lockfile (port)
  "Remove the lockfile for PORT."
  (when port
    (let ((lockfile-path (expand-file-name (format "%d.json" port) (amp--lock-dir))))
      (when (file-exists-p lockfile-path)
        (delete-file lockfile-path)))))

;;; Event System

(defun amp--on (event-name callback)
  "Register CALLBACK for EVENT-NAME."
  (let ((listeners (gethash event-name amp--event-listeners)))
    (puthash event-name (cons callback listeners) amp--event-listeners)))

(defun amp--emit (event-name &rest args)
  "Emit EVENT-NAME with ARGS to all registered listeners."
  (let ((listeners (gethash event-name amp--event-listeners)))
    (dolist (callback listeners)
      (apply callback args))))

;;; IDE Protocol

(defun amp--wrap-notification (notification)
  "Wrap NOTIFICATION in IDE protocol format."
  `((serverNotification . ,notification)))

(defun amp--wrap-response (id response)
  "Wrap RESPONSE with ID in IDE protocol format."
  `((serverResponse . ,(cons `(id . ,id) response))))

(defun amp--wrap-error (id error)
  "Wrap ERROR with ID in IDE protocol format."
  `((serverResponse . ((id . ,id) (error . ,error)))))

;;; WebSocket Server

(defun amp--handle-websocket-message (ws frame)
  "Handle incoming WebSocket message from WS with FRAME."
  (let* ((message (websocket-frame-text frame))
         (parsed (condition-case err
                     (json-read-from-string message)
                   (error
                    (amp--log 'warn "server" "Invalid JSON: %s" message)
                    nil)))
         (request (and parsed (alist-get 'clientRequest parsed))))
    (when request
      (amp--handle-ide-request ws request))))

(defun amp--handle-ide-request (ws request)
  "Handle IDE REQUEST from websocket WS."
  (let ((id (alist-get 'id request)))
    (cond
     ;; Ping request
     ((alist-get 'ping request)
      (amp--send-ide ws (amp--wrap-response id `((ping . ((message . ,(alist-get 'message (alist-get 'ping request)))))))))

     ;; Authenticate request
     ((alist-get 'authenticate request)
      (let* ((auth-req (alist-get 'authenticate request))
             (provided-token (alist-get 'token auth-req)))
        (if (and provided-token (string= provided-token amp--auth-token))
            (amp--send-ide ws (amp--wrap-response id '((authenticate . ((authenticated . t))))))
          (amp--send-ide ws (amp--wrap-error id '((code . -32603) (message . "Authentication failed") (data . "Invalid or missing authentication token")))))))

     ;; Read file request
     ((alist-get 'readFile request)
      (let* ((read-req (alist-get 'readFile request))
             (path (alist-get 'path read-req)))
        (if (not path)
            (amp--send-ide ws (amp--wrap-error id '((code . -32602) (message . "Invalid params") (data . "readFile requires path parameter"))))
          (condition-case err
              (let ((content (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))))
                (amp--send-ide ws (amp--wrap-response id `((readFile . ((success . t) (content . ,content) (encoding . "utf-8")))))))
            (error
             (amp--send-ide ws (amp--wrap-response id `((readFile . ((success . :json-false) (message . ,(error-message-string err))))))))))))

     ;; Edit file request
     ((alist-get 'editFile request)
      (let* ((edit-req (alist-get 'editFile request))
             (path (alist-get 'path edit-req))
             (full-content (alist-get 'fullContent edit-req)))
        (if (not path)
            (amp--send-ide ws (amp--wrap-error id '((code . -32602) (message . "Invalid params") (data . "editFile requires path parameter"))))
          (if (not full-content)
              (amp--send-ide ws (amp--wrap-error id '((code . -32602) (message . "Invalid params") (data . "editFile requires fullContent parameter"))))
            (condition-case err
            (let* ((full-path (expand-file-name path))
            (bufnr (or (find-buffer-visiting full-path)
            (find-file-noselect full-path))))
            (with-current-buffer bufnr
            (let ((saved-point (point))
                  (saved-window-start (and (get-buffer-window bufnr)
                                          (window-start (get-buffer-window bufnr)))))
                (erase-buffer)
                       (insert full-content)
                       (goto-char (min saved-point (point-max)))
                       (when saved-window-start
                         (set-window-start (get-buffer-window bufnr)
                                          (min saved-window-start (point-max))))
                       (save-buffer)))
                   (amp--send-ide ws (amp--wrap-response id `((editFile . ((success . t) (message . ,(format "Edit applied successfully to %s" path)) (appliedChanges . t)))))))
              (error
               (amp--send-ide ws (amp--wrap-response id `((editFile . ((success . :json-false) (message . ,(error-message-string err)))))))))))))

     ;; Get diagnostics request
     ((alist-get 'getDiagnostics request)
      (let* ((diag-req (alist-get 'getDiagnostics request))
             (path (alist-get 'path diag-req)))
        (if (not path)
            (amp--send-ide ws (amp--wrap-error id '((code . -32602) (message . "Invalid params") (data . "getDiagnostics requires path parameter"))))
          (let ((entries (amp--get-diagnostics path)))
            (amp--send-ide ws (amp--wrap-response id `((getDiagnostics . ((entries . ,entries))))))))))

     ;; Unknown request
     (t
      (amp--send-ide ws (amp--wrap-error id '((code . -32601) (message . "Method not found") (data . "Unknown IDE request method"))))))))

(defun amp--send-ide (ws data)
  "Send IDE protocol DATA to websocket WS."
  (when (websocket-openp ws)
    (let ((json-message (json-encode data)))
      (condition-case err
          (websocket-send-text ws json-message)
        (error
         (amp--log 'warn "server" "Failed to send message: %s" (error-message-string err)))))))

(defun amp--broadcast-ide (notification)
  "Broadcast IDE NOTIFICATION to all connected clients."
  (when amp--clients
    (let ((json-message (json-encode (amp--wrap-notification notification))))
      (dolist (client amp--clients)
        (when (websocket-openp client)
          (condition-case err
              (websocket-send-text client json-message)
            (error
             (amp--log 'warn "server" "Failed to broadcast to client: %s" (error-message-string err)))))))))

(defun amp--on-client-connect (ws)
  "Handle client connection from WS."
  (push ws amp--clients)
  (setq amp--connected t)
  (amp--log 'info "server" "Client connected")
  (amp--emit 'client-connect ws)
  ;; Send initial state after short delay
  (run-with-timer 0.05 nil #'amp--send-initial-state))

(defun amp--on-client-disconnect (ws)
  "Handle client disconnection from WS."
  (setq amp--clients (delq ws amp--clients))
  (when (null amp--clients)
    (setq amp--connected nil)
    (amp--log 'info "server" "Disconnected from Amp (no clients)"))
  (amp--emit 'client-disconnect ws))

;;; Selection Tracking

(defun amp--get-file-uri ()
  "Get the file URI for the current buffer."
  (when buffer-file-name
    (concat "file://" (expand-file-name buffer-file-name))))

(defun amp--get-cursor-selection ()
  "Get the current cursor position as a selection."
  (let ((uri (amp--get-file-uri)))
    (when uri
      (let* ((pos (point))
             (line (1- (line-number-at-pos pos)))
             (col (- pos (line-beginning-position)))
             (line-content (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
        `((text . "")
          (fileUrl . ,uri)
          (selection . ((start . ((line . ,line) (character . ,col)))
                       (end . ((line . ,line) (character . ,col)))))
          (lineContent . ,line-content))))))

(defun amp--get-visual-selection ()
  "Get the current visual selection."
  (when (and (use-region-p) (amp--get-file-uri))
    (let* ((uri (amp--get-file-uri))
           (start-pos (region-beginning))
           (end-pos (region-end))
           (start-line (1- (line-number-at-pos start-pos)))
           (start-col (- start-pos (save-excursion (goto-char start-pos) (line-beginning-position))))
           (end-line (1- (line-number-at-pos end-pos)))
           (end-col (- end-pos (save-excursion (goto-char end-pos) (line-beginning-position))))
           (text (buffer-substring-no-properties start-pos end-pos)))
      `((text . ,text)
        (fileUrl . ,uri)
        (selection . ((start . ((line . ,start-line) (character . ,start-col)))
                     (end . ((line . ,end-line) (character . ,end-col)))))))))

(defun amp--get-current-selection ()
  "Get the current selection (visual or cursor)."
  (or (amp--get-visual-selection)
      (amp--get-cursor-selection)))

(defun amp--selection-to-ide-format (selection)
  "Convert SELECTION to IDE protocol format."
  (let ((sel (alist-get 'selection selection)))
    `((uri . ,(alist-get 'fileUrl selection))
      (selections . [((range . ((startLine . ,(alist-get 'line (alist-get 'start sel)))
                               (startCharacter . ,(alist-get 'character (alist-get 'start sel)))
                               (endLine . ,(alist-get 'line (alist-get 'end sel)))
                               (endCharacter . ,(alist-get 'character (alist-get 'end sel)))))
                     (content . ,(alist-get 'text selection)))]))))

(defun amp--selection-changed-p (new-selection)
  "Check if NEW-SELECTION is different from the last broadcast selection."
  (not (equal new-selection amp--latest-selection)))

(defun amp--update-selection ()
  "Update and broadcast the current selection."
  (when amp--server
    (let ((selection (amp--get-current-selection)))
      (when (and selection (amp--selection-changed-p selection))
        (setq amp--latest-selection selection)
        (let ((ide-notification (amp--selection-to-ide-format selection)))
          (amp--broadcast-ide `((selectionDidChange . ,ide-notification)))
          (amp--log 'debug "selection" "Selection changed: %s" (alist-get 'uri ide-notification)))))))

(defun amp--debounced-selection-update ()
  "Debounced selection update."
  (when amp--selection-timer
    (cancel-timer amp--selection-timer))
  (setq amp--selection-timer
        (run-with-timer 0.1 nil #'amp--update-selection)))

;;; Visible Files Tracking

(defun amp--get-visible-files ()
  "Get all currently visible files."
  (let ((uris nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (window (window-list))
      (let* ((buf (window-buffer window))
             (name (buffer-file-name buf)))
        (when (and name (not (gethash name seen)))
          (puthash name t seen)
          (push (concat "file://" (expand-file-name name)) uris))))
    (nreverse uris)))

(defun amp--visible-files-changed-p (new-files)
  "Check if NEW-FILES is different from the last broadcast files."
  (not (equal (sort (copy-sequence new-files) #'string<)
              (sort (copy-sequence amp--latest-visible-files) #'string<))))

(defun amp--broadcast-visible-files (&optional force)
  "Broadcast visible files if changed or FORCE is non-nil."
  (when amp--server
    (let ((files (amp--get-visible-files)))
      (when (or force (amp--visible-files-changed-p files))
        (setq amp--latest-visible-files files)
        (amp--broadcast-ide `((visibleFilesDidChange . ((uris . ,(vconcat files))))))
        (amp--log 'debug "visible-files" "Visible files changed, count: %d" (length files))))))

;;; Diagnostics

(defun amp--severity-to-protocol (severity)
  "Convert SEVERITY to protocol format."
  (pcase severity
    ('flymake-error "error")
    ('eglot-error "error")
    (':error "error")
    ('flymake-warning "warning")
    ('eglot-warning "warning")
    (':warning "warning")
    ('flymake-note "info")
    ('eglot-note "info")
    (':note "info")
    (_ "info")))

(defun amp--diagnostic-to-protocol (diag)
  "Convert Flymake diagnostic DIAG to protocol format."
  (save-excursion
    (let* ((beg (flymake-diagnostic-beg diag))
           (end (flymake-diagnostic-end diag))
           (beg-line (progn (goto-char beg) (1- (line-number-at-pos))))
           (beg-col (- beg (line-beginning-position)))
           (end-line (progn (goto-char end) (1- (line-number-at-pos))))
           (end-col (- end (line-beginning-position)))
           (line-content (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
      `((range . ((startLine . ,beg-line)
                  (startCharacter . ,beg-col)
                  (endLine . ,end-line)
                  (endCharacter . ,end-col)))
        (severity . ,(amp--severity-to-protocol (flymake-diagnostic-type diag)))
        (description . ,(flymake-diagnostic-text diag))
        (lineContent . ,line-content)
        (startOffset . ,beg-col)
        (endOffset . ,end-col)))))

(defun amp--get-diagnostics (path)
  "Get diagnostics for PATH (file or directory prefix).
Returns an array of entries with uri and diagnostics."
  (let ((abs-path (expand-file-name path))
        (entries-by-uri (make-hash-table :test 'equal)))
    ;; Collect diagnostics from all buffers
    (dolist (buf (buffer-list))
      (when-let ((buf-name (buffer-file-name buf)))
        (let ((abs-buf-name (expand-file-name buf-name)))
          ;; Check if buffer path starts with the requested path (prefix match)
          (when (string-prefix-p abs-path abs-buf-name)
            (with-current-buffer buf
              (when (and (featurep 'flymake)
                        (bound-and-true-p flymake-mode))
                (let ((diags (mapcar #'amp--diagnostic-to-protocol
                                   (flymake-diagnostics))))
                  (when diags
                    (let ((uri (concat "file://" abs-buf-name)))
                      (puthash uri
                              `((uri . ,uri)
                                (diagnostics . ,(vconcat diags)))
                              entries-by-uri))))))))))
    ;; Convert hash table to array
    (let ((entries nil))
      (maphash (lambda (_key value)
                 (push value entries))
               entries-by-uri)
      (vconcat entries))))

;;; Hooks

(defun amp--setup-hooks ()
  "Set up hooks for tracking selection, visible files, and diagnostics."
  (add-hook 'post-command-hook #'amp--debounced-selection-update)
  (add-hook 'window-configuration-change-hook #'amp--broadcast-visible-files)
  (add-hook 'buffer-list-update-hook #'amp--broadcast-visible-files))

(defun amp--remove-hooks ()
  "Remove all Amp hooks."
  (remove-hook 'post-command-hook #'amp--debounced-selection-update)
  (remove-hook 'window-configuration-change-hook #'amp--broadcast-visible-files)
  (remove-hook 'buffer-list-update-hook #'amp--broadcast-visible-files))

;;; Server Management

(defun amp--cleanup-on-exit ()
  "Clean up Amp server on Emacs exit."
  (when amp--server
    (amp--remove-lockfile amp--port)
    (websocket-server-close amp--server)))

(defun amp--send-initial-state ()
  "Send initial state to newly connected clients."
  (amp--broadcast-visible-files t)
  (amp--update-selection))

;;;###autoload
(defun amp-start ()
  "Start the Amp WebSocket server."
  (interactive)
  (when amp--server
    (amp--log 'warn "server" "Server already running on port %d" amp--port)
    (user-error "Amp server already running on port %d" amp--port))

  (let* ((auth-token (amp--generate-auth-token))
         (port (+ 10000 (random 55535)))
         (server nil))
    (setq amp--auth-token auth-token)

    ;; Create WebSocket server
    (condition-case err
        (setq server
              (websocket-server
               port
               :host 'local
               :on-open #'amp--on-client-connect
               :on-message #'amp--handle-websocket-message
               :on-close #'amp--on-client-disconnect
               :on-error (lambda (_ws type err)
                          (amp--log 'error "server" "WebSocket error (%s): %s" type err))))
      (error
       (amp--log 'error "server" "Failed to start server: %s" (error-message-string err))
       (user-error "Failed to start Amp server: %s" (error-message-string err))))

    (setq amp--server server
          amp--port port
          amp--clients nil
          amp--connected nil)

    ;; Create lockfile
    (condition-case err
        (amp--create-lockfile port auth-token)
      (error
       (websocket-server-close server)
       (setq amp--server nil
             amp--port nil
             amp--auth-token nil)
       (amp--log 'error "server" "Failed to create lockfile: %s" (error-message-string err))
       (user-error "Failed to create lockfile: %s" (error-message-string err))))

    ;; Set up tracking hooks
    (amp--setup-hooks)

    ;; Add kill-emacs-hook to clean up on exit
    (add-hook 'kill-emacs-hook #'amp--cleanup-on-exit)

    ;; Send plugin metadata after delay
    (run-with-timer 0.2 nil
                    (lambda ()
                      (amp--broadcast-ide
                       `((pluginMetadata . ((version . "0.1.0")))))))

    (amp--log 'info "server" "Server started on port %d" port)
    (message "Amp server started on port %d" port)))

;;;###autoload
(defun amp-stop ()
  "Stop the Amp WebSocket server."
  (interactive)
  (unless amp--server
    (amp--log 'info "server" "Server is not running")
    (user-error "Amp server is not running"))

  (amp--remove-lockfile amp--port)
  (amp--remove-hooks)

  (websocket-server-close amp--server)

  (setq amp--server nil
        amp--port nil
        amp--auth-token nil
        amp--clients nil
        amp--connected nil
        amp--latest-selection nil
        amp--latest-visible-files nil)

  (amp--log 'info "server" "Server stopped")
  (message "Amp server stopped"))

;;;###autoload
(defun amp-status ()
  "Show the status of the Amp server."
  (interactive)
  (if amp--server
      (let ((status (if amp--connected "connected" "waiting for clients")))
        (message "Amp server running on port %d (%s)" amp--port status)
        (amp--log 'info "status" "Server running on port %d (%s)" amp--port status)
        (amp--log 'info "status" "Visible files: %d" (length amp--latest-visible-files))
        (when amp--last-error
          (amp--log 'info "status" "Last error: %s" (caddr amp--last-error))))
    (message "Amp server is not running")
    (amp--log 'info "status" "Server is not running")))

;;;###autoload
(defun amp-send-message (message)
  "Send MESSAGE to the Amp agent."
  (interactive "sMessage: ")
  (unless amp--server
    (user-error "Amp server is not running"))
  (amp--broadcast-ide `((userSentMessage . ((message . ,message)))))
  (amp--log 'debug "message" "Message sent to agent"))

;;;###autoload
(defun amp-send-region (start end)
  "Send region between START and END to Amp agent."
  (interactive "r")
  (amp-send-message (buffer-substring-no-properties start end)))

;;;###autoload
(defun amp-send-to-prompt (text)
  "Send TEXT to append to the Amp prompt."
  (interactive "sText: ")
  (unless amp--server
    (user-error "Amp server is not running"))
  (amp--broadcast-ide `((appendToPrompt . ((message . ,text)))))
  (amp--log 'debug "message" "Text appended to prompt"))

;;;###autoload
(define-minor-mode amp-mode
  "Minor mode for Amp IDE integration."
  :global t
  :group 'amp
  :lighter " Amp"
  (if amp-mode
      (unless amp--server
        (amp-start))
    (when amp--server
      (amp-stop))))

(provide 'amp)
;;; amp.el ends here
