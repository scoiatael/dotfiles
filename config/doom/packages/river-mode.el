;;; river-mode.el --- Emacs mode for River configuration language
;;; https://raw.githubusercontent.com/jdbaldry/river-mode/refs/heads/main/river-mode.el

;; TODO: add license.

;; Author: Jack Baldry <jack.baldry@grafana.com>
;; Version: 1.0
;; Package-Requires:
;; Keywords: river, grafana, agent, flow
;; URL: https://github.com/jdbaldry/river-mode

;;; Commentary:

;;; Code:
(defvar river-mode-hook nil "Hook executed when river-mode is run.")

(defvar river-mode-map (make-sparse-keymap) "Keymap for River major mode.")

(defvar river-tab-width 1 "Number of tabs characters to insert for indentation.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rvr\\'" . river-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.river\\'" . river-mode))

(rx-define bin (any ?0 ?1))
(rx-define oct (any "0-7"))

(rx-define readable-digits (digit-charset)
  (seq digit-charset (* (or (seq ?_ digit-charset) digit-charset))))

(rx-define bin-digits (readable-digits bin))
(rx-define dec-digits (readable-digits digit))
(rx-define hex-digits (readable-digits hex))
(rx-define oct-digits (readable-digits oct))

(rx-define bin-lit (seq ?0 (any ?b ?B) (? ?_) (* bin-digits)))
(rx-define dec-lit (seq (or ?0 (seq (any "1-9") (* (seq (? ?_) dec-digits))))))
(rx-define hex-lit (seq ?0 (any ?x ?X) (? ?_) (* hex-digits)))
(rx-define oct-lit (seq ?0 (? (any ?o ?O)) (? ?_) (* oct-digits)))

(rx-define dec-exp (seq (any ?e ?E) (? (any ?- ?+)) dec-digits))
(rx-define dec-float-lit (or (seq dec-digits ?. (* dec-digits) (* dec-exp))
                             (seq dec-digits dec-exp)
                             (seq ?. dec-digits (* dec-exp))))

(rx-define hex-exp (seq (any ?p ?P) (? (any ?- ?+)) dec-digits))
(rx-define hex-man (seq (or (seq (? ?_) hex-digits ?. (* hex-digits))
                            (seq (? ?_) hex-digits)
                            (seq ?. hex-digits))))
(rx-define hex-float-lit (seq ?0 (any ?x ?X) hex-man hex-exp))

(rx-define river-identifier (seq (any alpha ?_) (* (any alnum ?_))))

;; Integers are parsed with strconv.ParseInt which supports a leading sign.
(rx-define river-block-header (seq bol river-identifier (* (group ?. river-identifier))))
(rx-define river-constant (or "true" "false" "null"))
(rx-define river-float (or dec-float-lit hex-float-lit))
(rx-define river-int (seq (any ?- whitespace) (or bin-lit dec-lit hex-lit oct-lit) eow))
(rx-define river-todo (seq (not ?.) (group bow (or "TODO" "FIXME" "XXX" "BUG" "NOTE") eow)))

;; TODO: introduce levels of font lock
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Levels-of-Font-Lock.html
(defconst river-font-lock-keywords
  (let ((river-block-header-regexp (rx river-block-header))
        (river-constant-regexp (rx river-constant))
        (river-float-regexp (rx river-float))
        (river-int-regexp (rx river-int))
        (river-todo-regexp (rx river-todo)))

    `((,river-block-header-regexp . (0 font-lock-variable-name-face t))
      (,river-constant-regexp . font-lock-constant-face)
      (,river-float-regexp . font-lock-constant-face)
      (,river-int-regexp . font-lock-constant-face)
      (,river-todo-regexp . (0 font-lock-warning-face t))))
  "Syntax highlighting for `river-mode'.")

(defvar river-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    ;; From: https://www.emacswiki.org/emacs/ModeTutorial
    ;; > 1) That the character '/' is the start of a two-character comment sequence ('1'),
    ;; > that it may also be the second character of a two-character comment-start sequence ('2'),
    ;; > that it is the end of a two-character comment-start sequence ('4'),
    ;; > and that comment sequences that have this character as the second character in the sequence
    ;; > is a “b-style” comment ('b').
    ;; > It’s a rule that comments that begin with a “b-style” sequence must end with either the same
    ;; > or some other “b-style” sequence.
    ;; > 2) That the character '*' is the second character of a two-character comment-start sequence ('2')
    ;; > and that it is the start of a two-character comment-end sequence ('3').
    ;; > 3) That the character '\n' (which is the newline character) ends a “b-style” comment.
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `river-mode'.")

;;;###autoload
(define-derived-mode river-mode prog-mode "River"
  "Major mode for editing River configuration language files." ()
  (kill-all-local-variables)
  (set-syntax-table river-mode-syntax-table)
  (use-local-map river-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(river-font-lock-keywords))
  (setq major-mode 'river-mode)
  (setq mode-name "River")
  (setq indent-tabs-mode t)
  (setq tab-width river-tab-width)
  (setq tab-stop-list nil)
  (setq indent-line-function 'river-indent-line)
  (run-hooks 'river-mode-hook))

;; Taken from https://github.com/dominikh/go-mode.el/blob/08aa90d52f0e7d2ad02f961b554e13329672d7cb/go-mode.el#L1852-L1894
;; Adjusted to avoid relying on cl-libs or other go-mode internal functions.
(defun river--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in river--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (1- (- from line-offset)))
                (setq line-offset (+ line-offset len))
                (dotimes (_ len)
                  (delete-region (point) (save-excursion (move-end-of-line 1) (point)))
                  (delete-char 1))))
             (t
              (error "Invalid rcs patch or internal error in river--apply-rcs-patch")))))))
    (move-to-column column)))

(defun river-format()
  "Format buffer with 'flow agent fmt'.
Formatting requires the 'agent' binary in the PATH."
  (interactive)
  (let ((tmpfile (make-nearby-temp-file
                  "agent-fmt"
                  nil
                  (or (concat "." (file-name-extension (buffer-file-name))) ".river")))
        (patchbuf (get-buffer-create "*agent fmt patch*"))
        (errbuf (get-buffer-create "*agent fmt errors*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer errbuf (setq buffer-read-only nil) (erase-buffer))

          (write-region nil nil tmpfile)

          (message "Calling 'agent fmt'")
          (if (zerop (with-environment-variables
                         (("EXPERIMENTAL_ENABLE_FLOW" "true"))
                       (apply #'process-file "agent" nil errbuf nil `("fmt" "-w" ,(file-local-name tmpfile)))))
              (progn
                (if (zerop (let ((local-copy (file-local-copy tmpfile)))
                             (unwind-protect
                                 (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" (or (or local-copy tmpfile)))
                               (when local-copy (delete-file local-copy)))))
                    (message "Buffer is already formatted")
                  (river--apply-rcs-patch patchbuf)
                  (message "Applied 'agent fmt'"))
                (kill-buffer errbuf))
            (message "Could not apply 'agent fmt'")
            (let ((filename (buffer-file-name)))
              (with-current-buffer errbuf
                (goto-char (point-min))
                (insert "'agent fmt' errors:\n")
                (while (search-forward-regexp
                        (concat "^\\(" (regexp-quote (file-local-name tmpfile))
                                "\\):")
                        nil t)
                  (replace-match (file-name-nondirectory filename) t t nil 1))
                (compilation-mode)
                (display-buffer errbuf)))))
      (kill-buffer patchbuf)
      (delete-file tmpfile))))

;; Indentation hints taken from hcl-mode: https://github.com/purcell/emacs-hcl-mode.
(defun river--block-indentation ()
  "Maintain current indentation when inside a block."
  (let ((current-line (line-number-at-pos)))
    (save-excursion
      (condition-case nil
          (progn
            (backward-up-list)
            (unless (= current-line (line-number-at-pos))
              (current-indentation)))
        (scan-error nil)))))
(defun river--previous-indentation ()
  (save-excursion
    (forward-line -1)
    (let (finish)
      (while (not finish)
        (cond ((bobp) (setq finish t))
              (t
               (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                 (if (not (string-match-p "\\`\\s-*\\'" line))
                     (setq finish t)
                   (forward-line -1))))))
      (current-indentation))))
(defun river-indent-line ()
  "Indent current line respecting block indentation."
  (interactive)
  (let* ((current-point (point))
         (pos (- (point-max) current-point)))
    (back-to-indentation)
    (let ((block-indentation (river--block-indentation)))
      (delete-region (line-beginning-position) (point))
      (if block-indentation
          (if (looking-at "[]}]")
              (indent-to block-indentation)
            (indent-to (+ block-indentation river-tab-width)))
        (indent-to (river--previous-indentation)))
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(defun river-format-before-save ()
  "Add this to .emacs to run formatting on the current buffer when saving:
\(add-hook 'before-save-hook 'river-format-before-save)."
  (interactive)
  (when (eq major-mode 'river-mode) (river-format)))

(provide 'river-mode)
;;; river-mode.el ends here
