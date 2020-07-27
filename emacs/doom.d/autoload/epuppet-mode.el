;;; epuppet-mode.el --- minor mode for epuppet (.epp) template files

;; Copyright (C) 2020 Łukasz Czapliński
;; Based on https://github.com/petere/emacs-eruby-mode

;;; Code:

(defgroup epuppet-mode nil
  "Mode for epuppet template files"
  :group 'languages)

(defgroup epuppet-mode-faces nil
  "Faces for highlighting epuppet template files"
  :prefix "epuppet-mode-"
  :group 'epuppet-mode-
  :group 'faces)

(defface epuppet-standard-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face used to highlight epuppet template snippets"
  :group 'epuppet-mode-faces)

(defface epuppet-comment-face
  '((t (:inherit font-lock-comment-face :background-color "honeydew")))
  "Face used to highlight epuppet template snippets"
  :group 'epuppet-mode-faces)

(defvar epuppet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "%" 'epuppet-mode-electric-percent)
    map)
    "Keymap for epuppet mode.")

(defun epuppet-mode-electric-percent ()
  "Called when % is pressed."
  (interactive)
  (if (and electric-pair-mode
           (equal (char-before) ?<))
      (progn
        (insert "% %>")
        (backward-char 3))
    (insert "%")))

(defvar epuppet-mode-font-lock-keywords
  '(("<%.*?%>" . '(0 'epuppet-standard-face t))
    ("<%#.*?%>" . '(0 'epuppet-comment-face t))))

;;;###autoload
(define-minor-mode epuppet-mode
  "Minor mode for epuppet templates"
  :lighter "EPP"
  :keymap epuppet-mode-map
  (if epuppet-mode
      (font-lock-add-keywords nil epuppet-mode-font-lock-keywords)
    (font-lock-remove-keywords nil epuppet-mode-font-lock-keywords)))

;;;###autoload
(defconst epuppet-mode-file-regexp "\\.epp\\'")

;;;###autoload
(add-to-list 'auto-mode-alist `(,epuppet-mode-file-regexp ignore t))

;; https://stackoverflow.com/questions/13945782/emacs-auto-minor-mode-based-on-extension
;;;###autoload
(defun epuppet-mode-auto-mode ()
  "Turn on epuppet mode for appropriate file extensions."
  (when buffer-file-name
    (when (string-match epuppet-mode-file-regexp buffer-file-name)
      (epuppet-mode 1))))

;;;###autoload
(add-hook 'find-file-hook #'epuppet-mode-auto-mode)

;;; epuppet-mode.el ends here
