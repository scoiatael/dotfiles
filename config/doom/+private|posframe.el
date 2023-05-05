;;; ../../dotfiles/config/doom/+private|posframe.el -*- lexical-binding: t; -*-

;; Nicolas .P Rougier emacs configuration - mini-frame configuration
;; ---------------------------------------------------------------------
(require 'vertico)
(require 'marginalia)
(require 'mini-frame)

(defun minibuffer-setup ()
  ;; This prevents the header line to spill over second line
  (let ((inhibit-message t))
    (toggle-truncate-lines 1))

  (setq enable-recursive-minibuffers t)

  (cursor-intangible-mode)
  (face-remap-add-relative 'default :foreground "black")
  (face-remap-add-relative 'completions-first-difference :foreground "black")
  (let* ((left  (concat (propertize " "
                                    'display '(raise +0.20))
                        (propertize " Minibuffer")
                        (propertize " "
                                    'display '(raise -0.30))))
         (right (propertize "C-g: abort"
                            'face '(:weight light)))
         (spacer (propertize (make-string (- (window-width)
                                             (length left)
                                             (length right)
                                             1) ?\ )))
         (header (concat left spacer right " "))
         (overlay (make-overlay (+ (point-min) 0) (+ (point-min) 0))))
    (overlay-put overlay 'before-string
        (concat
         (propertize " " 'display header)
         "\n"
         ;; This provides a vertical gap (half a line) above the prompt.
         (propertize " " 'face `(:extend t)
                     'display '(raise .33)
                     'read-only t 'cursor-intangible t)))))

 ;; (add-hook 'minibuffer-setup-hook #'minibuffer-setup)


;; (defun minibuffer-exit ())
;; (add-hook 'minibuffer-exit-hook #'minibuffer-exit)

;; Prefix/Affix the current candidate. From
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
(defun minibuffer-format-candidate (orig cand prefix suffix index _start)
  (let ((prefix (if (= vertico--index index)
                    "  " "   ")))
    (funcall orig cand prefix suffix index _start)))

(advice-add #'vertico--format-candidate
            :around #'minibuffer-format-candidate)

(with-eval-after-load 'marginalia
  (setq truncate-string-ellipsis "…")
  (setq marginalia--ellipsis "…")
  (setq marginalia-align 'right)
  (setq marginalia-align-offset -1))


(with-eval-after-load 'mini-frame
  (setq mini-frame-default-height vertico-count)
  (setq mini-frame-create-lazy t)
  (setq mini-frame-show-parameters 'mini-frame-dynamic-parameters)
  (setq mini-frame-ignore-commands
        '("edebug-eval-expression" debugger-eval-expression))
  ;; (setq mini-frame-resize 'grow-only) ;; -> buggy as of 01/05/2021
  ;; (setq mini-frame-resize 'not-set)
  ;; (setq mini-frame-resize nil)
  (setq mini-frame-resize t)
  (setq mini-frame-resize-min-height 3)

  (defun mini-frame-dynamic-parameters ()
    (let* ((edges       (window-pixel-edges))      ;; (left top right bottom)
           (body-edges  (window-body-pixel-edges)) ;; (left top right bottom)
           (left   (nth 0 edges))      ;; Take margins into account
           (top    (nth 1 edges)) ;; Drop header line
           (right  (nth 2 edges))      ;; Take margins into account
           (bottom (nth 3 body-edges)) ;; Drop header line
           (left   (if (or (eq nil left-fringe-width) (eq left-fringe-width 0))
                       left
                     (- left (frame-parameter nil 'left-fringe))))
           (right  (nth 2 edges))
           (right  (if (or (eq nil right-fringe-width) (eq right-fringe-width 0))
                       right
                     (+ right (frame-parameter nil 'right-fringe))))
           (fringe-left 0)
           (fringe-right 0)
           (border 1)
           (fringe 20)
           ;; (width (- (frame-pixel-width) (* 2 (+ fringe border))))
           (width (- right left fringe-left fringe-right (* 2 border) (* 2 fringe)))
           (y (+ (/ (+ top bottom) 2) border)))
    `((left . ,(+ left border fringe))
      (top . ,y)
      (alpha . 1.0)
      (width . (text-pixels . ,width))
      (left-fringe . ,fringe-left)
      (right-fringe . ,fringe-right)
      (child-frame-border-width . ,border)
      (internal-border-width . ,border)))))

(vertico-mode)
(marginalia-mode)
(mini-frame-mode t)

(provide '+private|posframe)
