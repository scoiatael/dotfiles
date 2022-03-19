;;; evil-colemak-dh-mode.el --- Colemak-Dh layout for evil-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Lukasz Czaplinski
;; based on
;; https://raw.githubusercontent.com/chrisbarrett/evil-workman-mode/master/evil-workman-mode.el
;; Copyright (C) 2016 Chris Barrett

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Colemak-Dh layout for evil-mode. Enable by calling `evil-colemak-dh-global-mode'.

;;; Code:

(require 'evil)

(defvar evil-colemak-dh-mode-map
  (make-sparse-keymap))

(defun evil-colemak-dh-mode--swap-evil-keys (a b)
  (let ((x (or (lookup-key evil-motion-state-map a)
               (lookup-key evil-normal-state-map a)))
        (y (or (lookup-key evil-motion-state-map b)
               (lookup-key evil-normal-state-map b))))
    (eval `(dolist (state '(normal motion visual))
             (evil-define-key state evil-colemak-dh-mode-map (kbd ,a) #',y)
             (evil-define-key state evil-colemak-dh-mode-map (kbd ,b) #',x)))
    (cons x y)))

;;;###autoload
(define-minor-mode evil-colemak-dh-mode
  "Enable keybindings that are more ergonomic for colemak-dh."
  :keymap evil-colemak-dh-mode-map
  :lighter " Colemak-Dh"
  (evil-colemak-dh-mode--swap-evil-keys "l" "i")
  (evil-colemak-dh-mode--swap-evil-keys "L" "I")
  (evil-colemak-dh-mode--swap-evil-keys "j" "n")
  (evil-colemak-dh-mode--swap-evil-keys "J" "N")
  (evil-colemak-dh-mode--swap-evil-keys "k" "e")
  (evil-colemak-dh-mode--swap-evil-keys "K" "E")
  (evil-colemak-dh-mode--swap-evil-keys "h" "m")
  (evil-colemak-dh-mode--swap-evil-keys "H" "M"))

;;;###autoload
(defun turn-on-evil-colemak-dh-mode (&rest _)
  (evil-colemak-dh-mode +1))

;;;###autoload
(define-globalized-minor-mode evil-colemak-dh-global-mode evil-colemak-dh-mode turn-on-evil-colemak-dh-mode)

(provide 'evil-colemak-dh-mode)

;;; evil-colemak-dh-mode.el ends here
