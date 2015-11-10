;;; packages.el --- seeing-is-believing Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq seeing-is-believing-packages
    '(
      seeing-is-believing
      ))

;; List of packages to exclude.
(setq seeing-is-believing-excluded-packages '())

;; For each package, define a function seeing-is-believing/init-<package-name>
;;
(defun seeing-is-believing/init-seeing-is-believing ()
  "Initialize my package"
  (use-package seeing-is-believing
    :init
    (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
      (add-hook hook 'seeing-is-believing))
    :diminish seeing-is-believing)
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
