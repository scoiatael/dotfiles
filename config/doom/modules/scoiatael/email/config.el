;;; modules/scoiatael/email/config.el -*- lexical-binding: t; -*-

(after! notmuch
  ;; https://github.com/doomemacs/doomemacs/issues/4585
  (set-popup-rule! "^\\*notmuch-hello" :ignore t)
  ;; https://protesilaos.com/emacs/dotemacs#h:2c9a555f-d3ec-4d0e-bad6-6c26b576a49a
  (setq notmuch-show-logo t
        notmuch-column-control 1.0
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-thousands-separator ""
        notmuch-hello-sections '(notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)
        notmuch-show-all-tags-list t))