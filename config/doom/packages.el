;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(setq doom-pinned-packages nil)

;; ...but to unpin a single package:
;(package! pinned-package :pin nil)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
(package! evil-surround)
(package! pipenv :disable t)

(package! docker-tramp :disable t)

(package! puppet-mode)

(package! nose :disable t)

(package! groovy-mode)

(package! fish-mode)

(package! origami-mode
  :recipe (:host github :repo "gregsexton/origami.el"))

(package! lsp-origami
  :recipe (:host github :repo "emacs-lsp/lsp-origami"))

(package! docker-compose-mode)

(package! systemd)

(package! deadgrep)

(package! clojure-snippets)

(package! gitlab-ci-mode)

(package! eruby-mode
  :recipe (:host github :repo "petere/emacs-eruby-mode"))

(package! elixir-mode
  :recipe (:host github :repo "elixir-editors/emacs-elixir"))

(package! magit-section
          :recipe (:repo "magit/magit" :host github))

(package! elvish-mode)

;; Horrible performance on macOS.
(package! company-shell :disable t)

;; https://www.masteringemacs.org/article/tree-sitter-complications-of-parsing-languages
(package! tree-sitter)
(package! tree-sitter-langs)
(package! combobulate
  :recipe (:repo "mickeynp/combobulate" :host github))

(package! dired-git-info
          :pin "91d57e3a4c5104c66a3abc18e281ee55e8979176"
          :recipe (:repo "clemera/dired-git-info" :host github))

(package! ob-http)

(package! jq-mode)

(package! coffee-mode)

(package! list-unicode-display)

(package! ox-slack)

(package! vlang-mode
  :recipe (:host github :repo "Naheel-Azawy/vlang-mode"))

(package! nix-update
  :recipe (:host github :repo "jwiegley/nix-update-el"))

(package! meson-mode)

(package! ligature)

(package! emacs-nushell :recipe (:host github :repo "azzamsa/emacs-nushell"))

(package! dhall-mode)

;;
