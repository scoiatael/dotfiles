;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

(when EMACS29+
  (package! combobulate
    :recipe (:repo "mickeynp/combobulate" :host github)))

;; (package! modus-themes :recipe (:host sourcehut :repo "protesilaos/modus-themes"))

(package! moody :recipe (:host github :repo "tarsius/moody"))

(package! keycast :recipe (:host github :repo "tarsius/keycast"))

(package! devdocs :recipe (:host github :repo "astoff/devdocs.el"))

(package! doom-modeline :recipe (:host github :repo "seagle0128/doom-modeline"))

(package! spaceline :recipe (:host github :repo "TheBB/spaceline"))

(package! marginalia :recipe (:host github :repo "minad/marginalia"))
(package! mini-frame :recipe (:host github :repo "muffinmad/emacs-mini-frame"))

(package! ellama :recipe (:host github :repo "s-kostyaev/ellama"))
(package! llm :recipe (:host github :repo "ahyatt/llm"))

(package! gleam-ts-mode :recipe (:host github :repo "gleam-lang/gleam-mode"))

;; https://github.com/doomemacs/doomemacs/issues/8089
(package! compat :pin "9a234d0")

(package! gptel :pin "9245dfd")

(package! river-mode
  :type 'local
  :recipe (:local-repo "packages"))

;; Should be installed via Nix
(package! vterm :ignore t)

(package! jtsx :recipe (:host github :repo "llemaitre19/jtsx"))

(package! ultra-scroll :recipe (:host github :repo "jdtsmith/ultra-scroll"))

(package! reason-mode :recipe (:host github :repo "reasonml-editor/reason-mode"))
(package! rescript-mode :recipe (:host github :repo "jjlee/rescript-mode"))

(package! justl :recipe (:host github :repo "psibi/justl.el"))

(package! eldoc-box :recipe (:host github :repo "casouri/eldoc-box"))

(package! ob-graphql :recipe (:host github :repo "jdormit/ob-graphql"))
(package! ob-async :recipe (:host github :repo "astahlman/ob-async"))

(package! org-modern :recipe (:host github :repo "minad/org-modern"))

(package! prisma-mode :recipe (:host github :repo "davidarenas/prisma-mode"))
(package! org-reverse-datetree :recipe (:host github :repo "akirak/org-reverse-datetree"))
;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
