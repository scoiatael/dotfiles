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
    :recipe (:repo "mickeynp/combobulate" :host github) :pin "2aef9d4"))

(package! modus-themes :recipe (:host sourcehut :repo "protesilaos/modus-themes") :pin "cfc8c4ee")

(package! moody :recipe (:host github :repo "tarsius/moody") :pin "249c5f1")

(package! keycast :recipe (:host github :repo "tarsius/keycast") :pin "cabb3fa")

(package! devdocs :recipe (:host github :repo "astoff/devdocs.el") :pin "2988d4d")

(package! doom-modeline :recipe (:host github :repo "seagle0128/doom-modeline") :pin "ed72a56")

(package! spaceline :recipe (:host github :repo "TheBB/spaceline") :pin "e0f848c")

(package! marginalia :recipe (:host github :repo "minad/marginalia") :pin "3ddd2b7")
(package! mini-frame :recipe (:host github :repo "muffinmad/emacs-mini-frame") :pin "60838f3")

(package! ellama :recipe (:host github :repo "s-kostyaev/ellama") :pin "b8ef883")
(package! llm :recipe (:host github :repo "ahyatt/llm") :pin "4c0ff74")

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
