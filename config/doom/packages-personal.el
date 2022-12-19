(package! command-log-mode)

(package! free-keys
  :recipe (:host github
           :repo "Fuco1/free-keys"))

(package! titlecase)

(package! org-wild-notifier)

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

(package! svg-tag-mode)

(package! gnus-alias)

(package! org-msg)

(package! aggressive-indent)

(package! operate-on-number
  :recipe (:host github
           :repo "knu/operate-on-number.el"))

(package! anki-editor
  :recipe (:host github
           :repo "cashweaver/anki-editor"))

(package! citar-org-roam
  :recipe (:host github
           :repo "emacs-citar/citar-org-roam"))

(package! doct)

(package! ol-doi
  :recipe (:repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
           :branch "main"
           :files ("lisp/ol-doi.el")))

(package! orgtbl-aggregate)

(package! org-download)

(package! org-fc
  :recipe (:host github
           ;; :repo "l3kn/org-fc"
           :repo "cashweaver/org-fc"
           :branch "develop"
           :files (:defaults "awk" "demo.org")))

(package! org-gcal
  :recipe (:host github
           :repo "kidd/org-gcal.el"))

(package! org-gtasks
  :recipe (:host github
           :repo "JulienMasson/org-gtasks"))

(package! org-mime)

(package! org-noter
  :recipe (:host github
           :repo "cashweaver/org-noter"))

(package! ol-notmuch)

;;(package! org-protocol)

(package! org-protocol-capture-html
  :recipe (:host github
           :repo "alphapapa/org-protocol-capture-html"))

(package! org-ql)

(package! vulpea)

(unpin! org-roam)

(package! doct-org-roam
  :recipe (:host github
           :repo "cashpw/doct-org-roam"))

(package! org-roam-ui)

(package! org-super-agenda)

(package! org-transclusion)

(package! org-vcard)

(when (not (cashweaver/is-work-cloudtop-p))
  (package! ox-hugo))

(package! ox-pandoc)

(package! org-link-base
  :recipe (:host github
           :repo "cashweaver/org-link-base"))

(package! org-link-isbn
  :recipe (:host github
           :repo "cashweaver/org-link-isbn"))

(package! org-link-instagram
  :recipe (:host github
           :repo "cashweaver/org-link-instagram"))

(package! org-link-twitter
  :recipe (:host github
           :repo "cashweaver/org-link-twitter"))

(package! org-link-google-doc
  :recipe (:host github
           :repo "cashweaver/org-link-google-doc"))

(package! org-link-google-sheet
  :recipe (:host github
           :repo "cashweaver/org-link-google-sheet"))

(package! pdf-tools)

(package! toml
  :recipe (:host github
           :repo "gongo/emacs-toml"))

(package! toml
  :recipe (:host github
           :repo "gongo/emacs-toml"))

(package! electric-case
  :recipe (:host github
           :repo "zk-phi/electric-case"))

(package! org-capture-ref
  :recipe (:host github
           :repo "yantar92/org-capture-ref"))

(package! asoc
  :recipe (:host github
           :repo "troyp/asoc.el"))

(package! memoize
  :recipe (:host github
           :repo "skeeto/emacs-memoize"))
