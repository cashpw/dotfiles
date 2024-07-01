(package! day-of-week
  :recipe (:host github
           :repo "cashpw/day-of-week"))

(package! aggressive-indent)

(unless (cashpw/machine-p 'work-cloudtop)
  (package! centered-cursor-mode))

(package! command-log-mode)

(package! free-keys
  :recipe (:host github
           :repo "Fuco1/free-keys"))

(package! memoize
  :recipe (:host github
           :repo "skeeto/emacs-memoize"))

(package! operate-on-number
  :recipe (:host github
           :repo "knu/operate-on-number.el"))

(package! increment-ordinal
  :recipe (:host github
           :repo "cashpw/increment-ordinal"))

(package! titlecase)

(package! whisper
  :recipe (:host github
           :repo "natrys/whisper.el"))

(package! org-wild-notifier)

(package! scheduled-alert
  :recipe (:host github
           :repo "cashpw/scheduled-alert"))

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

;; (package! svg-tag-mode)

(package! nerd-icons)

;; (package! w3m)

(package! casual)

(package! ox-gfm)

(package! gnus-alias)

(package! org-msg)

(package! smtpmail)

(package! gnuplot)

(package! gptel)

(package! eglot-booster
  :recipe (:host github
           :repo "jdtsmith/eglot-booster"))

;; (package! flycheck-vale)

(unpin! org)
(unpin! org-contrib)

;; (package! org-link-beautify)

(package! org-extras
  :recipe (:host github
           :repo "cashpw/org-extras"))

(package! org-roam-contacts
  :recipe (:host github
           :repo "cashpw/org-roam-contacts"))

(package! citar-org-roam
  :recipe (:host github
           :repo "emacs-citar/citar-org-roam"))

(package! clocktable-by-category
  :recipe (:host github
           :repo "cashpw/clocktable-by-category"))

(package! clocktable-by-tag
  :recipe (:host github
           :repo "cashpw/clocktable-by-tag"))

(package! doct)

(package! doct-org-roam
  :recipe (:host github
           :repo "cashpw/doct-org-roam"))

(package! orgtbl-aggregate)

(package! org-download)

(package! org-fc
  :recipe (:host github
           ;; :repo "l3kn/org-fc"
           :repo "cashpw/org-fc"
           :branch "feat/classes"
           :files (:defaults "awk" "demo.org")))

(unpin! org-gcal)

(package! org-mime)

(package! org-multi-clock
  :recipe (:host gitlab
           :repo "OlMon/org-multi-clock"))

(package! org-noter
  :recipe (:host github
           :repo "cashpw/org-noter"))

(package! ol-notmuch)

(package! run-on-todo-state-change
  :recipe (:host github
           :repo "cashpw/run-on-todo-state-change"))

(package! org-protocol-capture-html
  :recipe (:host github
           :repo "alphapapa/org-protocol-capture-html"))

;; (package! org-ql)

(package! org-recipes
  :recipe (:host github
           :repo "cashpw/org-recipes"))

(unpin! org-roam)

(package! org-node
  :recipe (:host github
           :repo "meedstrom/org-node"))

(package! org-roam-ui)

;; (package! org-special-block-extras)

(package! org-super-agenda)

(package! org-superstar
  :disable t)

;; org-tempo is provided by org-mode

;; (package! org-transclusion)

(package! org-tree-slide)

(package! org-vcard)

(when (not (cashpw/machine-p 'work-cloudtop))
  (package! ox-hugo))

(package! ox-pandoc)

(package! summarize-agenda-time
  :recipe (:host github
           :repo "cashpw/summarize-agenda-time"))

(package! vulpea)

(package! deflink
  :recipe (:host github
           :repo "cashpw/deflink"))

(package! pdf-tools)

(package! protobuf-mode)

(package! toml
  :recipe (:host github
           :repo "gongo/emacs-toml"))

(package! toml
  :recipe (:host github
           :repo "gongo/emacs-toml"))

(package! electric-case
  :recipe (:host github
           :repo "zk-phi/electric-case"))

;; (package! org-window-habit
;;   :recipe (:host github
;;            :repo "colonelpanic8/org-window-habit"))
