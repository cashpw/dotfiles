(package! day-of-week
  :recipe (:host github
           :repo "cashpw/day-of-week"))

(package! secret
  :recipe (:host github :repo "cashpw/secret.el"))

(package! aggressive-indent)

(unless
    ;; Avoid 'void-variable mouse-wheel-up-event' error
    (or (cashpw/machine-p 'work-cloudtop) (cashpw/machine-p 'personal-phone))
  (package! centered-cursor-mode))

(package! openwith)

;; (package! electric-case
;;   :recipe (:host github
;;            :repo "zk-phi/electric-case"))

(package! elmacro)

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

(package! pcache)

(package! plru
  :recipe (:host github
           :repo "cashpw/plru"))

(package! persist)

(package! isbn
  :recipe (:host github
           :repo "cashpw/isbn.el"))

(package! read-multi
  :recipe (:host github
           :repo "cashpw/read-multi"))

(package! titlecase)

(package! whisper
  :recipe (:host github
           :repo "natrys/whisper.el"))

(package! org-wild-notifier)

(package! scheduled-alert
  :recipe (:host github
           :repo "cashpw/scheduled-alert"))

;; (package! sunshine)

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

(package! asana
  ;; :recipe (:host github :repo "lmartel/emacs-asana"))
  :recipe (:host github :repo "cashpw/asana.el"))

;; (package! go
;;   :recipe (:host github
;;            :repo "cashpw/el-go"
;;            :branch "feat/missing-backends"))

(package! search-engine
  :recipe (:host github
           :repo "cashpw/search-engine.el"))

;; (package! w3m)

(package! nov)

;; (package! org-remark)

(package! casual)

(package! ox-gfm)

(package! gnus-alias)

(package! org-mime)

(package! org-msg)

(package! elfeed-protocol)

(package! elfeed-export
  :recipe (:host codeberg :repo "bram85/elfeed-export"))

(package! gnuplot)

(unpin! gptel)
(unpin! gptel-quick)

(package! commit-message
  :recipe (:host github :repo "cashpw/commit-message"))

(package! speed-type)

(package! eglot-booster
  :recipe (:host github
           :repo "jdtsmith/eglot-booster"))

;; (package! flycheck-vale)

;; (package! treesit-auto)

(package! elisp-autofmt)

(unpin! org)
(unpin! org-contrib)

(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

(package! zotra)

(package! org-clock-act-on-overtime
  :recipe (:host github
           :repo "cashpw/org-clock-act-on-overtime"))

;; (package! org-link-beautify
;;   :recipe (:host nil
;;            :repo "https://repo.or.cz/org-link-beautify.git"
;;            :branch "v1.2.3"))

(package! org-extras
  :recipe (:host github
           :repo "cashpw/org-extras"))

(package! org-roam-contacts
  :recipe (:host github
           :repo "cashpw/org-roam-contacts"))

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
           :repo "l3kn/org-fc"
           ;; :repo "cashpw/org-fc"
           ;; :branch "feat/classes"
           :files (:defaults "awk" "python")
           ))

(package! org-gallery
  :recipe (:host github
           :repo "cashpw/org-gallery"))

(package! org-habit-stats)

(package! repeat-todo
  :recipe (:host github
           :repo "cashpw/repeat-todo"))

(unpin! org-gcal)

(package! org-gcal-extras
  :recipe (:host github
           :repo "cashpw/org-gcal-extras"))

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

(package! org-ql)

(package! org-recipes
  :recipe (:host github
           :repo "cashpw/org-recipes"))

(unpin! org-roam)

;; (package! org-roam-bibtex)

(package! org-daily-reflection)

(package! org-mem)

(package! org-node
  :recipe (:host github
           :repo "meedstrom/org-node"))

(package! org-roam-ui)

(package! org-defblock
  :recipe (:host github :repo "cashpw/org-defblock"))

(package! org-super-agenda)

(package! org-superstar
  :disable t)

;; org-tempo is provided by org-mode

(package! org-transclusion)

(package! org-tree-slide)

(package! org-vcard)

(when (not (cashpw/machine-p 'work-cloudtop))
  (package! ox-hugo))

(package! ox-pandoc)

(package! summarize-agenda-time
  :recipe (:host github
           :repo "cashpw/summarize-agenda-time"))

(package! vulpea)

(package! org-modern)

(package! deflink
  :recipe (:host github
           :repo "cashpw/deflink"))

(package! protobuf-mode)

(package! toml
  :recipe (:host github
           :repo "gongo/emacs-toml"))

;; (package! emacs-rag
;;   :recipe (:host github
;;            :repo "jkitchin/emacs-rag-libsql" :files ("emacs-rag/*.el")))

(package! pomm)

(package! toml
  :recipe (:host github
           :repo "gongo/emacs-toml"))

;; (package! org-window-habit
;;   :recipe (:host github
;;            :repo "colonelpanic8/org-window-habit"))

(package! font-lock-profiler
  :recipe (:host github
           :repo "Lindydancer/font-lock-profiler"))
