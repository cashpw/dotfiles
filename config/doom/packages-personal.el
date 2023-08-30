(package! command-log-mode)

(package! centered-cursor-mode)

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

(package! doom-modeline :pin "918730eff72e")

;; (package! svg-tag-mode)

(package! nerd-icons)

(package! gnus-alias)

(package! org-msg)

(package! aggressive-indent)

(package! operate-on-number
  :recipe (:host github
           :repo "knu/operate-on-number.el"))

;; Pin org to 9.6.1 to fix https://github.com/kaushalmodi/ox-hugo/issues/722
;; Reference: https://github.com/doomemacs/doomemacs/commit/c79f55f7760b09d0633dddfcc01cd6e0ea47ef45
(package! org
  :recipe (:host github
           ;; REVIEW: I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "emacs-straight/org-mode"
           :files (:defaults "etc")
           :depth 1
           ;; HACK: Org has a post-install step that generates org-version.el
           ;;   and org-loaddefs.el, but Straight doesn't invoke this step, and
           ;;   the former doesn't work if the Org repo is a shallow clone.
           ;;   Rather than impose the network burden of a full clone (and other
           ;;   redundant work in Org's makefile), I'd rather fake these files
           ;;   instead. Besides, Straight already produces a org-autoloads.el,
           ;;   so org-loaddefs.el isn't needed.
           :build t
           :pre-build
           (progn
             (with-temp-file "org-loaddefs.el")
             (with-temp-file "org-version.el"
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents (doom-path "lisp/org.el") nil 0 1024)
                        (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                            (match-string-no-properties 1)
                          "Unknown"))))
                 (insert (format "(defun org-release () %S)\n" version)
                         (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                                 version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                         "(provide 'org-version)\n")))))
  :pin "fe92a3cedba541482d5954eacb2b13e6f57a39c4")
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :pin "fff6c888065588527b1c1d7dd7e41c29ef767e17")

(package! citar-org-roam
  :recipe (:host github
           :repo "emacs-citar/citar-org-roam"))

(package! doct)

(package! ol-doi
  :recipe (:repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
           :branch "main"
           :files ("lisp/ol-doi.el")))

(package! orgtbl-aggregate)

(package! org-ai)

(package! org-download)

(package! org-fc
  :recipe (:host github
           ;; :repo "l3kn/org-fc"
           :repo "cashpw/org-fc"
           :branch "feat/classes"
           :files (:defaults "awk" "demo.org")))

;; org-tempo is provided by org-mode

(unpin! org-gcal)

;; (package! org-gtasks
;;   :recipe (:host github
;;            :repo "JulienMasson/org-gtasks"))

(package! org-mime)

(package! org-noter
  :recipe (:host github
           :repo "cashpw/org-noter"))

(package! ol-notmuch)

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

(package! org-superstar
  :disable t)

(package! org-super-agenda)

(package! org-transclusion)

(package! org-vcard)

(when (not (cashpw/is-work-cloudtop-p))
  (package! ox-hugo))

(package! ox-pandoc)

(after! org
  (setq
   ;; Prefer IDs to filenames+headers when creating links.
   ;; Headers can change, filenames can change, the IDs won't change
   ;; and can move to follow the relevant content.
   org-id-link-to-org-use-id t))

(package! org-link-base
  :recipe (:host github
           :repo "cashpw/org-link-base"))

(package! org-link-isbn
  :recipe (:host github
           :repo "cashpw/org-link-isbn"))

(package! org-link-instagram
  :recipe (:host github
           :repo "cashpw/org-link-instagram"))

(package! org-link-twitter
  :recipe (:host github
           :repo "cashpw/org-link-twitter"))

(package! org-link-google-doc
  :recipe (:host github
           :repo "cashpw/org-link-google-doc"))

(package! org-link-google-sheet
  :recipe (:host github
           :repo "cashpw/org-link-google-sheet"))

(package! org-link-amazon
  :recipe (:host github
           :repo "cashpw/org-link-amazon"))

(package! org-link-reddit
  :recipe (:host github
           :repo "cashpw/org-link-reddit"))

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
