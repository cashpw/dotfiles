;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; DO NOT EDIT THIS FILE MANUALLY.
;; This file is generated from doom.md. You should make your changes there and
;; this file using org-babel-tangle.

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
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
;; our package manager can't deal with; see raxod502/straight.el#279)
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

(setq
 cashweaver-home-dir-home
 "/home/cashweaver"
 cashweaver-home-dir-work
 "/usr/local/google/home/cashweaver")

(setq
 cashweaver-work-config-dir
 (format
  "%s/%s"
  cashweaver-home-dir-work
  ".config/doom"
  ))
(defun cashweaver-is-work-p ()
  "Return true if executed on my work machine."
  (file-directory-p cashweaver-work-config-dir))

(defvar
  cashweaver--local-package-path
  (if (cashweaver-is-work-p)
      "/usr/local/google/home/cashweaver/third_party"
    "/home/cashweaver/third_party")
  "Path to local emacs package files.")

(package! aggressive-indent)

(package! doct)

(package! free-keys
  :recipe (:host github
           :repo "Fuco1/free-keys"))

(package! gnus-alias)

(package! operate-on-number
  :recipe (:host github
           :repo "knu/operate-on-number.el"))

(package! org-gcal)

(package! org-mime)

(package! org-noter
  :recipe (:host github
           :repo "cashweaver/org-noter"))

(package! ol-notmuch
  :recipe `(:local-repo
            ,(concat
              cashweaver--local-package-path
              "/org-mode/contrib/lisp")
            :files
            ("ol-notmuch.el")))

(package! org-super-agenda)

(package! ox-pandoc)

(setq
 cashweaver-work-config-dir
 (format
  "%s/%s"
  cashweaver-home-dir-work
  ".config/doom"
  ))
(defun cashweaver-is-work-p ()
  "Return true if executed on my work machine."
  (file-directory-p cashweaver-work-config-dir))

(if (cashweaver-is-work-p)
    (load (concat cashweaver-work-config-dir "/packages-work.el")))
