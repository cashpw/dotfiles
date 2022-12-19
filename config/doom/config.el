(require 's)

(defvar cashweaver/home-dir-path-personal
  "/home/cashweaver"
  "Path to home directory on my personal machine.")

(defvar cashweaver/home-dir-path-work
  "/home/cashweaver"
  "Path to home directory on my work machine(s).")

(defvar cashweaver/home-dir-path-work-cloudtop
  "/usr/local/google/home/cashweaver/is-cloudtop"
  "File that, when present, indicates the current machine is my Cloudtop instance.")

(defun cashweaver/is-work-p ()
  "Return true if executed on my work machine."
  (file-directory-p
   cashweaver/home-dir-path-work))

(defun cashweaver/is-work-cloudtop-p ()
  "Return true if executed on my work machine."
  (file-exists-p
   cashweaver/home-dir-path-work-cloudtop))

(defvar cashweaver/home-dir-path
  (if (cashweaver/is-work-p)
      cashweaver/home-dir-path-work
    cashweaver/home-dir-path-personal)
  "Path to home directory.")

(defvar cashweaver/config-dir-path
  (s-lex-format
   "${cashweaver/home-dir-path}/.config")
  "Full path to configuration files.")

(defvar cashweaver/emacs-config-dir-path
  (s-lex-format
   "${cashweaver/config-dir-path}/doom")
  "Full path to Emacs configuration files.")

(load (s-lex-format
       "${cashweaver/emacs-config-dir-path}/config-personal.el"))

(when (cashweaver/is-work-cloudtop-p)
  (load (s-lex-format
         "${cashweaver/emacs-config-dir-path}/config-work.el")))
