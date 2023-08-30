(require 's)

(defvar cashpw/home-dir-path-personal
  "/home/cashweaver"
  "Path to home directory on my personal machine.")

(defvar cashpw/home-dir-path-work
  "/usr/local/google/home/cashweaver"
  "Path to home directory on my work machine(s).")

(defvar cashpw/home-dir-path-work-cloudtop
  "/usr/local/google/home/cashweaver/is-cloudtop"
  "File that, when present, indicates the current machine is my Cloudtop instance.")

(defun cashpw/is-work-p ()
  "Return true if executed on my work machine."
  (file-directory-p
   cashpw/home-dir-path-work))

(defun cashpw/is-work-cloudtop-p ()
  "Return true if executed on my work machine."
  (file-exists-p
   cashpw/home-dir-path-work-cloudtop))

(defvar cashpw/home-dir-path
  (if (cashpw/is-work-cloudtop-p)
      cashpw/home-dir-path-work
    cashpw/home-dir-path-personal)
  "Path to home directory.")

(defvar cashpw/config-dir-path
  (s-lex-format
   "${cashpw/home-dir-path}/.config")
  "Full path to configuration files.")

(defvar cashpw/emacs-config-dir-path
  (s-lex-format
   "${cashpw/config-dir-path}/doom")
  "Full path to Emacs configuration files.")

(defvar cashpw/third-party-path
  (if (cashpw/is-work-p)
      "/usr/local/google/home/cashweaver/third_party"
    "/home/cashweaver/third_party")
  "Path to third-party files.")

(load (s-lex-format
       "${cashpw/emacs-config-dir-path}/packages-personal.el"))

(when (cashpw/is-work-cloudtop-p)
  (load (s-lex-format
         "${cashpw/emacs-config-dir-path}/packages-work.el")))
