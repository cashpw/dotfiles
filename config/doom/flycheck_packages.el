(defvar cashpw/path--personal-home-dir
  "/home/cashweaver"
  "Path to home directory on my personal machine.")

(defvar cashpw/path--work-home-dir
  "/usr/local/google/home/cashweaver"
  "Path to home directory on my work machine(s).")

(defvar cashpw/path--work-cloudtop-id-file
  "/usr/local/google/home/cashweaver/is-cloudtop"
  "File that, when present, indicates the current machine is my Cloudtop instance.")

(defun cashpw/is-work-p ()
  "Return true if executed on my work machine."
  (file-directory-p
   cashpw/path--work-home-dir))

(defun cashpw/is-work-cloudtop-p ()
  "Return true if executed on my work machine."
  (file-exists-p
   cashpw/path--work-cloudtop-id-file))

(defcustom cashpw/path--home-dir
  (cond
   ((cashpw/is-work-p)
    cashpw/path--work-home-dir)
   (t
    cashpw/path--personal-home-dir))
  "Path to home directory.")

(use-package! s)
(require 's)
(defcustom cashpw/path--config-dir
  (s-lex-format
   "${cashpw/path--home-dir}/.config")
  "Full path to configuration files.")

(defcustom cashpw/path--emacs-config-dir
  (s-lex-format
   "${cashpw/path--config-dir}/doom")
  "Full path to Emacs configuration files.")

(defvar cashpw/path--third-party-dir
  (if (cashpw/is-work-p)
      "/usr/local/google/home/cashweaver/third_party"
    "/home/cashweaver/third_party")
  "Path to third-party files.")

(load (s-lex-format
       "${cashpw/path--emacs-config-dir}/packages-personal.el"))

(when (cashpw/is-work-cloudtop-p)
  (load (s-lex-format
         "${cashpw/path--emacs-config-dir}/packages-work.el")))
