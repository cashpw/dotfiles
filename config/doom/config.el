(defvar cashpw/path--personal-home-dir
  "/home/cashweaver"
  "Path to home directory on my personal machine.")

(defvar cashpw/path--work-home-dir
  "/usr/local/google/home/cashweaver"
  "Path to home directory on my work machine(s).")

(defvar cashpw/path--work-cloudtop-id-file
  "/usr/local/google/home/cashweaver/is-cloudtop"
  "File that, when present, indicates the current machine is my Cloudtop instance.")

(defun cashpw/is-personal-p ()
  "Return true if executed on my work machine."
  (file-directory-p
   cashpw/path--personal-home-dir))

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
   ((cashpw/is-personal-p)
    cashpw/path--personal-home-dir)
   ((cashpw/is-work-p)
    cashpw/path--work-home-dir)
   (t
    cashpw/path--personal-home-dir))
  "Path to home directory.")

(defcustom cashpw/path--config-dir
  (format "%s/.config" cashpw/path--home-dir)
  "Full path to configuration files.")

(defcustom cashpw/path--emacs-config-dir
  (format "%s/doom" cashpw/path--config-dir)
  "Full path to Emacs configuration files.")

(load (format "%s/config-personal.el" cashpw/path--emacs-config-dir))
(when (cashpw/is-work-cloudtop-p)
  (load (format "%s/config-work.el" cashpw/path--emacs-config-dir)))
