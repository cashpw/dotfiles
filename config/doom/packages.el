(defvar
  cashweaver-home-dir-path-personal
  "/home/cashweaver"
  "Path to home directory on my personal machine.")

(defvar
  cashweaver-home-dir-path-work
  "/usr/local/google/home/cashweaver"
  "Path to home directory on my work machine(s).")

(defun cashweaver-is-work-p ()
  "Return true if executed on my work machine."
  (file-directory-p
   cashweaver-home-dir-path-work))

(defvar
  cashweaver-home-dir-path
  (if (cashweaver-is-work-p)
      cashweaver-home-dir-path-work
    cashweaver-home-dir-path-personal)
  "Path to home directory.")

(defvar
  cashweaver-config-dir-path
  (format
   "%s/%s"
   cashweaver-home-dir-path
   ".config")
  "Full path to configuration files.")

(defvar
  cashweaver-emacs-config-dir-path
  (format
   "%s/%s"
   cashweaver-config-dir-path
   "doom")
  "Full path to Emacs configuration files.")

(load (concat cashweaver-emacs-config-dir-path "/packages-personal.el"))

(when (cashweaver-is-work-p)
  (load
   (format "%s/packages-work.el"
           cashweaver-emacs-config-dir-path)))
