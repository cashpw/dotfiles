(defgroup cashpw
  nil
  "Group for my customizations and configurations.")

(defun cashpw/machine-p (machine)
  "Return true if executed on my work machine."
  (pcase machine
    ('personal-phone
     (file-directory-p cashpw/path--personal-phone-home-dir))
    ('personal
     (file-directory-p cashpw/path--personal-home-dir))
    ('work
     (file-directory-p cashpw/path--work-home-dir))
    ('work-cloudtop
     (file-exists-p cashpw/path--work-cloudtop-id-file))
    ('work-laptop
     (file-exists-p cashpw/path--work-laptop-id-file))
    (_
     (cashpw/error
      "Unknown machine: %s"
      machine))))

(defvar cashpw/path--personal-home-dir
  "/home/cashpw"
  "Path to home directory on my personal machine.")

(defvar cashpw/path--work-home-dir
  "/usr/local/google/home/cashweaver"
  "Path to home directory on my work machine(s).")

(defvar cashpw/path--work-cloudtop-id-file
  "/usr/local/google/home/cashweaver/is-cloudtop"
  "File that, when present, indicates the current machine is my Cloudtop instance.")

(defvar cashpw/path--work-laptop-id-file
  "/usr/local/google/home/cashweaver/is-work-laptop"
  "File that, when present, indicates the current machine is my Cloudtop instance.")

(defvar cashpw/path--personal-phone-home-dir
  "/data/data/com.termux/files/home"
  "Path to home directory on my personal phone.")

(defvar cashpw/path--home-dir
  (cond
   ((cashpw/machine-p 'personal)
    cashpw/path--personal-home-dir)
   ((cashpw/machine-p 'personal-phone)
    cashpw/path--personal-phone-home-dir)
   ((cashpw/machine-p 'work)
    cashpw/path--work-home-dir)
   (t
    cashpw/path--personal-home-dir))
  "Path to home directory.")

(defvar cashpw/path--config-dir
  (format
   "%s/.config"
   cashpw/path--home-dir)
  "Full path to configuration files.")

(defvar cashpw/path--emacs-config-dir
  (format
   "%s/doom"
   cashpw/path--config-dir)
  "Full path to Emacs configuration files.")

(cashpw/load
 (format
  (if (cashpw/machine-p 'work-cloudtop)
      "%s/init-work.el"
    "%s/init-personal.el")
  cashpw/path--emacs-config-dir))
