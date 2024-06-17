(defgroup cashpw
  nil
  "Group for my customizations and configurations.")

(defun cashpw/error (error-message &rest args)
  (error
   "[cashpw] %s"
   (apply
    #'format
    error-message
    args)))

(defun cashpw/load (path &optional)
  "Return non-nil after loading PATH."
  (cashpw/message
   "Loading %s ..."
   path)
  (cashpw/message
   "Loaded %s in %.06f seconds."
   path
   (k-time
    (load path)))
  t)
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY.

https://akrl.sdf.org/#orgc15a10d"
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun cashpw/machine-p (machine)
  "Return true if executed on my work machine."
  (pcase machine
    ('personal
     (file-directory-p cashpw/path--personal-home-dir))
    ('work
     (file-directory-p cashpw/path--work-home-dir))
    ('work-cloudtop
     (file-exists-p cashpw/path--work-cloudtop-id-file))
    ('work-laptop
     (file-exists-p cashpw/path--work-laptop-id-file))
    (t
     (cashpw/error
      "Unknown machine: %s"
      machine))))

(defvar cashpw/path--personal-home-dir
  "/home/cashweaver"
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

(defvar cashpw/path--home-dir
  (cond
   ((cashpw/machine-p 'personal)
    cashpw/path--personal-home-dir)
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

(defcustom
  cashpw/personal-config-loaded-p
  nil
  "Non-nil if my config has finished loading."
  :group 'cashpw
  :type 'boolean)
(setq
 cashpw/personal-config-loaded-p (cashpw/load
                                  (format
                                   "%s/config-personal.el"
                                   cashpw/path--emacs-config-dir)))

(when (cashpw/machine-p 'work-cloudtop)
  (defcustom
    cashpw/work-config-loaded-p
    nil
    "Non-nil if my config has finished loading."
    :group 'cashpw
    :type 'boolean)
  (setq
   cashpw/work-config-loaded-p (cashpw/load
                                (format
                                 "%s/config-work.el"
                                 cashpw/path--emacs-config-dir))))
