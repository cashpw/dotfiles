(defgroup cashpw
  nil
  "Group for my customizations and configurations.")

(defcustom cashpw/debug nil
  "Non-nil if my personal debug mode is active.")

(cl-defun
    cashpw/log--internal
    (format-string state args)
  "Call `message' with FORMAT-STRING and ARGS."
  (message
   (apply #'format
          (append
           (list
            (format "[%s %s] %s"
                    state
                    (format-time-string "%F %T%Z")
                    format-string))
           args))))

(cl-defun
    cashpw/log-info
    (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS."
  (cashpw/log--internal format-string "INFO" args))

(cl-defun
    cashpw/log
    (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS."
  (apply #'cashpw/log-info (append (list format-string) args)))

(cl-defun
    cashpw/log-debug
    (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS."
  (when cashpw/debug
    (cashpw/log--internal format-string "DEBUG" args)))
(defun cashpw/error (error-message &rest args)
  (error
   "[cashpw %s] %s"
   (apply
    #'format
    error-message
    args)))
(defun cashpw/load (path)
  "Return non-nil after loading PATH."
  (cashpw/log
   "Loading %s ..."
   path)
  (cashpw/log
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
    ('personal-phone
     (file-exists-p cashpw/path--personal-phone-id-file))
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

(defvar cashpw/path--personal-phone-id-file
  "/data/data/com.termux/files/home/is-phone"
  "File that, when present, indicates the current machine is my personal phone.")

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

(defcustom
  cashpw/personal-packages-loaded-p
  nil
  "Non-nil if my packages have finished loading."
  :group 'cashpw
  :type 'boolean)
(defvar cashpw/personal-packages-loaded-hooks '()
  "Hooks to run after we finish loading personal packages.")
(setq
 cashpw/personal-packages-loaded-p (cashpw/load
                                    (format
                                     "%s/packages-personal.el"
                                     cashpw/path--emacs-config-dir)))
(run-hooks 'cashpw/personal-packages-loaded-hooks)

(when (cashpw/machine-p 'work-cloudtop)
  (defcustom
    cashpw/work-packages-loaded-p
    nil
    "Non-nil if my packages have finished loading."
    :group 'cashpw
    :type 'boolean)
(defvar cashpw/work-packages-loaded-hooks '()
  "Hooks to run after we finish loading work packages.")
  (setq
   cashpw/work-packages-loaded-p (cashpw/load
                                  (format
                                   "%s/packages-work.el"
                                   cashpw/path--emacs-config-dir))))
(run-hooks 'cashpw/work-packages-loaded-hooks)
