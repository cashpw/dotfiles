(defgroup cashpw
  nil
  "Group for my customizations and configurations.")

(defcustom cashpw/debug nil
  "Non-nil if my personal debug mode is active.")

(define-minor-mode cashpw-debug-mode
  "Minor mode to toggle my personal debug settings."
  :global t
  :lighter nil
  (setq cashpw/debug cashpw-debug-mode))

(cl-defun
    cashpw/log--internal
    (partial-log-string type &optional buffer)
  "Log PARTIAL-LOG-STRING with TYPE to BUFFER (or message, if nil)."
  (let ((full-log-string (format
                              "[%s %s] %s"
                              type
                              (format-time-string "%F %T%Z")
                              partial-log-string)))
    (if (and buffer (bufferp buffer))
        (with-current-buffer buffer
          (insert full-log-string "\n"))
      (message full-log-string))))

(cl-defun
    cashpw/log-info
    (format-string &rest args)
  "Log formatted, with ARGS, FORMAT-STRING with state of \"INFO\"."
  (cashpw/log--internal (apply #'format format-string args) "INFO"))
(defalias 'cashpw/log 'cashpw/log-info)

(cl-defun
    cashpw/log-to-buffer-info
    (buffer format-string &rest args)
  "Log formatted, with ARGS, FORMAT-STRING with state of \"INFO\" to BUFFER."
  (cashpw/log--internal (apply #'format format-string args) "INFO" buffer))
(defalias 'cashpw/log-to-buffer 'cashpw/log-to-buffer-info)

(cl-defun
    cashpw/log-debug
    (format-string &rest args)
  "Log formatted, with ARGS, FORMAT-STRING with state of \"DEBUG\"."
  (when cashpw/debug
    (cashpw/log--internal (apply #'format format-string args) "DEBUG")))

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

(defcustom
  cashpw/personal-config-loaded-p
  nil
  "Non-nil if my config has finished loading."
  :group 'cashpw
  :type 'boolean)
(defvar cashpw/personal-config-loaded-hooks '()
  "Hooks to run after we finish loading personal config.")
(setq
 cashpw/personal-config-loaded-p (cashpw/load
                                  (format
                                   "%s/config-personal.el"
                                   cashpw/path--emacs-config-dir)))
(run-hooks 'cashpw/personal-config-loaded-hooks)

(when (cashpw/machine-p 'work-cloudtop)
  (defcustom
    cashpw/work-config-loaded-p
    nil
    "Non-nil if my config has finished loading."
    :group 'cashpw
    :type 'boolean)
(defvar cashpw/work-config-loaded-hooks '()
  "Hooks to run after we finish loading work config.")
  (setq
   cashpw/work-config-loaded-p (cashpw/load
                                (format
                                 "%s/config-work.el"
                                 cashpw/path--emacs-config-dir))))
(run-hooks 'cashpw/work-config-loaded-hooks)
