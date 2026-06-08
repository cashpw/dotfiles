;;; verify-agenda.el --- Headless verification for Custom Org Agenda Views -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)
(require 'eieio)

(message "Bootstrapping headless environment...")

;; 1. Dynamic load-path extension to resolve straight dependencies
(let* ((version-str (format "%d.%d" emacs-major-version emacs-minor-version))
       (straight-build-dir (expand-file-name (format "~/.config/emacs/.local/straight/build-%s" version-str))))
  (when (file-directory-p straight-build-dir)
    (dolist (dir (directory-files straight-build-dir t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; 2. Smart/Dummy macros and variables to bypass Doom boot errors
(defun cashpw/feature-eval-p (spec)
  "Return non-nil if the Doom package SPEC is satisfied."
  (cond
   ((symbolp spec)
    (featurep spec))
   ((and (listp spec) (eq (car spec) :and))
    (cl-every #'cashpw/feature-eval-p (cdr spec)))
   ((and (listp spec) (eq (car spec) :or))
    (cl-some #'cashpw/feature-eval-p (cdr spec)))
   ((listp spec)
    (cl-every #'cashpw/feature-eval-p spec))
   (t nil)))

(defmacro after! (package &rest body)
  "Execute body immediately if PACKAGE is loaded, otherwise defer. Bypasses complex Doom module lists."
  (declare (indent defun))
  (if (cashpw/feature-eval-p package)
      `(progn ,@body)
    (cond
     ((symbolp package)
      `(with-eval-after-load ',package ,@body))
     (t nil))))

(defmacro use-package! (&rest _body) nil)
(defmacro map! (&rest _body) nil)
(defmacro cmd! (&rest _body) nil)
(defmacro add-hook! (&rest _body) nil)
(defmacro bind-key (&rest _body) nil)
(defmacro unbind-key (&rest _body) nil)
(defmacro deflink (&rest _body) nil)
(defmacro org-defblock (&rest _body) nil)
(defmacro doct (&rest _body) nil)

(defvar doom-leader-map (make-sparse-keymap))
(defvar doom-localleader-map (make-sparse-keymap))
(defvar plru-internal-version-constant "1.0")
(defvar org-recipes--properties nil)
(defvar notmuch-saved-searches nil)
(defvar native-comp-deferred-compilation-deny-list nil)

;; Org clock hooks
(defvar org-clock-in-hook nil)
(defvar org-clock-out-hook nil)

;; Define dummy plru-repository class so instantiation doesn't fail
(defclass plru-repository () () :abstract t)
;; Allow instantiation of the dummy class
(cl-defmethod make-instance ((_class (eql 'plru-repository)) &rest _slots)
  nil)

;; Dummy functions for packages loading
(defun make-org-gcal-profile (&rest _args) nil)
(defun secret-get (&rest _args) nil)
(defun alert-define-style (&rest _args) nil)

;; 3. Ensure core Emacs libraries are loaded
(require 'dash)
(require 'f)
(require 'org)
(require 'org-agenda)
(require 'org-super-agenda)
(require 'org-extras)

;; Enable org-super-agenda
(org-super-agenda-mode 1)

;; 4. Dynamically load our tangled configuration files from the active workspace
(let* ((tests-dir (file-name-directory (or load-file-name (buffer-file-name) default-directory)))
       (doom-dir (expand-file-name ".." tests-dir))
       (g3-dir (expand-file-name "../../../../../.." doom-dir)))
  (message "Loading configs from: %s" doom-dir)
  ;; Set up minimal paths so the config does not throw error during loading
  (setq cashpw/path--home-dir (expand-file-name "~"))
  (setq cashpw/path--emacs-config-dir doom-dir)
  ;; Load custom libraries
  (add-to-list 'load-path (expand-file-name "lisp" doom-dir))
  (require 'cashpw-time)
  ;; Load google3 org-buganizer library
  (message "Adding google3 editors load path: %s" g3-dir)
  (add-to-list 'load-path (expand-file-name "devtools/editors/emacs" g3-dir))
  (require 'org-buganizer)
  ;; Load config.el first to define cashpw/machine-p and other core functions/variables
  (load (expand-file-name "config.el" doom-dir))
  ;; Load work config explicitly to make sure it is loaded
  (load (expand-file-name "config-work.el" doom-dir))
  ;; Load tangled unit tests
  (load (expand-file-name "tests.el" doom-dir)))

;; 5. Setup Mocks and Mock Data
(defvar test-agenda-file nil)

(defun setup-mock-agenda-data ()
  "Create a temporary org file with pre-configured task capacities."
  (setq test-agenda-file (make-temp-file "verify-agenda-mock" nil ".org"))
  (with-temp-file test-agenda-file
    (insert "* TODO Task A\n")
    (insert "  SCHEDULED: <2026-05-26 Tue>\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :Effort:   1:30\n") ; 1.5h
    (insert "  :END:\n")
    (insert "* TODO Task B\n")
    (insert "  SCHEDULED: <2026-05-26 Tue>\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :Effort:   2:00\n") ; 2.0h
    (insert "  :END:\n")
    (insert "* TODO Task C :2026w22:\n") ; Tagged for current week
    (insert "  :PROPERTIES:\n")
    (insert "  :Effort:   4:00\n") ; 4.0h
    (insert "  :END:\n")))

(defun teardown-mock-agenda-data ()
  "Remove the temporary mock file."
  (when (and test-agenda-file (file-exists-p test-agenda-file))
    (delete-file test-agenda-file)
    (setq test-agenda-file nil)))

;; 6. ERT Tests for verification
(ert-deftest verify-agenda-today-header ()
  "Verify header math and presence in .today (Today) view."
  (setup-mock-agenda-data)
  (unwind-protect
      ;; Mock time to 2026-05-26 (Tuesday)
      (cl-letf* ((mock-time (date-to-time "2026-05-26 10:00:00"))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'org-today) (lambda () (time-to-days mock-time)))
                 ((symbol-function 'cashpw-time-current-week-tag) (lambda () "2026w22"))
                 ((symbol-function 'cashpw/org-agenda-custom-commands--update) #'ignore)
                 ((symbol-function 'cashpw/org-agenda-view--today--files) (lambda () (list test-agenda-file)))
                 (org-keys ".today")
                 (cashpw-org-agenda-daily-capacity-hours 6.0))
        (let ((org-agenda-files (list test-agenda-file))
              (org-agenda-custom-commands
               (list (list ".today" "Today"
                           (cashpw/org-agenda-view--today)
                           '((org-agenda-compact-blocks nil)
                             (org-agenda-block-separator ""))))))
          (message "Value of org-agenda-custom-commands in Today Test: %S" org-agenda-custom-commands)
          ;; Run the agenda command
          (org-agenda nil ".today")
          (with-current-buffer "*Org Agenda*"
            (goto-char (point-min))
            (message "--- Today View Output ---")
            (message "%s" (buffer-string))
            (message "-------------------------")
            ;; Search for dynamic header
            (goto-char (point-min))
            (should (re-search-forward "Planned Today: 3.5h / 6.0h" nil t)))))
    (teardown-mock-agenda-data)))

(ert-deftest verify-agenda-week-planner-header ()
  "Verify header math and presence in .plan-week (Week Planner) view."
  (setup-mock-agenda-data)
  (unwind-protect
      ;; Mock current week tag to match Task C
      (cl-letf* ((mock-time (date-to-time "2026-05-26 10:00:00"))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'org-today) (lambda () (time-to-days mock-time)))
                 ((symbol-function 'cashpw-time-current-week-tag) (lambda () "2026w22"))
                 ((symbol-function 'cashpw/org-agenda-custom-commands--update) #'ignore)
                 (org-keys ".plan-week")
                 (cashpw-org-agenda-weekly-capacity-hours 30.0))
        (let ((org-agenda-files (list test-agenda-file))
              (org-agenda-custom-commands
               (list (list ".plan-week" "Week" (cashpw/org-agenda-view--plan--week)))))
          ;; Run the agenda command
          (org-agenda nil ".plan-week")
          (with-current-buffer "*Org Agenda*"
            (goto-char (point-min))
            (message "--- Week Planner View Output ---")
            (message "%s" (buffer-string))
            (message "--------------------------------")
            ;; Search for week dynamic header
            (goto-char (point-min))
            (should (re-search-forward "Planned This Week: 4.0h / 30.0h" nil t)))))
    (teardown-mock-agenda-data)))

(ert-deftest verify-agenda-keybindings ()
  "Verify keybindings and bulk custom registrations if maps are loaded."
  ;; Verify Bulk registration (always available)
  (let ((bulk-W (assq ?W org-agenda-bulk-custom-functions)))
    (should bulk-W)
    (should (eq (nth 1 bulk-W) #'cashpw/org-agenda-bulk-assign-to-week))
    (should (eq (nth 2 bulk-W) #'cashpw/org-agenda-bulk-assign-to-week-arg)))

  ;; Verify keymaps if evil-org-agenda is available
  (when (boundp 'evil-org-agenda-mode-map)
    (let ((binding (lookup-key evil-org-agenda-mode-map (kbd "w"))))
      (should (keymapp binding)))
    (should (eq (lookup-key evil-org-agenda-mode-map (kbd "w .")) #'cashpw/org-agenda-assign-to-this-week))
    (should (eq (lookup-key evil-org-agenda-mode-map (kbd "w n")) #'cashpw/org-agenda-assign-to-next-week))
    (should (eq (lookup-key evil-org-agenda-mode-map (kbd "w w")) #'cashpw/org-agenda-assign-to-week))))

(ert-deftest cashpw/org-agenda-bulk-assign-to-week-arg-test ()
  "Test that cashpw/org-agenda-bulk-assign-to-week-arg returns correct tags."
  (cl-letf (((symbol-function 'cashpw-time-current-week-tag) (lambda () "2026w22"))
            ((symbol-function 'cashpw-time-next-week-tag) (lambda () "2026w23")))
    ;; Case 1: Choice '.' -> This week
    (cl-letf (((symbol-function 'read-char-choice) (lambda (_prompt _choices) ?\.)))
      (should (equal (cashpw/org-agenda-bulk-assign-to-week-arg) '("2026w22"))))
    ;; Case 2: Choice 'n' -> Next week
    (cl-letf (((symbol-function 'read-char-choice) (lambda (_prompt _choices) ?n)))
      (should (equal (cashpw/org-agenda-bulk-assign-to-week-arg) '("2026w23"))))
    ;; Case 3: Choice 'w' -> Custom week
    (cl-letf (((symbol-function 'read-char-choice) (lambda (_prompt _choices) ?w))
              ((symbol-function 'read-string) (lambda (_prompt _initial) "2026w25")))
      (should (equal (cashpw/org-agenda-bulk-assign-to-week-arg) '("2026w25"))))))

;; 7. Main runner
(defun run-verification-tests ()
  "Run all Ert verification tests and exit."
  (message "Running headless agenda verification...")
  (ert-run-tests-batch-and-exit))

(run-verification-tests)
