(setq search-invisible t)

;; (setq straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref))

(use-package! s
  :demand t)
(use-package! dash
  :demand t)
;; Fix error: "File mode specification error: (error Problem in magic-mode-alist with element ess-SAS-listing-mode-p)".
;; (use-package! ess-site)

;; Set dummy values before requiring org-gcal to prevent warning message
(defvar org-gcal-client-id "foo")
(defvar org-gcal-client-secret "foo")

(setq
 user-full-name "Cash Prokop-Weaver"
 user-mail-address "cashbweaver@gmail.com")

(setq calendar-date-style 'iso)

(use-package! day-of-week)

(defun cashpw/replace-regexp-in-buffer (regexp replacement &optional buffer)
  "Replace all occurences of REGEXP in BUFFER with REPLACMENT."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match replacement 'fixedcase)))))

(defun cashpw/replace-regexp-in-file (regexp replacement file-path)
  "Replace all occurences of REGEXP in FILE-PATH with REPLACMENT."
  (cashpw/replace-regexp-in-buffer regexp replacement
                                   (find-file-noselect file-path)))

(defun cashpw/iso-week-to-time(year week day)
  "Convert ISO year, week, day to elisp time value.

Reference: https://emacs.stackexchange.com/a/43985"
  (apply #'encode-time
         (append '(0 0 0)
                 (-select-by-indices
                  '(1 0 2)
                  (calendar-gregorian-from-absolute (calendar-iso-to-absolute
                                                     (list week day year)))))))

(defun cashpw/iso-beginning-of-week(year week)
  "Convert ISO year, week to elisp time for first day (Monday) of week.

Reference: https://emacs.stackexchange.com/a/43985"
  (cashpw/iso-week-to-time year week 1))

(defun cashpw/iso-end-of-week(year week)
  "Convert ISO year, week to elisp time for last day (Sunday) of week.

Reference: https://emacs.stackexchange.com/a/43985"
  (cashpw/iso-week-to-time year week 7))

(cl-defun cashpw/time--overwrite (time
                                  &key
                                  seconds
                                  minutes
                                  hours
                                  day
                                  month
                                  year
                                  day-of-week
                                  daylight-savings-time-p
                                  utc-offset)
  "Return TIME with overwritten values."
  (cl-destructuring-bind
      (prev-seconds
       prev-minutes
       prev-hours
       prev-day
       prev-month
       prev-year
       prev-day-of-week
       prev-daylight-savings-time-p
       prev-utc-offset)
      (decode-time time)
    (encode-time (or seconds prev-seconds)
                 (or minutes prev-minutes)
                 (or hours prev-hours)
                 (or day prev-days)
                 (or month prev-month)
                 (or year prev-year)
                 (or day-of-week prev-day-of-week)
                 (or daylight-savings-time-p prev-daylight-savings-time-p)
                 (or utc-offset prev-utc-offset))))

(defun cashpw/time--today-at-hh-mm (hh mm)
  "Return a time object for the current day at HH:MM."
  (cl-destructuring-bind (seconds
                          minutes
                          hours
                          days
                          months
                          years
                          day-of-week
                          daylight-savings-time-p
                          utc-offset)
      (decode-time (current-time))
    (encode-time 0
                 mm
                 hh
                 days
                 months
                 years
                 day-of-week
                 daylight-savings-time-p
                 utc-offset)))

(defun cashpw/time--end-of-day (time)
  "Return TIME with maximum hours, minutes, and seconds."
  (cl-destructuring-bind
      (_seconds
       _minutes
       _hours
       days
       months
       years
       day-of-week
       daylight-savings-time-p
       utc-offset)
      (decode-time time)
    (encode-time
     59
     59
     23
     days
     months
     years
     day-of-week
     daylight-savings-time-p
     utc-offset)))

(defun cashpw/time-future-p (time)
  "Return non-nil if TIME occurs in the future."
  (and
   (not
    (time-equal-p
     time
     (current-time)))
   (not
    (time-less-p
     time
     (current-time)))))

(defun cashpw/time-past-p (time)
  "Return non-nil if TIME occurs in the past."
  (time-less-p
   time
   (current-time)))

(defun cashpw/time-tomorrow-p (time)
  "Return non-nil if TIME occurs tomorrow."
  (let ((tomorrow
         (+ 1
            (time-to-days
             (current-time)))))
    (= (time-to-days
        time)
       tomorrow)))

(defun cashpw/time-same-day-p (time-a time-b)
  "Return non-nil if TIME-A and TIME-B are on the same day."
  (when (and time-a time-b)
    (= (time-to-days time-a) (time-to-days time-b))))

(defun cashpw/time-today-p (time)
  "Return non-nil if TIME occurs today."
  (cashpw/time-same-day-p time (current-time)))

(defun cashpw/time--zero-out-hh-mm-ss (time)
  "Return TIME with hours, minutes, and seconds set to 0."
  (cl-destructuring-bind
      (_seconds
       _minutes
       _hours
       days
       months
       years
       day-of-week
       daylight-savings-time-p
       utc-offset)
      (decode-time time)
    (encode-time
     0
     0
     0
     days
     months
     years
     day-of-week
     daylight-savings-time-p
     utc-offset)))

(defun cashpw/time-greater-than-p (a b)
  "Return non-nil if time value A is greater than time value B."
  (and (not (time-equal-p a b))
       (not (time-less-p a b))))

(defconst cashpw/time--day-number-sunday 0)
(defconst cashpw/time--day-number-monday 1)
(defconst cashpw/time--day-number-tuesday 2)
(defconst cashpw/time--day-number-wednesday 3)
(defconst cashpw/time--day-number-thursday 4)
(defconst cashpw/time--day-number-friday 5)
(defconst cashpw/time--day-number-saturday 6)

(defconst cashpw/time--month-number-january 1)
(defconst cashpw/time--month-number-february 2)
(defconst cashpw/time--month-number-march 3)
(defconst cashpw/time--month-number-april 4)
(defconst cashpw/time--month-number-may 5)
(defconst cashpw/time--month-number-june 6)
(defconst cashpw/time--month-number-july 7)
(defconst cashpw/time--month-number-august 8)
(defconst cashpw/time--month-number-september 9)
(defconst cashpw/time--month-number-october 10)
(defconst cashpw/time--month-number-november 11)
(defconst cashpw/time--month-number-december 12)

(defvar cashpw/holiday-mothers-day
  '(holiday-float
    cashpw/time--month-number-may
    cashpw/time--day-number-sunday
    2
    "Mother's Day"))
(defvar cashpw/holiday-fathers-day
  '(holiday-float
    cashpw/time--month-number-june
    cashpw/time--day-number-sunday
    3
    "Father's Day"))

(defun cashpw/holiday-get-next-time (holiday &optional force-next-year)
  "Return next HOLIDAY occurrance time.

The next occurrance may be in the current year. Use FORCE-NEXT-YEAR to get next year's time."
  (let* ((holiday-buffer-name "*get-next-holiday*")
         (this-year (string-to-number (format-time-string "%Y" (current-time))))
         (next-year (1+ this-year))
         (holiday-strings
          (progn
            (list-holidays
             (if force-next-year
                 next-year
               this-year)
             next-year (list holiday))
            (split-string (with-current-buffer holiday-buffer
                            (buffer-string))
                          "\n")))
         (holiday-times
          (mapcar
           (lambda (holiday-string)
             (date-to-time (car (split-string holiday-string ": "))))
           holiday-strings)))
    (with-current-buffer holiday-buffer
      (kill-buffer))
    (car
     (seq-filter
      (lambda (holiday-time)
        (cashpw/time-greater-than-p holiday-time (current-time)))
      holiday-times))))

(defvar cashpw/path--proj-dir
  (s-lex-format "${cashpw/path--home-dir}/proj")
  "Projects directory.")

(defvar cashpw/path--notes-dir
  (s-lex-format "${cashpw/path--proj-dir}/notes")
  "Personal org-roam notes directory.")

(defvar cashpw/path--browser-history-dir
  (file-name-concat cashpw/path--notes-dir "browser-history/")
   "Personal org-roam notes directory.")

(defvar cashpw/path--personal-todos
  (s-lex-format "${cashpw/path--notes-dir}/todos.org")
  "Personal TODOs file.")
(defun cashpw/path-todos ()
  "Return path to TODOs."
  cashpw/path--personal-todos)

(defvar cashpw/path--personal-calendar
  (s-lex-format "${cashpw/path--notes-dir}/calendar-personal.org")
  "Personal calendar file.")
(defun cashpw/path-calendar ()
  "Return path to TODOs."
  cashpw/path--personal-calendar)

(defvar cashpw/path--personal-asana
  (s-lex-format "${cashpw/path--notes-dir}/asana.org")
  "Personal calendar file.")

(defvar cashpw/path--sleep-calendar
  (s-lex-format "${cashpw/path--notes-dir}/calendar-sleep.org")
  "Sleep calendar file.")

(defvar cashpw/path--reading-list
  (s-lex-format "${cashpw/path--notes-dir}/reading_list.org")
  "Reading list.")

(defvar cashpw/path--notes-bibliography (format "%s/proj/notes/bibliography.bib"
                                        cashpw/path--home-dir)
  "Path to bibliograpy in notes.")

(defvar cashpw/bibliographies `(,cashpw/path--notes-bibliography)
  "List of my bibliographies.")

(setenv "GPG_AGENT_INFO")

(use-package!
 secret
 :config
 (set-secret-dir (format "%s/.config/secrets" cashpw/path--home-dir)))

; Reference; https://www.emacswiki.org/emacs/DocumentingKeyBindingToLambda
(defun cashpw/evil-lambda-key (mode keymap key def)
  "Wrap `evil-define-key' to provide documentation."
  (set 'sym (make-symbol (documentation def)))
  (fset sym def)
  (evil-define-key mode keymap key sym))

(defun cashpw/grep (command-string)
  "Return grep, with COMMAND-STRING, results as a list."
  (split-string
   (shell-command-to-string
    (format
     "grep %s"
     command-string))))

(defun cashpw/rgrep (command-string)
  "Return rgrep, with COMMAND-STRING, results as a list."
  (split-string
   (shell-command-to-string
    (format
     "rgrep %s"
     command-string))
   "\n"
   t))

(defun cashpw/pcregrep (command-string)
  "Return rgrep, with COMMAND-STRING, results as a list."
  (split-string
   (shell-command-to-string
    (format
     "pcregrep %s"
     command-string))
   "\n"
   t))

(defun cashpw/delete-lines-below (line-number)
  "Delete all lines beneath LINE-NUMBER."
  (interactive "nLine number: ")
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (delete-region (point) (point-max))))

(defun cashpw/run-function-in-file (filepath function &optional arguments)
  (let ((args (or arguments
                  nil)))
    (save-excursion
      (find-file filepath)
      (apply function arguments)
      (write-file filepath)
      (kill-buffer (current-buffer)))))

(defun cashpw/open-file (file-path)
  "Open file at FILE-PATH in another window."
  (let ((buffer (find-file-other-window file-path)))
    (with-current-buffer buffer
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun cashpw/cpp--get-header-file-path (file-path)
  "Return the path to the header file for the provided FILE-PATH."
  (cond
   ((s-ends-with-p ".cc" file-path)
    (concat (file-name-sans-extension file-path) ".h"))
   ((s-ends-with-p "_test.cc" file-path)
    (concat (substring 0 (- (length file-path) 5)
                       (file-name-sans-extension file-path))
            ".h"))
   (t
    file-path)))

(defun cashpw/cpp--get-test-file-path (cpp-file-path)
  "Return the path to the test file for the provided FILE-PATH."
  (cond
   ((or (s-ends-with-p ".cc" file-path)
        (s-ends-with-p ".h" file-path))
    (concat (file-name-sans-extension file-path) "_test.cc"))
   (t
    file-path)))

(defun cashpw/cpp--get-source-file-path (cpp-file-path)
  "Return the path to the source file for the provided CPP-FILE-PATH."
  (concat (file-name-sans-extension cpp-file-path) ".cc"))

(defun cashpw/file--get-readme-file-path (file-path)
  "Return the path to the readme file for the provided FILE-PATH."
  (concat (file-name-directory file-path) "README.md"))

(defun cashpw/cpp--switch-to-header-file ()
  "Switch to the header file for the current buffer."
  (interactive)
  (find-file (cashpw/cpp--get-header-file-path buffer-file-name)))

(defun cashpw/cpp--switch-to-test-file ()
  "Switch to the test file for the current buffer."
  (interactive)
  (find-file (cashpw/cpp--get-test-file-path buffer-file-name)))

(defun cashpw/cpp--switch-to-source-file ()
  "Switch to the test file for the current buffer."
  (interactive)
  (find-file (cashpw/cpp--get-source-file-path buffer-file-name)))

(defun cashpw/file--switch-to-readme-file ()
  "Switch to the readme file for the current buffer."
  (interactive)
  (find-file (cashpw/file--get-readme-file-path buffer-file-name)))

(defun cashpw/replace-selection ()
  (interactive)
  (let* ((register
          ?\")
         (to-replace
          (replace-regexp-in-string
           "\\["
           "\\\\["
           (replace-regexp-in-string
            "\\]"
            "\\\\]"
            (replace-regexp-in-string
             "/"
             "\\\\/"
             (progn
               (evil-yank (mark)
                          (point)
                          nil
                          register)
               (evil-get-register register)))))))
    (evil-ex (s-lex-format  "%s/${to-replace}/"))))

(defun cashpw/search-selection ()
  (interactive)
  (let* ((register ?\")
         (target
          (progn
            (evil-yank (mark) (point) nil register)
            (evil-get-register register))))
    (setq evil-ex-search-pattern `(,target t t))
    (evil-ex-search)))

(defun cashpw/reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun cashpw/advice-remove-all (sym)
  "Remove all advices from symbol SYM.

Reference: https://emacs.stackexchange.com/a/24658/37010"
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice))
               sym))

(defun cashpw/buffer-contains-regexp-p (regexp &optional buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME contains REGEXP."
  (with-current-buffer (or buffer-or-name (buffer-name))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regexp nil t))))

(defun cashpw/maybe-add-trailing-forward-slash (str)
  "Return STR with a trailing slash (added if it was missing)."
  (if (s-ends-with? "/" str)
      str
    (format "%s/" str)))

(defun cashpw/dim (text)
  "Dim color/brightness of TEXT."
  (propertize text 'face 'shadow))

(unless
    ;; Avoid 'void-variable mouse-wheel-up-event' error
    (or (cashpw/machine-p 'work-cloudtop) (cashpw/machine-p 'personal-phone))
  (use-package! centered-cursor-mode))

;; (use-package! electric-case
;;   :config
;;   (add-hook!
;;    'c++-mode-hook
;;    'electric-case-c-init)
;;   (add-hook!
;;    'c-mode-hook
;;    'electric-case-c-init)
;;   (add-hook!
;;    'java-mode-hook
;;    'electric-case-java-init))

(use-package! command-log-mode
  :config
  (setq
   command-log-mode-open-log-turns-on-mode t
   command-log-mode-window-size 80
   command-log-mode-is-global t))

(use-package! free-keys)

(use-package! memoize)

(use-package! operate-on-number)

(use-package! increment-ordinal
  :config
  (defun increment-ordinals-in-todo ()
    "Increment ordinal nubmers in TODO headline."
    (let ((headline
          (org-entry-get nil "ITEM")))
      (org-edit-headline
       (increment-ordinals-in-string headline)))))

(use-package! pcache
  :custom
  (pcache-directory (file-name-concat doom-profile-cache-dir "pcache/")))

(use-package! plru
  :custom
  (plru-directory (file-name-concat doom-profile-cache-dir "plru/")))

(use-package!
    isbn
  :config
  (defun cashpw/isbn-select-from-buffer ()
    "Return selected isbn from buffer."
    (let* ((isbn-10s (isbn-10-get-all-in-buffer))
           (isbn-13s (isbn-13-get-all-in-buffer)))
      (completing-read "ISBN: " (append isbn-10s isbn-13s) nil t))))
(after! json
  (defun cashpw/google-books--list (query key success)
    "Return results of Google Books QUERY with api KEY.
Invokes SUCCESS on success."
    (request "https://www.googleapis.com/books/v1/volumes"
      :sync t
      :params `(("key" . ,key) ("q" . , query) ("orderBy" . "relevance") ("maxResults" . 20))
      :parser 'json-read
      :success success)))

(defun cashpw/google-books-select-isbn-and-add-citation (query)
  "Select ISBN from Google Books."
  (interactive "MQuery: ")
  (cashpw/google-books--list
   query (secret-get "google-books")
   (cl-function (lambda (&key data &allow-other-keys)
                  (let ((options
                         (--map
                          (when-let ((volume-info (alist-get 'volumeInfo it)))
                            (cons
                             (concat
                              (let ((title (alist-get 'title volume-info))
                                    (max-length 50))
                                (if (> (length title) max-length)
                                    (substring title 0 max-length)
                                  (string-pad title max-length)))
                              ""
                              (let ((authors
                                     (string-join (alist-get
                                                   'authors volume-info)
                                                  ","))
                                    (max-length 30))
                                (if (> (length authors) max-length)
                                    (substring authors 0 max-length)
                                  (string-pad authors max-length)))
                              ""
                              (let ((description
                                     (alist-get 'description volume-info))
                                    (max-length 30))
                                (if (> (length description) max-length)
                                    (substring description 0 max-length)
                                  (string-pad description max-length))))
                             ;; First ISBN in list of identifiers
                             (cdr
                              (car
                               (cdr
                                (elt
                                 (alist-get
                                  'industryIdentifiers volume-info)
                                 0))))))
                          (alist-get 'items data))))
                    (let ((vertico-sort-function #'identity)
                          (isbn
                           (alist-get (completing-read
                                       "Volume: "
                                       options
                                       nil 'require-match)
                                      options
                                      nil nil #'string=)))
                      (zotra-add-entry isbn)))))))

(use-package! titlecase)

(use-package! whisper
  :config
  (setq whisper-install-directory "~/.config/emacs/.local/cache/"
        ;; whisper-model "large-v3"
        ;; whisper-model "medium"
        ;; whisper-model "small"
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        whisper--ffmpeg-input-device "hw:0"
        whisper-return-cursor-to-start nil))

(use-package! writeroom-mode
  :config
  (setq
   +zen-mixed-pitch-modes '()
   writeroom-width 45))

(setq
 alert-fade-time 60
 alert-default-style 'libnotify)

(use-package! org-wild-notifier
  :after org
  :defer t
  :custom
  (org-wild-notifier-alert-time '(0))
  :init
  (add-hook 'after-init-hook #'org-wild-notifier-mode))

(use-package! scheduled-alert)

(scheduled-alert-cancel-all)
(cl-dolist (hhmm '((10 . 0)
                   (11 . 0)
                   (12 . 0)
                   (13 . 0)
                   (14 . 0)
                   (15 . 0)
                   (16 . 0)))
  (scheduled-alert-schedule
   (cashpw/time--today-at-hh-mm
    (car hhmm)
    (cdr hhmm))
   "Stand up"
   '(:persistent t)))
(cl-dolist (hhmm '((10 . 15)
                   (11 . 15)
                   (12 . 15)
                   (13 . 15)
                   (14 . 15)
                   (15 . 15)
                   (16 . 15)))
  (scheduled-alert-schedule
   (cashpw/time--today-at-hh-mm
    (car hhmm)
    (cdr hhmm))
   "Sit down"
   '(:persistent t)))

; Reference; https://www.emacswiki.org/emacs/DocumentingKeyBindingToLambda
(defun cashpw/evil-lambda-key (mode keymap key def)
  "Wrap `evil-define-key' to provide documentation."
  (set 'sym (make-symbol (documentation def)))
  (fset sym def)
  (evil-define-key mode keymap key sym))

(map!
 ;; Keep in alphabetical order.
 (:leader
  :desc "at point"
  :n "h h" #'helpful-at-point
  ;; :desc "Langtool" :n "t L" #'langtool-check
  ;; :desc "LLM" :n "l" #'gptel-send
  :n "r" #'whisper-run
  :n "R" #'cashpw/whisper-run-and-cue-gptel
  (:prefix
   ("d" . "agenDa")
   :desc "Inbox"
   :n "i" (cmd! (org-agenda nil ".inbox"))
   :desc "Overdue"
   :n "o" (cmd! (org-agenda nil ".overdue"))
   (:prefix
    ("g" . "Gallery")
    :desc "Current buffer"
    :n "."
    (cmd!
     (defvar cashpw/gallery-file-name)
     (let ((org-agenda-cmp-user-defined #'cashpw/cmp-random)
           (default-directory cashpw/path--notes-dir)
           (cashpw/gallery-file-name (buffer-file-name))
           (org-agenda-sorting-strategy '((agenda . (user-defined-up)))))
       (org-agenda nil ".gallery-current-buffer")
       (cashpw/feh-gallery-of-linked-images-in-buffer)
       (org-agenda-quit)))
    :desc "Select tag"
    :n "t"
    (cmd!
     (let ((org-agenda-cmp-user-defined #'cashpw/cmp-random)
           (default-directory cashpw/path--notes-dir)
           (org-agenda-sorting-strategy '((agenda . (user-defined-up)))))
       (org-agenda nil ".gallery")
       (cashpw/feh-gallery-of-linked-images-in-buffer)
       (org-agenda-quit)))
    :desc "Photos"
    :n "p"
    (cmd!
     (let ((org-agenda-cmp-user-defined #'cashpw/cmp-random)
           (default-directory cashpw/path--notes-dir)
           (org-agenda-sorting-strategy '((agenda . (user-defined-up)))))
       (org-agenda nil ".gallery-photos")
       (cashpw/feh-gallery-of-linked-images-in-buffer)
       (org-agenda-quit)))
    :desc "All"
    :n "g"
    (cmd!
     (let ((org-agenda-cmp-user-defined #'cashpw/cmp-random)
           (default-directory cashpw/path--notes-dir)
           (org-agenda-sorting-strategy '((agenda . (user-defined-up)))))
       (org-agenda nil ".gallery-all")
       (cashpw/feh-gallery-of-linked-images-in-buffer)
       (org-agenda-quit))))
   :desc "Today"
   :n "d" (cmd! (org-agenda nil ".today"))
   :desc "Week"
   :n "w" (cmd! (org-agenda nil ".week"))
   :desc "Habits"
   :n "h" (cmd! (org-agenda nil ".habits"))
   (:prefix
    ("n" . "Roam")
    :desc "Roam"
    :n
    "n"
    (cmd!
     (cashpw/org-select-and-go-to-todo
      (-difference
       (cl-loop
        for file in (org-mem-all-files) unless
        (or (s-ends-with-p "archive" file)
            (member
             (f-expand file)
             (list
              (f-expand (cashpw/path-calendar))
              (f-expand (cashpw/path-todos))
              (f-expand cashpw/path--reading-list))))
        when
        (seq-find
         (lambda
           (entry)
           (member (org-mem-entry-todo-state entry) org-not-done-keywords))
         (org-mem-entries-in file))
        collect (f-expand file))
       (cashpw/notes-files-with-tag "journal"))))
    :desc "Reading List"
    :n
    "r"
    (cmd! (cashpw/org-select-and-go-to-todo (list cashpw/path--reading-list))))
   (:prefix
    ("r" . "Review")
    :desc "Clock check"
    :n "c" (cmd! (org-agenda nil ".review-clockcheck"))
    :desc "Logged"
    :n "l"
    (cmd!
     (if (equal current-prefix-arg '(4))
         (org-agenda nil ".review-logged")
       (org-agenda nil ".review-logged-today")))
    :desc "Clock report"
    :n "r" (cmd! (org-agenda nil ".review-clockreport")))
   (:prefix
    ("-" . "Without")
    :desc "Effort"
    :n
    "e"
    (cmd! (org-agenda nil ".without-effort"))
    :desc "Scheduled"
    :n
    "s"
    (cmd! (org-agenda nil ".without-scheduled"))
    :desc "Priority"
    :n
    "p"
    (cmd! (org-agenda nil ".without-priority")))
   (:prefix
    ("p" . "Plan")
    :desc "Week"
    :n
    "w"
    (cmd! (org-agenda nil ".plan-week")))
   :desc "Go to TODO"
   :n "." (cmd! (cashpw/select-from-todays-todos-and-go-to)))
  (:prefix
   ("l")
   :desc "default"
   :n
   "l"
   (cmd! (cashpw/gptel-send (llm-prompts-prompt-default)))
   :n
   "k"
   #'cashpw/gptel-kill-curl-process
   :desc "empty"
   :n
   "L"
   (cmd! (cashpw/gptel-send ""))
   :desc "Council"
   :n
   "c"
   (cmd! (cashpw/gptel-send (llm-prompts-prompt-solo-performance-prompt)))
   :desc "Follow up"
   :n
   "f"
   (cmd! (cashpw/gptel-send (llm-prompts-prompt-follow-up-questions)))
   :desc "YouTube"
   :n
   "y"
   (cmd!
    (let ((buffer (get-buffer-create "*Gptel YouTube*")))
      (with-current-buffer buffer
        (org-mode)
        (delete-region (point-min) (point-max))
        (insert
         (format "\n** %s\n"
                 (with-temp-buffer
                   (org-mode)
                   (org-timestamp '(16) t)
                   (buffer-string))))
        (insert
         (llm-prompts-prompt-extract-wisdom-yt
          (read-string "YouTube URL: "
                       (ignore-errors
                         (current-kill 0 t)))))
        (cashpw/gptel-send ""))
      (display-buffer buffer)))
   (:prefix
    ("C" . "Chain of thought")
    :desc "Basic"
    :n
    "c"
    (cmd! (cashpw/gptel-send llm-prompts-prompt-fragment--chain-of-thought))
    :desc "Agent"
    :n
    "a"
    (cmd!
     (cashpw/gptel-send
      (llm-prompts-prompt-append-chain-of-thought
       (llm-prompts-prompt-agent
        (read-string "Agent (e.g. \"a writer\", \"Abraham Lincoln\"): "))))))
   (:prefix
    ("t" . "Tree of thought")
    :desc "Basic"
    :n
    "t"
    (cmd! (cashpw/gptel-send llm-prompts-prompt-fragment--tree-of-thought))
    :desc "Agent"
    :n
    "a"
    (cmd!
     (cashpw/gptel-send
      (llm-prompts-prompt-append-tree-of-thought
       (llm-prompts-prompt-agent
        (read-string "Agent (e.g. \"a writer\", \"Abraham Lincoln\"): "))))))
   (:prefix
    ("a" . "Agent")
    :desc "Software engineer"
    :n "s"
    (cmd!
     (cashpw/gptel-send
      (llm-prompts-prompt-append-chain-of-thought
       (llm-prompts-prompt-agent "TODO"))))
    :desc "Editor (non-fiction)"
    :n "e"
    (cmd!
     (cashpw/gptel-send
      (llm-prompts-prompt-append-chain-of-thought
       (llm-prompts-prompt-agent
        "an editor and technical writer. You excel at improving spelling, grammar, clarity, concision, and overall readability of text while breaking down long sentences, reducing repetition, and suggesting improvements. You follow a style guide which emphasizes plain language, serial commas, being useful, avoiding qualifying language, being explicit, putting the bottom line up front, and using formatting (headings, lists, emphasis) to improve readability"))))))
  (:prefix
   ("o")
   :desc "Elfeed"
   :n "e" #'elfeed
   ;; Override DOOM keybinding
   :n "b" #'cashpw/browse-select-url
   (:prefix
    ("n" . "Notes")
    :desc "Calendar"
    :n "c"
    (cmd!
     (cashpw/open-file
      cashpw/path--personal-calendar))
    :n "C"
    (cmd!
     (cashpw/open-file
      (s-lex-format "${cashpw/path--notes-dir}/commonplace.org")))
    :desc "Journal"
    :n "j"
    (cmd!
     (cashpw/open-file
      (s-lex-format "${cashpw/path--notes-dir}/journal-2024.org")))
    :desc "Todos"
    :n "t" (cmd! (cashpw/open-file (cashpw/path-todos)))))
  (:prefix
   ("n")
   :desc "add note" :n "+" #'org-add-note
   :desc "Store email link" :n "L" #'org-notmuch-store-link
   (:prefix
    ("A" . "Flashcards")
    :n "d" #'org-fc-dashboard
    :n "i" #'org-fc-init
    :n "u" #'org-fc-update
    :n "r" #'cashpw/org-fc-review-all
    :n "R" #'org-fc-review)
   (:prefix
    ("r")
    :desc "New art node"
    :n "a" #'cashpw/org-roam-node-create--art
    :desc "New reference node"
    :n "c" #'cashpw/org-roam-node-from-cite
    (:prefix
     ("d")
     :desc "Reflect (toggle)" "r"
     (cmd!
      (if org-daily-reflection--list-of-newly-opened-entries
          (org-daily-reflection-restore-prior-windows)
        (org-daily-reflection
         (intern
          (completing-read "What time interval?" org-daily-reflection-time-spans
                           nil t nil nil))
         4))))))
  (:prefix ("p") :n "u" #'cashpw/projectile-refresh-known-paths)
  (:prefix
   ("t")
   :n "C" #'centered-cursor-mode
   :n "D" #'toggle-debug-on-error
   :n "k" #'clm/toggle-command-log-buffer)))

(map!
 ;; Keep in alphabetical order.
 :map
 global-map
 "M-N"
 #'operate-on-number-at-point
 :v
 "C-r"
 #'cashpw/replace-selection
 (:prefix ("z") :n "O" #'evil-open-fold-rec))

(setq
 auto-save-visited-interval 60)

(auto-save-visited-mode)

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (message "Garbage collection: Running...")
                         (message "Garbage collection: Ran for %.06fsec"
                                  (k-time (garbage-collect))))))

(defconst cashpw/location-latitude 37.2
  "Current latitude.")
(defconst cashpw/location-longitude -121.8
  "Current latitude.")
(defconst cashpw/location-name "San Jose, CA"
  "Human-readable name for current location.")

(defconst cashpw/openweather-api-key (secret-get "openweather-api-key")
  "API key for OpenWeatherMap.")

(defun cashpw/openweather-get-forecast (success-fn)
  "Get weather forecast, then call SUCCESS-FN."
  (request
    "https://api.openweathermap.org/data/2.5/forecast"
    :sync t
    :parser 'json-read
    :success success-fn
    :params
    `(("appid" . ,cashpw/openweather-api-key)
      ("units" . "imperial")
      ;; ("exclude" . "current,minutely,alerts")
      ("lat" . "37.283") ("lon" . "-121.86"))))

;; (defvar cashpw/openweather-data nil)
;; (cashpw/openweather-get-forecast
;;  (cl-function (lambda (&key data &allow-other-keys)
;;                 ;; (setq cashpw/openweather-data data)
;;                 (cashpw/openweather-insert-close-window-todo 70 data))))

(defun cashpw/openweather-get-hourly-forecast-temps (data)
  "Return list of \"(<time> . <temperature>)\" for openweather hourly forecast DATA."
  (--map
   (let ((main))
     (cons
     (seconds-to-time (alist-get 'dt it))
     ;; High and low are equal for hourly forecasts
     (alist-get 'temp_max (alist-get 'main it))))
   (alist-get 'list data)))

(defun cashpw/openweather--get-points-before-and-after-crossing-threshold (time-temps temp-threshold &optional into-threshold)
  "Return TIME-TEMPS before and after crossing INTO-THRESHOLD TEMP-THRESHOLD.

TIME-TEMPS is a list of \"(<time> . <temperature>)\"."
  (let ((i 1)
        before-crossing-threshold after-crossing-threshold)
    (when (> (length time-temps) 1)
      (while (and (< i (length time-temps))
                  (not before-crossing-threshold)
                  (not after-crossing-threshold))
        (let ((previous-time-temp (nth (1- i) time-temps))
              (time-temp (nth i time-temps)))
          (when (and (not before-crossing-threshold)
                     (not after-crossing-threshold)
                     (if into-threshold
                         (< (cdr previous-time-temp) temp-threshold)
                       (> (cdr previous-time-temp) temp-threshold))
                     (if into-threshold
                         (> (cdr time-temp) temp-threshold)
                       (< (cdr time-temp) temp-threshold)))
            (setq
             before-crossing-threshold previous-time-temp
             after-crossing-threshold time-temp)))
        (cl-incf i)))
    (cons before-crossing-threshold
          after-crossing-threshold)))

(defun cashpw/openweather-get-points-before-and-after-crossing-into-threshold (time-temps temp-threshold)
  (cashpw/openweather--get-points-before-and-after-crossing-threshold time-temps temp-threshold t))

(defun cashpw/openweather-get-points-before-and-after-crossing-out-of-threshold (time-temps temp-threshold)
  (cashpw/openweather--get-points-before-and-after-crossing-threshold time-temps temp-threshold nil))

(defun cashpw/interpolate-time-for-temperature (time-a temp-a time-b temp-b target-temp)
  "Calculate the time for a target temperature via linear interpolation.

This function assumes a linear relationship between time and
temperature. Times are numbers (e.g., hours as 12.0, 15.0) and
temperatures are numbers.

It returns the calculated time as a number. If the start and end
temperatures are identical but do not match the target, it
returns nil."
  (let ((temp-range (- temp-b temp-a)))
    (if (zerop temp-range)
        ;; If temperature does not change, result is valid only if
        ;; target-temp matches the constant temperature.
        (when (= target-temp temp-a)
          time-a)
      ;; Standard case: perform linear interpolation.
      (let* ((time-range (time-subtract time-b time-a))
             (target-offset (- target-temp temp-a))
             (proportion (/ (float target-offset) temp-range)))
        (time-add time-a (seconds-to-time (* proportion (time-to-seconds time-range))))))))

(defun cashpw/openweather-get-threshold-cross-into-time (temperature-threshold openweather-data)
  "Return the approximate time at which outdoor temp crosses into TEMPERATURE-THRESHOLD."
  (when-let* ((before-after
               (cashpw/openweather-get-points-before-and-after-crossing-into-threshold
                (--filter
                 (cashpw/time-tomorrow-p (car it))
                 (cashpw/openweather-get-hourly-forecast-temps openweather-data))
                temperature-threshold))
              (before (car before-after))
              (after (cdr before-after)))
    (cashpw/interpolate-time-for-temperature
     (car before)
     (cdr before)
     (car after)
     (cdr after)
     temperature-threshold)))

(defun cashpw/openweather-get-threshold-cross-out-time (temperature-threshold openweather-data)
  "Return the approximate time at which outdoor temp crosses out of TEMPERATURE-THRESHOLD."
  (when-let* ((before-after
               (cashpw/openweather-get-points-before-and-after-crossing-out-of-threshold
                (--filter
                 (cashpw/time-tomorrow-p (car it))
                 (cashpw/openweather-get-hourly-forecast-temps openweather-data))
                temperature-threshold))
              (before (car before-after))
              (after (cdr before-after)))
    (cashpw/interpolate-time-for-temperature
     (car before)
     (cdr before)
     (car after)
     (cdr after)
     temperature-threshold)))

(defun cashpw/openweather-insert-close-windows-todo (temperature-threshold openweather-data)
  "Insert todo to close the windows."
  (with-current-buffer (find-file-noselect cashpw/path--personal-todos)
    (save-excursion
      (cashpw/org-insert-todo
       "Close the windows"
       :point-or-marker (point-max)
       :priority 1
       :category "Home"
       :effort "5m"
       :start-time (cashpw/openweather-get-threshold-cross-into-time temperature-threshold openweather-data)
       :include-hh-mm t))))

(defun cashpw/openweather-insert-open-windows-todo (temperature-threshold openweather-data)
  "Insert todo to open the windows"
  (with-current-buffer (find-file-noselect cashpw/path--personal-todos)
    (save-excursion
      (cashpw/org-insert-todo
       "Open the windows"
       :point-or-marker (point-max)
       :priority 1
       :category "Home"
       :effort "5m"
       :start-time (cashpw/openweather-get-threshold-cross-out-time temperature-threshold openweather-data)
       :include-hh-mm t))))

(use-package! sunshine
  :custom
  (sunshine-location "95125,USA")
  (sunshine-appid cashpw/openweather-api-key))

(defun sunshine-make-url (location units appid)
  "Make a URL for retrieving the weather for LOCATION in UNITS.

Requires your OpenWeatherMap APPID."
  (concat "http://api.openweathermap.org/data/2.5/forecast?"
          "lat=" (url-encode-url (number-to-string cashpw/location-latitude))
          "&lon=" (url-encode-url (number-to-string cashpw/location-longitude))
          "&APPID=" appid
          "&units=" (url-encode-url (symbol-name units))
          "&cnt=5"))

(when (cashpw/machine-p 'personal-phone)
  (advice-add 'doom/increase-font-size :override #'ignore)
  (advice-add 'doom/reset-font-size :override #'ignore))

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(advice-add
 '+popup-close-on-escape-h
 :override #'ignore)

(defun +workspaces-switch-to-project-h (&optional dir)
  "Creates a workspace dedicated to a new project. If one already exists, switch
to it. If in the main workspace and it's empty, recycle that workspace, without
renaming it.

Afterwords, runs `+workspaces-switch-project-function'. By default, this prompts
the user to open a file in the new project.

This be hooked to `projectile-after-switch-project-hook'."
  (let* ((default-directory (or dir default-directory))
         (pname (doom-project-name))
         (proot (file-truename default-directory))
         ;; HACK: Clear projectile-project-root or cached roots could interfere
         ;;   with project switching (see #3166).
         projectile-project-root)
    (when persp-mode
      (if (and (not (null +workspaces-on-switch-project-behavior))
               (or (eq +workspaces-on-switch-project-behavior t)
                   (+workspace--protected-p (safe-persp-name (get-current-persp)))
                   (+workspace-buffer-list)))
          (let* ((persp (or (+workspace-get pname t)
                            (+workspace-new pname))))
            (+workspace-switch pname)
            (with-current-buffer (doom-fallback-buffer)
              (setq-local default-directory proot)
              (hack-dir-local-variables-non-file-buffer))
            (unless current-prefix-arg
              (funcall +workspaces-switch-project-function proot))
            (+workspace-message
             (format "Switched to '%s' in new workspace" pname)
             'success))
        (with-current-buffer (doom-fallback-buffer)
          (setq-local default-directory proot)
          (hack-dir-local-variables-non-file-buffer)
          (message "Switched to '%s'" pname))
        (with-demoted-errors "Workspace error: %s"
          (+workspace-rename (+workspace-current-name) pname))
        (unless current-prefix-arg
          (funcall +workspaces-switch-project-function proot))))))

;; (use-package! svg-tag-mode
;;   :config
;;   (setq
;;    svg-tag-tags '(("\\(:[A-Z]+:\\)" . ((lambda (tag) (svg-tag-make tag :beg 1 :end -1)))))))

(use-package! nerd-icons)

(setq
 show-trailing-whitespace t)

(setq
 doom-theme 'doom-tomorrow-night)

(setq
 doom-font (font-spec
            :family "Fira Code"
            :size (if (cashpw/machine-p 'work-laptop)
                      ;; Laptop has a different DPI
                      28
                    16)))

(setq
 +ligatures-extra-symbols '(;; org Disabled in favor of org-modern
                            ;; :name          "»"
                            ;; :src_block     "»"
                            ;; :src_block_end "«"
                            ;; :quote         "“"
                            ;; :quote_end     "”"

                            ;; Typography
                            ;; :list_property "∷"
                            ;; :em_dash       "—"
                            ;; :ellipses      "…"
                            ;; :arrow_right   "→"
                            ;; :arrow_left    "←"
                            ;; :arrow_lr      "↔"
                            ;; :properties    "⚙"
                            ;; :end           "∎"

                            ;; Functional
                            :lambda        "λ"
                            :def           "ƒ"
                            :composition   "∘"
                            :map           "↦"

                            ;; Types
                            :null          "∅"
                            ;; :true          "𝕥"
                            ;; :false         "𝕗"
                            :int           "ℤ"
                            :float         "ℝ"
                            :str           "S"
                            :bool          "𝔹"
                            :list          "L"

                            ;; Flow
                            ;; :not           "￢"
                            :not           "¬"
                            :in            "∈"
                            :not-in        "∉"
                            :and           "∧"
                            :or            "∨"
                            :for           "∀"
                            :some          "∃"
                            :return        "⟼"
                            :yield         "⟻"

                            ;; Other
                            ;; :union         "⋃"
                            :union         "∪"
                            :intersect     "∩"
                            :diff          "∖"
                            :tuple         "⨂"
                            :pipe          "" ;; FIXME: find a non-private char
                            :dot           "•"))

;; (defadvice! +org-init-appearance-h--no-ligatures-a ()
;;   :after #'+org-init-appearance-h
;;   (set-ligatures! 'org-mode nil)
;;   (set-ligatures! 'org-mode
;;     :list_property "::"
;;     :em_dash       "---"
;;     :ellipsis      "..."))

(setq
 cashpw/indent-level 2)
(setq-default
 standard-indent cashpw/indent-level
 tab-width cashpw/indent-level
 c-basic-offset cashpw/indent-level
 css-indent-offset cashpw/indent-level
 js-indent-level cashpw/indent-level
 typescript-indent-level cashpw/indent-level
 js-jsx-indent-level cashpw/indent-level)

(defun cashpw/json-mode--set-indent ()
  "Set indent size in `json-mode'."
  (setq
   tab-width cashpw/indent-level
   js-indent-level cashpw/indent-level))

(add-hook! 'json-mode-hook
           #'cashpw/json-mode--set-indent)

(defmacro cashpw/icon-alias (name path)
  "Specify an icon alias."
  `(unless (file-exists-p ,path)
     (error "Missing %s icon: %s" ,name ,path))
  `(setq ,name ,path))

(cashpw/icon-alias
 cashpw/icons-hourglass-empty
  (expand-file-name
   "~/.local/share/icons/google-material/hourglass_bottom_48dp_FFF_FILL0_wght400_GRAD0_opsz48.png"))

(cashpw/icon-alias
 cashpw/icons-notifications
  (expand-file-name
   "~/.local/share/icons/google-material/notifications_48dp_FFF_FILL0_wght400_GRAD0_opsz48.png"))

;; (use-package! helm)
;; (use-package! exec-path-from-shell)
(use-package! asana
  :config
  (setq
   asana-tasks-org-file cashpw/path--personal-asana
   asana-token (secret-get "asana")))

(defun hash-table-contains-p (key table)
  "Return non-nil if TABLE contains KEY.

Reference: https://lists.gnu.org/archive/html/emacs-devel/2018-02/msg00439.html"
  (let ((x '(:hash-table-contains-p)))
    (not (eq x (gethash key table x)))))

;; (use-package go
;;   :custom
;;   ;; Note that the black piece here is the unicode white piece. However, with a dark background, white looks black and black looks white.
;;   (go-board-black-piece "○")
;;   (go-board-white-piece "●"))

(defcustom cashpw/url-patterns-to-open-in-external-browser
  '(
    ;; Reddit
    ;; Why? Reddit blocks the EWW browser.
    "^https?:\\/\\/\\([^\\.]+\\.\\)?reddit\\.com"

    ;; Google documents (Sheets, Slides, Docs, Forms)
    ;; Why? Not usable in text browsers.
    "^https?:\\/\\/docs\\.google\\.com"

    ;; YouTube
    ;; Why? Not usable in text browsers.
    "^https?:\\/\\/\\(www.\\)?youtube\\.com"

    "^https?:\\/\\/\\([^\\.]+\\.\\)?amazon\\.com" "accounts.google.com")
  "All URLs which don't match one of these patterns will be opened in a text browser (EWW).")

(setq
 browse-url-handlers
 (--map
  `(,it . browse-url-firefox) cashpw/url-patterns-to-open-in-external-browser)
 browse-url-browser-function 'eww-browse-url)

(defun cashpw/browse-select-url ()
  "Return selected url based on history and search engines."
  (interactive)
  (message "foo foo")
  (let* ((url-history-options
          (-map
           (lambda (url)
             (cons
              url
              `(lambda ()
                 (cashpw/browser-url-history-put-url ,url)
                 (funcall browse-url-browser-function ,url))))
           (mapcar #'symbol-name (plru-entry-keys-most-to-least-recent cashpw/browser-url-history))))
         (search-engine-options
          (let ((search-engines-key-to-engine
                 (-flatten-n
                  1
                  (-map
                   #'-list
                   (-map
                    (lambda (engine)
                      (-map
                       (lambda (key) (cons key engine)) (oref engine keys)))
                    cashpw/browser-search-engines)))))
            (--map
             (let ((key (car it))
                   (engine (cdr it)))
               (cons key `(lambda () (search-engine-browse-url ,engine))))
             search-engines-key-to-engine)))
         (options
          ;; List of the form (selector . handler-fn)
          ;;
          ;; handler-fn is invoked when selector is selected
          (append url-history-options search-engine-options))
         (selection (completing-read "URL/Search: " options)))
    (message "selection: %s" selection)
    (message "assoc selection: %s" (assoc selection options))
    (if (assoc selection options)
        (funcall (cdr (assoc selection options)))
      ;; Allow literal URLs
      (funcall browse-url-browser-function selection))))

(use-package!
    search-engine
  :custom
  (search-engine-cache-name-fn
   (defun cashpw/search-engine-cache-name (search-engine)
     "Return a unique name for the search engine cache."
     (format "%s-%s-query-cache"
             (symbol-name (oref search-engine id))
             (format "%s-%s" emacs-version
                     plru-internal-version-constant))))
  :config
  (let ((plru-debug t))
    (setq cashpw/browser-search-engines
          `(
            ;; Google is excluded because it requires Javascript; it doesn't work with EWW.
            ,(search-engine
              :name "DuckDuckGo"
              :id 'duckduckgo
              :keys '("@ddg" "@duckduckgo")
              :url "https://html.duckduckgo.com/html/?q=%s"
              :cache-size 200
              :cache-directory cashpw/path--browser-history-dir)
            ,(search-engine
             :name "ISBNSearch"
             :id 'isbn
             :keys '("@isbn")
             :url "https://isbnsearch.org/search?s=%s"
             :cache-size 200
             :cache-directory cashpw/path--browser-history-dir)
            ,(search-engine
              :name "IMDb"
              :id 'imdb
              :keys '("@imdb")
              :url "https://www.imdb.com/find/?q=%s"
              :cache-size 200
              :cache-directory cashpw/path--browser-history-dir)
            ,(search-engine
              :name "Wikipedia"
              :id 'wikipedia
              :keys '("@wikipedia")
              :url "https://en.wikipedia.org/w/index.php?search=%s"
              :cache-size 200
              :cache-directory cashpw/path--browser-history-dir)))))

(after! shr
  (set-face-attribute 'shr-h1 nil :height 2.0 :weight 'bold)
  (set-face-attribute 'shr-h2 nil :height 1.8 :weight 'bold)
  (set-face-attribute 'shr-h3 nil :height 1.6 :weight 'bold)
  (set-face-attribute 'shr-h4 nil :height 1.4 :weight 'bold)
  (set-face-attribute 'shr-h5 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'shr-h6 nil :height 1.0 :weight 'bold))

(let ((plru-directory cashpw/path--browser-history-dir)
      (plru-debug t))
  (setq cashpw/browser-url-history
        (plru-repository (format "browser-url-history-%s-%s"
                                 emacs-version
                                 plru-internal-version-constant)
                         :max-size 200 :save-delay 5)))

(defcustom cashpw/browser-url-history-exclude-patterns '("isbnsearch\\.org")
  "Patterns to exclude from URL history.")

(after! search-engine
  (let ((search-engine-urls
         (--map
          (string-replace "%s" "" (oref it url))
          cashpw/browser-search-engines)))
    (dolist (url search-engine-urls)
      (add-to-list 'cashpw/browser-url-history-exclude-patterns (regexp-quote url)))))

(defun cashpw/browser-url-history-put-url (url)
  "Put URL into history."
  (unless (--any
           (string-match it url) cashpw/browser-url-history-exclude-patterns)
    (plru-put cashpw/browser-url-history (intern url) t)))

(defun cashpw/browser-url-history-put-eww-url ()
  "Add current buffer's url to history."
  (cashpw/browser-url-history-put-url
   (plist-get eww-data :url)))

(add-hook 'eww-after-render-hook 'cashpw/browser-url-history-put-eww-url)

;; (use-package! w3m
;;   :config
;;   (w3m-display-mode 'tabbed-dedicated-frames))

(defun cashpw/feh-gallery (image-paths)
  "Open a feh gallery of IMAGE-PATHS."
  (shell-command (concat "feh " "--fullscreen " (string-join image-paths " "))))

(defun cashpw/org-get-link-image-paths-in-buffer ()
  "Return list of image paths from links in current buffer."
  (org-element-map
      (org-element-parse-buffer) 'link
    (lambda (link)
      (cond
       ((string= (org-element-property :type link) "file")
        (concat
         (cashpw/maybe-add-trailing-forward-slash default-directory)
         (org-element-property :path link)))
       ((string= (org-element-property :type link) "attachment")
        (concat
         (cashpw/maybe-add-trailing-forward-slash
          (save-excursion
            (goto-char (org-element-property :begin link))
            (org-agenda-with-point-at-orig-entry nil
              (org-attach-dir))))
         (org-element-property :path link)))))))

(defun cashpw/feh-gallery-of-linked-images-in-buffer ()
  "Open a feh gallery of all images in the current buffer."
  (interactive)
  (cashpw/feh-gallery (cashpw/org-get-link-image-paths-in-buffer)))

(use-package! nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(setq
 calendar-latitude 37.2
 calendar-longitude -121.8
 calendar-location-name "San Jose, CA")

(use-package! casual
  :bind (:map calc-mode-map ("C-o" . 'casual-main-menu)))

(setq
 ediff-split-window-function #'split-window-horizontally)

(use-package! ox-gfm
  :config

  (defun cashpw/org-gfm-timestamp (timestamp contents info)
    "Translate TIMESTAMP to a compatible form. INFO is a plist holding contextual information."
    (format "%s" (org-timestamp-translate timestamp)))

  ;; Override so we can set some add to translate-alist
  (org-export-define-derived-backend 'gfm 'md
    :filters-alist '((:filter-parse-tree . org-md-separate-elements))
    :menu-entry
    '(?g "Export to Github Flavored Markdown"
      ((?G "To temporary buffer"
           (lambda (a s v b) (org-gfm-export-as-markdown a s v)))
       (?g "To file" (lambda (a s v b) (org-gfm-export-to-markdown a s v)))
       (?o "To file and open"
           (lambda (a s v b)
             (if a (org-gfm-export-to-markdown t s v)
               (org-open-file (org-gfm-export-to-markdown nil s v)))))))
    :translate-alist '((inner-template . org-gfm-inner-template)
                       (paragraph . org-gfm-paragraph)
                       (timestamp . cashpw/org-gfm-timestamp)
                       (strike-through . org-gfm-strike-through)
                       (example-block . org-gfm-example-block)
                       (src-block . org-gfm-src-block)
                       (table-cell . org-gfm-table-cell)
                       (table-row . org-gfm-table-row)
                       (table . org-gfm-table))))

(after!
  emacs-everywhere
  (setq
   emacs-everywhere-org-export-options
   "#+property: header-args :exports both
#+options: toc:nil ':nil -:nil <:nil\n"
   emacs-everywhere-pandoc-md-args
   `("--from"
     ,(concat "markdown" (concat "-auto_identifiers" "-smart" "+pipe_tables"))
     "--to"
     "org"))

  (--each
      '(
        ;; Google issue tracker
        "Buganizer"

        ;; Google code review
        "Critique"

        ;; Google chat
        "Chat")
    (add-to-list 'emacs-everywhere-markdown-windows it)))

(after!
  emacs-everywhere
  (defun emacs-everywhere-insert-selection ()
    "Insert the last text selection into the buffer."
    (pcase system-type
      ('darwin
       (progn
         (call-process
          "osascript"
          nil
          nil
          nil
          "-e"
          "tell application \"System Events\" to keystroke \"c\" using command down")
         (sleep-for emacs-everywhere-clipboard-sleep-delay) ; lets clipboard info propagate
         (yank)))
      ((or 'ms-dos 'windows-nt 'cygwin)
       (emacs-everywhere-insert-selection--windows))
      (_
       (when-let ((selection (gui-get-selection 'PRIMARY 'UTF8_STRING)))
         (gui-backend-set-selection 'PRIMARY "")
         (insert selection))))
    (when (and (eq major-mode 'org-mode)
               (emacs-everywhere-markdown-p)
               (executable-find "pandoc"))
      (apply #'call-process-region
             (point-min)
             (point-max)
             "pandoc"
             t
             t
             t
             emacs-everywhere-pandoc-md-args)
      (deactivate-mark)
      (goto-char (point-max)))
    (cond
     ((bound-and-true-p evil-local-mode)
      (evil-insert-state)))))

(use-package! gnus-alias
  :config
  (autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
  (gnus-alias-init))

(after! gnus-alias
  (setq
   gnus-alias-identity-alist '(("cashbweaver@gmail"
                                ;; Refers to
                                nil
                                "Cash Prokop-Weaver <cashbweaver@gmail.com>"
                                ;; Organization
                                nil
                                ;; Extra headers
                                nil
                                ;; Body
                                nil
                                "~/.config/email-signature-personal")
                               ("cash@cashpw"
                                ;; Refers to
                                nil
                                "Cash Prokop-Weaver <cash@cashpw.com>"
                                ;; Organization
                                nil
                                ;; Extra headers
                                nil
                                ;; Body
                                nil
                                "~/.config/email-signature-personal"))
   gnus-alias-default-identity "cash@cashpw"))

(use-package! org-mime
  :custom
  org-mime-export-options '(:with-latex dvipng
                            :section-numbers nil
                            :with-author nil
                            :with-toc nil))

(defun cashpw/notmuch--toggle-all-open ()
  "Toggle `cashpw/notmuch-all-open' between nil and t."
  (condition-case nil
      (setq-local
       cashpw/notmuch-all-open (not cashpw/notmuch-all-open))
    (error
     (setq-local
      cashpw/notmuch-all-open t)
     nil)))

(defun cashpw/notmuch-show-open-or-close-all ()
  "Toggle between showing and hiding all messages in the thread."
  (interactive)
  (cashpw/notmuch--toggle-all-open)
  (if cashpw/notmuch-all-open
      (progn
        (universal-argument)
        (notmuch-show-open-or-close-all))
    (notmuch-show-open-or-close-all)))

(defun cashpw/notmuch--search-thread-has-tag-p (match-tag)
  "Whether or not the thread has a tag."
  (interactive)
  (let ((thread-tags (notmuch-search-get-tags)))
    (member match-tag thread-tags)))

(defun cashpw/notmuch-search-toggle-tag (tag)
  "Toggle the provided tag."
  (interactive)
  (if (member tag (notmuch-search-get-tags))
      (notmuch-search-tag (list (concat "-" tag)))
    (notmuch-search-tag (list (concat "+" tag)))))

(defun cashpw/notmuch--search-thread-toggle-tag (key)
  "Toggle the specified tag(s)."
  (interactive "k")
  (let ((tags (assoc key cashpw/notmuch-tag-alist)))
    (apply 'notmuch-search-tag (cdr tags))))

(defun cashpw/notmuch--tag-search (key name tags)
  "Return a notmuch search query named NAME, assigned to KEY, which queries the provided TAGS.

TAGS which start with \"-\" are excluded."
  (let ((query (string-join
                (mapcar
                 (lambda (tag)
                   (if (s-starts-with-p "-"
                                        tag)
                       (let ((tag (string-trim-left tag
                                                    "-")))
                         (s-lex-format "-tag:${tag}"))
                     (s-lex-format "tag:${tag}")))
                 tags)
                " AND ")))
    `(:key ,key
      :name ,name
      :query ,query)))

(defun cashpw/notmuch-search-super-archive (&optional beg end)
  "Super archive the selected thread; based on `notmuch-search-archive-thread'."
  (interactive (notmuch-interactive-region))
  (notmuch-search-tag
   cashpw/notmuch-super-archive-tags
   beg
   end)
  (when (eq beg
            end)
    (notmuch-search-next-thread)))

(defun cashpw/notmuch-search-todo ()
  "Capture the email at point in search for a todo."
  (interactive)
  ;; (notmuch-search-show-thread)
  ;; (goto-char (point-max))
  (cashpw/email-set-vars-from-search)
  (org-capture
   ;; goto
   nil
   ;; keys
   "teE")
  (message "Captured TODO for %s." cashpw/email--subject))

(defun cashpw/notmuch-search-todo-today ()
  "Capture the email at point in search for a todo."
  (interactive)
  ;; (notmuch-search-show-thread)
  ;; (goto-char (point-max))
  (cashpw/email-set-vars-from-search)
  (org-capture
   ;; goto
   nil
   ;; keys
   "tee")
  (message "Captured TODO for %s." cashpw/email--subject))

(after! notmuch
  (setq
   notmuch-wash-wrap-lines-length 100
   notmuch-saved-searches `(,(cashpw/notmuch--tag-search "a"
                                                         "Attention"
                                                         '("attn"
                                                           "-drive"
                                                           "-calendar"
                                                           "-drafts"
                                                           "-waiting"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "A"
                                                         "Abridged"
                                                         '("abridged"
                                                           "inbox"
                                                           "-trash"))
                            (:key "c"
                             :name "Calendar"
                             :query "tag:calendar AND -tag:trash AND (tag:inbox OR tag:attn)")
                            (:key "d"
                             :name "Drive"
                             :query "tag:drive AND -tag:trash AND (tag:inbox OR tag:attn)")
                            ,(cashpw/notmuch--tag-search "D"
                                                         "Drafts"
                                                         '("draft"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "i"
                                                         "Inbox"
                                                         '("inbox"
                                                           "-critique"
                                                           "-bug"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "I"
                                                         "Archive"
                                                         '("-inbox"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "m"
                                                         "To Me"
                                                         '("inbox"
                                                           "to-me"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "M"
                                                         "CC Me"
                                                         '("inbox"
                                                           "cc-me"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "r"
                                                         "To Read"
                                                         '("to-read"
                                                           "-systems"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "R"
                                                         "reporting chain"
                                                         '("inbox"
                                                           "management-chain"
                                                           "-trash"))
                            (:key "s"
                             :name "Sent (30 days)"
                             :query "tag:sent AND -tag:trash AND date:last_month..today")
                            ,(cashpw/notmuch--tag-search "S"
                                                         "Sent (all)"
                                                         '("sent"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "t"
                                                         "Team"
                                                         '("team"
                                                           "inbox"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "w"
                                                         "Waiting"
                                                         '("waiting"
                                                           "-trash"))
                            ,(cashpw/notmuch--tag-search "y"
                                                         "Systems"
                                                         '("inbox"
                                                           "systems")))
   +notmuch-home-function (lambda ()
                            (notmuch-search "tag:inbox"))
   notmuch-archive-tags '("-inbox"
                          "-unread")
   notmuch-search-line-faces '(("attn" . '(:foreground "red3"))
                               ("waiting" . '(:foreground "orange3"))
                               ("calendar" . '(:foreground "DeepSkyBlue3"))
                               ("to-read" . '(:foreground "magenta3")))
   ;; Superset of `notmuch-archive-tags' for super archiving.
   cashpw/notmuch-super-archive-tags (append
                                      notmuch-archive-tags
                                      '("-attn"
                                        "-waiting"
                                        "-to-read")))

  ;; Prevent wrapping at 70 characters in email composition.
  (add-hook! 'message-mode-hook 'turn-off-auto-fill)
  (add-hook! 'message-mode-hook 'visual-line-mode))

;; (use-package! org-msg
;;   :config
;;   (setq
;;    org-msg-options "html-postamble:nil H:6 num:nil ^:{[ toc:nil author:nil email:nil \\n:t]}"
;;    org-msg-startup "hidestars indent inlineimages"
;;    org-msg-greeting-fmt "\nHi%s,\n\n"
;;    ;; org-msg-recipient-names
;;    org-msg-default-alternatives '((new . (text html))
;;                                   (reply-to-html . (text html))
;;                                   (reply-to-text . (text)))
;;    org-msg-convert-citation t
;;    ;; org-msg-signature is redundant -- use `gnus-alias-identity-alist'
;;    )
;;   (org-msg-mode))

(defvar cashpw/email--message-id nil
  "Holds an email id. Used to pass data across indirect function calls.")
(defvar cashpw/email--to nil
  "Holds an email to. Used to pass data across indirect function calls.")
(defvar cashpw/email--subject nil
  "Holds an email subject. Used to pass data across indirect function calls.")
(defvar cashpw/email--from nil
  "Holds an email from. Used to pass data across indirect function calls.")

(defun cashpw/notmuch-search-get-properties ()
  "Return properties object for thread at point in search."
  (with-current-buffer (notmuch-show (notmuch-search-find-thread-id t))
    (save-excursion
      (goto-char (point-max))
      (prog1
          (cond
           ((eq major-mode 'notmuch-show-mode)
	    (notmuch-show-get-message-properties))
	   ((eq major-mode 'notmuch-tree-mode)
	    (notmuch-tree-get-message-properties))
	   (t nil))
        (kill-buffer)))))

(defun cashpw/email-get-to ()
  "Return To value for message buffer, or message at point."
  (or
   cashpw/email--to
   (notmuch-show-get-to)
   (message-field-value "To")))

(defun cashpw/email-get-message-id ()
  "Return To value for message buffer, or message at point."
  (or
   cashpw/email--message-id
   (notmuch-show-get-message-id)))

(defun cashpw/email-get-from ()
  "Return From value for message buffer, or message at point."
  (or
   cashpw/email--from
   (notmuch-show-get-from)
   (message-field-value "From")))

(defun cashpw/email-get-subject ()
  "Return From value for message buffer, or message at point."
  (or
   cashpw/email--subject
   (notmuch-show-get-subject)
   (message-field-value "Subject")))

(defun cashpw/email--invalidate-vars ()
  "Invalidate email context variables."
  (setq
   cashpw/email--message-id nil
   cashpw/email--to nil
   cashpw/email--from nil
   cashpw/email--subject nil))

(defun cashpw/email-set-vars-from-search ()
  "Set email context variables."
  (cashpw/email--invalidate-vars)
  (when-let ((thread-id (notmuch-search-find-thread-id)))
    (with-current-buffer
        (notmuch-show
         thread-id
         nil
         (current-buffer))
      (save-excursion
        (goto-char (point-max))
        (setq
         cashpw/email--message-id (cashpw/email-get-message-id)
         cashpw/email--to (cashpw/email-get-to)
         cashpw/email--from (cashpw/email-get-from)
         cashpw/email--subject (cashpw/email-get-subject)))
      (notmuch-bury-or-kill-this-buffer))))

(defun cashpw/compose-mail-org ()
  (interactive)
  (compose-mail)
  (message-goto-body)
  (setq *compose-html-org* t)
  (org-mode))

;; Deprecated in favor of org-mime `org-mime-edit-mail-in-org-mode'
(defun cashpw/mail-toggle-org-message-mode ()
  (interactive)
  (if (derived-mode-p 'message-mode)
      (progn
        (setq *compose-html-org* t)
        (org-mode)
        (message "enabled org-mode"))
    (progn
      (setq *compose-html-org* nil)
      (notmuch-message-mode)
      (message "enabled notmuch-message-mode"))))

(defun cashpw/email--address-properties (address)
  "Return plist of ADDRESS contents."
  (when (string-match "\\([^<]*\\) <\\([^@]*\\)@\\([^>]*\\)>" address)
    (list
     :name (match-string 1 address)
     :email (format "%s@%s" (match-string 2 address) (match-string 3 address))
     :username (match-string 2 address)
     :domain (match-string 3 address))))

(defun cashpw/mail-get-short-address (address)
  "Returns \"foo@\" for an ADDRESS of \"Foo <foo@bar.com>\"."
  (cond
   ((not (string-match "<" address))
    address)
   (t
    (replace-regexp-in-string ".*<\\(.*\\)@.*>" "\\1@" address))))

(defun cashpw/mail-create-follow-up-todo ()
  (interactive)
  (cashpw/email-set-vars-from-search)
  (org-capture
   ;; goto
   nil
   ;; keys
   "tef")
  (message "Captured follow-up TODO for %s." cashpw/email--subject))

(defun cashpw/email--close-mail-buffer ()
  "TODO"
  (message "killing sent email buffers")
  (let ((buffers-to-kill (-filter (lambda (buffer)
                                         (string-match-p "sent mail" (buffer-name buffer)))
                                       (buffer-list))))
    (dolist (buffer-to-kill buffers-to-kill)
      (kill-buffer buffer-to-kill))))

(defun cashpw/email--close-mail-buffer-and-clean-up-hook ()
  "TODO"
  (message "and-clean-up-hook")
  (cashpw/email--close-mail-buffer)
  (remove-hook! 'org-capture-after-finalize-hook
    'cashpw/email--close-mail-buffer-and-clean-up-hook))

(defun cashpw/mail--close-mail-buffer-after-capture ()
  "TODO"
  (add-hook! 'org-capture-after-finalize-hook
             'cashpw/email--close-mail-buffer-and-clean-up-hook))

(defun cashpw/mail--maybe-create-follow-up-todo ()
  "Conditionally create follow-up todo based on user choice."
  (when (y-or-n-p "Create follow-up TODO?")
    (cashpw/mail--close-mail-buffer-after-capture)
    (cashpw/mail-create-follow-up-todo)))

(defun cashpw/message-send-and-exit ()
  (interactive)
  (org-mime-htmlize)
  (notmuch-mua-send)
  (cashpw/mail--maybe-create-follow-up-todo)
  (kill-buffer (current-buffer)))

(setq
 sendmail-program "gmi"
 ;; Don't save outgoing mail locally as it's already stored by GMail
 notmuch-fcc-dirs nil
 cashpw/email-address--gmail "cashbweaver@gmail.com"
 cashpw/email-address--personal "cash@cashpw.com")

(defcustom cashpw/email-addresses--personal `(,cashpw/email-address--personal
                                              ,cashpw/email-address--gmail)
  "Personal email addresses.")

(defun cashpw/configure-sendmail ()
  "Set appropriate sendmail arguments."
  (let ((from
         (replace-regexp-in-string
          "[^<]*<\\(.*\\)>" "\\1" (message-fetch-field "from" t))))
    (setq message-sendmail-extra-arguments
          (cond
           ((string= from cashpw/email-address--gmail)
            (cashpw/configure-sendmail--gmail))
           ((string= from cashpw/email-address--personal)
            (cashpw/configure-sendmail--personal))))))

(defun cashpw/configure-sendmail--gmail ()
  "Configure sendmail for my personal Gmail account."
  (setq
   sendmail-program "gmi"
   message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/mail/cashbweaver.gmail")))

(defun cashpw/configure-sendmail--personal ()
  "Configure sendmail for my personal email account."
  (setq
   sendmail-program "gmi"
   message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/mail/cash.cashpw")))

(add-hook 'message-send-hook 'cashpw/configure-sendmail)

(defun cashpw/send-mail-function (&rest args)
  "Wrapper method for `send-mail-function' for easy overriding in work environment."
  ;;(apply #'sendmail-query-once args)
  (apply #'smtpmail-send-it args))

(defun cashpw/message-send-mail-function (&rest args)
  "Wrapper method for `message-send-mail-function' for easy overriding in work environment."
  ;; (apply #'message--default-send-mail-function args)
  (apply #'smtpmail-send-it args))

(setq
 send-mail-function #'cashpw/send-mail-function
 message-send-mail-function #'cashpw/message-send-mail-function)

(map! :map message-mode-map "C-c C-c" #'cashpw/message-send-and-exit)

(map!
 :map message-mode-map
 :localleader
 "e"
 #'org-mime-edit-mail-in-org-mode
 "h t"
 #'cashpw/email-set-to
 "h T"
 #'cashpw/email-append-to
 "h c"
 #'cashpw/email-set-cc
 "h C"
 #'cashpw/email-append-cc
 "@"
 #'gnus-alias-select-identity)

(after!
  notmuch
  ;; Keep in alphabetical order.
  (map! :map notmuch-message-mode-map "C-c C-c" #'cashpw/message-send-and-exit)

  (map!
   :map notmuch-message-mode-map
   :localleader
   "e"
   #'org-mime-edit-mail-in-org-mode)

  (map!
   :map notmuch-show-mode-map "M-RET" #'cashpw/notmuch-show-open-or-close-all)

  ;; Reply-all should be the default.
  (evil-define-key
    'normal notmuch-show-mode-map "r"
    (cmd! (notmuch-show-reply))) ;;(gnus-alias-select-identity)))
  (evil-define-key 'normal notmuch-show-mode-map "R" #'notmuch-show-reply-sender)
  (evil-define-key
    'normal notmuch-search-mode-map "r" #'notmuch-search-reply-to-thread)
  (evil-define-key
    'normal notmuch-search-mode-map "R" #'notmuch-search-reply-to-thread-sender)

  ;; Easy archive for my most-used tags.
  (evil-define-key
    'normal notmuch-search-mode-map "A" 'notmuch-search-archive-thread)
  (evil-define-key
    'normal notmuch-search-mode-map "a" 'cashpw/notmuch-search-super-archive)
  (evil-define-key
    'visual notmuch-search-mode-map "a" 'cashpw/notmuch-search-super-archive)

  ;; Create todos
  (evil-define-key 'normal notmuch-search-mode-map "t" nil)
  ;; Note this unbinds `notmuch-search-filter-by-tag'.
  (evil-define-key
    'normal notmuch-search-mode-map "tt" 'cashpw/notmuch-search-todo-today)
  (evil-define-key
    'normal notmuch-search-mode-map "tT" 'cashpw/notmuch-search-todo)

  ;; Helpers for toggling often-used tags.
  (evil-define-key 'normal notmuch-search-mode-map "T" nil)
  ;; (cashpw/evil-lambda-key
  ;;  'normal notmuch-search-mode-map "T0"
  ;;  '(lambda ()
  ;;     "Toggle p0"
  ;;     (interactive)
  ;;     (cashpw/notmuch-search-toggle-tag "p0")))
  ;; (cashpw/evil-lambda-key
  ;;  'normal notmuch-search-mode-map "Tr"
  ;;  '(lambda ()
  ;;     "Toggle Read!"
  ;;     (interactive)
  ;;     (cashpw/notmuch-search-toggle-tag "Read!")))
  ;; (cashpw/evil-lambda-key
  ;;  'normal notmuch-search-mode-map "Tw"
  ;;  '(lambda ()
  ;;     "Toggle waiting"
  ;;     (interactive)
  ;;     (cashpw/notmuch-search-toggle-tag "waiting")))
  )

(defun cashpw/pandoc--convert-buffer-from-markdown-to-org-in-place ()
  "Converts the current buffer to org-mode in place."
  (interactive)
  (let ((buffer-content (buffer-string))
        (tmp-file
         (format "/tmp/%s.md" (format-time-string "%s" (current-time)))))
    (with-temp-buffer
      (insert buffer-content)
      (write-file tmp-file))
    (erase-buffer)
    (insert
     (shell-command-to-string
      (concat
       (format "pandoc --wrap=none -f markdown -t org %s" tmp-file)
       ;; Remove :PROPERTIES: drawers beneath headings
       " | sed -E '/^[[:space:]]*:/d'")))
    (org-mode)))

(defun cashpw/pandoc-cli (command)
  (let ((pandoc-command (format "pandoc %s" command)))
    (message "Running %s" pandoc-command)
    (shell-command-to-string pandoc-command)))

(defun cashpw/pandoc-convert (text source-format target-format)
  "Convert TEXT from SOURCE-FORMAT to TARGET-FORMAT."
  (cashpw/pandoc-cli
   (s-lex-format "-f ${source-format} -t ${target-format} <<< \"${text}\"")))

(defun cashpw/pandoc-convert-via-file (text source-format target-format)
  "Convert TEXT from SOURCE-FORMAT to TARGET-FORMAT."
  (let ((tmp-input-file-path
         (format "/tmp/pandoc-tmp-input-%s" (format-time-string "%s")))
        (tmp-output-file-path
         (format "/tmp/pandoc-tmp-output-%s" (format-time-string "%s"))))
    (with-temp-buffer text
      (let ((coding-system-for-write 'utf-8))
        (write-file tmp-input-file-path)))
    (cashpw/pandoc-cli
     (s-lex-format
      "${tmp-input-file-path} -f ${source-format} -t ${target-format} -o ${tmp-output-file-path}"))
    (with-current-buffer (find-file-noselect tmp-output-file-path)
      (buffer-substring-no-properties (point-min) (point-max)))))

(after!
  elfeed (elfeed-set-timeout 90)
  (setq
   elfeed-use-curl t
   elfeed-curl-extra-arguments '("--insecure")
   elfeed-sort-order 'ascending
   ;; elfeed-search-sort-function 'cashpw/elfeed-search-compare-by-date
   elfeed-search-sort-function 'cashpw/elfeed-search-compare-by-title
   elfeed-db-directory (format "%s/elfeed" cashpw/path--notes-dir))

  (evil-define-key
    'normal
    elfeed-search-mode-map
    "+"
    #'cashpw/elfeed-search-set-tag
    "a"
    #'elfeed-search-untag-all-unread
    "u"
    #'elfeed-update
    "f"
    (cmd! (if (elfeed-tagged-p 'star (elfeed-search-selected :ignore-region))
              (elfeed-search-untag-all 'star)
            (elfeed-search-tag-all 'star)))
    (kbd "M-RET")
    #'elfeed-search-browse-url
    (kbd "o A")
    (cmd! (setq elfeed-sort-order 'ascending) (elfeed-search-update--force))
    (kbd "o D")
    (cmd! (setq elfeed-sort-order 'descending) (elfeed-search-update--force))
    (kbd "o t")
    (cmd!
     (setq elfeed-search-sort-function 'cashpw/elfeed-search-compare-by-title)
     (elfeed-search-update--force))
    (kbd "o d")
    (cmd!
     (setq elfeed-search-sort-function 'cashpw/elfeed-search-compare-by-date)
     (elfeed-search-update--force))
    (kbd "J f")
    (cmd! (cashpw/elfeed-search-for-feed nil 'unread-only))
    (kbd "J t")
    (cmd! (cashpw/elfeed-search-for-tags nil 'unread-only))))

(defun cashpw/elfeed-search-compare-by-date (a b)
  "Return non-nil if A is newer than B."
  (let ((date-a (elfeed-entry-date a))
        (date-b (elfeed-entry-date b)))
    (if (= date-a date-b)
        (string< (prin1-to-string b) (prin1-to-string a))
      (> date-a date-b))))

(defun cashpw/elfeed-search-compare-by-title (a b)
  "Return non-nil if A is newer than B."
  (string> (elfeed-entry-title a) (elfeed-entry-title b)))

(setq
 cashpw/reading-list-tags
 (string-split
  (shell-command-to-string
   (format
    "rgrep \" :[a-zA-Z0-9_:]*:$\" %s | sed 's/.*\\(:[a-zA-Z0-9_:]*:\\)/\\1/' | uniq | tr '\\n' ':' | sed 's/:::/:/g' | sed 's/:/\\n/g' | uniq | tr '\\n' ':'"
    cashpw/path--reading-list))
  ":" 'omit-nulls))

(defun cashpw/elfeed-search-set-tag (tags)
  "Set tags on elfeed entries in search view."
  (interactive (list
                (completing-read-multiple
                 "Tag(s): "
                 (append (elfeed-db-get-all-tags) cashpw/reading-list-tags))))
  (let ((elfeed-search-remain-on-entry t))
    (dolist (tag tags)
      (elfeed-search-tag-all (intern tag)))))

(defun cashpw/elfeed-search-for-tags (&optional tags unread-only)
  "Search for Elfeed entries tagged with TAGS."
  (interactive)
  (let* ((tags
          (or tags
              (completing-read-multiple "Tags: " (elfeed-db-get-all-tags))))
         (search-filter
          (string-join (append
                        (when unread-only
                          '("+unread"))
                        (mapcar (lambda (tag) (concat "+" tag)) tags))
                       " ")))
    (elfeed-search-set-filter search-filter)
    (elfeed-search-update)))

(defun cashpw/elfeed-search-for-feed (&optional feed unread-only)
  "Search for Elfeed entries from feed."
  (interactive)
  (let* ((feed
          (or feed
              (completing-read
               "Feed: "
               (let (feed-titles)
                 (maphash
                  (lambda (_ value)
                    (push (elfeed-feed-title value) feed-titles)
                    value)
                  elfeed-db-feeds)
                 (reverse (delete nil feed-titles)))))))
    (elfeed-search-set-filter
     (format "=%s%s"
             (replace-regexp-in-string "[^a-zA-Z0-9]" "." feed)
             (if unread-only
                 " +unread"
               "")))
    (elfeed-search-update)))

;; Alphapapa's solution to speed up elfeed-update
;;
;; Source: https://github.com/skeeto/elfeed/issues/293#issuecomment-425627688

(after!
  elfeed
  (defvar ap/elfeed-update-complete-hook nil
    "Functions called with no arguments when `elfeed-update' is finished.")

  (defvar ap/elfeed-updates-in-progress 0
    "Number of feed updates in-progress.")

  (defvar ap/elfeed-search-update-filter nil
    "The filter when `elfeed-update' is called.")

  (defun ap/elfeed-update-complete-hook (&rest ignore)
    "When update queue is empty, run `ap/elfeed-update-complete-hook' functions."
    (when (= 0 ap/elfeed-updates-in-progress)
      (run-hooks 'ap/elfeed-update-complete-hook)))

  (add-hook 'elfeed-update-hooks #'ap/elfeed-update-complete-hook)

  (defun ap/elfeed-update-message-completed (&rest _ignore)
    (message "Feeds updated"))

  (add-hook 'ap/elfeed-update-complete-hook #'ap/elfeed-update-message-completed)

  (defun ap/elfeed-search-update-restore-filter (&rest ignore)
    "Restore filter after feeds update."
    (when ap/elfeed-search-update-filter
      (elfeed-search-set-filter ap/elfeed-search-update-filter)
      (setq ap/elfeed-search-update-filter nil)))

  (add-hook
   'ap/elfeed-update-complete-hook #'ap/elfeed-search-update-restore-filter)

  (defun ap/elfeed-search-update-save-filter (&rest ignore)
    "Save and change the filter while updating."
    (setq ap/elfeed-search-update-filter elfeed-search-filter)
    (setq elfeed-search-filter "#0"))

  ;; NOTE: It would be better if this hook were run before starting the feed updates, but in
  ;; `elfeed-update', it happens afterward.
  (add-hook 'elfeed-update-init-hooks #'ap/elfeed-search-update-save-filter)

  (defun ap/elfeed-update-counter-inc (&rest ignore)
    (cl-incf ap/elfeed-updates-in-progress))

  (advice-add #'elfeed-update-feed :before #'ap/elfeed-update-counter-inc)

  (defun ap/elfeed-update-counter-dec (&rest ignore)
    (cl-decf ap/elfeed-updates-in-progress)
    (when (< ap/elfeed-updates-in-progress 0)
      ;; Just in case
      (setq ap/elfeed-updates-in-progress 0)))

  (add-hook 'elfeed-update-hooks #'ap/elfeed-update-counter-dec))

(use-package! elfeed-protocol
  :after elfeed
  :custom
  (elfeed-log-level 'debug)
  (elfeed-protocol-fever-update-unread-only nil)
  (elfeed-protocol-fever-fetch-category-as-tag t)
  (elfeed-protocol-enabled-protocols '(fever newsblur owncloud ttrss))
  (elfeed-protocol-feeds `(("fever+http://fever@rss.cashpw.com"
                            :api-url "http://rss.cashpw.com/fever/"
                            :password ,(secret-get "rss.cashpw.com"))))
  :config
  (elfeed-protocol-enable))

;; (after! elfeed-org
;;   (setq
;;    rmh-elfeed-org-files `(,(concat cashpw/path--notes-dir "/elfeed.org"))))

(use-package! gnuplot)

(use-package! llm-prompts)

;; Based on https://github.com/danielmiessler/fabric/blob/main/patterns/extract_article_wisdom/system.md
(defvar llm-prompts-prompt--summarize
  "Please carefully review the following text with the aim of providing accurate and representative responses to the following requirements, in order:

1. In a section titled \"Speakers\": Identify the speaker, or speakers, in a bullet-point list.
2. In a section titled \"Thesis\": Identify and describe the thesis statement(s) in a bullet point list.
3. In a section titled \"Summary\": Summarize the content in 300 words or less. Prefer bullet-point lists where appropriate.
4. In a section titled \"References\": List all of the external sources referenced in the text. This includes books, papers, articles, songs, movies, etc.
5. In a section titled \"Ideas\": List up to 30 key, representative, and distinct ideas from the text in an bullet-point list.
6. In a section titled \"Next steps\": List up to 10 next steps mentioned by the speakers"
  "LLM prompt to summarize content.")

(defvar fabric-command "~/third_party/fabric/fabric")

(defun cashpw/fabric-transcript-youtube (youtube-url)
  "(Return|Insert) the transcript of YOUTUBE-URL.

Insert the transcript if run interactively."
  (interactive "sYouTube URL: ")
  (let ((transcript
         (shell-command-to-string
          (format "%s --youtube=%s" fabric-command youtube-url))))
    (if (interactive-p)
        (insert transcript)
      transcript)))

(defun cashpw/youtube--video-id-from-url (url)
  "Return the video id within the URL.

Example: https://www.youtube.com/watch?v=xzseFskewlE"
  (when (string-match ".*youtube.com\\/watch\\?v=\\([^&]*\\)\\(&.*\\)?" url)
    (match-string 1 url)))

(defun cashpw/ytt-api-transcript (youtube-video-id)
  "Return transcript of YOUTUBE-VIDEO-ID."
   (shell-command-to-string
    (concat
    "source ~/third_party/yt-transcripts/bin/activate;"
    (format
    "python -c \"from youtube_transcript_api import YouTubeTranscriptApi; from functools import reduce; print(reduce(lambda acc, snippet: acc + ' ' + snippet, map(lambda snippet: snippet.text, YouTubeTranscriptApi().fetch('%s').snippets), ''))\""
    youtube-video-id))))

(defun llm-prompts-prompt-extract-wisdom-yt (youtube-url)
  "Return prompt."
  (format "%s

%s"
          llm-prompts-prompt--summarize
          (cashpw/ytt-api-transcript (cashpw/youtube--video-id-from-url youtube-url))))

(use-package!
    gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-track-media t)

  :config
  (setq-default
   gptel-show-progress-in-mode-line t
   gptel-mode-line--indicator-querying " "
   gptel-mode-line--indicator-responding "💬 "
   gptel-backend
   (gptel-make-gemini
       "Gemini"
     :key
     (secret-get
      (if (cashpw/machine-p 'work-cloudtop)
          "corporate-gemini"
        "personal-gemini"))
     :stream t)
   gptel-model 'gemini-2.5-pro-preview-05-06
   gptel-quick-backend gptel-backend
   gptel-quick-model 'gemini-2.5-flash-preview-05-20)

  (defun gptel-mode-line--indicator (mode)
    "Return indicator string for MODE."
    (pcase mode
      ('querying gptel-mode-line--indicator-querying)
      ('responding gptel-mode-line--indicator-responding)
      (t "")))
  (defun gptel-mode-line (command mode)
    "Update mode line to COMMAND (show|hide) indicator for MODE."
    (when gptel-show-progress-in-mode-line
      (let ((indicator (list t (gptel-mode-line--indicator mode))))
        (pcase command
          ('show (cl-pushnew indicator global-mode-string :test #'equal))
          ('hide
           (setf global-mode-string (remove indicator global-mode-string)))))
      (force-mode-line-update t)))
  (defun gptel-mode-line--hide-all (&rest _)
    (gptel-mode-line 'hide 'querying)
    (gptel-mode-line 'hide 'responding))
  (defun gptel-mode-line--show-querying ()
    (gptel-mode-line--hide-all)
    (gptel-mode-line 'show 'querying))
  (defun gptel-mode-line--show-responding ()
    (gptel-mode-line--hide-all)
    (gptel-mode-line 'show 'responding))
  (add-hook! 'gptel-post-request-hook 'gptel-mode-line--show-querying)
  (add-hook! 'gptel-pre-response-hook 'gptel-mode-line--show-responding)
  (add-hook! 'gptel-post-response-functions 'gptel-mode-line--hide-all)

  (defun cashpw/gptel-context-add-file-glob (pattern)
    "Add glob of files matched by PATTERN."
    (interactive "sPattern: ")
    (dolist (file (f-glob pattern))
      (gptel-context-add-file file)))

  (defun cashpw/gptel-send (prompt)
    "Invoke `gptel-send' with specific PROMPT."
    (interactive (list (llm-prompts-select)))
    (let ((gptel--system-message prompt))
      (gptel-send))))

(defun cashpw/gptel-kill-curl-process ()
  "Kill any running gptel curl process."
  (interactive)
  (kill-process "gptel-curl"))

(after!
  (:and gptel whisper) (setq cashpw/gptel-after-whisper nil)

  (defun cashpw/whisper-run-and-cue-gptel ()
    (interactive)
    (setq cashpw/gptel-after-whisper t)
    (whisper-run))

  (defun cashpw/maybe-gptel-after-whisper ()
    (when cashpw/gptel-after-whisper
      (gptel-send)
      (setq cashpw/gptel-after-whisper nil)))

  (add-hook 'whisper-post-insert-hook #'cashpw/maybe-gptel-after-whisper))

(setq
 ;; PERF
 diff-hl-flydiff-delay 3.0)

(use-package!
    commit-message
  :custom
  (commit-message-builder-fn
   (defun cashpw/commit-message-builder-fn ()
     "Return commit message"
     (let ((buffer-path
            (if-let* ((filename (or (buffer-file-name (buffer-base-buffer))
                                    (bound-and-true-p list-buffers-directory))))
                (abbreviate-file-name
                 filename))))
       (cond
        ((file-in-directory-p
          buffer-path
          cashpw/path--notes-dir)
         (cashpw/commit-message-builder--notes))
        (t
         (cashpw/commit-message-builder))))))

  :config
  (defun cashpw/commit-message-builder ()
    "Return commit message"
    (let ((category (commit-message-read-category))
          (scope (commit-message-read-scope))
          (breaking (commit-message-read-breaking)))
      (with-slots
          (short) category
        (let ((wrapped-scope
               (if scope
                   (format "(%s)" scope)
                 ""))
              (breaking-bang
               (if breaking
                   "!"
                 "")))
          (s-lex-format "${short}${wrapped-scope}${breaking-bang}: CURSOR

**What?**

TODO

**Why?**

TODO")))))

  (defun cashpw/commit-message-builder--notes ()
    "Return commit message"
    (let* ((commit-message-categories cashpw/commit-message-notes-categories)
           (category (commit-message-read-category)))
      (with-slots
          (short) category
        (cond
         ((string= short "misc")
          "feat: Misc")
         ((string= short "flashcards")
          "feat: Flashcards")
         (t
          (s-lex-format "${short}: CURSOR"))))))

  (setq cashpw/commit-message-notes-categories (list
                                                (commit-message-category :name "Flashcards" :short "flashcards")
                                                (commit-message-category :name "Misc" :short "misc")
                                                (commit-message-category :name "Fix" :short "fix" :aliases '("Bug"))
                                                (commit-message-category :name "Feature" :short "feat" :aliases '("Add"))))
  (remove-hook 'git-commit-setup-hook '+vc-start-in-insert-state-maybe-h)
  (add-hook 'git-commit-setup-hook 'commit-message-maybe-insert-message)
  (add-hook
   'git-commit-setup-hook
   (defun cashpw/vc-start-in-insert-state ()
     (evil-insert-state))))

(setq
 company-idle-delay 1
 +vertico-company-completion-styles '(orderless)
 ;; completion-styles '(orderless)
 ;; orderless-matching-styles '(orderless-literal
 ;;                             orderless-prefixes
 ;;                             orderless-initialism
 ;;                             orderless-regexp)
 ;; company-dabbrev-ignore-case t
 completion-ignore-case t)

(defun completion--capf-wrapper (fun which)
  ;; FIXME: The safe/misbehave handling assumes that a given function will
  ;; always return the same kind of data, but this breaks down with functions
  ;; like comint-completion-at-point or mh-letter-completion-at-point, which
  ;; could be sometimes safe and sometimes misbehaving (and sometimes neither).
  (if (pcase which
        ('all t)
        ('safe (member fun completion--capf-safe-funs))
        ('optimist (not (member fun completion--capf-misbehave-funs))))
      (let ((res (funcall fun)))
        (cond
         ((and (consp res) (not (functionp res)))
          (unless (member fun completion--capf-safe-funs)
            (push fun completion--capf-safe-funs))
          (and (eq 'no (plist-get (nthcdr 3 res) :exclusive))
               ;; FIXME: Here we'd need to decide whether there are
               ;; valid completions against the current text.  But this depends
               ;; on the actual completion UI (e.g. with the default completion
               ;; it depends on completion-style) ;-(
               ;; We approximate this result by checking whether prefix
               ;; completion might work, which means that non-prefix completion
               ;; will not work (or not right) for completion functions that
               ;; are non-exclusive.

               ;; cashpw's changes
               (if (eq #'org-roam-complete-everywhere fun)
                   (null (let ((target (buffer-substring-no-properties (nth 0 res)
                                                                       (nth 1 res)))
                               (candidates (nth 2 res)))
                           (--any
                            (string-match-p target it)
                            candidates)))
                 (null (try-completion (buffer-substring-no-properties
                                        (car res) (point))
                                       (nth 2 res)
                                       (plist-get (nthcdr 3 res) :predicate))))
               (setq res nil)))
         ((not (or (listp res) (functionp res)))
          (unless (member fun completion--capf-misbehave-funs)
            (message
             "Completion function %S uses a deprecated calling convention" fun)
            (push fun completion--capf-misbehave-funs))))
        (if res (cons fun res)))))

;; (eglot)

(unless (cashpw/machine-p 'personal-phone)
  (unless (executable-find "emacs-lsp-booster")
    (cashpw/error "Cannot find 'emacs-lsp-booster' executable."))
  (use-package! eglot-booster :after eglot :config (eglot-booster-mode)))

(defun cashpw/eglot-pause ()
  "Pause eglot; see `cashpw/eglot-unpause'."
  (interactive)
  (eglot-shutdown-all)
  (advice-add
   'eglot--maybe-activate-editing-mode
   :override #'ignore)
  (advice-add
   'eglot--connect
   :override #'ignore)
  (advice-add
   'eglot-ensure
   :override #'ignore))

(defun cashpw/eglot-unpause ()
  "Unpause eglot; see `cashpw/eglot-pause'."
  (interactive)
  (advice-remove
   'eglot--connect
   #'ignore)
  (advice-remove
   'eglot--maybe-activate-editing-mode
   #'ignore)
  (advice-remove
   'eglot-ensure
   #'ignore))

;; (use-package! flycheck-vale
;;   :config
;;   (flycheck-vale-setup))

;; (set-eglot-client!
;; 'org-mode
;; '("vale-ls"))

;; (add-hook! 'org-mode-hook
;; #'eglot-ensure)

;; Doom Emacs provides flycheck
;; (after! flycheck
;;   (setq
;;    flycheck-idle-change-delay 3)
;;   (add-hook 'after-init-hook
;;             #'global-flycheck-mode))

;; (use-package! treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   (treesit-auto-langs '(cpp))
;;   :config
;;   (setq
;;    cashpw/treesit-recipe-c (make-treesit-auto-recipe
;;                             :lang 'c
;;                             :ts-mode 'c-ts-mode
;;                             :remap 'c-mode
;;                             :url "https://github.com/tree-sitter/tree-sitter-c"
;;                             :revision "v0.21.4"
;;                             :ext "\\.c\\'")
;;    cashpw/treesit-recipe-c++ (make-treesit-auto-recipe
;;                               :lang 'cpp
;;                               :ts-mode 'c++-ts-mode
;;                               :remap 'c++-mode
;;                               :url "https://github.com/tree-sitter/tree-sitter-cpp"
;;                               :revision "v0.22.0"
;;                               :ext "\\.cc\\'")
;;    treesit-auto-recipe-list `(,cashpw/treesit-recipe-c
;;                               ,cashpw/treesit-recipe-c++))
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(after! embark
  (define-key global-map (kbd "M-E") #'embark-act))

(setq
 flutter-sdk-path "/home/cashweaver/snap/flutter/common/flutter"
 lsp-dart-flutter-sdk flutter-sdk-path
 lsp-dart-sdk-dir (s-lex-format "${flutter-sdk-path}/bin/cache/dart-sdk"))

(use-package! aggressive-indent
  :config
  (add-hook
   'emacs-lisp-mode-hook
   #'aggressive-indent-mode))

(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(setq
 org-modules '(ol-doi
               ol-w3m
               ol-bibtex
               ol-eww
               ol-notmuch))

(after! org-crypt
  (advice-add
   'org-encrypt-entries
   :override #'ignore))

(use-package!
    zotra
  :custom
  (zotra-backend 'zotra-server)
  (zotra-use-curl t)
  (zotra-default-bibliography cashpw/path--notes-bibliography)
  (zotra-local-server-directory (f-expand "~/.local/share/zotra-server/"))
  (zotra-after-get-bibtex-entry-hook nil)
  (zotra-default-entry-format "biblatex")

  (bibtex-dialect 'biblatex)
  (bibtex-entry-format
   '(opts-or-alts
     ;; required-fields
     numerical-fields))
  (bibtex-autokey-edit-before-use nil)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "")
  (bibtex-autokey-year-title-separator "")
  (bibtex-autokey-titleword-separator "")
  (bibtex-autokey-titlewords 5)
  (bibtex-autokey-titlewords-stretch 1)
  (bibtex-autokey-titleword-length 'infty)
  (bibtex-autokey-titleword-ignore
   '("A"
     "An"
     "On"
     "The"
     "Eine?"
     "Der"
     "Die"
     "Das"
     ".*[^[:upper:][:lower:]0-9].*"))
  (bibtex-autokey-name-case-convert-function #'identity)
  (bibtex-autokey-titleword-case-convert-function
   (lambda (str) (titlecase--string str nil)))

  :config
  (add-hook
   'zotra-after-get-bibtex-entry-hook 'cashpw/bibtex-clean-entry-override-key))

(defun cashpw/replace-tabs-with-two-spaces ()
  "Replace all tabs in buffer with two spaces."
  (interactive)
  (cashpw/replace-regexp-in-buffer "\t" "  "))

(defun cashpw/bibtex-clean-entry-override-key ()
  "Invoke `bibtex-clean-entry' with key override."
  (interactive)
  (message "cashpw/bibtex-clean-entry-override-key")
  (bibtex-clean-entry (bibtex-generate-autokey))
  (cashpw/replace-tabs-with-two-spaces))

(defun cashpw/bibtex--read-and-set-required-fields (buffer point-or-marker)
  "Prompt user to set missing required fields in BUFFER at POINT-OR-MARKER."
  (message "cashpw/bibtex--read-and-set-required-fields")
  (save-excursion
    (let ((window
           (select-window
            (display-buffer-in-side-window
             buffer '((side . bottom))))))
      (with-current-buffer (window-buffer window)
        (goto-char point-or-marker)
        (when (looking-at bibtex-entry-maybe-empty-head)
          (with-restriction
              (point)
              (save-excursion
                (evil-jump-item)
                (1+ (point)))
            (bibtex-entry-update)
            (while (let ((case-fold-search nil))
                     (search-forward-regexp "^\s*\\([a-z]\\|ALT\\)" nil t))
              (let* ((field
                      (replace-regexp-in-string
                       "^\s*\\([^\s]*\\).*" "\\1" (org-current-line-string)))
                     (new-value
                      (read-string (format "%s (leave empty to skip): " field)))
                     (new-line
                      (when new-value
                        (replace-regexp-in-string
                         "{}" (format "{%s}" new-value) (org-current-line-string)))))
                (when new-line
                  (delete-line)
                  (insert new-line)
                  (newline))))
            (bibtex-format-entry))))
      (+workspace/close-window-or-workspace))))

(defun cashpw/bibtex-generate-autokey ()
  "Return a key for the current bibtex entry.

The key is in the form: (authors|journal)_title_year."
  (message "cashpw/bibtex-generate-autokey")
  (when
      ;; Missing required fields
      (or (string-empty-p
           (bibtex-autokey-get-names))
          (not
           (ignore-errors
             (bibtex-autokey-get-year))))
    (cashpw/bibtex--read-and-set-required-fields
     (current-buffer)
     (point)))
  (let* ((journal (bibtex-autokey-get-field "journal"))
         (names (bibtex-autokey-get-names))
         (title (bibtex-autokey-get-title))
         (year
          (or (ignore-errors
                (bibtex-autokey-get-year))
              ""))
         (key
          (substring (concat
                      (cond
                       ((not (string-empty-p names))
                        (s-lex-format "${names}_"))
                       ((not (string-empty-p journal))
                        (s-lex-format "${journal}_")))
                      (if (string-empty-p title)
                          ""
                        (s-lex-format "${title}_"))
                      (if (string-empty-p year)
                          ""
                        (s-lex-format "${year}_")))
                     0 -1)))
    (if bibtex-autokey-before-presentation-function
        (funcall bibtex-autokey-before-presentation-function key)
      key)))

(advice-add 'bibtex-generate-autokey :override 'cashpw/bibtex-generate-autokey)

(defun cashpw/zotra-add-entry-from-url (url)
  "Add an entry for the current page's url."
  (interactive)
  (let
      ((rewriters
        (list
         (cons
          "https:\\/\\/www.youtube.com\\/supported_browsers\\?next_url=\\(.*\\)"
          (lambda () (url-unhex-string (match-string 1 url)))))))
    (zotra-add-entry
     (if-let ((rewriter (--first (string-match (car it) url) rewriters)))
         (funcall (cdr rewriter))
       url))))

(defun cashpw/zotra-add-entry-from-eww ()
  "Add an entry for the current eww buffer."
  (interactive)
  (let ((url (plist-get eww-data :url)))
    (cond
     ((string-match-p "isbnsearch\\.org" url)
      (zotra-add-entry (cashpw/isbn-select-from-buffer)))
     (t
      (cashpw/zotra-add-entry-from-url url)))))

(use-package!
    org-clock-act-on-overtime
  :custom
  (add-hook 'org-clock-act-on-overtime-hook #'cashpw/notify-overtime)
  (add-hook 'org-clock-out-hook #'cashpw/notify-overtime--reset)
  (org-clock-act-on-overtime-mode 'enable))

(defvar cashpw/notify-overtime--have-notified nil
  "Non-nil if we've already notified for the current overtime clock.")

(defun cashpw/notify-overtime ()
  "Notify that the current task is overtime."
  (unless cashpw/notify-overtime--have-notified
    (alert
     org-clock-heading
     :title "Over time!"
     :persistent t
     :icon cashpw/icons-hourglass-empty)
    (setq cashpw/notify-overtime--have-notified t)))

(defun cashpw/notify-overtime--reset ()
  "Reset overtime notifier so we can notify again."
  (setq cashpw/notify-overtime--have-notified nil))

;; (use-package! org-link-beautify
;;   :after org
;;   ;; :custom
;;   ;; (org-link-beautify-async-preview t)
;;   :config
;;   (set-face-attribute 'org-link-beautify-link-icon-face nil :weight 'normal)
;;   (org-link-beautify-mode 1))

(use-package! org-extras
  :after org)

(use-package!
    org-roam-contacts
  :after org-roam

  :config
  (defun org-roam-contacts--get-all ()
    "Return list of contacts."
    (-map
     #'make-org-roam-contact-from-file
     (cashpw/notes-files-with-tag org-roam-contacts-tag)))

  (defun cashpw/email-set-header (tag value)
    "Set email header TAG to VALUE."
    (save-excursion
      (message-position-on-field tag)
      (search-backward ":" nil 'noerror)
      (kill-line)
      (insert ": " value)))

  (defun cashpw/email-append-header (tag value)
    "Set email header TAG to VALUE."
    (save-excursion
      (message-position-on-field tag)
      (insert (format " %s" value))))

  (defun cashpw/email-set-to (email)
    "Set email \"To\" header to EMAIl."
    (interactive (list (cashpw/email-select-contact-email)))
    (cashpw/email-set-header "To" email))

  (defun cashpw/email-append-to (email)
    "Append EMAIL onto email \"To\" header."
    (interactive (list (cashpw/email-select-contact-email)))
    (cashpw/email-append-header "To" email))

  (defun cashpw/email-set-cc (email)
    "Set email \"To\" header to EMAIL."
    (interactive (list (cashpw/email-select-contact-email)))
    (cashpw/email-set-header "CC" email))

  (defun cashpw/email-append-cc (email)
    "Append EMAIL onto email \"To\" header."
    (interactive (list (cashpw/email-select-contact-email)))
    (cashpw/email-append-header "CC" email))

  (defun cashpw/email-select-contact-email ()
    "Return user-selected email from org-roam contact list."
    (interactive)
    (let ((name-to-email-alist
           (-reduce-from
            (lambda (acc contact)
              (let ((names (org-roam-contact-names contact))
                    (emails (org-roam-contact-emails contact))
                    (permutations '()))
                (dotimes (name-index (length names))
                  (dotimes (email-index (length emails))
                    (let ((name (nth name-index names))
                          (email (cdr (nth email-index emails)))
                          (email-name (car (nth email-index emails))))
                      (push (cons (format "%s (%s) %s" name email-name (cashpw/dim email)) email)
                            permutations))))
                (append acc permutations)))
            '()
            (--filter
             (org-roam-contact-emails it) (org-roam-contacts--get-all)))))
      (alist-get
       (completing-read "Who?: " name-to-email-alist) name-to-email-alist
       nil nil 'string=))))

(use-package! clocktable-by-category
  :after org)

(use-package! clocktable-by-tag
  :after org)

(use-package! doct
  :commands (doct))

(use-package! doct-org-roam
  :after doct)

(use-package! orgtbl-aggregate)

(after! (:and oc
              memoize))
  ;; Speed up exports
  ;; (memoize 'citeproc-hash-itemgetter-from-any))

(use-package! org-download
  :after org
  :custom
  (org-download-method 'attach)
  (org-download-heading-lvl nil))

;; (remove-hook! 'org-mode-hook #'org-fancy-priorities-mode)

(defvar cashpw/org-fc--card-timer nil
  "The timer for the current card.")

(defun cashpw/org-fc--handle-card-timer-expired ()
  (set-background-color "black"))

(defun cashpw/org-fc--reset-card-timer-expired-effects ()
  (if cashpw/org-fc--card-timer
      (cancel-timer cashpw/org-fc--card-timer))
  (set-background-color "#1d1f21"))

(defun cashpw/org-fc-review-pause ()
  (widen)
  (global-hide-mode-line-mode -1)
  ;; (global-flycheck-mode 1)
  (ignore-errors
    (doom/reset-font-size)))

(defun cashpw/org-fc--before-review ()
  (setq org-format-latex-options
        '(:foreground
          default
          :background default
          :scale 5.0
          :html-foreground "Black"
          :html-background "Transparent"
          :html-scale 1.0
          :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (ignore-errors
    (doom/reset-font-size))
  (setq org-image-actual-width 1200)
  ;; (cashpw/eglot-pause)
  ;; (global-flycheck-mode -1)
  (global-hide-mode-line-mode)
  (doom/increase-font-size 2))

(defun cashpw/org-fc--before-setup ()
  (cashpw/org-fc--reset-card-timer-expired-effects)
  (setq cashpw/org-fc--card-timer
        (run-with-timer
         cashpw/org-fc--seconds-per-card
         nil
         #'cashpw/org-fc--handle-card-timer-expired)))

(defun cashpw/org-fc--after-review ()
  (cashpw/org-fc--reset-card-timer-expired-effects)
  (setq
   org-format-latex-options
   '(:foreground
     default
     :background default
     :scale 1.5
     :html-foreground "Black"
     :html-background "Transparent"
     :html-scale 1.0
     :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
   org-image-actual-width nil)
  ;; (global-flycheck-mode)
  ;; (cashpw/eglot-unpause)
  (global-hide-mode-line-mode -1)
  (ignore-errors
    (doom/reset-font-size)))

(defun cashpw/org-fc--maybe-increment-new-seen-today ()
  "Increment `cashpw/org-fc-review-new-limit--new-seen-today' if flipped card is new."
  (let ((current-position (oref org-fc-review--session current-item)))
    (when (org-fc-position--new-p current-position)
      (cl-incf cashpw/org-fc-review-new-limit--new-seen-today))))

(defun cashpw/org-fc--show-latex-for-tree ()
  "Show latex under current tree."
  (org-map-entries
   (lambda () (org-latex-preview 4))
   ;; match
   nil
   ;; scope
   'tree))

(defun cashpw/org-fc--open-front-link ()
  "Open first link/attachment for \"front\" position if that's the one we're reviewing."
  (unless (null org-fc-review--session)
    (let* ((pos (oref org-fc-review--session current-item))
           (card (oref pos card)))
      (when (and (string= "double" (oref card type))
                 (and (stringp (oref pos pos))
                      (string= "front" (oref pos pos))))
        (let ((bound (org-element-end (org-element-context))))
          (dolist (target '("[[file:" "[[attachment:"))
            (save-excursion
              (when (search-forward target bound 'noerror)
                (let ((extension
                       (s-downcase
                        (url-file-extension
                         (org-element-property :path (org-element-context))))))
                  (cond
                   ((member extension '(".jpg" ".jpeg" ".gif" ".png"))
                    ;; This happends automatically.
                    ;; (org-link-preview)
                    )
                   ((member extension '(".mp4"))
                    (org-open-at-point))))))))))))

(defun cashpw/org-fc--after-flip ()
  (evil-open-fold-rec)
  (cancel-timer cashpw/org-fc--card-timer))

(defun cashpw/org-fc-review-all ()
  "Review everything except reading flashcards."
  (interactive)
  (org-fc-cache-mode)
  (org-fc-review '(:paths all :filter (not (tag "reading")))))

(defun cashpw/org-fc-review-skip-card ()
  "Skip card and proceed to next. Based on `org-fc-review-suspend-card'."
  (interactive)
  (org-fc-review-reset)
  (org-fc-review-session--next org-fc-review--session))

(use-package!
 org-fc
 :after org
 :custom
 (org-fc-directories `(,cashpw/path--notes-dir))
 (org-fc-review-history-file
  (s-lex-format "${cashpw/path--notes-dir}/org-fc-reviews.tsv"))
 (org-fc-bury-siblings t)
 (org-fc-bury-siblings t)
 (org-fc-algo-sm2-intervals '(0.0 1.0 2.0 6.0))
 (org-fc-review-new-limit 20)
 (org-fc-review-new-limit-schedule 'day)
 (org-fc-review-hide-title-in-header-line t)
 ;; Define twice so the keys show up in the hint
 ;; See https://www.leonrische.me/fc/use_with_evil-mode.html
 (org-fc-review-flip-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'org-fc-review-flip)
    (define-key map (kbd "q") 'org-fc-review-quit)
    (define-key map (kbd "e") 'org-fc-review-edit)
    (define-key map (kbd "p") 'cashpw/org-fc-review-pause)
    (define-key map (kbd "s") 'cashpw/org-fc-review-skip-card)
    (define-key map (kbd "S") 'org-fc-review-suspend-card)
    map))
 (org-fc-review-rate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "0") 'org-fc-review-rate-again)
    (define-key map (kbd "1") 'org-fc-review-rate-hard)
    (define-key map (kbd "2") 'org-fc-review-rate-good)
    (define-key map (kbd "3") 'org-fc-review-rate-easy)
    (define-key map (kbd "s") 'cashpw/org-fc-review-skip-card)
    (define-key map (kbd "S") 'org-fc-review-suspend-card)
    (define-key map (kbd "e") 'org-fc-review-edit)
    (define-key map (kbd "q") 'org-fc-review-quit)
    map))

 :config
 (require 'org-fc-hydra)
 (require 'org-fc-keymap-hint)

 (setq cashpw/org-fc--seconds-per-card 10)

 (add-to-list
  'org-fc-custom-contexts '(reading-list . (:filter (tag "reading"))))
 (add-to-list
  'org-fc-custom-contexts '(not-reading-list . (:filter (not (tag "reading")))))

 ;; Define twice so the keys show up in the hint
 ;; See https://www.leonrische.me/fc/use_with_evil-mode.html
 (evil-define-minor-mode-key
  '(normal insert emacs)
  'org-fc-review-flip-mode
  (kbd "n")
  'org-fc-review-flip
  (kbd "s")
  'cashpw/org-fc-review-skip-card
  (kbd "S")
  'org-fc-review-suspend-card
  (kbd "e")
  'org-fc-review-edit
  (kbd "p")
  'cashpw/org-fc-review-pause
  (kbd "q")
  'org-fc-review-quit)
 (evil-define-minor-mode-key
  '(normal insert emacs)
  'org-fc-review-rate-mode
  (kbd "0")
  'org-fc-review-rate-again
  (kbd "1")
  'org-fc-review-rate-hard
  (kbd "2")
  'org-fc-review-rate-good
  (kbd "3")
  'org-fc-review-rate-easy
  (kbd "s")
  'cashpw/org-fc-review-skip-card
  (kbd "S")
  'org-fc-review-suspend-card
  (kbd "e")
  'org-fc-review-edit
  (kbd "q")
  'org-fc-review-quit)
 (add-hook!
  'org-fc-review-edit-mode-hook
  #'cashpw/org-fc--reset-card-timer-expired-effects)

 (add-hook! 'org-fc-before-setup-hook '(cashpw/org-fc--before-setup))
 (add-hook! 'org-fc-after-setup-hook '(cashpw/org-fc--open-front-link))

 (add-hook!
  'org-fc-after-flip-hook
  '(cashpw/org-fc--after-flip
    cashpw/org-fc--maybe-increment-new-seen-today
    cashpw/org-fc--show-latex-for-tree))

 (add-hook! 'org-fc-before-review-hook #'cashpw/org-fc--before-review)

 (add-hook! 'org-fc-after-review-hook #'cashpw/org-fc--after-review)
 ;; (setq
 ;;  org-fc-review-position-filters '())
 ;; (setq
 ;;  org-fc-review-position-filters '(cashpw/org-fc--filter-one-per-file
 ;;                                   cashpw/org-fc--filter-limit-implement
 ;;                                   cashpw/org-fc--filter-limit-new))

 (setq org-roam-db-node-include-function
       (lambda ()
         ;; Exclude org-fc cards from roam
         (not (org-fc-entry-p)))))

(use-package! org-fc-type-vocab :after org-fc)

(defun cashpw/org-fc-awk-index-files (files)
  "Generate a list of all cards and positions in FILES.
Unlike `org-fc-awk-index-paths', files are included directly in
the AWK command and directories are not supported."
  (mapcar
   (lambda (file)
     (plist-put
      file
      :cards
      (mapcar
       (lambda (card)
         (plist-put
          card
          :blocked-by (split-string (or (plist-get card :blocked-by) "") ","))
         (plist-put
          card
          :tags
          (org-fc-awk-combine-tags
           (plist-get card :inherited-tags) (plist-get card :local-tags))))
       (plist-get file :cards))))
   (read
    (shell-command-to-string
     (org-fc-awk--command
      "awk/index.awk"
      :variables (org-fc-awk--indexer-variables)
      ;; Avoid "Argument list too long" error
      ;; Also appears as 'sequencep, /data/data/com.termux/files/usr/bin/emacs:'
      :input
      (concat
       (cashpw/maybe-add-trailing-forward-slash cashpw/path--notes-dir)
       "*.org"))))))

(after!
  org-fc
  (advice-add
   'org-fc-awk-index-files
   :override 'cashpw/org-fc-awk-index-files))

(defcustom cashpw/org-fc--one-per-file-exceptions
  '()
  "List of filetitles to exclude from the one-position-per-file filter.")

(cl-defmethod cashpw/org-fc--filter-one-per-file ((positions list))
  "Return nil to remove the POSITIONS (`org-fc-position's) from the review list."
  (let ((excluded-positions (--filter (-contains-p cashpw/org-fc--one-per-file-exceptions
                                                   (oref (oref it card) filetitle))
                                      positions))
        (one-per-file-positions (--filter (not (-contains-p cashpw/org-fc--one-per-file-exceptions
                                                            (oref (oref it card) filetitle)))
                                          positions))
        (-compare-fn (lambda (position-a position-b)
                       (string-equal (oref (oref position-a card) filetitle)
                                     (oref (oref position-b card) filetitle)))))
    (append excluded-positions
            (-uniq one-per-file-positions))))

(defcustom cashpw/org-fc-review-new-limit -1
  "Limits the number of new positions shown per `org-fc-review-new-limit-schedule'.
-1 for unlimited."
  :type 'integer
  :group 'org-fc)

(defcustom cashpw/org-fc-review-new-limit-schedule 'session
  "The schedule at which to limit the inclusion of new positions.
- `session': Each review session will include, at most, `org-fc-review-new-limit' new cards.
- `day': New cards will be limited to `org-fc-review-new-limit' across review sessions; resets at midnight."
  :type '(choice (const session)
          (const day))
  :group 'org-fc)

(defvar cashpw/org-fc-review-new-limit--new-seen-today 0
  "Remaining new cards for today's reviews.
Don't access directly! Use `org-fc-review-new-limit--get-remaining'.
Not persisted; resets when reloading Emacs!")

(defvar cashpw/org-fc-review-new-limit--reset-day nil
  "The day number on which we should reset `org-fc-review-new-limit--new-seen-today'.
Not persisted; resets when reloading Emacs!")

(cl-defmethod org-fc-position--new-p ((position org-fc-position))
  "Return t if the provided POS ition is new; nil otherwise."
  (eq -1 (oref position box)))

(defun cashpw/org-fc-review-new-limit--get-remaining ()
  "Return the remaining new cards for the `org-fc-review-new-card-schedule'."
  (cond
   ((eq 'session cashpw/org-fc-review-new-limit-schedule)
    cashpw/org-fc-review-new-limit)
   ((eq 'day cashpw/org-fc-review-new-limit-schedule)
    (let ((current-day (time-to-days (current-time))))
      (cashpw/org-fc-review-new-limit--update-reset-day)
      (- cashpw/org-fc-review-new-limit
         cashpw/org-fc-review-new-limit--new-seen-today)))))

(defun cashpw/org-fc-review-new-limit--update-reset-day ()
  (when (or (not cashpw/org-fc-review-new-limit--reset-day)
            (= cashpw/org-fc-review-new-limit--reset-day current-day))
    (setq cashpw/org-fc-review-new-limit--reset-day (1+ current-day)
          cashpw/org-fc-review-new-limit--new-seen-today 0)))

(cl-defmethod cashpw/org-fc--filter-limit-new ((positions list))
  "Return nil to remove the POSITIONS (`org-fc-position's) from the review list."
  (let ((remaining-new (cashpw/org-fc-review-new-limit--get-remaining)))
    (--filter
     (if (not (org-fc-position--new-p it))
         t
       (cond
        ((< remaining-new 0)
         ;; Negative `remaining-new' indicates unlimited new cards. Allow them all.
         t)
        ((= remaining-new 0)
         nil)
        (t
         (cl-decf remaining-new)
         t)))
     positions)))

(after! org-fc
  (setq
   cashpw/org-fc-review-new-limit 10
   cashpw/org-fc-review-new-limit-schedule 'day))

(defmacro cashpw/org-fc--make-tag-limit-fn (tag max-count)
  "Return a function suitable for `org-fc-review-position-filters' which limits tagged cards.

- TAG: The tag to limit
- MAX-COUNT: The maximum count of cards tagged with TAG to include in a review."
  (let ((function-name (format "cashpw/org-fc--filter-limit-tag-%s" tag)))
    `(defun ,(intern function-name) (positions)
       ,(format "Remove all but %d positions which are tagged %s."
                max-count
                tag)
       (let ((position-limit ,max-count)
             (included-count 0)
             (removed-count 0))
         (prog1 (--filter
                 (let ((tags (oref (oref it card) tags)))
                   (if (member ,tag tags)
                       (if (= included-count position-limit)
                           (progn
                             (cl-incf removed-count)
                             nil)
                         (cl-incf included-count)
                         t)
                     t))
                 positions)
           (message
            "[%s] %d positions were tagged with %s. We removed %d from the set, leaving %d for review."
            ,function-name
            (+ removed-count included-count)
            ,tag
            removed-count
            included-count))))))

(cashpw/org-fc--make-tag-limit-fn "implement" 1)
(cashpw/org-fc--make-tag-limit-fn "asl" 5)
(cashpw/org-fc--make-tag-limit-fn "photo" 3)

(after!
  org-fc
  (setq
   org-fc-review-position-filters
   '(cashpw/org-fc--filter-one-per-file
     cashpw/org-fc--filter-limit-new
     cashpw/org-fc--filter-limit-tag-implement
     cashpw/org-fc--filter-limit-tag-asl
     cashpw/org-fc--filter-limit-tag-photo
     org-fc-positions--filter-blocked)))

(after! org-fc
  (setq
   cashpw/org-fc--sm2-initial-review-spacing-interval 7.0)

  ;; Override
  (defun org-fc-review-data-update (positions)
    "Update review data to POSITIONS.
If a doesn't exist already, it is initialized with default
values.  Entries in the table not contained in POSITIONS are
removed."
    (let* ((old-data (org-fc-review-data-get))
           (index -1)
           (data (mapcar
                  (lambda (pos)
                    (cl-incf index)
                    (or
                     (assoc pos old-data #'string=)
                     (org-fc-review-data-default pos index)))
                  positions)))
      (org-fc-review-data-set data)))

  (defun org-fc-review-data-default (position index)
    "Default review data for position POSITION."
    (cl-case org-fc-algorithm
      (sm2-v1 (org-fc-algo-sm2-initial-review-data position index))
      (sm2-v2 (org-fc-algo-sm2-initial-review-data position index))))

  (defun org-fc-algo-sm2-initial-review-data (position index)
    "Initial SM2 review data for POSITION."
    (let* ((box -1)
           (ease (org-fc-algo-sm2-ease-initial))
           (interval 0)
           (due (org-fc-timestamp-in (* index
                                        cashpw/org-fc--sm2-initial-review-spacing-interval))))
      (list position ease box interval due))))

(after! org-fc
  (cl-defmethod org-fc-review-session--next ((review-session org-fc-review-session) &optional resuming)
    "Review the next card of the current session.

If RESUMING is non-nil, some parts of the buffer setup are skipped."
    (if (not (null (oref review-session positions)))
        (condition-case err
            (let* ((pos (pop (oref review-session positions)))
                   (card (oref pos card))
                   (path (oref card path))
                   (id (oref card id))
                   (type (oref card type)))
              (setf (oref review-session current-item) pos)
              (let ((buffer (find-buffer-visiting path)))
                (with-current-buffer (find-file path)
                  (unless resuming
                    ;; If buffer was already open, don't kill it after rating the card
                    (if buffer
                        (setq-local org-fc-reviewing-existing-buffer t)
                      (setq-local org-fc-reviewing-existing-buffer nil))
                    (org-fc-set-header-line))

                  (goto-char (point-min))
                  (org-fc-id-goto id path)

                  (org-fc-indent)
                  ;; Make sure the headline the card is in is expanded
                  (org-reveal)
                  (redisplay t)
                  (org-fc-narrow)
                  (org-fc-hide-keyword-times)
                  (org-fc-hide-drawers)
                  (org-fc-show-latex)
                  (org-display-inline-images)
                  (run-hooks 'org-fc-before-setup-hook)

                  (setq org-fc-review--timestamp (time-to-seconds (current-time)))
                  (let ((step (funcall (org-fc-type-setup-fn type) (oref pos pos))))
                    (run-hooks 'org-fc-after-setup-hook)

                    ;; If the card has a no-noop flip function,
                    ;; skip to rate-mode
                    (let ((flip-fn (org-fc-type-flip-fn type)))
                      (if (or
                           (eq step 'rate)
                           (null flip-fn)
                           (eq flip-fn #'org-fc-noop))
                          (org-fc-review-rate-mode 1)
                        (org-fc-review-flip-mode 1)))))))
          (error
           (org-fc-review-quit)
           (signal (car err) (cdr err))))
      (message "Review Done")
      (org-fc-review-quit))))

(defun org-fc-narrow ()
  "Narrow the outline tree.
Only parent headings of the current heading remain visible."
  (interactive)
  (let* ((tags (org-get-tags nil 'local)))
    ;; Find the first heading with a :narrow: tag or the top level
    ;; ancestor of the current heading and narrow to its region
    (save-excursion
      (while (org-up-heading-safe))
      (org-narrow-to-subtree)
      (outline-hide-subtree))
    ;; Show only the ancestors of the current card
    (org-show-set-visibility org-fc-narrow-visibility)
    (if (member "noheading" tags) (org-fc-hide-heading))))

(use-package!
    org-gallery
  :after (:all org org-download)
  :config
  (defun cashpw/org-download-image--no-insert (image-url)
    (cl-letf (((symbol-function 'org-download-insert-link) #'ignore))
      (org-download-image image-url))))

(use-package! org-habit-stats
  :custom
  (org-agenda-block-separator 9472))

(use-package! repeat-todo
  :after org
  :config
  ;; Enable the mode without calling `repeat-todo-mode-enable' because I have extra todo done state management customizations.
  (setq repeat-todo-mode t))

(use-package! org-gcal-extras)

(defun cashpw/org-gcal--timestamp-from-event (event)
  (let* ((start-time (plist-get (plist-get event :start)
                                :dateTime))
         (end-time (plist-get (plist-get event :end)
                              :dateTime))
         (start-day  (plist-get (plist-get event :start)
                                :date))
         (end-day  (plist-get (plist-get event :end)
                              :date))
         (start (if start-time
                    (org-gcal--convert-time-to-local-timezone
                     start-time
                     org-gcal-local-timezone)
                  start-day))
         (end   (if end-time
                    (org-gcal--convert-time-to-local-timezone
                     end-time
                     org-gcal-local-timezone)
                  end-day))
         (old-time-desc (org-gcal--get-time-and-desc))
         (old-start (plist-get old-time-desc :start))
         (old-end (plist-get old-time-desc :start))
         (recurrence (plist-get event :recurrence)))
    ;; Keep existing timestamps for parent recurring events.
    (when (and recurrence old-start old-end)
      (setq start old-start
            end old-end))
    (cashpw/org-gcal--timestamp start end)))

(defun cashpw/org-gcal--timestamp (start end)
  (if (or (string= start end)
          (org-gcal--alldayp start end))
      (org-gcal--format-iso2org start)
    (if (and (= (plist-get (org-gcal--parse-date start) :year)
                (plist-get (org-gcal--parse-date end)   :year))
             (= (plist-get (org-gcal--parse-date start) :mon)
                (plist-get (org-gcal--parse-date end)   :mon))
             (= (plist-get (org-gcal--parse-date start) :day)
                (plist-get (org-gcal--parse-date end)   :day)))
        (format "<%s-%s>"
                (org-gcal--format-date start "%Y-%m-%d %a %H:%M")
                (org-gcal--format-date end "%H:%M"))
      (format "%s--%s"
              (org-gcal--format-iso2org start)
              (org-gcal--format-iso2org
               (if (< 11 (length end))
                   end
                 (org-gcal--iso-previous-day end)))))))

(defun cashpw/org-gcal--start (event)
  "Return the scheduled start time for EVENT.

Reference: `org-gcal--update-entry'."
  (let* ((start (plist-get event :start))
         (start-datetime (plist-get start :dateTime))
         (start-date (plist-get start :date)))
    (if start-datetime
        (parse-iso8601-time-string start-datetime)
      (date-to-time start-date))))

(defun cashpw/org-gcal--end (event)
  "Return the scheduled end time for EVENT.

Reference: `org-gcal--update-entry'."
  (let* ((end (plist-get event :end))
         (end-datetime (plist-get end :dateTime))
         (end-date (plist-get end :date)))
    (if end-datetime
        (parse-iso8601-time-string end-datetime)
      (date-to-time end-date))))

(defun cashpw/org-gcal--convert-description (_calendar-id _event _update-mode)
  (save-excursion
    (org-up-heading-safe)
    (let ((description (cashpw/org-get-drawer-contents "org-gcal")))
      (if (string-empty-p (string-clean-whitespace description))
          (cashpw/org-delete-drawer "org-gcal")
        (let* ((from-language (cond
                               ((or
                                 (s-contains-p "## TODO" description)
                                 (s-contains-p "- [ ] " description))
                                "markdown")
                               (t
                                "html")))
               (converted-description
                (cashpw/pandoc-convert
                 (format "%s" (cashpw/org-get-drawer-contents "org-gcal"))
                 (format
                  "%s-auto_identifiers"
                  from-language)
                 "org")))
          (save-excursion
            (org-up-heading-safe)
            (save-restriction
              (org-narrow-to-subtree)
              (cashpw/org-delete-drawer "org-gcal")
              (goto-char (point-max))
              (insert converted-description))))))))

(defcustom cashpw/org-gcal--summaries-to-exclude
  '()
  "List of event summaries, as regexps, (titles) which should be excluded during sync/fetch."
  :type '(repeat string)
  :group 'org-gcal)

(defun cashpw/org-gcal--filter-summaries (item)
  "Return nil to exclude the result."
  (let ((summary (plist-get item :summary)))
    (if (--any (string-match it summary)
               cashpw/org-gcal--summaries-to-exclude)
        nil
      t)))

(defun cashpw/org-gcal--get-attendee-by-email (event email)
  "Return attendee object from EVENT matched by EMAIL

Return nil if no attendee exists with that EMAIL."
  (when-let*
      ((attendee
        (--first
         (string=
          it
          attendee-email)
         (append
          nil
          (plist-get event :attendees))))
       attendee)))

(defun cashpw/org-gcal--attendees-include (attendees regexp)
  "Return non-nil if at least one email in ATTENDEES matches REGEXP."
  (let ((attendees (append attendees nil)))
    (--any (let ((email (plist-get it :email)))
             (string-match regexp email))
           attendees)))

(cl-assert (cashpw/org-gcal--attendees-include '[(:email "foo1@bar.com")
                                                 (:email "foo2@bar.com")]
                                               "foo1@bar.com")
           "Should support vector of plists.")
(cl-assert (cashpw/org-gcal--attendees-include '((:email "foo1@bar.com")
                                                 (:email "foo2@bar.com"))
                                               "foo1@bar.com"))
(cl-assert (cashpw/org-gcal--attendees-include '((:email "foo1@bar.com")
                                                 (:email "foo2@bar.com"))
                                               "foo2@bar.com"))
(cl-assert (cashpw/org-gcal--attendees-include '((:email "foo1@bar.com")
                                                 (:email "foo2@bar.com")
                                                 (:email "foo1@baz.com"))
                                               "baz.com"))
(cl-assert (not (cashpw/org-gcal--attendees-include '((:email "foo1@bar.com")
                                                      (:email "foo2@bar.com"))
                                                    "foo3@bar.com")))

(defun cashpw/org-gcal--is-1-on-1 (event)
  "Return non-nil if EVENT is a 1-on-1."
  (let ((attendees (plist-get event :attendees))
        (my-email "cashweaver@google.com")
        (meeting-room-email-domain "resource.calendar.google.com"))
    (cond
     ((and (= (length attendees)
              2)
           (cashpw/org-gcal--attendees-include attendees
                                               my-email))
      t)
     ((and (= (length attendees)
              3)
           (cashpw/org-gcal--attendees-include attendees
                                               my-email)
           (cashpw/org-gcal--attendees-include attendees
                                               meeting-room-email-domain))
      t)
     ;; Other counts of attendees are not 1-on-1s
     (t
      nil))))

(cl-assert (not (cashpw/org-gcal--is-1-on-1 '(:attendees ((:email "foo@bar.com")))))
           t
           "A 1-on-1 requires at least two attendees.")
(cl-assert (not (cashpw/org-gcal--is-1-on-1 '(:attendees ((:email "foo1@bar.com")
                                                          (:email "foo2@bar.com")))))
           t
           "A 1-on-1 must include my email.")
(cl-assert (cashpw/org-gcal--is-1-on-1 '(:attendees ((:email "foo1@bar.com")
                                                     (:email "cashweaver@google.com"))))
           t
           "Should be a valid 1-on-1.")
(cl-assert (cashpw/org-gcal--is-1-on-1 '(:attendees ((:email "foo1@bar.com")
                                                     (:email "cashweaver@google.com")
                                                     (:email "baz@resource.calendar.google.com"))))
           t
           "Should be a valid 1-on-1 with a meeting room.")
(cl-assert (not (cashpw/org-gcal--is-1-on-1 '(:attendees ((:email "foo1@bar.com")
                                                          (:email "foo2@bar.com")
                                                          (:email "foo3@bar.com")
                                                          (:email "foo4@bar.com")))))
           t
           "A 1-on-1 requires at most three attendees.")

(defun cashpw/org-get-drawer-contents (drawer-name)
  (if (not (org-at-heading-p))
      (error "Cannot get drawer contents because point is not at an org heading")
    (save-excursion
      (save-restriction
        (org-narrow-to-element)
        (re-search-forward
         (format "^[ \t]*:%s:[ \t]*$" drawer-name)
         (point-max)
         'noerror)
        (let ((element (org-element-at-point)))
          (when (string= (org-element-property :drawer-name (org-element-at-point)) drawer-name)
            (buffer-substring-no-properties (org-element-property :contents-begin element)
                                            (org-element-property :contents-end element))))))))

(defun cashpw/delete-lines-on-and-between (start-point end-point)
  "Delete lines between START-POINT and END-POINT."
  (delete-region
   (progn
     (goto-char start-point)
     (beginning-of-line)
     (point))
   (progn
     (goto-char end-point)
     (next-line)
     (beginning-of-line)
     (point))))

(defun cashpw/org-delete-drawer (drawer-name)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (cashpw/delete-lines-on-and-between
       (re-search-forward
        (format "^[ \t]*:%s:[ \t]*$" drawer-name)
        (point-max))
       (re-search-forward
        (format ":END:")
        (point-max))))))

(defun cashpw/org-gcal--get-schedule-string (pom)
  "Return schedule string for heading (calendar event) at POM."
  (save-excursion
    (goto-char pom)
    (org-end-of-meta-data)
    (when-let* ((element (org-element-at-point))
                (drawer-name (org-element-property :drawer-name element)))
      (when (string= drawer-name
                     "org-gcal")
        (buffer-substring-no-properties (org-element-property :contents-begin element)
                                        (org-element-property :contents-end element))))))

(defun cashpw/org-gcal--get-schedule-time (pom)
  "Return scheduled time for heading at POM."
  (when-let (time-string (cashpw/org-gcal--get-schedule-string pom))
    (org-read-date t t time-string nil)))

(defcustom cashpw/org-gcal--no-prep-reminder-summaries '()
  "List of event summaries (titles), as regexps, for which we shouldn't create 'Prepare: ...' todos."
  :type '(repeat string)
  :group 'org-gcal)

(defcustom cashpw/org-gcal-prepare-tag "prepare"
  "Tag for identifying \"Prepare\" events."
  :type 'string
  :group 'org-gcal)

(defun cashpw/org-gcal--create-prep-meeting (summary time)
  "Insert a preparation evnet."
  (cashpw/org-insert-todo
   (s-lex-format "Prepare: ${summary} :${cashpw/org-gcal-prepare-tag}:")
   :priority 1
   :effort "5m"
   :start-time time
   :include-hh-mm nil))

(defun cashpw/org-gcal--maybe-create-prep-meeting
    (_calendar-id event _update-mode)
  "Insert a prep TODO if there are more than one attendees to the meeting."
  (when (and (not (org-gcal-extras--processed-p))
             (sequencep event) (> (length (plist-get event :attendees)) 1)
             (--none-p
              (string-match-p it (plist-get event :summary))
              cashpw/org-gcal--no-prep-reminder-summaries))
    (let* ((event-start-time (cashpw/org-gcal--start event))
           (prepare-time
            (cashpw/time--zero-out-hh-mm-ss
             (cond
              ((day-of-week-monday-p event-start-time)
               ;; Prepare for Monday events on the preceding Friday
               (org-time-subtract event-start-time (days-to-time 3)))
              (t
               ;; Prepare for all other events on the day before
               (org-time-subtract event-start-time (days-to-time 1)))))))
      (unless (cashpw/time-past-p prepare-time)
        (cashpw/org-gcal--create-prep-meeting
         (plist-get event :summary) prepare-time)))))

(defun cashpw/org-gcal-remove-tagged-entries (tag)
  "Remove all events tagged with TAG in current buffer."
  (org-map-entries
   (lambda ()
     (org-cut-subtree)
     (setq org-map-continue-from
           (save-excursion
             (beginning-of-line)
             (point))))
   (format "+%s" tag)))

(defcustom cashpw/org-gcal--no-extract-todo-reminder-summaries
  '("Walk" "Clean house")
  "List of event summaries (titles), as regexps, for which we shouldn't create 'Extract todos: ...' todos."
  :type '(repeat string)
  :group 'org-gcal)

(defcustom cashpw/org-gcal-extract-todos-tag "extract_todos"
  "Tag for identifying \"Extract TODOs\" events."
  :type 'string
  :group 'org-gcal)


(defun cashpw/org-gcal--create-todo-extract-reminder
    (_calendar-id event _update-mode)
  "Insert a reminder to extract todos folling an EVENT."
  (let* ((event-summary (plist-get event :summary))
         (event-end-time (cashpw/org-gcal--end event)))
    (cashpw/org-insert-todo
     (s-lex-format "Extract TODOs: ${event-summary} :${cashpw/org-gcal-extract-todos-tag}:")
     :priority 2
     :effort "5m"
     :start-time event-end-time
     :include-hh-mm t)))

(defun cashpw/org-gcal--maybe-create-todo-extract-reminder
    (_calendar-id event _update-mode)
  "Insert a 1-on-1 prep heading todo if EVENT is for a 1-on-1 event."
  (when (and (not (org-gcal-extras--processed-p))
             (sequencep event) (>= (length (plist-get event :attendees)) 2)
             (--none-p
              (string-match-p it (plist-get event :summary))
              cashpw/org-gcal--no-extract-todo-reminder-summaries))
    (cashpw/org-gcal--create-todo-extract-reminder
     _calendar-id event _update-mode)))

(defun cashpw/org-gcal--maybe-handle-sleep (_calendar-id event _update-mode)
  "Maybe handle a sleep EVENT."
  (if (member "processed" (org-get-tags))
      (message "Skipping processed event.")
    (when (and (sequencep event)
               (string-match-p "^Sleep$" (plist-get event :summary)))
      (let ((start-time
             (org-gcal--parse-calendar-time-string
              (plist-get (plist-get event :start) :dateTime)))
            (end-time
             (org-gcal--parse-calendar-time-string
              (plist-get (plist-get event :end) :dateTime))))
        (cashpw/org-clock-add-entry start-time end-time)))
    ;; (let ((inhibit-message t)
    ;;       (message-log-max nil)))
    ))

(defcustom cashpw/org-gcal--profile-personal
  (make-org-gcal-profile
   :fetch-file-alist `(("cashbweaver@gmail.com" . ,(cashpw/path-calendar)))
   :client-id "878906466019-a9891dnr9agpleamia0p46smrbsjghvc.apps.googleusercontent.com"
   :client-secret (secret-get "org-gcal--personal")
   :after-update-entry-functions
   '(org-gcal-extras--set-scheduled
     org-gcal-extras--set-category
     cashpw/org-gcal--maybe-create-todo-extract-reminder
     cashpw/org-gcal--maybe-create-prep-meeting
     ;; cashpw/org-gcal--convert-description
     )
   :fetch-event-filters '()
   :summaries-to-skip
   '("^Nap$"
     "^Sleeping$"
     "^Slack$"
     "^Flashcards$"
     "^Drive "
     "^Shower, etc$"
     "^Chores$"
     "^Work$"
     "^End the day"
     "^Fall asleep$")
   :categories
   (-flatten
    (--map
     (-flatten
      (let ((category (car it))
            (summaries (cdr it)))
        (--map `(,it . ,category) summaries)))
     '(("Fitness" .
        ("Shoulders"
         "Back"
         "Chest"
         "Legs"
         "Mobility, Grip, Neck"
         "Cardio"
         "Strength"
         "Stretch"
         "Stretch: Hamstrings"
         "Stretch: Hips"
         "Walk"))
       ("Car" . ("Tidy car"))
       ("Finance" . ("Finances and net worth"))
       ("Pet" .
        ("Empty cat boxes"
         "Myth's inhaler"
         "Feed cats"
         "Clean pet water and food dishes"))
       ("Family" . ("Call parents"))
       ("Friend" . ("Call with Alian, Ethan, and Austin"))
       ("Food" . ("Lunch" "Dinner"))
       ("Home" .
        ("Take out the trash"
         "Get the mail"
         "Clean house"
         "Flip pillowcase"
         "New bedsheets"
         "New pillowcase"))
       ("Hygeine" . ("Shower" "Brush teeth" "Teeth" "Shave" "Acne"))
       ("Pottery" . ("Pottery" "Wheel Projects with Khaled"))
       ("Study" . ("Study" "Flashcards"))
       ("Food" . ("Huel shake"))
       ("Sleep" . ("Sleep"))
       ("Therapy". ("Therapy"))
       ("R&R" . ("R&R")))))
   :on-activate
   (lambda ()
     (setq cashpw/org-gcal--no-prep-reminder-summaries
           '("Walk" "Clean house" "Debrief"))))
  "Personal profile for `org-gcal'."
  :group 'cashpw
  :type 'sexp)

;; "amc7oe0cqlg989fda4akqjl2f8@group.calendar.google.com"
(setq
 cashpw/org-gcal--profile-sleep
 (make-org-gcal-profile
  :fetch-file-alist
  `(("mu8kpccp2uqujvhkhh6iv8pla0@group.calendar.google.com"
     .
     ,cashpw/path--sleep-calendar))
  :client-id "878906466019-a9891dnr9agpleamia0p46smrbsjghvc.apps.googleusercontent.com"
  :client-secret (secret-get "org-gcal--personal")
  :after-update-entry-functions '(cashpw/org-gcal--remove-gcal-timestamp
                                  cashpw/org-gcal--maybe-handle-sleep)
  :categories
  (-flatten
   (--map
    (-flatten
     (let ((category (car it))
           (summaries (cdr it)))
       (--map `(,it . ,category) summaries)))
    '(("Sleep" . ("Sleep")))))
  :on-activate (lambda () (setq cashpw/org-gcal--no-prep-reminder-summaries '())))
 ;; "Personal sleep profile for `org-gcal'."
 ;; :group 'cashpw
 ;; :type 'sexp)
 )

(defun cashpw/org-gcal--remove-gcal-timestamp (_calendar-id _event _update-mode)
  "Wrapper."
  (org-gcal-extras--remove-gcal-timestamp))

(defun cashpw/org-gcal-fetch-sleep (n-days)
  "Fetch the last N-DAYS of sleep calendar."
  (interactive "nDays to fetch: ")
  (let* ((previous-profile org-gcal--current-profile)
         (org-gcal-up-days n-days)
         (org-gcal-down-days 1)
         (calendar-path (cdr (car org-gcal-fetch-file-alist)))
         (org-agenda-files
          ;; Set limited agenda files to improve performance
          `(,calendar-path)))
    (org-gcal-sync-tokens-clear)
    (org-gcal-activate-profile cashpw/org-gcal--profile-sleep)
    (deferred:sync! (org-gcal-fetch))
    ;; (org-gcal-activate-profile previous-profile)
    ))

(defun cashpw/org-gcal-fetch ()
  "Clear calendar buffer and fetch events."
  (interactive)

  ;; Ignore these methods to improve performance. This is safe
  ;; because I don't push any events to GCal
  (advice-add 'org-generid-id-update-id-locations :override #'ignore)
  (advice-add 'org-gcal-sync-buffer :override #'ignore)
  (flyspell-mode 0)

  (org-gcal-sync-tokens-clear)
  (let* ((calendar-path (cdr (car org-gcal-fetch-file-alist)))
         (org-agenda-files
          ;; Set limited agenda files to improve performance
          `(,calendar-path)))
    (deferred:sync! (org-gcal-fetch)))

  (flyspell-mode 1)
  (advice-remove 'org-generid-id-update-id-locations #'ignore)
  (advice-remove 'org-gcal-sync-buffer #'ignore))

(defun cashpw/org-remove-past-scheduled-events ()
  "Remove events scheduled in the past from current buffer."
  (org-map-entries
   (lambda ()
     ;; (message "Testing calendar event: %s" (org-entry-get nil "ITEM"))
     (when (cashpw/time-past-p (org-get-scheduled-time (point)))
       (org-cut-subtree)
       (setq org-map-continue-from
             (save-excursion
               (beginning-of-line)
               (point)))))))

(defun cashpw/org-gcal-clear-and-fetch ()
  "Clear calendar buffer and fetch events."
  (interactive)
  (org-gcal-activate-profile cashpw/org-gcal--profile-personal)
  (let ((calendar-path (cdr (car org-gcal-fetch-file-alist))))
    (with-current-buffer (find-file-noselect calendar-path)
      (when (cashpw/buffer-contains-regexp-p ":LOGBOOK:")
        (error
         "Wait! Calendar contains un-archived LOGBOOK entries. Archive these, then try again."))
      ;; Keep the buffer tidy
      (cashpw/org-remove-past-scheduled-events)

      ;; Delete extra TODOs. We'll re-create these in the fetch if they're still relevant.
      (cashpw/org-gcal-remove-tagged-entries cashpw/org-gcal-extract-todos-tag)
      (cashpw/org-gcal-remove-tagged-entries cashpw/org-gcal-prepare-tag))
    (cashpw/org-gcal-fetch)))

(after! org-gcal-extras
  (org-gcal-activate-profile cashpw/org-gcal--profile-personal)
  (use-package! org-gcal
    :custom
    (plstore-cache-passphrase-for-symmetric-encryption t)
    (org-gcal-up-days 1)
    (org-gcal-down-days 8)
    (org-gcal-remove-cancelled-events t)
    (org-gcal-remove-events-with-cancelled-todo t)
    ;; See https://github.com/kidd/org-gcal.el/issues/172
    (org-gcal-auto-archive nil)
    (org-gcal-recurring-events-mode 'top-level)

    :config
    ;; https://github.com/dengste/org-caldav/issues/117
    (setenv "GPG_AGENT_INFO")
    (org-gcal-reload-client-id-secret)))

(after! org-habit
  (setq
    org-habit-show-done-always-green t))

(use-package! org-multi-clock)

(use-package! org-noter)

(use-package! ol-notmuch
  :after org)

(use-package! run-on-todo-state-change
  :after org)

(use-package! org-protocol
  :config
  (setq
   org-protocol-default-template-key "w"))

(use-package! org-protocol-capture-html
  :after org-protocol)

(use-package! org-ql)

(use-package! org-recipes
  :after org)

(use-package!
    org-daily-reflection
  :custom
  (org-daily-reflection-close-unmodified-newly-opened-buffers t)
  (org-daily-reflection-capture-nascent-files nil)
  (org-daily-reflection-dailies-directory cashpw/path--notes-dir))

(use-package!
    org-mem
  :custom
  (org-mem-watch-dirs `(,cashpw/path--notes-dir))
  (org-mem-do-sync-with-org-id t)
  (org-mem-seek-link-types '("http" "https" "id" "file" "attachment"))
  :config
  (org-mem-updater-mode))

(use-package!
    org-node
  :demand t
  :after org-roam
  :custom
  (org-node-creation-fn #'org-node-new-via-roam-capture)
  (org-node-slug-fn #'org-node-slugify-like-roam-actual)
  (org-node-filter-fn
   (lambda (node)
     (and (not (assoc "ROAM_EXCLUDE" (org-node-get-properties node)))
          ;; Exclude archives
          (not (s-ends-with-p "archive" (org-node-get-file node)))
          ;; Exclude flashcards
          (not (member "fc" (org-node-get-tags-local node))))))

  :config
  (org-node-cache-mode)
  (org-node-roam-accelerator-mode)
  (org-node-complete-at-point-mode)
  (advice-add 'org-roam-node-find :override 'org-node-find)
  (advice-add 'org-roam-node-insert :override 'org-node-insert-link))

(use-package!
    org-defblock
  :after org)

(use-package!
    org-defblock
  :after org
  :hook (org-mode . org-defblock-mode)
  :config
  (defun cashpw/org-defblock--quote-format ()
    "TODO."
    (message "contents: %s, depth: %d, backend: %s, raw-contents: %s" contents depth backend raw-contents)
    (pcase backend
      (`hugo
       (s-join
        "\n"
        (--map
           (format "> %s" it)
         (cl-delete
          "#+begin_export hugo "
          (cl-delete "#+end_export" (s-split "\n" raw-contents t) :test #'string=)
          :test #'string=))))
      (`markdown
       (s-join
        "\n"
        (--map
           (format "> %s" it)
         (cl-delete
          "#+begin_export markdown "
          (cl-delete "#+end_export" (s-split "\n" raw-contents t) :test #'string=)
          :test #'string=))))
      (`md
       (s-join
        "\n"
        (--map
           (format "> %s" it)
         (cl-delete
          "#+begin_export md "
          (cl-delete "#+end_export" (s-split "\n" raw-contents t) :test #'string=)
          :test #'string=))))
      (_ raw-contents)))
  (org-defblock quote2 nil (cashpw/org-defblock--quote-format))
  (org-defblock quote3 nil (cashpw/org-defblock--quote-format))
  (org-defblock quote4 nil (cashpw/org-defblock--quote-format))
  (org-defblock quote5 nil (cashpw/org-defblock--quote-format)))

(use-package! org-tempo)

(use-package! org-tree-slide)

(use-package! org-vcard)

(when (not (cashpw/machine-p 'work-cloudtop))
  (use-package!
      ox-hugo
    :after ox
    :custom
    (org-hugo-base-dir "~/proj/notes.cashpw.com")
    (org-hugo-section "posts")
    :config
    (add-hook 'cashpw/org-mode-done-cut-hook 'org-roam-file-p)))

(use-package! summarize-agenda-time
  :after org
  :custom
  (summarize-agenda-time--ignore-entry-fns
   '((lambda (marker)
       (member
        (org-with-point-at marker
          (org-entry-get
           nil
           "ITEM"))
        '("Huel shake"
          "Fall asleep")))))
  (summarize-agenda-time--max-duration-minutes (+
                                                ;; 05:00-10:00
                                                (* 60 5)
                                                ;; 10:00-17:00: Work
                                                ;; 17:00-20:30
                                                (* 60 3.5)
                                                ;; 20:30-21:00: Fall asleep
                                                ;; 21:00-05:00: Sleep
                                                )))

(use-package! vulpea)

(defun cashpw/org-roam--get-filetags ()
  "Return a list of all tags used in roam."
  (org-roam-db-query [:select
                      :distinct tag
                      :from tags]))

(defun cashpw/org-roam--get-filetags-not-in-node (node-id)
  "Return a list of all tags used in roam, excluding those used in NODE-ID."
  (org-roam-db-query [:select
                      :distinct [tag]
                      :from tags
                      :where tag :not-in [:select tag
                                          :from tags
                                          :where (= node_id $s1)]]
                     node-id))

(defun cashpw/org-roam-make-filepath (title &optional time time-zone)
  "Return a filenaem for an org-roam node.

Reference: https://ag91.github.io/blog/2020/11/12/write-org-roam-notes-via-elisp"
  (let ((slug
         (org-roam-node-slug
          (org-roam-node-create
           :title title))))
    (format
     "%s/%s.org"
     org-roam-directory
     slug)))

(defun cashpw/org-mode-insert-option (option value)
  "Insert an org-mode option (#+OPTION: VALUE).

TODO: move to org-mode section"
  (insert (s-lex-format "#+${option}: ${value}\n")))

(defun cashpw/org-mode-insert-options (options)
  "Insert an alist of org-mode options (#+OPTION: VALUE)."
  (cl-loop for (option . value) in options
           do (cashpw/org-mode-insert-option
               option
               value)))

(defun cashpw/org-mode-insert-properties (properties)
  "Insert an alist of org-mode properties (:PROPERTY: VALUE).

When WRAP is non-nil: Wrap the properties with :PROPERTIES:/:END:."
  (interactive)
  (cl-loop for (property . value) in properties
           do (org-set-property
               property
               value)))

(defun cashpw/org-roam-add-citation-as-ref ()
  "Based on `citar-org-roam-ref-add."
  (interactive)
  (let ((citation (with-temp-buffer
                    (org-cite-insert nil)
                    (buffer-string))))
    (org-roam-ref-add citation)))

(defun cashpw/org-hugo--remove-empty-bibliography-from-export (export-file-path)
  "Delete bibliography in EXPORT-FILE-PATH if there are no citations.

The relevant markdown looks like either:

## Bibliography {#bibliography}

<style>.csl-entry{text-indent: -1.5em; margin-left: 1.5em;}</style><div class=\"csl-bib-body\">
  <div class=\"csl-entry\">...</div>
</div>

or:

## Bibliography {#bibliography}

<style>.csl-entry{text-indent: -1.5em; margin-left: 1.5em;}</style><div class=\"csl-bib-body\">
</div>"

  (with-current-buffer (find-file-noselect export-file-path)
    (when (save-excursion
            (goto-char (point-min))
            (search-forward "csl-entry" nil t))
      (save-excursion
        (goto-char (point-min))
        (search-forward "## Bibliography" nil t)
        ;; # Bibliography
        (delete-line)
        ;;
        (delete-line)
        ;; <style>...
        (delete-line)
        ;; </div>
        (delete-line)
        (save-buffer)))))

(defun cashpw/org-hugo-export-wim-to-md ()
  "Function for `after-save-hook' to run `org-hugo-export-wim-to-md'.

The exporting happens only when Org Capture is not in progress."
  (interactive)
  (when (and (not (eq real-this-command
                      'org-capture-finalize))
             (not (member (buffer-file-name)
                          cashpw/org-roam--file-path-exceptions-to-export-after-save)))
    (save-excursion
      (let* ((org-id-extra-files (org-roam-list-files))
             (export-file-path (org-hugo-export-wim-to-md)))
        (cashpw/org-hugo--remove-empty-bibliography-from-export export-file-path)
        ;; (when cashpw/org-hugo-replace-front-matter-with-title
        ;;   (with-current-buffer (find-file-noselect
        ;;                         export-file-path)
        ;;     (cashpw/replace-toml-front-matter-with-md-heading)
        ;;     (save-buffer)))
        ))))

(defun cashpw/org-get-filetags (&optional file-path)
  "Return filetags in FILE-PATH."
  (let ((filetag-string
         (shell-command-to-string
          (concat
           (format "rgrep '^#+filetags:' %s"
                   (or file-path
                       (buffer-file-name)))
           "| sed 's/.*#+filetags: \\(.*\\)/\\1/'"))))
    (s-split ":" filetag-string 'omit-nulls)))

(defun cashpw/org-files-and-filetags (directory)
  "Return list of org files in DIRECTORY and their filetags."
  (let ((grep-result
         (shell-command-to-string
          (concat
           (format "rgrep --max-count=1 '^#+filetags:' %s/*.org"
                   (directory-file-name directory))
           " | sed 's/\\(.*\\):#+filetags: \\(.*\\)/\\1 \\2/'"))))
    (--map
     (cl-destructuring-bind
         (file filetags)
         (s-split " " it 'omit-nulls)
       (cons file (s-split ":" filetags 'omit-nulls)))
     (s-split "\n" grep-result 'omit-nulls))))

(defun cashpw/org-files-with-tag (tag directory)
  "Return list of org files in DIRECTORY tagged (filetag) with TAG."
  (cashpw/org-files-with-tags (list tag) directory))

(defun cashpw/org-files-with-tags (tags directory)
  "Return list of org files in DIRECTORY tagged (filetag) with TAG."
  (--map
   (car it)
   (-filter
    (lambda (file-and-filetags)
      (-every (lambda (tag) (member tag (cdr file-and-filetags))) tags))
    (cashpw/org-files-and-filetags directory))))

(defun cashpw/notes-files-with-tag (tag)
  "Return a list of note files containing 'hastodo tag."
  (cashpw/org-files-with-tag tag cashpw/path--notes-dir))

(defun cashpw/notes-files-with-tags (&rest tags)
  "Return a list of note files tagged with all TAGS."
  (cashpw/org-files-with-tags tags cashpw/path--notes-dir))

(defun cashpw/org-mode--buffer-has-todo-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks.

Note that we explicitly don't use `org-element-parse-buffer'
because it's slow."
  (when (string-equal
         mode-name
         "Org")
    (cashpw/buffer-contains-regexp-p
     "^\\*+ TODO")))

(defun cashpw/magit-buffer-p ()
  (and
   (derived-mode-p 'magit-mode)
   (not
    (eq major-mode 'magit-process-mode))))

(defun cashpw/org-roam-update-hastodo-tag ()
  "Update HASTODO tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (not (cashpw/magit-buffer-p))
             (vulpea-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (cashpw/org-mode--buffer-has-todo-p)
            (setq tags (cons "hastodo" tags))
          (setq tags (remove "hastodo" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p (expand-file-name (file-name-as-directory org-roam-directory))
                        (file-name-directory buffer-file-name))))

(defun cashpw/org-mode--buffer-has-gallery-p ()
  "Return non-nil if current buffer has a heading tagged as a gallery entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks.

Note that we explicitly don't use `org-element-parse-buffer'
because it's slow."
  (when (string-equal
         mode-name
         "Org")
    (cashpw/buffer-contains-regexp-p
     ":gallery:")))

(defun cashpw/org-roam-update-has-gallery-tag ()
  "Update :has-gallery: tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (not (cashpw/magit-buffer-p))
             (vulpea-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (cashpw/org-mode--buffer-has-gallery-p)
            (setq tags (cons "has_gallery" tags))
          (setq tags (remove "has_gallery" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p (expand-file-name (file-name-as-directory org-roam-directory))
                        (file-name-directory buffer-file-name))))

(cl-defun cashpw/org-insert-todo
    (text &key point-or-marker priority effort category start-time end-time include-hh-mm)
  "Insert TODO at POINT-OR-MARKER.

Optionally set the TODO's TEXT, PRIORITY, EFFORT, and START-TIME/END-TIME (INCLUDE-HH-MM)."
  (save-excursion
    (when point-or-marker
      (goto-char point-or-marker))
    (org-insert-todo-heading-respect-content)
    (insert text)
    (shut-up
      (when priority
        (org-priority priority))
      (when effort
        (org-set-property "Effort" effort))
      (when category
        (org-set-property "CATEGORY" category))
      (when (or start-time end-time)
        (cond
         ((and end-time (null start-time))
          (error "Cannot schedule end-time without start-time."))
         ((and start-time end-time)
          (org-schedule
           nil
           (format "%s-%s"
                   (format-time-string "%F %a %H:%M" start-time)
                   (format-time-string "%H:%M" end-time))))
         ((and start-time include-hh-mm)
          (org-schedule nil (format-time-string "%F %a %H:%M" start-time)))
         ((and start-time)
          (org-schedule nil (format-time-string "%F %a" start-time))))))))

(setq
 cashpw/-schedule-block-day '(:start "07:00" :end "19:00")
 cashpw/-schedule-block-one '(:start "07:00" :end "09:00")
 cashpw/-schedule-block-two '(:start "09:00" :end "11:00")
 cashpw/-schedule-block-three '(:start "14:00" :end "16:00")
 cashpw/-schedule-block-four '(:start "16:00" :end "18:00"))

(defun cashpw/org-schedule-for-block (block-time &optional date)
  (interactive)
  (let ((start-time (plist-get block-time :start))
        (end-time (plist-get block-time :end))
        (date (or date "today")))
    (org-schedule nil (format "%s %s-%s"
                              date
                              start-time
                              end-time))))

(defun cashpw/org-schedule-today-from-to (start-time end-time &optional date)
  (interactive)
  (let ((date (or date "today")))
    (org-schedule nil (format "%s %s-%s"
                              date
                              start-time
                              end-time))))

(defun cashpw/org--archive-buffer-p (buffer)
  "Return non-nil if BUFFER is an org-mode archive."
  (string-match-p
   "org_archive$"
   (or
    (buffer-file-name buffer)
    "")))

(defun cashpw/org-set-last-modified ()
  "Update the LAST_MODIFIED property on the file."
  (interactive)
  (when (and (not (cashpw/org--archive-buffer-p (current-buffer)))
             (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char (point-min))
      (org-set-property
       "LAST_MODIFIED" (format-time-string "[%Y-%m-%d %a %H:%M]")))))

(add-hook
 'org-mode-hook
 (lambda ()
   (add-hook! 'before-save-hook :local #'cashpw/org-set-last-modified)))

(defun cashpw/org--insert-holiday-reminders (year)
  "Insert TODO reminders for holidays."
  (interactive (let* ((year
                       (calendar-read-sexp
                        "Year?"
                        (lambda (x) (> x 0))
                        (calendar-extract-year (calendar-current-date)))))
                 (list year)))
  (let ((all-holidays
         (s-split
          "\n"
          (progn
            (list-holidays
             year year
             (append holiday-general-holidays holiday-christian-holidays))
            (with-current-buffer holiday-buffer
              (buffer-string))))))
    (cl-labels
        ((insert-reminder
           (name schedule-time)
           (progn
             (org-insert-heading)
             (org-todo "TODO")
             (insert name)
             (org-schedule nil (format-time-string "%F %a" schedule-time)))))
      (--map
       (cl-destructuring-bind
           (date-string holiday-name)
           ;; Example Monday, January 1, 2024: New Year's Day
           (s-split ": " it)
         (let ((holiday-time (date-to-time date-string)))
           (insert-reminder holiday-name holiday-time)
           (insert-reminder
            (s-concat holiday-name " in 30 days")
            (time-subtract holiday-time (days-to-time 30)))
           (insert-reminder
            (s-concat holiday-name " in 90 days")
            (time-subtract holiday-time (days-to-time 90)))))
       all-holidays)))
  (kill-buffer holiday-buffer))

(defun cashpw/org-agenda-files--notes-all ()
  "Return list of all notes files.

Don't call directly. Use `cashpw/org-agenda-files'."
  (let* ((org-roam-directory cashpw/path--notes-dir)
         (org-roam-db-location (expand-file-name "org-roam.db"
                                                 org-roam-directory)))
    (org-roam-list-files)))

(defun cashpw/org-agenda-files--journal-this-year ()
  "Return list of journal agenda files.

Don't call directly. Use `cashpw/org-agenda-files'."
  (let* ((org-roam-directory cashpw/path--notes-dir)
         (org-roam-db-location (expand-file-name "org-roam.db"
                                                 org-roam-directory))
         (yyyy (format-time-string "%Y" (current-time))))
    (--filter
     (string-match-p yyyy it)
     (cashpw/notes-files-with-tag "journal"))))

(defun cashpw/org-agenda-files--journal-this-year-with-todo ()
  "Return list of journal agenda files.

Don't call directly. Use `cashpw/org-agenda-files'."
  (let* ((org-roam-directory cashpw/path--notes-dir)
         (org-roam-db-location (expand-file-name "org-roam.db"
                                                 org-roam-directory))
         (yyyy (format-time-string "%Y" (current-time))))
    (--filter
     (string-match-p yyyy it)
     (cashpw/notes-files-with-tags "journal" "hastodo"))))

(defun cashpw/org-agenda-files (context &optional include-archive)
  "Return list of agenda files for CONTEXT. Include archived files when INCLUDE-ARCHIVE."
  (let ((files
         (cond
          ((equal context 'notes-with-todo)
           (cashpw/notes-files-with-tag "hastodo"))
          ((equal context 'notes-all)
           (cashpw/org-agenda-files--notes-all))
          ((equal context 'personal)
           (append
            `(,(cashpw/path-todos)
              ,cashpw/path--personal-asana
              ,(cashpw/path-calendar))))
          ((equal context 'projects-with-todo)
           (cashpw/notes-files-with-tags "hastodo" "project"))
          ((equal context 'pets-with-todo)
           (cashpw/notes-files-with-tags "hastodo" "pet"))
          ((equal context 'calendar)
           '())
          ((equal context 'journal-this-year)
           (cashpw/org-agenda-files--journal-this-year))
          ((equal context 'journal-this-year-with-todo)
           (cashpw/org-agenda-files--journal-this-year-with-todo))
          ((equal context 'people-private-with-todo)
           (cashpw/notes-files-with-tags "person" "private" "hastodo"))
          ((equal context 'people-private)
           (cashpw/notes-files-with-tags "person" "private")))))
    (if include-archive
        (append
         files
         (org-extras-files-archive-files files))
      files)))

(defun cashpw/org-agenda-files--update ()
  "Update `org-agenda-files'."
  (setq
   org-agenda-files
   (seq-uniq
    (append
     (cashpw/org-agenda-files 'personal)
     (cashpw/org-agenda-files 'calendar)
     (cashpw/org-agenda-files 'projects-with-todo)
     (cashpw/org-agenda-files 'pets-with-todo)
     (cashpw/org-agenda-files 'journal-this-year-with-todo)
     (cashpw/org-agenda-files 'people-private-with-todo)))))

(cashpw/org-agenda-files--update)

(defun cashpw/org-element-cache-reset-all ()
  "Helper method to interactively reset all."
  (interactive)
  (org-element-cache-reset 'all))

(defun cashpw/narrow-between-text (start-text end-text)
  "Narrow between START-TEXT and END-TEXT around point."
  (let ((start (save-excursion (search-backward start-text)))
        (end (save-excursion (search-forward end-text))))
    (narrow-to-region start end)))

(defmacro cashpw/org-with-narrow-between-text (start-text end-text &rest body)
  "Execute BODY with current buffer narrowed between START-TEXT and END-TEXT around point."
  `(save-restriction
    (cashpw/narrow-between-text ,start-text ,end-text)
    ,@body))

(defun cashpw/org-join-lines-in-quote ()
  "Join lines in current quote block which are not separated by an empty line."
  (interactive)
  (cashpw/org-with-narrow-between-text
   "#+begin_quote"
   "#+end_quote"
   (save-excursion
     ;; Go to #+begin_quote
     (goto-char (point-min))
     ;; Go to first line of quote
     (evil-next-line)
     ;; Go to second line of quote
     (evil-next-line)
     ;; Join each empty-line-delineated paragraph
     (while (not
             (string-equal-ignore-case
              (org-current-line-string) "#+end_quote"))
       ;; 2. While 'next line isn't empty': (evil-join)
       (while (and (not
                    (string-equal-ignore-case
                     (org-current-line-string) "#+end_quote"))
                   (not (string-empty-p (org-current-line-string))))
         (join-line)
         (evil-next-line))
       (ignore-errors
         (evil-next-line)
         (evil-next-line)))

     ;; Join line-break-hyphenated words (for example: "back- ground")
     (goto-char (point-min))
     (replace-regexp "\\([A-Za-z]\\)[-‐] \\([A-Za-z]\\)" "\\1\\2"))))

(defun cashpw/org-remove-square-brackets-in-quote ()
  "Join lines in current quote block which are not separated by an empty line."
  (interactive)
  (cashpw/org-with-narrow-between-text
   "#+begin_quote"
   "#+end_quote"
   (cashpw/replace-regexp-in-buffer "\\( \\)?\\[[^]]*\\]\\(,\\)?" "")))

(setq
 org-image-max-width 'window
 org-id-locations-file-relative nil
 ;; org-return-follows-link t
 org-default-properties (append org-default-properties org-recipes--properties)
 org-agenda-bulk-custom-functions '((?L org-extras-reschedule-overdue-todo-agenda)
                                    (?> cashpw/org-agenda-reschedule-to-next-occurrence-or-kill)
                                    (?y cashpw/org-agenda-done-yesterday)
                                    (?. cashpw/org-agenda-reschedule-to-today)))

(after! org
  ;; Allow in-word emphasis (e.g. c**a**t with bold 'a')
  ;; Reference: https://stackoverflow.com/a/24540651
  ;; (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  ;; (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  ;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (setq
   org-auto-align-tags nil
   org-pretty-entities t
   org-ellipsis "…"
   org-hide-leading-stars t))

(use-package!
    org-modern
  :after org
  :custom (org-modern-horizontal-rule "──────────")
  (org-modern-star 'fold)
  (org-modern-hide-stars nil)
  (org-modern-list
   '((?+ . "•")
     (?- . "•")
     (?* . "•")))
  (org-modern-todo-faces
   '(("PROJ" .
      (:weight
       semi-bold
       :foreground "dark gray"
       :background "black"
       :inverse-video t))
     ("INPROGRESS" .
      (:weight
       semi-bold
       :foreground "orange"
       :background "black"
       :inverse-video t))))
  (org-modern-priority-faces
   '((?0
      .
      (:weight
       semi-bold
       :foreground "firebrick"
       :background "white"
       :inverse-video t))
     (?1
      .
      (:weight
       semi-bold
       :foreground "gray100"
       :background "black"
       :inverse-video t))
     (?2
      .
      (:weight
       semi-bold
       :foreground "gray80"
       :background "black"
       :inverse-video t))
     (?3
      .
      (:weight
       semi-bold
       :foreground "gray65"
       :background "black"
       :inverse-video t))
     (?4
      .
      (:weight
       semi-bold
       :foreground "gray60"
       :background "black"
       :inverse-video t))))
  :config
  ;; (set-face-attribute 'org-modern-horizontal-rule nil :strike-through "gray70")
  (global-org-modern-mode))

(after! org
  (defun cashpw/org--enable-insert-in-note-buffer ()
    "Enable insert mode when entering a note buffer."
    (evil-insert-state nil))
  (add-hook 'org-log-buffer-setup-hook 'cashpw/org--enable-insert-in-note-buffer))

(after! org-roam
  ;; Override to only replace if it's a roam link.
  (defun cashpw/org-roam-link-replace-all ()
    "Replace all \"roam:\" links in buffer with \"id:\" links."
    (interactive)
    (org-with-point-at 1
      (while (re-search-forward org-link-bracket-re nil t)
        (when (s-starts-with-p
               "roam:"
               (match-string 1))
          (org-roam-link-replace-at-point)))))
  (advice-add 'org-roam-link-replace-all :override 'cashpw/org-roam-link-replace-all)

  ;; Disabling for now in favor of org-node. I don't use this for much outside of the org-roam-buffer.
  (advice-add 'org-roam-db-sync :override 'ignore)

  (setq
   org-roam-db-update-on-save nil
   org-roam-link-auto-replace nil)

  ;; Deprecated in favor of =org-node='s cache.
  ;; Sync when I'm away from keyoard.
  ;; (setq
  ;;  cashpw/org-roam-sync-timer (run-with-idle-timer
  ;;                              60
  ;;                              t
  ;;                              (lambda ()
  ;;                                (message "Roam sync: Running...")
  ;;                                (let* ((org-roam-directory
  ;;                                        (if (cashpw/machine-p 'work-cloudtop))
  ;;                                            cashpw/path--work-notes-dir
  ;;                                          cashpw/path--notes-dir))
  ;;                                       (org-roam-db-location
  ;;                                        (expand-file-name
  ;;                                         "org-roam.db"
  ;;                                         org-roam-directory)))
  ;;                                  (message "Roam sync: Ran for %.06fsec"
  ;;                                           (k-time (org-roam-db-sync)))))))
  )

(after!
  org-roam
  ;; Disable to improve performance
  (org-roam-db-autosync-mode -1)
  (setq
   ;; Disable org-roam completion in favor of (faster) org-node.
   org-roam-completion-everywhere nil

   org-roam-directory cashpw/path--notes-dir
   org-attach-directory (file-truename (format "%s/attachments/" org-roam-directory))
   org-directory org-roam-directory
   org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (add-hook
   'org-mode-hook
   (lambda ()
     (add-hook! 'before-save-hook :local #'cashpw/org-roam-before-save))))

(after!
  doct-org-roam
  (setq
   ;; Note that I've enumerated the "head" entries, rather than defining them in the "group"
   ;; and specifying the tag with a variable, because this didn't produce the right output.
   ;; I didn't have time to dive in an understand why.
   org-roam-capture-templates
   (doct-org-roam
    `((:group
       "org-roam"
       :type plain
       :template "%?"
       :file "${slug}.org"
       :unnarrowed t
       :after-finalize cashpw/org--set-refile-targets
       :before-finalize
       (lambda ()
         (save-excursion
           (let ((id (org-entry-get (point-min) "ID")))
             (goto-char (point-min))
             (org-set-property
              "DIR"
              (format "attachments/%s/%s"
                      (substring id 0 2)
                      (substring id 2))))))
       :children
       (("Concept"
         :keys "c"
         :head
         (
          "#+title: ${title}
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :concept:"))
        ("On X"
         :keys "o"
         :head
         (
          "#+title: ${title}
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :concept:

An [[id:2a6113b3-86e9-4e70-8b81-174c26bfeb01][On X]]."))
        ("American sign language (ASL)"
         :keys "a"
         :prepare-finalize
         (lambda ()
           (org-with-point-at
               (org-find-exact-headline-in-buffer "ASL") (org-fc-type-double-init))
           (org-with-point-at (point-min) (org-attach))
           (let ((attachment-file-name
                  (nth 0 (directory-files (org-attach-dir) nil "mp4"))))
             (save-excursion
               (cashpw/replace-regexp-in-buffer
                "ATTACH_\\(.*\\)_END_ATTACH"
                (format "[[attachment:%s][\\1]]" attachment-file-name)))))
         :head
         (
          "#+title: ${title} (ASL)
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :concept:

ATTACH_${title}_END_ATTACH in [[id:1056a7fd-6e6c-4c83-9910-a3528277ab0b][American sign language]].

* Flashcards :noexport:
** ASL :asl:

ATTACH_ASL_END_ATTACH

*** Back

${title}

*** Source
[cite:@asluniversityASLAmericanSignLanguage]

* Bibliography
#+print_bibliography:"))
        ("Decision"
         :keys "d"
         :file "decision--${slug}.org"
         :head
         ("#+title: Decision: ${title}
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :decision:private:

* TODO Background
* TODO Decison

The current, overall, decision taking all considerations into account.

* PROJ Consideration [%<%Y-%m-%d %a>]

** TODO Do I have enough ([[id:bceac98c-163f-4208-8ef5-75f98640553b][40-70%]]) information?

** TODO Options

1. Option 1
2. Option 2
3. Option 3

** TODO Comparison

|             | Option 1 | Option 2 | Option 3 |
|-------------+----------+----------+----------|
| Dimension 1 | 🟩 Great | 🟨 Good  | 🟥 Bad   |
| Dimension 2 | 🟥 Bad   | 🟨 Good  | 🟥 Good  |

** TODO Conclusion

Option 2

* PROJ Admin
** TODO Review
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Friday\") nil nil nil nil \" ++1m\")

Review current state of the project and update any tracking documentation.
** TODO Communicate
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Friday\") nil nil nil nil \" ++1m\")

Communicate project status, blockers, etc, to relevant stakeholders.
* Flashcards :noexport:
"))
        ("Project"
         :keys "P"
         :file "proj--${slug}.org"
         :head
         (
          "#+title: ${title}
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :project:private:

* Notes
* Log
* Questions
* PROJ ${title}
** TODO Close out :unscheduled:
*** TODO Retrospective :unscheduled:
*** TODO Communicate :unscheduled:
** TODO Administration :unscheduled:
** TODO Review
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Friday\") nil nil nil nil \" ++1w\")

Review current state of the project and update any tracking documentation.
** TODO Communicate
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Friday\") nil nil nil nil \" ++1w\")

Communicate project status, blockers, etc, to relevant stakeholders.
* Flashcards :noexport:
"))
        ("Person"
         :keys "p"
         :head
         (
          "#+title: ${title}
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :person:
* Flashcards :noexport:"))


        ("Photographer"
         :keys "h"
         :head
         (
          "#+title: ${title}
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :person:

A [[id:5ab4e578-5360-4b9b-b8f1-2cf57b7793c7][Photographer]].
* TODO [#2] Add photos :noexport:
* Flashcards :noexport:"))

        ("Friend (person)"
         :keys "f"
         :head
         (
          "#+title: ${title}
#+category: %(replace-regexp-in-string \" \" \"\" \"${title}\")
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :person:

* Photo
* Relationships
* Gifts
* Events
* Reminders
* Notes"))
        ("Verse"
         :keys "v"
         :head
         (
          "#+title: ${title}
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :verse:"))
        ("Quote"
         :keys "u"
         :head
         (
          "#+title: ${title}
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :quote:"))
        ("Recipe"
         :keys "r"
         :head
         (
          "#+title: ${title}
#+author: Cash Prokop-Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :recipe:

* TODO [#2] Ingredients

#+begin_ingredients
| Ingredients | Notes | Quantity |
|-------------+-------+----------|
|             |       |          |
#+end_ingredients

* TODO [#2] Steps"))))))))

(defun cashpw/org-roam-node-create--art-inner
    (title artist-node-link &optional image-url citekey)
  "Create a roam node based on bibliography citation.

See: https://jethrokuan.github.io/org-roam-guide"
  (let ((citation
         (format "%s%s"
                 artist-node-link
                 (if citekey
                     (format ", [cite:@%s]" citekey)
                   "")))
        (node-properties
         (format ":PROPERTIES:%s
:END:"
                 (if citekey
                     (format "\n:ROAM_REFS: [cite:@%s]" citekey)
                   "")))
        (downloaded-image
         (if image-url
             (format "\n%s\n"
                     (string-trim
                      (let ((org-download-image-dir cashpw/path--notes-dir))
                        (with-temp-buffer
                          (org-mode)
                          (org-download-image image-url)
                          (buffer-string)))))
           "")))
    (org-roam-capture-
     :templates
     '(("a" "art" plain "%?"
        :if-new
        (file+head
         "${slug}.org"
         "${node-properties}
#+title: ${title}
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :art:

${citation}
${downloaded-image}
* TODO [#2] Notes :noexport:
* TODO [#2] Thoughts :noexport:")
        :immediate-finish t
        :unnarrowed t))
     :info
     (list
      :citation citation
      :node-properties node-properties
      :downloaded-image downloaded-image)
     :node (org-roam-node-create :title title)
     :props '(:finalize find-file))))

(defun cashpw/org-roam-node-create--art ()
  "Create a roam node.

See: https://jethrokuan.github.io/org-roam-guide"
  (interactive)
  (let* ((artist-node-link
          (with-temp-buffer
            (org-mode)
            (org-node-insert-link nil)
            (buffer-string)))
         (artist-name (cashpw/org-link--get-description artist-node-link))
         (reference
          (when (y-or-n-p "Add citation? ")
            (citar-select-ref)))
         (reference-title
          (when reference
            (citar-format--entry (citar-format--parse "${title}") reference)))
         (image-url
          (when (y-or-n-p "Add image? ")
            (read-string "Image URL: " (gui-get-selection 'CLIPBOARD 'STRING))))
         (title (format "%s | %s" artist-name (read-string "Title: "))))
    (cashpw/org-roam-node-create--art-inner title artist-node-link
                                            image-url
                                            reference)))

(defun cashpw/org-roam-node-from-cite--inner (entry title)
  "Create a roam node based on bibliography citation.

See: https://jethrokuan.github.io/org-roam-guide"
  (interactive (list (citar-select-ref)))
  (org-roam-capture- :templates
                     '(("r" "reference" plain "%?" :if-new
                        (file+head "${slug}.org"
                                   ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :reference:

TODO_AUTHOR, [cite:@${citekey}]

* TODO [#2] Summary
* TODO [#2] Thoughts
* TODO [#2] Notes")
                        :immediate-finish t
                        :unnarrowed t))
                     :info (list
                            :citekey entry)
                     :node (org-roam-node-create
                            :title title)
                     :props '(:finalize find-file)))

(defun cashpw/format-cited-author-for-org-roam (raw-authors)
  (if (not (s-contains? "," raw-authors))
      raw-authors
    (cond
     (;; e.g. "Doe, Jane and Doe, John"
      (s-contains? " and " raw-authors)
      (s-join " and "
              (mapcar
               (lambda (author)
                 (s-join " " (reverse (s-split ", " author))))
               (s-split " and " raw-authors 'omit-nulls))))
     (;; e.g. "Doe, Jane"
      t
      (s-join " " (reverse (s-split ", " raw-authors 'omit-nulls)))))))

(defun cashpw/org-roam-node-from-cite ()
  "Create a roam node based on bibliography citation.

See: https://jethrokuan.github.io/org-roam-guide"
  (interactive)
  (let* ((entry (citar-select-ref))
         (author (cashpw/format-cited-author-for-org-roam
                  (citar-format--entry (citar-format--parse "${author editor journal}")
                                       entry)))
         (citation-title (citar-format--entry (citar-format--parse "${title}")
                                              entry))
         (default-title (cond
                         ((string-empty-p citation-title)
                          "Something went wrong when extracting the title.")
                         ((string-empty-p author)
                          citation-title)
                         (t
                          (s-lex-format
                           "${author} | ${citation-title}"))))
         (title (read-string "Title: "
                             default-title)))
    (cashpw/org-roam-node-from-cite--inner entry title)))

(defvar cashpw/org-roam--file-path-exceptions-to-export-after-save
  '()
  "List of org-roam file paths which should NOT be exported after they are saved. This list is populated within the particular .dir-local.el files.")

(defvar cashpw/org-roam--file-path-exceptions-to-mirror-refs-to-front-matter
  '()
  "List of org-roam file paths which should NOT have references mirrored to front matter. This list is populated within the particular .dir-local.el files.")

(defun cashpw/org-roam-rewrite-smart-to-ascii ()
  (when (org-roam-file-p)
    (cashpw/replace-smart-quotes-in-buffer)))

(defun cashpw/org-roam-open-ref ()
  "Open the ROAM_REF."
  (interactive)
  (let ((roam-refs (org-entry-get
                    (point)
                    "ROAM_REFS")))
    (if (s-starts-with-p "http"
                         roam-refs)
        (browse-url roam-refs)
      (message "Not an http(s) ref (%s)"
               roam-refs))))

(defun cashewaver-org-roam--append-to-custom-front-matter (key value)
  "Append the provided KEY and VALUE to hugo_custom_front_matter."
  (when (org-roam-file-p)
    (let
        ((keyword
          "HUGO_CUSTOM_FRONT_MATTER")
         (current-value
          (org-collect-keywords
           keyword)))
      (org-roam-set-keyword
       (downcase keyword)
       (format "%s %s"
               key
               value)))))

(defun cashpw/org-roam--mirror-roam-aliases-to-hugo-aliases ()
  "Copy the list of ROAM_ALIASES into HUGO_ALIASES.

Work in progress"
  (interactive)
  (when (org-roam-file-p)
    (when-let*
        ((option
          "HUGO_ALIASES")
         (raw-roam-aliases
          (read (format "(%s)"
                        (org-export-get-node-property
                         :ROAM_ALIASES
                         (org-element-parse-buffer)))))
         (roam-aliases
          (mapcar
           #'downcase
           (mapcar
            (lambda (alias)
              (replace-regexp-in-string
               " "
               "_"
               alias))
            raw-roam-aliases))))
      ;;roam-aliases
      roam-aliases
      )))

(defun cashpw/org-roam--mirror-roam-aliases-to-hugo-aliases ()
  "Copy the list of ROAM_ALIASES into HUGO_ALIASES."
  (interactive)
  (when (org-roam-file-p)
    (when-let
        ((option
          "HUGO_ALIASES")
         (raw-roam-aliases
          (org-export-get-node-property
           :ROAM_ALIASES
           (org-element-parse-buffer))))
      (message raw-roam-aliases))))

(defun cashpw/org-hugo--build-custom-front-matter-from-properties (properties)
  (string-join
   (mapcar
    (lambda (property)
      (goto-char (point-min))
      (let ((key (downcase property))
            (value (org-entry-get (point)
                                  property)))
        (s-lex-format
         ":${key} ${value}")))
    properties)
   " "))

(defun cashpw/org-roam-mirror-roam-refs-to-front-matter ()
  "Copy the list of ROAM_REFS into hugo_custom_front_matter."
  (interactive)
  (when (and (org-roam-file-p)
             (not (member
                   (buffer-file-name)
                   cashpw/org-roam--file-path-exceptions-to-mirror-refs-to-front-matter)))
    (when-let*
        ((keyword "HUGO_CUSTOM_FRONT_MATTER")
         (raw-roam-refs (org-export-get-node-property
                         :ROAM_REFS
                         (org-element-parse-buffer)))
         (refs (split-string
                raw-roam-refs
                " +"))
         (valid-refs (-filter
                      (lambda (ref)
                        (not (string-match-p "^\\[cite" ref)))
                      refs))
         (roam-refs (format
                     "roam_refs '(%s)"
                     (string-join
                      (mapcar
                       (lambda (ref)
                         (format "\"%s\""
                                 ref))
                       valid-refs)
                      " ")))
         (current-roam-refs (or
                             (org-roam-get-keyword
                              keyword)
                             "")))
      (if (not (string=
                roam-refs
                current-roam-refs))
          (org-roam-set-keyword
           (downcase keyword)
           roam-refs)))))

(defun cashpw/bibliography-present-in-buffer-p ()
  "Return non-nil if there is a bibliography in the current buffer; nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^\\* Bibliography" nil t)))

(defun cashpw/citation-present-in-buffer-p ()
  "Return non-nil if a citation is present in the current buffer, nil otherwise."
  (let ((citation-prefix "[cite"))
    (save-excursion
      (goto-char (point-min))
      (search-forward citation-prefix nil t))))

(defcustom cashpw/org-roam--file-path-exceptions-to-add-bibliography
  '()
  "File paths which will not have a bibliography added by `cashpw/org-roam-add-bibliography'.")

(defun cashpw/org-roam-add-bibliography (&optional skip-if-present)
  "Add bibiliography to the current buffer."
  (interactive)
  (when (and (org-roam-file-p)
             (not (member (buffer-file-name)
                          cashpw/org-roam--file-path-exceptions-to-add-bibliography))
             (not (cashpw/bibliography-present-in-buffer-p))
             (cashpw/citation-present-in-buffer-p))
    (save-excursion
      (goto-char (point-max))
      (org-insert-heading nil t t)
      (insert "Bibliography")
      (newline)
      (insert "#+print_bibliography:"))))

(defcustom cashpw/org-roam--file-path-exceptions-to-add-flashcards
  '()
  "File paths which won't hold flashcards.")

(defun cashpw/org--insert-heading-if-missing (heading-text &optional todo priority tags)
  "Insert heading with HEADING-TEXT if no such heading exists in current buffer.

Optional:

- Set TODO state
- Set PRIORITY
- Set TAGS"
  (let ((heading-regexp
         (rx
          "* "
          ;; todo state
          (zero-or-more alpha)
          (zero-or-one " ")
          ;; priority
          (zero-or-one "\[#" (= 1 alphanumeric) "\]")
          (zero-or-one " ")
          (literal heading-text))))
    (unless (cashpw/buffer-contains-regexp-p heading-regexp)
      (save-excursion
        (goto-char (point-max))
        (org-insert-heading nil t t)
        (insert heading-text)
        (when tags
          (org-set-tags tags))
        (when priority
          (org-priority priority))
        (when todo
          (org-todo todo))))))

(defun cashpw/org-roam--current-buffer-archive-p ()
  "TODO."
  (cashpw/org--archive-buffer-p
   (current-buffer)))

(defun cashpw/org-roam--current-file-path-no-flashcard-p ()
  "TODO."
  (member
   (buffer-file-name)
   cashpw/org-roam--file-path-exceptions-to-add-flashcards))

(defcustom cashpw/org-roam--skip-add-flashcard-fns
  '(
    (lambda ()
      (not
       (org-roam-file-p)))
    cashpw/org-roam--current-buffer-archive-p
    cashpw/org-roam--current-file-path-no-flashcard-p)
  "Skip adding flashcard heading if any of these functions return non-nil."
  :type 'hook)

(defun cashpw/org-roam-add-flashcards (&optional todo priority tags)
  "Add flashcard heading to the current buffer."
  (interactive)
  (unless (--any-p
         (funcall it)
         cashpw/org-roam--skip-add-flashcard-fns)
    (cashpw/org--insert-heading-if-missing
     "Flashcards"
     todo
     priority
     tags)))

(defun cashpw/org-roam-insert-tag-link ()
  "Insert a link to the selected tag"
  (interactive)
  (let ((tag
         (completing-read
          "Select tag: "
          (cashpw/org-roam--get-filetags)
          )))
    (insert
     (format "[[/tags/%s][%s]]"
             (downcase
              (nth 0
                   (org-hugo--tag-processing-fn-replace-with-hyphens-maybe
                    `(,tag)
                    `(:hugo-prefer-hyphen-in-tags ,org-hugo-prefer-hyphen-in-tags))))
             tag))))

(defun cashpw/org-roam-before-save ()
  (when (vulpea-buffer-p)
    (cashpw/org-roam-rewrite-smart-to-ascii)
    ;; (cashpw/org-roam-mirror-roam-refs-to-front-matter)
    (cashpw/org-roam-add-bibliography)
    (cashpw/org-roam-add-flashcards
     "TODO"
     2
     ":noexport:")
    (cashpw/org-roam-update-hastodo-tag)
    (cashpw/org-roam-update-has-gallery-tag)
    ;; (cashpw/org-hugo-export-wim-to-md)
    ))

(defun cashpw/org-hugo-linkify-mathjax (mathjax-post-map)
  (cl-loop for (target . post-id) in mathjax-post-map
           do (save-excursion
                (goto-char (point-min))
                (replace-regexp target
                                (s-lex-format "\\\\href{/posts/${post-id}}{\\1}")))))

(setq
 cashpw/org-hugo--mathjax-post-map '(("\\(C\\\\_{n}\\)" . "centering_matrix")
                                         ("\\(I\\\\_{n}\\)" . "identity_matrix")
                                         ("\\(I\\\\_{[0-9]+}\\)" . "identity_matrix")
                                         ("\\(J\\\\_{[0-9]+}\\)" . "matrix_of_ones")
                                         ("\\(J\\\\_{[0-9]+,[0-9]+}\\)" . "matrix_of_ones")
                                         ("\\(J\\\\_{[0-9]+ \\\\times [0-9]+}\\)" . "matrix_of_ones")
                                         ("\\(\\\\cos\\)" . "cosine")
                                         ("\\(\\\\sin\\)" . "sine")
                                         ("\\(\\\\vert . \\\\vert\\)" . "cardinality")
                                         ("\\(\\\\tan\\)" . "tangent")))

(defun org-hugo--after-1-export-function (info outfile)
  "Function to be run after exporting one post.

The post could be exported using the subtree-based or file-based
method.

This function is called in the end of `org-hugo-export-to-md',
and `org-hugo-export-as-md'.

INFO is a plist used as a communication channel.

OUTFILE is the Org exported file name.

This is an internal function."
  (advice-remove 'org-cite-export-bibliography #'org-hugo--org-cite-export-bibliography)
  (advice-remove 'org-info-export #'org-hugo--org-info-export)
  (advice-remove 'org-babel--string-to-number #'org-hugo--org-babel--string-to-number)
  (advice-remove 'org-babel-exp-code #'org-hugo--org-babel-exp-code)
  (when (and outfile
             (org-hugo--pandoc-citations-enabled-p info))
    (require 'ox-hugo-pandoc-cite)
    (plist-put info :outfile outfile)
    (plist-put info :front-matter org-hugo--fm)
    (org-hugo-pandoc-cite--parse-citations-maybe info))
  (setq org-hugo--fm nil)
  (setq org-hugo--fm-yaml nil)
  (when outfile
    (with-current-buffer
        (find-file-noselect outfile)
      (cashpw/org-hugo-linkify-mathjax cashpw/org-hugo--mathjax-post-map)
      (save-buffer))))

(defun cashpw/org-hugo--get-custom-front-matter ()
  "Return custom front-matter as a string."
  (string-join
   (mapcar
    (lambda (item)
      (cl-destructuring-bind (label . value) item
        (s-lex-format
         ":${label} \"${value}\"")))
    (cl-remove-if
     (lambda (item)
       (not (cdr item)))
     `(("prep_time" . ,(org-recipes-get-prep-duration (point-min)))
       ("cook_time" . ,(org-recipes-get-cook-duration (point-min)))
       ("total_time" . ,(org-recipes-get-total-duration (point-min)))
       ("servings" . ,(org-recipes-get-servings (point-min)))
       ("yield" . ,(org-recipes-get-yield (point-min)))
       ("slug" . ,(save-excursion
                    (org-entry-get (point-min) "ID"))))))
   " "))

(defun cashpw/org-hugo--set-custom-front-matter ()
  "Set custom hugo front-matter."
  (org-roam-set-keyword
   "HUGO_CUSTOM_FRONT_MATTER"
   (cashpw/org-hugo--get-custom-front-matter)))

(defun cashpw/get-property (property)
  (save-excursion
    (goto-char (point-min))
    (org-entry-get (point)
                   property)))

(defun cashpw/split-aliases-to-string (roam-aliases)
  (mapcar
   (lambda (roam-alias)
     (downcase
      (replace-regexp-in-string
       "\""
       ""
       (replace-regexp-in-string
        " "
        "-"
        roam-alias))))
   (split-string roam-aliases
                 "\" \""
                 nil)))

(defun cashpw/get-aliases ()
  (interactive)
  (let* ((roam-aliases (cashpw/get-property "ROAM_ALIASES"))
         (aliases
          (if roam-aliases
              (cashpw/split-aliases-to-string
               roam-aliases)
            '())))
    (string-join
     (mapcar
      (lambda (roam-alias)
        (s-lex-format "/posts/${roam-alias}"))
      aliases)
     " ")))

(after! org-roam
  (defun org-hugo-export-wim-to-md-after-save ()
    (cashpw/org-hugo-export-wim-to-md))
  (setq
   cashpw/org-hugo-replace-front-matter-with-title nil))

(defun cashpw/revert-file (filename)
  "Revert FILENAME."
  (with-current-buffer
      (find-file-noselect
       filename)
    (revert-buffer)))

(defun cashpw/revert-common-files ()
  (interactive)
  (let* ((files
          `(,(cashpw/path-todos)
            ,(cashpw/path-calendar)))
         (files-with-archive
          (append
           files
           (org-extras-files-archive-files
            files))))
    (--each
        files-with-archive
      (cashpw/revert-file it))))

;; (defun vulpea-agenda-files-update (&rest _)
;;   "Update the value of `org-agenda-files'."
;;   (setq
;;    org-agenda-files (cashpw/org-agenda-files 'notes-with-todo)))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defun cashpw/org-roam--export-backlinks (backend)
  "Add backlinks to roam buffer for export; see `org-export-before-processing-hook'."
  (when (org-roam-file-p)
    (let* ((current-node (org-roam-node-at-point))
           (current-node-file (org-roam-node-file current-node))
           (backlinks (let ((-compare-fn (lambda (a b)
                                           (equal (org-roam-node-id (org-roam-backlink-source-node a))
                                                  (org-roam-node-id (org-roam-backlink-source-node b))))))
                        (--sort
                         ;; Alpha sort, descending
                         (string< (org-roam-node-title (org-roam-backlink-source-node it))
                                  (org-roam-node-title (org-roam-backlink-source-node other)))
                         (--filter
                          ;; Exclude backlinks to the current file
                          (not (equal (org-roam-node-file (org-roam-backlink-source-node it))
                                      current-node-file))
                          (-uniq (org-roam-backlinks-get current-node))))))
           (backlinks-as-string (--reduce-from
                                 (let* ((source-node (org-roam-backlink-source-node it))
                                        (id (org-roam-node-id source-node))
                                        (file (org-roam-node-file source-node))
                                        (title (org-roam-node-title source-node)))
                                   (concat acc
                                           (s-lex-format " - [[id:${id}][${title}]]\n")))
                                 ""
                                 backlinks)))
      (unless (string= backlinks-as-string "")
        (save-excursion
          (goto-char
           (point-max))
          (insert (concat "\n* Backlinks\n"
                          backlinks-as-string)))))))

(add-hook! 'org-export-before-processing-hook
           'cashpw/org-roam--export-backlinks)

(defun cashpw/journal--create (name)
  "Create a daily, journal, entry with NAME."
  (org-roam-dailies--capture time t)
  (save-buffer))

(defun cashpw/journal--remove-old-hastodo (pattern)
  "Remove hastodo tag from old journal entries matching PATTERN."
  (let ((journal-files
         (--filter
          (string-match-p pattern it)
          (cashpw/org-files-with-tag "hastodo" org-roam-directory))))
    (dolist (journal-file journal-files)
      (with-current-buffer (find-file-noselect journal-file)
        (set-buffer-modified-p t)
        (save-buffer)))))

(after! doct-org-roam
  (setq
   org-roam-dailies-directory cashpw/path--notes-dir
   org-roam-dailies-capture-templates (doct-org-roam `((:group "org-roam-dailies"
                                                        :type plain
                                                        :template "%?"
                                                        :file "%<%Y-%m-%d>.org"
                                                        :unnarrowed t
                                                        :immediate-finish t
                                                        :children (("Day"
                                                                    :keys "d"
                                                                    :head ("#+title: %<%Y-%m-%d>"
                                                                           "#+author: Cash Prokop-Weaver"
                                                                           "#+date: [%<%Y-%m-%d %a %H:%M>]"
                                                                           "#+category: Journal"
                                                                           "#+filetags: :journal:private:hastodo:"
                                                                           "
* TODO [#2] Journal
SCHEDULED: <%<%Y-%m-%d %a 19:30>>
:PROPERTIES:
:Effort:   15m
:END:

** Me

*** Thought records

** Cayla

** About the day

* TODO [#2] Gratitude
SCHEDULED: <%<%Y-%m-%d %a 19:30>>
:PROPERTIES:
:Effort:   2m
:END:

1. TODO
2. TODO
3. TODO

* TODO [#2] Retrospective
SCHEDULED: %(org-insert-time-stamp (time-add (date-to-time \"%<%Y-%m-%d> 05:00:00\") (days-to-time 1)) t)
:PROPERTIES:
:Effort:   1m
:END:

#+begin_src emacs-lisp :results none
(outline-next-heading)
(previous-line)
(cashpw/org-clocktable-by-category-yesterday)
(org-up-heading-safe)
(search-forward-regexp \"begin_src\")
(dotimes (i 8) (delete-line))
#+end_src

* Flashcards :noexport:"))))))))

(defun cashpw/org-clocktable-by-category-yesterday ()
  "Insert a retrospective clocktable for yesterday."
  (interactive)
  (let ((clocktable-by-category--default-properties
         (cashpw/clocktable-by-category--properties
          (time-subtract
           (current-time)
           (days-to-time
            1)))))
    (clocktable-by-category-report)))

(defun cashpw/org-clocktable-by-category-last-week ()
  "Insert a retrospective clocktable for yesterday."
  (interactive)
  (let ((clocktable-by-category--default-properties
         (cashpw/clocktable-by-category--properties
          (time-subtract
           (current-time)
           (days-to-time
            1)))))
    (plist-put
     clocktable-by-category--default-properties
     :block
     (format-time-string
      "%Y-W%W"
      (time-subtract
       (current-time)
       (days-to-time
        7))))
    (clocktable-by-category-report)))

(defun cashpw/org-noter-insert-selected-text-inside-note-content ()
  "Insert selected text in org-noter note.

Reference: https://github.com/weirdNox/org-noter/issues/88#issuecomment-700346146"
  (interactive)
  (progn
    (setq
     current-buffer-name (buffer-name))
    (org-noter-insert-precise-note)
    (set-buffer current-buffer-name)
    (org-noter-insert-note)))

(use-package! org-agenda)
(use-package! evil-org-agenda)
(use-package! org-super-agenda
  :demand t

  :after (:all org-agenda
               evil
               evil-org-agenda)
  :hook
  ((org-agenda-mode . org-super-agenda-mode))

  :custom
  (org-agenda-prefix-format
   '((agenda . " %i %-30(cashpw/org-agenda-category 30)%?-12t%-6e% s")
     (todo . " %i %-30(cashpw/org-agenda-category 30) %-6e %-40(cashpw/org-agenda-buganizer-title)")
     (tags . " %i %-12c")
     (search . " %i %-12c")))
  (org-agenda-log-mode-items
   '(state
     closed
     clock))
  (org-super-agenda-header-map
   evil-org-agenda-mode-map)
  ;; https://emacs.stackexchange.com/a/17128
  (org-agenda-sorting-strategy
   '((agenda time-up priority-down category-keep)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))

  :config
  (setq org-super-agenda-header-map evil-org-agenda-mode-map)
  (cl-defun org-super-agenda--group-dispatch-take (items (n group))
    ;;(cl-defun org-super-agenda--group-dispatch-take (items n-and-group)
    "Take N ITEMS that match selectors in GROUP.
If N is positive, take the first N items, otherwise take the last N items.
Note: the ordering of entries is not guaranteed to be preserved, so this may
not always show the expected results."
    (-let* (((name non-matching matching) (org-super-agenda--group-dispatch items group))
            (take-fn (if (cl-minusp n) #'-take-last #'-take))
            (placement (if (cl-minusp n) "Last" "First"))
            (name (format "%s %d %s" placement (abs n) name)))
      (list name non-matching (funcall take-fn (abs n) matching)))))

(defun cashpw/org-super-agenda--get-priority (item)
  "'org-super-agenda' `:auto-map'-compatible for the given ITEM."
  (-when-let* ((marker (or (get-text-property 0 'org-marker item)
                           (get-text-property 0 'org-hd-marker)))
               (default-priority "?")
               (priority (or (org-extras-get-priority marker)
                             default-priority)))
    (s-lex-format "p${priority}")))

(defun cashpw/org-super-agenda--get-heading (item)
  "'org-super-agenda' `:auto-map'-compatible for the given ITEM."
  (-when-let* ((marker (or (get-text-property 0 'org-marker item)
                           (get-text-property 0 'org-hd-marker))))
     (org-entry-get marker "ITEM")))

(defun cashpw/org-super-agenda--get-category (item)
  "'org-super-agenda' `:auto-map'-compatible for the given ITEM."
  (-when-let* ((marker (or (get-text-property 0 'org-marker item)
                           (get-text-property 0 'org-hd-marker))))
    (org-get-category marker)))

(defun cashpw/org-super-agenda--get-first-n-from-roam-tag (n tag)
  "Return plist to group the first N headings tagged TAG.

Intended for use with `org-super-agenda-groups'."
  `(:name ,(s-lex-format "${tag} (${n})")
    :transformer (replace-regexp-in-string "TODO " ""
                                           (replace-regexp-in-string "\\[#[0-9]\\] " "" it))
    :take (,n (:tag ,tag))))

(after! org
  :config
  (setq
   calendar-week-start-day 1
   org-agenda-entry-text-maxlines 30
   org-agenda-entry-text-leaders "  "
   org-agenda-time-grid '((daily
                           today
                           require-timed
                           ;; remove-match
                           )
                          (500 600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
                          ;; (800 1000 1200 1400 1600 1800 2000)
                          "......" "----------------")
   ))

(defun cashpw/org-mode-buffer-property-get (property-name)
  (org-with-point-at 1
    (when (re-search-forward
           (concat "^#\\+" property-name ": \\(.*\\)")
           (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(after! org-agenda
  (setq
   org-duration-units `(("m" . 1)
                        ("min" . 1)
                        ("mins" . 1)
                        ("h" . 60)
                        ("d" . ,(* 60 24))
                        ("w" . ,(* 60 24 7))
                        ("mo" . ,(* 60 24 30))
                        ("mos" . ,(* 60 24 30))
                        ("M" . ,(* 60 24 30))
                        ("y" . ,(* 60 24 365.25)))
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-include-deadlines t
   org-agenda-block-separator nil
   org-agenda-compact-blocks t
   org-agenda-start-day nil ;; i.e. today
   org-agenda-span 3
   org-agenda-include-diary t
   org-agenda-start-on-weekday nil))

(defun cashpw/org-agenda-custom-commands--update ()
  "Update `org-agenda-custom-commands'."
  (setq
   org-agenda-custom-commands `((".inbox" "Inbox" ,(cashpw/org-agenda-view--inbox))
                                (".overdue" "Overdue" ,(cashpw/org-agenda-view--overdue))
                                (".gallery" "Gallery" ,(cashpw/org-agenda-view--gallery))
                                (".gallery-all" "Gallery (all)" ,(cashpw/org-agenda-view--gallery-all))
                                (".gallery-photos" "Gallery (photos)" ,(cashpw/org-agenda-view--gallery-photos))
                                (".gallery-current-buffer" "Gallery (current buffer)" ,(cashpw/org-agenda-view--gallery-current-buffer))
                                (".plan-week" "Week" ,(cashpw/org-agenda-view--plan--week))
                                (".review-clockcheck" "Clock check" ,(cashpw/org-agenda-view--review--clockcheck))
                                (".review-clockreport" "Clock report" ,(cashpw/org-agenda-view--review--clockreport))
                                (".review-logged" "Logged" ,(cashpw/org-agenda-view--review-logged))
                                (".review-logged-today" "Logged" ,(cashpw/org-agenda-view--review-logged-today))
                                (".roam-roam" "Roam" ,(cashpw/org-agenda-view--roam--roam))
                                (".roam-readinglist" "Reading list" ,(cashpw/org-agenda-view--roam--readinglist))
                                (".today" "Today" ,(cashpw/org-agenda-view--today))
                                (".habits" "Habits" ,(cashpw/org-agenda-view--habits))
                                (".without-effort" "Without effort" ,(cashpw/org-agenda-view--no-effort))
                                (".without-priority" "Priority" ,(cashpw/org-agenda-view--no-priority))
                                (".without-scheduled" "Not scheduled" ,(cashpw/org-agenda-view--not-scheduled)))))

(defun cashpw/org-agenda-custom-commands--maybe-update ()
  "Update when all functions are defined."
  (when cashpw/personal-config-loaded-p
    (cashpw/org-agenda-custom-commands--update)))

(defun cashpw/org-agenda--simplify-line (line)
  "Return LINE without frills."
  (-->
   line
   (cashpw/org-agenda--remove-priorities it)
   (cashpw/org-agenda--remove-todo it)))

(defun cashpw/org-agenda--remove-priorities (line)
  "Return LINE without priorities."
  (--> line (replace-regexp-in-string "\\[#[0-9]\\] " "" it)))

(defun cashpw/org-agenda--remove-todo (line)
  "Return LINE without todo."
  (--> line (replace-regexp-in-string "TODO " "" it)))

(defun cashpw/org-super-agenda--simplify-map (group)
  "Return GROUP after simplifying each line.

GROUP is a plist of the form `(:name ... :items ...)'.

Intended for use with `org-super-agenda' `:transformer'. "
  (plist-put
   group
   :items
   (--map (cashpw/org-agenda--simplify-line it) (plist-get group :items))))

(defun cashpw/org-super-agenda--remove-todo-map (group)
  "Return GROUP after simplifying each line.

GROUP is a plist of the form `(:name ... :items ...)'.

Intended for use with `org-super-agenda' `:transformer'. "
  (plist-put
   group
   :items (--map (cashpw/org-agenda--remove-todo it) (plist-get group :items))))

(defun cashpw/org-agenda--dim-line (line)
  "Dim color/brightness of LINE."
  (propertize line 'face 'shadow))

(defcustom cashpw/org-agenda--dim-headline-regexps
  '(
    ;; "Stretch"
    "Slack"
    "Huel"
    "Meditate"
    ;; "Shower"
    ;; "Shave"
    ;; "Walk"
    "Lunch"
    "Dinner"
    "Journal.*Journal"
    "Journal.*Retrospective"
    "Journal.*Gratitude"
    ;; "Debrief"
    "Tidy: "
    "Diet: "
    "forsale mailing lists"
    ;; <= 5 minutes effort
    ;; " [12345]m "
    ;; No duration, no effort; brittle
    " \\([0-9]\\)?[0-9]:[0-9][0-9]\\.\\.\\.\\.\\.\\. \\(. \\)  ")
  "List of headlines to dim in the org agenda view."
  :type '(repeat string))

(defun cashpw/time-hh-mm-to-minutes (hh-mm)
  "Return number of minutes since midnight for HH-MM."
  (let ((decoded-time (parse-time-string hh-mm)))
    (+ (* 60 (decoded-time-hour decoded-time))
       (decoded-time-minute decoded-time))))

(defun cashpw/time-minutes-between-time-range (start-hh-mm end-hh-mm)
  "Return length of time between START-HH-MM and END-HH-MM."
  (let ((start-minutes (cashpw/time-hh-mm-to-minutes start-hh-mm))
        (end-minutes (cashpw/time-hh-mm-to-minutes end-hh-mm)))
    (- end-minutes start-minutes)))

(defconst cashpw/time-re-hh-mm-hh-mm
  (rx (group num num ":" num num) "-" (group num num ":" num num))
  "Matches HH:MM-HH:MM.")

(defun cashpw/org-agenda--maybe-dim-headline (line)
  "Dim LINE if it's on the list."
  (if (or
       ;; (when (string-match cashpw/time-re-hh-mm-hh-mm line)
       ;;   (< (cashpw/time-minutes-between-time-range
       ;;       (match-string 1 line) (match-string 2 line))
       ;;      5))
       (--any (string-match-p it line) cashpw/org-agenda--dim-headline-regexps))
      (cashpw/org-agenda--dim-line line)
    line))

(defun cashpw/org-agenda-category (max-length)
  (s-truncate
   max-length
   (or (org-entry-get (point) "CATEGORY" t)
       (org-get-title)
       "")))

(defun cashpw/org-agenda-icon ()
  (let ((properties (org-entry-properties (point))))
    (concat
     (cond
      ((when-let ((scheduled (alist-get "SCHEDULED" properties nil nil 'string=)))
         (org-get-repeat scheduled))
       "⟳")
      ((when-let* ((file-path (alist-get "FILE" properties nil nil 'string=))
                   (file-name (f-filename file-path)))
         (string= file-name "calendar-personal.org"))
       "C")
      (t
       " "))
     " ")))

(defun cashpw/org-agenda-priority-offset ()
  (if (org-extras-get-priority nil)
      ""
    "    "))

(defun cashpw/org-agenda-reschedule-to-next-occurrence-or-kill ()
  "Reschedule to next occurrence if item at point repeats; else kill."
  (interactive)
  (org-agenda-with-point-at-orig-entry
      nil
    (if (org-get-repeat)
        (org-extras-reschedule-overdue-todo)
      (org-mark-subtree)
      (kill-region))))

(defun cashpw/org-agenda-done-yesterday ()
  "Mark as done yesterday."
  (interactive)
  (org-agenda-with-point-at-orig-entry
      nil
    (org-todo-yesterday 'done)))

(defun cashpw/org-reschedule-to-today-at-point ()
  "Reschedule heading at point to today. Keep duration and repeater."
  (interactive)
  (when-let ((scheduled-time-string (org-entry-get (point) "SCHEDULED"))
             (scheduled-time-string-without-year-month-day
              (replace-regexp-in-string
               "[0-9]\+-[0-9]\\{2\\}-[0-9]\\{2\\}"
               ""
               scheduled-time-string)))
    (cl-destructuring-bind
        (_ _ _ today-day today-month today-year _ _ _) (decode-time (current-time))
      (org-schedule
       nil
       (format "%s-%s-%s%s"
               today-year
               today-month
               today-day
               scheduled-time-string-without-year-month-day)))))

(defun cashpw/org-agenda-reschedule-to-today ()
  "Reschedule event at point to today."
  (interactive)
  (org-agenda-with-point-at-orig-entry
      nil (cashpw/org-reschedule-to-today-at-point)))

(defun cashpw/org-agenda-view-collapse ()
  "Collapse org-agenda view sections."
  (interactive)
  (set-selective-display 1))

(defun cashpw/org-agenda-view-expand ()
  "Expand org-agenda view sections."
  (interactive)
  (set-selective-display nil))

(defun cashpw/org-agenda-view-toggle-collapse ()
  "Toggle collapse/expand in org-agenda view."
  (interactive)
  (if selective-display
      (cashpw/org-agenda-view-expand)
    (cashpw/org-agenda-view-collapse)))

(defun cashpw/org-agenda-view--today--files ()
  (seq-uniq
   (append
    (cashpw/rgrep
     (format
      "-l :everyday: %s/*.org"
      cashpw/path--notes-dir))
    (cashpw/rgrep
     (format
      "-l \"\\(SCHEDULED\\|DEADLINE\\): <%s\" %s/*.org"
      (format-time-string "%F" (current-time))
      cashpw/path--notes-dir)))))

;; Deprecased because it's 2x slower
;; (defun cashpw/org-agenda-view--today--files-mem ()
;;   (let ((today-yyyy-mm-dd (format-time-string "%F" (current-time)))
;;         (org-not-done-keywords (with-temp-buffer
;;                                  (org-mode)
;;                                  org-not-done-keywords)))
;;     (cl-loop
;;      for file in (org-mem-all-files)
;;      unless (s-ends-with-p "archive" file)
;;      when (seq-find (lambda (entry)
;;                       (or
;;                        (member "everyday" (org-mem-entry-tags entry))
;;                        (or
;;                         (and
;;                          (org-mem-entry-scheduled entry)
;;                          (string-match today-yyyy-mm-dd (org-mem-entry-scheduled entry)))
;;                         (and
;;                          (org-mem-entry-deadline entry)
;;                          (string-match today-yyyy-mm-dd (org-mem-entry-deadline entry))))))
;;                     (org-mem-entries-in file))
;;      collect file)))

(defun cashpw/org-agenda-view--today ()
  "Return custom agenda command."
  `((agenda
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-dim-blocked-tasks t)
      (org-agenda-use-tag-inheritance t)
      (org-use-property-inheritance t)
      (org-agenda-span 1)
      (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
      (org-agenda-files (cashpw/org-agenda-view--today--files))
      (org-agenda-prefix-format '((agenda . " %i %-21(cashpw/org-agenda-category 20)%-12t%-2(cashpw/org-agenda-icon)%-5e")))
      (org-super-agenda-groups
       '((:discard
          (:scheduled future
           :scheduled past))
         (:name "Schedule"
          :time-grid t
          :order 0
          :transformer (--> it
                            (cashpw/org-agenda--simplify-line it)
                            (cashpw/org-agenda--maybe-dim-headline it)))
         (:discard
          (:and (:file-path ,(cashpw/path-calendar)
                 :scheduled past)))
         (:name "In Progress"
          :todo "INPROGRESS")
         (:auto-map cashpw/org-super-agenda--get-priority
          :transformer cashpw/org-super-agenda--simplify-map)
         (;; Toss all other todos
          :discard
          (:anything))))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(setq
 org-habit-preceding-days 7
 org-habit-following-days 0)

(defun cashpw/org-agenda-view--habits ()
  "Return custom agenda command."
  `((agenda
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-span 1)
      (org-agenda-prefix-format '((agenda . "%-20(cashpw/org-agenda-category 30)")))
      (org-agenda-sorting-strategy '((agenda . (alpha-up time-up))))
      (org-agenda-hide-tags-regexp ".*")
      (org-agenda-format-date "")
      (org-habit-show-all-today t)
      ;; (org-habit-show-habits-only-for-today nil)
      (org-super-agenda-groups
       '(
         (:name ""
          :auto-map cashpw/org-super-agenda--get-category
          :transformer cashpw/org-super-agenda--simplify-map)
         (;; Toss everything else
          :discard
          (:todo t
           :todo nil))))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-agenda-view--habit (files habit-heading-regexp preceding-days)
  "Return custom agenda command."
  (let ((groups
         `((:name "Habits" :and (:heading-regexp ,habit-heading-regexp :habit))
           ( ;; Toss everything else
            :discard
            (:todo t :todo nil)))))
    `((agenda
       ""
       ((org-agenda-overriding-header "")
        (org-agenda-span 1)
        (org-agenda-files ',files)
        (org-agenda-prefix-format
         '((agenda . "%-20(cashpw/org-agenda-category 30)")))
        (org-agenda-sorting-strategy '((agenda . (alpha-up time-up))))
        (org-agenda-hide-tags-regexp ".*")
        (org-agenda-format-date "")
        (org-habit-show-all-today t)
        ;; (org-habit-show-habits-only-for-today nil)
        (org-super-agenda-groups ',groups))))))

(defun cashpw/org-agenda-view--plan--week ()
  "Return custom agenda command."
  `((agenda
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-prefix-format '((agenda . " %i %-20(cashpw/org-agenda-category 30)%-12t%-5e")))
      (org-agenda-span 'week)
      (org-agenda-show-all-dates t)
      (org-habit-show-habits-only-for-today nil)
      (org-super-agenda-groups
       `((:name "Schedule"
          :time-grid t
          :order 0
          :transformer (--> it
                            (cashpw/org-agenda--simplify-line it)
                            (cashpw/org-agenda--maybe-dim-headline it)))
         ;; (:discard
          ;; (:scheduled future))
         (:auto-map cashpw/org-super-agenda--get-priority
          :transformer cashpw/org-super-agenda--simplify-map)
         (:discard
          (:todo t))))))
    (alltodo
     ""
     ((org-agenda-overriding-header "\n\nUnscheduled")
      (org-habit-show-habits-only-for-today nil)
      (org-super-agenda-groups
       '((:discard
          (:scheduled t))
         (:auto-map cashpw/org-super-agenda--get-priority
          :transformer cashpw/org-super-agenda--simplify-map)))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-agenda-view--review-files (&optional time)
  "Return list of files for review agenda views for TIME."
  (let* ((yyyy-mm-dd (format-time-string "%F" (or time (current-time))))
         (state-grep-string (format "\\- State.* \\[%s" yyyy-mm-dd))
         (clock-grep-string (format "CLOCK: \\[%s" yyyy-mm-dd)))
    (seq-uniq
     (append
      (cashpw/rgrep
       (format "-l \"%s\" %s/*.org" state-grep-string cashpw/path--notes-dir))
      (cashpw/rgrep
       (format "-l \"%s\" %s/*.org" clock-grep-string cashpw/path--notes-dir))
      (cashpw/rgrep
       (format "-l \"%s\" %s/*.org_archive"
               state-grep-string
               cashpw/path--notes-dir))
      (cashpw/rgrep
       (format "-l \"%s\" %s/*.org_archive"
               clock-grep-string
               cashpw/path--notes-dir))))))

;; Deprecased because it's ~2x slower
;; (defun cashpw/org-agenda-view--review--today-files ()
;;   (let ((today-yyyy-mm-dd (format-time-string "%F" (current-time))))
;;     (cl-loop
;;      for file in (org-mem-all-files)
;;      when (seq-find
;;            (lambda (entry)
;;              (seq-find
;;               (lambda (clock-list)
;;                 (when (listp clock-list)
;;                   (let ((start (car clock-list)))
;;                     (cashpw/time-today-p (encode-time (parse-time-string start))))))
;;               (org-mem-entry-clocks entry)))
;;            (org-mem-entries-in file))
;;      collect file)))

(defun cashpw/org-agenda-view--review-logged ()
  "Return custom agenda command."
  `((agenda
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-span 'day)
      (org-agenda-start-day (org-read-date))
      (org-agenda-files (cashpw/org-agenda-view--review-files (org-read-date nil t)))
      (org-agenda-show-log t)
      (org-agenda-start-with-log-mode '(state closed clock))
      (org-agenda-hide-tags-regexp
       (concat org-agenda-hide-tags-regexp "\\|ARCHIVE"))
      (org-clocktable-defaults '(:fileskip0 t))
      (org-super-agenda-groups
       '((:name "Logged" :log t)
         ( ;; Toss all other todos
          :discard
          (:todo t))))))))

(defun cashpw/org-agenda-view--review-logged-today ()
  "Return custom agenda command."
  `((agenda
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-span 'day)
      (org-agenda-files (cashpw/org-agenda-view--review-files))
      (org-agenda-show-log t)
      (org-agenda-start-with-log-mode '(state closed clock))
      (org-agenda-hide-tags-regexp
       (concat org-agenda-hide-tags-regexp "\\|ARCHIVE"))
      (org-clocktable-defaults '(:fileskip0 t))
      (org-super-agenda-groups
       '((:name "Logged" :log t)
         ( ;; Toss all other todos
          :discard
          (:todo t))))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-agenda-view--review--clockcheck ()
  "Return custom agenda command."
  `((agenda
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-span 'day)
      (org-agenda-files (cashpw/org-agenda-view--review--today-files))
      (org-agenda-show-log 'clockcheck)
      (org-agenda-start-with-log-mode 'clockcheck)
      (org-clocktable-defaults '(:fileskip0 t))
      (org-super-agenda-groups '((:name "Clockcheck"
                                  :log t)
                                 (;; Toss all other todos
                                  :discard
                                  (:todo t))))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun clocktable-by-category--get-clocktable (&rest props)
  "Get a formatted clocktable with parameters according to PROPS.
The table is created in a temporary buffer, fully formatted and
fontified, and then returned."
  ;; Set the defaults
  (setq props (plist-put props :name "clocktable-by-category"))
  (unless (plist-member props :maxlevel)
    (setq props (plist-put props :maxlevel 2)))
  (unless (plist-member props :scope)
    (setq props (plist-put props :scope 'agenda)))
  (setq props (plist-put props :files org-agenda-files))
  (with-temp-buffer
    (org-mode)
    (org-create-dblock props)
    (org-update-dblock)
    (font-lock-ensure)
    (forward-line 2)
    (buffer-substring (point) (progn
                                (re-search-forward "^[ \t]*#\\+END" nil t)
                                (line-beginning-position)))))

;; Override to replace `org-agenda-get-clocktable' with `clocktable-by-category--get-clocktable`
(defun org-agenda-list (&optional arg start-day span with-hour)
  "Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm."
  (interactive "P")
  (when org-agenda-overriding-arguments
    (setq arg (car org-agenda-overriding-arguments)
          start-day (nth 1 org-agenda-overriding-arguments)
          span (nth 2 org-agenda-overriding-arguments)))
  (when (and (integerp arg) (> arg 0))
    (setq span arg arg nil))
  (when (numberp span)
    (unless (< 0 span)
      (user-error "Agenda creation impossible for this span(=%d days)" span)))
  (catch 'exit
    (setq org-agenda-buffer-name
          (org-agenda--get-buffer-name
           (and org-agenda-sticky
                (cond ((and org-keys (stringp org-match))
                       (format "*Org Agenda(%s:%s)*" org-keys org-match))
                      (org-keys
                       (format "*Org Agenda(%s)*" org-keys))
                      (t "*Org Agenda(a)*")))))
    (org-agenda-prepare "Day/Week")
    (setq start-day (or start-day org-agenda-start-day))
    (when (stringp start-day)
      ;; Convert to an absolute day number
      (setq start-day (time-to-days (org-read-date nil t start-day))))
    (org-compile-prefix-format 'agenda)
    (org-set-sorting-strategy 'agenda)
    (let* ((span (org-agenda-ndays-to-span (or span org-agenda-span)))
           (today (org-today))
           (sd (or start-day today))
           (ndays (org-agenda-span-to-ndays span sd))
           (org-agenda-start-on-weekday
            (and (or (eq ndays 7) (eq ndays 14))
                 org-agenda-start-on-weekday))
           (thefiles (org-agenda-files nil 'ifmode))
           (files thefiles)
           (start (if (or (null org-agenda-start-on-weekday)
                          (< ndays 7))
                      sd
                    (let* ((nt (calendar-day-of-week
                                (calendar-gregorian-from-absolute sd)))
                           (n1 org-agenda-start-on-weekday)
                           (d (- nt n1)))
                      (- sd (+ (if (< d 0) 7 0) d)))))
           (day-numbers (list start))
           (day-cnt 0)
           ;; FIXME: This may cause confusion when users are trying to
           ;; debug agenda.  The debugger will not trigger without
           ;; redisplay.
           (inhibit-redisplay (not debug-on-error))
           (org-agenda-show-log-scoped org-agenda-show-log)
           s rtn rtnall file date d start-pos end-pos todayp ;; e
           clocktable-start clocktable-end) ;; filter
      (setq org-agenda-redo-command
            (list 'org-agenda-list (list 'quote arg) start-day (list 'quote span) with-hour))
      (dotimes (_ (1- ndays))
        (push (1+ (car day-numbers)) day-numbers))
      (setq day-numbers (nreverse day-numbers))
      (setq clocktable-start (car day-numbers)
            clocktable-end (1+ (or (org-last day-numbers) 0)))
      (setq-local org-starting-day (car day-numbers))
      (setq-local org-arg-loc arg)
      (setq-local org-agenda-current-span (org-agenda-ndays-to-span span))
      (unless org-agenda-compact-blocks
        (let* ((d1 (car day-numbers))
               (d2 (org-last day-numbers))
               (w1 (org-days-to-iso-week d1))
               (w2 (org-days-to-iso-week d2)))
          (setq s (point))
          (org-agenda--insert-overriding-header
            (concat (org-agenda-span-name span)
                    "-agenda"
                    (cond ((<= 350 (- d2 d1)) "")
                          ((= w1 w2) (format " (W%02d)" w1))
                          (t (format " (W%02d-W%02d)" w1 w2)))
                    ":\n")))
        ;; Add properties if we actually inserted a header.
        (when (> (point) s)
          (add-text-properties s (1- (point))
                               (list 'face 'org-agenda-structure
                                     'org-date-line t))
          (org-agenda-mark-header-line s)))
      (while (setq d (pop day-numbers))
        (setq date (calendar-gregorian-from-absolute d)
              s (point))
        (if (or (setq todayp (= d today))
                (and (not start-pos) (= d sd)))
            (setq start-pos (point))
          (when (and start-pos (not end-pos))
            (setq end-pos (point))))
        (setq files thefiles
              rtnall nil)
        (while (setq file (pop files))
          (catch 'nextfile
            (org-check-agenda-file file)
            (let ((org-agenda-entry-types org-agenda-entry-types))
              ;; Starred types override non-starred equivalents
              (when (member :deadline* org-agenda-entry-types)
                (setq org-agenda-entry-types
                      (delq :deadline org-agenda-entry-types)))
              (when (member :scheduled* org-agenda-entry-types)
                (setq org-agenda-entry-types
                      (delq :scheduled org-agenda-entry-types)))
              ;; Honor with-hour
              (when with-hour
                (when (member :deadline org-agenda-entry-types)
                  (setq org-agenda-entry-types
                        (delq :deadline org-agenda-entry-types))
                  (push :deadline* org-agenda-entry-types))
                (when (member :scheduled org-agenda-entry-types)
                  (setq org-agenda-entry-types
                        (delq :scheduled org-agenda-entry-types))
                  (push :scheduled* org-agenda-entry-types)))
              (unless org-agenda-include-deadlines
                (setq org-agenda-entry-types
                      (delq :deadline* (delq :deadline org-agenda-entry-types))))
              (cond
               ((memq org-agenda-show-log-scoped '(only clockcheck))
                (setq rtn (org-agenda-get-day-entries
                           file date :closed)))
               (org-agenda-show-log-scoped
                (setq rtn (apply #'org-agenda-get-day-entries
                                 file date
                                 (append '(:closed) org-agenda-entry-types))))
               (t
                (setq rtn (apply #'org-agenda-get-day-entries
                                 file date
                                 org-agenda-entry-types)))))
            (setq rtnall (append rtnall rtn)))) ;; all entries
        (when org-agenda-include-diary
          (let ((org-agenda-search-headline-for-time t))
            (require 'diary-lib)
            (setq rtn (org-get-entries-from-diary date))
            (setq rtnall (append rtnall rtn))))
        (when (or rtnall org-agenda-show-all-dates)
          (setq day-cnt (1+ day-cnt))
          (insert
           (if (stringp org-agenda-format-date)
               (format-time-string org-agenda-format-date
                                   (org-time-from-absolute date))
             (funcall org-agenda-format-date date))
           "\n")
          (put-text-property s (1- (point)) 'face
                             (org-agenda-get-day-face date))
          (put-text-property s (1- (point)) 'org-date-line t)
          (put-text-property s (1- (point)) 'org-agenda-date-header t)
          (put-text-property s (1- (point)) 'org-day-cnt day-cnt)
          (when todayp
            (put-text-property s (1- (point)) 'org-today t))
          (setq rtnall
                (org-agenda-add-time-grid-maybe rtnall ndays todayp))
          (when rtnall (insert ;; all entries
                        (org-agenda-finalize-entries rtnall 'agenda)
                        "\n"))
          (put-text-property s (1- (point)) 'day d)
          (put-text-property s (1- (point)) 'org-day-cnt day-cnt)))
      (when (and org-agenda-clockreport-mode clocktable-start)
        (let ((org-agenda-files (org-agenda-files nil 'ifmode))
              ;; the above line is to ensure the restricted range!
              (p (copy-sequence org-agenda-clockreport-parameter-plist))
              tbl)
          (setq p (org-plist-delete p :block))
          (setq p (plist-put p :tstart clocktable-start))
          (setq p (plist-put p :tend clocktable-end))
          (setq p (plist-put p :scope 'agenda))
          (setq tbl (apply #'clocktable-by-category--get-clocktable p))
          (when org-agenda-clock-report-header
            (insert (propertize org-agenda-clock-report-header 'face 'org-agenda-structure))
            (unless (string-suffix-p "\n" org-agenda-clock-report-header)
              (insert "\n")))
          (insert tbl)))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (unless (or (not (get-buffer-window org-agenda-buffer-name))
                  (and (pos-visible-in-window-p (point-min))
                       (pos-visible-in-window-p (point-max))))
        (goto-char (1- (point-max)))
        (recenter -1)
        (when (not (pos-visible-in-window-p (or start-pos 1)))
          (goto-char (or start-pos 1))
          (recenter 1)))
      (goto-char (or start-pos 1))
      (add-text-properties (point-min) (point-max)
                           `(org-agenda-type agenda
                             org-last-args (,arg ,start-day ,span)
                             org-redo-cmd ,org-agenda-redo-command
                             org-series-cmd ,org-cmd))
      (when (eq org-agenda-show-log-scoped 'clockcheck)
        (org-agenda-show-clocking-issues))
      (org-agenda-finalize)
      (setq buffer-read-only t)
      (message ""))))

(defun cashpw/org-agenda-view--review--clockreport ()
  "Return custom agenda command."
  `((agenda
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-span 'day)
      (org-agenda-files (cashpw/org-agenda-view--review--today-files))
      ;; (org-clock-get-clocktable #'clocktable-by-tag--get-clocktable)
      (org-agenda-clockreport-mode t)
      (org-clocktable-defaults '(:fileskip0 t))
      (org-super-agenda-groups '(
                                 ;; (:name "Logged"
                                 ;; :log t)
                                 ;; (:log closed)
                                 ;; (:log clock)
                                 (;; Toss all other todos
                                  :discard
                                  (:todo t))))
      )
     ;; "~/review-day.html"
     )))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-select-and-go-to-todo (files)
  (let*
      ((separator "¥")
       (todos-with-file
        ;; A list of <file><separator><line-number><separator><heading> entries
        (-flatten
         (-map
          (lambda (file-sublist)
            (let ((space-separated-list-of-files (s-join " " file-sublist)))
              (cashpw/pcregrep
               (s-lex-format
                ;; Ignore headings with SCHEDULED or DEADLINE
                "--multiline --line-number --with-filename '^\\*[\\*]* (TODO|INPROGRESS) ([^\\n]*)\\n(?!SCHEDULED:)(?!DEADLINE:)' ${space-separated-list-of-files} | sed 's/:\\([0-9]*\\):/${separator}\\1${separator}/' | grep -v :unscheduled:"))))
          (-partition-all 50 files))))
       (match-alist
        (mapcar
         (lambda (todo-with-file)
           (cl-destructuring-bind (file line-number match)
               (s-split separator todo-with-file 'omit-nulls)
             (if (string-match org-complex-heading-regexp match)
                 (let* (
                        (priority (or (match-string 3 match) "[#?]"))
                        (priority-face (let ((face (or (cdr (assq (aref (substring priority 2 3) 0) org-modern-priority-faces))
                                                       t)))
                                         `(:inherit (,face org-modern-label))))
                        (heading (match-string 4 match)))
                   `(,(format "%s %s %s"
                              (propertize
                               (concat " #" (substring priority 2 3) " ")
                               'face
                               priority-face)
                              heading
                              (propertize
                               (format "(%s:%s)" file line-number)
                               'face
                               'shadow))
                     .
                     (:file ,file
                      :heading ,heading
                      :priority ,priority
                      :line-number ,(string-to-number line-number))))
               (error "Error! %s (match: %s)" todo-with-file match))))
         todos-with-file))
       (selection
        (let ((vertico-sort-function
               (lambda (candidates)
                 ;; Sort by priority and randomize order for same priority
                 (sort
                  candidates
                  (lambda (a b)
                    (let ((priority-a (substring a 0 4))
                          (priority-b (substring b 0 4)))
                      (cond
                       ((string= priority-a priority-b)
                        (= 0 (random 2)))
                       (t
                        (string< a b)))))))))
          (alist-get (completing-read "Select TODO: " match-alist) match-alist
                     nil
                     nil
                     #'string=))))
    (goto-line (plist-get selection :line-number)
               (find-file (plist-get selection :file)))))

(defun cashpw/org-agenda-view--roam--roam ()
  "Return custom agenda command."
  `((alltodo
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-dim-blocked-tasks nil)
      (org-agenda-inhibit-startup t)
      (org-agenda-use-tag-inheritance nil)
      (org-agenda-prefix-format '((agenda . "%-20(org-get-title)")))
      (org-agenda-ignore-properties '(effort appt category stats))
      (org-agenda-files
       )
      (org-super-agenda-groups
       `((:discard
          (:scheduled t
           :deadline t))
         (:name "In Progress"
          :todo "INPROGRESS")
         (:auto-map cashpw/org-super-agenda--get-priority)
         ))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-agenda-view--roam--readinglist ()
  "Return custom agenda command."
  `((alltodo
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-prefix-format '((todo . " %i ")))
      (org-agenda-files (list cashpw/path--reading-list))
      (org-agenda-dim-blocked-tasks nil)
      (org-super-agenda-groups
       (--map
        (cashpw/org-super-agenda--get-first-n-from-roam-tag 10
                                                            it)
        (with-current-buffer (find-file-noselect cashpw/path--reading-list)
          (org-extras-get-all-tags-in-file))))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-agenda-view--inbox-get-scheduled-time-string ()
  "Return scheduled string for inbox view."
  (cond
   ((org-entry-get
     (point)
     "SCHEDULED"
     nil)
    (progn
      (search-forward "SCHEDULED: ")
      (org-element-timestamp-parser)
      (let* ((timestamp (org-element-timestamp-parser))
             (start-time (org-timestamp-to-time timestamp))
             (end-time (org-timestamp-to-time timestamp t)))
        (if (and start-time
                 end-time
                 (not (time-equal-p start-time
                                    end-time)))
            "[X]"
          ;; "📅⌛"
          "[X]"
          ;; "📅"
          ))))
   (t
    "[ ]"
    ;; "❌"
    )))

(defun cashpw/org-agenda-view--inbox-get-effort-string ()
  "Return effort string for inbox view."
  (let ((effort-minutes
         (let ((effort (org-entry-get
                        (point)
                        "Effort")))
           (when effort
             (org-duration-to-minutes effort)))))
    (cond
     ((and effort-minutes
           (> effort-minutes
              (* 60 4)))
      ">4h")
     (effort-minutes
      "[X]"
      ;; "✅"
      )
     (t
      "[ ]"
      ;; "❌"
      ))))

(defun cashpw/org-agenda-view--inbox-get-category-string ()
  "Return effort string for inbox view."
  (let ((category (org-get-category)))
    (cond
     ((and category
           (string= category "Inbox"))
      "Inbox")
     (category
      "[X]"
      ;; "✅"
      )
     (t
      "[ ]"
      ;; "❌"
      ))))

(defun cashpw/org-agenda-view--specified-p (item)
  "Return non-nil if ITEM should be shown in the Inbox agenda view.

Indended to be passed as a `:pred` in `org-super-agenda-groups'."
  (let* ((marker
          (or (get-text-property 0 'org-marker item)
              (get-text-property 0 'org-hd-marker item)))
         (category
          (get-text-property 0 'org-category item))
         (effort-minutes
          (get-text-property 0 'effort-minutes item))
         (scheduled
          (org-super-agenda--when-with-marker-buffer marker
            (org-narrow-to-element)
            (when (org-entry-get marker "SCHEDULED" nil)

              (search-forward "SCHEDULED: ")
              ;; (buffer-substring-no-properties (point) (+ (point) 15))
              (org-element-timestamp-parser)
              (let* ((timestamp (org-element-timestamp-parser))
                     (start-time (org-timestamp-to-time timestamp))
                     (end-time (org-timestamp-to-time timestamp t)))
                `(:start ,start-time
                  :end ,end-time)))))
         (duration-minutes
          (when scheduled
            (/ (time-subtract (plist-get scheduled :end)
                              (plist-get scheduled :start))
               60)))
         (duration-or-effort-minutes
          (or (if (and duration-minutes
                       (= duration-minutes 0))
                  nil
                duration-minutes)
              effort-minutes))
         (explicit-category-p
          ;; Must have an explicit category; default is "Inbox".
          (and category
               (not (string=
                     category
                     "Inbox")))))
    ;; (message "[cashpw] %s
    ;; explicit-category-p: %s
    ;; duration-or-effort-minutes: %s
    ;; scheduled: %s"
    ;;              item
    ;;              explicit-category-p
    ;;              duration-or-effort-minutes
    ;;              (prin1-to-string scheduled))
    (and
     explicit-category-p
     scheduled
     duration-or-effort-minutes
     ;; Must have a time estimate of <4 hours
     (< duration-or-effort-minutes
        (1+ (* 60 4))))))

(defun cashpw/org-agenda-view--inbox ()
  "Return custom agenda command."
  `((alltodo
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-use-tag-inheritance nil)
      (org-use-property-inheritance nil)
      (org-agenda-files (cashpw/org-agenda-files--update))
      (org-agenda-prefix-format '((todo . "%-8(cashpw/org-agenda-view--inbox-get-category-string)   %-9(cashpw/org-agenda-view--inbox-get-scheduled-time-string)   %-6(cashpw/org-agenda-view--inbox-get-effort-string) %(cashpw/org-agenda-buganizer-title)")))
      (org-super-agenda-groups
       '((;; Automatically named "Log"
          :log t)
         (:discard
          (;; Don't bother listing PROJ items. They are used to group actionable TODOs.
           :todo "PROJ"))
         (:discard
          (:tag ("someday"
                 "reminder")))
         (:name "Inbox

To remove from Inbox:

1. Tag with \"someday\"
2. Tag with \"reminder\"
3. Specify all of the following:
   - SCHEDULE start time
   - Set effort or SCHEDULE end time for <4 hours
   - An explicit CATEGORY

Category | Scheduled | Effort
"
          :not (:pred cashpw/org-agenda-view--specified-p))
         (:discard
          (:anything))))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-agenda-view--no-effort ()
  "Return custom agenda command."
  `((alltodo
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-files (cashpw/org-agenda-files--update))
      (org-super-agenda-groups
       '((;; Automatically named "Log"
          :log t)
         (:discard
          (;; Don't bother listing PROJ items. They are used to group actionable TODOs.
           :todo "PROJ"))
         (:discard
          (:property "Effort"))
         ;; (:name "Without effort"
         ;;  :effort< "0:01")
         (:auto-category t)
         ))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-agenda-view--no-priority ()
  "Return custom agenda command."
  `((alltodo
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-files (cashpw/org-agenda-files--update))
      (org-super-agenda-groups
       '((;; Automatically named "Log"
          :log t)
         (:discard
          (;; Don't bother listing PROJ items. They are used to group actionable TODOs.
           :todo "PROJ"
           ;; Discard everything with a priority
           :pred (lambda (item)
                   (org-extras-get-priority (get-text-property 0 'org-hd-marker item)))))
         (:auto-category t)))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-agenda-view--not-scheduled ()
  "Return custom agenda command."
  `((alltodo
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-files (cashpw/org-agenda-files--update))
      (org-agenda-cmp-user-defined
       (lambda (a b)
         (let* ((a-priority-or-nil
                 (org-extras-get-priority
                  (get-text-property 0 'org-hd-marker a)))
                (a-priority
                 (string-to-number
                  (or a-priority-or-nil
                      (number-to-string (1+ org-priority-lowest)))))
                (b-priority-or-nil
                 (org-extras-get-priority
                  (get-text-property 0 'org-hd-marker b)))
                (b-priority
                 (string-to-number
                  (or b-priority-or-nil
                      (number-to-string (1+ org-priority-lowest))))))
           (if (> a-priority b-priority)
               1
             -1))))
      (org-agenda-sorting-strategy '((todo . (user-defined-up))))
      (org-super-agenda-groups
       '(( ;; Automatically named "Log"
          :log
          t)
         (:discard
          ( ;; Don't bother listing PROJ items. They are used to group actionable TODOs.
           :todo "PROJ"
           :tag ("unscheduled" "everyday")))
         (:discard (:scheduled t))
         (:auto-category t)))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/org-agenda-view--overdue--files ()
  (let ((org-not-done-keywords
         (with-temp-buffer
           (org-mode)
           org-not-done-keywords)))
    (cl-loop
     for file in (org-mem-all-files) unless (s-ends-with-p "archive" file) when
     (seq-find
      (lambda (entry)
        (and (member (org-mem-entry-todo-state entry) org-not-done-keywords)
             (or (and (org-mem-entry-scheduled entry)
                      (cashpw/time-past-p
                       (encode-time
                        (parse-time-string (org-mem-entry-scheduled entry)))))
                 (and (org-mem-entry-deadline entry)
                      (cashpw/time-past-p
                       (encode-time
                        (parse-time-string (org-mem-entry-deadline entry))))))))
      (org-mem-entries-in file))
     collect file)))

(defun cashpw/org-agenda-view--overdue ()
  "Return custom agenda command."
  `((agenda
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-span 1)
      (org-agenda-files (cashpw/org-agenda-view--overdue--files))
      (org-super-agenda-groups
       '((:discard
          (:scheduled
           future
           :deadline future
           :scheduled today
           :deadline today
           :file-path ,(cashpw/path-calendar)))
         (:auto-map
          (lambda (item)
            (-when-let* ((marker
                          (or (get-text-property 0 'org-marker item)
                              (get-text-property 0 'org-hd-marker)))
                         (default-priority "?")
                         (priority
                          (or (org-extras-get-priority marker)
                              default-priority)))
              (cond
               ((org-extras-scheduled-to-repeat-daily-p marker)
                "1 Repeats daily")
               ((org-extras-scheduled-to-repeat-weekly-p marker)
                "2 Repeats weekly")
               ((org-extras-scheduled-to-repeat-p marker)
                "3 Repeats")
               (t
                "4 Doesn't repeat")))))
         ( ;; Toss all other todos
          :discard
          (:todo t))))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(defun cashpw/cmp-random (a b)
  (if (> 0.5 (random))
      1
    -1))

(defun cashpw/org-agenda-view--gallery ()
  "Return custom agenda command."
  `((tags
     ""
     ((org-agenda-overriding-header "")
      (org-agenda-files (cashpw/org-files-with-tag "has_gallery" cashpw/path--notes-dir))
      (org-super-agenda-groups
       '((:name "Gallery"
          :todo nil)
         (:discard (:anything))))))))

(defun cashpw/org-agenda-view--gallery-all ()
  "Return custom agenda command."
  `((tags
     "gallery"
     ((org-agenda-overriding-header "")
      (org-agenda-files (cashpw/org-files-with-tag "has_gallery" cashpw/path--notes-dir))
      (org-super-agenda-groups
       '((:name "Gallery"
          :todo nil)
         (:discard (:anything))))))))

(defun cashpw/org-agenda-view--gallery-photos ()
  "Return custom agenda command."
  `((tags
     "photo"
     ((org-agenda-overriding-header "")
      (org-agenda-files (cashpw/org-files-with-tag "has_gallery" cashpw/path--notes-dir))
      (org-super-agenda-groups
       '((:name "Gallery"
          :todo nil)
         (:discard (:anything))))))))

(defun cashpw/org-agenda-view--gallery-current-buffer ()
  "Return custom agenda command."
  `((tags
     "gallery"
     ((org-agenda-overriding-header "")
      (org-agenda-files (list cashpw/gallery-file-name))
      (org-super-agenda-groups
       '((:name "Gallery"
          :todo nil)
         (:discard (:anything))))))))

(cashpw/org-agenda-custom-commands--maybe-update)

(cashpw/org-agenda-custom-commands--update)

(defun cashpw/org-agenda-buganizer-title ()
  "Overridden in my work config."
  "")

(after! org-agenda
  (define-key org-agenda-mode-map (kbd "@") #'cashpw/org-agenda-view-toggle-collapse)
  ;; Override
  (define-key org-agenda-mode-map (kbd ".") #'cashpw/org-agenda-reschedule-to-today)
  (define-key org-agenda-mode-map (kbd ">") #'cashpw/org-agenda-reschedule-to-next-occurrence-or-kill))

(defun cashpw/org-clock--agenda-with-archives ()
  "Return list of agenda files to use with clocktable."
  (append
   `(,cashpw/path--sleep-calendar)
   (cashpw/org-agenda-files 'personal t)
   (cashpw/org-agenda-files 'calendar t)
   (cashpw/org-agenda-files 'journal-this-year t)
   (cashpw/org-agenda-files 'people-private t)))

(defun cashpw/org-set-inprogress-to-todo ()
  "Set the current heading's todo status to TODO if it's currently INPROGRESS."
  (when (string= "INPROGRESS" (org-get-todo-state))
    (org-todo "TODO")))

(after! org
  (setq
   ;; Prevent org-clock from double-checking /every/ agenda file for dangling clock during `org-clock-in'.
   ;; See https://github.com/doomemacs/doomemacs/issues/5317
   org-clock-auto-clock-resolution nil)
  (add-to-list 'org-clock-out-hook #'cashpw/org-set-inprogress-to-todo))

(defun cashpw/org-clock-add-entry (clock-in-time clock-out-time)
  "Add single clock entry.

Clock in at CLOCK-IN-TIME and clock out at CLOCK-OUT-TIME."
  (interactive
   (list
    (org-read-date t t nil "Clock in")
    (org-read-date t t nil "Clock out")))
  (message "Clocking from %s to %s"
           (format-time-string "%F %T%Z" clock-in-time)
           (format-time-string "%F %T%Z" clock-out-time))
  (org-clock-in nil clock-in-time)
  (org-clock-out nil nil clock-out-time))

(defun cashpw/org-clock-duration-in-minutes (&optional arg)
  "Return total clocked minutes.

Based on `org-clock-display'.

By default, show the total time for the range defined in
`org-clock-display-default-range'.  With `\\[universal-argument]' \
prefix, show
the total time for today instead.

With `\\[universal-argument] \\[universal-argument]' prefix, \
use a custom range, entered at prompt.

With `\\[universal-argument] \ \\[universal-argument] \
\\[universal-argument]' prefix, display the total time in the
echo area.

Use `\\[org-clock-remove-overlays]' to remove the subtree times."
  (interactive "P")
  (org-clock-remove-overlays)
  (let* ((todayp (equal arg '(4)))
         (customp (member arg '((16) today yesterday
                                thisweek lastweek thismonth
                                lastmonth thisyear lastyear
                                untilnow interactive)))
         (prop (cond ((not arg) :org-clock-minutes-default)
                     (todayp :org-clock-minutes-today)
                     (customp :org-clock-minutes-custom)
                     (t :org-clock-minutes))))
    (cond ((not arg) (org-clock-sum-custom
                      nil org-clock-display-default-range prop))
          (todayp (org-clock-sum-today))
          (customp (org-clock-sum-custom nil arg))
          (t (org-clock-sum)))
    (unless (equal arg '(64))
      (save-excursion
        (goto-char (point-min))
        (let ((p nil))
          (while (or (and (equal (setq p (point)) (point-min))
                          (get-text-property p prop))
                     (setq p (next-single-property-change (point) prop)))
            (goto-char p)
            (let ((time (get-text-property p prop)))
              (when time (org-clock-put-overlay time)))))
        ;; Arrange to remove the overlays upon next change.
        (when org-remove-highlights-with-change
          (add-hook 'before-change-functions #'org-clock-remove-overlays
                    nil 'local))))
    org-clock-file-total-minutes))

(defun cashpw/org-clock-clocked-minutes (&optional key)
  "Return the total clocked minutes for KEY.

See `org-clock-special-range' for KEY."
  (interactive)
  (let ((key (or key
                 'interactive))
        (total-minutes
         0)
        (special-range (org-clock-special-range key))
        (org-clock-display-default-range
         'interactive))
    ;; Avoid a prompt from `org-clock-special-range' for every file by hard-coding a response.
    (cl-letf (((symbol-function 'org-clock-special-range)
               (lambda (_key &optional _time _as-strings _wstart _mstart)
                 special-range)))
      (dolist (file (cashpw/org-clock--agenda-with-archives))
        (with-current-buffer
            (find-file-noselect
             file)
          (setq
           total-minutes (+ total-minutes
                            (or (cashpw/org-clock-duration-in-minutes)
                                0)))))
      total-minutes)))

(defun cashpw/org-clock-clocked-minutes-for-time-range (start-time end-time)
  "`cashpw/org-clock-clocked-minutes' by specifying a DATE-TIME."
  (let* ((org-read-date-counter -1))
    (cl-letf (((symbol-function 'org-read-date)
               (lambda (&optional
                   _with-time
                   _to-time
                   _from-string
                   _prompt
                   _default-time
                   _default-input
                   _inactive)
                 (cl-incf org-read-date-counter)
                 (cond
                  ((= org-read-date-counter
                      0)
                   start-time)
                  ((= org-read-date-counter
                      1)
                   end-time)))))
      (cashpw/org-clock-clocked-minutes 'interactive))))

(defun cashpw/org-clock-non-clocked-minutes (&optional key)
  "Return the total non-clocked minutes for a selected date range."
  (interactive)
  (let ((key
         (or key
             'interactive))
        (minutes-in-day
         (* 24
            60)))
    (- minutes-in-day
       (cashpw/org-clock-clocked-minutes
        key))))

(defun cashpw/org-clock-clocked-minutes-today ()
  (interactive)
  (cashpw/org-clock-clocked-minutes 'today))

(defun cashpw/org-clock-non-clocked-minutes-today ()
  (interactive)
  (cashpw/org-clock-non-clocked-minutes 'today))

(defun cashpw/org-clock--clocktable--properties (time)
  "Return default clocktable properties."
  `(:scope cashpw/org-clock--agenda-with-archives
    :block ,(format-time-string
             "%Y-%m-%d"
             time)
    :narrow 200
    :fileskip0 t
    :filetitle t
    :maxlevel 3))

(defun cashpw/org-clock--clocktable-update-default-properties ()
  "Return default clocktable properties"
  (setq
   org-clock-clocktable-default-properties (cashpw/org-clock--clocktable--properties
                                            (current-time))))

(cashpw/org-clock--clocktable-update-default-properties)
;; Update the properties once per day as they include `:block' with today's date.
(let ((seconds-in-day (* 60 60 24)))
  (cancel-function-timers #'cashpw/org-clock--clocktable-update-default-properties)
  (run-at-time "00:00"
               seconds-in-day
               #'cashpw/org-clock--clocktable-update-default-properties))


(defun cashpw/org-clocktable-files-for-yyyy-mm-dd (yyyy-mm-dd)
  "Return list of files with clock entries for YYYY-MM-DD."
  (cashpw/rgrep (format "-l \"\\[%s\" %s/*.org*" yyyy-mm-dd cashpw/path--notes-dir)))

(defun cashpw/clocktable-by-category--properties (time)
  "Return clocktable-by-category properties."
  `(:files-fn
    (lambda ()
      (cashpw/org-clocktable-files-for-yyyy-mm-dd
       ,(format-time-string "%F" time)))
    :block ,(format-time-string "%F" time)
    :merge-duplicate-headlines t
    ;; :narrow 200
    ;; :fileskip0 t
    ;; :filetitle t
    ))

(defun cashpw/clocktable-by-category--update-default-properties ()
  "Return default clocktable-by-category properties"
  (setq clocktable-by-category--default-properties
        (cashpw/clocktable-by-category--properties (current-time))))

(cashpw/clocktable-by-category--update-default-properties)
;; Update the properties once per day as they include `:block' with today's date.
(let ((seconds-in-day (* 60 60 24)))
  (cancel-function-timers
   #'cashpw/clocktable-by-category--update-default-properties)
  (run-at-time
   "00:00"
   seconds-in-day
   #'cashpw/clocktable-by-category--update-default-properties))

(defun cashpw/clocktable-by-tag--properties ()
  "Return clocktable-by-tag properties."
  `(:files-fn cashpw/org-clock--agenda-with-archives
    :block ,(format-time-string "%Y-%m-%d")
    :merge-duplicate-headlines t
    ;; :narrow 200
    ;; :fileskip0 t
    ;; :filetitle t
    ))

(defun cashpw/clocktable-by-tag--update-default-properties ()
  "Return default clocktable-by-tag properties"
  (setq
   clocktable-by-tag--default-properties (cashpw/clocktable-by-tag--properties)))

(cashpw/clocktable-by-tag--update-default-properties)
;; Update the properties once per day as they include `:block' with today's date.
(let ((seconds-in-day (* 60 60 24)))
  (cancel-function-timers #'cashpw/clocktable-by-tag--update-default-properties)
  (run-at-time "00:00"
               seconds-in-day
               #'cashpw/clocktable-by-tag--update-default-properties))

(defvar cashpw/feedback-loop--timer nil)

(defcustom cashpw/feedback-loop--duration-minutes 30
  "Minutes before triggering feedback loop alert."
  :type 'number)

(defun cashpw/feedback-loop--should-alert-p ()
  "Return non-nil if we should show the alert."
  (not
   (or
    (member org-clock-heading '("Work"))
    (org-with-point-at org-clock-hd-marker
      (not (string= (f-filename buffer-file-name) "calendar-personal.org"))))))

(defun cashpw/feedback-loop-alert ()
  "Show feedback loop alert."
  (when (cashpw/feedback-loop--should-alert-p)
    (alert
     org-clock-heading
     :title (format "Feedback loop: %d minutes" cashpw/feedback-loop--duration-minutes)
     :persistent t
     :icon cashpw/icons-notifications)
    (cashpw/feedback-loop--schedule-alert)))

(defun cashpw/feedback-loop--schedule-alert ()
  "Schedule feedback loop alert."
  (setq cashpw/feedback-loop--timer
        (run-at-time
         (time-add
          (current-time)
          (* 60 cashpw/feedback-loop--duration-minutes))
         nil
         #'cashpw/feedback-loop-alert)))

(defun cashpw/feedback-loop--unschedule-alert ()
  "Schedule feedback loop alert."
  (cancel-timer cashpw/feedback-loop--timer)
  (setq cashpw/feedback-loop--timer nil))

(add-hook 'org-clock-in-hook #'cashpw/feedback-loop--schedule-alert)
(add-hook 'org-clock-out-hook #'cashpw/feedback-loop--unschedule-alert)

(after! org
  (setq ;; showeverything to make large files open faster
        org-startup-folded 'showeverything
        org-log-into-drawer t
        org-log-repeat t))

(after!
  org
  (defun cashpw/org--set-refile-targets ()
    "Refresh refile targets."
    (setq
     cashpw/org-refile-targets
     (seq-uniq
      (append
       (cashpw/org-agenda-files 'personal)
       (cashpw/notes-files-with-tags "project" "hastodo")
       (cashpw/notes-files-with-tags "decision" "hastodo")
       (cashpw/notes-files-with-tags "pet" "hastodo")
       (cashpw/notes-files-with-tags "people" "private" "hastodo")))
     org-refile-targets `((,cashpw/org-refile-targets :todo . "PROJ"))))
  (cashpw/org--set-refile-targets)

  (add-hook! 'org-after-refile-insert-hook
             #'cashpw/org--prompt-for-priority-when-missing
             #'cashpw/org--prompt-for-effort-when-missing
             #'cashpw/org--prompt-for-category-when-missing
             #'cashpw/org--prompt-for-schedule-when-missing
             #'cashpw/org--prompt-for-deadline-when-missing))

(after! org
  :config
  (setq
   org-priority-highest 0
   org-priority-default 2
   org-priority-lowest 4))

(after! org
  :config
  (setq
   org-todo-keywords '((sequence
                        ;; A task that needs doing & is ready to do
                        "TODO(t)"
                        ;; A task that is in progress
                        "INPROGRESS(i)"
                        ;; Something external is holding up this task
                        "BLOCKED(b)"
                        ;; This task is paused/on hold because of me
                        "HOLD(h)"
                        ;; A project, which usually contains other tasks
                        "PROJ(p)"
                        "|"
                        ;; Task successfully completed
                        "DONE(d)"
                        ;; Calendar event is declined
                        "DECLINED(`)"
                        ;; Calendar event is cancelled
                        "CANCELLED(~)"
                        ;; Task was moved
                        "MOVE(m)"
                        ;; Task was moved
                        "RESCHEDULE(_)"
                        ;; Task was cancelled, aborted or is no longer applicable
                        "KILL(k)")
                       (sequence
                        ;; A task that needs doing
                        "[ ](T)"
                        ;; Task is in progress
                        "[-](S)"
                        ;; Task is being held up or paused
                        "[?](W)"
                        "|"
                        ;; Task was completed
                        "[X](D)"))
   org-todo-keyword-faces '(("[-]"  . +org-todo-active)
                            ("INPROGRESS" . +org-todo-active)
                            ("[?]"  . +org-todo-onhold)
                            ("BLOCKED" . +org-todo-onhold)
                            ("HOLD" . +org-todo-onhold)
                            ("PROJ" . +org-todo-project))))

(defcustom cashpw/org-mode-on-inprogress--clock-in-paths
  nil
  "TODOs marked as INPROGRESS in these files will trigger a clock in.")

(defun cashpw/org-mode-on-inprogress--in-clock-in-file-p ()
  (member
   buffer-file-name
   cashpw/org-mode-on-inprogress--clock-in-paths))

(defun cashpw/org-mode-when-inprogress ()
  "Handle inprogress behavior."
  (when (string-equal (org-get-todo-state)
                      "INPROGRESS")
    (cond
     ;; ((cashpw/org-mode-on-inprogress--in-clock-in-file-p)
     ;;  (org-clock-in))
     ;; Trying this out for a while
     (t
      (org-clock-in)))))

(after! org
  :config
  (setq
   cashpw/org-mode-on-inprogress--clock-in-paths (append
                                                  (f-glob "*.org"
                                                          cashpw/path--notes-dir)

                                                  ;; DEBUGGING
                                                  ;; (let* ((org-roam-directory cashpw/path--notes-dir)
                                                  ;;        (org-roam-db-location (expand-file-name "org-roam.db"
                                                  ;;                                                org-roam-directory)))
                                                  ;;   (cashpw/notes-files-with-tag "person"))
                                                  )))

(after! org
  :config
  (setq
   org-log-done 'time))

(defgroup cashpw/org-mode-on-done nil
  "Handle Org-mode DONE headlines."
  :tag "Org-mode on done.")

(defcustom cashpw/org-mode-on-done--noop-hook nil
  "List of functions which return non-nil to indicate we should do nothing with the current heading."
  :group 'cashpw/org-mode-on-done
  :type 'hook)

(defcustom cashpw/org-mode-on-done--keep-hook nil
  "List of functions which return non-nil to indicate we should keep the current heading in the done state."
  :group 'cashpw/org-mode-on-done
  :type 'hook)

(defcustom cashpw/org-mode-on-done--delete-hook nil
  "List of functions which return non-nil to indicate we should delete the current heading."
  :group 'cashpw/org-mode-on-done
  :type 'hook)

(defcustom cashpw/org-mode-on-done--property-name "CASHPW_ON_DONE"
  "Property name to indicate how to handle DONE event."
  :group 'cashpw/org-mode-on-done
  :type 'string)

(defcustom cashpw/org-mode-on-done--property--noop "noop"
  "No-op."
  :group 'cashpw/org-mode-on-done
  :type 'string)

(defcustom cashpw/org-mode-on-done--property--keep "keep"
  "Keep"
  :group 'cashpw/org-mode-on-done
  :type 'string)

(defcustom cashpw/org-mode-on-done--property--delete "delete"
  "Delete."
  :group 'cashpw/org-mode-on-done
  :type 'string)

(defun cashpw/org-mode-on-done--property-value-equals-p (expected-value)
  "Return non-nil if the heading's `cashpw/org-mode-on-done--property-name' is EXPECTED-VALUE."
  (let ((actual-value (org-entry-get
                       (point)
                       cashpw/org-mode-on-done--property-name
                       'inherit)))
    (equal actual-value
           expected-value)))

(add-hook 'cashpw/org-mode-on-done--noop-hook
          (lambda ()
            (cashpw/org-mode-on-done--property-value-equals-p cashpw/org-mode-on-done--property--noop)))
(add-hook 'cashpw/org-mode-on-done--keep-hook
          (lambda ()
            (cashpw/org-mode-on-done--property-value-equals-p cashpw/org-mode-on-done--property--keep)))
(add-hook 'cashpw/org-mode-on-done--delete-hook
          (lambda ()
            (cashpw/org-mode-on-done--property-value-equals-p cashpw/org-mode-on-done--property--delete)))

(defcustom cashpw/org-mode-on-done--noop-file-paths '()
  "TODOs in these files will be noop by default."
  :group 'cashpw/org-mode-on-done
  :type '(repeat string))

(defcustom cashpw/org-mode-on-done--keep-file-paths
  (append
   `(,(s-lex-format "${cashpw/path--notes-dir}/journal-2024.org")
     ,(s-lex-format "${cashpw/path--notes-dir}/retrospective-2024.org")))
  "TODOs in these files will be keep by default."
  :group 'cashpw/org-mode-on-done
  :type '(repeat string))

(defcustom cashpw/org-mode-on-done--delete-file-paths '()
  "TODOs in these files will be delete by default."
  :group 'cashpw/org-mode-on-done
  :type '(repeat string))

(defun cashpw/org-mode-on-done--noop-file-p ()
  "Return non-nil if current file is a no-op file."
  (member buffer-file-name
          cashpw/org-mode-on-done--noop-file-paths))

(defun cashpw/org-mode-on-done--keep-file-p ()
  "Return non-nil if current file is a keep file."
  (member buffer-file-name
          cashpw/org-mode-on-done--keep-file-paths))

(defun cashpw/org-mode-on-done--delete-file-p ()
  "Return non-nil if current file is a delete file."
  (member buffer-file-name
          cashpw/org-mode-on-done--delete-file-paths))

(add-hook 'cashpw/org-mode-on-done--noop-hook
          'cashpw/org-mode-on-done--noop-file-p)
(add-hook 'cashpw/org-mode-on-done--keep-hook
          'cashpw/org-mode-on-done--keep-file-p)
(add-hook 'cashpw/org-mode-on-done--delete-hook
          'cashpw/org-mode-on-done--delete-file-p)

(defcustom cashpw/org-mode-on-done--noop-filetags
  '("noop_on_done")
  "Filetags for which we should noop on done."
  :group 'cashpw/org-mode-on-done
  :type '(repeat string))

(defcustom cashpw/org-mode-on-done--keep-filetags
  '("keep_on_done"
    "journal"
    "person"
    "pet"
    "decision"
    "project")
  "Filetags for which we should keep on done."
  :group 'cashpw/org-mode-on-done
  :type '(repeat string))

(defcustom cashpw/org-mode-on-done--delete-filetags
  '("delete_on_done")
  "Filetags for which we should delete on done."
  :group 'cashpw/org-mode-on-done
  :type '(repeat string))

(defun cashpw/org-mode-on-done--noop-filetag-p ()
  "Return non-nil if current file has a no-op filetag."
  (>
   (length
    (-intersection
     cashpw/org-mode-on-done--noop-filetags
     (org-extras-get-inbuffer-option "filetags")))
   0))

(defun cashpw/org-mode-on-done--keep-filetag-p ()
  "Return non-nil if current file has a keep filetag."
  (> (length
      (-intersection
       cashpw/org-mode-on-done--keep-filetags
       (org-extras-get-inbuffer-option "filetags")))
     0))

(defun cashpw/org-mode-on-done--delete-filetag-p ()
  "Return non-nil if current file has a delete filetag."
  (> (length
      (-intersection
       cashpw/org-mode-on-done--delete-filetags
       (org-extras-get-inbuffer-option "filetags")))
     0))

(add-hook 'cashpw/org-mode-on-done--noop-hook
          'cashpw/org-mode-on-done--noop-filetag-p)
(add-hook 'cashpw/org-mode-on-done--keep-hook
          'cashpw/org-mode-on-done--keep-filetag-p)
(add-hook 'cashpw/org-mode-on-done--delete-hook
          'cashpw/org-mode-on-done--delete-filetag-p)

(add-hook 'cashpw/org-mode-on-done--noop-hook
          #'org-get-repeat)

(add-hook 'cashpw/org-mode-on-done--delete-hook
          (lambda ()
            (string= org-state
                     "KILL")))
;; (add-hook
;;  'cashpw/org-mode-on-done--delete-hook
;;  'org-roam-file-p)

(defun cashpw/org-mode-on-done--is-noop ()
  "Return non-nil if we should noop the current entry."
  (-any 'funcall
        cashpw/org-mode-on-done--noop-hook))

(defun cashpw/org-mode-on-done--is-keep ()
  "Return non-nil if we should keep the current entry."
  (-any 'funcall
        cashpw/org-mode-on-done--keep-hook))

(defun cashpw/org-mode-on-done--is-delete ()
  "Return non-nil if we should delete the current entry."
  (-any 'funcall
        cashpw/org-mode-on-done--delete-hook))

(defun cashpw/org-mode-when-done ()
  "Archive entry when it is marked as done (as defined by `org-done-keywords')."
  (when (org-entry-is-done-p)
    (org-clock-out-if-current)
    (repeat-todo--reschedule (point))
    (cond
     ((cashpw/org-mode-on-done--is-noop)
      ;; (unless (org-get-repeat)
      ;; (org-schedule '(4)))
      (org-todo "TODO"))
     ((cashpw/org-mode-on-done--is-keep)
      nil)
     ((cashpw/org-mode-on-done--is-delete)
      (org-cut-subtree))
     (t
      (org-archive-subtree-default)))))

(after! org
  :config
  (add-hook! 'org-after-todo-state-change-hook
             ;; run-on-todo-state-change must be first
             'run-on-todo-state-change
             'cashpw/org-mode-when-inprogress
             'cashpw/org-mode-when-done))

(after! org
  :config
  (setq
   org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("Eh" . "export html")
     ("El" . "export latex")
     ;; ("i" . "ingredients")
     ("q" . "quote")
     ("s" . "src")
     ("sd" . "src dot :file TODO.png :cmdline -Kdot -Tpng")
     ("se" . "src emacs-lisp")
     ("sc" . "src C++")
     ("sp" . "src python :results output")
     ("ss" . "src sh :results output")
     ("v" . "verse"))))

(defun cashpw/org--prompt-for-priority-when-missing ()
  "Prompt for Effort if it's missing."
  (unless (org-extras-get-priority (point))
    (org-priority)))

(defun cashpw/org--prompt-for-effort-when-missing ()
  "Prompt user for an effort until they provide a non-empty value."
  (while
      (or
       (not (org-entry-get (point) "Effort"))
       ;; The default case (clicking <enter> without typing an effort) sets :EFFORT: to an empty string.
       (string= (org-entry-get (point) "Effort") ""))
    (org-set-effort)))

(defun cashpw/org--prompt-for-deadline-when-missing ()
  "Prompt user for an deadline if there isn't already one present."
  (unless (org-get-deadline-time (point))
    (condition-case nil
        (org-deadline nil)
      (quit))))

(defun cashpw/org--prompt-for-schedule-when-missing ()
  "Prompt user for an schedule if there isn't already one present."
  (unless (org-get-scheduled-time (point))
    (condition-case nil
        (org-schedule nil)
      (quit))))

(defun cashpw/org--prompt-for-category-when-missing ()
  "Prompt for category if it's missing."
  (let ((file-category (org-get-category (point-min)))
        (category (org-get-category (point))))
    (when (or (not category)
              (if (file-equal-p buffer-file-name cashpw/path--personal-todos)
                  (string= category file-category)
                ;; Allow default category in non-todo files.
                nil))
      (org-set-property "CATEGORY" (org-read-property-value "CATEGORY")))))

(after!
  org
  (setq
   cashpw/org-capture-templates--todo
   `(:group
     "Todo"
     :children
     (("Todo"
       :keys "t"
       :file (lambda () (cashpw/path-todos))
       :children
       (("Todo"
         :keys "t"
         :after-finalize
         (lambda ()
           (save-excursion
             (with-current-buffer (marker-buffer org-capture-last-stored-marker)
               (goto-char org-capture-last-stored-marker)
               (cashpw/org--prompt-for-priority-when-missing)
               (cashpw/org--prompt-for-effort-when-missing)
               (cashpw/org--prompt-for-category-when-missing)
               (cashpw/org--prompt-for-schedule-when-missing)
               (cashpw/org--prompt-for-deadline-when-missing))))
         :template ("* TODO %?" ":PROPERTIES:" ":Created: %U" ":END:"))
        ("Roam"
         :keys "r"
         :file (lambda () (concat cashpw/path--notes-dir "/todos-roam.org"))
         :template ("* TODO %?" ":PROPERTIES:" ":Created: %U" ":END:"))
        ("Email"
         :keys "e"
         :message-id
         (lambda ()
           (cashpw/email-get-message-id))
         :message-subject
         (lambda ()
           (s-trim (truncate-string-to-width (cashpw/email-get-subject) 30)))
         :message-from-to
         (lambda ()
           (let* ((to
                   (plist-get
                    (cashpw/email--address-properties (cashpw/email-get-to))
                    :email))
                  (from
                   (plist-get
                    (cashpw/email--address-properties (cashpw/email-get-from))
                    :email)))
             (if (and (member to cashpw/email-addresses--personal)
                      (member from cashpw/email-addresses--personal))
                 ""
               (format "%s → %s"
                       (if (member from cashpw/email-addresses--personal)
                           "me"
                         from)
                       (if (member to cashpw/email-addresses--personal)
                           "me"
                         to)))))
         :children
         (("Follow-up"
           :keys "f"
           :before-finalize
           (lambda ()
             (let ((tomorrow (time-add (current-time) (days-to-time 1))))
               (org-schedule nil tomorrow)))
           :immediate-finish t
           :template
           ("* TODO [#2] [[notmuch:%{message-id}][%{message-subject}]] (%{message-from-to}) :email:"
            ":PROPERTIES:"
            ":Created: %U"
            ":END:"))
          ("Todo today"
           :keys "e"
           :before-finalize (lambda () (org-schedule nil (current-time)))
           :immediate-finish t
           :template
           ("* TODO [#2] [[notmuch:%{message-id}][%{message-subject}]] (%{message-from-to}) :email:"
            ":PROPERTIES:"
            ":Created: %U"
            ":END:"))
          ("Todo (unscheduled)"
           :keys "E"
           :template
           ("* TODO [#2] [[notmuch:%{message-id}][%{message-subject}]] (%{message-from-to}) :email:"
            ":PROPERTIES:"
            ":Created: %U"
            ":END:"))))))))
   cashpw/org-capture-templates--flashcards
   `(:group
     "Flashcards"
     :file (lambda () (buffer-name))
     :olp ("Flashcards")
     :children
     (("Flashcards"
       :keys "f"
       :children
       (("Cloze"
         :keys "c"
         :template
         ("* %^{Name of card}"
          ":PROPERTIES:"
          ":CREATED: %U"
          ":END:"
          ""
          "%?"
          ""
          "** TODO Source")
         :prepare-finalize
         (lambda ()
           (goto-char (point-min))
           (org-fc-type-cloze-init 'deletion)))
        ("Photo"
         :keys "p"
         :template
         ("* %^{Name of card} :photo:"
          ":PROPERTIES:"
          ":CREATED: %U"
          ":END:"
          ""
          "%?"
          ""
          "** TODO Back"
          ""
          "TODO")
         :prepare-finalize
         (lambda ()
           (goto-char (point-min))
           (org-fc-type-double-init)))
        ("Double"
         :keys "d"
         :template
         ("* %^{Name of card}"
          ":PROPERTIES:"
          ":CREATED: %U"
          ":END:"
          ""
          "%?"
          ""
          "** TODO Back"
          ""
          "TODO"
          ""
          "** TODO Source")
         :prepare-finalize
         (lambda ()
           (goto-char (point-min))
           (org-fc-type-double-init)))
        ("Normal"
         :keys "n"
         :template
         ("* %^{Name of card}"
          ":PROPERTIES:"
          ":CREATED: %U"
          ":END:"
          ""
          "%?"
          ""
          "** TODO Back"
          ""
          "TODO"
          ""
          "** TODO Source")
         :prepare-finalize
         (lambda ()
           (goto-char (point-min))
           (org-fc-type-normal-init)))
        ("Vocab"
         :keys "v"
         :template
         ("* %^{Term}"
          ":PROPERTIES:"
          ":CREATED: %U"
          ":END:"
          ""
          "%?"
          ""
          "** TODO Source")
         :prepare-finalize
         (lambda ()
           (goto-char (point-min))
           (org-fc-type-vocab-init)))
        ("Text input"
         :keys "t"
         :template
         ("* %^{Name of card}"
          ":PROPERTIES:"
          ":CREATED: %U"
          ":END:"
          ""
          "%?"
          ""
          "** TODO Back"
          ""
          "TODO"
          ""
          "** TODO Source")
         :prepare-finalize
         (lambda ()
           (goto-char (point-min))
           (org-fc-type-text-input-init)))))))
   cashpw/org-capture-templates--journal
   `(:group
     "Journal"
     :file
     (lambda ()
       (format "%s/%s.org"
               cashpw/path--notes-dir
               (format-time-string "%F" (current-time))))
     :immediate-finish nil
     :children
     ("Journal"
      :keys "j"
      :children
      (("Thought record"
        :keys "t"
        :olp ("Journal" "Me" "Thought records")
        :record-situation (lambda () (read-string "Situation/Trigger: "))
        :record-pre-reflection-emotions
        (lambda ()
          (read-string
           "Pre-reflection emotions (e.g. Anxiety 40%, shame 80%): "))
        :record-automatic-thought (lambda () (read-string "Automatic thought: "))
        :record-alternative-thought (lambda () (read-string "Alternative thought: "))
        :record-distortions
        (lambda ()
          (let* ((distortions
                  '(("All-or-nothing thinking"
                     .
                     "id:161a1843-d228-4e46-afd0-f587356ef03a")
                    ("Mind reading" . "id:85eee943-87dc-4728-a03e-63a096ff8df5")
                    ("Fortune-telling"
                     .
                     "id:522e7027-ebac-4c06-8de5-ab338aec390a")
                    ("Labeling" . "id:f67f2787-f097-466f-a5c9-21cd8d6286ba")
                    ("Emotional reasoning"
                     .
                     "id:7b9b6518-05eb-4d48-9265-459847052d4d")
                    ("Should/Must" . "id:2a3b0da0-7d1f-4f36-a161-c9fb17d64dfa")
                    ("Personalization"
                     .
                     "id:582999ae-1911-4a97-8c46-0f2a811ecfa2")
                    ("Blaming" . "id:40d20f7b-3fe1-453c-968a-04d6a19d2c60")
                    ("Always being right"
                     .
                     "id:8865a566-0748-4c34-84b0-43d91b35ea3b")
                    ("Magnification/Minimization"
                     .
                     "id:bde41a0c-7893-47b9-b059-1c4165ad3e94")
                    ("Catastrophizing"
                     .
                     "id:f528e19d-6acd-44c8-b5fb-9eaaf6b16f6f")
                    ("Overgeneralizing"
                     .
                     "id:be56163c-6d7e-4908-a842-d5672dbe27c0")
                    ("Disqualifying the positive"
                     .
                     "id:7fb82fde-fdf7-4dd9-a717-0639d4de3524")
                    ("Filtering" . "id:0b509a6f-0fc2-41a3-8770-4c1f9a881a04")))
                 (selections
                  (completing-read-multiple "Distortions: " distortions nil t))
                 (link-list
                  (string-join (--map
                                (format "- %s
"
                                        (org-link-make-string
                                         (format "id:%s"
                                                 (alist-get it distortions
                                                            nil nil #'string=))
                                         it))
                                selections)
                               "")))
            link-list))
        :record-post-reflection-emotions
        (lambda ()
          (read-string
           "Post-reflection emotions (e.g. Anxiety 30%, shame 50%): "))
        :template
        "*** %U

**** Situation/Trigger

%{record-situation}

**** Pre-reflection emotions

%{record-pre-reflection-emotions}

**** Automatic thought

%{record-automatic-thought}

**** Distortions

%{record-distortions}
**** Alternative thought(s)

%{record-alternative-thought}

**** Post-reflection emotions

%{record-post-reflection-emotions}%?"))))
   cashpw/org-capture-templates--gallery
   `(:group
     "Gallery"
     :file (lambda () (buffer-name))
     :children
     (("Gallery"
       :keys "g"
       :children
       (("Photo"
         :keys "p"
         :olp ("Photos")
         :immediate-finish t
         :template
         (lambda ()
           (let* ((image-url
                   (read-string "Image URL: "
                                (gui-get-selection 'CLIPBOARD 'STRING)))
                  (title (read-string "Title: "))
                  (description (read-string "Description: "))
                  (description
                   (if (string-empty-p description)
                       description
                     (concat "\n" description)))
                  (artist-name
                   (when (member "person" (cashpw/org-get-filetags))
                     (org-get-title)))
                  (artist-tag
                   (when artist-name
                     (-->
                      (downcase artist-name)
                      (replace-regexp-in-string " " "_" it)
                      (replace-regexp-in-string "-" "_" it))))
                  (tags
                   (org-make-tag-string
                    (list
                     "photo"
                     artist-tag)))
                  (image-link
                   (progn
                     (cashpw/org-download-image--no-insert image-url)
                     (if (and (>= (string-to-number org-version) 9.3)
                              (eq org-download-method 'attach))
                         (format "[[attachment:%s]%s]"
                                 (org-link-escape
                                  (file-relative-name org-download-path-last-file
                                                      (org-attach-dir)))
                                 (if (string-empty-p title)
                                     ""
                                   (format "[%s]" title)))
                       (format "[[file:%s]%s]"
                               (org-link-escape
                                (funcall
                                 org-download-abbreviate-filename-function
                                 org-download-path-last-file))
                               (if (string-empty-p title)
                                   ""
                                 (format "[%s]" title)))))))
             (s-lex-format
              "* ${image-link} ${tags}
:PROPERTIES:
:CREATED: %U
:IMAGE_SOURCE: ${image-url}
:END:
${description}"))))))))
   org-capture-templates
   (doct
    `(("Website"
       :keys "w"
       :file ""
       :template "* %a :website:\n\n%U %?\n\n%:initial")
      ,cashpw/org-capture-templates--todo
      ,cashpw/org-capture-templates--flashcards
      ,cashpw/org-capture-templates--journal
      ,cashpw/org-capture-templates--gallery))))

(after! org
  (setq
   ;; Prefer IDs to filenames+headers when creating links.
   ;; Headers can change, filenames can change, the IDs won't change
   ;; and can move to follow the relevant content.
   org-id-link-to-org-use-id 'use-existing))

(use-package! deflink)

(defun cashpw/org-link--decompose (link-string)
  "Return plist with link and description of LINK-STRING."
  (string-match "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" link-string)
  `(:link ,(match-string 1 link-string)
    :description ,(match-string 2 link-string)))

(defun cashpw/org-link--get-link (link-string)
  "Extract link from LINK-STRING."
  (plist-get (cashpw/org-link--decompose link-string) :link))

(defun cashpw/org-link--get-description (link-string)
  "Extract description from LINK-STRING."
  (plist-get (cashpw/org-link--decompose link-string) :description))

(deflink "amazon"
         "https://amazon.com/dp/%s")

(deflink "google-doc"
         "https://docs.google.com/document/d/%s")

;; Prefer singular
(deflink "google-sheet"
         "https://docs.google.com/spreadsheets/d/%s")

(deflink "google-slide"
         "https://docs.google.com/presentation/d/%s")

(defun cashpw/org-roam-id-complete (&optional initial-input filter-fn sort-fn require-match prompt)
  "Read an `org-roam-node', returning its id.

All args are passed to `org-roam-node-read'."
  (concat
   "id:"
   (org-roam-node-id
    (org-roam-node-read
     initial-input filter-fn sort-fn require-match prompt))))

(org-link-set-parameters "id"
                         :complete #'cashpw/org-roam-id-complete)

(deflink
 "instagram"
 "https://instagram.com/%s"
 (lambda (link _) (concat "@" link)))

(deflink "isbn"
         "https://books.google.com/books?vid=ISBN/%s")

(deflink "reddit"
         "https://reddit.com/%s")

(deflink "stackoverflow"
         "https://stackoverflow.com/%s")

(deflink "twitter"
         "https://twitter.com")

(deflink "stock"
         "https://www.google.com/finance/quote/%s"
         (lambda (link _) (concat "$" (upcase link))))

;; (use-package! org-transclusion
;;   :after org
;;   :config
;;   (setq
;;    org-transclusion-exclude-elements '(property-drawer
;;                                        keyword)
;;    org-transclusion-extensions-loaded t
;;    org-transclusion-extensions '(org-transclusion-src-lines
;;                                  org-transclusion-font-lock
;;                                  org-transclusion-indent-mode))
;;   (add-hook! 'org-mode-hook 'org-transclusion-mode)
;;   ;; (set-face-attribute
;;   ;;  'org-transclusion-fringe nil
;;   ;;  :foreground "white"
;;   ;;  :background nil)
;;   (define-fringe-bitmap 'org-transclusion-fringe-bitmap
;;     [17 34 68 136 68 34 17]
;;     nil nil 'center)
;;   ;; Re-load extensions to activate `org-transclusion-indent-mode'.
;;   (org-transclusion-load-extensions-maybe t))

(setq
 org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}
\\newcommand{\\bigo}[1]{O(#1)}
\\newcommand{\\littleo}[1]{o(#1)}
\\newcommand{\\bigomega}[1]{\\Omega(#1)}
\\newcommand{\\bigtheta}[1]{\\Theta(#1)}
\\newcommand{\\determinant}[1]{\\operatorname{det}(#1)}"
 org-latex-default-packages-alist '(("" "amsmath" t
                                     ("lualatex" "xetex"))
                                    ("" "fontspec" t
                                     ("lualatex" "xetex"))
                                    ("AUTO" "inputenc" t
                                     ("pdflatex"))
                                    ("T1" "fontenc" t
                                     ("pdflatex"))
                                    ("" "graphicx" t)
                                    ("" "longtable" nil)
                                    ("" "wrapfig" nil)
                                    ("" "rotating" t)
                                    ("" "multirow" t)
                                    ("normalem" "ulem" t)
                                    ("" "amsmath" t
                                     ("pdflatex"))
                                    ("" "amssymb" t
                                     ("pdflatex"))
                                    ("" "capt-of" nil)
                                    ("" "hyperref" nil)))



(defcustom cashpw/latex-toggle-preview--buffers-with-preview-displayed-p
  '("a")
  "List of buffers with latex previews showing.")

(defun cashpw/latex-toggle-preview--current-buffer-has-preview-displayed-p ()
  (member
   (buffer-name (current-buffer))
   cashpw/latex-toggle-preview--buffers-with-preview-displayed-p))

(defun cashpw/latex-toggle-preview--show ()
  (interactive)
  (cl-pushnew (buffer-name (current-buffer))
           cashpw/latex-toggle-preview--buffers-with-preview-displayed-p)
  (org-latex-preview '(16)))

(defun cashpw/latex-toggle-preview--hide ()
  (interactive)
  (setq cashpw/latex-toggle-preview--buffers-with-preview-displayed-p
        (delete (buffer-name (current-buffer))
                cashpw/latex-toggle-preview--buffers-with-preview-displayed-p))
  (org-latex-preview '(64)))

(defun cashpw/latex-toggle-preview ()
  (interactive)
  (if (cashpw/latex-toggle-preview--current-buffer-has-preview-displayed-p)
      (cashpw/latex-toggle-preview--hide)
    (cashpw/latex-toggle-preview--show)))

(after!
  org
  :config
  (setq
   org-export-with-tags nil
   org-export-with-priority nil
   org-export-with-todo-keywords nil))

(setq
 org-html-checkbox-type 'html)

(defun org-pandoc-pan-to-pub (o)
  (intern
   (format ":org-pandoc-%s" o)))

(use-package! ox-pandoc
  :after (:all org)
  :config
  (setq
   org-pandoc-format-extensions '(pipe_tables+raw_html)
   org-pandoc-menu-entry
   '((?D "to docx and open." org-pandoc-export-to-docx-and-open)
     (?d "to docx." org-pandoc-export-to-docx)
     (?m "to markdown." org-pandoc-export-to-markdown)
     (?M "to markdown and open." org-pandoc-export-to-markdown-and-open)))
  (defconst org-pandoc-publish-options
    (mapcar
     'org-pandoc-pan-to-pub
     (append
      org-pandoc-valid-options
      org-pandoc-colon-separated-options
      org-pandoc-file-options)))
  (when (cashpw/machine-p 'work)
    (setq
     org-pandoc-options-for-docx
     '((lua-filter . "/usr/local/google/home/cashweaver/third_party/google_docs_pandoc/pandoc/GenericDocFilter.lua")
       (reference-doc . "/usr/local/google/home/cashweaver/third_party/google_docs_pandoc/pandoc/CashWeaverGenericDocTemplate.docx")
       ;;(reference-doc . "/usr/local/google/home/cashweaver/third_party/google_docs_pandoc/pandoc/GenericDocTemplate.docx")
       (highlight-style . "/usr/local/google/home/cashweaver/third_party/google_docs_pandoc/pandoc/Kodify.theme")))
    (add-hook! 'org-pandoc-after-processing-markdown-hook
               'cashpw/remove-yaml-header)
    ))

(defun cashpw/remove-yaml-header ()
  "Remove the 'front matter'/YAML header content from the current buffer."
  (goto-char (point-min))
  (replace-regexp
   "---\\(.\\|\n\\)*?---"
   "")
  (goto-char (point-min))
  (delete-blank-lines)
  (delete-blank-lines))

(defun cashpw/remove-toml-header ()
  "Remove the 'front matter'/TOML header content from the current buffer."
  (goto-char (point-min))
  (replace-regexp
   "\\+\\+\\+\\(.\\|\n\\)*?\\+\\+\\+"
   "")
  (goto-char (point-min))
  (delete-blank-lines)
  (delete-blank-lines))

(defun cashpw/get-toml-header ()
  "Return the 'front matter'/TOML header content from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp
     "\\+\\+\\+\n\\(\\(.\\|\n\\)*?\\)\n\\+\\+\\+")
    (match-string 1)))

(defun cashpw/remove-yaml-front-matter-current-buffer ()
  (interactive)
  (cashpw/remove-yaml-header))

(defun cashpw/remove-toml-front-matter-current-buffer ()
  (interactive)
  (cashpw/remove-toml-header))


(defun cashpw/get-title-toml-front-matter ()
  (interactive)
  (let* ((toml-string
          (cashpw/get-toml-header))
         (toml-lines
          (split-string
           toml-string
           "\n"))
         (title
          (replace-regexp-in-string
           "title = \"\\(.*\\)\""
           "\\1"
           (nth 0 (seq-filter
                   (lambda (line)
                     (s-starts-with?
                      "title"
                      line))
                   toml-lines)))))
    title))

(defun cashpw/replace-toml-front-matter-with-md-heading ()
  (interactive)
  (let ((title (cashpw/get-title-toml-front-matter)))
    (cashpw/remove-toml-header)
    (save-excursion
      (goto-char (point-min))
      (insert (format
               "# %s\n\n"
               title)))))

(defun org-pandoc-publish-to (format plist filename pub-dir &optional remove-yaml-header)
  "Publish using Pandoc (https://github.com/kawabata/ox-pandoc/issues/18#issuecomment-262979338)."
  (setq
   org-pandoc-format format
   org-pandoc-option-table (make-hash-table))
  (let ((tempfile
         (org-publish-org-to
          'pandoc filename (concat (make-temp-name ".tmp") ".org") plist pub-dir))
        (outfile (format "%s.%s"
                         (concat
                          pub-dir
                          (file-name-sans-extension (file-name-nondirectory filename)))
                         (assoc-default format org-pandoc-extensions))))
    (org-pandoc-put-options (org-pandoc-plist-to-alist plist))
    (let ((process
           (org-pandoc-run tempfile outfile format 'org-pandoc-sentinel
                           org-pandoc-option-table))
          (local-hook-symbol
           (intern (format "org-pandoc-after-processing-%s-hook" format))))
      (process-put process 'files (list tempfile))
      (process-put process 'output-file outfile)
      (process-put process 'local-hook-symbol local-hook-symbol))))

(defun org-pandoc-pub-to-pan (o)
  (intern
   (substring (symbol-name o) 12)))

(defun org-pandoc-plist-to-alist (plist)
  (let ((alist '()))
    (while plist
      (let ((p (car plist))
            (v (cadr plist)))
        (when (member p org-pandoc-publish-options)
          (add-to-list 'alist (cons (org-pandoc-pub-to-pan p) v))))
      (setq plist (cddr plist)))
    alist))

(defun org-pandoc-publish-to-md (plist filename pub-dir)
  "Publish to markdown using Pandoc."
  ;;(org-pandoc-publish-to 'markdown plist filename pub-dir t))
  (org-pandoc-publish-to 'markdown plist filename pub-dir t))

(defun org-pandoc-publish-to-plain (plist filename pub-dir)
  "Publish to markdown using Pandoc."
  (org-pandoc-publish-to 'plain plist filename pub-dir))

(defun cashpw/kill-all-markdown-buffers ()
  "Kill all other org-roam buffers except current."
  (let ((buffers-to-kill
         (--filter (s-ends-with-p ".md" (buffer-name it)) (buffer-list))))
    (mapc 'kill-buffer buffers-to-kill)))
;; (cashpw/kill-all-markdown-buffers)
;; (buffer-list)

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value) (file-notify-rm-watch key)) file-notify-descriptors))

(defun cashpw/org-hugo--remove-missing-relrefs (directory)
  "Remove missing relref links from hugo DIRECTORY."
  (let ((post-paths (directory-files (format "%s/content/posts" directory) t "\\.md$"))
        (missing-relrefs
         (mapcar
          (lambda (result)
            (replace-regexp-in-string
             ".*REF_NOT_FOUND: Ref \"\\([^\"]*\\).*" "\\1" result))
          (split-string (shell-command-to-string
                         (format "cd %s; hugo | grep REF_NOT_FOUND" directory))
                        "\n" t))))
    (dolist (post-path post-paths)
      (message "removing missing relrefs from %s" post-path)
      (with-current-buffer (find-file-noselect post-path)
        (dolist (missing-relref missing-relrefs)
          (cashpw/replace-regexp-in-buffer
            (format "relref \"%s\"" missing-relref)
           "REPLACED"))
        (save-buffer)))))

(defun cashpw/org-hugo-export-directory (directory &optional files-to-ignore)
  "Export all hugo files in DIRECTORY.

Optionally skip FILES-TO-IGNORE."
  (interactive)
  (let* ((org-roam-directory directory)
         (files-to-export
          (seq-difference
           (cashpw/org-files-with-tag
            "public" org-roam-directory)
           files-to-ignore))
         (org-id-extra-files (org-roam-list-files))
         (file-index 0)
         (log-file-path "/tmp/hugo-export.log")
         (count-files-to-export (length files-to-export))
         ;; Last updated: 2023-12-05
         (recent-run-file-count 1467)
         ;; Last updated: 2023-12-05
         (recent-run-seconds 7791)
         (seconds-per-file (/ recent-run-seconds recent-run-file-count)))
    (cl-flet ((remaining-minutes
                (file-number)
                (let ((files-left (- count-files-to-export file-number)))
                  (/ (* seconds-per-file files-left) 60))))
      (let*
          ((run-time-estimate
            (org-duration-from-minutes (remaining-minutes 0)))
           (should-run
            (y-or-n-p
             (s-lex-format
              "Found ${count-files-to-export} public notes in ${directory}. Export estimate: ${run-time-estimate}."))))
        (when should-run
          (let* ((progress-reporter
                  (make-progress-reporter "Exporting roam notes"
                                          0 count-files-to-export))
                 (start-time (current-time)))
            (org-roam-db-sync)
            ;; Speed up the export
            ;; (cashpw/eglot-pause)
            (memoize 'citeproc-hash-itemgetter-from-any)
            (advice-add 'org-id-find :override 'org-roam-id-find)
            (memoize 'org-roam-id-find)
            ;; (memoize 'org-roam-node-id)
            ;; (memoize 'org-roam-node-file)
            ;; (global-flycheck-mode -1)

            (-each
                files-to-export
              (lambda (file-path)
                (let ((file-export-start-time (current-time))
                      (roam-file-buffer (find-file-noselect file-path))
                      (start-time (current-time)))
                  (shut-up
                    (when (= 0 (% file-index 10))
                      ;; (message "[cashpw] fix cannot redirect stderr too many open files")
                      ;; Prevent `Error: (file-error "Cannot redirect stderr" "Too many open files" "/dev/null")'
                      (cashpw/kill-all-markdown-buffers)
                      (file-notify-rm-all-watches))
                    ;; (message "cashpw/org-hugo-export-all (%d/%d) exporting [%s]"
                    ;;          (1+ file-index)
                    ;;          count-files-to-export
                    ;;          file-path)
                    (with-current-buffer roam-file-buffer
                      (remove-hook 'before-save-hook 'org-encrypt-entries t)
                      (org-hugo-export-to-md))
                    (kill-buffer roam-file-buffer)
                    ;; (message "cashpw/org-hugo-export-all (%d/%d) exported [%s] %.06f"
                    ;;          (1+ file-index)
                    ;;          count-files-to-export
                    ;;          file-path
                    ;;          (float-time
                    ;;           (time-since
                    ;;            file-export-start-time)))
                    (let* ((file-number (1+ file-index))
                           (time-string (format-time-string "%F %H:%M:%S"))
                           (export-duration-in-seconds (time-since start-time)))
                      (append-to-file
                       (s-lex-format
                        "${time-string}: ${file-number}/${count-files-to-export} ${file-path} (duration: ${export-duration-in-seconds})\n")
                       nil log-file-path))))
                (progress-reporter-update progress-reporter
                                          file-index
                                          (concat
                                           "Remaining time (estimate): "
                                           (org-duration-from-minutes
                                            (remaining-minutes file-index))))
                (setq file-index (1+ file-index))))
            (progress-reporter-done progress-reporter)

            ;; Remove speed-up changes
            ;; (cashpw/eglot-unpause)
            (advice-remove 'org-id-find 'org-roam-id-find)
            (memoize-restore 'org-roam-id-find)
            ;; (memoize-restore 'org-roam-node-id)
            ;; (memoize-restore 'org-roam-node-file)
            (memoize-restore 'citeproc-hash-itemgetter-from-any)

            (message "cashpw/org-hugo-export-all %.06f"
                     (float-time (time-since start-time)))))))))

(defun cashpw/org-hugo--tag-processing-fn-roam-tags (tag-list info)
  "Add tags from filetags to tag-list for org-roam to ox-hugo compatibility.

Reference: https://sidhartharya.me/exporting-org-roam-notes-to-hugo/#goal

See `org-hugo-tag-processing-functions'."
  (if (org-roam-file-p)
      (append
       tag-list
       (-map #'downcase (org-extras-filetags-in-buffer (current-buffer))))
    tag-list))

(after!
  ox-hugo
  (setq
   org-hugo-citations-plist '(:bibliography-section-heading "")
   org-hugo-allow-spaces-in-tags nil)
  (add-to-list
   'org-hugo-tag-processing-functions
   'cashpw/org-hugo--tag-processing-fn-roam-tags)

  ;; Override to allow for empty `bibliography-section-heading'.
  (defun org-hugo--org-cite-export-bibliography (orig-fun &rest args)
    "Insert a heading before the exported bibliography.

ORIG-FUN is the original function `org-cite-export-bibliography'
that this function is designed to advice using `:around'.  ARGS
are the arguments of the ORIG-FUN."
    (let ((bib (apply orig-fun args)))
      (when (org-string-nw-p bib)
        ;; Auto-inject Bibliography heading.
        (let ((info (nth 2 args)) ;(org-cite-export-bibliography KEYWORD _ INFO)
              (bib-heading
               (org-string-nw-p
                (plist-get
                 org-hugo-citations-plist
                 :bibliography-section-heading))))
          (if bib-heading
              (let* ((bib-heading
                      (org-blackfriday--translate nil info bib-heading))
                     (loffset
                      (string-to-number
                       (or (org-entry-get
                            nil "EXPORT_HUGO_LEVEL_OFFSET"
                            :inherit)
                           (plist-get info :hugo-level-offset))))
                     (level-mark (make-string (+ loffset 1) ?#)))
                (format "%s %s\n\n%s" level-mark bib-heading bib))
            (format "%s" bib))))))


  ;; Speed up exporting files
  ;; (advice-add 'org-id-find :override 'org-roam-id-find)
  ;; (memoize 'org-roam-node-id)
  ;; (memoize 'org-roam-node-file)
  )

(defun cashpw/org-hugo-export-all ()
  "Export all hugo notes files.."
  (interactive)
  (cashpw/org-hugo-export-directory cashpw/path--notes-dir))

;; (defun cashpw/org-hugo-export-to-md--maybe
;;     (&optional async subtreep visible-only)
;;   "Conditional export."
;;   (when (seq-contains (org-extras-filetags-in-buffer (current-buffer)) "public")
;;     (org-hugo-export-to-md async subtreep visible-only)))

;; (after!
;;   org
;;   (setq org-publish-project-alist
;;         '(("notes.cashpw.com"
;;            :base-directory "~/proj/notes"
;;            :base-extension "org"
;;            :publishing-directory "~/proj/notes.cashpw.com/content/posts/"
;;            :publishing-function cashpw/org-hugo-export-to-md--maybe
;;            ;; :section-numbers t
;;            :with-toc nil))))

(defcustom
  cashpw/smart-to-ascii
  '(("\x201C" . "\"")
    ("\x201D" . "\"")
    ("\x2018" . "'")
    ("\x2019" . "'"))
  "Mapping from known 'smart' quotes/etc to their ascii equivalent.")

(defun cashpw/replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes.

Reference: https://superuser.com/a/604264"
  (interactive "r")
  (format-replace-strings
   cashpw/smart-to-ascii
   nil
   beg
   end))

(defun cashpw/replace-smart-quotes-in-buffer ()
  "Replace 'smart quotes' in current buffer."
  (interactive)
  (cashpw/replace-smart-quotes
   (point-min)
   (point-max)))

(after! citar
  :when (modulep! :completion vertico)
  :config
  (setq
   citar-bibliography cashpw/bibliographies
   citar-symbols `((file ,(nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-green :v-adjust -0.1) . " ")
                   (note ,(nerd-icons-faicon "nf-fa-sticky_note_o" :face 'nerd-icons-blue :v-adjust -0.3) . " ")
                   (link ,(nerd-icons-octicon "nf-oct-link" :face 'nerd-icons-orange :v-adjust 0.01) . " "))
   citar-symbol-separator "  "
   citar-notes-paths `(,cashpw/path--notes-dir))

  (defun cashpw/citar-full-names (names)
    "Transform names like LastName, FirstName to FirstName LastName.

Reference: https://gist.github.com/bdarcus/a41ffd7070b849e09dfdd34511d1665d"
    (when (stringp names)
      (mapconcat
       (lambda (name)
         (if (eq 1 (length name))
             (split-string name " ")
           (let ((split-name (split-string name ", ")))
             (cl-concatenate 'string (nth 1 split-name) " " (nth 0 split-name)))))
       (split-string names " and ") ", ")))
  (setq citar-display-transform-functions
        '((("author" "editor") . cashpw/citar-full-names))))

;; (use-package! citar-org)

(use-package! oc
  :after org citar
  :config
  (setq
   org-cite-global-bibliography cashpw/bibliographies
   org-cite-insert-processor 'citar
   org-cite-follow-processor 'citar
   org-cite-activate-processor 'citar))

(after! org
  ;; Keep in alphabetical order.
  (map!
   :map org-mode-map
   ;; Mosh converts S-<up>, etc, into M-[
   "M-[ a" #'org-shiftup
   "M-[ b" #'org-shiftdown
   "M-[ c" #'org-shiftright
   "M-[ d" #'org-shiftleft)

  (map!
   :map eww-mode-map
   :n "yy" #'evil-yank

   (:localleader
    :n "@" #'cashpw/zotra-add-entry-from-eww
    :desc "jump to hedaing" "." #'+eww/jump-to-heading-on-page))

  (map!
   :map org-mode-map
   :localleader
   :nv "@" nil
   (:prefix ("@" . "Citation")
    :n "a" #'zotra-add-entry
    :n "b" #'cashpw/google-books-select-isbn-and-add-citation
    :n "@" #'org-cite-insert)
   (:prefix ("b")
    :n "RET" #'org-table-copy-down)
   (:prefix ("c")
    :n "E" #'org-clock-modify-effort-estimate
    :n "e" #'org-set-effort
    :n "a" #'cashpw/org-clock-add-entry
    :n "p" #'omc-make-new-parallel-clock
    :n "s" #'omc-set-active-clock
    (:prefix ("R" . "Report")
     :n "c" #'clocktable-by-category-report
     :n "C" #'org-clock-report
     :n "t" #'clocktable-by-tag-report))
   (:prefix ("d")
            (:prefix ("h" . "insert heading")
             :n "t" (cmd! (org-extras-insert-heading-for-today
                           ;; top
                           nil
                           ;; time-in-heading
                           nil
                           ;; include-all-tags
                           nil))
             :n "T" (cmd! (org-extras-insert-heading-for-today nil t t)))
            (:prefix ("S")
                     (:prefix ("h" . "hour")
                              (:prefix ("0" . "0?:??")
                               :desc "00:00" :n "0" (cmd! (cashpw/org-schedule-today-from-to "00:00" "00:45"))
                               :desc "01:00" :n "1" (cmd! (cashpw/org-schedule-today-from-to "01:00" "01:45"))
                               :desc "02:00" :n "2" (cmd! (cashpw/org-schedule-today-from-to "02:00" "02:45"))
                               :desc "03:00" :n "3" (cmd! (cashpw/org-schedule-today-from-to "03:00" "03:45"))
                               :desc "04:00" :n "4" (cmd! (cashpw/org-schedule-today-from-to "04:00" "04:45"))
                               :desc "05:00" :n "5" (cmd! (cashpw/org-schedule-today-from-to "05:00" "05:45"))
                               :desc "06:00" :n "6" (cmd! (cashpw/org-schedule-today-from-to "06:00" "06:45"))
                               :desc "07:00" :n "7" (cmd! (cashpw/org-schedule-today-from-to "07:00" "07:45"))
                               :desc "08:00" :n "8" (cmd! (cashpw/org-schedule-today-from-to "08:00" "08:45"))
                               :desc "09:00" :n "9" (cmd! (cashpw/org-schedule-today-from-to "09:00" "09:45")))
                              (:prefix ("1" . "1?:??")
                               :desc "01:00" :n "RET" (cmd! (cashpw/org-schedule-today-from-to "01:00" "01:45"))
                               :desc "10:00" :n "0" (cmd! (cashpw/org-schedule-today-from-to "10:00" "10:45"))
                               :desc "11:00" :n "1" (cmd! (cashpw/org-schedule-today-from-to "11:00" "11:45"))
                               :desc "12:00" :n "2" (cmd! (cashpw/org-schedule-today-from-to "12:00" "12:45"))
                               :desc "13:00" :n "3" (cmd! (cashpw/org-schedule-today-from-to "13:00" "13:45"))
                               :desc "14:00" :n "4" (cmd! (cashpw/org-schedule-today-from-to "14:00" "14:45"))
                               :desc "15:00" :n "5" (cmd! (cashpw/org-schedule-today-from-to "15:00" "15:45"))
                               :desc "16:00" :n "6" (cmd! (cashpw/org-schedule-today-from-to "16:00" "16:45"))
                               :desc "17:00" :n "7" (cmd! (cashpw/org-schedule-today-from-to "17:00" "17:45"))
                               :desc "18:00" :n "8" (cmd! (cashpw/org-schedule-today-from-to "18:00" "18:45"))
                               :desc "19:00" :n "9" (cmd! (cashpw/org-schedule-today-from-to "19:00" "19:45")))
                              (:prefix ("2" . "2?:??")
                               :desc "20:00" :n "0" (cmd! (cashpw/org-schedule-today-from-to "20:00" "20:45"))
                               :desc "21:00" :n "3" (cmd! (cashpw/org-schedule-today-from-to "21:00" "21:45"))
                               :desc "22:00" :n "2" (cmd! (cashpw/org-schedule-today-from-to "22:00" "22:45"))
                               :desc "23:00" :n "3" (cmd! (cashpw/org-schedule-today-from-to "23:00" "23:45")))
                              :desc "03:00" :n "3" (cmd! (cashpw/org-schedule-today-from-to "03:00" "03:45"))
                              :desc "04:00" :n "4" (cmd! (cashpw/org-schedule-today-from-to "04:00" "04:45"))
                              :desc "05:00" :n "5" (cmd! (cashpw/org-schedule-today-from-to "05:00" "05:45"))
                              :desc "06:00" :n "6" (cmd! (cashpw/org-schedule-today-from-to "06:00" "06:45"))
                              :desc "07:00" :n "7" (cmd! (cashpw/org-schedule-today-from-to "07:00" "07:45"))
                              :desc "08:00" :n "8" (cmd! (cashpw/org-schedule-today-from-to "08:00" "08:45"))
                              :desc "09:00" :n "9" (cmd! (cashpw/org-schedule-today-from-to "09:00" "09:45")))))

   (:prefix ("D" . "Download")
    :n "R" #'org-download-rename-last-file
    :n "c" #'org-download-clipboard
    :n "d" #'org-download-delete
    :n "e" #'org-download-edit
    :n "i" #'org-download-image
    :n "r" #'org-download-rename-at-point
    :n "s" #'org-download-screenshot
    :n "y" #'org-download-yank)

   (:prefix ("E" . "Emphasis")
    :n "*" (cmd! (org-emphasize ?*))
    :n "/" (cmd! (org-emphasize ?/))
    :n "+" (cmd! (org-emphasize ?+))
    :n "~" (cmd! (org-emphasize ?~))
    :n "_" (cmd! (org-emphasize ?_))
    :n "=" (cmd! (org-emphasize ?=)))

   (:prefix ("l")
            (:prefix ("T" . "transclusion")
             :n "a" #'org-transclusion-add
             :n "A" #'org-transclusion-add-all
             :n "i" #'org-transclusion-make-from-link
             :n "l" #'org-transclusion-live-sync-start
             :n "r" #'org-transclusion-remove
             :n "R" #'org-transclusion-remove-all))

   (:prefix ("L" . "Latex")
    :desc "toggle preview" :n "t" #'cashpw/latex-toggle-preview)

   (:prefix ("m" . "org-roam")
    :desc "Open ref" :n "O" #'cashpw/org-roam-open-ref
    (:prefix ("o")
     :n "r" #'cashpw/org-roam-add-citation-as-ref)
    (:prefix ("l" . "link")
     :n "q" #'cashpw/org-roam-insert-tag-link)
    :desc "Publish all" :n "p" #'cashpw/org-hugo-export-all)
   :n "p" #'org-priority
   (:prefix ("S" . "Structure")
    :n "i" #'org-insert-structure-template)))

(after! org-noter
  (map!
   :map pdf-view-mode-map
   :localleader

   :n "n" #'org-noter-insert-note
   :n "N" #'org-noter-insert-precise-note
   :desc "Quote (precise)" :n "Q" #'cashpw/org-noter-insert-selected-text-inside-note-content))

(unless (cashpw/machine-p 'personal-phone)
(use-package! pdf-tools
  :config
  (pdf-tools-install)))

(use-package! protobuf-mode)

(use-package! toml)

(defcustom cashpw/project-path-fns
  '()
  "List of functions which return a list of project paths.")

(defun cashpw/get-proj-dir-paths (&optional projects-to-exclude proj-dir-path)
  "Return a list of absolute paths to project directories.

Exclude project names listed in PROJECTS-TO-EXCLUDE."
  (let* ((projects-to-exclude
          (or
           projects-to-exclude
           '()))
         (proj-dir-path
          (or
           proj-dir-path
           (format
            "%s/proj"
            cashpw/path--home-dir)))
         (proj-names
          (cl-remove-if
           (lambda (file-name)
             (or
              (member file-name projects-to-exclude)
              (string= ".." file-name)
              (string= "." file-name)
              (not (f-dir?
                    (expand-file-name
                     file-name
                     proj-dir-path)))))
           (directory-files
            proj-dir-path)))
         (absolute-proj-dir-paths
          (mapcar
           (lambda (proj-name)
             (format "%s/%s"
                     proj-dir-path
                     proj-name))
           proj-names)))
    absolute-proj-dir-paths))

(add-to-list
 'cashpw/project-path-fns
 'cashpw/get-proj-dir-paths)

(defvar cashpw/gdrive-mount-dir-path
  "/mnt/cashbweaver-gdrive"
  "Absoltue path to personal Google Drive mount.")

(defvar cashpw/gdrive-notes-dir-path
  (format "%s/notes"
          cashpw/gdrive-mount-dir-path)
  "Absolute path to personal notes directory in Google Drive.")

;; (add-to-list
;;  'cashpw/project-paths
;;  cashpw/gdrive-notes-dir-path)

(defun cashpw/get-flattened-known-project-paths ()
  "Return a list of all known project paths"
  (let* ((nested-paths
          (cl-loop for fn in cashpw/project-path-fns collect (funcall fn)))
         (paths
          (mapcar
           (lambda (path)
             (cashpw/maybe-add-trailing-forward-slash path))
           (flatten-tree nested-paths))))
    paths))

(defun cashpw/projectile-refresh-known-paths ()
  "Refresh the paths which projectile knows about."
  (interactive)
  (projectile-clear-known-projects)
  (setq projectile-known-projects (cashpw/get-flattened-known-project-paths)))

(unless (cashpw/machine-p 'work-cloudtop)
  (after! projectile (cashpw/projectile-refresh-known-paths)))

(use-package! pomm
  :custom
  (pomm-work-period 50)
  (pomm-short-break-period 10)
  (pomm-short-break-period 10)
  :commands
  (pomm pomm-third-time))

(use-package! toml)

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.

Reference:https://stackoverflow.com/q/23622296"
  `(let ((start-time (current-time)))
     ,@body
     (message "%.06f"
              (float-time (time-since start-time)))))

;; Reference: https://notes.alexkehayias.com/exporting-org-mode-documents-with-many-org-id-links-is-slow/
(after! ox
  ;; Org export is very slow when processing org-id links. Override it
  ;; to skip opening the file and loading all modes.
  (defun org-export--collect-tree-properties (data info)
    "Extract tree properties from parse tree.

    DATA is the parse tree from which information is retrieved.  INFO
    is a list holding export options.

    Following tree properties are set or updated:

    `:headline-offset' Offset between true level of headlines and
                       local level.  An offset of -1 means a headline
                       of level 2 should be considered as a level
                       1 headline in the context.

    `:headline-numbering' Alist of all headlines as key and the
                          associated numbering as value.

    `:id-alist' Alist of all ID references as key and associated file
                as value.

    Return updated plist."
    ;; Install the parse tree in the communication channel.
    (setq info (plist-put info :parse-tree data))
    ;; Compute `:headline-offset' in order to be able to use
    ;; `org-export-get-relative-level'.
    (setq info
          (plist-put info
                     :headline-offset
                     (- 1 (org-export--get-min-level data info))))
    ;; From now on, properties order doesn't matter: get the rest of the
    ;; tree properties.
    (org-combine-plists
     info
     (list :headline-numbering (org-export--collect-headline-numbering data info)
           :id-alist
           (org-element-map data 'link
             (lambda (l)
               (and (string= (org-element-property :type l) "id")
                    (let* ((id (org-element-property :path l))
                           (file (org-id-find-id-file id)))
                      (and file (cons id (file-relative-name file)))))))))))

;; Reference: https://github.com/politza/pdf-tools/issues/651

(defun pdf-view-scroll-up-or-next-column (&optional arg)
  (interactive "P")
  (message (prin1-to-string (image-mode-window-get 'hscroll)))
  (let ((vscroll (image-mode-window-get 'vscroll)))
    (unless (/= vscroll (image-scroll-up arg))
      (if (= 0 (image-mode-window-get 'hscroll))
          (progn
            (image-bob)
            (image-eol nil))
        (pdf-view-next-page)
        (image-bob)))))

(defun pdf-view-scroll-down-or-previous-column (&optional arg)
  (interactive "P")
  (if (/= 0 (image-mode-window-get 'vscroll))
      (image-scroll-down arg)
    (if (/= 0 (image-mode-window-get 'hscroll))
        (progn
          (image-eob)
          (image-bol nil))
      (pdf-view-previous-page)
      (image-eob))))

(defun sow-two-column-pdf-view (&optional arg)
  (interactive "P")
  (if arg
      (setq sow-scroll-up-command
            'pdf-view-scroll-up-or-next-page
            sow-scroll-down-command
            'pdf-view-scroll-down-or-previous-page))
  (setq sow-scroll-up-command
        'pdf-view-scroll-up-or-next-column
        sow-scroll-down-command
        'pdf-view-scroll-down-or-previous-column))

(defvar pdf-sync-last-forward-correlate)
(defun advice/memorize-pdf-sync-forward-correlate (proc &rest r)
  (setq pdf-sync-last-forward-correlate (apply proc r)))
(advice-add 'pdf-sync-forward-correlate :around #'advice/memorize-pdf-sync-forward-correlate)

(defun pdf-sync-scroll-to-column (&rest r)
  (cl-destructuring-bind (pdf page x1 y1 x2 y2)
      pdf-sync-last-forward-correlate
    (cond ((< x2 0.55) (image-bol nil))
          ((> x1 0.45) (image-eol nil)))))

(defun advice/pdf-sync-scroll-to-column (proc &rest r)
  (prog2
      (advice-add 'pdf-util-tooltip-arrow :before #'pdf-sync-scroll-to-column)
      (apply proc r)
    (advice-remove 'pdf-util-tooltip-arrow #'pdf-sync-scroll-to-column)))

(advice-add 'pdf-sync-forward-search :around #'advice/pdf-sync-scroll-to-column)

(map!
 :map pdf-view-mode-map
 "M-k" #'pdf-view-scroll-down-or-previous-column
 "M-j" #'pdf-view-scroll-up-or-next-column)

;; (use-package! org-window-habit
  ;; :config
  ;; (org-window-habit-mode +1))

(defun cashpw/org-today--select-marker-from-alist (label-to-marker-alist)
  "Prompt user to select from LABEL-TO-MARKER-ALIST and to to that marker."
  (interactive)
  (let* ((vertico-sort-function #'vertico-sort-alpha)
         (selection (completing-read "Select: " label-to-marker-alist nil t)))
    (alist-get selection label-to-marker-alist nil nil #'string=)))

(defun cashpw/org-today--format-heading ()
  "Return string identifier for heading at point."
  (let* ((scheduled (org-element-property :scheduled (org-element-at-point)))
         (time-string
          (let ((hour-start (org-element-property :hour-start scheduled))
                (minute-start (org-element-property :minute-start scheduled))
                (hour-end (org-element-property :hour-end scheduled))
                (minute-end (org-element-property :minute-end scheduled)))
            (cond
             ((and hour-start minute-start hour-end minute-end)
              (format "%02d:%02d-%02d:%02d "
                      hour-start
                      minute-start
                      hour-end
                      minute-end))
             ((and hour-start minute-start)
              (format "%02d:%02d " hour-start minute-start))
             (t
              "")))))
    (concat (propertize time-string 'face 'shadow) (org-entry-get nil "ITEM"))))

(defun cashpw/select-from-todays-todos-and-go-to ()
  "Prompt user to select a todo, then go to it."
  (interactive)
  (let ((marker
         (cashpw/org-today--select-marker-from-alist
          (org-ql-query
            :select
            (lambda () (cons (cashpw/org-today--format-heading) (point-marker)))
            :from (cashpw/org-agenda-view--today--files)
            :where
            `(and (or (tags "everyday") (deadline 0) (scheduled 0))
                  (and (not (todo "CANCELLED"))
                       (not (todo "DECLINED"))
                       (not (todo "DONE"))
                       (not (todo "KILL"))
                       (not (todo "MOVE"))
                       (not (todo "RESCHEDULE"))))))))
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)))

(use-package! font-lock-profiler)

(defun eww--capture-headings-on-page ()
  "Return an alist in the form \"LABEL . POINT\" for the current buffer."
  (let ((heading-stack '())
        headings match)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'outline-level))
        (let* ((level (prop-match-value match))
               (start-point-prop (prop-match-beginning match))
               (end-point-prop (prop-match-end match))
               (text (replace-regexp-in-string
                      "\n" " "      ; NOTE 2021-07-25: newlines break completion
                      (buffer-substring-no-properties
                       start-point-prop end-point-prop))))
          (cond
           ((= level (length heading-stack))
            (pop heading-stack)
            (push text heading-stack))
           ((< level (length heading-stack))
            ;; There's an upward gap between headings (for example: h5, then h2)
            (dotimes (_ (1+ (- (length heading-stack) level)))
              (pop heading-stack))
            (push text heading-stack))
           ((> level (length heading-stack))
            ;; There's a downward gap between headings (for example: h2, then h5)
            (dotimes (_ (1- (- level (length heading-stack))))
              (push nil heading-stack))
            (push text heading-stack)))
          (push (cons
                 (concat
                  (let ((preceeding-heading-stack (remove nil (cdr heading-stack))))
                    (when preceeding-heading-stack
                      (propertize
                       (concat
                        (string-join (reverse preceeding-heading-stack) "/")
                        "/")
                       'face 'shadow)))
                  (car heading-stack))
                 start-point-prop)
                headings))))
    headings))

(defun +eww/jump-to-heading-on-page ()
  "Jump to heading position on the page (whole buffer) using completion."
  (interactive nil 'eww-mode)
  (unless (derived-mode-p 'eww-mode)
    (user-error "Not in an eww buffer!"))
  (let* ((headings (eww--capture-headings-on-page))
         (selection (completing-read "Jump to heading: " headings nil t)))
    (goto-char (alist-get selection headings nil nil #'string=))
    (recenter)))
