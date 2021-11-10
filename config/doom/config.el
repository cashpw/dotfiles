;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; DO NOT EDIT THIS FILE MANUALLY.
;; This file is generated from doom.md. You should make your changes there and
;; this file using org-babel-tangle.

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
; (setq user-full-name "John Doe"
;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
; (setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq
 user-full-name "Cash Weaver"
 user-mail-address "cashweaver@google.com")

(setq
 doom-theme 'doom-tomorrow-night
 show-trailing-whitespace t)

(setq
 ;; Use YYYY-MM-DD date format.
 calendar-date-style 'iso)

(defun cashweaver-get-date (&optional date-format offset-days)
  "Return the (offset) date in format."
  (interactive)
  (let ((date-format (or date-format "%Y-%m-%d"))
        (offset-days (or offset-days 0)))
    (shell-command-to-string
     (format "echo -n $(date \"+%s\" --date=\"%d days\")" date-format offset-days))))

(defun cashweaver-todays-date ()
  "Return todays date as YYYY-MM-DD."
  (cashweaver-get-date
   ; date-format
   "%Y-%m-%d"
   ; offset-days
   0))

(defun cashweaver-yesterdays-date ()
  "Return yesterday's date as YYYY-MM-DD."
  (cashweaver-get-date
   ; date-format
   "%Y-%m-%d"
   ; offset-days
   -1))

(use-package! aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(after! gnus-alias
  (setq
   gnus-alias-identity-alist '(("work"
                                ;; Refers to
                                nil
                                "Cash Weaver <cashweaver@google.com>"
                                ;; Organization
                                nil
                                ;; Extra headers
                                nil
                                ;; Body
                                nil "~/.email_signature"))
   gnus-alias-default-identity "work"))

;(use-package! calfw-cal
;  :config
;  (setq
;   ; Start the week on Monday
;   calendar-week-start-day 1))
;
;(use-package! calfw-ical)
;(use-package! calfw-org)
;
;(defun cashweaver-calfw-open ()
;  "Open my calendar"
;  (interactive)
;  (cfw:open-calendar-buffer
;   :contents-sources
;   (list
;    (cfw:org-create-source "Green"))))

(use-package! doct
  :commands (doct))

(use-package! gnus-alias
  :config
  (autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
  (gnus-alias-init))

(after! gnus-alias
  (setq
   gnus-alias-identity-alist '(("work"
                                ;; Refers to
                                nil
                                "Cash Weaver <cashweaver@google.com>"
                                ;; Organization
                                nil
                                ;; Extra headers
                                nil
                                ;; Body
                                nil "~/.email_signature"))
   gnus-alias-default-identity "work"))

(defun cashweaver-notmuch--search-thread-has-tag-p (match-tag)
  "Whether or not the thread has a tag."
  (interactive)
  (let ((thread-tags (notmuch-search-get-tags)))
    (member match-tag thread-tags)))

(defun cashweaver-notmuch-search-toggle-tag (tag)
  "Toggle the provided tag."
  (interactive)
  (if (member tag (notmuch-search-get-tags))
      (notmuch-search-tag (list (concat "-" tag)))
    (notmuch-search-tag (list (concat "+" tag)))))

(defun cashweaver-notmuch--search-thread-toggle-tag (key)
  "Toggle the specified tag(s)."
  (interactive "k")
  (let ((tags (assoc key cashweaver-notmuch-tag-alist)))
    (apply 'notmuch-search-tag (cdr tags))))

(defun cashweaver-notmuch-search-super-archive (&optional beg end)
  "Super archive the selected thread; based on `notmuch-search-archive-thread'."
  (interactive (notmuch-interactive-region))
  (notmuch-search-tag cashweaver-notmuch-super-archive-tags beg end)
  (when (eq beg end)
    (notmuch-search-next-thread)))

(defun cashweaver-org-notmuch-capture-follow-up-mail()
  "Capture mail to org mode."
  (interactive)
  (org-store-link nil)
  (org-capture nil "ef"))

(after! notmuch
  (setq
   notmuch-wash-wrap-lines-length 100
   notmuch-saved-searches '((:name "inbox"
                             :key "i"
                             :query "tag:inbox")
                            (:name "p0"
                             :key "0"
                             :query "tag:p0")
                            (:name "bugs"
                             :key "b"
                             :query "tag:Bug AND tag:inbox")
                            (:name "bugs (all)"
                             :key "B"
                             :query "tag:Bug")
                            (:name "waiting"
                             :key "w"
                             :query "tag:waiting")
                            (:name "automated"
                             :key "a"
                             :query "tag:Automated AND tag:inbox")
                            (:name "to-read"
                             :key "r"
                             :query "tag:Read!")
                            (:name "sent"
                             :key "s"
                             :query "tag:sent")
                            (:name "drafts"
                             :key "d"
                             :query "tag:draft"))
   +notmuch-home-function (lambda ()
                            (notmuch-search "tag:inbox"))
   notmuch-archive-tags '("-inbox"
                          "-unread")
   notmuch-search-line-faces '(("p0" . '(:foreground "red"))
                               ("Bug" . '(:foreground "green"))
                               ("waiting" . '(:foreground "yellow"))
                               ("Calendar-Events" . '(:foreground "blue"))
                               ("Read!" . '(:foreground "magenta")))
                                        ; Superset of `notmuch-archive-tags' for super archiving.
   cashweaver-notmuch-super-archive-tags (append
                                          notmuch-archive-tags
                                          '("-p0"
                                            "-waiting"
                                            "-Read!")))

                                        ; Prevent wrapping at 70 characters in email composition.
  (add-hook! 'message-mode-hook 'turn-off-auto-fill)
  (add-hook! 'message-mode-hook 'visual-line-mode)

                                        ; Reply-all should be the default.
  (evil-define-key 'normal notmuch-show-mode-map "cr" 'notmuch-show-reply)
  (evil-define-key 'normal notmuch-show-mode-map "cR" 'notmuch-show-reply-sender)

                                        ; Easy archive for my most-used tags.
  (evil-define-key 'normal notmuch-search-mode-map "A" 'notmuch-search-archive-thread)
  (evil-define-key 'normal notmuch-search-mode-map "a" 'cashweaver-notmuch-search-super-archive)
  (evil-define-key 'visual notmuch-search-mode-map "a" 'cashweaver-notmuch-search-super-archive)

                                        ; Unbind "t", and re-bind it to "T", so we can set it up as a prefix.
  (evil-define-key 'normal notmuch-search-mode-map "t" nil)
  (evil-define-key 'normal notmuch-search-mode-map "T" 'notmuch-search-filter-by-tag)

                                        ; Helpers for toggling often-used tags.
  (evil-lambda-key 'normal notmuch-search-mode-map "t0" '(lambda ()
                                                           "Toggle p0"
                                                           (interactive)
                                                           (cashweaver-notmuch-search-toggle-tag "p0")))
  (evil-lambda-key 'normal notmuch-search-mode-map "tr" '(lambda ()
                                                           "Toggle Read!"
                                                           (interactive)
                                                           (cashweaver-notmuch-search-toggle-tag "Read!")))
  (evil-lambda-key 'normal notmuch-search-mode-map "tw" '(lambda ()
                                                           "Toggle waiting"
                                                           (interactive)
                                                           (cashweaver-notmuch-search-toggle-tag "waiting"))))

(use-package! org-mime)

(defun cashweaver-compose-mail-org ()
  (interactive)
  (compose-mail)
  (message-goto-body)
  (setq *compose-html-org* t)
  (org-mode))

(defun cashweaver-htmlize-and-send-mail-org ()
  (interactive)
  (when *compose-html-org*
    (setq *compose-html-org* nil)
    (message-mode)
    (org-mime-htmlize)
    (message-send-and-exit)))

(defun cashweaver-org-mode-when-done ()
  "Perform actions when a task is marked as done."
  (when (org-entry-is-done-p)
    (org-clock-out-if-current)
    ;; Archive
    (unless (org-get-repeat)
      (org-archive-subtree-default))))

(after! org
  (setq
   org-ellipsis " ▾ "
   org-log-done 'time
   ;; Start the org agenda mini-calendar on Monday.
   calendar-week-start-day 1
   org-priority-highest 0
   org-priority-default 2
   org-priority-lowest 4
   org-hide-leading-stars t)
  (add-hook!
   'org-after-todo-state-change-hook
   'cashweaver-org-mode-when-done))

(after! org
  (setq
   org-todo-keywords
   '((sequence
      ; A task that needs doing & is ready to do
      "TODO(t)"
      ; A project, which usually contains other tasks
      "PROJ(p)"
      ; A task that is in progress
      "INPROGRESS(i)"
      ; Something external is holding up this task
      "BLOCKED(b)"
      ; This task is paused/on hold because of me
      "HOLD(h)"
      "|"
      ; Task successfully completed
      "DONE(d)"
      ; Task was moved
      "MOVE(m)"
      ; Task was cancelled, aborted or is no longer applicable
      "KILL(k)")
     (sequence
      ; A task that needs doing
      "[ ](T)"
      ; Task is in progress
      "[-](S)"
      ; Task is being held up or paused
      "[?](W)"
      "|"
      ; Task was completed
      "[X](D)"))
   org-todo-keyword-faces
   '(("[-]"  . +org-todo-active)
     ("INPROGRESS" . +org-todo-active)
     ("[?]"  . +org-todo-onhold)
     ("BLKD" . +org-todo-onhold)
     ("HOLD" . +org-todo-onhold)
     ("PROJ" . +org-todo-project))))

(after! org-agenda
  (setq
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-include-deadlines t
   org-agenda-block-separator nil
   org-agenda-compact-blocks t
   org-agenda-start-day nil ;; i.e. today
   org-agenda-span 1
   org-agenda-start-on-weekday nil))

(defun cashweaver-org-mode-insert-heading-for-today ()
  "Insert a heading for today's date, with relevant tags."
  (interactive)
  (let* ((today-week-number (cashweaver-get-date "%W"))
         (today-quarter-number (cashweaver-get-date "%q"))
         (today-year (cashweaver-get-date "%Y"))
         (today-month (cashweaver-get-date "%m"))
         (today-day (cashweaver-get-date "%d"))
         (today-weekday-name (cashweaver-get-date "%A")))
    (org-insert-heading-respect-content)
    (insert
     (format "%s-%s-%s %s :week%s:quarter%s:"
             today-year
             today-month
             today-day
             today-weekday-name
             today-week-number
             today-quarter-number))))

(defun cashweaver-org-mode-insert-heading-for-this-week ()
  "Insert a heading for this week, with relevant tags."
  (interactive)
  (let* ((today-week-number (cashweaver-get-date "%W"))
         (today-quarter-number (cashweaver-get-date "%q"))
         (today-year (cashweaver-get-date "%Y")))
    (org-insert-heading-respect-content)
    (insert
     (format "%s Week %s :week%s:quarter%s:"
             today-year
             today-week-number
             today-week-number
             today-quarter-number))))

(setq
 cashweaver--schedule-block-day '(:start "07:00" :end "19:00")
 cashweaver--schedule-block-one '(:start "07:00" :end "09:00")
 cashweaver--schedule-block-two '(:start "09:00" :end "11:00")
 cashweaver--schedule-block-three '(:start "14:00" :end "16:00")
 cashweaver--schedule-block-four '(:start "16:00" :end "18:00"))

(defun cashweaver-org-schedule-for-block (block-time &optional date)
  (interactive)
  (let ((start-time (plist-get block-time :start))
        (end-time (plist-get block-time :end))
        (date (or date "today")))
    (org-schedule nil (format "%s %s-%s"
                              date
                              start-time
                              end-time))))

(defun cashweaver-org--schedule-today-at (start-time-as-string)
  "Schedule a task today at the specified time."
  (interactive "sWhen?: ")
  (message start-time-as-string)
  (string-match
   "^\\([1-9]\\|[01][0-9]\\|2[0-3]\\):?\\([0-5][0-9]\\)?$"
   start-time-as-string)
  (let
      ((hour
        (string-to-number
         (or
          (match-string 1 start-time-as-string)
          "0")))
       (minute
        (string-to-number
         (or
          (match-string 2 start-time-as-string)
          "0"))))
    (org-schedule nil (format "today %02d:%02d"
                              hour
                              minute))
    (message (number-to-string hour))
    ))

(defun cashweaver-org--schedule-for (start-time end-time &optional date)
  (let ((date (or date "today")))
    (org-schedule nil (format "%s %s-%s"
                              date
                              start-time
                              end-time))))
    ;(org-schedule nil (format "%s %s-%s"
                              ;date
                              ;start-time
                              ;end-time))))

(defun cashweaver-org--schedule-at-for-minutes (start-minute start-hour duration-in-minutes &optional date)
  (let* ((start-time-in-minutes-since-midnight
         (+ start-minute (* start-hour 60)))
        (end-time-in-minutes-since-midnight
         (+ start-time-in-minutes-since-midnight duration-in-minutes))
        (end-minute (mod end-time-in-minutes-since-midnight 60))
        (end-hour (/ end-time-in-minutes-since-midnight 60))
        (date (or date "today")))
    (org-schedule nil (format "%s %02d:%02d-%02d:%02d"
                              date
                              start-hour
                              start-minute
                              end-hour
                              end-minute))))

(setq
 cashweaver--schedule-pomodoro-one '(:start "09:00" :end "09:50")
 cashweaver--schedule-pomodoro-two '(:start "10:00" :end "10:50")
 cashweaver--schedule-pomodoro-three '(:start "11:00" :end "11:50")
 cashweaver--schedule-pomodoro-four '(:start "12:00" :end "12:50")
 cashweaver--schedule-pomodoro-five '(:start "13:00" :end "13:50")
 cashweaver--schedule-pomodoro-six '(:start "14:00" :end "14:50")
 cashweaver--schedule-pomodoro-seven '(:start "15:00" :end "15:50")
 cashweaver--schedule-pomodoro-eight '(:start "16:00" :end "16:50")
 cashweaver--schedule-pomodoro-nine '(:start "17:00" :end "17:50")
 cashweaver--schedule-pomodoro-ten '(:start "18:00" :end "18:50"))

(defun cashweaver-org-schedule-at-pomodoro (pomodoro-time &optional date)
  (interactive)
  (let ((start-time (plist-get pomodoro-time :start)))
        (date (or date "today")))
    (org-schedule nil (format "%s %s"
                              date
                              start-time)))

(defun cashweaver-org-schedule-in-n-hours (offset-hours &optional date)
  (interactive)
  (let* ((time-list (parse-time-string (current-time-string)))
         (current-hour (nth 2 time-list))
         (current-minute (nth 1 time-list))
         (hour (mod (+ current-hour offset-hours) 24))
         (date (or date "today")))
    (org-schedule nil (format "%s %s:%s"
                              date
                              hour
                              current-minute))))

(defun cashweaver-org-schedule-in-n-workdays (num-days &optional time)
  (interactive)
  (let*
      ((time (or time "09:00"))
       (offset-days))
    (org-schedule
     nil
     (format "%s %s"
             offset-days
             time))))

(use-package! ox-pandoc
  :after (:all org)
  :config
  (setq org-pandoc-menu-entry
        '((?D "to docx and open." org-pandoc-export-to-docx-and-open)
          (?d "to docx." org-pandoc-export-to-docx)
          (?m "to markdown." org-pandoc-export-to-markdown)
          (?M "to markdown and open." org-pandoc-export-to-markdown-and-open))
        org-pandoc-options-for-docx
        '((lua-filter . "/usr/local/google/home/cashweaver/third_party/google_docs_pandoc/pandoc/GenericDocFilter.lua")
          (reference-doc . "/usr/local/google/home/cashweaver/third_party/google_docs_pandoc/pandoc/CashWeaverGenericDocTemplate.docx")
          ;;(reference-doc . "/usr/local/google/home/cashweaver/third_party/google_docs_pandoc/pandoc/GenericDocTemplate.docx")
          (highlight-style . "/usr/local/google/home/cashweaver/third_party/google_docs_pandoc/pandoc/Kodify.theme"))))

(defun org-pandoc-publish-to (format plist filename pub-dir)
  "Publish using Pandoc (https://github.com/kawabata/ox-pandoc/issues/18#issuecomment-262979338)."
  (setq org-pandoc-format format)
  (let ((tempfile
         (org-publish-org-to
          'pandoc
          filename
          (concat (make-temp-name ".tmp") ".org")
          plist pub-dir))
        (outfile
         (format
          "%s.%s"
          (concat
           pub-dir
           (file-name-sans-extension (file-name-nondirectory filename)))
          (assoc-default format org-pandoc-extensions))))
    ;;(org-pandoc-put-options (org-pandoc-plist-to-alist plist))
    (let ((process
           (org-pandoc-run
            tempfile
            outfile
            format
            'org-pandoc-sentinel
            org-pandoc-option-table))
          (local-hook-symbol
           (intern
            (format "org-pandoc-after-processing-%s-hook" format))))
      (process-put process 'files (list tempfile))
      (process-put process 'output-file filename)
      (process-put process 'local-hook-symbol local-hook-symbol))))

(defun org-pandoc-publish-to-md (plist filename pub-dir)
  "Publish to markdown using Pandoc."
  (org-pandoc-publish-to 'markdown plist filename pub-dir))

(defun cashweaver-org-roam-make-filepath (title &optional time time-zone)
  "Return a filenaem for an org-roam node.

Reference: https://ag91.github.io/blog/2020/11/12/write-org-roam-notes-via-elisp"
  (let ((timestamp
         (format-time-string
          "%Y%m%d%H%M%S"
          (or time
              (current-time))
          (or time-zone
              (current-time-zone))))
        (slug
         (org-roam-node-slug
          (org-roam-node-create
           :title title))))
    (format
     "%s/%s_%s.org"
     org-roam-directory
     timestamp
     slug)))
;;(cashweaver-org-roam-make-filepath "This is the foo")

(defun cashweaver-org-mode-insert-option (option value)
  "Insert an org-mode option (#+OPTION: VALUE)."
  (interactive)
  (insert
   (format
    "#+%s: %s\n"
    option
    value)))

(defun cashweaver-org-mode-insert-options (options)
  "Insert an alist of org-mode options (#+OPTION: VALUE)."
  (interactive)
  (cl-loop for (option . value) in options
           do (cashweaver-org-mode-insert-option
               option
               value)))

(defun cashweaver-org-mode-insert-property (property value)
  "Insert an org-mode property (:PROPERTY: VALUE)."
  (interactive)
  (insert
   (format
    ":%s: %s\n"
    property
    value)))

(defun cashweaver-org-mode-insert-properties (properties)
  "Insert an alist of org-mode properties (:PROPERTY: VALUE).

When WRAP is non-nil: Wrap the properties with :PROPERTIES:/:END:."
  (interactive)
  (cl-loop for (property . value) in properties
           do (org-set-property
               property
               value)))

(defun cashweaver-org-roam-new-node (file-path title &optional properties)
  "Build a new org-roam node in a temp file.

PROPERTIES is expected to be an alist of additional properties to include.

Reference: https://ag91.github.io/blog/2020/11/12/write-org-roam-notes-via-elisp"
  (let* ((id (org-id-new))
         (all-properties
          (append
           `(("ID" . ,id)
             ("DIR" . ,cashweaver-org-roam-attachment-dir-path))
           properties)))
    (with-temp-file
        file-path
      (cashweaver-org-mode-insert-properties
       all-properties)
      (cashweaver-org-mode-insert-options
       `(("TITLE" . ,title)
         ("STARTUP" . "overview"))))))

(defun cashweaver-org-roam-new-node-from-link-heading-at-point (&optional mark-as-done)
  "Build a new org-roam node from the link heading at point."
  (interactive)
  (let* ((link
          (org-element-context))
         (type
          (org-element-property
           :type
           link))
         (url
          (org-element-property
           :raw-link
           link))
         (description
          (cashweaver-org-mode-get-description-from-link-at-point))
         (org-roam-node-file-path
          (cashweaver-org-roam-make-filepath description)))
    ;; TODO Replace with regexp?
    (unless (or (string= type "http")
                (string= type "https")))
    (cashweaver-org-roam-new-node
     org-roam-node-file-path
     description
     `(("ROAM_REFS" . ,url)))
    (if mark-as-done
        (org-todo "DONE"))
    (find-file
     org-roam-node-file-path)))

(defun cashweaver-org-mode-get-description-from-link-at-point ()
  "Reference: https://emacs.stackexchange.com/a/38297"
  (interactive)
  (let ((link
         (org-element-context)))
    (message
     "%s"
     (buffer-substring-no-properties
      (org-element-property
       :contents-begin
       link)
      (org-element-property
       :contents-end
       link)))))

(defun cashweaver-org-roam-open-ref ()
  "Open the ROAM_REF."
  (interactive)
  (let ((roam-refs
         (org-entry-get
          (point)
          "ROAM_REFS")))
    (if (s-starts-with-p
         "http"
         roam-refs)
        (browse-url roam-refs)
      (message "Not an http(s) ref."))))

(defun cashweaver-org-roam-insert-attachment-path ()
  (save-excursion
    (org-set-property
     "DIR"
     cashweaver-org-roam-attachment-dir-path)))

(use-package! org-roam
  :after org
  :config
  (setq
   org-roam-directory
   (file-truename
    "~/proj/roam")
   cashweaver-org-roam-attachment-dir-path
   (file-truename
    (format
     "%s/attachments"
     org-roam-directory))
   org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (add-hook!
   'org-roam-capture-new-node-hook
   'cashweaver-org-roam-insert-attachment-path)
  (org-roam-db-autosync-mode))

;;(use-package! org-noter
  ;;;; Based on https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/contrib/noter.el
  ;;:defer t
  ;;:preface
  ;;;; Allow the user to preempt this and set the document search path
  ;;;; If not set then use `org-directory'
  ;;(defvar org-noter-notes-search-path nil)
  ;;:config
  ;;(unless org-noter-notes-search-path
    ;;(setq org-noter-notes-search-path (list org-directory)))
  ;;(setq org-noter-auto-save-last-location t
        ;;org-noter-separate-notes-from-heading t))

(use-package! pdf-tools
  :config
  (pdf-tools-install))

(defun cashweaver-org-noter-insert-selected-text-inside-note-content ()
  (interactive)
  (progn (setq currenb (buffer-name))
	 (org-noter-insert-precise-note)
	 (set-buffer currenb)
	 (org-noter-insert-note)
	 ;;(org-noter-quote)
         ))

(fset 'org-noter-quote
   (kmacro-lambda-form [return S-f3 backspace ?# ?+ ?e ?n ?d ?_ ?q ?u ?o ?t ?e ?\C-r ?: ?e return ?\C-e return delete ?# ?+ ?b ?e ?g ?i ?n ?_ ?q ?u ?o ?t ?e] 1 "%d"))

(define-key pdf-view-mode-map (kbd "y") 'cashweaver-org-noter-insert-selected-text-inside-note-content)

; Reference; https://www.emacswiki.org/emacs/DocumentingKeyBindingToLambda
(defun evil-lambda-key (mode keymap key def)
  "Wrap `evil-define-key' to provide documentation."
  (set 'sym (make-symbol (documentation def)))
  (fset sym def)
  (evil-define-key mode keymap key sym))

;; Keep in alphabetical order.
(map!
 (:leader
  :desc "at point" :n "h h" #'helpful-at-point

  ;;:desc "Store email link" :n "n L" #'org-notmuch-store-link
  ))

(after! org
  (map!
   :map org-mode-map
   :localleader

   (:prefix ("b")
    :n "RET" #'org-table-copy-down)

   (:prefix ("d")
    (:prefix ("h" . "insert heading")
     :n "d" #'cashweaver-org-mode-insert-heading-for-today
     :n "w" #'cashweaver-org-mode-insert-heading-for-this-week)

    (:prefix ("S")
     (:prefix ("." . "today")
      :desc "at" :n "a" #'cashweaver-org--schedule-today-at
      )))

   (:prefix ("m")
    :desc "Open ref" :n "O" #'cashweaver-org-roam-open-ref
    :desc "Create node from headline link" :n "N" (cmd! ()
                                                        (cashweaver-org-roam-new-node-from-link-heading-at-point
                                                         ;; mark-as-done
                                                         t)))))

(setq
 cashweaver-work-config-dir "/usr/local/google/home/cashweaver/.config/doom")
(defun cashweaver-is-work-p ()
  "Return true if executed on my work machine."
  (file-directory-p cashweaver-work-config-dir))

(if (cashweaver-is-work-p)
    (load (concat cashweaver-work-config-dir "/config-work.el")))
