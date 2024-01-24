(setq
 search-invisible t)

(defgroup cashpw nil
  "Group for my customizations and configurations.")

(use-package! s
  :ensure t)
(use-package! dash
  :ensure t)
;; Fix error: "File mode specification error: (error Problem in magic-mode-alist with element ess-SAS-listing-mode-p)".
(use-package! ess-site
  :ensure t)

(setq
 user-full-name "Cash Prokop-Weaver"
 user-mail-address "cashbweaver@gmail.com")

(setq
 ;; Use YYYY-MM-DD date format.
 calendar-date-style 'iso)

(defun cashpw/day-of-week (time)
  "Return day of week of TIME as integer; 1 is Monday, 7 is Sunday."
  (string-to-number (format-time-string "%u" time)))

(defun cashpw/is-monday (time)
  "Return non-nil if TIME is on a Monday."
  (= 1
     (cashpw/day-of-week time)))

(defun cashpw/is-tuesday (time)
  "Return non-nil if TIME is on a Tuesday."
  (= 2
     (cashpw/day-of-week time)))

(defun cashpw/is-wednesday (time)
  "Return non-nil if TIME is on a Wednesday."
  (= 3
     (cashpw/day-of-week time)))

(defun cashpw/is-thursday (time)
  "Return non-nil if TIME is on a Thursday."
  (= 4
     (cashpw/day-of-week time)))

(defun cashpw/is-friday (time)
  "Return non-nil if TIME is on a Friday."
  (= 5
     (cashpw/day-of-week time)))

(defun cashpw/is-saturday (time)
  "Return non-nil if TIME is on a saturday."
  (= 6
     (cashpw/day-of-week time)))

(defun cashpw/is-sunday (time)
  "Return non-nil if TIME is on a Sunday."
  (= 7
     (cashpw/day-of-week time)))

(defun cashpw/is-weekday (time)
  "Return non-nil if TIME is a weekday."
  (< (cashpw/day-of-we timeek)
     6))

(defun cashpw/is-weekend (time)
  "Return non-nil if TIME is a weekend."
  (not (cashpw/is-weekday time)))

(defcustom cashpw/path--proj-dir
  (s-lex-format "${cashpw/path--home-dir}/proj")
  "Projects directory."
  :group 'cashpw
  :type 'string)

(defcustom cashpw/path--notes-dir
  (s-lex-format "${cashpw/path--proj-dir}/notes")
  "Personal org-roam notes directory."
  :group 'cashpw
  :type 'string)

(defcustom cashpw/path--notes-private-dir
  (s-lex-format "${cashpw/path--proj-dir}/notes-private")
  "Personal private org-roam notes directory."
  :group 'cashpw
  :type 'string)

(defcustom cashpw/path--notes-personal-dir
  (s-lex-format "${cashpw/path--proj-dir}/notes-personal")
  "Personal notes directory."
  :group 'cashpw
  :type 'string)

(defcustom cashpw/path--people-dir
  (s-lex-format "${cashpw/path--proj-dir}/people")
  "Personal people org-roam notes directory."
  :group 'cashpw
  :type 'string)

;; (setq
 ;; browse-url-firefox-program "firefox-esr")

(use-package! command-log-mode
  :config
  (setq
   command-log-mode-open-log-turns-on-mode t
   command-log-mode-window-size 80
   command-log-mode-is-global t))

(use-package! centered-cursor-mode)

(after! evil
  ;; Speed up org-mode table editing
  ;; https://github.com/emacs-evil/evil/issues/1623#issuecomment-1414406022
  (advice-remove 'set-window-buffer #'ad-Advice-set-window-buffer))

(use-package! free-keys)

(use-package! titlecase)

(use-package! whisper
  :config
  (setq whisper-install-directory "~/.config/emacs/.local/cache/"
        ;; whisper-model "large-v3"
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        whisper--ffmpeg-input-device "hw:0"))

(setq
 alert-fade-time 60
 alert-default-style 'libnotify)

;; Too early load error
;; (use-package! org-wild-notifier
  ;; :after org
  ;; :config
  ;; (setq
   ;; org-wild-notifier-alert-time '(2))
  ;; (org-wild-notifier-mode))

(defun cashpw/delete-lines-below (line-number)
  "Delete all lines beneath LINE-NUMBER."
  (interactive "nLine number: ")
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (delete-region (point) (point-max))))

(defun cashpw/org-set-property-on-all-top-level-headings (property value)
  "Reference: Chat GPT"
  (interactive "sEnter property name: \nsEnter property value: ")
  (org-map-entries
   (lambda ()
     (when (= (org-outline-level) 1)
       (org-entry-put (point) property value)))
   nil 'file))

(defun run-function-in-file (filepath function &optional arguments)
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

(defcustom cashpw/secrets-dir-path
  (s-lex-format "${cashpw/path--home-dir}/.config/secrets")
  "Path to directory containing secret files.")

(defun cashpw/get-secret (name)
  "Get content of NAME secret file."
  (let ((secret-file-path (s-lex-format "${cashpw/secrets-dir-path}/${name}")))
    (if (file-exists-p secret-file-path)
        (string-clean-whitespace
         (with-temp-buffer
           (insert-file-contents secret-file-path)
           (buffer-string)))
      "")))

(defun cashpw/file--get-header-file-path (file-path)
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

(defun cashpw/file--get-test-file-path (cpp-file-path)
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

(defun cashpw/directory-files--org (dir-path &optional include-archive)
  "Return a list of all .org$ files at DIR-PATH; include .org_archive if INCLUDE-ARCHIVE is non-nil."
  (let ((match (if include-archive
                   "\\.org\\(_archive\\)?$"
                 "\\.org$")))
    (directory-files dir-path t match)))

; Reference; https://www.emacswiki.org/emacs/DocumentingKeyBindingToLambda
(defun cashpw/evil-lambda-key (mode keymap key def)
  "Wrap `evil-define-key' to provide documentation."
  (set 'sym (make-symbol (documentation def)))
  (fset sym def)
  (evil-define-key mode keymap key sym))

(map!
 ;; Keep in alphabetical order.
 (:leader
  :desc "at point" :n "h h" #'helpful-at-point
  :desc "Langtool" :n "t L" #'langtool-check
  :n "r" #'whisper-run
  :n "A" #'org-agenda
  (:prefix ("o")
           (:prefix ("n")
            :desc "Commonplace" :n "C" (cmd! (cashpw/open-file (s-lex-format "${cashpw/path--notes-personal-dir}/commonplace.org")))
            ;; :desc "Todos" :n "c" (cmd! (cashpw/open-file (s-lex-format "${cashpw/path--notes-dir}/calendar.org")))
            :desc "Journal" :n "j" (cmd! (cashpw/open-file (s-lex-format "${cashpw/path--notes-personal-dir}/journal-2024.org")))
            :desc "Retrospective" :n "r" (cmd! (cashpw/open-file (s-lex-format "${cashpw/path--notes-personal-dir}/retrospective-2024.org")))
            :desc "Todos" :n "t" (cmd! (cashpw/open-file (s-lex-format "${cashpw/path--notes-personal-dir}/todos.org")))))
  (:prefix ("n")
   :desc "Store email link" :n "L" #'org-notmuch-store-link
   (:prefix ("A" . "Flashcards")
    :n "d" #'org-fc-dashboard
    :n "i" #'org-fc-init
    :n "u" #'org-fc-update
    :n "r" #'cashpw/org-fc-review-all
    :n "R" #'org-fc-review)
   (:prefix ("r")
    :desc "New node from citation" :n "c" #'cashpw/org-roam-node-from-cite))
  (:prefix ("p")
   :n "u" #'cashpw/projectile-refresh-known-paths)
  (:prefix ("t")
   :n "C" #'centered-cursor-mode
   :n "k" #'clm/toggle-command-log-buffer)))

(map!
 ;; Keep in alphabetical order.
 :map global-map
 "M-N" #'operate-on-number-at-point
 :v "C-r" #'cashpw/replace-selection
 (:prefix ("z")
  :n "O" #'evil-open-fold-rec))

(setq
 auto-save-visited-interval 60)
(auto-save-visited-mode)

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

(setq
 doom-theme 'doom-tomorrow-night
 show-trailing-whitespace t)

;; (use-package! svg-tag-mode
;;   :config
;;   (setq
;;    svg-tag-tags '(("\\(:[A-Z]+:\\)" . ((lambda (tag) (svg-tag-make tag :beg 1 :end -1)))))))

(use-package! nerd-icons)

(use-package! gnus-alias
  :config
  (autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
  (gnus-alias-init))

(after! gnus-alias
  (setq
   gnus-alias-identity-alist '(("cashbweaver@gmail"
                                ;; Refers to
                                nil
                                "Cash Weaver <cashbweaver@gmail.com>"
                                ;; Organization
                                nil
                                ;; Extra headers
                                nil
                                ;; Body
                                nil
                                "~/.config/email-signature-personal"))
   gnus-alias-default-identity "cashbweaver@gmail"))

(defun cashpw/notmuch-show-open-or-close-all ()
  "Toggle between showing and hiding all messages in the thread."
  (interactive))

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

(defun cashpw/notmuch-search-follow-up ()
  "Capture the email at point in search for following up."
  (interactive)
  (notmuch-search-show-thread)
  (goto-char
   (point-max))
  (org-capture
   ;; goto
   nil
   ;; keys
   "tef"))

(defun cashpw/org-notmuch-capture-follow-up-mail ()
  "Capture mail to org mode."
  (interactive)
  (org-store-link nil)
  (org-capture nil "ef"))

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

(after! notmuch
  (setq
   notmuch-wash-wrap-lines-length 100
   notmuch-saved-searches `(
                            ,(cashpw/notmuch--tag-search "a"
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

(defun cashpw/mail-get-short-address (address)
  "Returns \"foo@\" for an ADDRESS of \"Foo <foo@bar.com>\"."
  (cond
   ((not (string-match "<" address))
    address)
   (t
    (replace-regexp-in-string
     ".*<\\(.*\\)@.*>"
     "\\1@"
     address))))

(defun cashpw/mail-create-follow-up-todo ()
  (interactive)
  (let* ((file cashpw/path--file--notes-todos)
         (to-short (cashpw/mail-get-short-address
                    (message-field-value "To")))
         (from-short (cashpw/mail-get-short-address
                      (message-field-value "From")))
         (subject (message-field-value "Subject"))
         (message-id (replace-regexp-in-string
                      "<\\(.*\\)>"
                      "\\1"
                      (message-field-value "Message-ID")))
         (headline-text
          (s-lex-format
           "[[notmuch:id:${message-id}][${subject} (${from-short} ➤ ${to-short})]]: Follow up :email:")))
    (with-current-buffer (get-file-buffer file)
      (goto-char (point-max))
      (org-insert-heading-respect-content)
      (org-todo "TODO")
      (insert headline-text)
      (cashpw/org-mode--set-created)
      (org-schedule nil))))

(defun cashpw/message-send-and-exit ()
  (interactive)
  (org-mime-htmlize)
  (notmuch-mua-send)
  (if (y-or-n-p "Create follow-up TODO?")
      (cashpw/mail-create-follow-up-todo))
  (kill-buffer
   (current-buffer)))

(defun cashpw/send-mail-function (&rest args)
  "Wrapper method for `send-mail-function' for easy overriding in work environment."
  (apply #'sendmail-query-once args))

(defun cashpw/message-send-mail-function (&rest args)
  "Wrapper method for `message-send-mail-function' for easy overriding in work environment."
  (apply #'message--default-send-mail-function args))

(setq
 send-mail-function #'cashpw/send-mail-function
 message-send-mail-function #'cashpw/message-send-mail-function)

(map!
 :map message-mode-map
 "C-c C-c" #'cashpw/message-send-and-exit)
(map!
 :map message-mode-map
 "C-c C-c" #'cashpw/message-send-and-exit)

(map!
 :map message-mode-map
 :localleader
 "e" #'org-mime-edit-mail-in-org-mode)

(after! notmuch
  ;; Keep in alphabetical order.
  (map!
   :map notmuch-message-mode-map
   "C-c C-c" #'cashpw/message-send-and-exit)

  (map!
   :map notmuch-message-mode-map
   :localleader
   "e" #'org-mime-edit-mail-in-org-mode)

  (map!
   :map notmuch-show-mode-map
   "M-RET" #'cashpw/notmuch-show-open-or-close-all)

  ;; Reply-all should be the default.
  (evil-define-key 'normal notmuch-show-mode-map "cr" 'notmuch-show-reply)
  (evil-define-key 'normal notmuch-show-mode-map "cR" 'notmuch-show-reply-sender)

  ;; Easy archive for my most-used tags.
  (evil-define-key 'normal notmuch-search-mode-map "A" 'notmuch-search-archive-thread)
  (evil-define-key 'normal notmuch-search-mode-map "a" 'cashpw/notmuch-search-super-archive)
  (evil-define-key 'visual notmuch-search-mode-map "a" 'cashpw/notmuch-search-super-archive)
  (evil-define-key 'normal notmuch-search-mode-map "f" 'cashpw/notmuch-search-follow-up)

  ;; Unbind "t", and re-bind it to "T", so we can set it up as a prefix.
  (evil-define-key 'normal notmuch-search-mode-map "t" nil)
  (evil-define-key 'normal notmuch-search-mode-map "T" 'notmuch-search-filter-by-tag)

  ;; Helpers for toggling often-used tags.
  (cashpw/evil-lambda-key 'normal notmuch-search-mode-map "t0" '(lambda ()
                                                           "Toggle p0"
                                                           (interactive)
                                                           (cashpw/notmuch-search-toggle-tag "p0")))
  (cashpw/evil-lambda-key 'normal notmuch-search-mode-map "tr" '(lambda ()
                                                           "Toggle Read!"
                                                           (interactive)
                                                           (cashpw/notmuch-search-toggle-tag "Read!")))
  (cashpw/evil-lambda-key 'normal notmuch-search-mode-map "tw" '(lambda ()
                                                           "Toggle waiting"
                                                           (interactive)
                                                           (cashpw/notmuch-search-toggle-tag "waiting"))))



(setq
 ediff-split-window-function #'split-window-horizontally)

(defgroup cashpw/source-control nil
  "Source control."
  :group 'cashpw)

(defcustom cashpw/source-control--commit-categories '(("Fix" . (:emoji "🐛"))
                                                      ("Add" . (:emoji "✨"))
                                                      ("Feature" . (:emoji "✨"))
                                                      ("Document" . (:emoji "📝"))
                                                      ("Refactor" . (:emoji "♻"))
                                                      ("Rollout" . (:emoji "🚀"))
                                                      ("Launch" . (:emoji "🚀"))
                                                      ("Deploy" . (:emoji "🚀"))
                                                      ("Delete" . (:emoji "🔥"))
                                                      ("Remove" . (:emoji "🔥"))
                                                      ("Test" . (:emoji "✅")))
  "Alist of commit categories and extras."
  :group 'cashpw/source-control
  :type 'string)

(defun cashpw/source-control--read-commit-category ()
  "Return commit noun as selected by user."
  (let ((category (completing-read "Category: "
                                   cashpw/source-control--commit-categories
                                   ;; predicate
                                   nil
                                   ;; require-match
                                   t)))
    (assoc category
           cashpw/source-control--commit-categories)))

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

(use-package! langtool
  :init
  (setq
   langtool-language-tool-server-jar
   "~/third_party/LanguageTool-5.5/languagetool-server.jar"
   ;;langtool-language-tool-jar
   ;;"~/third_party/LanguageTool-5.5/languagetool-commandline.jar"
   )
  :config
  (setq
   langtool-default-language
   "en-US"
   langtool-mother-tongue
   "en"))

(use-package! operate-on-number)

(use-package! writeroom-mode
  :config
  (setq
   +zen-mixed-pitch-modes '()
   writeroom-width 45))

(use-package! aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package! org-anki
  :after org)

(use-package! anki-editor
  :after org
  :custom
  (defun cashpw/anki-to-fc ()
    (interactive)
    (let ((anki-card-heading-points (org-map-entries
                                     (lambda ()
                                       (point))
                                     "+LEVEL=2-fc+ANKI_NOTE_ID={.}")))
      (dolist (pom (reverse anki-card-heading-points))
        (goto-char pom)
        (cashpw/anki-to-fc--single))))

  (defun cashpw/anki-to-fc--single (&optional pom)
    (interactive)
    (let* ((pom (or pom (point)))
           (heading-text (org-entry-get pom "ITEM"))
           (anki-note-type (org-entry-get pom "ANKI_NOTE_TYPE"))
           (anki-note-id (org-entry-get pom "ANKI_NOTE_ID"))
           (data (cashpw/anki-to-fc--get-data anki-note-id))
           (card-created-time (plist-get (nth 0 data) :card-created-time)))
      (if (or (string= "AKA" anki-note-type)
              (string= "Cloze with Source" anki-note-type)
              (string= "Definition" anki-note-type)
              (string= "Denotes" anki-note-type))
          (org-fc--add-tag "orgfc_migration_safetodelete")
        (org-fc--add-tag "orgfc_migration_needswork"))
      (org-insert-heading)
      (org-set-tags ":fc:")
      (org-id-get-create)
      (org-set-property "ANKI_NOTE_ID"
                        anki-note-id)
      (org-set-property "FC_CREATED"
                        (cashpw/anki-to-fc--time-to-fc-time card-created-time))
      (cond
       ((string= "AKA" anki-note-type)
        (cashpw/anki-to-fc--aka heading-text
                                data
                                anki-note-id))
       ((string= "Cloze with Source" anki-note-type)
        (cashpw/anki-to-fc--cloze-with-source heading-text
                                              data
                                              anki-note-id))
       ;; ((string= "Compare/Contrast" anki-note-type)
       ;;  (cashpw/anki-to-fc--compare-contrast heading-text
       ;;                                     data
       ;;                                     anki-note-id))
       ((string= "Definition" anki-note-type)
        (cashpw/anki-to-fc--definition heading-text
                                       data
                                       anki-note-id))
       ((string= "Denotes" anki-note-type)
        (cashpw/anki-to-fc--denotes heading-text
                                    data
                                    anki-note-id))
       ;; ((string= "Equivalence" anki-note-type)
       ;;  (cashpw/anki-to-fc--equivalence heading-text
       ;;                                      data
       ;;                                      anki-note-id))
       (t
        (cashpw/anki-to-fc--default heading-text
                                    data
                                    anki-note-id
                                    anki-note-type)))))

  (defun cashpw/anki-to-fc--general (heading-text review-data fc-type)
    (insert (s-lex-format " ${heading-text}"))
    (org-set-property "FC_TYPE" fc-type)
    (org-fc-review-data-set review-data))

  (defun cashpw/anki-to-fc--normal (heading-text review-data front back &optional extra source)
    (cashpw/anki-to-fc--general heading-text
                                review-data
                                "normal")
    (save-excursion
      ;; Jump below the drawers
      (org-insert-subheading nil)
      ;; Delete the heading we just created
      (delete-backward-char 4)
      (insert front))
    (save-excursion
      ;; Jump below the drawers
      (org-insert-subheading nil)
      (insert "Back")
      (newline)
      (insert back))
    (when extra
      (save-excursion
        (org-insert-subheading nil)
        (insert "Extra")
        (newline)
        (insert extra)))
    (when source
      (save-excursion
        (org-insert-subheading nil)
        (insert "Source")
        (newline)
        (insert source))))

  (defun cashpw/anki-to-fc--double (heading-text review-data front back &optional extra source)
    (cashpw/anki-to-fc--normal heading-text
                               review-data
                               front
                               back
                               extra
                               source)
    (org-set-property "FC_TYPE" "double"))

  (defun cashpw/anki-to-fc--cloze (heading-text review-data &optional body extra source)
    (let* ((fc-cloze-max (number-to-string
                          (length review-data)))
           (heading-text (cashpw/anki-to-fc--convert-text
                          heading-text)))
      (cashpw/anki-to-fc--general heading-text
                                  review-data
                                  "cloze")
      (org-set-property "FC_CLOZE_MAX" fc-cloze-max)
      (org-set-property "FC_CLOZE_TYPE" "deletion")
      (when body
        (save-excursion
          ;; Jump below the drawers
          (org-insert-subheading nil)
          ;; Delete the heading we just created
          (delete-backward-char 4)
          (insert body)))
      (when extra
        (save-excursion
          (org-insert-subheading nil)
          (insert "Extra")
          (newline)
          (insert extra)))
      (when source
        (save-excursion
          (org-insert-subheading nil)
          (insert "Source")
          (newline)
          (insert source)))))

  (defun cashpw/anki-to-fc--default (heading-text data anki-note-id anki-note-type)
    (let* ((review-data (mapcar
                         (lambda (datum)
                           (list (plist-get datum :position)
                                 (plist-get datum :ease)
                                 (plist-get datum :box)
                                 (plist-get datum :interval)
                                 (plist-get datum :due)))
                         data)))
      (cashpw/anki-to-fc--normal anki-note-type
                                 review-data
                                 heading-text
                                 "TODO: Back")
      (org-set-tags ":fc:todo:")))

  (defun cashpw/anki-to-fc--cloze-with-source (heading-text data anki-note-id)
    (let* ((review-data (mapcar
                         (lambda (datum)
                           (list (plist-get datum :position)
                                 (plist-get datum :ease)
                                 (plist-get datum :box)
                                 (plist-get datum :interval)
                                 (plist-get datum :due)))
                         data))
           (fields (cashpw/anki-to-fc--get-fields anki-note-id)))
      (cl-destructuring-bind (text extra source) fields
        (let ((extra (if (> (length extra) 0)
                         (cashpw/anki-to-fc--convert-text extra)
                       nil))
              (source (if (> (length source) 0)
                          source
                        ;; "TODO: Source"
                        nil))))
        (cashpw/anki-to-fc--cloze heading-text
                                  review-data
                                  ;; body
                                  nil
                                  extra
                                  source))))

  (defun cashpw/anki-to-fc--compare-contrast (heading-text data anki-note-id)
    (let* ((fields (cashpw/anki-to-fc--get-fields anki-note-id))
           (review-data (mapcar
                         (lambda (datum)
                           (list "front"
                                 (plist-get datum :ease)
                                 (plist-get datum :box)
                                 (plist-get datum :interval)
                                 (plist-get datum :due)))
                         data)))
      (cl-destructuring-bind (concepts context comparisons-contrasts source) fields
        (let* ((concepts (cashpw/anki-to-fc--convert-text concepts))
               (context (if (> (length context) 0)
                            (cashpw/anki-to-fc--convert-text context)
                          nil))
               (comparisons-contrasts (if (> (length comparison-contrasts) 0)
                                          (cashpw/anki-to-fc--convert-text comparison-contrasts)
                                        nil))
               (source (if (> (length source) 0)
                           source
                         ;; "TODO: Source"
                         nil))
               (heading-text (if context
                                 (s-lex-format "Compare/Contrast (${context})")
                               "Compare/Contrast"))
               (front-text concepts)
               (back-text comparisons-contrasts))
          (cashpw/anki-to-fc--normal heading-text
                                     review-data
                                     front
                                     back
                                     ;; extra
                                     nil
                                     source)))))

  (defun cashpw/anki-to-fc--definition (heading-text data anki-note-id)
    (let* ((fields (cashpw/anki-to-fc--get-fields anki-note-id))
           (review-data (mapcar
                         (lambda (datum)
                           (list (if (= 0 (plist-get datum :position))
                                     "back"
                                   "front")
                                 (plist-get datum :ease)
                                 (plist-get datum :box)
                                 (plist-get datum :interval)
                                 (plist-get datum :due)))
                         data)))
      (cl-destructuring-bind (term-val context definition extra source) fields
        (let* ((term-val (cashpw/anki-to-fc--convert-text term-val))
               (context (if (> (length context) 0)
                            (cashpw/anki-to-fc--convert-text context)
                          nil))
               (definition (if (> (length definition) 0)
                               (cashpw/anki-to-fc--convert-text definition)
                             nil))
               (extra (if (> (length extra) 0)
                          (cashpw/anki-to-fc--convert-text extra)
                        nil))
               (source (if (> (length source) 0)
                           source
                         ;; "TODO: Source"
                         nil))
               (heading-text (if context
                                 (s-lex-format "Definition (${context})")
                               "Definition"))
               (front term-val)
               (back definition))
          (cashpw/anki-to-fc--double heading-text
                                     review-data
                                     front
                                     back
                                     extra
                                     source)))))

  (defun cashpw/anki-to-fc--denotes (heading-text data anki-note-id)
    (let* ((review-data (mapcar (lambda (datum)
                                  (list (plist-get datum :position)
                                        (plist-get datum :ease)
                                        (plist-get datum :box)
                                        (plist-get datum :interval)
                                        (plist-get datum :due)))
                                data))
           (fc-cloze-max (number-to-string
                          (length review-data)))
           (fields (cashpw/anki-to-fc--get-fields anki-note-id)))
      (cl-destructuring-bind (symbol-1 symbol-2 symbol-3 symbol-4 context description extra source) fields
        (let* ((symbol-1 (cashpw/anki-to-fc--convert-text symbol-1))
               (symbol-2 (if (> (length symbol-2) 0)
                             (cashpw/anki-to-fc--convert-text symbol-2)
                           nil))
               (symbol-3 (if (> (length symbol-3) 0)
                             (cashpw/anki-to-fc--convert-text symbol-3)
                           nil))
               (symbol-4 (if (> (length symbol-4) 0)
                             (cashpw/anki-to-fc--convert-text symbol-4)
                           nil))
               (context (if (> (length context) 0)
                            (cashpw/anki-to-fc--convert-text context)
                          nil))
               (description (if (> (length description) 0)
                                (cashpw/anki-to-fc--convert-text description)
                              nil))
               (extra (if (> (length extra) 0)
                          (cashpw/anki-to-fc--convert-text extra)
                        nil))
               (source (if (> (length source) 0)
                           (cashpw/anki-to-fc--convert-text source)
                         nil))
               (heading-text (if context
                                 (s-lex-format "Denotes (${context})")
                               "Denotes"))
               (body
                (concat (s-lex-format "- {{${symbol-1}}@0}\n")
                        (if symbol-2 (s-lex-format "- {{${symbol-2}}@1}\n") "")
                        (if symbol-3 (s-lex-format "- {{${symbol-3}}@2}\n") "")
                        (if symbol-4 (s-lex-format "- {{${symbol-4}}@3}\n") "")
                        (s-lex-format "\n${description}"))))
          (cashpw/anki-to-fc--cloze heading-text
                                    review-data
                                    body
                                    extra
                                    source)))))

  (defun cashpw/anki-to-fc--aka (heading-text data anki-note-id)
    (let* ((review-data (mapcar
                         (lambda (datum)
                           (list (plist-get datum :position)
                                 (plist-get datum :ease)
                                 (plist-get datum :box)
                                 (plist-get datum :interval)
                                 (plist-get datum :due)))
                         data))
           (fc-cloze-max (number-to-string
                          (length review-data)))
           (fields (cashpw/anki-to-fc--get-fields anki-note-id)))
      (cl-destructuring-bind (term-1 term-2 term-3 term-4 term-5 term-6 context extra source) fields
        (let* ((term-1 (cashpw/anki-to-fc--convert-text term-1))
               (term-2 (if (> (length term-2) 0)
                           (cashpw/anki-to-fc--convert-text term-2)
                         nil))
               (term-3 (if (> (length term-3) 0)
                           (cashpw/anki-to-fc--convert-text term-3)
                         nil))
               (term-4 (if (> (length term-4) 0)
                           (cashpw/anki-to-fc--convert-text term-4)
                         nil))
               (term-5 (if (> (length term-5) 0)
                           (cashpw/anki-to-fc--convert-text term-5)
                         nil))
               (term-6 (if (> (length term-6) 0)
                           (cashpw/anki-to-fc--convert-text term-6)
                         nil))
               (context (if (> (length context) 0)
                            (cashpw/anki-to-fc--convert-text context)
                          nil))
               (extra (if (> (length extra) 0)
                          (cashpw/anki-to-fc--convert-text extra)
                        nil))
               (source (if (> (length source) 0)
                           (cashpw/anki-to-fc--convert-text source)
                         nil))
               (heading-text (if context
                                 (s-lex-format "AKA (${context})")
                               "AKA"))
               (body
                (concat (s-lex-format "- {{${term-1}}@0}\n")
                        (if term-2 (s-lex-format "- {{${term-2}}@1}\n") "")
                        (if term-3 (s-lex-format "- {{${term-3}}@2}\n") "")
                        (if term-4 (s-lex-format "- {{${term-4}}@3}\n") "")
                        (if term-5 (s-lex-format "- {{${term-5}}@4}\n") "")
                        (if term-6 (s-lex-format "- {{${term-6}}@5}\n") ""))
                ))
          (cashpw/anki-to-fc--cloze heading-text
                                    review-data
                                    body
                                    extra
                                    source)))))

  (defun cashpw/anki-to-fc--convert-text (text)
    (cashpw/anki-to-fc--convert-cloze
     (cashpw/anki-to-fc--convert-roam-link text)))

  (defun cashpw/anki-to-fc--convert-roam-link (text)
    (replace-regexp-in-string "<a href=\".*?\\#ID-\\(.*?\\)\">\\(.*?\\)<\\/a>"
                              "[[\\1][\\2]]"
                              text))

  (defun cashpw/anki-to-fc--convert-latex (text)
    "LaTeX code in cloze delections can't contain a }} , to work around this limitation, insert a space between the braces.

Example: \frac{1}{\sqrt{2} }

See: https://www.leonrische.me/fc/card_types.html"
    (replace-regexp-in-string "}}" "} }" text))

  (defun cashpw/anki-to-fc--convert-cloze (cloze-text)
    (replace-regexp-in-string
     "}@\\([0-9]+\\)"
     (lambda (match)
       (cl-destructuring-bind (prefix cloze-id) (s-split "@" match)
         (concat prefix
                 "@"
                 (number-to-string
                  (1- (string-to-number
                       cloze-id))))))
     (replace-regexp-in-string
      "{{\\(.*?\\)::\\(.*?\\)}@" "{{\\1}{\\2}@"
      (replace-regexp-in-string
       "{{c\\([0-9]+\\)::\\(.*?\\)}}"
       "{{\\2}@\\1}"
       cloze-text))))

  (defun cashpw/anki-to-fc--time-to-fc-time (time)
    (format-time-string "%FT%TZ" time "UTC0"))

  (defun cashpw/anki-to-fc--get-fields (anki-note-id)
    (let* ((anki-field-separator "")
           (db "/home/cashpw/collection.anki2")
           (query (s-lex-format "select flds from notes where id=${anki-note-id}"))
           (command (s-lex-format "sqlite3 ${db} \"${query}\""))
           (fields (shell-command-to-string command)))
      (s-split anki-field-separator fields)))

  (defun cashpw/anki-to-fc--query-db (query)
    (let* ((db "/home/cashpw/collection.anki2")
           (command (s-lex-format "sqlite3 ${db} \"${query}\""))
           (results (shell-command-to-string command))
           (lines (s-split "\n"
                           results
                           'omit-nulls)))
      lines))

  (defun cashpw/anki-to-fc--get-data (note-id)
    "Get due,ivl information from the anki card at point."
    (let* ((anki-collection-creation-time
            ;; https://github.com/ankidroid/Anki-Android/wiki/Database-Structure#collection
            (seconds-to-time 1553518800))
           (query
            ;; https://github.com/ankidroid/Anki-Android/wiki/Database-Structure
            (s-lex-format "select due,ivl,factor,reps,lapses,ord,cards.id from notes inner join cards on notes.id = cards .nid where notes.id = ${note-id};"))
           (lines (cashpw/anki-to-fc--query-db query))
           ;; (anki-field-separator "")
           (initial-ease 2.5)
           (positions (mapcar
                       (lambda (line)
                         (cl-destructuring-bind (due interval factor reps lapses ordinal card-id) (s-split "|" line)
                           (let* ((due (cashpw/anki-to-fc--time-to-fc-time
                                        (time-add anki-collection-creation-time
                                                  (days-to-time (string-to-number due)))))
                                  (reps (string-to-number reps))
                                  (lapses (string-to-number lapses))
                                  (interval (string-to-number interval))
                                  (last-sm2-interval 6.0)
                                  (last-sm2-interval-index 3)
                                  (box (if (> interval last-sm2-interval)
                                           (max (- reps lapses)
                                                (1+ last-sm2-interval-index))
                                         (- reps lapses)))
                                  (factor (string-to-number factor))
                                  (ease (if (= factor 0)
                                            initial-ease
                                          (/ factor 1000.0)))
                                  (pos (string-to-number ordinal))
                                  (card-created-time (seconds-to-time (/ (string-to-number card-id)
                                                                         1000))))
                             `(:card-created-time ,card-created-time
                               :position ,pos
                               :ease ,ease
                               :box ,box
                               :interval ,interval
                               :due ,due))))
                       lines)))
      positions)))

(defgroup cashpw/contacts nil
  "Group for contacts."
  :tag "Contacts"
  :group 'org)

(defcustom cashpw/contacts--birthday-prop "BIRTHDAY"
  "Property name for a contact's birthday."
  :type 'string
  :group 'cashpw/contacts)

(defcustom cashpw/contacts--reminders-heading "Reminders"
  "Heading text for the reminders heading."
  :type 'string
  :group 'cashpw/contacts)

(defcustom cashpw/contacts--path-dir (concat cashpw/path--proj-dir "/people")
  "Directory path."
  :type 'string
  :group 'cashpw/contacts)

(defun cashpw/contacts--time-as-current-year (time)
  "Return new time equal to TIME in all but year, which is changed to the current year."
  (cl-destructuring-bind (seconds minutes hours days months years day-of-week daylight-savings-time-p utc-offset) (decode-time time)
    (let* ((current-year (nth 5 (decode-time (current-time)))))
      (encode-time seconds minutes hours days months current-year day-of-week daylight-savings-time-p utc-offset))))

(defun cashpw/contacts--get-next-annual-time (time)
  "Return time string for the next annual recurrence of TIME."
  (if (not (time-less-p time (current-time)))
      time
    (cl-destructuring-bind (seconds minutes hours days months years day-of-week daylight-savings-time-p utc-offset) (decode-time time)
      (let* ((current-year (nth 5 (decode-time (current-time))))
             (next-year (1+ current-year)))
        (encode-time seconds minutes hours days months next-year day-of-week daylight-savings-time-p utc-offset)))))

(cl-letf (((symbol-function 'current-time) (lambda ()
                                             (date-to-time "2022-10-05T08:00:00-0700"))))
  (cl-assert
   (equal
    (cashpw/contacts--get-next-annual-time (date-to-time "2022-10-10T08:00:00-0700"))
    (date-to-time "2022-10-10T08:00:00-0700"))
   "Next time should be this year (2022) because the date hasn't yet passed.")
  (cl-assert
   (equal
    (cashpw/contacts--get-next-annual-time (date-to-time "2022-10-01T08:00:00-0700"))
    (date-to-time "2023-10-01T08:00:00-0700"))
   "Next time should be next year (2023) because the date has passed.")
  (cl-assert
   (equal
    (cashpw/contacts--get-next-annual-time (date-to-time "2000-10-10T08:00:00-0700"))
    (date-to-time "2023-10-10T08:00:00-0700"))
   "Next time should be next year (2023) because the date has passed.")
  (cl-assert
   (equal
    (cashpw/contacts--get-next-annual-time (date-to-time "2000-10-01T08:00:00-0700"))
    (date-to-time "2023-10-01T08:00:00-0700"))
   "Next time should be next year (2023) because the date has passed."))

(cl-defun cashpw/contacts--has-prop-p (prop)
  "Returns nil if the contact lacks the PROP."
  (member prop
          (org-buffer-property-keys)))

(cl-defun cashpw/contacts--get-prop (prop)
  "Returns value of PROP or nil if PROP not found."
  (org-entry-get (point-min)
                 prop))

(cl-defun cashpw/contacts--list-top-level-headings ()
  "TODO"
  (org-map-entries
   (lambda ()
     (org-entry-get nil "ITEM"))
   "LEVEL=1"))

(cl-defun cashpw/contacts--heading-exists-p (heading-text)
  "Return t if HEADING-TEXT is among top-level headings and nil otherwise."
  (and (org-find-exact-headline-in-buffer
        heading-text)
       t))

(cl-defun cashpw/contacts--top-level-heading-exists? (heading-text)
  "Return t if HEADING-TEXT is among top-level headings and nil otherwise."
  (member heading-text
          (cashpw/contacts--list-top-level-headings)))

(cl-defun cashpw/contacts--list-child-headings ()
  "TODO"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-entry-get nil "ITEM"))
   nil
   'tree))

(cl-defun cashpw/contacts--create-top-level-heading-if-absent (heading-text &optional pos)
  "Creates a top-level heading with HEADING-TEXT at POS if such a heading doesn't exist in buffer.

Returns nil if the heading already existed."
  (let ((pos (or pos
                 (point-max))))
    (unless (member heading-text
                    (cashpw/contacts--list-top-level-headings))
      (goto-char pos)
      (org-insert-heading nil t t)
      (insert heading-text))))

(cl-defun cashpw/contacts--goto-heading (heading-text)
  "Move pointer to the heading with HEADING-TEXT.

Does nothing if such a heading is absent."
  (let ((heading-position (org-find-exact-headline-in-buffer
                           heading-text)))
    (when heading-position
      (goto-char heading-position))))

(defun cashpw/org-set-property--created-at (&optional time)
  "Set the CREATED_AT property for the current heading.

Time defaults to `(current-time)'."
  (let ((created-at-time (or time
                             (current-time))))
    (org-set-property "CREATED_AT"
                      (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                                          created-at-time))))
(defun cashpw/contacts-create-reminder (reminder-text &optional time repeater-interval)
  "Creates a reminder."
  (interactive "sReminder heading: ")
  (let* ((time-format-string (if repeater-interval
                                 (s-lex-format "<%F ${repeater-interval}>")
                               (s-lex-format "<%F>")))
         (time (or time
                   (org-read-date nil t)))
         (time-string (format-time-string time-format-string
                                          time)))
    (cashpw/contacts--create-top-level-heading-if-absent cashpw/contacts--reminders-heading)
    (cashpw/contacts--goto-heading cashpw/contacts--reminders-heading)
    (org-insert-todo-subheading nil)
    (insert reminder-text)
    (org-entry-put nil "SCHEDULED" time-string)
    (cashpw/org-set-property--created-at)))

(cl-defun cashpw/contacts-file-p ()
  "Contacts files are roam files in a specific directory."
  (and (org-roam-file-p)
       (file-in-directory-p buffer-file-name
                            cashpw/contacts--path-dir)))

(defun cashpw/contacts--get-birthday-time ()
  "Get emacs time representation of the contact's birthday."
  (org-time-string-to-time
   (cashpw/contacts--get-prop
    cashpw/contacts--birthday-prop)))

(cl-defun cashpw/contacts-create-birthday-reminders (&optional advance-notice-days)
  "Create the following birthday reminders:

1. Annually on the person's birthday
2. Annually ADVANCE-NOTICE-DAYS before the person's birthday"
  (interactive)
  (when (and (cashpw/contacts-file-p)
             (cashpw/contacts--has-prop-p cashpw/contacts--birthday-prop))
    (let* ((birth-time (cashpw/contacts--get-birthday-time))
           (contact-name (cashpw/contacts--get-name))
           (birthday-heading-text (s-lex-format
                                   "${contact-name}'s birthday"))
           (advance-notice-days (or advance-notice-days
                                    30))
           (upcoming-birthday-heading-text (s-lex-format
                                            "${contact-name}'s birthday in ${advance-notice-days} days")))
      (unless (cashpw/contacts--heading-exists-p upcoming-birthday-heading-text)
        (let* ((reminder-time (cashpw/contacts--get-next-annual-time
                               (time-subtract birth-time
                                              (days-to-time
                                               advance-notice-days)))))
          (cashpw/contacts-create-reminder upcoming-birthday-heading-text
                                           reminder-time
                                           "++1y")))

      (unless (cashpw/contacts--heading-exists-p birthday-heading-text)
        (let* ((reminder-time (cashpw/contacts--get-next-annual-time
                               birth-time)))
          (cashpw/contacts-create-reminder birthday-heading-text
                                           reminder-time
                                           "++1y"))))))

(cl-defun cashpw/contacts--get-name (&optional path)
  "Return name of contact at PATH."
  (let ((path (or path
                  (buffer-file-name (buffer-base-buffer)))))
    (when path
      (with-current-buffer (get-file-buffer path)
        (pcase
            (org-collect-keywords '("TITLE"))
          (`(("TITLE" . ,val))
           (car val)))))))

(defun cashpw/contacts-aniversaries (contact-file-directory &optional field)
  "Compute FIELD anniversaries for each contact.

Based on `org-contacts-anniversaries'."
  (let ((field (or field
                   cashpw/contacts-field-birthday))
        (contact-files
         (org-roam--list-files
          (expand-file-name
           contact-file-directory))))
    ;; (cl-loop for file in contact-files
    ;;       for anniversary = (let ((anniversary
    ;;                                ))))
    ))

(defun cashpw/contacts--get-contacts ()
  (let ((org-roam-directory "~/proj/people")
        (org-roam-db-location "~/proj/people/org-roam.db"))
    (when (emacsql-live-p
           (org-roam-db--get-connection))
      (emacsql-close
       (org-roam-db--get-connection)))
    (org-roam-db)
    (org-roam-db-query [:select *
                        :from nodes])))

(use-package! clocktable-by-category
  :after org)

(use-package! clocktable-by-tag
  :after org)

(use-package! doct
  :commands (doct))

(use-package! org-special-block-extras
  :after org
  :hook (org-mode . org-special-block-extras-mode)
  :custom
  (o-docs-libraries
   '("~/org-special-block-extras/documentation.org")
   "The places where I keep my ‘#+documentation’")
  (org-defblock hugogallery
                (editor "Editor HugoGallery") ()
                "Docstring"
                (if (not (equal backend 'hugo))
                    contents
                  (format "{{< gallery >}}%s{{< /gallery >}}"
                          (replace-regexp-in-string ":class:class"
                                                    ":class"
                                                    (replace-regexp-in-string "\\(attr_html: \\(.*:class\\)?\\)"
                                                                              "\\1:class hugo-gallery-image "
                                                                              contents))))))

;; (use-package! ol-doi
;;  :after org)

(use-package! orgtbl-aggregate)

;; (use-package! org-ai
;;   :after org
;;   :commands (org-ai-mode)
;;   :init
;;   (add-hook 'org-mode-hook #'org-ai-mode)
;;   :custom
;;   (org-ai-openai-api-token (cashpw/secrets-get "openai")))

(after! (:and oc
              memoize))
  ;; Speed up exports
  ;; (memoize 'citeproc-hash-itemgetter-from-any))

(use-package! org-download
  :after org
  :custom
  (org-download-heading-lvl nil))

(after! org-habit
  (setq
    org-habit-show-done-always-green t))

;; (remove-hook! 'org-mode-hook #'org-fancy-priorities-mode)

(defvar cashpw/org-fc--card-timer
  nil
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
  (ignore-errors
    (doom/reset-font-size)))

(defun cashpw/org-fc--before-review ()
  (setq
   org-format-latex-options '(:foreground default
                              :background default
                              :scale 5.0
                              :html-foreground "Black"
                              :html-background "Transparent"
                              :html-scale 1.0
                              :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (ignore-errors
    (doom/reset-font-size))
  (setq
   org-image-actual-width 1200)
  (global-hide-mode-line-mode)
  (doom/increase-font-size 2))

(defun cashpw/org-fc--before-setup ()
  (cashpw/org-fc--reset-card-timer-expired-effects)
  (setq
   cashpw/org-fc--card-timer (run-with-timer cashpw/org-fc--seconds-per-card
                                             nil
                                             #'cashpw/org-fc--handle-card-timer-expired)))

(defun cashpw/org-fc--after-review ()
  (cashpw/org-fc--reset-card-timer-expired-effects)
  (setq
   org-format-latex-options '(:foreground default
                              :background default
                              :scale 1.5
                              :html-foreground "Black"
                              :html-background "Transparent"
                              :html-scale 1.0
                              :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
   org-image-actual-width nil)
  (global-hide-mode-line-mode -1)
  (ignore-errors
    (doom/reset-font-size)))

(defun cashpw/org-fc--after-flip ()
  (let ((current-position (oref org-fc-review--session current-item)))
    (if (org-fc-position--new-p current-position)
        (cl-incf cashpw/org-fc-review-new-limit--new-seen-today)))
  (evil-open-fold-rec)
  (cancel-timer cashpw/org-fc--card-timer)
  (org-map-entries (lambda ()
                     (org-latex-preview 4))
                   ;; match
                   nil
                   ;; scope
                   'tree))

(defun cashpw/org-fc-review-all ()
  "Review everything except reading flashcards."
  (interactive)
  (org-fc-cache-mode)
  (org-fc-review '(:paths all
                   :filter (not (tag "reading")))))

(defun cashpw/org-fc-review-skip-card ()
  "Skip card and proceed to next. Based on `org-fc-review-suspend-card'."
  (interactive)
  (org-fc-review-reset)
  (org-fc-review-session--next org-fc-review--session))

(use-package! org-fc
  :after org
  :custom
  (org-fc-directories `(,(s-lex-format "${cashpw/path--home-dir}/proj/notes")
                        ,(s-lex-format "${cashpw/path--home-dir}/proj/people")
                        ,(s-lex-format "${cashpw/path--home-dir}/proj/notes-personal")
                        ,(s-lex-format "${cashpw/path--home-dir}/proj/notes-private")))
  (org-fc-review-history-file (s-lex-format "${cashpw/path--home-dir}/.config/org-fc/org-fc-reviews.tsv"))
  (org-fc-bury-siblings t)
  (org-fc-bury-siblings t)
  (org-fc-algo-sm2-intervals '(0.0 1.0 2.0 6.0))
  (org-fc-review-new-limit 20)
  (org-fc-review-new-limit-schedule 'day)
  (org-fc-review-hide-title-in-header-line t)
  ;; Define twice so the keys show up in the hint
  ;; See https://www.leonrische.me/fc/use_with_evil-mode.html
  (org-fc-review-flip-mode-map (let ((map (make-sparse-keymap)))
                                 (define-key map (kbd "n") 'org-fc-review-flip)
                                 (define-key map (kbd "q") 'org-fc-review-quit)
                                 (define-key map (kbd "e") 'org-fc-review-edit)
                                 (define-key map (kbd "p") 'cashpw/org-fc-review-pause)
                                 (define-key map (kbd "s") 'cashpw/org-fc-review-skip-card)
                                 (define-key map (kbd "S") 'org-fc-review-suspend-card)
                                 map))
  (org-fc-review-rate-mode-map (let ((map (make-sparse-keymap)))
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

  (setq
   cashpw/org-fc--seconds-per-card 10)

  (add-to-list 'org-fc-custom-contexts
               '(reading-list . (:filter (tag "reading"))))
  (add-to-list 'org-fc-custom-contexts
               '(not-reading-list . (:filter (not (tag "reading")))))

  ;; Define twice so the keys show up in the hint
  ;; See https://www.leonrische.me/fc/use_with_evil-mode.html
  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
    (kbd "n") 'org-fc-review-flip
    (kbd "s") 'cashpw/org-fc-review-skip-card
    (kbd "S") 'org-fc-review-suspend-card
    (kbd "e") 'org-fc-review-edit
    (kbd "p") 'cashpw/org-fc-review-pause
    (kbd "q") 'org-fc-review-quit)
  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
    (kbd "0") 'org-fc-review-rate-again
    (kbd "1") 'org-fc-review-rate-hard
    (kbd "2") 'org-fc-review-rate-good
    (kbd "3") 'org-fc-review-rate-easy
    (kbd "s") 'cashpw/org-fc-review-skip-card
    (kbd "S") 'org-fc-review-suspend-card
    (kbd "e") 'org-fc-review-edit
    (kbd "q") 'org-fc-review-quit)
  (add-hook! 'org-fc-review-edit-mode-hook
             #'cashpw/org-fc--reset-card-timer-expired-effects)
  (add-hook! 'org-fc-before-setup-hook
             #'cashpw/org-fc--before-setup)
  (add-hook! 'org-fc-after-flip-hook
             #'cashpw/org-fc--after-flip)
  (add-hook! 'org-fc-before-review-hook
             #'cashpw/org-fc--before-review)
  (add-hook! 'org-fc-after-review-hook
             #'cashpw/org-fc--after-review)
  (setq
   org-fc-review-position-filters '())
  ;; (setq
  ;;  org-fc-review-position-filters '(cashpw/org-fc--filter-one-per-file
  ;;                                   cashpw/org-fc--filter-limit-implement
  ;;                                   cashpw/org-fc--filter-limit-new))

  (setq org-roam-db-node-include-function (lambda ()
                                            ;; Exclude org-fc cards from roam
                                            (not (org-fc-entry-p))))
  )

(use-package! org-fc-type-vocab
  :after org-fc)

(defcustom cashpw/org-fc--one-per-file-exceptions
  '("Italian")
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

(cl-defmethod cashpw/org-fc--filter-limit-implement ((positions list))
  "Return nil to remove the POSITIONS (`org-fc-position's) from the review list."
  (let ((implement-position-limit 1)
        (implement-position-count 0))
    (--filter
     (let ((tags (oref (oref it card) tags)))
       (if (member "implement" tags)
           (if (= implement-position-count implement-position-limit)
               nil
             (cl-incf implement-position-count)
             t)
         t))
     positions)))

(after! org-fc
  (setq
   ;; org-fc-review-position-filters '()
   org-fc-review-position-filters '(cashpw/org-fc--filter-one-per-file
                                    cashpw/org-fc--filter-limit-implement
                                    cashpw/org-fc--filter-limit-new
                                    org-fc-positions--filter-blocked)
   ))

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
      (while (not (equal (org-up-heading-safe)
                         1)))
      (org-narrow-to-subtree)
      (outline-hide-subtree))
    ;; Show only the ancestors of the current card
    (org-show-set-visibility org-fc-narrow-visibility)
    (if (member "noheading" tags) (org-fc-hide-heading))))

(use-package! org-tempo)

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

(defun cashpw/org-gcal--set-scheduled (_calendar-id event _update-mode)
  "See `org-gcal-after-update-entry-functions'."
  (org-schedule nil (cashpw/org-gcal--timestamp-from-event event)))

(defun cashpw/org-gcal--set-effort (_calendar-id event _update-mode)
  "Set Effort property based on EVENT if not already set.

Reference: https://github.com/kidd/org-gcal.el/issues/150#issuecomment-825837044"
  (when-let* ((start-time (plist-get (plist-get event :start)
                           :dateTime))
              (end-time (plist-get (plist-get event :end)
                                :dateTime))
              (diff (float-time
                     (time-subtract (org-gcal--parse-calendar-time-string end-time)
                                    (org-gcal--parse-calendar-time-string start-time))))
              (minutes (floor (/ diff 60))))
    (let ((effort (org-entry-get (point) org-effort-property)))
      (unless effort
        (message "need to set effort - minutes %S" minutes)
        (org-entry-put (point)
                       org-effort-property
                       (apply #'format "%d:%02d" (cl-floor minutes 60)))))))

(after! org-gcal
  (add-hook 'org-gcal-after-update-entry-functions
            #'cashpw/org-gcal--set-effort)
  ;; (add-hook 'org-gcal-after-update-entry-functions
  ;;           #'cashpw/org-gcal--set-scheduled)
  )

(defun cashpw/org-gcal--event-declined-p (event)
  "Return non-nil if we've declined to attend EVENT."
  (let ((attendees (append (plist-get event :attendees)
                           nil)))
    (--any (and (plist-get it :self)
                (s-equals-p "declined"
                            (plist-get it :responseStatus)))
           attendees)))

(defcustom cashpw/org-gcal--summaries-to-exclude '("Nap"
                                                   "Email, etc"
                                                   "Lunch"
                                                   "Retrospective: Week"
                                                   ".*Work\\(ing\\)? Session.*"
                                                   "End the day"
                                                   "Focus time (ask before scheduling)"
                                                   "Meditate")
  "List of event summaries, as regexps, (titles) which should be excluded during sync/fetch."
  :type '(repeat string)
  :group 'org-gcal)

(defun cashpw/org-gcal--filter (item)
  "Return nil to exclude the result."
  (let ((summary (plist-get item :summary)))
    (if (--any (string-match it summary)
               cashpw/org-gcal--summaries-to-exclude)
        nil
      t)))

(defun cashpw/org-gcal--event-cancelled-p (event)
  "Return non-nil if EVENT is cancelled."
  (string= (plist-get event :status) "cancelled"))

(defun cashpw/org-gcal--get-secret-key ()
  "Get the secret key for org-gcal."
  (let ((path-to-secret (s-lex-format "${cashpw/path--home-dir}/.org-gcal-secret-key")))
    (car (split-string
          (with-temp-buffer
            (insert-file-contents path-to-secret)
            (buffer-string))))))

;; (defun cashpw/org-gcal--is-1-on-1 (pom)
;;   "Return non-nil if the event at POM is a 1:1.

;; 1:1's are named like \"Cash/Foo\" or \"Foo/Cash\"."
;;   (let ((summary (org-entry-get pom "ITEM")))
;;     (or (string-match "Cash\s*\\/" summary)
;;         (string-match "\\/\s*Cash" summary))))

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

;; (defun cashpw/org-gcal-create-1-on-1-prep-todos ()
;;   "Create TODO entries in current buffer for each 1:1."
;;   (interactive)
;;   (let ((1-on-1s '()))
;;     (org-map-entries (lambda ()
;;                        (when (cashpw/org-gcal--is-1-on-1 (point))
;;                          (let (
;;                                (schedule-time (cashpw/org-gcal--get-schedule-time (point)))
;;                                (title (org-entry-get (point)
;;                                                      "ITEM")))
;;                            (add-to-list '1-on-1s
;;                                         `(:time ,schedule-time
;;                                           :title ,title)))))
;;                      nil)
;;     (cl-dolist (1-on-1 1-on-1s)
;;       (cl-destructuring-bind (&key time title &allow-other-keys) 1-on-1
;;         (let ((heading-title (s-lex-format "Prepare for: ${title}"))
;;               (schedule-time (org-time-subtract time
;;                                                 (days-to-time (cond
;;                                                                ((cashpw/is-monday time)
;;                                                                 3)
;;                                                                (t
;;                                                                 1))))))
;;           (save-excursion
;;             (goto-char (point-max))
;;             (org-insert-heading nil t t)
;;             (insert heading-title)
;;             (org-todo "TODO")
;;             (org-priority 2)
;;             (org-set-property "Effort" "5m")
;;             (org-schedule nil
;;                           (format-time-string "%Y-%m-%d"
;;                                               schedule-time))))))))

(after! org-gcal
  :config
  (setq
   plstore-cache-passphrase-for-symmetric-encryption t
   org-gcal-up-days 1
   org-gcal-down-days 7
   org-gcal-remove-cancelled-events nil
   org-gcal-remove-events-with-cancelled-todo nil
   ;; See https://github.com/kidd/org-gcal.el/issues/172
   org-gcal-auto-archive nil
   org-gcal-recurring-events-mode 'top-level)
  (add-to-list 'org-gcal-fetch-event-filters
               #'cashpw/org-gcal--filter)
  (defun org-gcal--event-cancelled-p (event)
    "Has EVENT been cancelled?"
    (or (cashpw/org-gcal--event-declined-p event)
        (cashpw/org-gcal--event-cancelled-p event))))

(defun cashpw/org-gcal--attendees-include (attendees regexp)
  "Return non-nil if at least one email in ATTENDEES matches REGEXP."
  (let ((attendees (append attendees nil)))
    (--any (let ((email (plist-get it :email)))
             (string-match regexp email))
           attendees)))

(cl-assert (cashpw/org-gcal--attendees-include '[(:email "foo1@bar.com")
                                                 (:email "foo2@bar.com")]
                                               "foo1@bar.com")
           t
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

(defun cashpw/org-gcal--start (event)
  "Return the scheduled start time for EVENT.

Reference: `org-gcal--update-entry'."
  (let* ((start-time (plist-get (plist-get event :start) :dateTime)))
    (parse-iso8601-time-string start-time)))

(defun cashpw/org-gcal--end (event)
  "Return the scheduled end time for EVENT.

Reference: `org-gcal--update-entry'."
  (let* ((end-time (plist-get (plist-get event :end) :dateTime)))
    (parse-iso8601-time-string end-time)))

(defun cashpw/org-gcal--maybe-create-1-on-1-prep (_calendar-id event _update-mode)
  "Insert a 1-on-1 prep heading todo if EVENT is for a 1-on-1 event."
  (when (and (sequencep event)
             (cashpw/org-gcal--is-1-on-1 event))
    ;; No need to call `save-excursion' as this function is called from within it.
    (let* ((event-summary (plist-get event :summary))
           (event-start-time (cashpw/org-gcal--start event))
           (prepare-time (org-time-subtract event-start-time
                                            (days-to-time (cond
                                                           ((cashpw/is-monday event-start-time)
                                                            3)
                                                           (t
                                                            1))))))
      ;; Create 1-on-1 prep TODO if we still have time to prepare (i.e. the 1-on-1 is tomorrow or later).
      (when (time-less-p (current-time) prepare-time)
        (org-insert-todo-heading-respect-content)
        (insert (s-lex-format "Prepare: ${event-summary}"))
        (org-priority 2)
        (org-set-property "Effort" "5m")
        (org-schedule nil (format-time-string "%F" prepare-time))
        ))))

(after! org-gcal
  (add-hook 'org-gcal-after-update-entry-functions
            #'cashpw/org-gcal--maybe-create-1-on-1-prep))

(defun cashpw/org-gcal--maybe-create-todo-extract-reminder (_calendar-id event _update-mode)
  "Insert a 1-on-1 prep heading todo if EVENT is for a 1-on-1 event."
  (when (and (sequencep event)
             ;; (cashpw/org-gcal--is-1-on-1 event)
             )
    ;; No need to call `save-excursion' as this function is called from within it.
    (let* ((event-summary (plist-get event :summary))
           (event-end-time (cashpw/org-gcal--end event)))
      (org-insert-todo-heading-respect-content)
      (insert (s-lex-format "Extract TODOs: ${event-summary}"))
      (org-priority 2)
      (org-set-property "Effort" "5m")
      (org-schedule nil (format-time-string "%F %H:%M" event-end-time)))))

(after! org-gcal
  (add-hook 'org-gcal-after-update-entry-functions
            #'cashpw/org-gcal--maybe-create-todo-extract-reminder))

(use-package! org-multi-clock)

(use-package! org-mime)

(use-package! ol-notmuch
  :after org)

(use-package! org-ql)

(use-package! vulpea)

(use-package! doct-org-roam
  :after doct)

(use-package! org-roam
  :after org)

(use-package! summarize-agenda-time
  :after org)

(when (not (cashpw/is-work-cloudtop-p))
  (use-package! ox-hugo
    :after ox))

(use-package! org-protocol
  :config
  (setq
   org-protocol-default-template-key "p"
   ;; cashpw/org-protocol--capture-template--protocol '("p" "Protocol" entry (file+headline "/tmp/notes.org" "Inbox")
   ;;                                                   "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
   ;; cashpw/org-protocol--capture-template--protocol-link '("L" "Protocol Link" entry (file+headline "/tmp/notes.org" "Inbox")
   ;;                                                        "* %? [[%:link][%:description]] \nCaptured On: %U")
   cashpw/org-protocol--capture-template--web-site '("Website"
                                                     :keys "w"
                                                     :file ""
                                                     :template "* %a :website:\n\n%U %?\n\n%:initial")
   ;; '("w" "Web site" entry (file "") "* %a :website:\n\n%U %?\n\n%:initial")
   ))

(use-package! org-protocol-capture-html
  ;; see https://github.com/alphapapa/org-protocol-capture-html for usage
  :after org-protocol)

(use-package! org-vcard)

(defun cashpw/org-mode--set-created (&optional time)
  "Set the 'Created' property to now, or TIME if present."
  (let ((time (or time
                  (current-time))))
    (org-set-property "Created"
                      (format-time-string "[%F %a %H:%M]"
                                          time))))

(defun cashpw/org-mode--heading-text-for-today (&optinoal time-in-heading include-all-tags)
  "Return the heading text for today as a string."
  (let* ((time-in-heading (or time-in-heading
                              nil))
         (include-all-tags (or include-all-tags
                               nil))
         (today-week-number (format-time-string "%W"))
         (today-quarter-number (format-time-string "%q"))
         (today-year (format-time-string "%Y"))
         (today-month-number (format-time-string "%m"))
         (today-day-number (format-time-string "%d"))
         (today-weekday-abbreviated-name (format-time-string "%a"))
         (tags (if include-all-tags
                   (s-lex-format
                    ":${today-year}:${today-year}week${today-week-number}:${today-year}Q${today-quarter-number}:")
                 ""))
         (hh-mm (if time-in-heading
                    (concat " " (format-time-string "%H:%M"))
                  ""))
         (yyyy-mm-dd (format-time-string "%F")))
    (s-lex-format
     "[${yyyy-mm-dd} ${today-weekday-abbreviated-name}${hh-mm}] ${tags}")))

(defun cashpw/org-mode-insert-heading-for-today (&optional top time-in-heading include-all-tags)
  "Insert a heading for today's date, with relevant tags."
  (interactive)
  (let ((heading-text
         (cashpw/org-mode--heading-text-for-today
          ;; top
          nil
          time-in-heading
          include-all-tags))
        (yyyy-mm-dd (format-time-string "%Y-%m-%d"))
        (hh-mm (format-time-string "%H:%M"))
        (weekday-abbreviated-name (format-time-string "%a")))
    (if top
        (org-insert-heading nil t t)
      (org-insert-heading-respect-content))
    (insert heading-text)
    (cashpw/org-mode--set-created)))

(defun cashpw/org-mode-heading-marker-for-today ()
  "Return t if a heading for today exists.

Refer to `cashpw/org-mode-insert-heading-for-today'."
  (let ((headline-text
         (cashpw/org-mode--heading-text-for-today))
        (headline-marker
         (org-find-exact-headline-in-buffer
          headline-text)))
    headline-marker))

(defun iso-week-to-time(year week day)
  "Convert ISO year, week, day to elisp time value.

Reference: https://emacs.stackexchange.com/a/43985"
  (apply #'encode-time
         (append '(0 0 0)
                 (-select-by-indices
                  '(1 0 2)
                  (calendar-gregorian-from-absolute (calendar-iso-to-absolute
                                                     (list week day year)))))))

(defun iso-beginning-of-week(year week)
  "Convert ISO year, week to elisp time for first day (Monday) of week.

Reference: https://emacs.stackexchange.com/a/43985"
  (iso-week-to-time year week 1))

(defun iso-end-of-week(year week)
  "Convert ISO year, week to elisp time for last day (Sunday) of week.

Reference: https://emacs.stackexchange.com/a/43985"
  (iso-week-to-time year week 7))

(defun cashpw/org-mode-insert-heading-with-time (heading-text start-date &optional end-date)
  "Insert a heading for a span of time."
  (interactive)
  (org-insert-heading-respect-content)
  (insert heading-text)
  (newline)
  (if end-date
      (insert
       (concat
        (format-time-string "[%Y-%m-%d %a]--" start-date)
        (format-time-string "[%Y-%m-%d %a]" end-date)))
    (insert
     (format-time-string "[%Y-%m-%d %a]" start-date))))

(defun cashpw/org-mode-insert-heading-for-this-week (&optional include-all-tags)
  "Insert a heading for this week, with relevant tags."
  (interactive)
  (let* ((include-all-tags (or include-all-tags
                               nil))
         (today-week-number (format-time-string "%W"))
         (today-quarter-number (format-time-string "%q"))
         (today-year (format-time-string "%Y"))
         (beginning-of-week (iso-beginning-of-week
                             (string-to-number today-year)
                             (string-to-number today-week-number)))
         (end-of-week (iso-end-of-week
                       (string-to-number today-year)
                       (string-to-number today-week-number)))
         (tags (if include-all-tags
                   (s-lex-format
                    ":${today-year}week${today-week-number}:${today-year}Q${today-quarter-number}:")
                 (s-lex-format
                  ":${today-year}week${today-week-number}:")))
         (heading-text
          (s-lex-format
           "${today-year} Week ${today-week-number} ${tags}")))
    (cashpw/org-mode-insert-heading-with-time
     heading-text
     beginning-of-week
     end-of-week)))

(defun cashpw/org-mode-insert-heading-for-today-log ()
  "Insert a heading for today's date formatted for the log file."
  (interactive)
  (let* ((today-year
          (format-time-string
           "%Y"))
         (today-month-number
          (format-time-string
           "%m"))
         (today-day-number
          (format-time-string
           "%d"))
         (today-YYYY-MM-DD
          (s-lex-format
           "${today-year}-${today-month-number}-${today-day-number}")
          ))
    (cashpw/org-mode-insert-heading-for-today)
    (org-insert-subheading
     nil)
    (insert
     (s-lex-format
      "[${today-YYYY-MM-DD} 08:00-09:00]"))
    (cl-loop for (start . end) in '(("09:00" . "10:00")
                                    ("10:00" . "11:00")
                                    ("11:00" . "12:00")
                                    ("14:00" . "15:00")
                                    ("15:00" . "16:00")
                                    ("16:00" . "17:00"))
             do
             (org-insert-heading
              nil)
             (insert
              (s-lex-format
               "[${today-YYYY-MM-DD} ${start}-${end}]")))))

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

(defun cashpw/org--schedule-today-at (start-time-as-string)
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

(defun cashpw/org--schedule-for (start-time end-time &optional date)
  (let ((date (or date "today")))
    (org-schedule nil (format "%s %s-%s"
                              date
                              start-time
                              end-time))))
    ;(org-schedule nil (format "%s %s-%s"
                              ;date
                              ;start-time
                              ;end-time))))

(defun cashpw/org--schedule-at-for-minutes (start-minute start-hour duration-in-minutes &optional date)
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
 cashpw/-schedule-pomodoro-one '(:start "09:00" :end "09:50")
 cashpw/-schedule-pomodoro-two '(:start "10:00" :end "10:50")
 cashpw/-schedule-pomodoro-three '(:start "11:00" :end "11:50")
 cashpw/-schedule-pomodoro-four '(:start "12:00" :end "12:50")
 cashpw/-schedule-pomodoro-five '(:start "13:00" :end "13:50")
 cashpw/-schedule-pomodoro-six '(:start "14:00" :end "14:50")
 cashpw/-schedule-pomodoro-seven '(:start "15:00" :end "15:50")
 cashpw/-schedule-pomodoro-eight '(:start "16:00" :end "16:50")
 cashpw/-schedule-pomodoro-nine '(:start "17:00" :end "17:50")
 cashpw/-schedule-pomodoro-ten '(:start "18:00" :end "18:50"))

(defun cashpw/org-schedule-at-pomodoro (pomodoro-time &optional date)
  (interactive)
  (let ((start-time (plist-get pomodoro-time :start)))
        (date (or date "today")))
    (org-schedule nil (format "%s %s"
                              date
                              start-time)))

(defun cashpw/org-schedule-in-n-hours (offset-hours &optional date)
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

(defun cashpw/org-schedule-in-n-workdays (num-days &optional time)
  (interactive)
  (let*
      ((time (or time "09:00"))
       (offset-days))
    (org-schedule
     nil
     (format "%s %s"
             offset-days
             time))))

(defun cashpw/org-get-timestamps-in-time-order ()
  "Return a list of timestamps from the current buffer in time order."
  (cl-sort
   (org-element-map
       (org-element-parse-buffer)
       'timestamp
     (lambda (timestamp)
       `(,(org-element-property :raw-value timestamp) . ,(org-element-property :begin timestamp))))
   'org-time>
   :key 'car))

(defun cashpw/org-goto-most-recent-timestamp ()
  "`goto-char' the most recent timestamp in the current buffer."
  (interactive)
  (let ((timestamps
         (cashpw/org-get-timestamps-in-time-order)))
    (goto-char
     (cdr
      (pop timestamps)))))

(defun cashpw/org-goto-most-recent-timestamp-with-property (property)
  "`goto-char' the most recent timestamp in the current buffer with a non-nil value for the provided property."
  (interactive)
  (let ((timestamps
         (cashpw/org-get-timestamps-in-time-order)))
    (goto-char
     (cdr
      (pop timestamps)))
    (while (and timestamps
                (not
                 (org-entry-get
                  (point)
                  property)))
      (goto-char
       (cdr
        (pop timestamps))))))

(defun cashpw/org-mode-set-filetag (value)
   "Add another option; requires at least one option to already be present."
  (message "---")
  (goto-char
   (point-min))
  (if (search-forward-regexp
       "#\\+\\(FILETAGS\\|filetags\\): "
       ;; bound
       nil
       ;; noerror
       t)
      (progn
        (end-of-line)
        (insert (format "%s:" value)))
    (progn
      ;; Add filetags beneath the title; assumes there is a title
      (goto-char
       (point-min))
      (when (search-forward-regexp
          "^#\\+\\(TITLE\\|title\\):")
        (end-of-line)
        (newline)
        (cashpw/org-mode-insert-option
         "FILETAGS"
         (format ":%s:"
                 value))))))

(defun cashpw/org-mode-insert-option (option value)
  "Insert an org-mode option (#+OPTION: VALUE)."
  (insert
   (format
    "#+%s: %s\n"
    option
    value)))

(defun cashpw/org-remove-all-results-blocks ()
  "Removes all result blocks; basically an alias"
  (interactive)
  (org-babel-remove-result-one-or-many t))

(defun cashpw/org-mode--has-tag-p (tag)
  "Return t if TAG is a member of the tags of the entry at point."
  (member
   tag
   (org-get-tags)))

;;; ORG-MODE:  * My Task
;;;              SCHEDULED: <%%(diary-last-day-of-month date)>
;;; DIARY:  %%(diary-last-day-of-month date) Last Day of the Month
;;; See also:  (setq org-agenda-include-diary t)
;;; (diary-last-day-of-month '(2 28 2017))
;;; Reference: https://emacs.stackexchange.com/a/31708
(defun cashpw/diary-last-day-of-month (date)
  "Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
          (calendar-last-day-of-month month year)))
    (= day 30)
    ;; (= day last-day-of-month)
    ))

(defun cashpw/org-set-last-modified ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (org-set-property "LAST_MODIFIED"
                        (format-time-string "[%Y-%m-%d %a %H:%M]")))))

(defun cashpw/org-mode-insert-property (property value)
  "Insert an org-mode property (:PROPERTY: VALUE)."
  (insert (s-lex-format
           ":${property}: ${value}\n")))

(defun cashpw/org-get-property (property)
  "Return value of PROPERTY at point, else nil."
  (org-entry-properties (point) property))

(defun cashpw/org-mode-get-description-from-link-at-point ()
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

(defun cashpw/is-valid-priority-p (priority)
  "Return nil if the provided PRIORITY is not valid."
  (or
   (string= "0" priority)
   (string= "1" priority)
   (string= "2" priority)
   (string= "3" priority)
   (string= "4" priority)))

;; (after! flycheck
;;   :config
;;   (flycheck-reset-enabled-checker 'proselint))

(after! org
  (setq
   org-ellipsis " ▾"
   org-hide-leading-stars t))

(after! org
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        ;; showeverything to make large files open faster
        org-startup-folded 'showeverything
        org-log-into-drawer t
        org-log-repeat t))

(after! org
  :config
  (setq
   org-priority-highest 0
   org-priority-default 2
   org-priority-lowest 4))

(after! org
  :config
  (setq
   org-todo-keywords
   '((sequence
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
   org-todo-keyword-faces
   '(("[-]"  . +org-todo-active)
     ("INPROGRESS" . +org-todo-active)
     ("[?]"  . +org-todo-onhold)
     ("BLOCKED" . +org-todo-onhold)
     ("HOLD" . +org-todo-onhold)
     ("PROJ" . +org-todo-project))))

;; (after! org
;;   (add-hook!
;;    'org-after-todo-state-change-hook
;;    'save-buffer))

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
     ((cashpw/org-mode-on-inprogress--in-clock-in-file-p)
      (org-clock-in)))))

(after! org
  :config
  (setq
   cashpw/org-mode-on-inprogress--clock-in-paths (append
                                                  `(,(s-lex-format "${cashpw/path--notes-personal-dir}/journal-2024.org")
                                                    ,(s-lex-format "${cashpw/path--notes-personal-dir}/retrospective-2024.org")
                                                    ,(s-lex-format "${cashpw/path--notes-personal-dir}/todos.org"))
                                                  (f-glob "*.org"
                                                          cashpw/path--people-dir)))
  (add-hook! 'org-after-todo-state-change-hook
             'cashpw/org-mode-when-inprogress))

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
  (let ((actual-value (org-entry-get (point)
                                     cashpw/org-mode-on-done--property-name)))
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

(defcustom cashpw/org-mode-on-done--keep-file-paths (append
                                                     `(,(s-lex-format "${cashpw/path--notes-personal-dir}/journal-2024.org")
                                                       ,(s-lex-format "${cashpw/path--notes-personal-dir}/retrospective-2024.org"))
                                                     (cashpw/directory-files--org (s-lex-format "${cashpw/path--home-dir}/proj/people")))
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

(add-hook 'cashpw/org-mode-on-done--noop-hook
          #'org-get-repeat)

(add-hook 'cashpw/org-mode-on-done--delete-hook
          (lambda ()
            (string= org-state
                     "KILL")))
(add-hook
 'cashpw/org-mode-on-done--delete-hook
 'org-roam-file-p)

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
    (cond
     ((cashpw/org-mode-on-done--is-noop)
      (progn
        ;; (unless (org-get-repeat)
          ;; (org-schedule '(4)))
        (org-todo "TODO")))
     ((cashpw/org-mode-on-done--is-keep)
      nil)
     ((cashpw/org-mode-on-done--is-delete)
      (org-cut-subtree))
     (t
      (org-archive-subtree-default)))))

(after! org
  :config
  (add-hook! 'org-after-todo-state-change-hook
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
     ("i" . "ingredients")
     ("q" . "quote")
     ("s" . "src")
     ("sd" . "src dot :file TODO.png :cmdline -Kdot -Tpng")
     ("se" . "src emacs-lisp")
     ("sc" . "src C++")
     ("sp" . "src python :results output")
     ("ss" . "src sh :results output")
     ("v" . "verse"))))

(defun cashpw/org-agenda-buganizer-title ()
  "Overridden in my work config."
  "")

(defun cashpw/org-agenda-files--people ()
  "Return a list of all people files at DIR-PATH."
  (cashpw/directory-files--org
   (s-lex-format "${cashpw/path--home-dir}/proj/people")))

(defun cashpw/org-agenda-files--notes-personal ()
  "Return a list of all personal notes files at DIR-PATH."
  (cashpw/directory-files--org
   (s-lex-format "${cashpw/path--home-dir}/proj/notes-personal")))

;; TODO Speed up by pre-computing
(defun cashpw/org-agenda-files--notes-private ()
  "Return a list of all private notes files at DIR-PATH."
  (cashpw/directory-files--org
   (s-lex-format "${cashpw/path--home-dir}/proj/notes-private")))

;; TODO Speed up by pre-computing
(defun cashpw/org-agenda-files--notes ()
  "Return a list of all private notes files at DIR-PATH."
  (let* ((org-roam-directory cashpw/path--notes-dir)
         (org-roam-db-location (expand-file-name "org-roam.db"
                                                 org-roam-directory))
         (files-to-ignore `(,(s-lex-format "${org-roam-directory}/unread.org")
                            ,(s-lex-format "${org-roam-directory}/todos.org"))))
    (seq-difference (cashpw/org-roam-todo-files)
                    files-to-ignore)))

(defun cashpw/org-agenda-files--notes-private ()
  "Return a list of all private notes files at DIR-PATH."
  (let* ((org-roam-directory (s-lex-format "${cashpw/path--home-dir}/proj/notes-private"))
         (org-roam-db-location (expand-file-name
                                "org-roam.db"
                                org-roam-directory)))
    (message org-roam-db-location)
    (cashpw/org-roam-todo-files)))

(defun cashpw/org-agenda-files--update ()
  "Update `org-agenda-files' and related variables."
  (interactive)
  (setq
   cashpw/org-agenda-files--people (cashpw/org-agenda-files--people)
   cashpw/org-agenda-files--notes-personal (cashpw/org-agenda-files--notes-personal)
   org-agenda-files (append
                     cashpw/org-agenda-files--people
                     cashpw/org-agenda-files--notes-personal)))
(cashpw/org-agenda-files--update)

(use-package! org-agenda)
(use-package! evil-org-agenda)
(use-package! org-super-agenda
  :demand t
  :after (:all org-agenda
               evil
               evil-org-agenda)
  :hook ((org-agenda-mode . org-super-agenda-mode))
  :config
  (setq
   org-agenda-prefix-format '((agenda . " %i %-20(cashpw/org-agenda-category)%?-12t%-6e% s")
                              (todo . " %i %-20(cashpw/org-agenda-category) %-6e %-40(cashpw/org-agenda-buganizer-title)")
                              (tags . " %i %-12c")
                              (search . " %i %-12c"))

   org-agenda-log-mode-items '(state
                               closed
                               clock)
   org-super-agenda-header-map evil-org-agenda-mode-map
   ;; https://emacs.stackexchange.com/a/17128
   org-agenda-sorting-strategy '((agenda time-up priority-down category-keep)
                                 (todo priority-down category-keep)
                                 (tags priority-down category-keep)
                                 (search category-keep)))
  (defun cashpw/org-agenda-category ()
    (or (org-get-category)
        ""))

  (cl-defun org-super-agenda--group-dispatch-take (items (n group))
    ;;(cl-defun org-super-agenda--group-dispatch-take (items n-and-group)
    "Take N ITEMS that match selectors in GROUP.
If N is positive, take the first N items, otherwise take the last N items.
Note: the ordering of entries is not guaranteed to be preserved, so this may
not always show the expected results."
    (message (format "%s" group))
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
               (priority (or (cashpw/org--get-priority marker)
                             default-priority)))
    (s-lex-format "p${priority}")))

(defun cashpw/org--get-priority (pos)
  "Return priority of heading at POS, or nil."
  (let ((priority (org-entry-get pos
                                 "PRIORITY")))
    (if (cashpw/is-valid-priority-p priority)
        priority
      nil)))

(defun cashpw/org-super-agenda--get-first-n-from-roam-tag (n tag)
  "Return plist to group the first N headings tagged TAG.

Intended for use with `org-super-agenda-groups'."
  `(:name ,(s-lex-format "${tag} (${n})")
    :take (,take
           (:and
            (:tag ,tag)))))

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

(defun cashpw/org-agenda-files (context &optional archive)
  "Return list of agenda files for CONTEXT.

ARCHIVE should be one of `include-archive', `only-archive'.

Returns list of relevant non-archive files by default. Set WITH-ARCHIVES to non-nil to include archive files."
  (let* ((path (cond
                ((equal context 'notes)
                 cashpw/path--notes-dir)
                ((equal context 'notes-personal)
                 cashpw/path--notes-personal-dir)
                ((equal context 'notes-private)
                 cashpw/path--notes-private-dir)
                ((equal context 'people)
                 cashpw/path--people-dir)
                (t
                 "")))
         (files (f-glob "*.org"
                        path))
         (archived-files (f-glob "*.org_archive"
                                 path)))
    (cond
     ((equal archive
             'archive-and-non-archive)
      (append files
              archived-files))
     ((equal archive
             'only-archive)
      archived-files)
     (t
      files))))

(defun cashpw/update-agenda-files ()
  (setq
   org-agenda-files (append
                     (cashpw/org-agenda-files 'people
                                              'non-archive)
                     (cashpw/org-agenda-files 'notes-personal
                                              'non-archive))))

(setq
 cashpw/org-agenda-view--super `((agenda
                                  ""
                                  ((org-agenda-overriding-header "")
                                   ;; (org-agenda-files cashpw/org-agenda-files--without-roam)
                                   (org-agenda-dim-blocked-tasks nil)
                                   (org-agenda-use-tag-inheritance nil)
                                   (org-super-agenda-groups
                                    '((:name "Schedule"
                                       :time-grid t)
                                      (:name "Scheduled/Due Today"
                                       :scheduled today
                                       :deadline today)
                                      (:discard
                                       (:tag "habit"
                                        :tag "repeating"))
                                      (:name "Overdue"
                                       :scheduled past
                                       :deadline past)
                                      (;; Toss all other todos
                                       :discard
                                       (:todo t))))))
                                 (alltodo
                                  ""
                                  ((org-agenda-overriding-header "")
                                   ;; (org-agenda-files cashpw/org-agenda-files--without-roam)
                                   (org-agenda-dim-blocked-tasks nil)
                                   (org-agenda-use-tag-inheritance nil)
                                   (org-super-agenda-groups
                                    '((;; Automatically named "Log"
                                       :log t)
                                      (;; PROJ items group actionable TODOs; ignore them.
                                       :discard
                                       (:todo "PROJ"))
                                      (:name "In Progress"
                                       :todo "INPROGRESS")
                                      (:discard
                                       (:tag "repeating"))
                                      (;; Toss all tasks scheduled for or due in the past.
                                       :discard
                                       (:scheduled past
                                        :deadline past))
                                      (;; Toss all tasks scheduled for or due today.
                                       :discard
                                       (:scheduled today
                                        :deadline today))
                                      (;; Toss all tasks scheduled for or due in the future.
                                       :discard
                                       (:scheduled future
                                        :deadline future))
                                      (;; Group by priority
                                       :auto-map
                                       (lambda (item)
                                         (-when-let* ((marker
                                                       (or (get-text-property 0 'org-marker item)
                                                           (get-text-property 0 'org-hd-marker)))
                                                      (priority
                                                       (org-entry-get
                                                        marker
                                                        "PRIORITY")))
                                           (s-lex-format
                                            "p${priority}"))))))))))

(setq
 cashpw/org-agenda-view--today `((agenda
                                  ""
                                  ((org-agenda-overriding-header "")
                                   (org-agenda-dim-blocked-tasks t)
                                   (org-agenda-use-tag-inheritance t)
                                   (org-use-property-inheritance t)
                                   (org-agenda-span 1)
                                   (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
                                   (org-super-agenda-groups
                                    '((:discard
                                       (:scheduled future
                                        :deadline future))
                                      (:name "Schedule"
                                       :time-grid t
                                       :order 0
                                       :transformer (replace-regexp-in-string "TODO " ""
                                                                              (replace-regexp-in-string "\\[#[0-9]\\] " "" it)))
                                      (:name "In Progress"
                                       :todo "INPROGRESS")
                                      (:auto-map cashpw/org-super-agenda--get-priority
                                       :transformer (plist-put it :items (-map
                                                                          (lambda (line)
                                                                            (replace-regexp-in-string "TODO " ""
                                                                                                      (replace-regexp-in-string "\\[#[0-9]\\] " "" line)))
                                                                          (plist-get it :items))))
                                      ;; (:name "Scheduled/Due Today"
                                      ;;  :scheduled today
                                      ;;  :deadline today)
                                      (;; Toss all other todos
                                       :discard
                                       (:anything))))))))

(setq
 cashpw/org-agenda-view--week `((agenda
                            ""
                            ((org-agenda-overriding-header "")
                             (org-agenda-span 7)
                             (org-super-agenda-groups
                              '((:name ""
                                 :and (:not (:tag "repeating")))
                                (;; Toss all other todos
                                 :discard
                                 (:todo t))))))))

(setq
 cashpw/org-agenda-view--plan--week `((agenda
                                       ""
                                       (
                                        ;; (org-agenda-start-with-log-mode nil)
                                        (org-habit-show-habits-only-for-today nil)
                                        (org-agenda-overriding-header "")
                                        (org-agenda-span 'week)
                                        (org-agenda-show-all-dates t)
                                        (org-agenda-prefix-format '((agenda . " %i %-20(cashpw/org-agenda-category)%-12t%-5e")))
                                        (org-super-agenda-groups '(
                                                                   (:name "Schedule"
                                                                    :time-grid t
                                                                    :order 0)
                                                                   (:auto-map cashpw/org-super-agenda--get-priority)))))
                                      (alltodo
                                       ""
                                       ((org-agenda-overriding-header "\n\n\nTODOs")
                                        ;; (org-agenda-files (cashpw/org-agenda-files))
                                        (org-super-agenda-groups
                                         '((:discard
                                            (:todo "PROJ"
                                             :scheduled t
                                             :deadline t
                                             ))
                                           (:auto-map cashpw/org-super-agenda--get-priority)))))))

(defun cashpw/org-agenda-view--review--files-fn ()
  "Return list of files for review agenda views."
  (append
   (cashpw/org-agenda-files 'notes-personal
                            'archive-and-non-archive)
   (cashpw/org-agenda-files 'people
                            'archive-and-non-archive)))

(setq
 cashpw/org-agenda-view--review--logged `((agenda
                                           ""
                                           ((org-agenda-overriding-header "")
                                            (org-agenda-span 'day)
                                            (org-agenda-files (cashpw/org-agenda-view--review--files-fn))
                                            (org-agenda-show-log t)
                                            (org-agenda-start-with-log-mode '(state closed clock))
                                            (org-agenda-hide-tags-regexp (concat org-agenda-hide-tags-regexp "\\|ARCHIVE"))
                                            (org-clocktable-defaults '(:fileskip0 t))
                                            (org-super-agenda-groups '(
                                                                       (:name "Logged"
                                                                        :log t)
                                                                       (;; Toss all other todos
                                                                        :discard
                                                                        (:todo t))))))))

(setq
 cashpw/org-agenda-view--review--clockcheck `((agenda
                                               ""
                                               ((org-agenda-overriding-header "")
                                                (org-agenda-span 'day)
                                                (org-agenda-files (cashpw/org-agenda-view--review--files-fn))
                                                (org-agenda-show-log 'clockcheck)
                                                (org-agenda-start-with-log-mode 'clockcheck)
                                                (org-clocktable-defaults '(:fileskip0 t))
                                                (org-super-agenda-groups '((:name "Clockcheck"
                                                                            :log t)
                                                                           (;; Toss all other todos
                                                                            :discard
                                                                            (:todo t))))))))

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

(setq
 cashpw/org-agenda-view--review--clockreport `((agenda
                                                ""
                                                ((org-agenda-overriding-header "")
                                                 (org-agenda-span 'day)
                                                 (org-agenda-files (cashpw/org-agenda-view--review--files-fn))
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

(setq
 cashpw/org-agenda-view--roam--roam `((alltodo
                                       ""
                                       ((org-agenda-overriding-header "")
                                        ;; Speed up
                                        (org-agenda-dim-blocked-tasks nil)
                                        (org-agenda-inhibit-startup t)
                                        (org-agenda-use-tag-inheritance nil)
                                        (org-agenda-ignore-properties '(effort appt category stats))
                                        ;; TODO Speed up by pre-computing
                                        (org-agenda-files (append (cashpw/org-agenda-files--notes)
                                                                  (cashpw/org-agenda-files--notes-private)))
                                        (org-super-agenda-groups
                                         `((:name "In Progress"
                                            :todo "INPROGRESS")
                                      (:auto-map cashpw/org-super-agenda--get-priority)
                                           ))))))

(setq
 cashpw/org-agenda-view--roam--unread `((alltodo
                                         ""
                                         ((org-agenda-overriding-header "")
                                          (org-agenda-files (s-lex-format "${cashpw/path--notes-dir}/unread.org"))
                                          (org-agenda-dim-blocked-tasks nil)
                                          (org-super-agenda-groups
                                           `(
                                             ;; (:name "essay (10)"
                                             ;;  :take (10 (:and
                                             ;;             (:tag "essay"
                                             ;;              :not (:tag "someday"
                                             ;;                    :tag "link_group")))))
                                             ,(cashpw/org-super-agenda--get-first-n-from-roam-tag 10
                                                                                                  "essay")
                                             ,(cashpw/org-super-agenda--get-first-n-from-roam-tag 10
                                                                                                  "discussion")
                                             ,(cashpw/org-super-agenda--get-first-n-from-roam-tag 10
                                                                                                  "book")
                                             ,(cashpw/org-super-agenda--get-first-n-from-roam-tag 10
                                                                                                  "link_group")
                                             ,(cashpw/org-super-agenda--get-first-n-from-roam-tag 10
                                                                                                  "class")
                                             ,(cashpw/org-super-agenda--get-first-n-from-roam-tag 10
                                                                                                  "someday")
                                             (:discard
                                              (:todo t))))))))

(setq
 cashpw/org-agenda-view--no-effort `((alltodo
                                 ""
                                 ((org-agenda-overriding-header "")
                                  (org-super-agenda-groups
                                   '((;; Automatically named "Log"
                                      :log t)
                                     (:discard
                                      (;; Don't bother listing PROJ items. They are used to group actionable TODOs.
                                       :todo "PROJ"))
                                     (:name "Without effort"
                                      :effort< "0:01")))))))

(setq
 cashpw/org-agenda-view--no-priority `((alltodo
                                   ""
                                   ((org-agenda-overriding-header "")
                                    (org-super-agenda-groups
                                     '((;; Automatically named "Log"
                                        :log t)
                                       (:discard
                                        (;; Don't bother listing PROJ items. They are used to group actionable TODOs.
                                         :todo "PROJ"))
                                       (:name "Without priority"
                                        :priority>= "0")))))))

(setq
 cashpw/org-agenda-view--not-scheduled `((alltodo
                                          ""
                                          ((org-agenda-overriding-header "")
                                           (org-super-agenda-groups
                                            '((;; Automatically named "Log"
                                               :log t)
                                              (:discard
                                               (;; Don't bother listing PROJ items. They are used to group actionable TODOs.
                                                :todo "PROJ"))
                                              (:discard
                                               (:scheduled t))
                                              (:auto-category t)))))))

(defun cashpw/org--scheduled-to-repeat-daily-p (pom)
  "Return non-nil if the headline at POM repeats daily."
  (when-let* ((scheduled-alist (org-entry-properties pom
                                                     "SCHEDULED"))
              (scheduled-string (cdr (car scheduled-alist))))
    (or (s-contains-p "++1d" scheduled-string)
        (s-contains-p ".+1d" scheduled-string))))

(defun cashpw/org--scheduled-to-repeat-weekly-p (pom)
  "Return non-nil if the headline at POM repeats weekly."
  (when-let* ((scheduled-alist (org-entry-properties pom
                                                     "SCHEDULED"))
              (scheduled-string (cdr (car scheduled-alist))))
    (or (s-contains-p "++1w" scheduled-string)
        (s-contains-p ".+1w" scheduled-string))))

(defun cashpw/org--scheduled-to-repeat-p (pom)
  "Return non-nil if the headline at POM repeats weekly."
  (when-let* ((scheduled-alist (org-entry-properties pom
                                                     "SCHEDULED"))
              (scheduled-string (cdr (car scheduled-alist))))
    (or (s-contains-p "++" scheduled-string)
        (s-contains-p ".+" scheduled-string))))

(defun cashpw/foo ()
  (interactive)
  (message "%s" (org-entry-properties (point)
                                      "SCHEDULED")))

(setq
 org-agenda-bulk-custom-functions `((?L (lambda () (call-interactively 'org-agenda-date-later))))
 cashpw/org-agenda-view--overdue `((agenda
                                    ""
                                    ((org-agenda-overriding-header "")
                                     (org-super-agenda-groups
                                      '((:discard
                                         (
                                          :scheduled future
                                          :deadline future
                                          ;; :scheduled nil
                                          ;; :deadline nil
                                          :scheduled today
                                          :deadline today
                                          ))
                                        (:auto-map
                                         (lambda (item)
                                           (-when-let* ((marker (or (get-text-property 0 'org-marker item)
                                                                    (get-text-property 0 'org-hd-marker)))
                                                        (default-priority "?")
                                                        (priority (or (cashpw/org--get-priority marker)
                                                                      default-priority)))
                                             (cond
                                              ((cashpw/org--scheduled-to-repeat-daily-p marker)
                                               "Repeats daily")
                                              ((cashpw/org--scheduled-to-repeat-weekly-p marker)
                                               "Repeats weekly")
                                              ((cashpw/org--scheduled-to-repeat-p marker)
                                               "Repeats")
                                              (t
                                               "z-Other")))))
                                        (;; Toss all other todos
                                         :discard
                                         (:todo t))))))))

(setq
 org-agenda-custom-commands `(("d" "Today" ,cashpw/org-agenda-view--today)

                              ("o" "Overdue" ,cashpw/org-agenda-view--overdue)

                              ("p" . "Plan")
                              ("pw" "Week" ,cashpw/org-agenda-view--plan--week)

                              ("r" . "Review")
                              ("rc" "Clock check" ,cashpw/org-agenda-view--review--clockcheck)
                              ("rl" "Logged" ,cashpw/org-agenda-view--review--logged)
                              ("rr" "Clock report" ,cashpw/org-agenda-view--review--clockreport)

                              ("R" . "Roam")
                              ("Rr" "Roam" ,cashpw/org-agenda-view--roam--roam)
                              ("Ru" "Unread" ,cashpw/org-agenda-view--roam--unread)

                              ("-" . "Without")
                              ("-e" "Without effort" ,cashpw/org-agenda-view--no-effort)
                              ("-p" "Priority" ,cashpw/org-agenda-view--no-priority)
                              ("-s" "Not scheduled" ,cashpw/org-agenda-view--not-scheduled)

                              ("w" "Week" ,cashpw/org-agenda-view--week)))

(defun cashpw/org-clock--agenda-with-archives ()
  "Return list of agenda files to use with clocktable."
  (append
   (cashpw/org-agenda-files 'notes-personal
                            'archive-and-non-archive)
   (cashpw/org-agenda-files 'people
                            'archive-and-non-archive)))

(after! org
  (setq
   ;; Prevent org-clock from double-checking /every/ agenda file for dangling clock during `org-clock-in'.
   ;; See https://github.com/doomemacs/doomemacs/issues/5317
   org-clock-auto-clock-resolution nil))

(defun cashpw/org-clock--clocktable--properties ()
  "Return default clocktable properties."
  `(:scope cashpw/org-clock--agenda-with-archives
    :block ,(format-time-string "%Y-%m-%d")
    :narrow 200
    :fileskip0 t
    :filetitle t
    :maxlevel 3))

(defun cashpw/org-clock--clocktable--update-default-properties ()
  "Return default clocktable properties"
  (setq
   org-clock-clocktable-default-properties (cashpw/org-clock--clocktable--properties)))

(cashpw/org-clock--clocktable--update-default-properties)
;; Update the properties once per day as they include `:block' with today's date.
(let ((seconds-in-day (* 60 60 24)))
  (cancel-function-timers #'cashpw/org-clock--clocktable--update-default-properties)
  (run-at-time "00:00"
               seconds-in-day
               #'cashpw/org-clock--clocktable--update-default-properties))

(defun cashpw/clocktable-by-category--properties ()
  "Return clocktable-by-category properties."
  `(:files-fn cashpw/org-clock--agenda-with-archives
    :block ,(format-time-string "%Y-%m-%d")
    :merge-duplicate-headlines t
    ;; :narrow 200
    ;; :fileskip0 t
    ;; :filetitle t
    ))

(defun cashpw/clocktable-by-category--update-default-properties ()
  "Return default clocktable-by-category properties"
  (setq
   clocktable-by-category--default-properties (cashpw/clocktable-by-category--properties)))

(cashpw/clocktable-by-category--update-default-properties)
;; Update the properties once per day as they include `:block' with today's date.
(let ((seconds-in-day (* 60 60 24)))
  (cancel-function-timers #'cashpw/clocktable-by-category--update-default-properties)
  (run-at-time "00:00"
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

(defun cashpw/org-roam-set-filetag ()
  "Set the filetag option based on org-roam tags."
  (interactive)
  (when (org-roam-file-p)
    (let* ((current-node-id (org-roam-node-id
                             (org-roam-node-at-point)))
           (tag (completing-read "Select tag: "
                                 (cashpw/org-roam--get-filetags-not-in-node
                                  current-node-id))))
      (cashpw/org-mode-set-filetag tag))))

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

(defun cashpw/org-mode-add-option (option value)
  "Add another option; requires at least one option to already be present.

TODO: move to org-mode section"
  (goto-char
   (point-max))
  (insert "foo")
  (when (search-backward-regexp
         "#\\+[A-Za-z_]+:"
         ;; bound
         nil
         ;; noerror
         t)
    (cashpw/org-mode-insert-option
     option
     value)))

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
        (when cashpw/org-hugo-replace-front-matter-with-title
          (with-current-buffer
              (find-file-noselect
               export-file-path)
            (cashpw/replace-toml-front-matter-with-md-heading)
            (save-buffer)))))))

(defun cashpw/org-roam-new-node (file-path title &optional properties)
  "Build a new org-roam node in a temp file.

PROPERTIES is expected to be an alist of additional properties to include.

Reference: https://ag91.github.io/blog/2020/11/12/write-org-roam-notes-via-elisp"
  (let* ((id (org-id-new))
         (created-date (format-time-string
                        "[%Y-%m-%d %a %H:%M]"))
         (all-properties (append
                          `(("ID" . ,id))
                          properties)))
    (with-temp-file file-path
      (goto-char (point-min))
      (insert (s-lex-format ":PROPERTIES:\n:ID: ${id}\n:END:\n"))
      (if properties
          (cashpw/org-mode-insert-properties all-properties))
      (goto-char (point-max))
      (cashpw/org-mode-insert-options
       `(("title" . ,title)
         ("author" . "Cash Weaver")
         ("date" . ,created-date)))
      )))

(defun cashpw/org-roam-new-node-from-link-heading-at-point (&optional mark-as-done)
  "Build a new org-roam node from the link heading at point."
  (interactive)
  (let* ((link (org-element-context))
         (type (org-element-property
                :type
                link))
         (url (org-element-property
               :raw-link
               link))
         (description (cashpw/org-mode-get-description-from-link-at-point))
         (org-roam-node-file-path (cashpw/org-roam-make-filepath
                                   description)))
    ;; TODO Replace with regexp?
    (unless (or (string= type "http")
                (string= type "https")))
    (cashpw/org-roam-new-node org-roam-node-file-path
                                  description)
    (if mark-as-done
        (org-todo "DONE"))
    (find-file org-roam-node-file-path)
    (goto-char (point-max))
    (insert "\n")
    (insert (s-lex-format "${url}\n"))
      (org-insert-heading)
      (insert "TODO Summary")
      (org-insert-heading)
      (insert "TODO Notes")
      (org-insert-heading)
      (insert "TODO Thoughts")
    ))

(after! doct-org-roam
  (setq
   ;; Note that I've enumerated the "head" entries, rather than defining them in the "group"
   ;; and specifying the tag with a variable, because this didn't produce the right output.
   ;; I didn't have time to dive in an understand why.
   org-roam-capture-templates (doct-org-roam `((:group "org-roam"
                                                :type plain
                                                :template "%?"
                                                :file "${slug}.org"
                                                :unnarrowed t
                                                :children (("Concept"
                                                            :keys "c"
                                                            :head ("#+title: ${title}"
                                                                   "#+author: Cash Prokop-Weaver"
                                                                   "#+date: [%<%Y-%m-%d %a %H:%M>]"
                                                                   "#+filetags: :concept:"))
                                                           ("Person"
                                                            :keys "p"
                                                            :head ("#+title: ${title}"
                                                                   "#+author: Cash Prokop-Weaver"
                                                                   "#+date: [%<%Y-%m-%d %a %H:%M>]"
                                                                   "#+filetags: :person:"))
                                                           ("Verse"
                                                            :keys "v"
                                                            :head ("#+title: ${title}"
                                                                   "#+author: Cash Prokop-Weaver"
                                                                   "#+date: [%<%Y-%m-%d %a %H:%M>]"
                                                                   "#+filetags: :verse:"))
                                                           ("Quote"
                                                            :keys "u"
                                                            :head ("#+title: ${title}"
                                                                   "#+author: Cash Prokop-Weaver"
                                                                   "#+date: [%<%Y-%m-%d %a %H:%M>]"
                                                                   "#+filetags: :quote:"))
                                                           ("Recipe"
                                                            :keys "r"
                                                            :head ("#+title: ${title}"
                                                                   "#+author: Cash Prokop-Weaver"
                                                                   "#+date: [%<%Y-%m-%d %a %H:%M>]"
                                                                   "#+filetags: :recipe:"
                                                                   ""
                                                                   "* TODO [#2] Ingredients"
                                                                   ""
                                                                   "#+begin_ingredients"
                                                                   "| Ingredients | Notes | Quantity |"
                                                                   "|-------------+-------+----------|"
                                                                   "|             |       |          |"
                                                                   "#+end_ingredients"
                                                                   ""
                                                                   "* TODO [#2] Steps"))))))))


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
    (message roam-refs)
    (if (s-starts-with-p "http"
                         roam-refs)
        (browse-url roam-refs)
      (message "Not an http(s) ref (%s)"
               roam-refs))))

;; This bit is no longer necessary as I've discovered .dir-locals.el.
;; Setting `org-attach-directory' in .dir-locals.el has the desired
;; effect of this function.
;;
;; (defun cashpw/org-roam-insert-attachment-path ()
;;   (let ((dir
;;          (format
;;           "%s/%s"
;;           cashpw/org-roam-attachment-base-path
;;           (org-id-get))))
;;     (save-excursion
;;       (org-set-property
;;        "DIR"
;;        dir))))
;; (after! org-roam
;;   (remove-hook! 'org-roam-capture-new-node-hook
;;               'cashpw/org-roam-insert-attachment-path))

(defun cashewaver-org-roam--append-to-custom-front-matter (key value)
  "Append the provided KEY and VALUE to hugo_custom_front_matter."
  (when (org-roam-file-p)
    (let
        ((keyword
          "HUGO_CUSTOM_FRONT_MATTER")
         (current-value
          (org-collect-keywords
           keyword)))
      (message current-value)
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

;; (cashpw/org-roam--mirror-roam-aliases-to-hugo-aliases)

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

(defun cashpw/org-roam--process-ref-before-adding-to-front-matter (ref)
  (cond
   ((string-match-p "^\\[cite" ref)
    nil
    ;; (let ((citation
    ;;        (save-excursion
    ;;          (beginning-of-buffer)
    ;;          (search-forward ref)
    ;;          (org-element-citation-parser))))
    ;;   (org-cite-export-citation
    ;;    citation
    ;;    nil
    ;;    '(:cite-export nil)
    ;;    ))
    )
   (t
    ref)))

(defun cashpw/org-hugo--set-custom-front-matter (text)
  (org-roam-set-keyword
   (downcase "HUGO_CUSTOM_FRONT_MATTER")
   text))

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

(defun cashpw/anki-available-p ()
  (condition-case error
      (anki-editor-api-check)
    ('error
     nil))
  t)

(defun cashpw/enable-anki-editor-mode ()
  (if (cashpw/anki-available-p)
      (anki-editor-mode t)
    (message "Skipping enable anki-editor-mode because Anki isn't available.")))

(defun cashpw/anki-editor-push-notes ()
  (if (cashpw/anki-available-p)
      (anki-editor-push-notes)
    (message "Skipping anki-editor-push-notes because Anki isn't available.")))

(defun cashpw/org-roam-add-flashcards (&optional skip-if-present)
  "Add flashcard heading to the current buffer."
  (interactive)
  (let ((skip-if-present (or skip-if-present
                             t))
        (flashcard-header-regexp (rx "*"
                                     (or " TODO"
                                         " INPROGRESS"
                                         "")
                                     (or (and " \[#"
                                              (any "0" "1" "2" "3" "4")
                                              "\]")
                                         "")
                                     " Flashcards"))
        (is-valid-file (and (org-roam-file-p)
                            (not (member (buffer-file-name)
                                         cashpw/org-roam--file-path-exceptions-to-add-flashcards)))))
    (when is-valid-file
      (let* ((flashcard-present-in-buffer (save-excursion
                                            (goto-char (point-min))
                                            (re-search-forward flashcard-header-regexp
                                                               ;; bound
                                                               nil
                                                               ;; noerror
                                                               t))))
        (unless (and skip-if-present
                     flashcard-present-in-buffer)
          (save-excursion
            (goto-char (point-max))
            (org-insert-heading nil t t)
            (insert "TODO [#2] Flashcards :noexport:")))))))

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
#+author: Cash Weaver
#+date: [%<%Y-%m-%d %a %H:%M>]
#+filetags: :reference:

TODO_AUTHOR, [cite:@${citekey}]

* TODO Summary
* TODO Thoughts
* TODO Notes")
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
         (title (read-string "Title:"
                             default-title)))
    (cashpw/org-roam-node-from-cite--inner entry title)))

(defun cashpw/org-roam-before-save ()
  (cashpw/org-roam-rewrite-smart-to-ascii)
  (cashpw/org-roam-mirror-roam-refs-to-front-matter)
  (cashpw/org-roam-add-bibliography)
  (cashpw/org-roam-add-flashcards)
  ;; (cashpw/org-hugo-export-wim-to-md)
  ;; (cashpw/anki-editor-push-notes)
  )

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

(defun org-roam-id-complete (&optional initial-input filter-fn sort-fn require-match prompt)
  "Read an `org-roam-node', returning its id.

All args are passed to `org-roam-node-read'."
  (concat
   "id:"
   (org-roam-node-id
    (org-roam-node-read
     initial-input filter-fn sort-fn require-match prompt))))

(org-link-set-parameters "id" :complete #'org-roam-id-complete)

(after! org-roam
  (defun org-hugo-export-wim-to-md-after-save ()
    (cashpw/org-hugo-export-wim-to-md))
  (setq
   cashpw/org-hugo-replace-front-matter-with-title nil))

(defun cashpw/buffer-has-todo-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find (lambda (type)
              (eq type 'todo))
            (org-element-map
                (org-element-parse-buffer 'headline)
                'headline
              (lambda (h)
                (org-element-property :todo-type h)))))

(defun cashpw/org-roam-update-hastodo-tag ()
  "Update HASTODO tag in the current buffer."
  (message "cashpw/org-roam-update-hastodo-tag")
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (cashpw/buffer-has-todo-p)
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

(defun cashpw/org-roam-todo-files ()
  "Return a list of note files containing 'hastodo tag." ;
  (seq-uniq (seq-map #'car
                     (org-roam-db-query [:select [nodes:file]
                                         :from tags
                                         :left-join nodes
                                         :on (= tags:node-id nodes:id)
                                         :where (like tag "hastodo")]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq
   org-agenda-files (cashpw/org-roam-todo-files)))

;; Commented out because it caused the tag to disappear from org-roam.db on sync.
;; (add-hook 'find-file-hook #'cashpw/org-roam-update-todo-tag)
(add-hook! 'before-save-hook
           'cashpw/org-roam-update-hastodo-tag)

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
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n"
                          backlinks-as-string)))))))

(add-hook! 'org-export-before-processing-hook
           'cashpw/org-roam--export-backlinks)

(defun cashpw/org-hugo--set-gallery-item-citation ()
  "Based on `citar-org-roam-ref-add."
  (interactive)
  (let ((citation (with-temp-buffer (org-cite-insert nil)
                                    (buffer-string))))
    (org-set-property "CITATION" citation)))

(defun cashpw/org-roam--export-gallery (backend)
  "Transform a list of headings into a list of files compatible with {{< gallery >}}.

Only run when BACKEND is `'hugo'."
  (when (and (org-roam-file-p)
             (equal backend 'hugo))
    (let ((gallery-tag "gallery")
          (org-use-tag-inheritance nil))
      (save-excursion
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let* ((sub-heading-level (1+ (org-outline-level)))
                  (match (s-lex-format "LEVEL=${sub-heading-level}"))
                  ;; Speed up `org-entry-properties' (see `org-map-entries')
                  (org-trust-scanner-tags t)
                  (image-lines '()))
             (org-narrow-to-subtree)
             (org-map-entries
              (lambda ()
                (let* ((file (org-entry-get nil "ITEM"))
                       (citation-property-name "CITATION")
                       (citation-key (when-let ((citation-property-alist (org-entry-properties nil citation-property-name)))
                                       (replace-regexp-in-string "\\[cite.*:@\\([^;]*\\)\\(;\\)?.*\\]"
                                                                 "\\1"
                                                                 (cdr (assoc citation-property-name
                                                                             citation-property-alist)))))
                       (citar-entry (citar-get-entry citation-key))
                       (attr (cdr (assoc "author" citar-entry)))
                       (attrlink (cdr (assoc "url" citar-entry)))
                       (attr-html (concat
                                   "#+attr_html: "
                                   (cond
                                    ((and attr attrlink)
                                     (s-lex-format " :attr ${attr} :attrlink ${attrlink}"))
                                    (attr
                                     (s-lex-format " :attr ${attr}"))
                                    (t
                                     ""))))
                       (line (s-lex-format "
${attr-html}
${file}
")))
                  (push line
                        image-lines)
                  ))
              match
              'tree)
             (end-of-line)
             (newline)
             (delete-region (point) (point-max))
             (newline)
             (insert "#+begin_hugogallery")
             (newline)
             (insert (s-join "" (nreverse image-lines)))
             (insert "#+end_hugogallery")
             (newline)
             (widen)))
         gallery-tag)))))

(add-hook! 'org-export-before-processing-hook
           'cashpw/org-roam--export-gallery)

(defgroup org-roam-recipe
  nil
  "Options related to recipes in org-mode."
  :tag "Recipes in org-roam.")

(defcustom org-roam-recipe--property-prefix
  "recipe"
  "Prefix for all org-mode headline properties used by org-roam-recipe.")

(defun org-roam-recipe--prop-prep-minutes ()
  "Org-mode property for preparation minutes."
  (concat org-roam-recipe--property-prefix "_prep_minutes"))

(defun org-roam-recipe--prop-cook-minutes ()
  "Org-mode property for cooking minutes."
  (concat org-roam-recipe--property-prefix "_cook_minutes"))

(defun org-roam-recipe--prop-servings ()
  "Org-mode property for servings."
  (concat org-roam-recipe--property-prefix "_servings"))

(defun org-roam-recipe--prop-yield ()
  "Org-mode property for yield."
  (concat org-roam-recipe--property-prefix "_yield"))

(defun org-roam-recipe--prop-stars ()
  "Org-mode property for stars (rating)."
  (concat org-roam-recipe--property-prefix "_stars"))

(defcustom org-roam-recipe--properties
  `(,(org-roam-recipe--prop-prep-minutes)
    ,(org-roam-recipe--prop-cook-minutes)
    ,(org-roam-recipe--prop-servings)
    ,(org-roam-recipe--prop-yield)
    ,(org-roam-recipe--prop-stars))
  "Org-mode properties used by org-roam-recipe."
  :type '(repeat string)
  :group 'org-roam-recipe)

(defun org-roam-recipe--get-prop (property pom &optional default)
  "Return value for PROPERTY at POM or (optional) DEFAULT if value is nil."
  (message "org-roam-recipe--get-prop pos: %s" pom)
  (let ((property-value (org-entry-get pom property)))
    (if default
        (or property-value
            default)
      property-value)))

(defun org-roam-recipe--format-duration (minutes)
  "Return formatted time string of duration MINUTES."
  (org-duration-from-minutes minutes))

(defun org-roam-recipe--get-minutes (property &optional pos)
  "Return prep time in minutes at POS."
  (let* ((pos (or pos
                  (point-min)))
         (minutes (string-to-number
                   (org-roam-recipe--get-prop property
                                              pos
                                              "0"))))
    minutes))

(defun org-roam-recipe-get-prep-minutes (&optional pos)
  "Return prep time in minutes at POS."
  (org-roam-recipe--get-minutes (org-roam-recipe--prop-prep-minutes)))

(defun org-roam-recipe-get-cook-minutes (&optional pos)
  "Return cook time in minutes at POS."
  (org-roam-recipe--get-minutes (org-roam-recipe--prop-cook-minutes)))

(defun org-roam-recipe--get-int-property (property &optional pos)
  "Return the integer value of PROPERTY (at POS) or nil if it's not set."
  (let* ((pos (or pos
                  (point-min)))
         (value (org-roam-recipe--get-prop property
                                           pos)))
    (message "%s" pos)
    (message "%s" value)
    (if value
        (string-to-number value)
      nil)))

(defun org-roam-recipe-get-servings (&optional pos)
  "Return servings at POS (e.g. \"8\")."
  (org-roam-recipe--get-int-property (org-roam-recipe--prop-servings)
                                     pos))

(defun org-roam-recipe-get-stars (&optional pos)
  "Return stars at POS (e.g. \"4\")."
  (org-roam-recipe--get-int-property (org-roam-recipe--prop-stars)
                                     pos))

(defun org-roam-recipe-get-yield (&optional pos)
  "Return yield at POS (e.g. \"7 liters\")."
  (let ((pos (or pos
                 (point-min))))
    (org-roam-recipe--get-prop (org-roam-recipe--prop-yield)
                               pos)))

(defun org-roam-recipe-get-prep-duration ()
  "Return formatted preparation duration."
  (let ((prep-minutes (org-roam-recipe-get-prep-minutes)))
    (if (= 0 prep-minutes)
        nil
      (org-roam-recipe--format-duration prep-minutes))))

(defun org-roam-recipe-get-cook-duration ()
  "Return formatted cooking duration."
  (let ((cook-minutes (org-roam-recipe-get-cook-minutes)))
    (if (= 0 cook-minutes)
        nil
      (org-roam-recipe--format-duration cook-minutes))))

(defun org-roam-recipe-get-total-duration ()
  "Return formatted total duration."
  (let* ((cook-minutes (org-roam-recipe-get-cook-minutes))
         (prep-minutes (org-roam-recipe-get-prep-minutes))
         (total-minutes (-sum
                         (-non-nil `(,cook-minutes
                                     ,prep-minutes)))))
    (if (= 0 total-minutes)
        nil
      (org-roam-recipe--format-duration total-minutes))))

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

(setq
 ;; org-roam
 cashpw/org-roam--capture-template--todo `("Roam"
                                           :keys "r"
                                           :file ,(lambda () (s-lex-format "${org-roam-directory}/todos.org"))
                                           :template ("* TODO [#2] %?"
                                                      ":PROPERTIES:"
                                                      ":CREATED: %U"
                                                      ":END:"))
 ;; org-fc
 cashpw/org-fc--capture-template--normal `("Normal"
                                           :keys "n"
                                           :file ,(lambda () (buffer-name))
                                           :olp ("Flashcards")
                                           :template ("* %^{Name of card}"
                                                      ":PROPERTIES:"
                                                      ":CREATED: %U"
                                                      ":END:"
                                                      ""
                                                      "%?"
                                                      ""
                                                      "** TODO Back"
                                                      "** TODO Source"
                                                      )
                                           :prepare-finalize ,(lambda ()
                                                                (goto-char (point-min))
                                                                (org-fc-type-normal-init)))
 cashpw/org-fc--capture-template--double `("Double"
                                           :keys "d"
                                           :file ,(lambda () (buffer-name))
                                           :olp ("Flashcards")
                                           :template ("* %^{Name of card}"
                                                      ":PROPERTIES:"
                                                      ":CREATED: %U"
                                                      ":END:"
                                                      ""
                                                      "%?"
                                                      ""
                                                      "** TODO Back"
                                                      "** TODO Source"
                                                      )
                                           :prepare-finalize ,(lambda ()
                                                                (goto-char (point-min))
                                                                (org-fc-type-double-init)))
 cashpw/org-fc--capture-template--cloze `("Cloze"
                                          :keys "c"
                                          :file ,(lambda () (buffer-name))
                                          :olp ("Flashcards")
                                          :template ("* %^{Name of card}"
                                                     ":PROPERTIES:"
                                                     ":CREATED: %U"
                                                     ":END:"
                                                     ""
                                                     "%?"
                                                     ""
                                                     "** TODO Source")
                                          :prepare-finalize ,(lambda ()
                                                               (goto-char (point-min))
                                                               (org-fc-type-cloze-init 'deletion)))

 cashpw/org-fc--capture-template--text-input `("Text input"
                                               :keys "t"
                                               :file ,(lambda () (buffer-name))
                                               :olp ("Flashcards")
                                               :template ("* %^{Name of card}"
                                                          ":PROPERTIES:"
                                                          ":CREATED: %U"
                                                          ":END:"
                                                          ""
                                                          "%?"
                                                          ""
                                                          "** TODO Back"
                                                          "** TODO Source")
                                               :prepare-finalize ,(lambda ()
                                                                    (goto-char (point-min))
                                                                    (org-fc-type-text-input-init)))


 cashpw/org-fc--capture-template--vocab `("Vocab"
                                          :keys "v"
                                          :file ,(lambda () (buffer-name))
                                          :olp ("Flashcards")
                                          :template ("* %^{Term}"
                                                     ":PROPERTIES:"
                                                     ":CREATED: %U"
                                                     ":END:"
                                                     ""
                                                     "%?"
                                                     ""
                                                     "** TODO Source")
                                          :prepare-finalize ,(lambda ()
                                                               (goto-char (point-min))
                                                               (org-fc-type-vocab-init)))

 ;; General
 cashpw/org-capture-templates--todo--todo `("Todo"
                                            :keys "t"
                                            :file ,(lambda () (s-lex-format "${cashpw/path--notes-personal-dir}/todos.org"))
                                            :template ("* TODO %?"
                                                       ":PROPERTIES:"
                                                       ":Created: %U"
                                                       ":END:")))

(after! org
  (setq
   org-capture-templates (doct `(,cashpw/org-protocol--capture-template--web-site
                                 (:group "Todo"
                                  :children (("Todo"
                                              :keys "t"
                                              :children (,cashpw/org-roam--capture-template--todo
                                                         ,cashpw/org-capture-templates--todo--todo))))
                                 (:group "Flashcards"
                                  :children (("Flashcards"
                                              :keys "f"
                                              :children (,cashpw/org-fc--capture-template--cloze
                                                         ,cashpw/org-fc--capture-template--double
                                                         ,cashpw/org-fc--capture-template--normal
                                                         ,cashpw/org-fc--capture-template--vocab
                                                         ,cashpw/org-fc--capture-template--text-input))))))))

(use-package! org-link-base)

(use-package! org-link-isbn)

(use-package! org-link-instagram)

(use-package! org-link-twitter)

(use-package! org-link-google-doc)

(use-package! org-link-google-sheet)

(use-package! org-link-amazon)

(use-package! org-link-reddit
  )

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

(setq org-format-latex-header "\\documentclass{article}
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
\\newcommand{\\determinant}[1]{\\operatorname{det}(#1)}
")

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

(after! org
  :config
  (setq
   org-export-with-tags nil))

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
  (when (cashpw/is-work-p)
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
  (let ((buffers-to-kill (--filter (s-ends-with-p ".md" (buffer-name it))
                                   (buffer-list))))
    (mapc 'kill-buffer
          buffers-to-kill)))
;; (cashpw/kill-all-markdown-buffers)
;; (buffer-list)

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(defun cashpw/org-hugo-export-all (&optional directory)
  "Export all hugo files in DIRECTORY."
  (interactive)
  (let* ((directory (or directory
                        (s-lex-format "${cashpw/path--home-dir}/proj/notes")))
         (org-roam-directory directory)
         (files-to-ignore `(
                            ;; ,(s-lex-format "${directory}/unread.org")
                            ;; ,(s-lex-format "${directory}/todos.org")
                            ))
         (files-to-export (seq-difference (directory-files
                                           directory
                                           ;; full
                                           t
                                           ;; match
                                           ".org$")
                                          files-to-ignore))
         ;; For debugging
         ;; (files-to-export (-slice files-to-export 1015))
         (count-files-to-export (length
                                 files-to-export))
         ;; Last updated: 2023-12-05
         (recent-run-file-count 1467)
         (recent-run-seconds 7791)
         (seconds-per-file (/ recent-run-seconds
                              recent-run-file-count))
         (calc-remaining-minutes (lambda (current-file-number)
                                   (let ((files-left (- count-files-to-export current-file-number)))
                                     (/ (* seconds-per-file
                                           files-left)
                                        60))))
         (run-time-estimate (org-duration-from-minutes (funcall calc-remaining-minutes 0)))
         (log-file-path "/tmp/hugo-export.log")
         (last-percent-start-time nil)
         (should-run (y-or-n-p (s-lex-format
                                "Found ${count-files-to-export} nodes in ${directory}. Export estimate: ${run-time-estimate}."))))
    (when should-run
      (let* ((progress-reporter (make-progress-reporter "Exporting roam notes"
                                                        0
                                                        (length files-to-export)))
             (start-time (current-time))
             (org-id-extra-files (org-roam-list-files))
             (i 0))
        ;; Speed up the export
        ;; (advice-add 'org-id-find :override 'org-roam-id-find)
        ;; (memoize 'org-roam-node-id)
        ;; (memoize 'org-roam-node-file)
        (save-excursion
          (mapc
           (lambda (filepath)
             (let ((inner-start-time (current-time))
                   (inhibit-message t)
                   (roam-file-buffer (find-file-noselect filepath))
                   (start-time (current-time)))
               (when (= 0 (% i 10))
                 (message "[cashpw] fix cannot redirect stderr too many open files")
                 ;; Prevent `Error: (file-error "Cannot redirect stderr" "Too many open files" "/dev/null")'
                 (cashpw/kill-all-markdown-buffers)
                 (file-notify-rm-all-watches))
               (message "cashpw/org-hugo-export-all (%d/%d) exporting [%s]"
                        (1+ i)
                        count-files-to-export
                        filepath)
               (with-current-buffer roam-file-buffer
                 (remove-hook 'before-save-hook 'org-encrypt-entries t)
                 (org-hugo-export-to-md))
               (kill-buffer roam-file-buffer)
               (message "cashpw/org-hugo-export-all (%d/%d) exported [%s] %.06f"
                        (1+ i)
                        count-files-to-export
                        filepath
                        (float-time (time-since inner-start-time)))
               (let* ((log-file-path "/tmp/hugo-export.log")
                      (end-time (current-time))
                      (file-number (1+ i))
                      (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
                      (export-duration-in-seconds (float-time (subtract-time end-time
                                                                             start-time))))
                 (append-to-file (s-lex-format "${time-string}: ${file-number}/${count-files-to-export} ${filepath} (duration: ${export-duration-in-seconds})\n")
                                 nil
                                 log-file-path)))
             (progress-reporter-update progress-reporter
                                       i
                                       (concat "Remaining time (estimate): "
                                               (org-duration-from-minutes
                                                (funcall calc-remaining-minutes
                                                         i))))
             (setq i (1+ i)))
           files-to-export))
        (progress-reporter-done progress-reporter)
        ;; Remove speed-up changes
        ;; (advice-remove 'org-id-find 'org-roam-id-find)
        ;; (memoize-restore 'org-roam-node-id)
        ;; (memoize-restore 'org-roam-node-file)

        (message "cashpw/org-hugo-export-all %.06f" (float-time (time-since start-time)))))))

(defun cashpw/org-mode--split-tags-to-list (tags-as-string)
  "Strip the wrapping ':' from TAG; if present."
  (if tags-as-string
      (if (string-match "^:\\(.*\\):$"
                        tags-as-string)
          (split-string (match-string 1
                                      tags-as-string)
                        ":")
        nil)
    nil))

(defun cashpw/org-hugo--tag-processing-fn-roam-tags (tag-list info)
  "Add tags from filetags to tag-list for org-roam to ox-hugo compatibility.

Reference: https://sidhartharya.me/exporting-org-roam-notes-to-hugo/#goal

See `org-hugo-tag-processing-functions'."
  (if (org-roam-file-p)
      (let* ((filetags
              (car
               (cdr
                (assoc-string
                 "FILETAGS"
                 (org-collect-keywords
                  '("FILETAGS"))))))
             (filetag-list
              (or
               (cashpw/org-mode--split-tags-to-list
                filetags)
               '())))
        (append tag-list
                (mapcar
                 #'downcase
                 filetag-list)))
    tag-list))

(after! ox-hugo
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
              (bib-heading (org-string-nw-p (plist-get org-hugo-citations-plist :bibliography-section-heading))))
          (if bib-heading
              (let* ((bib-heading (org-blackfriday--translate nil info bib-heading))
                     (loffset (string-to-number
                               (or (org-entry-get nil "EXPORT_HUGO_LEVEL_OFFSET" :inherit)
                                   (plist-get info :hugo-level-offset))))
                     (level-mark (make-string (+ loffset 1) ?#)))
                (format "%s %s\n\n%s" level-mark bib-heading bib))
            (format "%s" bib))))))


  ;; Speed up exporting files
  ;; (advice-add 'org-id-find :override 'org-roam-id-find)
  ;; (memoize 'org-roam-node-id)
  ;; (memoize 'org-roam-node-file)
  )

(after! org
  (setq
   org-publish-project-alist
   '(("notes.cashpw.com"
      :base-directory "~/proj/notes"
      :base-extension "org"
      :publishing-directory "~/proj/notes.cashpw.com/content/posts/"
      ;;:publishing-function org-pandoc-publish-to-md
      :publishing-function org-hugo-export-to-md
      :section-numbers t
      :with-toc nil))))

;; Publish org-roam files without using org-publish because org-publish requires a top-level headline.
;; ("roam"
;; :base-directory "~/proj/notes/"
;; :base-extension "org"
;; :publishing-directory "~/proj/cashweaver.com/content/posts/"
;; :publishing-function org-hugo-export-to-md
;; :table-of-contents nil
;; :section-numbers t
;; :with-toc nil))))

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

(setq
 cashpw/path--roam-bibliography
 (format "%s/proj/notes/bibliography.bib"
         cashpw/path--home-dir)
 cashpw/bibliographies `(,cashpw/path--roam-bibliography))

(use-package! citar
  :after all-the-icons
  :when (modulep! :completion vertico)
  :config
  (setq
   citar-bibliography cashpw/bibliographies
   citar-symbols `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
                   (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
                   (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))
   citar-symbol-separator "  "
   citar-notes-paths `(,cashpw/path--notes-dir)
   )
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

(use-package! citar-org)

;; (use-package! citar-org-roam
;;   :after citar org-roam
;;   :no-require
;;   :config (citar-org-roam-mode))

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
   :map org-mode-map
   :localleader
   :nv "@" nil
   (:prefix ("@" . "Citation")
    :n "@" #'org-cite-insert)
   (:prefix ("b")
    :n "RET" #'org-table-copy-down)
   (:prefix ("c")
    :n "E" #'org-clock-modify-effort-estimate
    :n "e" #'org-set-effort
    :n "p" #'omc-make-new-parallel-clock
    :n "s" #'omc-set-active-clock
    (:prefix ("R" . "Report")
     :n "c" #'clocktable-by-category-report
     :n "C" #'org-clock-report
     :n "t" #'clocktable-by-tag-report))
   (:prefix ("d")
            (:prefix ("h" . "insert heading")
             :n "t" (cmd! (cashpw/org-mode-insert-heading-for-today
                           ;; top
                           nil
                           ;; time-in-heading
                           nil
                           ;; include-all-tags
                           nil))
             :n "T" (cmd! (cashpw/org-mode-insert-heading-for-today nil t t))
             :n "w" (cmd! (cashpw/org-mode-insert-heading-for-this-week nil)))
            (:prefix ("S")
                     (:prefix ("." . "today")
                      :desc "at" :n "a" #'cashpw/org--schedule-today-at)
                     ;; (:prefix ("d" . "day")
                     ;;  :desc "Monday" :n "m" )
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

   (:prefix ("D")
    :n "R" #'org-download-rename-last-file
    :n "c" #'org-download-clipboard
    :n "d" #'org-download-delete
    :n "e" #'org-download-edit
    :n "i" #'org-download-image
    :n "r" #'org-download-rename-at-point
    :n "s" #'org-download-screenshot
    :n "y" #'org-download-yank
    )

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
    :desc "Create node from headline link" :n "N" (cmd! ()
                                                        (cashpw/org-roam-new-node-from-link-heading-at-point
                                                         ;; mark-as-done
                                                         t))
    :desc "Publish all" :n "p" #'cashpw/org-hugo-export-all)
   (:prefix ("S" . "Structure")
    :n "i" #'org-insert-structure-template)))

(after! org-noter
  (map!
   :map pdf-view-mode-map
   :localleader

   :n "n" #'org-noter-insert-note
   :n "N" #'org-noter-insert-precise-note
   :desc "Quote (precise)" :n "Q" #'cashpw/org-noter-insert-selected-text-inside-note-content))

(use-package! pdf-tools)

(use-package! pdf-tools
  :config
  (pdf-tools-install))

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

(defun cashpw/maybe-add-trailing-forward-slash (str)
  "Return STR with a trailing slash (added if it was missing)."
  (if (s-ends-with? "/" str)
      str
    (format "%s/" str)))

(defun cashpw/get-flattened-known-project-paths ()
  "Return a list of all known project paths"
  (let* ((nested-paths
          (cl-loop for fn in cashpw/project-path-fns
                collect (funcall fn)))
         (paths
          (mapcar
           (lambda (path)
             (cashpw/maybe-add-trailing-forward-slash path))
           (flatten-tree
            nested-paths))
          ))
    paths))

(defun cashpw/projectile-refresh-known-paths ()
  "Refresh the paths which projectile knows about."
  (interactive)
  (projectile-clear-known-projects)
  (setq projectile-known-projects
        (cashpw/get-flattened-known-project-paths)))

(after! projectile
  (cashpw/projectile-refresh-known-paths))

(defun cashpw/replace-selection ()
  (interactive)
  (let* ((register ?\")
         (to-replace (replace-regexp-in-string
                      "/"
                      "\\\\/"
                      (progn
                        (evil-yank (mark)
                                   (point)
                                   nil
                                   register)
                        (evil-get-register register)))))
    (evil-ex (s-lex-format  "%s/${to-replace}/"))))

(defun cashpw/reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(use-package! toml)

(use-package! electric-case)

;; (add-hook!
;;  'java-mode-hook
;;  'electric-case-java-init)

(defun cashpw/org-roam-push-anki ()
  (interactive)
  (measure-time
   (let* ((org-hugo-auto-set-lastmod nil)
          (description "Pushing anki cards")
          (expected-seconds-per-file 3.0
                                     ;; Determined by timing the export of 100 files.
                                     ))

     (let ((export-file-name "/tmp/anki.org")
           (export-buffer-name "*Org ORG Export*")
           (anki-heading-regexp "\\* \\(TODO \\)?\\(\\[#2\\] \\)?Anki"))
       ;; Clear /tmp/anki.org
       (with-current-buffer (find-file-noselect
                             export-file-name)
         (erase-buffer)
         (goto-char (point-min))
         (insert "#+PROPERTY: ANKI_DECK Default\n\n")
         (save-buffer))

       ;; Export all anki cards to /tmp/anki.org
       (cashpw/org-roam-in-all-files
        (lambda ()
          (let ((before-save-hook '())
                (org-export-with-title nil)
                (org-export-with-date nil)
                (org-export-with-author nil)
                (org-export-with-creator nil)
                (org-export-with-properties t)
                (org-global-properties '(("ANKI_DECK" . "DEFAULT")))
                (anki-heading-regexp "\\* \\(TODO \\)?\\(\\[#2\\] \\)?Anki"))
            (org-mode)
            (when (buffer-contains-regexp anki-heading-regexp)
              (goto-char (point-min))
              (search-forward-regexp anki-heading-regexp)
              (org-org-export-as-org
               ;; async
               nil
               ;; subtreep
               t)
              (set-buffer-modified-p nil)
              (with-current-buffer (find-file-noselect
                                    export-file-name)
                (goto-char (point-max))
                (insert
                 (with-current-buffer export-buffer-name
                   (goto-char (point-min))
                   ;; Remove "# Created YYYY-MM-DD ..."
                   (kill-whole-line)
                   (set-buffer-modified-p nil)
                   (buffer-substring-no-properties (point-min)
                                                   (point-max))))
                (save-buffer))
              )))
        description
        expected-seconds-per-file)

       ;; Push cards
       (with-current-buffer (find-file-noselect
                             export-file-name)
         (anki-editor-push-notes))
       ))))

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

(use-package! org-capture-ref)
(use-package! asoc)

(let ((templates (doct '( :group "Browser link"
                          :type entry
                          :file "~/proj/notes/bibliography.org"
                          :fetch-bibtex (lambda () (org-capture-ref-process-capture)) ; this must run first
                          :bibtex (lambda () (org-capture-ref-get-bibtex-field :bibtex-string))
                          :extra (lambda () (if (org-capture-ref-get-bibtex-field :journal)
                                                (s-join "\n"
                                                        '("- [ ] download and attach pdf"
                                                          "- [ ] [[elisp:org-attach-open][read paper capturing interesting references]]"
                                                          "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.semanticscholar.org/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check citing articles]]"
                                                          "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.connectedpapers.com/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check related articles]]"
                                                          "- [ ] check if bibtex entry has missing fields"))
                                              ""))
                          :org-entry (lambda () (org-capture-ref-get-org-entry))
                          :template
                          ("%{fetch-bibtex}* TODO %?%{space}%{org-entry}"
                           "%{extra}"
                           "#+begin_src bibtex :tangle bibliography.bib"
                           "%{bibtex}"
                           "#+end_src")
                          :children (("Interactive link"
                                      :keys "b"
                                      :space " "
                                      )
                                     ("Silent link"
                                      :keys "B"
                                      :space ""
                                      :immediate-finish t))))))
  (dolist (template templates)
    (asoc-put! org-capture-templates
               (car template)
               (cdr  template)
               'replace)))

(use-package! memoize)

(defun cashpw/anki-editor-insert-note ()
  (interactive)
  (with-current-buffer
      (find-file-noselect
       "~/proj/anki-cards/anki.org")
    (point-min)
    (anki-editor-insert-note)))

(defun cashpw/pointer-between-chars-p (chars-before chars-after &optional explicit)
  "Return t if the pointer is between the provided chars.

Examples (| is the pointer):
  - \"abc|dab\": (cashpw/pointer-between-chars-p \"c\" \"d\") -> t
  - \"abc|dab\": (cashpw/pointer-between-chars-p \"a\" \"b\") -> t
  - \"abc|dab\": (cashpw/pointer-between-chars-p \"a\" \"b\" t) -> nil
  - \"aFc|dab\": (cashpw/pointer-between-chars-p \"a\" \"b\" t) -> t"
  (let* ((bol (save-excursion
                (beginning-of-line)))
         (eol (save-excursion
                (end-of-line)))
         (pos-chars-before (save-excursion
                             (search-backward chars-before bol t)
                             (point)))
         (pos-chars-after (save-excursion
                            (search-forward chars-after eol t)
                            (point)))
         (char-at-point (char-after (point)))
         (point-between-chars (and
                               ;; pos-chars-before < (point) < chars-after-point
                               (> (point)
                                  pos-chars-before)
                               (< (point)
                                  pos-chars-after))))
    ;; (message (buffer-substring (line-beginning-position)
    ;;                            (line-end-position)))
    ;; (message (concat
    ;;           "(point): "
    ;;           (number-to-string (point))))
    ;; (message (s-lex-format
    ;;           "char-at-point: ${char-at-point}"))
    ;; (message (s-lex-format
    ;;           "pos-chars-before: ${pos-chars-before}"))
    ;; (message (s-lex-format
    ;;           "pos-chars-after: ${pos-chars-after}"))
    (if explicit
        (let* ((pos-prev-chars-after (save-excursion
                                       (search-backward chars-after eol t)
                                       (point))))
          ;; (message (s-lex-format
          ;;           "pos-prev-chars-after: ${pos-prev-chars-after}"))
          ;; (message (s-lex-format
          ;;           "point-between-chars: ${point-between-chars}"))
          (cond
           ((not point-between-chars)
            nil)
           ((= pos-prev-chars-after
               (point))
            t)
           ((< pos-prev-chars-after
               (point))
            nil)))
      point-between-chars)))

;; test cases
(with-temp-buffer
  (insert "[e]")
  (goto-char 2)
  (cl-assert
   (cashpw/pointer-between-chars-p "[" "]")))
(with-temp-buffer
  (insert "[]e[]")
  (goto-char 3)
  (cl-assert
   (cashpw/pointer-between-chars-p "[" "]")))
(with-temp-buffer
  (insert "[ced[]")
  (goto-char 3)
  (cl-assert
   (cashpw/pointer-between-chars-p "[" "]" t)))
(with-temp-buffer
  (insert "[]ced[]")
  (goto-char 4)
  (cl-assert
   (not (cashpw/pointer-between-chars-p "[" "]" t))))


(defun cashpw/bounds-of-chars-surrounding-point (before after)
  (interactive)
  (let* ((bol (save-excursion
                (beginning-of-line)))
         (eol (save-excursion
                (end-of-line)))
         (end (save-excursion
                (search-forward after eol t)
                (point)))
         (begin (save-excursion
                  (search-backward before bol t)
                  (point))))
    `(,begin . ,end)))

(defun cashpw/pointer-in-link-p ()
  (cashpw/pointer-between-chars-p "[[" "]]"))

(defun cashpw/bounds-of-link-at-point ()
  (cashpw/bounds-of-chars-surrounding-point "[[" "]]"))

(defun cashpw/pointer-in-mathjax-p ()
  (cashpw/pointer-between-chars-p "\\(" "\\)"))

(defun cashpw/bounds-of-mathjax-at-point ()
  (cashpw/bounds-of-chars-surrounding-point "\\(" "\\)"))

(defun cashpw/anki-editor-cloze-dwim (&optional arg hint)
  "Cloze current active region or a word the under the cursor"
  (interactive "p\nsHint (optional): ")
  (cond
   ((region-active-p)
    (anki-editor-cloze (region-beginning) (region-end) arg hint))
   ((cashpw/pointer-in-link-p)
    (let ((bounds (cashpw/bounds-of-link-at-point)))
      (anki-editor-cloze (car bounds)
                         (cdr bounds)
                         arg
                         hint)))
   ((cashpw/pointer-in-mathjax-p)
    (let ((bounds (cashpw/bounds-of-mathjax-at-point)))
      (anki-editor-cloze (car bounds)
                         (cdr bounds)
                         arg
                         hint)))
   ((thing-at-point 'word)
    (let ((bounds (bounds-of-thing-at-point
                   'word)))
      (message "word")
      (anki-editor-cloze (car bounds)
                         (cdr bounds)
                         arg
                         hint)))
   (t
    (error "Nothing to create cloze from"))))

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
