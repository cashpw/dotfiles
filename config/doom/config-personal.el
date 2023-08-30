(setq
 search-invisible t)

(use-package! dash
  :ensure t)

(require 'ess-site)

(setq
 user-full-name "Cash Prokop-Weaver"
 user-mail-address "cashbweaver@gmail.com")

(setq
 ;; Use YYYY-MM-DD date format.
 calendar-date-style 'iso)

(defun cashpw/is-weekday (time)
  (cl-member (format-time-string "%a" time)
             '("Mon" "Tue" "Wed" "Thu" "Fri")
             :test #'equal))

(let ((sunday (date-to-time "2023-01-01T08:00:00-0700"))
      (monday (date-to-time "2023-01-02T08:00:00-0700")))
  (cl-assert
   (equal
    (---truthy? (cashpw/is-weekday sunday))
    nil))
  (cl-assert
   (equal
    (---truthy? (cashpw/is-weekday monday))
    t)))

(defun cashpw/is-weekend (time)
  (not (cashpw/is-weekday time)))

(let ((sunday (date-to-time "2023-01-01T08:00:00-0700"))
      (monday (date-to-time "2023-01-02T08:00:00-0700")))
  (cl-assert
   (equal
    (---truthy? (cashpw/is-weekend sunday))
    t))
  (cl-assert
   (equal
    (---truthy? (cashpw/is-weekend monday))
    nil)))

(defun cashpw/format-time (&optional time-format offset-days)
  "Return the date, with optional OFFSET-DAYS, in format."
  (interactive)
  (let* ((time-format (or time-format
                          "%Y-%m-%d"))
         (offset-days (or offset-days
                          0))
         (time (cond
                ((= offset-days
                    0)
                 (current-time))
                (t
                 (time-add (current-time)
                           (days-to-time offset-days))))))
    (format-time-string time-format
                        time)))

(defun cashpw/todays-date (&optional time-format)
  "Return the date, with optional OFFSET-DAYS, in format."
  (cashpw/format-time time-format))

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

(setq
 alert-fade-time 60
 alert-default-style 'libnotify)

;; Too early load error
(use-package! org-wild-notifier
  :config
  (setq
   org-wild-notifier-alert-time '(2))
  (org-wild-notifier-mode))

(map!
 ;; Keep in alphabetical order.
 (:leader
  :desc "at point" :n "h h" #'helpful-at-point
  :desc "Langtool" :n "t L" #'langtool-check
  (:prefix ("n")
   :desc "Store email link" :n "L" #'org-notmuch-store-link
   (:prefix ("A" . "Flashcards")
    :n "d" #'org-fc-dashboard
    :n "i" #'org-fc-init
    :n "u" #'org-fc-update
    :n "r" #'cashpw/org-fc-review-all
    :n "R" #'org-fc-review)
   (:prefix ("r")
    :n "c" #'cashpw/org-roam-node-from-cite))
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

; Reference; https://www.emacswiki.org/emacs/DocumentingKeyBindingToLambda
(defun cashpw/evil-lambda-key (mode keymap key def)
  "Wrap `evil-define-key' to provide documentation."
  (set 'sym (make-symbol (documentation def)))
  (fset sym def)
  (evil-define-key mode keymap key sym))

(defun run-function-in-file (filepath function &optional arguments)
  (let ((args (or arguments
                  nil)))
    (save-excursion
      (find-file filepath)
      (apply function arguments)
      (write-file filepath)
      (kill-buffer (current-buffer)))))

(defcustom cashpw/secrets-dir-path
  (s-lex-format "${cashpw/home-dir-path}/.config/secrets")
  "Path to directory containing secret files.")

(defun cashpw/secrets-get (name)
  "Get content of NAME secret file."
  (let ((secret-file-path (s-lex-format "${cashpw/secrets-dir-path}/${name}")))
    (if (file-exists-p secret-file-path)
        (with-temp-buffer
          (insert-file-contents secret-file-path)
          (buffer-string))
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
  (let* ((file
          cashpw/path--file--notes-todos)
         (to-short
          (cashpw/mail-get-short-address
           (message-field-value "To")))
         (from-short
          (cashpw/mail-get-short-address
           (message-field-value "From")))
         (subject
          (message-field-value "Subject"))
         (message-id
          (replace-regexp-in-string
           "<\\(.*\\)>"
           "\\1"
           (message-field-value "Message-ID")))
         (now
          (with-temp-buffer
            (org-mode)
            (org-time-stamp-inactive '(16))
            (buffer-substring-no-properties
             (point-min)
             (point-max)))))
    (with-current-buffer
        (get-file-buffer file)
      (goto-char
       (point-max))
      (org-insert-heading-respect-content)
      (org-todo "TODO")
      (insert
       (s-lex-format
        "[[notmuch:id:${message-id}][${subject} (${from-short} ➤ ${to-short})]]: Follow up :email:"
        ))
      (org-set-property
       "Created"
       now)
      (org-schedule
       nil))))

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

;(use-package! calfw-cal
;  :config
;  (setq
;   ; Start the week on Monday
;   calendar-week-start-day 1))
;
;(use-package! calfw-ical)
;(use-package! calfw-org)
;
;(defun cashpw/calfw-open ()
;  "Open my calendar"
;  (interactive)
;  (cfw:open-calendar-buffer
;   :contents-sources
;   (list
;    (cfw:org-create-source "Green"))))

(setq
 ediff-split-window-function #'split-window-horizontally)

(setq
 completion-ignore-case t)

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

(defun cashpw/contacts--get-next-annual-time (time)
  "TODO."
  (if (not (time-less-p time (current-time)))
      time
    (cl-destructuring-bind (seconds minutes hours days months years day-of-week daylight-savings-time-p utc-offset)
        (decode-time time)
      (let* ((current-year (nth 5 (decode-time (current-time))))
             (next-year (1+ current-year)))
        (encode-time seconds minutes hours days months next-year day-of-week daylight-savings-time-p utc-offset)))))


(cl-letf (((symbol-function 'current-time) (lambda ()
                                             (date-to-time "2022-10-05T08:00:00-0700"))))
  (cl-assert
   (equal
    (cashpw/contacts--get-next-annual-time (date-to-time "2022-10-10T08:00:00-0700"))
    (date-to-time "2022-10-10T08:00:00-0700")))
  (cl-assert
   (equal
    (cashpw/contacts--get-next-annual-time (date-to-time "2022-10-01T08:00:00-0700"))
    (date-to-time "2023-10-01T08:00:00-0700")))

  (cl-assert
   (equal
    (cashpw/contacts--get-next-annual-time (date-to-time "2000-10-10T08:00:00-0700"))
    (date-to-time "2023-10-10T08:00:00-0700")))
  (cl-assert
   (equal
    (cashpw/contacts--get-next-annual-time (date-to-time "2000-10-01T08:00:00-0700"))
    (date-to-time "2023-10-01T08:00:00-0700"))))

(defcustom cashpw/contacts--birthday-prop
  "BIRTHDAY"
  "Property name for a contact's birthday.")

(defcustom cashpw/contacts--reminders-heading
  "Reminders"
  "Heading text for the reminders heading.")

(cl-defun cashpw/contacts--has-prop-p (prop)
  "Returns nil if the contact lacks the PROP."
  (member
   prop
   (org-buffer-property-keys)))

(cl-defun cashpw/contacts--get-prop (prop)
  "Returns value of PROP or nil if PROP not found."
  (org-entry-get
   (point-min)
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
   nil 'tree))

(cl-defun cashpw/contacts--create-top-level-heading-if-absent (heading-text &optional pos)
  "Creates a top-level heading with HEADING-TEXT at POS if such a heading doesn't exist in buffer.

Returns nil if the heading already existed."
  (let ((pos
         (or pos
             (point-max))))
    (if (member heading-text
                (cashpw/contacts--list-top-level-headings))
        (cl-return-from
            cashpw/contacts--create-top-level-heading-if-absent
          nil))

    (goto-char
     pos)
    (org-insert-heading
     ;; arg
     nil
     ;; invisible-ok
     t
     ;; top
     t)
    (insert heading-text)))

(cl-defun cashpw/contacts--goto-heading (heading-text)
  "Move pointer to the heading with HEADING-TEXT.

Does nothing if such a heading is absent."
  (let ((heading-position
         (org-find-exact-headline-in-buffer
          heading-text)))
    (unless heading-position
      (cl-return-from
          cashwever/contacts--goto-heading
        nil))
    (goto-char
     heading-position)))

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
  "Contacts files are roam files."
  (org-roam-file-p))

(cl-defun cashpw/contacts--create-birthday-reminder ()
  "Creates an annual birthday reminder."
  (when (and (cashpw/contacts-file-p)
             (cashpw/contacts--has-prop-p cashpw/contacts--birthday-prop))
    (let ((contact-name (cashpw/contacts--get-name))
          (heading-text (s-lex-format
                         "${contact-name}'s Birthday")))
      (unless (cashpw/contacts--heading-exists-p
               heading-text)
        (let* ((birth-time (org-time-string-to-time
                            (org-read-date nil ;; with-time
                                           t   ;; to-time
                                           (cashpw/contacts--get-prop ;; from-string
                                            ;; TODO: Convert this to a defcustom.
                                            cashpw/contacts--birthday-prop)
                                           nil ;; prompt
                                           )
                            ))
               (reminder-time (cashpw/contacts--get-next-annual-time
                               birth-time)))
          (cashpw/contacts-create-reminder
           heading-text
           reminder-time
           "+1y"))))))

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
          (cashpw/contacts-create-reminder
           upcoming-birthday-heading-text
           reminder-time
           "++1y")))

      (unless (cashpw/contacts--heading-exists-p birthday-heading-text)
        (let* ((reminder-time (cashpw/contacts--get-next-annual-time
                               birth-time)))
          (cashpw/contacts-create-reminder
           birthday-heading-text
           reminder-time
           "++1y"))))))

(cl-defun cashpw/contacts--get-name (&optional path)
  (let ((path
         (or
          path
          (buffer-file-name
           (buffer-base-buffer)))))
    (unless path
      (cl-return-from
          cashpw/contacts--get-name
        nil))
    (with-current-buffer
        (get-file-buffer path)
      (pcase
          (org-collect-keywords '("TITLE"))
        (`(("TITLE" . ,val))
         (car val))))))

(defun cashpw/contacts-aniversaries (contact-file-directory &optional field)
  "Compute FIELD anniversaries for each contact.

Based on `org-contacts-anniversaries'."
  (let ((field
         (or field
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
  (let ((org-roam-directory
         "~/proj/people")
        (org-roam-db-location
         "~/proj/people/org-roam.db"))
    (when (emacsql-live-p
           (org-roam-db--get-connection))
      (emacsql-close
       (org-roam-db--get-connection)))
    (org-roam-db)
    (org-roam-db-query
     [:select *
      :from nodes])))


;; (cashpw/contacts--create-birthday-reminder)
;; (cashpw/contacts--create-top-level-heading-if-absent "foo")
;; (cashpw/contacts--get-name)
;; (cashpw/contacts--list-top-level-headings)

(use-package! doct
  :commands (doct))

;; (use-package! ol-doi
;;  :after org)

(use-package! orgtbl-aggregate)

(use-package! org-ai
  :after org
  :commands (org-ai-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :custom
  (org-ai-openai-api-token (cashpw/secrets-get "openai")))

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

(remove-hook! 'org-mode-hook #'org-fancy-priorities-mode)

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
  (org-fc-directories `(,(s-lex-format "${cashpw/home-dir-path}/proj/notes")
                        ,(s-lex-format "${cashpw/home-dir-path}/proj/people")
                        ,(s-lex-format "${cashpw/home-dir-path}/proj/notes-personal")
                        ,(s-lex-format "${cashpw/home-dir-path}/proj/notes-private")))
  (org-fc-review-history-file (s-lex-format "${cashpw/home-dir-path}/.config/org-fc/org-fc-reviews.tsv"))
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
      ('sm2-v1 (org-fc-algo-sm2-initial-review-data position index))
      ('sm2-v2 (org-fc-algo-sm2-initial-review-data position index))))

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

(use-package! org-mime)

(use-package! ol-notmuch
  :after org)

(use-package! org-ql)

(use-package! vulpea)

(use-package! doct-org-roam
  :after doct)

(use-package! org-roam
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

(defun cashpw/org-gtasks--get-client-secret ()
  "Return client secret for Cash Weaver's Google Tasks."
  (replace-regexp-in-string
   "\n"
   ""
   (with-temp-buffer
     (insert-file-contents
      (file-truename
       "~/.config/org-gtasks/client-secret"))
     (buffer-string))))

;; (use-package! org-gtasks
;;   :after org
;;   :config
;;   (setq
;;    cashpw/org-gtasks-client-id "TODO"
;;    cashpw/org-gtasks-client-secret (cashpw/org-gtasks--get-client-secret)
;;    org-gtasks-accounts '())
;;   (org-gtasks-register-account
;;    :name "People"
;;    :directory "~/proj/people"
;;    :client-id cashpw/org-gtasks-client-id
;;    :client-secret cashpw/org-gtasks-client-secret))

(use-package! org-vcard)

(defun cashpw/org-mode--heading-text-for-today (&optinoal time-in-heading include-all-tags)
  "Return the heading text for today as a string."
  (let* ((time-in-heading (or time-in-heading
                              nil))
         (include-all-tags (or include-all-tags
                               nil))
         (today-week-number (cashpw/format-time "%W"))
         (today-quarter-number (cashpw/format-time "%q"))
         (today-year (cashpw/format-time "%Y"))
         (today-month-number (cashpw/format-time "%m"))
         (today-day-number (cashpw/format-time "%d"))
         (today-weekday-abbreviated-name (cashpw/format-time "%a"))
         (tags (if include-all-tags
                   (s-lex-format
                    ":${today-year}:${today-year}week${today-week-number}:${today-year}Q${today-quarter-number}:")
                 ""))
         (hh-mm (if time-in-heading
                    (concat " " (cashpw/format-time "%H:%M"))
                  ""))
         (yyyy-mm-dd (cashpw/format-time)))
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
        (today-yyyy-mm-dd (cashpw/format-time "%Y-%m-%d"))
        (today-hh-mm (cashpw/format-time "%H:%M"))
        (today-weekday-abbreviated-name (cashpw/format-time "%a")))
    (if top
        (org-insert-heading nil t t)
      (org-insert-heading-respect-content))
    (insert
     heading-text)
    (org-set-property
     "Created"
     (format "[%s %s %s]"
             today-yyyy-mm-dd
             today-weekday-abbreviated-name
             today-hh-mm))))

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
         (today-week-number (cashpw/format-time "%W"))
         (today-quarter-number (cashpw/format-time "%q"))
         (today-year (cashpw/format-time "%Y"))
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
          (cashpw/format-time
           "%Y"))
         (today-month-number
          (cashpw/format-time
           "%m"))
         (today-day-number
          (cashpw/format-time
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
                        (cashpw/format-time "[%Y-%m-%d %a %H:%M]")))))

(defun cashpw/org-mode-insert-property (property value)
  "Insert an org-mode property (:PROPERTY: VALUE)."
  (insert (s-lex-format
           ":${property}: ${value}\n")))

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

(defcustom cashpw/org-mode--inprogress-clock-in-paths
  nil
  "TODOs marked as INPROGRESS in these files will trigger a clock in.")

(defun cashpw/org-mode--inprogress-in-clock-in-file-p ()
  (member
   buffer-file-name
   cashpw/org-mode--inprogress-clock-in-paths))

(defun cashpw/org-mode-when-inprogress ()
  "Handle inprogress behavior."
  (when (string-equal (org-get-todo-state)
                       "INPROGRESS")
    (cond
     ((cashpw/org-mode--inprogress-in-clock-in-file-p)
      (org-clock-in)))))

(after! org
  :config
  (setq
   cashpw/org-mode--inprogress-clock-in-paths
   '("/home/cashweaver/proj/notes-personal/todos.org"))
  (add-hook!
   'org-after-todo-state-change-hook
   'cashpw/org-mode-when-inprogress))

(after! org
  :config
  (setq
   org-log-done 'time))

(defcustom cashpw/org-mode-done-noop-hook
  nil
  "Functions which are non-nil when we should noop the TODO at point."
  :type 'hook)

(defcustom cashpw/org-mode-done-cut-hook
  nil
  "Functions which are non-nil when we should cut the TODO at point."
  :type 'hook)

(defcustom cashpw/org-mode-noop-tag
  "noop"
  "Tag which, when present, indicates that the TODO item should be noop.")

(defun cashpw/org-mode--has-noop-tag-p ()
  (cashpw/org-mode--has-tag-p
   cashpw/org-mode-noop-tag))

(add-hook
 'cashpw/org-mode-done-noop-hook
 'cashpw/org-mode--has-noop-tag-p)

(defcustom cashpw/org-mode-cut-tag
  "cut"
  "Tag which, when present, indicates that the TODO item should be cut.")

(defun cashpw/org-mode--has-cut-tag-p ()
  (cashpw/org-mode--has-tag-p
   cashpw/org-mode-cut-tag))

(add-hook
 'cashpw/org-mode-done-cut-hook
 'cashpw/org-mode--has-cut-tag-p)

(defcustom cashpw/org-mode--done-noop-file-paths
  (append
   '()
   (cashpw/directory-files--org (s-lex-format "${cashpw/home-dir-path}/proj/people")))
  "TODOs in these files will be noop by default.")

(defun cashpw/org-mode--done-in-noop-file-p ()
  (member
   buffer-file-name
   cashpw/org-mode--done-noop-file-paths))

(add-hook
 'cashpw/org-mode-done-noop-hook
 'cashpw/org-mode--done-in-noop-file-p)

(defcustom cashpw/org-mode--done-cut-file-paths
  nil
  "TODOs in these files will be cut by default.")

(defun cashpw/org-mode--done-in-cut-file-p ()
  (member
   buffer-file-name
   cashpw/org-mode--done-cut-file-paths))

(add-hook
 'cashpw/org-mode-done-cut-hook
 'cashpw/org-mode--done-in-cut-file-p)

(add-hook
 'cashpw/org-mode-done-noop-hook
 (lambda ()
   (org-get-repeat)))

(add-hook
 'cashpw/org-mode-done-cut-hook
 (lambda ()
   (string=
    org-state
    "KILL")))

(defun cashpw/org-mode--should-noop-todo-when-done-p ()
  "Return non-nil if we should noop the current entry."
  (-any
   'funcall
   cashpw/org-mode-done-noop-hook))

(defun cashpw/org-mode--should-cut-todo-when-done-p ()
  "Return non-nil if we should cut the current entry."
  (-any
   'funcall
   cashpw/org-mode-done-cut-hook))

(defun cashpw/org-mode-when-done ()
  "Archive entry when it is marked as done (as defined by `org-done-keywords')."
  (when (org-entry-is-done-p)
    (org-clock-out-if-current)
    (cond
     ((cashpw/org-mode--should-noop-todo-when-done-p)
      nil)
     ((cashpw/org-mode--should-cut-todo-when-done-p)
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
   (s-lex-format "${cashpw/home-dir-path}/proj/people")))

(defun cashpw/org-agenda-files--notes-personal ()
  "Return a list of all personal notes files at DIR-PATH."
  (cashpw/directory-files--org
   (s-lex-format "${cashpw/home-dir-path}/proj/notes-personal")))

;; TODO Speed up by pre-computing
(defun cashpw/org-agenda-files--notes-private ()
  "Return a list of all private notes files at DIR-PATH."
  (cashpw/directory-files--org
   (s-lex-format "${cashpw/home-dir-path}/proj/notes-private")))

;; TODO Speed up by pre-computing
(defun cashpw/org-agenda-files--notes ()
  "Return a list of all private notes files at DIR-PATH."
  (let* ((org-roam-directory (s-lex-format "${cashpw/home-dir-path}/proj/notes"))
         (files-to-ignore `(,(s-lex-format "${org-roam-directory}/unread.org")
                            ,(s-lex-format "${org-roam-directory}/todos.org"))))
    (seq-difference (cashpw/org-roam-todo-files)
                    files-to-ignore)))

(defun cashpw/org-agenda-files--notes-private ()
  "Return a list of all private notes files at DIR-PATH."
  (let ((org-roam-directory (s-lex-format "${cashpw/home-dir-path}/proj/notes-private")))
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
   ;; TODO: Move this variable
   cashpw/roam-dir-path (s-lex-format
                         "${cashpw/home-dir-path}/proj/notes")
   cashpw/roam-unread-file-path (s-lex-format
                                 "${cashpw/roam-dir-path}/unread.org")
   org-agenda-prefix-format '((agenda . " %i %-20(cashpw/org-agenda-category)%?-12t%-6e% s")
                              (todo . " %i %-20(cashpw/org-agenda-category) %-6e %-40(cashpw/org-agenda-buganizer-title)")
                              (tags . " %i %-12c")
                              (search . " %i %-12c"))
   org-super-agenda-header-map evil-org-agenda-mode-map
   ;; https://emacs.stackexchange.com/a/17128
   org-agenda-sorting-strategy '((agenda time-up priority-down category-keep)
                                 (todo priority-down category-keep)
                                 (tags priority-down category-keep)
                                 (search category-keep))
   org-agenda-custom-commands `(("c" "Super view"
                                 ((agenda "" ((org-agenda-overriding-header "")
                                        ;(org-agenda-files cashpw/org-agenda-files--without-roam)
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
                                  (alltodo "" ((org-agenda-overriding-header "")
                                        ;(org-agenda-files cashpw/org-agenda-files--without-roam)
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

                                ("h" "Habits"
                                 ((agenda "" ((org-agenda-overriding-header "")
                                        ;(org-agenda-files cashpw/org-agenda-files--without-roam)
                                              (org-agenda-dim-blocked-tasks nil)
                                              (org-agenda-use-tag-inheritance nil)
                                              (org-habit-show-all-today t)
                                              (org-super-agenda-groups
                                               '((;; Automatically named "Log"
                                                  :log t)
                                                 (:name "People"
                                                  :and (:habit t :file-path "proj\\/people"))
                                                 (:name "Habits"
                                                  :habit)
                                                 (;; Toss all other todos
                                                  :discard
                                                  (:todo t))
                                                 ))))))
                                ("r" "Roam"
                                 ((alltodo "" ((org-agenda-overriding-header "")
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
                                                  ,(cashpw/org-super-agenda--group-by-priority)
                                                  ;; (:name "Todos"
                                                  ;;  :todo t)
                                                  ))))))
                                ("R"
                                 "Roam Unread"
                                 ((alltodo "" ((org-agenda-overriding-header "")
                                               (org-agenda-files `(,cashpw/roam-unread-file-path))
                                               (org-agenda-dim-blocked-tasks nil)
                                               (org-super-agenda-groups
                                                `(
                                                  (:name "essay (10)"
                                                   :take (10 (:and
                                                              (:tag "essay"
                                                               :not (:tag "someday"
                                                                     :tag "link_group")))))
                                                  ,(cashpw/org-super-agenda--roam-group
                                                    "discussion"
                                                    10)
                                                  ,(cashpw/org-super-agenda--roam-group
                                                    "book"
                                                    10)
                                                  ,(cashpw/org-super-agenda--roam-group
                                                    "link_group"
                                                    10)
                                                  ,(cashpw/org-super-agenda--roam-group
                                                    "class"
                                                    10)
                                                  (:name "someday (10)"
                                                   :take (10 (:and (:tag "someday"))))

                                                  (:discard
                                                   (:todo t)
                                                   )))))))))
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

(defun cashpw/org-super-agenda--group-by-priority ()
  "Group by my priorities (e.g. p0 through p4)."
  '(:auto-map
    (lambda (item)
      (-when-let* ((marker (or (get-text-property 0 'org-marker item)
                               (get-text-property 0 'org-hd-marker)))
                   (priority (org-entry-get
                              marker
                              "PRIORITY")))
        (if (string= priority "")
            "unknown"
          (s-lex-format
           "p${priority}")
          )))))

(defun cashpw/org-super-agenda--roam-group (tag take)
  "Return a plist TODO."
  `(:name ,(format "%s (%d)"
                   tag
                   take)
    :take (,take
           (:and
            (:tag ,tag
             :not (:tag "someday"))))))

(after! org
  :config
  (setq
   calendar-week-start-day 1
   org-agenda-entry-text-maxlines 30
   org-agenda-entry-text-leaders "  "
   org-agenda-time-grid '((daily
                           today
                           require-timed
                           remove-match)
                          (800 900 1000 1100 1200 1300 1400 1500 1600 1700)
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

(defun cashpw/org-clock--agenda-with-archives ()
   (cashpw/org-agenda-files t))

(defun cashpw/org-clock--update-clocktable-default-properties ()
  (setq
   org-clock-clocktable-default-properties `(:scope cashpw/org-clock--agenda-with-archives
                                             :block ,(format-time-string "%Y-%m-%d")
                                             :narrow 200
                                             :fileskip0 t
                                             :filetitle t)))

(cashpw/org-clock--update-clocktable-default-properties)
(let ((seconds-in-day (* 60 60 24)))
  (cancel-function-timers #'cashpw/org-clock--update-clocktable-default-properties)
  (run-at-time "00:00"
               seconds-in-day
               #'cashpw/org-clock--update-clocktable-default-properties))

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
         (created-date (cashpw/format-time
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
                                                                   "#+filetags: %{filetags}"
                                                                   ""
                                                                   "* TODO [#2] Ingredients"
                                                                   "* TODO [#2] Steps"))))))))

(add-hook
 'cashpw/org-mode-done-cut-hook
 'org-roam-file-p)

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

(defun cashpw/citation-present-in-buffer-p ()
  "Return true if a citation is present in the current buffer, nil otherwise."
  (let ((citation-prefix
         "[cite"))
    (save-excursion
      (goto-char
       (point-min))
      (search-forward
       citation-prefix
       ;; bound
       nil
       ;; no-error
       t))))

(defcustom cashpw/org-roam--file-path-exceptions-to-add-bibliography
  '()
  "File paths which will not have a bibliography added by `cashpw/org-roam-add-bibliography'.")

(defun cashpw/org-roam-add-bibliography (&optional skip-if-present)
  "Add #+print_bibiliography to the current buffer."
  (interactive)
  (when (and (org-roam-file-p)
             (not (member (buffer-file-name)
                          cashpw/org-roam--file-path-exceptions-to-add-bibliography))
             (cashpw/citation-present-in-buffer-p))
    (delete-matching-lines "\s*#\\+print_bibliography:$")
    (save-excursion
      (goto-char (point-max))
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
            (org-insert-heading
             ;; arg
             nil
             ;; invisible-ok
             t
             ;; top
             t)
            (insert "TODO [#2] Flashcards")))))))
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
  (seq-find                                 ; (3)
   (lambda (type)
     (eq type 'todo))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun cashpw/org-roam-update-todo-tag ()
    "Update HAS_TODO tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (cashpw/buffer-has-todo-p)
              (setq tags (cons "has_todo" tags))
            (setq tags (remove "has_todo" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun cashpw/org-roam-todo-files ()
    "Return a list of note files containing 'has_todo tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"has_todo\"%"))]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (cashpw/org-roam-todo-files)))

(add-hook 'find-file-hook #'cashpw/org-roam-update-todo-tag)
(add-hook 'before-save-hook #'cashpw/org-roam-update-todo-tag)

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

(defun cashpw/org-export-preprocessor (backend)
  (when (org-roam-file-p)
    (let* ((current-node (org-roam-node-at-point))
           (current-node-file (org-roam-node-file current-node))
           (-compare-fn (lambda (a b)
                          (equal (org-roam-node-id (org-roam-backlink-source-node a))
                                 (org-roam-node-id (org-roam-backlink-source-node b)))))
           (backlinks (-uniq (org-roam-backlinks-get current-node)))
           (backlinks-as-string (--reduce-from
                                 (let* ((source-node (org-roam-backlink-source-node it))
                                        (id (org-roam-node-id source-node))
                                        (file (org-roam-node-file source-node))
                                        (title (org-roam-node-title source-node)))
                                   (if (equal file current-node-file)
                                       acc
                                     (concat acc
                                             (s-lex-format " - [[id:${id}][${title}]]\n"))))
                                 ""
                                 backlinks)))
      (unless (string= backlinks-as-string "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n"
                          backlinks-as-string)))))))
(add-hook 'org-export-before-processing-hook 'cashpw/org-export-preprocessor)

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
                                            ;;  Can't figure out how to use `cashpw/path--file--notes-todos' here.
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
                                                         ;; ,cashpw/org-capture-templates--todo--todo
                                                         ))))
                                 (:group "Flashcards"
                                  :children (("Flashcards"
                                              :keys "f"
                                              :children (,cashpw/org-fc--capture-template--cloze
                                                         ,cashpw/org-fc--capture-template--double
                                                         ,cashpw/org-fc--capture-template--normal
                                                         ,cashpw/org-fc--capture-template--vocab
                                                         ,cashpw/org-fc--capture-template--text-input))))))
   ))

(use-package! org-link-base)

(use-package! org-link-isbn)

(use-package! org-link-instagram)

(use-package! org-link-twitter)

(use-package! org-link-google-doc)

(use-package! org-link-google-sheet)

(use-package! org-link-amazon)

(use-package! org-link-reddit)

(use-package! org-transclusion
  :after org
  :config
  (setq
   org-transclusion-exclude-elements '(property-drawer
                                       keyword)
   org-transclusion-extensions-loaded t
   org-transclusion-extensions '(org-transclusion-src-lines
                                 org-transclusion-font-lock
                                 org-transclusion-indent-mode))
  (add-hook! 'org-mode-hook 'org-transclusion-mode)
  ;; (set-face-attribute
  ;;  'org-transclusion-fringe nil
  ;;  :foreground "white"
  ;;  :background nil)
  (define-fringe-bitmap 'org-transclusion-fringe-bitmap
    [17 34 68 136 68 34 17]
    nil nil 'center)
  ;; Re-load extensions to activate `org-transclusion-indent-mode'.
  (org-transclusion-load-extensions-maybe t))

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
  (let ((title
         (cashpw/get-title-toml-front-matter)))
    (cashpw/remove-toml-header)
    (save-excursion
      (goto-char
       (point-min))
      (insert
       (format
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

(defun cashpw/org-hugo-export-all (&optional directory)
  "Export all hugo files in DIRECTORY."
  (interactive)
  (let* ((directory (or directory
                        (s-lex-format "${cashpw/home-dir-path}/proj/notes")))
         (org-roam-directory directory)
         (files-to-ignore `(,(s-lex-format "${directory}/unread.org")
                            ,(s-lex-format "${directory}/unread2.org")
                            ,(s-lex-format "${directory}/unread3.org")
                            ,(s-lex-format "${directory}/unread4.org")
                            ,(s-lex-format "${directory}/unread5.org")
                            ;; ,(s-lex-format "${directory}/todos.org")
                            ))
         (files-to-export (seq-difference
                           (directory-files
                            directory
                            ;; full
                            t
                            ;; match
                            ".org$")
                           files-to-ignore))
         ;; (files-to-export '("/usr/local/google/home/cashweaver/proj/notes/it_s_a_feature_not_a_bug.org"))
         ;; (files-to-export (-slice files-to-export 1040))
         (count-files-to-export (length
                                 files-to-export))
         (seconds-per-file 4)
         (time-estimate-minutes (/
                                 (* seconds-per-file
                                    count-files-to-export)
                                 60))
         (should-run (y-or-n-p (s-lex-format
                                "Found ${count-files-to-export} nodes in ${directory}. Export estimate: ${time-estimate-minutes} minutes."))))
    (when should-run
      (let* ((progress-reporter (make-progress-reporter "Exporting roam notes"
                                                        0
                                                        (length files-to-export)))
             (start-time (current-time))
             (org-id-extra-files (org-roam-list-files))
             (i 0))
        ;; Speed up the export
        (remove-hook! 'org-mode-hook #'org-fancy-priorities-mode)
        (advice-add 'org-id-find :override 'org-roam-id-find)
        (memoize 'org-roam-node-id)
        (memoize 'org-roam-node-file)

        (save-excursion
          (mapc
           (lambda (filepath)
             (let ((inner-start-time (current-time))
                   (inhibit-message t))
               (message "cashpw/org-hugo-export-all (%d/%d) exporting [%s]"
                        (1+ i)
                        count-files-to-export
                        filepath)
               (with-current-buffer (find-file-noselect filepath)
                 (ignore-errors
                 (org-hugo-export-to-md)))
               (message "cashpw/org-hugo-export-all (%d/%d) exported [%s] %.06f"
                        (1+ i)
                        count-files-to-export
                        filepath
                        (float-time (time-since inner-start-time))))
             (progress-reporter-update progress-reporter
                                       i)
             (setq i (1+ i)))
           files-to-export))
        (progress-reporter-done progress-reporter)

        ;; Remove speed-up changes
        (add-hook! 'org-mode-hook #'org-fancy-priorities-mode)
        (advice-remove 'org-id-find 'org-roam-id-find)
        (memoize-restore 'org-roam-node-id)
        (memoize-restore 'org-roam-node-file)

        (message "cashpw/org-hugo-export-all %.06f" (float-time (time-since start-time)))))))

(defun cashpw/org-mode--split-tags-to-list (tags-as-string)
  "Strip the wrapping ':' from TAG; if present."
  (if tags-as-string
      (if (string-match
           "^:\\(.*\\):$"
           tags-as-string)
          (split-string
           (match-string 1 tags-as-string)
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
   org-hugo-allow-spaces-in-tags nil)
  (add-to-list
   'org-hugo-tag-processing-functions
   'cashpw/org-hugo--tag-processing-fn-roam-tags)
  ;; Speed up exporting files
  ;; (advice-add 'org-id-find :override 'org-roam-id-find)
  ;; (memoize 'org-roam-node-id)
  ;; (memoize 'org-roam-node-file)
  )

(after! org
  (setq
   org-publish-project-alist
   '(("cashweaver.com"
      :base-directory "~/proj/blog-posts/posts/"
      :base-extension "org"
      :publishing-directory "~/proj/cashweaver.com/content/posts/"
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
         cashpw/home-dir-path)
 cashpw/bibliographies `(,cashpw/path--roam-bibliography))

(use-package! citar
  :when (modulep! :completion vertico)
  :config
  (setq
   citar-bibliography cashpw/bibliographies
   citar-symbols `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
                   (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
                   (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))
   citar-symbol-separator "  "
   citar-notes-paths `(,cashpw/roam-dir-path)
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
    :n "e" #'org-set-effort)
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
            cashpw/home-dir-path)))
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
