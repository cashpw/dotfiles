(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((eval add-hook! 'before-save-hook :local #'cashpw/contacts-create-birthday-reminder)
     (eval progn
      (defun cashpw/format-days-hours-minutes
          (days hours minutes)
        (string-join
         (remove nil
                 `(,(if
                        (> days 0)
                        (s-lex-format "${days}d"))
                   ,(if
                        (> hours 0)
                        (s-lex-format "${hours}h"))
                   ,(if
                        (> minutes 0)
                        (s-lex-format "${minutes}m"))))
         " "))
      (defun cashpw/get-property
          (property)
        (save-excursion
          (goto-char
           (point-min))
          (org-entry-get
           (point)
           property)))
      (defun cashpw/split-aliases-to-string
          (roam-aliases)
        (mapcar
         (lambda
           (roam-alias)
           (downcase
            (replace-regexp-in-string "\"" ""
                                      (replace-regexp-in-string " " "-" roam-alias))))
         (split-string roam-aliases "\" \"" nil)))
      (defun cashpw/get-aliases nil
        (interactive)
        (let*
            ((roam-aliases
              (cashpw/get-property "ROAM_ALIASES"))
             (aliases
              (if roam-aliases
                  (cashpw/split-aliases-to-string roam-aliases)
                'nil)))
          (string-join
           (mapcar
            (lambda
              (roam-alias)
              (s-lex-format "/posts/${roam-alias}"))
            aliases)
           " ")))
      (defun cashpw/set-custom-front-matter nil
        (interactive)
        (let*
            ((prep-duration
              (org-roam-recipe-get-prep-duration))
             (cook-duration
              (org-roam-recipe-get-cook-duration))
             (total-duration
              (org-roam-recipe-get-total-duration))
             (yield
              (org-roam-recipe-get-yield))
             (stars
              (org-roam-recipe-get-stars))
             (servings
              (org-roam-recipe-get-servings))
             (id
              (cashpw/get-property "ID"))
             (properties
              `(("prep_time" \, prep-duration)
                ("cook_time" \, cook-duration)
                ("total_time" \, total-duration)
                ("servings" \, servings)
                ("stars" \, stars)
                ("yield" \, yield)
                ("slug" \, id)))
             (front-matter
              (string-join
               (mapcar
                (lambda
                  (item)
                  (destructuring-bind
                      (label . value)
                      item
                    (s-lex-format ":${label} \"${value}\"")))
                (--filter
                 (cdr it)
                 properties))
               " ")))
          (cashpw/org-hugo--set-custom-front-matter front-matter)))
      (add-hook! 'before-save-hook :local #'cashpw/org-roam-before-save)
      (add-hook! 'before-save-hook :local #'cashpw/org-set-last-modified)
      (add-hook! 'before-save-hook :local #'cashpw/set-custom-front-matter))
     (eval setq-local org-default-properties
      (append org-default-properties org-roam-recipe--properties)
      org-roam-directory
      (if
          (file-exists-p
           (concat default-directory ".dir-locals.el"))
          (expand-file-name
           (locate-dominating-file default-directory ".dir-locals.el"))
        nil)
      org-attach-directory
      (file-truename
       (format "%s/attachments/" org-roam-directory))
      org-roam-db-location
      (expand-file-name "org-roam.db" org-roam-directory)
      org-hugo-auto-set-lastmod t org-directory org-roam-directory cashpw/org-roam--file-path-exceptions-to-export-after-save
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory))
      cashpw/org-roam--file-path-exceptions-to-add-bibliography
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory)))
     (eval progn
      (defun cashpw/format-days-hours-minutes
          (days hours minutes)
        (string-join
         (remove nil
                 `(,(if
                        (> days 0)
                        (s-lex-format "${days}d"))
                   ,(if
                        (> hours 0)
                        (s-lex-format "${hours}h"))
                   ,(if
                        (> minutes 0)
                        (s-lex-format "${minutes}m"))))
         " "))
      (defun cashpw/get-prep-minutes nil
        (string-to-number
         (or
          (save-excursion
            (goto-char
             (point-min))
            (org-entry-get
             (point)
             "PREP_MINUTES"))
          "0")))
      (defun cashpw/get-cook-minutes nil
        (string-to-number
         (or
          (save-excursion
            (goto-char
             (point-min))
            (org-entry-get
             (point)
             "COOK_MINUTES"))
          "0")))
      (defun cashpw/get-cook-time nil
        (let
            ((cook-minutes
              (cashpw/get-cook-minutes)))
          (cashpw/get-recipe-time cook-minutes)))
      (defun cashpw/get-prep-time nil
        (let
            ((prep-minutes
              (cashpw/get-prep-minutes)))
          (cashpw/get-recipe-time prep-minutes)))
      (defun cashpw/get-total-time nil
        (let*
            ((prep-minutes
              (cashpw/get-prep-minutes))
             (cook-minutes
              (cashpw/get-cook-minutes))
             (total-minutes
              (+ prep-minutes cook-minutes)))
          (cashpw/get-recipe-time total-minutes)))
      (defun cashpw/get-recipe-time
          (minutes)
        (let*
            ((minutes-in-hour 60)
             (minutes-in-day
              (* 24 minutes-in-hour))
             (days
              (/ minutes minutes-in-day))
             (minutes
              (if days
                  (- minutes
                     (* minutes-in-day days))
                minutes))
             (hours
              (/ minutes minutes-in-hour))
             (minutes
              (if hours
                  (- minutes
                     (* minutes-in-hour hours))
                minutes)))
          (cashpw/format-days-hours-minutes days hours minutes)))
      (defun cashpw/get-property
          (property)
        (save-excursion
          (goto-char
           (point-min))
          (org-entry-get
           (point)
           property)))
      (defun cashpw/split-aliases-to-string
          (roam-aliases)
        (mapcar
         (lambda
           (roam-alias)
           (downcase
            (replace-regexp-in-string "\"" ""
                                      (replace-regexp-in-string " " "-" roam-alias))))
         (split-string roam-aliases "\" \"" nil)))
      (defun cashpw/get-aliases nil
        (interactive)
        (let*
            ((roam-aliases
              (cashpw/get-property "ROAM_ALIASES"))
             (aliases
              (if roam-aliases
                  (cashpw/split-aliases-to-string roam-aliases)
                'nil)))
          (string-join
           (mapcar
            (lambda
              (roam-alias)
              (s-lex-format "/posts/${roam-alias}"))
            aliases)
           " ")))
      (defun cashpw/set-custom-front-matter nil
        (interactive)
        (let*
            ((prep-time
              (cashpw/get-prep-time))
             (cook-time
              (cashpw/get-cook-time))
             (total-time
              (cashpw/get-total-time))
             (servings
              (cashpw/get-property "SERVINGS"))
             (id
              (cashpw/get-property "ID"))
             (stars
              (cashpw/get-property "STARS"))
             (front-matter
              (-filter
               (lambda
                 (item)
                 (destructuring-bind
                     (label . value)
                     item
                   (length> value 0)))
               `(("prep_time" \, prep-time)
                 ("cook_time" \, cook-time)
                 ("total_time" \, total-time)
                 ("servings" \, servings)
                 ("slug" \, id)
                 ("stars" \, stars)))))
          (cashpw/org-hugo--set-custom-front-matter
           (string-join
            (mapcar
             (lambda
               (item)
               (destructuring-bind
                   (label . value)
                   item
                 (s-lex-format ":${label} \"${value}\"")))
             front-matter)
            " "))))
      (add-hook! 'before-save-hook :local #'cashpw/org-roam-before-save)
      (add-hook! 'before-save-hook :local #'cashpw/org-set-last-modified)
      (add-hook! 'before-save-hook :local #'cashpw/set-custom-front-matter))
     (eval setq-local org-export-with-priority nil org-export-with-todo-keywords nil)
     (eval setq-local org-roam-directory
      (if
          (file-exists-p
           (concat default-directory ".dir-locals.el"))
          (expand-file-name
           (locate-dominating-file default-directory ".dir-locals.el"))
        nil)
      org-attach-directory
      (file-truename
       (format "%s/attachments/" org-roam-directory))
      org-roam-db-location
      (expand-file-name "org-roam.db" org-roam-directory)
      org-hugo-auto-set-lastmod t)
     (eval add-hook 'cashpw/org-mode-done-cut-hook 'org-roam-file-p)
     (eval setq-local org-default-properties
      '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
      org-roam-directory
      (if
          (file-exists-p
           (concat default-directory ".dir-locals.el"))
          (expand-file-name
           (locate-dominating-file default-directory ".dir-locals.el"))
        nil)
      org-attach-directory
      (file-truename
       (format "%s/attachments/" org-roam-directory))
      org-roam-db-location
      (expand-file-name "org-roam.db" org-roam-directory)
      org-hugo-auto-set-lastmod t org-directory org-roam-directory cashpw/org-roam--file-path-exceptions-to-export-after-save
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory))
      cashpw/org-roam--file-path-exceptions-to-add-bibliography
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory)))
     (eval setq-local org-capture-template--todo
      `("Todo" :keys "t" :file ,(lambda nil
                                  (let
                                      ((directory-path
                                        (expand-file-name
                                         (locate-dominating-file default-directory ".dir-locals.el"))))
                                    (s-lex-format "${directory-path}/todos.org")))
        :template
        ("* TODO [#2] %?" ":PROPERTIES:" ":CREATED: %U" ":END:"))
      org-capture-templates
      (doct
       `(,org-capture-template--todo)))
     (eval setq-local org-roam-db-location
      (expand-file-name "org-roam.db" org-roam-directory))
     (eval add-hook! 'before-save-hook :local #'cashpw/contacts--create-birthday-reminder)
     (eval add-hook! 'before-save-hook :local #'cashpw/org-set-last-modified)
     (eval add-hook! 'before-save-hook :local #'cashpw/org-roam-before-save)
     (eval setq-local org-default-properties
      '("ADDRESS" "BIRTHDAY" "CELL" "DATE_MET" "DISCORD" "EMAIL_HOME" "EMAIL_WORK" "EMPLOYER" "FOOD_PREFERENCE" "JOB_TITLE" "LAST_MODIFIED" "LINKEDIN" "NICKNAME" "PHONE_WORK" "TWITTER" "WEBSITE" "WORK_PHONE" "ARCHIVE" "CATEGORY" "SUMMARY" "DESCRIPTION" "CUSTOM_ID" "LOCATION" "LOGGING" "COLUMNS" "VISIBILITY" "TABLE_EXPORT_FORMAT" "TABLE_EXPORT_FILE" "EXPORT_OPTIONS" "EXPORT_TEXT" "EXPORT_FILE_NAME" "EXPORT_TITLE" "EXPORT_AUTHOR" "EXPORT_DATE" "UNNUMBERED" "ORDERED" "NOBLOCKING" "COOKIE_DATA" "LOG_INTO_DRAWER" "REPEAT_TO_STATE" "CLOCK_MODELINE_TOTAL" "STYLE" "HTML_CONTAINER_CLASS" "ORG-IMAGE-ACTUAL-WIDTH")
      org-capture-template--org-fc-normal
      `("Normal" :keys "n" :file ,(lambda nil
                                    (buffer-name))
        :olp
        ("Flashcards")
        :template
        ("* %^{Name of card}" ":PROPERTIES:" ":CREATED: %U" ":END:" "" "%?" "" "** TODO Back" "** TODO Source")
        :prepare-finalize ,(lambda nil
                             (goto-char
                              (point-min))
                             (org-fc-type-normal-init)))
      org-capture-template--org-fc-double
      `("Double" :keys "d" :file ,(lambda nil
                                    (buffer-name))
        :olp
        ("Flashcards")
        :template
        ("* %^{Name of card}" ":PROPERTIES:" ":CREATED: %U" ":END:" "" "%?" "" "** TODO Back" "** TODO Source")
        :prepare-finalize ,(lambda nil
                             (goto-char
                              (point-min))
                             (org-fc-type-double-init)))
      org-capture-template--org-fc-cloze
      `("Cloze" :keys "c" :file ,(lambda nil
                                   (buffer-name))
        :olp
        ("Flashcards")
        :template
        ("* %^{Name of card}" ":PROPERTIES:" ":CREATED: %U" ":END:" "" "- %?" "" "** TODO Source")
        :prepare-finalize ,(lambda nil
                             (goto-char
                              (point-min))
                             (org-fc-type-cloze-init 'deletion)))
      org-capture-templates
      (doct
       `(,org-capture-template--org-fc-cloze ,org-capture-template--org-fc-double ,org-capture-template--org-fc-normal))
      org-roam-capture-templates
      `(("p" "person" plain "%?" :target
         (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "* About
" "* Photo
" "* Relationships
"))
         :unnarrowed t))
      org-roam-directory
      (expand-file-name
       (locate-dominating-file default-directory ".dir-locals.el"))
      org-attach-directory
      (file-truename
       (format "%s/attachments/" org-roam-directory))
      cashweaver-org-roam-attachment-base-path
      (file-truename
       (format "%s/attachments" org-roam-directory)))
     (eval setq-local org-refile-use-outline-path nil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
