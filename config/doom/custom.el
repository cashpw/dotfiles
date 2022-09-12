(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error-regexp-alist
   '(google3-build-log-parser-info google3-build-log-parser-warning google3-build-log-parser-error google3-build-log-parser-python-traceback bazel google-blaze-error google-blaze-warning google-log-error google-log-warning google-log-info google-log-fatal-message google-forge-python gunit-stack-trace absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek gradle-kotlin iar ibm irix java jikes-file maven jikes-line clang-include clang-include gcc-include ruby-Test::Unit gmake gnu lcc makepp mips-1 mips-2 omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line))
 '(safe-local-variable-values
<<<<<<< HEAD
   '((eval setq-local org-default-properties
=======
   '((eval setq-local org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory))
     (eval progn
           (defun cashweaver/format-days-hours-minutes
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
           (defun cashweaver/get-prep-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "PREP_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "COOK_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-time nil
             (let
                 ((cook-minutes
                   (cashweaver/get-cook-minutes)))
               (cashweaver/get-recipe-time cook-minutes)))
           (defun cashweaver/get-prep-time nil
             (let
                 ((prep-minutes
                   (cashweaver/get-prep-minutes)))
               (cashweaver/get-recipe-time prep-minutes)))
           (defun cashweaver/get-total-time nil
             (let*
                 ((prep-minutes
                   (cashweaver/get-prep-minutes))
                  (cook-minutes
                   (cashweaver/get-cook-minutes))
                  (total-minutes
                   (+ prep-minutes cook-minutes)))
               (cashweaver/get-recipe-time total-minutes)))
           (defun cashweaver/get-recipe-time
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
               (cashweaver/format-days-hours-minutes days hours minutes)))
           (defun cashweaver/get-property
               (property)
             (save-excursion
               (goto-char
                (point-min))
               (org-entry-get
                (point)
                property)))
           (defun cashweaver/split-aliases-to-string
               (roam-aliases)
             (mapcar
              (lambda
                (roam-alias)
                (downcase
                 (replace-regexp-in-string "\"" ""
                                           (replace-regexp-in-string " " "-" roam-alias))))
              (split-string roam-aliases "\" \"" nil)))
           (defun cashweaver/get-aliases nil
             (interactive)
             (let*
                 ((roam-aliases
                   (cashweaver/get-property "ROAM_ALIASES"))
                  (aliases
                   (if roam-aliases
                       (cashweaver/split-aliases-to-string roam-aliases)
                     'nil)))
               (string-join
                (mapcar
                 (lambda
                   (roam-alias)
                   (s-lex-format "/posts/${roam-alias}"))
                 aliases)
                " ")))
           (defun cashweaver/set-custom-front-matter nil
             (interactive)
             (let*
                 ((prep-time
                   (cashweaver/get-prep-time))
                  (cook-time
                   (cashweaver/get-cook-time))
                  (total-time
                   (cashweaver/get-total-time))
                  (servings
                   (cashweaver/get-property "SERVINGS"))
                  (id
                   (cashweaver/get-property "ID"))
                  (stars
                   (cashweaver/get-property "STARS"))
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
               (cashweaver/org-hugo--set-custom-front-matter
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
           (org-hugo-auto-export-mode)
           (cashweaver/enable-anki-editor-mode)
           (add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save)
           (add-hook! 'before-save-hook :local #'cashweaver/set-custom-front-matter))
     (eval progn
           (defun cashweaver/format-days-hours-minutes
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
           (defun cashweaver/get-prep-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "PREP_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "COOK_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-time nil
             (let
                 ((cook-minutes
                   (cashweaver/get-cook-minutes)))
               (cashweaver/get-recipe-time cook-minutes)))
           (defun cashweaver/get-prep-time nil
             (let
                 ((prep-minutes
                   (cashweaver/get-prep-minutes)))
               (cashweaver/get-recipe-time prep-minutes)))
           (defun cashweaver/get-total-time nil
             (let*
                 ((prep-minutes
                   (cashweaver/get-prep-minutes))
                  (cook-minutes
                   (cashweaver/get-cook-minutes))
                  (total-minutes
                   (+ prep-minutes cook-minutes)))
               (cashweaver/get-recipe-time total-minutes)))
           (defun cashweaver/get-recipe-time
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
               (cashweaver/format-days-hours-minutes days hours minutes)))
           (defun cashweaver/get-property
               (property)
             (save-excursion
               (goto-char
                (point-min))
               (org-entry-get
                (point)
                property)))
           (defun cashweaver/split-aliases-to-string
               (roam-aliases)
             (mapcar
              (lambda
                (roam-alias)
                (downcase
                 (replace-regexp-in-string "\"" ""
                                           (replace-regexp-in-string " " "-" roam-alias))))
              (split-string roam-aliases "\" \"" nil)))
           (defun cashweaver/get-aliases nil
             (interactive)
             (let*
                 ((roam-aliases
                   (cashweaver/get-property "ROAM_ALIASES"))
                  (aliases
                   (if roam-aliases
                       (cashweaver/split-aliases-to-string roam-aliases)
                     'nil)))
               (string-join
                (mapcar
                 (lambda
                   (roam-alias)
                   (s-lex-format "/posts/${roam-alias}"))
                 aliases)
                " ")))
           (defun cashweaver/set-custom-front-matter nil
             (interactive)
             (let*
                 ((prep-time
                   (cashweaver/get-prep-time))
                  (cook-time
                   (cashweaver/get-cook-time))
                  (total-time
                   (cashweaver/get-total-time))
                  (servings
                   (cashweaver/get-property "SERVINGS"))
                  (id
                   (cashweaver/get-property "ID"))
                  (aliases
                   (cashweaver/get-aliases))
                  (stars
                   (cashweaver/get-property "STARS"))
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
                      ("aliases" \, aliases)
                      ("slug" \, id)
                      ("stars" \, stars)))))
               (cashweaver/org-hugo--set-custom-front-matter
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
           (org-hugo-auto-export-mode)
           (cashweaver/enable-anki-editor-mode)
           (add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save)
           (add-hook! 'before-save-hook :local #'cashweaver/set-custom-front-matter))
     (eval progn
           (defun cashweaver/format-days-hours-minutes
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
           (defun cashweaver/get-prep-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "PREP_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "COOK_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-time nil
             (let
                 ((cook-minutes
                   (cashweaver/get-cook-minutes)))
               (cashweaver/get-recipe-time cook-minutes)))
           (defun cashweaver/get-prep-time nil
             (let
                 ((prep-minutes
                   (cashweaver/get-prep-minutes)))
               (cashweaver/get-recipe-time prep-minutes)))
           (defun cashweaver/get-total-time nil
             (let*
                 ((prep-minutes
                   (cashweaver/get-prep-minutes))
                  (cook-minutes
                   (cashweaver/get-cook-minutes))
                  (total-minutes
                   (+ prep-minutes cook-minutes)))
               (cashweaver/get-recipe-time total-minutes)))
           (defun cashweaver/get-recipe-time
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
               (cashweaver/format-days-hours-minutes days hours minutes)))
           (defun cashweaver/get-property
               (property)
             (save-excursion
               (goto-char
                (point-min))
               (org-entry-get
                (point)
                property)))
           (defun cashweaver/split-aliases-to-string
               (roam-aliases)
             (mapcar
              (lambda
                (roam-alias)
                (downcase
                 (replace-regexp-in-string "\"" ""
                                           (replace-regexp-in-string " " "-" roam-alias))))
              (split-string roam-aliases "\" \"" nil)))
           (defun cashweaver/get-aliases nil
             (interactive)
             (let*
                 ((id
                   (cashweaver/get-property "ID"))
                  (roam-aliases
                   (cashweaver/get-property "ROAM_ALIASES"))
                  (aliases
                   (append
                    `(,id)
                    (if roam-aliases
                        (cashweaver/split-aliases-to-string roam-aliases)
                      'nil))))
               (string-join
                (mapcar
                 (lambda
                   (roam-alias)
                   (s-lex-format "/posts/${roam-alias}"))
                 aliases)
                " ")))
           (defun cashweaver/set-custom-front-matter nil
             (interactive)
             (let*
                 ((prep-time
                   (cashweaver/get-prep-time))
                  (cook-time
                   (cashweaver/get-cook-time))
                  (total-time
                   (cashweaver/get-total-time))
                  (servings
                   (cashweaver/get-property "SERVINGS"))
                  (aliases
                   (cashweaver/get-aliases))
                  (stars
                   (cashweaver/get-property "STARS"))
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
                      ("aliases" \, aliases)
                      ("stars" \, stars)))))
               (cashweaver/org-hugo--set-custom-front-matter
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
           (org-hugo-auto-export-mode)
           (cashweaver/enable-anki-editor-mode)
           (add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save)
           (add-hook! 'before-save-hook :local #'cashweaver/set-custom-front-matter))
     (eval progn
           (defun cashweaver/format-days-hours-minutes
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
           (defun cashweaver/get-prep-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "PREP_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "COOK_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-time nil
             (let
                 ((cook-minutes
                   (cashweaver/get-cook-minutes)))
               (cashweaver/get-recipe-time cook-minutes)))
           (defun cashweaver/get-prep-time nil
             (let
                 ((prep-minutes
                   (cashweaver/get-prep-minutes)))
               (cashweaver/get-recipe-time prep-minutes)))
           (defun cashweaver/get-total-time nil
             (let*
                 ((prep-minutes
                   (cashweaver/get-prep-minutes))
                  (cook-minutes
                   (cashweaver/get-cook-minutes))
                  (total-minutes
                   (+ prep-minutes cook-minutes)))
               (cashweaver/get-recipe-time total-minutes)))
           (defun cashweaver/get-recipe-time
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
               (cashweaver/format-days-hours-minutes days hours minutes)))
           (defun cashweaver/get-property
               (property)
             (save-excursion
               (goto-char
                (point-min))
               (org-entry-get
                (point)
                property)))
           (defun cashweaver/get-aliases nil
             (interactive)
             (let*
                 ((id
                   (cashweaver/get-property "ID"))
                  (roam-aliases
                   (cashweaver/get-property "ROAM_ALIASES"))
                  (aliases
                   (append
                    `(,id)
                    (mapcar
                     (lambda
                       (roam-alias)
                       (downcase
                        (replace-regexp-in-string "\"" ""
                                                  (replace-regexp-in-string " " "-" roam-alias))))
                     (split-string roam-aliases "\" \"" nil)))))
               (string-join
                (mapcar
                 (lambda
                   (roam-alias)
                   (s-lex-format "/posts/${roam-alias}"))
                 aliases)
                " ")))
           (defun cashweaver/set-custom-front-matter nil
             (interactive)
             (let*
                 ((prep-time
                   (cashweaver/get-prep-time))
                  (cook-time
                   (cashweaver/get-cook-time))
                  (total-time
                   (cashweaver/get-total-time))
                  (servings
                   (cashweaver/get-property "SERVINGS"))
                  (aliases
                   (cashweaver/get-aliases))
                  (stars
                   (cashweaver/get-property "STARS"))
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
                      ("aliases" \, aliases)
                      ("stars" \, stars)))))
               (cashweaver/org-hugo--set-custom-front-matter
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
           (org-hugo-auto-export-mode)
           (cashweaver/enable-anki-editor-mode)
           (add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save)
           (add-hook! 'before-save-hook :local #'cashweaver/set-custom-front-matter))
     (eval progn
           (defun cashweaver/format-days-hours-minutes
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
           (defun cashweaver/get-prep-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "PREP_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "COOK_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-time nil
             (let
                 ((cook-minutes
                   (cashweaver/get-cook-minutes)))
               (cashweaver/get-recipe-time cook-minutes)))
           (defun cashweaver/get-prep-time nil
             (let
                 ((prep-minutes
                   (cashweaver/get-prep-minutes)))
               (cashweaver/get-recipe-time prep-minutes)))
           (defun cashweaver/get-total-time nil
             (let*
                 ((prep-minutes
                   (cashweaver/get-prep-minutes))
                  (cook-minutes
                   (cashweaver/get-cook-minutes))
                  (total-minutes
                   (+ prep-minutes cook-minutes)))
               (cashweaver/get-recipe-time total-minutes)))
           (defun cashweaver/get-recipe-time
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
               (cashweaver/format-days-hours-minutes days hours minutes)))
           (defun cashweaver/get-property
               (property)
             (save-excursion
               (goto-char
                (point-min))
               (org-entry-get
                (point)
                property)))
           (defun cashweaver/set-custom-front-matter nil
             (interactive)
             (let*
                 ((prep-time
                   (cashweaver/get-prep-time))
                  (cook-time
                   (cashweaver/get-cook-time))
                  (total-time
                   (cashweaver/get-total-time))
                  (servings
                   (cashweaver/get-property "SERVINGS"))
                  (id
                   (cashweaver/get-property "ID"))
                  (stars
                   (cashweaver/get-property "STARS"))
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
                      ("aliases" \,
                       (s-lex-format "/posts/${id}"))
                      ("stars" \, stars)))))
               (cashweaver/org-hugo--set-custom-front-matter
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
           (org-hugo-auto-export-mode)
           (cashweaver/enable-anki-editor-mode)
           (add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save)
           (add-hook! 'before-save-hook :local #'cashweaver/set-custom-front-matter))
     (eval progn
           (defun cashweaver/format-days-hours-minutes
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
           (defun cashweaver/get-prep-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "PREP_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "COOK_MINUTES"))
               "0")))
           (defun cashweaver/get-cook-time nil
             (let
                 ((cook-minutes
                   (cashweaver/get-cook-minutes)))
               (cashweaver/get-recipe-time cook-minutes)))
           (defun cashweaver/get-prep-time nil
             (let
                 ((prep-minutes
                   (cashweaver/get-prep-minutes)))
               (cashweaver/get-recipe-time prep-minutes)))
           (defun cashweaver/get-total-time nil
             (let*
                 ((prep-minutes
                   (cashweaver/get-prep-minutes))
                  (cook-minutes
                   (cashweaver/get-cook-minutes))
                  (total-minutes
                   (+ prep-minutes cook-minutes)))
               (cashweaver/get-recipe-time total-minutes)))
           (defun cashweaver/get-recipe-time
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
               (cashweaver/format-days-hours-minutes days hours minutes)))
           (defun cashweaver/get-property
               (property)
             (save-excursion
               (goto-char
                (point-min))
               (org-entry-get
                (point)
                property)))
           (defun cashweaver/set-custom-front-matter nil
             (interactive)
             (let*
                 ((prep-time
                   (cashweaver/get-prep-time))
                  (cook-time
                   (cashweaver/get-cook-time))
                  (total-time
                   (cashweaver/get-total-time))
                  (servings
                   (cashweaver/get-property "SERVINGS"))
                  (id
                   (cashweaver/get-property "ID"))
                  (stars
                   (cashweaver/get-property "STARS"))
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
               (cashweaver/org-hugo--set-custom-front-matter
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
           (org-hugo-auto-export-mode)
           (cashweaver/enable-anki-editor-mode)
           (add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save)
           (add-hook! 'before-save-hook :local #'cashweaver/set-custom-front-matter))
     (eval progn
           (defun roam-recipes/format-days-hours-minutes
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
           (defun roam-recipes/get-prep-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "PREP_MINUTES"))
               "0")))
           (defun roam-recipes/get-cook-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "COOK_MINUTES"))
               "0")))
           (defun roam-recipes/get-cook-time nil
             (let
                 ((cook-minutes
                   (roam-recipes/get-cook-minutes)))
               (roam-recipes/get-recipe-time cook-minutes)))
           (defun roam-recipes/get-prep-time nil
             (let
                 ((prep-minutes
                   (roam-recipes/get-prep-minutes)))
               (roam-recipes/get-recipe-time prep-minutes)))
           (defun roam-recipes/get-total-time nil
             (let*
                 ((prep-minutes
                   (roam-recipes/get-prep-minutes))
                  (cook-minutes
                   (roam-recipes/get-cook-minutes))
                  (total-minutes
                   (+ prep-minutes cook-minutes)))
               (roam-recipes/get-recipe-time total-minutes)))
           (defun roam-recipes/get-recipe-time
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
               (roam-recipes/format-days-hours-minutes days hours minutes)))
           (defun roam-recipes/get-property
               (property)
             (save-excursion
               (goto-char
                (point-min))
               (org-entry-get
                (point)
                property)))
           (defun roam-recipes/set-custom-front-matter nil
             (interactive)
             (let*
                 ((prep-time
                   (roam-recipes/get-prep-time))
                  (cook-time
                   (roam-recipes/get-cook-time))
                  (total-time
                   (roam-recipes/get-total-time))
                  (servings
                   (roam-recipes/get-property "SERVINGS"))
                  (stars
                   (roam-recipes/get-property "STARS"))
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
                      ("stars" \, stars)))))
               (cashweaver/org-hugo--set-custom-front-matter
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
           (org-hugo-auto-export-mode)
           (cashweaver/enable-anki-editor-mode)
           (add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save)
           (add-hook! 'before-save-hook :local #'roam-recipes/set-custom-front-matter))
     (eval setq-local org-default-properties
           '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
           org-roam-capture-templates
           `(("c" "concept" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :concept:")
                                         "
"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "Among other things:
" "* TODO [#4] :noexport:"))
              :unnarrowed t)
             ("P" "poem" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :poem:
" "#+begin_verse
" "#+end_verse
"))
              :unnarrowed t)
             ("q" "quote" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :quote:
" "#+begin_quote
" "#+end_quote
"))
              :unnarrowed t)
             ("r" "recipe" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :recipe:" "
" "* TODO [#2] Ingredients" "* TODO [#2] Steps")
                                         "
"))
              :unnarrowed t)
             ("i" "ingredient" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :ingredient:" "
" "* TODO [#2] Ingredients" "* TODO [#2] Steps")
                                         "
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory))
           cashweaver/org-roam--file-path-exceptions-to-add-bibliography
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory)))
     (eval progn
           (org-hugo-auto-export-mode)
           (cashweaver/enable-anki-editor-mode)
           (defun roam-recipes/format-days-hours-minutes
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
           (defun roam-recipes/get-prep-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "PREP_MINUTES"))
               "0")))
           (defun roam-recipes/get-cook-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "COOK_MINUTES"))
               "0")))
           (defun roam-recipes/get-cook-time nil
             (let
                 ((cook-minutes
                   (roam-recipes/get-cook-minutes)))
               (roam-recipes/get-recipe-time cook-minutes)))
           (defun roam-recipes/get-prep-time nil
             (let
                 ((prep-minutes
                   (roam-recipes/get-prep-minutes)))
               (roam-recipes/get-recipe-time prep-minutes)))
           (defun roam-recipes/get-total-time nil
             (let*
                 ((prep-minutes
                   (roam-recipes/get-prep-minutes))
                  (cook-minutes
                   (roam-recipes/get-cook-minutes))
                  (total-minutes
                   (+ prep-minutes cook-minutes)))
               (roam-recipes/get-recipe-time total-minutes)))
           (defun roam-recipes/get-recipe-time
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
               (roam-recipes/format-days-hours-minutes days hours minutes)))
           (defun roam-recipes/get-property
               (property)
             (save-excursion
               (goto-char
                (point-min))
               (org-entry-get
                (point)
                property)))
           (defun roam-recipes/set-custom-front-matter nil
             (interactive)
             (let*
                 ((prep-time
                   (roam-recipes/get-prep-time))
                  (cook-time
                   (roam-recipes/get-cook-time))
                  (total-time
                   (roam-recipes/get-total-time))
                  (servings
                   (roam-recipes/get-property "SERVINGS"))
                  (stars
                   (roam-recipes/get-property "STARS"))
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
                      ("stars" \, stars)))))
               (cashweaver/org-hugo--set-custom-front-matter
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
           (add-hook! 'before-save-hook :local #'roam-recipes/set-custom-front-matter)
           (add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save))
     (eval setq-local org-default-properties
           '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
           org-roam-capture-templates
           `(("r" "recipe" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :recipe:" "
" "* TODO [#2] Ingredients" "* TODO [#2] Steps")
                                         "
"))
              :unnarrowed t)
             ("i" "ingredient" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :ingredient:" "
" "* TODO [#2] Ingredients" "* TODO [#2] Steps")
                                         "
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t)
     (eval setq-local org-default-properties
           '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
           org-roam-capture-templates
           `(("r" "recipe" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :recipe:" "
" "* TODO [#2] Ingredients" "* TODO [#2] Steps")
                                         "
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t)
     (eval setq-local org-export-with-priority nil org-export-with-todo-keywords nil)
     (eval progn
           (org-hugo-auto-export-mode)
           (cashweaver/enable-anki-editor-mode)
           (defun roam-recipes/format-days-hours-minutes
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
           (defun roam-recipes/get-prep-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "PREP_MINUTES"))
               "0")))
           (defun roam-recipes/get-cook-minutes nil
             (string-to-number
              (or
               (save-excursion
                 (goto-char
                  (point-min))
                 (org-entry-get
                  (point)
                  "COOK_MINUTES"))
               "0")))
           (defun roam-recipes/get-cook-time nil
             (let
                 ((cook-minutes
                   (roam-recipes/get-cook-minutes)))
               (roam-recipes/get-recipe-time cook-minutes)))
           (defun roam-recipes/get-prep-time nil
             (let
                 ((prep-minutes
                   (roam-recipes/get-prep-minutes)))
               (roam-recipes/get-recipe-time prep-minutes)))
           (defun roam-recipes/get-total-time nil
             (let*
                 ((prep-minutes
                   (roam-recipes/get-prep-minutes))
                  (cook-minutes
                   (roam-recipes/get-cook-minutes))
                  (total-minutes
                   (+ prep-minutes cook-minutes)))
               (roam-recipes/get-recipe-time total-minutes)))
           (defun roam-recipes/get-recipe-time
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
               (roam-recipes/format-days-hours-minutes days hours minutes)))
           (defun roam-recipes/get-ingredients nil
             (let*
                 ((ingredients-raw
                   (save-excursion
                     (goto-char
                      (point-min))
                     (org-entry-get
                      (point)
                      "INGREDIENTS")))
                  (ingredients
                   (replace-regexp-in-string "; " ";" ingredients-raw)))
               ingredients))
           (defun roam-recipes/get-servings nil
             (save-excursion
               (goto-char
                (point-min))
               (org-entry-get
                (point)
                "SERVINGS")))
           (defun roam-recipes/set-custom-front-matter nil
             (interactive)
             (let
                 ((prep-time
                   (roam-recipes/get-prep-time))
                  (cook-time
                   (roam-recipes/get-cook-time))
                  (total-time
                   (roam-recipes/get-total-time))
                  (servings
                   (roam-recipes/get-servings))
                  (ingredients
                   (roam-recipes/get-ingredients)))
               (cashweaver/org-hugo--set-custom-front-matter
                (string-join
                 `(,(s-lex-format ":prep_time \"${prep-time}\"")
                   ,(s-lex-format ":cook_time \"${cook-time}\"")
                   ,(s-lex-format ":total_time \"${total-time}\"")
                   ,(s-lex-format ":servings \"${servings}\"")
                   ,(s-lex-format ":ingredients \"${ingredients}\""))
                 " "))))
           (add-hook! 'before-save-hook :local #'roam-recipes/set-custom-front-matter)
           (add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save))
     (eval setq-local org-default-properties
           '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "INGREDIENTS")
           org-roam-capture-templates
           `(("r" "recipe" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :recipe:" "
" "* TODO [#2] Ingredients" "* TODO [#2] Steps")
                                         "
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t)
     (eval setq-local org-default-properties
           '("PREP_MINUTES" "COOK_MINUTES")
           org-roam-capture-templates
           `(("r" "recipe" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :recipe:" "
" "* TODO [#2] Ingredients" "* TODO [#2] Steps")
                                         "
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t)
     (eval setq-local org-roam-capture-templates
           `(("r" "recipe" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :recipe:" "
" "* TODO [#2] Ingredients" "* TODO [#2] Steps")
                                         "
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t)
     (eval setq-local org-roam-capture-templates
           `(("r" "recipe" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :recipe:")
                                         "
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t)
     (eval setq-local org-roam-capture-templates
           `(("c" "concept" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :concept:")
                                         "
"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "Among other things:
" "* TODO [#4] :noexport:"))
              :unnarrowed t)
             ("P" "poem" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :poem:
" "#+begin_verse
" "#+end_verse
"))
              :unnarrowed t)
             ("q" "quote" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :quote:
" "#+begin_quote
" "#+end_quote
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t)
     (eval setq-local org-default-properties
>>>>>>> 5d4efbd58cbe99746db1e14a8d25aad464adc2e3
           '("ADDRESS" "BIRTHDAY" "CELL" "DATE_MET" "DISCORD" "EMAIL_HOME" "EMAIL_WORK" "EMPLOYER" "FOOD_PREFERENCE" "JOB_TITLE" "LAST_MODIFIED" "LINKEDIN" "NICKNAME" "PHONE_WORK" "TWITTER" "WEBSITE" "WORK_PHONE")
           org-roam-capture-templates
           `(("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
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
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1))
     (eval setq-local org-roam-capture-templates
           `(("c" "concept" plain "%?" :target
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :concept:")
                                         "
"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "Among other things:
" "* TODO [#4] :noexport:"))
              :unnarrowed t)
             ("P" "poem" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :poem:
" "#+begin_verse
" "#+end_verse
"))
              :unnarrowed t)
             ("q" "quote" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :quote:
" "#+begin_quote
" "#+end_quote
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory))
           cashweaver/org-roam--file-path-exceptions-to-add-bibliography
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory)))
     (eval setq-local org-roam-capture-templates
           `(("c" "concept" plain "%?" :target <<<<<<< HEAD
              (file+head "${slug}.org" ,(string-join
                                         '("#+title: ${title}" "#+author: Cash Weaver" "#+date: [%<%Y-%m-%d %a %H:%M>]" "#+filetags: :concept:")
                                         "
"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "Among other things:
" "* TODO [#4] :noexport:"))
              :unnarrowed t)
             ("P" "poem" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :poem:
" "#+begin_verse
" "#+end_verse
"))
              :unnarrowed t)
             ("q" "quote" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :quote:
" "#+begin_quote
" "#+end_quote
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory))
           cashweaver/org-roam--file-path-exceptions-to-add-bibliography
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory)))
     (eval setq-local org-roam-capture-templates
           `(("c" "concept" plain "%?" :target ======= >>>>>>> 81e6295551b7d04c38cf2e17621bb4f9456f6bd6
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :concept:
"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "Among other things:
" "* TODO [#4] :noexport:"))
              :unnarrowed t)
             ("P" "poem" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :poem:
" "#+begin_verse
" "#+end_verse
"))
              :unnarrowed t)
             ("q" "quote" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :quote:
" "#+begin_quote
" "#+end_quote
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory))
           cashweaver/org-roam--file-path-exceptions-to-add-bibliography
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory)))
     <<<<<<< HEAD
     (eval setq org-export-with-priority nil org-export-with-todo-keywords nil)
     (eval add-hook 'cashweaver/org-mode-done-cut-hook 'org-roam-file-p)
     =======
     (eval add-hook 'cashweaver/org-mode-done-cut-hook 'org-roam-file-p)
     (eval add-hook 'cashweaver/org-mode-done-noop-hook 'org-roam-file-p)
     (eval add-hook 'org-roam-file-p 'cashweaver/org-mode--done-in-cut-file-p)
     (eval add-hook
           (lambda nil
             (org-roam-file-p))
           'cashweaver/org-mode--done-in-cut-file-p)
     >>>>>>> 81e6295551b7d04c38cf2e17621bb4f9456f6bd6
     (eval setq-local org-roam-capture-templates
           `(("c" "concept" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :concept:
"))
              :unnarrowed t)
             ("d" "default" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "* TODO"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "Among other things:
" "* TODO"))
              :unnarrowed t)
             ("P" "poem" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :poem:
"))
              :unnarrowed t)
             ("q" "quote" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :quote:
" "#+begin_quote
" "TODO_QUOTE
" "
" "/[[https:foo][source]]/
" "#+end_quote
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory)
           org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory))
           cashweaver/org-roam--file-path-exceptions-to-add-bibliography
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory)))
     <<<<<<< HEAD =======
     (eval setq org-export-with-priority nil org-export-with-todo-keywords nil)
     >>>>>>> 81e6295551b7d04c38cf2e17621bb4f9456f6bd6
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save)
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (eval add-hook! 'before-save-hook :local #'cashweaver/contacts--create-birthday-reminder)
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-set-last-modified)
     (eval setq-local org-default-properties
           '("NICKNAME" "EMAIL_HOME" "EMAIL_WORK" "ADDRESS" "CELL" "PHONE_WORK" "WORK_PHONE" "BIRTHDAY" "WEBSITE" "LINKEDIN" "TWITTER" "DISCORD" "FOOD_PREFERENCE" "JOB_TITLE" "EMPLOYER" "LAST_MODIFIED" "DATE_MET")
           org-roam-capture-templates
           `(("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
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
     (eval setq org-export-with-todo-keywords nil)
     (anki-editor-mode . t)
     (org-hugo-section . "posts")
     (org-hugo-base-dir . "~/proj/cashweaver.com")
     (eval add-hook! 'before-save-hook :local #'cashweaver/anki-editor-push-notes)
     (eval add-hook! 'before-save-hook :local #'anki-editor-push-notes)
     (eval anki-editor-mode t)
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-roam-add-anki)
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-roam-add-bibliography)
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-roam-mirror-roam-refs-to-front-matter)
     (eval setq-local org-roam-capture-templates
           `(("c" "concept" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :concept:
"))
              :unnarrowed t)
             ("d" "default" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "* TODO"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "Among other things:
" "* TODO"))
              :unnarrowed t)
             ("P" "poem" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :poem:
"))
              :unnarrowed t)
             ("q" "quote" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :quote:
" "#+begin_quote
" "TODO_QUOTE
" "
" "/[[https:foo][source]]/
" "#+end_quote
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))
           org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory))
           cashweaver/org-roam--file-path-exceptions-to-add-bibliography
           `(,(format "%sunread.org" org-roam-directory)
             ,(format "%sunread.org_archive" org-roam-directory)))
     (eval setq-local org-hugo-base-dir cashweaver/path--directory--company-publish org-roam-capture-templates
           `(("d" "default" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "* ${title}
" "@@html:<!--*@@
" "@@html:# Document freshness: For more information, see go/fresh-source.@@
" "@@html:freshness: { owner: 'cashweaver' reviewed: '%<%Y-%m-%d>' }@@
" "@@html:*-->@@
" "** TODO"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "* ${title}
" "@@html:<!--*@@
" "@@html:# Document freshness: For more information, see go/fresh-source.@@
" "@@html:freshness: { owner: 'cashweaver' reviewed: '%<%Y-%m-%d>' }@@
" "@@html:*-->@@
" "[[who:%^{LDAP}][%}@]]
" "** TODO"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory)))
     (eval setq-local org-hugo-base-dir cashweaver/path--directory--roam org-roam-capture-templates
           `(("d" "default" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "* ${title}
" "@@html:<!--*@@
" "@@html:# Document freshness: For more information, see go/fresh-source.@@
" "@@html:freshness: { owner: 'cashweaver' reviewed: '%<%Y-%m-%d>' }@@
" "@@html:*-->@@
" "** TODO"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "* ${title}
" "@@html:<!--*@@
" "@@html:# Document freshness: For more information, see go/fresh-source.@@
" "@@html:freshness: { owner: 'cashweaver' reviewed: '%<%Y-%m-%d>' }@@
" "@@html:*-->@@
" "[[who:%^{LDAP}][%}@]]
" "** TODO"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory)))
     (eval setq-local org-hugo-base-dir
           (\, cashweaver/path--directory--roam)
           org-roam-capture-templates
           `(("d" "default" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "* ${title}
" "@@html:<!--*@@
" "@@html:# Document freshness: For more information, see go/fresh-source.@@
" "@@html:freshness: { owner: 'cashweaver' reviewed: '%<%Y-%m-%d>' }@@
" "@@html:*-->@@
" "** TODO"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "* ${title}
" "@@html:<!--*@@
" "@@html:# Document freshness: For more information, see go/fresh-source.@@
" "@@html:freshness: { owner: 'cashweaver' reviewed: '%<%Y-%m-%d>' }@@
" "@@html:*-->@@
" "[[who:%^{LDAP}][%}@]]
" "** TODO"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory)))
     (org-hugo-base-dir \, cashweaver/path--directory--roam)
     (org-hugo-base-dir . cashweaver/path--directory--roam)
     (org-hugo-base-dir cashweaver/path--directory--roam)
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-roam-rewrite-smart-to-ascii)
     (eval setq-local org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory))
     (eval setq-local org-roam-capture-templates
           `(("d" "default" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "* ${title}
" "@@html:<!--*@@
" "@@html:# Document freshness: For more information, see go/fresh-source.@@
" "@@html:freshness: { owner: 'cashweaver' reviewed: '%<%Y-%m-%d>' }@@
" "@@html:*-->@@
" "** TODO"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+filetags: :person:
" "* ${title}
" "@@html:<!--*@@
" "@@html:# Document freshness: For more information, see go/fresh-source.@@
" "@@html:freshness: { owner: 'cashweaver' reviewed: '%<%Y-%m-%d>' }@@
" "@@html:*-->@@
" "[[who:%^{LDAP}][%}@]]
" "** TODO"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))
           org-attach-directory
           (file-truename
            (format "%s/attachments/" org-roam-directory))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
