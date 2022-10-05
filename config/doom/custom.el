(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval setq-local org-roam-capture-templates
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
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-roam-rewrite-smart-to-ascii)
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
     (eval setq-local org-default-properties
      '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
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
      org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory))
      cashweaver/org-roam--file-path-exceptions-to-add-bibliography
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory)))
     (eval setq-local org-default-properties
      '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
      org-capture-template--org-fc-normal
      `("Normal" :keys "n" :file ,(lambda nil
                                    (buffer-name))
        :olp
        ("Flashcards")
        :template
        ("* %^{Name of card}" ":PROPERTIES:" ":CREATED: %U" ":END:" "" "%?" "" "** TODO Back" "** TODO Source")
        :before-finalize ,(lambda nil
                            (org-fc-type-normal-init)))
      org-capture-template--org-fc-double
      `("Double" :keys "d" :file ,(lambda nil
                                    (buffer-name))
        :olp
        ("Flashcards")
        :template
        ("* %^{Name of card}" ":PROPERTIES:" ":CREATED: %U" ":END:" "" "%?" "" "** TODO Back" "** TODO Source")
        :before-finalize ,(lambda nil
                            (org-fc-type-double-init)))
      org-capture-template--org-fc-cloze
      `("Cloze" :keys "c" :file ,(lambda nil
                                   (buffer-name))
        :olp
        ("Flashcards")
        :template
        ("* %^{Name of card}" ":PROPERTIES:" ":CREATED: %U" ":END:" "" "- %?" "" "** TODO Source")
        :before-finalize ,(lambda nil
                            (org-fc-type-cloze-init 'deletion)))
      org-capture-templates
      (doct
       `(,org-capture-template--org-fc-cloze ,org-capture-template--org-fc-double ,org-capture-template--org-fc-normal))
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
      org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory))
      cashweaver/org-roam--file-path-exceptions-to-add-bibliography
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory)))
     (eval setq-local org-default-properties
      '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
      org-capture-template--org-fc-normal
      `("Normal" :keys "n" :file
        (lambda nil
          (buffer-name))
        :olp
        ("Flashcards")
        :template
        ("* %^{Name of card}" ":PROPERTIES:" ":CREATED: %U" ":END:" "" "%?" "" "** TODO Back" "** TODO Source")
        :before-finalize ,(lambda nil
                            (org-fc-type-normal-init)))
      org-capture-template--org-fc-double
      `("Double" :keys "d" :file
        (lambda nil
          (buffer-name))
        :olp
        ("Flashcards")
        :template
        ("* %^{Name of card}" ":PROPERTIES:" ":CREATED: %U" ":END:" "" "%?" "" "** TODO Back" "** TODO Source")
        :before-finalize ,(lambda nil
                            (org-fc-type-double-init)))
      org-capture-template--org-fc-cloze
      `("Cloze" :keys "c" :file
        (lambda nil
          (buffer-name))
        :olp
        ("Flashcards")
        :template
        ("* %^{Name of card}" ":PROPERTIES:" ":CREATED: %U" ":END:" "" "- %?" "" "** TODO Source")
        :before-finalize ,(lambda nil
                            (org-fc-type-cloze-init 'deletion)))
      org-capture-templates
      (doct
       `(,org-capture-template--org-fc-cloze ,org-capture-template--org-fc-double ,org-capture-template--org-fc-normal))
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
      org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory))
      cashweaver/org-roam--file-path-exceptions-to-add-bibliography
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory)))
     (eval setq-local org-default-properties
      '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
      org-capture-templates
      (doct
       '(("Equivalence" :keys "e" :file
          (lambda nil
            (buffer-name))
          :olp
          ("Flashcards")
          :template
          ("* Equivalence" ":PROPERTIES:" ":CREATED: [%<%Y-%m-%d %a %H:%M>]" ":END:" "" "- %?")
          :hook
          (lambda nil
            (org-fc-type-cloze-init deletion)))))
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
      org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory))
      cashweaver/org-roam--file-path-exceptions-to-add-bibliography
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory)))
     (eval setq-local org-default-properties
      '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
      org-capture-templates
      (doct
       '(("Equivalence" :keys "e" :file
          (lambda nil
            (buffer-name))
          :olp
          ("Flashcards")
          :template
          ("* Equivalence" ":PROPERTIES:" ":CREATED: [%<%Y-%m-%d %a %H:%M>]" ":END:" "" "- %?")
          :hook
          (lambda nil
            (org-fc-type-cloze-init)))))
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
      org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory))
      cashweaver/org-roam--file-path-exceptions-to-add-bibliography
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory)))
     (eval setq-local org-default-properties
      '("PREP_MINUTES" "COOK_MINUTES" "SERVINGS" "STARS")
      org-capture-templates
      (doct
       '(("Equivalence" :keys "a" :file
          (buffer-name)
          :olp
          ("Flashcards")
          :template
          ("* Equivalence" ":PROPERTIES:" ":CREATED: [%<%Y-%m-%d %a %H:%M>]" ":END:" "" "- %?")
          :hook
          (lambda nil
            (org-fc-type-cloze-init)))))
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
      org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory))
      cashweaver/org-roam--file-path-exceptions-to-add-bibliography
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory)))
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
      org-hugo-auto-set-lastmod t cashweaver/org-roam--file-path-exceptions-to-export-after-save
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory))
      cashweaver/org-roam--file-path-exceptions-to-add-bibliography
      `(,(format "%sunread.org" org-roam-directory)
        ,(format "%sunread.org_archive" org-roam-directory)))
     (eval add-hook! 'before-save-hook :local #'cashweaver/contacts--create-birthday-reminder)
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-set-last-modified)
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-roam-before-save)
     (eval setq-local org-roam-db-location
      (expand-file-name "org-roam.db" org-roam-directory))
     (eval setq-local org-default-properties
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
     (eval setq-local org-export-with-priority nil org-export-with-todo-keywords nil)
     (anki-editor-mode . t)
     (eval add-hook 'cashweaver/org-mode-done-cut-hook 'org-roam-file-p)
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
        ,(format "%sunread.org_archive" org-roam-directory))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
