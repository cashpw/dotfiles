(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval setq-local org-roam-capture-templates
           `(("c" "concept" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
" "#+filetags: :concept:
" "#+hugo_auto_set_lastmod: t
"))
              :unnarrowed t)
             ("d" "default" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
" "#+hugo_auto_set_lastmod: t
" "* TODO"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
" "#+filetags: :person:
" "#+hugo_auto_set_lastmod: t
" "Among other things:
" "* TODO"))
              :unnarrowed t)
             ("q" "quote" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
" "#+filetags: :quote:
" "#+hugo_auto_set_lastmod: t
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
            (format "%s/attachments/" org-roam-directory)))
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
            (format "%s/attachments" org-roam-directory))
           cashweaver-org-roam-attachment-base-path
           (file-truename
            (format "%s/attachments" org-roam-directory)))
     (eval setq-local org-default-properties
           '("NICKNAME" "PERSONAL_EMAIL" "WORK_EMAIL" "ADDRESS" "PERSONAL_PHONE" "WORK_PHONE" "BIRTHDAY" "WEBSITE" "LINKEDIN" "TWITTER" "DISCORD" "FOOD_PREFERENCE" "JOB_TITLE" "EMPLOYER" "LAST_MODIFIED" "DATE_MET")
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
            (locate-dominating-file default-directory ".dir-locals.el")))
     (eval add-hook! 'before-save-hook :local #'cashweaver-org-roam--add-bibliography)
     (eval add-hook! 'before-save-hook :local #'cashweaver-org-roam--mirror-roam-refs-to-front-matter)
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-roam--rewrite-smart-to-ascii)
     (eval setq-local org-roam-capture-templates
           `(("c" "concept" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
" "#+filetags: :concept:
" "#+hugo_auto_set_lastmod: t
"))
              :unnarrowed t)
             ("d" "default" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
" "#+hugo_auto_set_lastmod: t
" "* TODO"))
              :unnarrowed t)
             ("p" "person" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
" "#+filetags: :person:
" "#+hugo_auto_set_lastmod: t
" "Among other things:
" "* TODO"))
              :unnarrowed t)
             ("q" "quote" plain "%?" :target
              (file+head "${slug}.org" ,(concat "#+title: ${title}
" "#+author: Cash Weaver
" "#+date: [%<%Y-%m-%d %a %H:%M>]
" "#+startup: overview
" "#+filetags: :quote:
" "#+hugo_auto_set_lastmod: t
" "#+begin_quote
" "TODO_QUOTE
" "
" "/[[https:foo][source]]/
" "#+end_quote
"))
              :unnarrowed t))
           org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el")))
     (eval add-hook! 'before-save-hook :local #'cashweaver/contacts--create-birthday-reminder)
     (eval add-hook! 'before-save-hook :local #'cashweaver/org-set-last-modified)
     (eval setq-local org-default-properties
           '("NICKNAME" "PERSONAL_EMAILS" "WORK_EMAILS" "ADDRESSES" "PERSONAL_PHONES" "WORK_PHONES" "BIRTHDAY" "WEBSITES" "LINKEDIN" "TWITTER" "DISCORD" "FOOD_PREFERENCE" "JOB_TITLE" "EMPLOYER" "LAST_MODIFIED" "DATE_MET")
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
            (locate-dominating-file default-directory ".dir-locals.el")))
     (eval setq-local org-roam-db-location
           (expand-file-name "org-roam.db" org-roam-directory))
     (eval setq-local org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
