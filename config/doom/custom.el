(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error-regexp-alist
   '(google3-build-log-parser-info google3-build-log-parser-warning google3-build-log-parser-error google3-build-log-parser-python-traceback bazel google-blaze-error google-blaze-warning google-log-error google-log-warning google-log-info google-log-fatal-message google-forge-python gunit-stack-trace absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek gradle-kotlin iar ibm irix java jikes-file maven jikes-line clang-include clang-include gcc-include ruby-Test::Unit gmake gnu lcc makepp mips-1 mips-2 omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line))
 '(safe-local-variable-values
   '((eval add-hook! 'before-save-hook :local #'anki-editor-push-notes)
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
