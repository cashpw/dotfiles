(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((eval progn
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
      (add-hook! 'before-save-hook :local #'cashpw/org-roam-before-save)
      (add-hook! 'before-save-hook :local #'cashpw/org-set-last-modified))
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
     (eval progn
      (add-hook! 'before-save-hook :local #'cashpw/org-roam-before-save)
      (add-hook! 'before-save-hook :local #'cashpw/org-set-last-modified))
     (eval setq-local org-export-with-priority nil org-export-with-todo-keywords nil)
     (eval add-hook 'cashpw/org-mode-done-cut-hook 'org-roam-file-p)
     (eval setq-local org-default-properties
      (append org-default-properties org-recipes--properties)
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
        ,(format "%sunread.org_archive" org-roam-directory))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
