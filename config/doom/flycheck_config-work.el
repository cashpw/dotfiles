(defvar cashweaver/org-link--google-docs-base-url
  "https://docs.google.com/document/d"
  "The base url for Google Docs")
(defvar cashweaver/org-link--google-docs-type
  "google-docs"
  "TODO")

(defun cashweaver/org-link--google-docs-build-url (sheet-id)
  "Return a url to Google Doce for the provided SHEET-ID."
  (s-format
   "${base-url}/${id}"
   'aget
   `(("base-url" . ,cashweaver/org-link--google-docs-base-url)
     ("id" . ,sheet-id))))

(defun cashweaver/org-link--google-docs-build-org-link (sheet-id description)
  "Return a url to Google Docs for the provided SHEET-ID."
  (s-format
   "[[${type}:${id}][${description}]]"
   'aget
   `(("type" . ,cashweaver/org-link--google-docs-type)
     ("id" . ,sheet-id)
     ("description" . ,description))))

(defun cashweaver/org-link--google-docs-open (path arg)
  (browse-url
   (url-encode-url
    (cashweaver/org-link--google-docs-build-url
     path))
   arg))

(defun cashweaver/org-link--google-docs-export (path desc backend info)
  "Export a Google Docs link."
  (let ((uri
         (cashweaver/org-link--google-docs-build-url
          path)))
    (pcase backend
      (`md
       (s-format
        "[${description}}](${uri})"
        'aget
        `(("description" . ,desc)
          ("uri" . ,uri))))
      ('html
       (s-format
        "<a href=\"${uri}\">${description}</a>"
        'aget
        `(("description" . ,desc)
          ("uri" . ,uri))))
      (_
       uri))))

(org-link-set-parameters
 cashweaver/org-link--google-docs-type
 :follow #'cashweaver/org-link--google-docs-open
 :export #'cashweaver/org-link--google-docs-export)
