;;; google-issues.el --- Interface to go/issues CLI -*- lexical-binding: t; -*-

;; Version: 0.1.0

;;; Commentary:
;;
;; This package wraps the /google/bin/releases/issues-cli/issues tool
;; and provides compatibility with google-bugged-bug structs.

;;; Code:

(require 'cl-lib)
(require 'deferred)
(require 's)
(require 'dash)
(require 'google-bugged) ; For google-bugged-bug struct

(defgroup google-issues nil
  "Customisation group for `google-issues`."
  :tag "Google Issues"
  :group 'google)

(defcustom google-issues-program "/google/bin/releases/issues-cli/issues"
  "Program name for the `issues` command."
  :group 'google-issues
  :type 'string
  :risky t)

(defvar google-issues--parent-ids-cache (make-hash-table :test 'equal)
  "Cache of bug-id -> list of parent-ids (integers).")

(defun google-issues-get-parent-ids (bug-id)
  "Get parent IDs for BUG-ID (string or integer) from cache."
  (let ((id-str (if (numberp bug-id) (number-to-string bug-id) bug-id)))
    (gethash id-str google-issues--parent-ids-cache)))

(defun google-issues-clear-cache ()
  "Clear the parent IDs cache."
  (interactive)
  (clrhash google-issues--parent-ids-cache))

(defun google-issues--parse-ids (str)
  "Parse comma-separated string of IDs into list of integers."
  (if (or (null str) (string= "N/A" str) (string-empty-p (s-trim str)))
      nil
    (mapcar #'string-to-number
            (mapcar #'s-trim
                    (s-split "," str 'omit-nulls)))))

(defun google-issues--parse-bug-block (block)
  "Parse a single bug block from issues CLI output."
  (let ((lines (s-lines (s-trim block)))
        (bug (make-google-bugged-bug))
        parent-ids)
    (dolist (line lines)
      (when (string-match "^\\([^:]+\\): \\(.*\\)$" line)
        (let ((key (match-string 1 line))
              (val (s-trim (match-string 2 line))))
          (cond
           ((string= key "Issue ID")
            (setf (google-bugged-bug-id bug) val))
           ((string= key "Status")
            (setf (google-bugged-bug-status bug) val))
           ((string= key "Assignee")
            (setf (google-bugged-bug-assignee bug) val))
           ((string= key "Component ID")
            (setf (google-bugged-bug-component-id bug) val))
           ((string= key "Priority")
            (setf (google-bugged-bug-priority bug) val))
           ((string= key "Title")
            (setf (google-bugged-bug-summary bug) val))
           ((string= key "Issue Type")
            (setf (google-bugged-bug-type bug) val))
           ((string= key "Blocked By Issue IDs")
            (setf (google-bugged-bug-blocked-by bug)
                  (google-issues--parse-ids val)))
           ((string= key "Parent Issue IDs")
            (setq parent-ids (google-issues--parse-ids val)))))))
    ;; Store parent-ids in cache if we found any
    (when (and (google-bugged-bug-id bug) parent-ids)
      (puthash (google-bugged-bug-id bug) parent-ids google-issues--parent-ids-cache))
    ;; Return bug only if we got at least an ID
    (if (google-bugged-bug-id bug)
        bug
      nil)))

(defun google-issues--parse-search-output (output)
  "Parse the full output of issues search."
  (let* ((lines (s-lines (s-trim output)))
         ;; Remove header line if it exists (e.g., "Showing first N...")
         (body-lines (if (string-prefix-p "Showing" (car lines))
                         (cdr lines)
                       lines))
         (body (s-join "\n" body-lines))
         ;; Split by --- with optional newlines
         (blocks (s-split "\n\n---\n\n" body 'omit-nulls))
         bugs)
    (dolist (block blocks)
      (when-let ((bug (google-issues--parse-bug-block block)))
        (push bug bugs)))
    (nreverse bugs)))

;;;###autoload
(defun google-issues-get-bugs-deferred (query)
  "Retrieve a list of `google-bugged-bug` that match QUERY using `issues` CLI.
Returns a deferred object."
  (deferred:$
   (deferred:process
    google-issues-program "search"
    "--fields" "issue_id,status,assignee,component_id,priority,title,type,parent_issue_ids,blocked_by"
    "--limit" "200" ; Higher limit for bulk fetch
    "--" query)
   (deferred:nextc it
     (lambda (output)
       (google-issues--parse-search-output output)))))

;;;###autoload
(defun google-issues-get-bug-deferred (id)
  "Retrieve a `google-bugged-bug` for ID using `issues` CLI.
Returns a deferred object."
  (deferred:$
   (google-issues-get-bugs-deferred (format "id:%s" id))
   (deferred:nextc it #'car)))

(provide 'google-issues)

;;; google-issues.el ends here
