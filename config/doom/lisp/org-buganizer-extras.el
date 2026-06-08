;;; org-buganizer-extras.el --- Extras for org-buganizer  -*- lexical-binding: t; -*-

(require 'org-buganizer)
(require 'google-bugged)
(require 'google-issues)
(require 'deferred)
(require 's)
(require 'dash)

(defvar org-buganizer-extras-path-work-notes-dir nil
  "Directory where work notes Org files are stored.")

(defvar org-buganizer-extras--syncing-bugs nil
  "List of bug IDs currently undergoing synchronization.")

(defconst org-buganizer-extras--project-types
  '("PROGRAM" "MILESTONE" "EPIC" "PROJECT" "STORY")
  "Buganizer issue types that should be mapped to PROJ todo state.")

(cl-defstruct org-buganizer-extras-session
  "Session context for recursive synchronization."
  mode
  bulk-bugs-cache
  parent-to-children-map)

(defun org-buganizer-extras-query-heading-p ()
  "Return non-nil if the current heading is a buganizer query."
  (string-prefix-p "Query: " (org-get-heading)))

(defun org-buganizer-extras-bug-file-exists-p (bug-id)
  "Return a list of matching Org file paths for BUG-ID (integer or string), or nil if none exist."
  (when (and org-buganizer-extras-path-work-notes-dir
              (file-directory-p org-buganizer-extras-path-work-notes-dir))
    (let* ((bug-id-str (if (numberp bug-id) (number-to-string bug-id) bug-id))
           (match (format "^bug-%s-.*\\.org\\(_archive\\)?$" bug-id-str))
           (files (directory-files
                   org-buganizer-extras-path-work-notes-dir t match))
           (active-files (--filter (not (string-suffix-p "_archive" it)) files)))
      (or active-files files))))

(defun org-buganizer-extras-get-id-from-bug-file (bug-id)
  "Get the Org ID from the existing bug file for BUG-ID, if it exists."
  (when-let ((files (org-buganizer-extras-bug-file-exists-p bug-id))
              (file (car files)))
    (with-temp-buffer
      (insert-file-contents file nil nil 500)
      (goto-char (point-min))
      (when (re-search-forward "^:ID:\\s-+\\(.+\\)$" nil t)
        (match-string 1)))))

(defun org-buganizer-extras--propose-title-from-bug-advice
    (orig-fun bug &rest args)
  "Advice to use Org ID link if an org file exists for the bug."
  (let* ((bug-id (google-bugged-bug-id bug))
          (summary (google-bugged-bug-summary bug))
          (org-id (org-buganizer-extras-get-id-from-bug-file bug-id)))
    (if org-id
        (format "[[id:%s][%s]]" org-id summary)
      (apply orig-fun bug args))))

(advice-add #'org-buganizer--propose-title-from-bug
            :around #'org-buganizer-extras--propose-title-from-bug-advice)

(defun org-buganizer-extras-find-bug-heading (bug-id)
  "Find the heading for BUG-ID (string or number) in the current subtree.
Return point if found, nil otherwise. Does NOT move point."
  (let ((bug-id-str (if (numberp bug-id) (number-to-string bug-id) bug-id)))
    (save-excursion
      (catch 'found
        (org-map-entries
         (lambda ()
           (when (string= (org-entry-get (point) "buganizer_id") bug-id-str)
             (throw 'found (point))))
         nil 'tree)
         nil))))

(defun org-buganizer-extras-goto-query-heading ()
  "Go to the nearest query heading at or above point.
Return non-nil if found, and move point there.
Otherwise return nil and don't move point."
  (let ((orig-point (point))
        (found nil))
    (back-to-indentation)
    (when (ignore-errors (org-back-to-heading t) t)
      (if (org-buganizer-extras-query-heading-p)
          (setq found (point))
        (while (and (not found)
                    (org-up-heading-safe))
          (when (org-buganizer-extras-query-heading-p)
            (setq found (point))))))
    (unless found
      (goto-char orig-point))
    found))

(defun org-buganizer-extras--todo-state-for-bug (bug)
  "Return \"DONE\" if BUG is closed.
Return \"PROJ\" if BUG is open and its type is in `org-buganizer-extras--project-types`.
Otherwise return \"TODO\"."
  (cond
   ((member (google-bugged-bug-status bug) org-buganizer--status-closed)
    "DONE")
   ((member (upcase (or (google-bugged-bug-type bug) ""))
            org-buganizer-extras--project-types)
    "PROJ")
   (t
    "TODO")))

(defun org-buganizer-extras--update-node-priority-and-tags (bug)
  "Set the Org subtree priority and tags based on BUG details.
Adapts to numeric or alphabetic priorities based on active Org configuration."
  (let* ((priority (google-bugged-bug-priority bug))
         (priority-number (string-to-number (substring priority 1 2)))
         (priority-val (if (< org-priority-lowest 65)
                           priority-number
                         (+ org-priority-highest
                            (max 0 (min 2 (1- priority-number))))))
         (bug-id (google-bugged-bug-id bug))
         (tags (s-lex-format ":bug:bug_${bug-id}:")))
    (org-priority priority-val)
    (org-set-tags tags)))

(defun org-buganizer-extras--clean-bug-ids (ids)
  "Clean and filter a list of bug IDs (strings or numbers).
Return a list of integers. Filter out invalid IDs like empty strings or spaces."
  (delq nil
        (mapcar
         (lambda (id)
           (let ((id-str (cond
                          ((numberp id) (number-to-string id))
                          ((stringp id) (s-trim id))
                          (t nil))))
             (when (and id-str (not (string-empty-p id-str)) (string-match-p "^[0-9]+$" id-str))
               (string-to-number id-str))))
         ids)))

(defun org-buganizer-extras--get-bug-deferred (bug-id &optional session)
  "Get bug for BUG-ID (string or integer), using bulk cache from SESSION if available."
  (let ((id-str (if (numberp bug-id) (number-to-string bug-id) bug-id)))
    (if-let ((cached-bug (and session
                              (org-buganizer-extras-session-bulk-bugs-cache session)
                              (gethash id-str (org-buganizer-extras-session-bulk-bugs-cache session)))))
        (deferred:succeed cached-bug)
      (google-issues-get-bug-deferred id-str))))

(defun org-buganizer-extras--get-child-ids (bug &optional session)
  "Get child IDs for BUG based on SESSION mode."
  (let ((bug-id (google-bugged-bug-id bug))
        (mode (if session (org-buganizer-extras-session-mode session) 'dependency)))
    (if (eq mode 'parent)
        (when (and session (org-buganizer-extras-session-parent-to-children-map session))
          (gethash (string-to-number bug-id) (org-buganizer-extras-session-parent-to-children-map session)))
      (org-buganizer-extras--clean-bug-ids
       (google-bugged-bug-blocked-by bug)))))

(defun org-buganizer-extras--fetch-bug-tree-deferred (bug-id &optional visited session)
  "Fetch bug details for BUG-ID (integer) and recursively fetch its children.
VISITED is a list of bug IDs (integers) already visited to avoid cycles.
SESSION is the optional sync session context.
Returns a deferred that resolves to a tree node: (BUG . CHILDREN-NODES)."
  (if (member bug-id visited)
      (deferred:succeed nil)
    (let ((visited (cons bug-id visited)))
      (deferred:$
        (org-buganizer-extras--get-bug-deferred bug-id session)
        (deferred:nextc it
          (lambda (bug)
            (cond
             ((null bug)
              (deferred:succeed nil))
             ((org-buganizer-extras-bug-file-exists-p bug-id)
              (deferred:succeed (cons bug nil)))
             (t
              (let ((child-ids (org-buganizer-extras--get-child-ids bug session)))
                (if (null child-ids)
                    (deferred:succeed (cons bug nil))
                  (deferred:$
                    (apply #'deferred:parallel
                           (mapcar
                            (lambda (cid)
                              (org-buganizer-extras--fetch-bug-tree-deferred
                               cid visited session))
                            child-ids))
                    (deferred:nextc it
                      (lambda (children-trees)
                        (cons bug (remove nil children-trees)))))))))))))))

(defun org-buganizer-extras--insert-bug-tree (node level)
  "Insert NODE (bug . children) at Org outline heading LEVEL.
This function mutates the current buffer by inserting headings,
updating properties, and setting tags/priorities. It also moves the point."
  (when node
    (let ((bug (car node))
          (children (cdr node)))
      (when bug
        (let ((org-insert-heading-respect-content nil))
          (org-insert-heading)
          (let ((cur-level (org-outline-level)))
            (cond
             ((< cur-level level)
              (dotimes (_ (- level cur-level))
                (org-demote-subtree)))
             ((> cur-level level)
              (dotimes (_ (- cur-level level))
                (org-promote-subtree)))))
          (org-buganizer--update-properties-from-bug bug nil t)
          (org-edit-headline (org-buganizer--propose-title-from-bug bug))
          (let ((org-after-todo-state-change-hook nil))
            (org-todo (org-buganizer-extras--todo-state-for-bug bug)))
          (org-buganizer-extras--update-node-priority-and-tags bug)
          (dolist (child children)
            (org-end-of-subtree)
            (org-buganizer-extras--insert-bug-tree child (1+ level))))))))

(defun org-buganizer-extras--sync-bug-deferred
    (bug-id level &optional ancestors session parent-heading-marker)
  "Sync bug for BUG-ID (integer) at LEVEL recursively.
Optional ANCESTORS is a list of bug IDs already processed in the
current recursion tree, used to detect cyclic dependencies and avoid
infinite loops.
SESSION is the optional sync session context.
PARENT-HEADING-MARKER is the marker of the parent heading (if any)."
  (if (or (member bug-id ancestors)
          (member bug-id org-buganizer-extras--syncing-bugs))
      (deferred:succeed nil)
    (push bug-id org-buganizer-extras--syncing-bugs)
    (let* ((visited (cons bug-id ancestors))
           (d (if-let ((heading-point
                        (org-buganizer-extras-find-bug-heading bug-id)))
                 ;; Case 1: Exists. Update in place.
                 (let ((marker (copy-marker heading-point)))
                   (deferred:$
                     (org-buganizer-extras--get-bug-deferred bug-id session)
                      (deferred:nextc it
                        (lambda (bug)
                          (let ((d-child
                                 (progn
                                   (when bug
                                     (org-with-point-at marker
                                       (unwind-protect
                                           (let ((org-element-use-cache nil))
                                             ;; Correct the heading level if it has shifted
                                             (let ((cur-level (org-outline-level)))
                                               (cond
                                                ((< cur-level level)
                                                 (dotimes (_ (- level cur-level))
                                                   (org-demote-subtree)))
                                                ((> cur-level level)
                                                 (dotimes (_ (- cur-level level))
                                                   (org-promote-subtree)))))
                                             (org-buganizer--update-properties-from-bug
                                              bug nil t)
                                             (let ((org-after-todo-state-change-hook nil))
                                               (org-todo
                                                (org-buganizer-extras--todo-state-for-bug
                                                 bug)))
                                             (org-buganizer-extras--update-node-priority-and-tags
                                              bug))
                                         (org-element-cache-reset))))
                                   (if-let ((child-ids
                                             (and bug
                                                  (org-buganizer-extras--get-child-ids bug session))))
                                       (apply #'deferred:parallel
                                              (mapcar
                                               (lambda (cid)
                                                 (org-buganizer-extras--sync-bug-deferred
                                                  cid (1+ level) visited session marker))
                                               child-ids))
                                     (deferred:succeed nil)))))
                            (deferred:watch d-child
                              (lambda ()
                                (set-marker marker nil))))))))
               ;; Case 2: Does not exist. Fetch tree and insert.
               (let ((insertion-marker (or parent-heading-marker (point-marker))))
                 (deferred:$
                   (org-buganizer-extras--fetch-bug-tree-deferred
                    bug-id ancestors session)
                   (deferred:nextc it
                     (lambda (tree)
                       (unwind-protect
                           (when tree
                             (org-with-point-at insertion-marker
                               (unwind-protect
                                   (let ((org-element-use-cache nil))
                                     (org-end-of-subtree)
                                     (org-buganizer-extras--insert-bug-tree
                                      tree level))
                                 (org-element-cache-reset))))
                         (unless parent-heading-marker
                           (set-marker insertion-marker nil))))))))))
      (deferred:$
        d
        (deferred:nextc it
          (lambda (res)
            (setq org-buganizer-extras--syncing-bugs
                  (remove bug-id org-buganizer-extras--syncing-bugs))
            res))
        (deferred:error it
          (lambda (err)
            (setq org-buganizer-extras--syncing-bugs
                  (remove bug-id org-buganizer-extras--syncing-bugs))
            (deferred:fail err)))))))

;;;###autoload
(defun org-buganizer-extras-sync-query (query level)
  "Sync all bugs for QUERY at LEVEL recursively, defaulting to parent/child if applicable."
  (let* ((parent-query-p (string-match "^parentid:\\([0-9]+\\)\\(+\\)?$" query))
         (root-id (when parent-query-p (match-string 1 query)))
         (session (make-org-buganizer-extras-session
                   :mode (if parent-query-p 'parent 'dependency)
                   :bulk-bugs-cache (make-hash-table :test 'equal)
                   :parent-to-children-map (make-hash-table :test 'eql))))
    (if parent-query-p
        ;; Parent/Child Bulk Sync Mode (force status:open and transitive search)
        (let ((bulk-query (format "parentid:%s+ status:open" root-id)))
          (deferred:$
            (google-issues-get-bugs-deferred bulk-query)
            (deferred:nextc it
              (lambda (bugs)
                ;; Populate caches
                (dolist (bug bugs)
                  (let ((bug-id (google-bugged-bug-id bug)))
                    (puthash bug-id bug (org-buganizer-extras-session-bulk-bugs-cache session))
                    ;; Parse parents to build children map
                    (when-let ((parents (google-issues-get-parent-ids bug-id)))
                      (dolist (pid parents)
                        (puthash pid
                                 (cl-adjoin (string-to-number bug-id)
                                            (gethash pid (org-buganizer-extras-session-parent-to-children-map session)))
                                 (org-buganizer-extras-session-parent-to-children-map session))))))
                ;; Find top-level bugs (direct children of root-id)
                (let* ((root-id-num (string-to-number root-id))
                       (top-level-ids (gethash root-id-num (org-buganizer-extras-session-parent-to-children-map session))))
                  (if (null top-level-ids)
                      (deferred:succeed nil)
                    (apply #'deferred:parallel
                           (mapcar
                            (lambda (cid)
                              (org-buganizer-extras--sync-bug-deferred cid level nil session))
                            top-level-ids))))))))
      ;; Standard Dependency Sync Mode (force status:open if not overridden)
      (let ((clean-query (if (string-match-p "status:" query)
                             query
                           (concat query " status:open"))))
        (deferred:$
          (google-issues-get-bugs-deferred clean-query)
          (deferred:nextc it
            (lambda (bugs)
              (when bugs
                (apply #'deferred:parallel
                       (mapcar
                        (lambda (bug)
                          (let ((bug-id (string-to-number (google-bugged-bug-id bug))))
                            (org-buganizer-extras--sync-bug-deferred bug-id level nil session)))
                        bugs))))))))))

(provide 'org-buganizer-extras)
