;;; org-buganizer-extras-test.el --- Tests for org-buganizer-extras  -*- lexical-binding: t; -*-

(require 'org-buganizer-extras)
(require 'cl-lib)
(require 'google-issues)

(defmacro org-buganizer-extras--with-temp-buffer (contents &rest body)
  "Create a `org-mode` enabled temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((org-adapt-indentation nil)
           (org-todo-keywords '((sequence "TODO" "PROJ" "|" "DONE"))))
       (org-mode)
       (org-buganizer-mode)
       (insert ,contents)
       (goto-char (point-min))
       ,@body)))

(ert-deftest org-buganizer-extras-fetch-tree-happy-path ()
  "Verify recursive tree fetching works happy path."
  (cl-letf (((symbol-function 'google-issues-get-bug-deferred)
             (lambda (id-str)
               (let ((id (string-to-number id-str)))
                 (deferred:succeed
                   (cond
                    ((eq id 1)
                     (make-google-bugged-bug
                      :id "1" :summary "Parent" :blocked-by '(2 3)
                      :priority "P2" :status "ASSIGNED"))
                    ((eq id 2)
                     (make-google-bugged-bug
                      :id "2" :summary "Child 1" :blocked-by nil
                      :priority "P3" :status "NEW"))
                    ((eq id 3)
                     (make-google-bugged-bug
                      :id "3" :summary "Child 2" :blocked-by nil
                      :priority "P1" :status "ACCEPTED"))))))))
    (let* ((tree (deferred:sync!
                   (org-buganizer-extras--fetch-bug-tree-deferred 1)))
           (parent (car tree))
           (children (cdr tree))
           (child1 (car children))
           (child2 (cadr children)))
      (should (equal (google-bugged-bug-id parent) "1"))
      (should (equal (google-bugged-bug-summary parent) "Parent"))
      (should (equal (length children) 2))
      (should (equal (google-bugged-bug-id (car child1)) "2"))
      (should (equal (google-bugged-bug-id (car child2)) "3")))))

(ert-deftest org-buganizer-extras-fetch-tree-termination ()
  "Verify tree fetching terminates when a local file exists."
  (let ((temp-dir (make-temp-file "org-buganizer-extras-test" t))
        (org-buganizer-extras-path-work-notes-dir nil))
    (unwind-protect
        (progn
          (setq org-buganizer-extras-path-work-notes-dir temp-dir)
          ;; Create a dummy file for bug 2
          (write-region
           ":PROPERTIES:\n:ID: mock-id-2\n:END:\n#+title: Child 1 (b/2)"
           nil (expand-file-name "bug-2-Child-1.org" temp-dir))
          
          (cl-letf (((symbol-function 'google-issues-get-bug-deferred)
                     (lambda (id-str)
                       (let ((id (string-to-number id-str)))
                         (deferred:succeed
                           (cond
                            ((eq id 1)
                             (make-google-bugged-bug
                              :id "1" :summary "Parent" :blocked-by '(2 3)
                              :priority "P2" :status "ASSIGNED"))
                            ((eq id 2)
                             (make-google-bugged-bug
                              :id "2" :summary "Child 1" :blocked-by '(4)
                              :priority "P3" :status "NEW"))
                            ((eq id 3)
                             (make-google-bugged-bug
                              :id "3" :summary "Child 2" :blocked-by nil
                              :priority "P1" :status "ACCEPTED"))))))))
            (let* ((tree (deferred:sync!
                           (org-buganizer-extras--fetch-bug-tree-deferred 1)))
                   (parent (car tree))
                   (children (cdr tree))
                   (child1 (car children))
                   (child2 (cadr children)))
              (should (equal (google-bugged-bug-id parent) "1"))
              (should (equal (length children) 2))
              (should (equal (google-bugged-bug-id (car child1)) "2"))
              (should (null (cdr child1)))
              (should (equal (google-bugged-bug-id (car child2)) "3")))))
      (delete-directory temp-dir t))))

(ert-deftest org-buganizer-extras-fetch-tree-cycle ()
  "Verify tree fetching terminates when a cycle is detected."
  (cl-letf (((symbol-function 'google-issues-get-bug-deferred)
             (lambda (id-str)
               (let ((id (string-to-number id-str)))
                 (deferred:succeed
                   (cond
                    ((eq id 1)
                     (make-google-bugged-bug
                      :id "1" :summary "Parent" :blocked-by '(2)
                      :priority "P2" :status "ASSIGNED"))
                    ((eq id 2)
                     (make-google-bugged-bug
                      :id "2" :summary "Child" :blocked-by '(1)
                      :priority "P3" :status "NEW"))))))))
    (let* ((tree (deferred:sync!
                   (org-buganizer-extras--fetch-bug-tree-deferred 1)))
           (parent (car tree))
           (children (cdr tree))
           (child1 (car children))
           (child1-children (cdr child1)))
      (should (equal (google-bugged-bug-id parent) "1"))
      (should (equal (length children) 1))
      (should (equal (google-bugged-bug-id (car child1)) "2"))
      (should (null child1-children)))))

(ert-deftest org-buganizer-extras-fetch-tree-not-found ()
  "Verify tree fetching resolves to nil when bug is not found."
  (cl-letf (((symbol-function 'google-issues-get-bug-deferred)
             (lambda (_id-str)
               (deferred:succeed nil))))
    (let ((tree (deferred:sync!
                  (org-buganizer-extras--fetch-bug-tree-deferred 999))))
      (should (null tree)))))

(ert-deftest org-buganizer-extras-goto-query-heading ()
  "Verify cursor positioning functions."
  (org-buganizer-extras--with-temp-buffer
   "* Heading 1
** Query: 'parentid:123'
*** PROJ [#2] [[bug:123]]: Parent :bug:bug_123:
**** TODO [#3] [[bug:456]]: Child :bug:bug_456:
"
   (goto-char (point-min))
   (re-search-forward "Query: 'parentid:123'")
   (should (org-buganizer-extras-goto-query-heading))
   (should (looking-at-p "\\*\\* Query: 'parentid:123'"))

   (goto-char (point-min))
   (re-search-forward "Child")
   (should (org-buganizer-extras-goto-query-heading))
   (should (looking-at-p "\\*\\* Query: 'parentid:123'"))

   (goto-char (point-min))
   (re-search-forward "Heading 1")
   (should-not (org-buganizer-extras-goto-query-heading))))

(ert-deftest org-buganizer-extras-find-bug-heading ()
  "Verify find-bug-heading works."
  (org-buganizer-extras--with-temp-buffer
   "* Query: 'parentid:123'
** PROJ [#2] [[bug:123]]: Parent :bug:bug_123:
:PROPERTIES:
:buganizer_id: 123
:END:
*** TODO [#3] [[bug:456]]: Child :bug:bug_456:
:PROPERTIES:
:buganizer_id: 456
:END:
"
   (goto-char (point-min))
   (let ((parent-pos (org-buganizer-extras-find-bug-heading 123))
         (child-pos (org-buganizer-extras-find-bug-heading 456)))
     (should parent-pos)
     (should child-pos)
     (org-with-point-at parent-pos
       (should (looking-at-p
                "\\*\\* PROJ \\[#2\\] \\[\\[bug:123\\]\\]: Parent")))
     (org-with-point-at child-pos
       (should (looking-at-p
                "\\*\\*\\* TODO \\[#3\\] \\[\\[bug:456\\]\\]: Child")))
     (should-not (org-buganizer-extras-find-bug-heading 999)))))

(ert-deftest org-buganizer-extras-propose-title-link-generation ()
  "Verify advised propose-title uses Org ID link if local file exists."
  (let ((temp-dir (make-temp-file "org-buganizer-extras-test" t))
        (org-buganizer-extras-path-work-notes-dir nil))
    (unwind-protect
        (progn
          (setq org-buganizer-extras-path-work-notes-dir temp-dir)
          ;; Create dummy file with valid ID
          (write-region
           ":PROPERTIES:\n:ID: mock-id-123\n:END:\n#+title: Bug 123"
           nil (expand-file-name "bug-123-title.org" temp-dir))
          (let* ((bug (make-google-bugged-bug
                       :id "123" :summary "Bug 123" :priority "P2" :status "NEW"))
                 (title (org-buganizer--propose-title-from-bug bug)))
            (should (equal title "[[id:mock-id-123][Bug 123]]"))))
      (delete-directory temp-dir t))))

(ert-deftest org-buganizer-extras-sync-bug-recursive-update ()
  "Verify recursive sync updates existing and inserts new."
  (org-buganizer-extras--with-temp-buffer
   "* Query: 'parentid:1'
** PROJ [#2] [[bug:1]]: Parent :bug:bug_1:
:PROPERTIES:
:buganizer_id: 1
:buganizer_status: ASSIGNED
:END:
*** PROJ [#3] [[bug:2]]: Child 1 :bug:bug_2:
:PROPERTIES:
:buganizer_id: 2
:buganizer_status: NEW
:END:
"
   (cl-letf (((symbol-function 'google-issues-get-bug-deferred)
              (lambda (id-str)
                (message "MOCK google-issues-get-bug-deferred: id-str=%s" id-str)
                (let ((id (string-to-number id-str)))
                  (deferred:succeed
                    (let ((res
                           (cond
                            ((eq id 1)
                             (make-google-bugged-bug
                              :id "1" :summary "Parent" :blocked-by '(2 3)
                              :priority "P2" :status "FIXED" :type "PROJECT"))
                            ((eq id 2)
                             (make-google-bugged-bug
                              :id "2" :summary "Child 1" :blocked-by nil
                              :priority "P3" :status "ASSIGNED" :type "TASK"))
                            ((eq id 3)
                             (make-google-bugged-bug
                              :id "3" :summary "Child 2" :blocked-by nil
                              :priority "P1" :status "NEW" :type "STORY")))))
                      (message
                       "MOCK google-issues-get-bug-deferred: id=%s returning %s"
                       id res)
                      res))))))
     (goto-char (point-min))
     (re-search-forward "Query: 'parentid:1'")
     (deferred:sync! (org-buganizer-extras--sync-bug-deferred 1 2))
     
     (goto-char (point-min))
     (re-search-forward "\\[\\[bug:1\\]\\]")
     (org-back-to-heading)
     (should (looking-at-p "\\*\\* DONE \\[#B\\] \\[\\[bug:1\\]\\]: Parent"))
     (should (equal (org-entry-get (point) "buganizer_status") "FIXED"))

     (goto-char (point-min))
     (re-search-forward "\\[\\[bug:2\\]\\]")
     (org-back-to-heading)
     (should (looking-at-p
              "\\*\\*\\* TODO \\[#C\\] \\[\\[bug:2\\]\\]: Child 1"))
     (should (equal (org-entry-get (point) "buganizer_status") "ASSIGNED"))

     (goto-char (point-min))
     (re-search-forward "\\[\\[bug:3\\]\\]")
     (org-back-to-heading)
     (should (looking-at-p
              "\\*\\*\\* PROJ \\[#A\\] \\[\\[bug:3\\]\\]: Child 2"))
     (should (equal (org-entry-get (point) "buganizer_status") "NEW")))))

(ert-deftest org-buganizer-extras-fetch-tree-invalid-blocked-by ()
  "Verify tree fetching handles invalid blocked-by IDs gracefully."
  (cl-letf (((symbol-function 'google-issues-get-bug-deferred)
             (lambda (id-str)
               (let ((id (string-to-number id-str)))
                 (deferred:succeed
                   (cond
                    ((eq id 1)
                     (make-google-bugged-bug
                      :id "1" :summary "Parent" :blocked-by '("   " 2)
                      :priority "P2" :status "ASSIGNED"))
                    ((eq id 2)
                     (make-google-bugged-bug
                      :id "2" :summary "Child 1" :blocked-by nil
                      :priority "P3" :status "NEW"))))))))
    (let* ((tree (deferred:sync!
                   (org-buganizer-extras--fetch-bug-tree-deferred 1)))
           (parent (car tree))
           (children (cdr tree)))
      (should (equal (google-bugged-bug-id parent) "1"))
      (should (equal (length children) 1))
      (should (equal (google-bugged-bug-id (car (car children))) "2")))))

(ert-deftest org-buganizer-extras-sync-query-parent-child ()
  "Verify Parent/Child bulk sync works."
  (org-buganizer-extras--with-temp-buffer
   "* Query: 'parentid:1'
"
   (cl-letf (((symbol-function 'google-issues-get-bugs-deferred)
              (lambda (query)
                (should (equal query "parentid:1+ status:open"))
                (deferred:succeed
                  (list
                   (make-google-bugged-bug
                    :id "2" :summary "Child 1" :priority "P2" :status "NEW" :type "PROJECT")
                   (make-google-bugged-bug
                    :id "3" :summary "Child 2" :priority "P3" :status "ASSIGNED" :type "TASK")))))
             ((symbol-function 'google-issues-get-parent-ids)
              (lambda (id-str)
                (cond
                 ((equal id-str "2") '(1))
                 ((equal id-str "3") '(1))
                 (t nil)))))
     (goto-char (point-min))
     (re-search-forward "Query: 'parentid:1'")
     (deferred:sync! (org-buganizer-extras-sync-query "parentid:1" 2))
     
     (goto-char (point-min))
     (re-search-forward "\\[\\[bug:2\\]\\]")
     (org-back-to-heading)
     (should (looking-at-p "\\*\\* PROJ \\[#B\\] \\[\\[bug:2\\]\\]: Child 1"))
     
     (goto-char (point-min))
     (re-search-forward "\\[\\[bug:3\\]\\]")
     (org-back-to-heading)
     (should (looking-at-p "\\*\\* TODO \\[#C\\] \\[\\[bug:3\\]\\]: Child 2")))))
