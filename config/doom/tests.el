(ert-deftest cashpw/org-agenda-get-property-on-line-test ()
  "Test retrieving text properties anywhere on a line."
  (with-temp-buffer
    (insert "  Category:  Some Headline Text")
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      ;; Apply property to a sub-span
      (put-text-property (+ beg 5) (+ beg 15) 'effort-minutes 45)
      (put-text-property (+ beg 10) (+ beg 20) 'custom-prop "val")
      
      (goto-char beg)
      (should (equal (cashpw/org-agenda-get-property-on-line 'effort-minutes) 45))
      (should (equal (cashpw/org-agenda-get-property-on-line 'custom-prop) "val"))
      (should (null (cashpw/org-agenda-get-property-on-line 'non-existent)))
      
      ;; Move point to various positions on the line and ensure it still works
      (goto-char (+ beg 10))
      (should (equal (cashpw/org-agenda-get-property-on-line 'effort-minutes) 45))
      (goto-char (- end 1))
      (should (equal (cashpw/org-agenda-get-property-on-line 'effort-minutes) 45)))))

(ert-deftest cashpw/org-agenda-calculate-total-effort-test ()
  "Test summing effort-minutes in agenda buffer."
  (with-temp-buffer
    (insert "Line 1: Task A\n")
    (insert "Line 2: Task B\n")
    (insert "Line 3: Task C\n")
    
    ;; Apply effort-minutes and custom type properties
    (goto-char (point-min))
    ;; Line 1
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (put-text-property (+ beg 2) (+ beg 10) 'effort-minutes 30)
      (put-text-property (+ beg 2) (+ beg 10) 'task-type "work"))
    
    (forward-line 1)
    ;; Line 2
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (put-text-property (+ beg 2) (+ beg 10) 'effort-minutes 60)
      (put-text-property (+ beg 2) (+ beg 10) 'task-type "personal"))
    
    (forward-line 1)
    ;; Line 3
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (put-text-property (+ beg 2) (+ beg 10) 'effort-minutes 15)
      (put-text-property (+ beg 2) (+ beg 10) 'task-type "work"))
    
    ;; Test total effort without predicate
    (should (equal (cashpw/org-agenda-calculate-total-effort) 105))
    
    ;; Test total effort with predicate (only "work" tasks)
    (should (equal (cashpw/org-agenda-calculate-total-effort
                    (lambda ()
                      (equal (cashpw/org-agenda-get-property-on-line 'task-type) "work")))
                   45))
    
    ;; Test total effort with predicate (only "personal" tasks)
    (should (equal (cashpw/org-agenda-calculate-total-effort
                    (lambda ()
                      (equal (cashpw/org-agenda-get-property-on-line 'task-type) "personal")))
                   60))

    ;; Test edge case 1: Predicate that moves point (Point Safety)
    (should (equal (cashpw/org-agenda-calculate-total-effort
                    (lambda ()
                      (goto-char (point-min))
                      t))
                   105))

    ;; Test edge case 2: Non-numeric / malformed effort values
    (with-temp-buffer
      (insert "Line 1: Bad Task\n")
      (insert "Line 2: Good Task\n")
      (goto-char (point-min))
      ;; Line 1: non-numeric effort property
      (let ((beg (line-beginning-position)))
        (put-text-property (+ beg 2) (+ beg 10) 'effort-minutes "invalid-string"))
      
      (forward-line 1)
      ;; Line 2: numeric effort property
      (let ((beg (line-beginning-position)))
        (put-text-property (+ beg 2) (+ beg 10) 'effort-minutes 30))
      
      ;; Should ignore the non-numeric effort and only sum the numeric one (30)
      (should (equal (cashpw/org-agenda-calculate-total-effort) 30)))))

(ert-deftest cashpw/org-agenda-display-planned-capacity-header-test ()
  "Test the capacity header injection, teardown, and safety logic."
  (let ((org-keys ".today")
        (cashpw-org-agenda-daily-capacity-hours 6.0))
    ;; 1. Happy Path
    (with-temp-buffer
      (insert "Today's Schedule\n")
      (insert "  Task 1 (scheduled today)\n")
      (insert "  Task 2 (not scheduled, weekly priority backlog)\n")
      
      ;; Apply properties to Task 1
      (goto-char (point-min))
      (forward-line 1)
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (put-text-property beg end 'date "2026-05-26")
        (put-text-property beg end 'effort-minutes 120)) ; 2.0h
      
      ;; Apply properties to Task 2 (no date, but has effort)
      (forward-line 1)
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (put-text-property beg end 'effort-minutes 180)) ; 3.0h (should be ignored)
      
      ;; Run injection
      (cashpw/org-agenda-display-planned-capacity-header)
      
      ;; Verify injection
      (goto-char (point-min))
      (should (re-search-forward "^Planned Today: 2\\.0h / 6\\.0h Capacity (33%)\n\n" nil t))
      ;; Verify the header carries the custom 'cashpw-header text property on the first char
      (should (get-text-property (point-min) 'cashpw-header))
      
      ;; 2. Idempotency / Teardown
      ;; Call it again. If teardown works, the header shouldn't be duplicated.
      (cashpw/org-agenda-display-planned-capacity-header)
      (goto-char (point-min))
      ;; Count matches of the header
      (let ((matches 0))
        (while (re-search-forward "^Planned Today: 2\\.0h / 6\\.0h Capacity (33%)\n\n" nil t)
          (setq matches (1+ matches)))
        (should (equal matches 1))))
    
    ;; 3. No-Op Buffer (when org-keys is different)
    (let ((org-keys ".other-view"))
      (with-temp-buffer
        (insert "Today's Schedule\n")
        (insert "  Task 1\n")
        (goto-char (point-min))
        (forward-line 1)
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (put-text-property beg end 'date "2026-05-26")
          (put-text-property beg end 'effort-minutes 120))
        
        (cashpw/org-agenda-display-planned-capacity-header)
        ;; Verify NO header is injected
        (goto-char (point-min))
        (should-not (re-search-forward "^Planned Today:" nil t))))))

(ert-deftest cashpw/org-agenda-item-tags-marker-fallback-test ()
  "Verify marker-based fallback path when 'tags property is absent."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :work:personal:\n")
    (let* ((marker (copy-marker (point-min)))
           (item (propertize "Task" 'org-marker marker)))
      (should (equal (cashpw/org-agenda-item-tags item) '("work" "personal"))))))

(ert-deftest cashpw/org-agenda-item-has-week-tag-p-test ()
  "Verify cashpw/org-agenda-item-has-week-tag-p detects YYYYwWW tags."
  (let ((item-with-week-tag (propertize "Task 1" 'tags '("work" "2026w22")))
        (item-without-week-tag (propertize "Task 2" 'tags '("work" "personal")))
        (item-with-malformed-tag (propertize "Task 3" 'tags '("2026w2" "2026w222"))))
    (should (cashpw/org-agenda-item-has-week-tag-p item-with-week-tag))
    (should-not (cashpw/org-agenda-item-has-week-tag-p item-without-week-tag))
    (should-not (cashpw/org-agenda-item-has-week-tag-p item-with-malformed-tag))))

(ert-deftest cashpw/org-agenda-item-future-week-p-test ()
  "Verify cashpw/org-agenda-item-future-week-p compares tags lexicographically."
  (cl-letf (((symbol-function 'cashpw-time-next-week-tag)
             (lambda () "2026w23")))
    (let ((item-past (propertize "Past" 'tags '("2026w22")))
          (item-current (propertize "Current" 'tags '("2026w23")))
          (item-future (propertize "Future" 'tags '("2026w24")))
          (item-no-tag (propertize "No Tag" 'tags '("work"))))
      (should-not (cashpw/org-agenda-item-future-week-p item-past))
      (should-not (cashpw/org-agenda-item-future-week-p item-current))
      (should (cashpw/org-agenda-item-future-week-p item-future))
      (should-not (cashpw/org-agenda-item-future-week-p item-no-tag)))))

(ert-deftest cashpw/org-agenda-current-line-tags-marker-fallback-test ()
  "Verify cashpw/org-agenda-current-line-tags marker-based fallback path."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :work:personal:\n")
    (let ((marker (copy-marker (point-min))))
      (with-temp-buffer
        (insert "  Task\n")
        (put-text-property (point-min) (line-end-position) 'org-marker marker)
        (goto-char (point-min))
        (should (equal (cashpw/org-agenda-current-line-tags) '("work" "personal")))))))

(ert-deftest cashpw/org-agenda-view--plan--week-test ()
  "Verify the structural correctness of the custom Week Planner view."
  (cl-letf (((symbol-function 'cashpw-time-current-week-tag)
             (lambda () "2026w22"))
            ((symbol-function 'cashpw-time-next-week-tag)
             (lambda () "2026w23")))
    (let* ((view (cashpw/org-agenda-view--plan--week))
           (block (car view))
           (type (car block))
           (opts (caddr block))
           (groups-raw (cadr (assoc 'org-super-agenda-groups opts)))
           (groups (if (eq (car groups-raw) '\`) (cadr groups-raw) groups-raw))
           (eval-val (lambda (val)
                       (if (and (listp val) (eq (car val) '\,))
                           (eval (cadr val))
                         val))))
      ;; 1. Outer structure is a list of length 1 containing alltodo command
      (should (equal (length view) 1))
      (should (equal type 'alltodo))
      ;; 2. Header is correct
      (should (equal (cadr (assoc 'org-agenda-overriding-header opts)) "Week Planner & Backlog Board"))
      ;; 3. Groups structure matches spec (indices shifted by 1 due to prepended PROJ discard group)
      (should (plist-get (nth 0 groups) :discard))
      (should (equal (plist-get (nth 1 groups) :name) "This Week"))
      (should (equal (funcall eval-val (plist-get (nth 1 groups) :tag)) "2026w22"))
      
      (should (equal (plist-get (nth 2 groups) :name) "Next Week"))
      (should (equal (funcall eval-val (plist-get (nth 2 groups) :tag)) "2026w23"))
      
      (should (equal (plist-get (nth 3 groups) :name) "Future Weeks"))
      (should (equal (plist-get (nth 3 groups) :pred) 'cashpw/org-agenda-item-future-week-p))
      
      (should (equal (plist-get (nth 4 groups) :name) "Unassigned Backlog"))
      (should (equal (plist-get (nth 4 groups) :and)
                      '(:todo t
                        :not (:scheduled t)
                        :not (:deadline t)
                        :not (:tag "unscheduled")
                        :not (:pred cashpw/org-agenda-item-has-week-tag-p))))
      
      (should (plist-get (nth 5 groups) :discard)))))

(ert-deftest cashpw/org-agenda-display-planned-capacity-header-weekly-test ()
  "Test the weekly capacity header injection, filtering, and safety logic."
  (cl-letf (((symbol-function 'cashpw-time-current-week-tag)
             (lambda () "2026w22"))
            ((symbol-function 'cashpw-time-next-week-tag)
             (lambda () "2026w23")))
    (let ((org-keys ".plan-week")
          (cashpw-org-agenda-weekly-capacity-hours 30.0))
      (with-temp-buffer
        (insert "Week Planner & Backlog Board\n")
        (insert "  Task 1 (this week)\n")
        (insert "  Task 2 (next week)\n")

        ;; Apply properties and current week tag to Task 1
        (goto-char (point-min))
        (forward-line 1)
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (put-text-property beg end 'effort-minutes 300)
          (put-text-property beg end 'tags '("2026w22"))) ; 5.0h

        ;; Apply properties and next week tag to Task 2
        (forward-line 1)
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (put-text-property beg end 'effort-minutes 600)
          (put-text-property beg end 'tags '("2026w23"))) ; 10.0h

        ;; Run injection
        (cashpw/org-agenda-display-planned-capacity-header)

        ;; Verify injection (should only sum Task 1)
        (goto-char (point-min))
        (should (re-search-forward "^Planned This Week: 5\\.0h / 30\\.0h Capacity (17% of Weekly Limit)\n\n" nil t))
        (should (get-text-property (point-min) 'cashpw-header))

        ;; Verify idempotency
        (cashpw/org-agenda-display-planned-capacity-header)
        (goto-char (point-min))
        (let ((matches 0))
          (while (re-search-forward "^Planned This Week: 5\\.0h" nil t)
            (setq matches (1+ matches)))
          (should (equal matches 1)))))))

(ert-deftest cashpw/org-agenda-display-planned-capacity-header-weekly-zero-capacity-test ()
  "Verify that a weekly capacity of 0.0 is handled safely without division by zero."
  (cl-letf (((symbol-function 'cashpw-time-current-week-tag)
             (lambda () "2026w22"))
            ((symbol-function 'cashpw-time-next-week-tag)
             (lambda () "2026w23")))
    (let ((org-keys ".plan-week")
          (cashpw-org-agenda-weekly-capacity-hours 0.0))
      (with-temp-buffer
        (insert "Week Planner & Backlog Board\n")
        (insert "  Task 1 (this week)\n")
        (goto-char (point-min))
        (forward-line 1)
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (put-text-property beg end 'effort-minutes 300)
          (put-text-property beg end 'tags '("2026w22"))) ; 5.0h
        
        (cashpw/org-agenda-display-planned-capacity-header)
        
        (goto-char (point-min))
        (should (re-search-forward "^Planned This Week: 5\\.0h / 0\\.0h Capacity (0% of Weekly Limit)\n\n" nil t))))))

(ert-deftest cashpw/org-agenda-display-planned-capacity-header-weekly-isolation-test ()
  "Verify that no capacity header is injected when org-keys is set to an unrelated view."
  (let ((org-keys ".other-view")
        (cashpw-org-agenda-weekly-capacity-hours 30.0))
    (with-temp-buffer
      (insert "Some Other Board\n")
      (insert "  Task 1\n")
      
      (cashpw/org-agenda-display-planned-capacity-header)
      
      (goto-char (point-min))
      (should-not (re-search-forward "^Planned" nil t)))))

(ert-deftest cashpw/org-agenda-update-week-tags-test ()
  "Test tag updates using cashpw/org-agenda-update-week-tags."
  ;; 1. Add a new week tag (replaces existing week tag)
  (should (equal (cashpw/org-agenda-update-week-tags '("work" "2026w21") "2026w22")
                 '("work" "2026w22")))
  ;; 2. Empty week tag removes existing week tag
  (should (equal (cashpw/org-agenda-update-week-tags '("work" "2026w21") "")
                 '("work")))
  ;; 3. Nil week tag removes existing week tag
  (should (equal (cashpw/org-agenda-update-week-tags '("work" "2026w21") nil)
                 '("work")))
  ;; 4. Invalid week tag signals a user-error
  (should-error (cashpw/org-agenda-update-week-tags '("work") "invalid-week")
                :type 'user-error))

(ert-deftest cashpw/org-agenda-assign-to-week-replaces-existing-week-tag ()
  "Test that assigning a new week tag replaces the existing week tag on the entry at point."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task 1 :work:2026w21:\n")
    (let ((marker (copy-marker (point-min))))
      (cl-letf (((symbol-function 'org-agenda-redo) #'ignore))
        (with-temp-buffer
          (insert "  Task 1\n")
          (put-text-property (point-min) (line-end-position) 'org-marker marker)
          (goto-char (point-min))
          (cashpw/org-agenda-assign-to-week "2026w22")
          (with-current-buffer (marker-buffer marker)
            (org-with-point-at marker
              (should (equal (org-get-tags) '("work" "2026w22"))))))))))

(ert-deftest cashpw/org-agenda-assign-to-week-removes-tag-when-empty ()
  "Test that assigning an empty string removes any existing week tag."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task 1 :work:2026w21:\n")
    (let ((marker (copy-marker (point-min))))
      (cl-letf (((symbol-function 'org-agenda-redo) #'ignore))
        (with-temp-buffer
          (insert "  Task 1\n")
          (put-text-property (point-min) (line-end-position) 'org-marker marker)
          (goto-char (point-min))
          (cashpw/org-agenda-assign-to-week "")
          (with-current-buffer (marker-buffer marker)
            (org-with-point-at marker
              (should (equal (org-get-tags) '("work"))))))))))

(ert-deftest cashpw/org-agenda-assign-to-week-removes-tag-when-nil ()
  "Test that assigning nil removes any existing week tag."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task 1 :work:2026w21:\n")
    (let ((marker (copy-marker (point-min))))
      (cl-letf (((symbol-function 'org-agenda-redo) #'ignore))
        (with-temp-buffer
          (insert "  Task 1\n")
          (put-text-property (point-min) (line-end-position) 'org-marker marker)
          (goto-char (point-min))
          (cashpw/org-agenda-assign-to-week nil)
          (with-current-buffer (marker-buffer marker)
            (org-with-point-at marker
              (should (equal (org-get-tags) '("work"))))))))))

(ert-deftest cashpw/org-agenda-assign-to-week-preserves-inherited-tags ()
  "Test that assigning a week tag does NOT convert inherited tags into local tags."
  (with-temp-buffer
    (org-mode)
    ;; Parent heading with :noexport: tag
    (insert "* Parent :noexport:\n")
    ;; Child heading (Task 1) with local :work: tag
    (insert "** TODO Task 1 :work:2026w21:\n")
    (let ((marker (copy-marker (line-beginning-position 0))))
      (cl-letf (((symbol-function 'org-agenda-redo) #'ignore))
        (with-temp-buffer
          (insert "  Task 1\n")
          (put-text-property (point-min) (line-end-position) 'org-marker marker)
          (goto-char (point-min))
          (cashpw/org-agenda-assign-to-week "2026w22")
          (with-current-buffer (marker-buffer marker)
            (org-with-point-at marker
              ;; Local tags should only be "work" and "2026w22". "noexport" should NOT be here locally!
              (should (equal (org-get-tags nil 'local) '("work" "2026w22")))
              ;; But it should still be inherited globally
              (should (member "noexport" (org-get-tags))))))))))

(ert-deftest cashpw/org-agenda-assign-to-week-no-marker-test ()
  "Test that cashpw/org-agenda-assign-to-week fails gracefully with a user-error if no marker is present."
  (with-temp-buffer
    (insert "  Task without marker\n")
    (goto-char (point-min))
    (should-error (cashpw/org-agenda-assign-to-week "2026w22") :type 'user-error)))

(ert-deftest cashpw/org-agenda-assign-to-week-invalid-marker-test ()
  "Test that cashpw/org-agenda-assign-to-week fails gracefully with a user-error if marker points to a dead buffer."
  (let ((marker (make-marker)))
    (with-temp-buffer
      (insert "  Task with dead marker\n")
      (put-text-property (point-min) (line-end-position) 'org-marker marker)
      (goto-char (point-min))
      (should-error (cashpw/org-agenda-assign-to-week "2026w22") :type 'user-error))))

(ert-deftest cashpw/org-agenda-assign-to-this-week-test ()
  "Test that cashpw/org-agenda-assign-to-this-week assigns the current week tag."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task 1 :work:\n")
    (let ((marker (copy-marker (point-min))))
      (cl-letf (((symbol-function 'org-agenda-redo) #'ignore)
                ((symbol-function 'cashpw-time-current-week-tag) (lambda () "2026w22")))
        (with-temp-buffer
          (insert "  Task 1\n")
          (put-text-property (point-min) (line-end-position) 'org-marker marker)
          (goto-char (point-min))
          (cashpw/org-agenda-assign-to-this-week)
          (with-current-buffer (marker-buffer marker)
            (org-with-point-at marker
              (should (equal (org-get-tags) '("work" "2026w22"))))))))))

(ert-deftest cashpw/org-agenda-assign-to-next-week-test ()
  "Test that cashpw/org-agenda-assign-to-next-week assigns the next week tag."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task 1 :work:\n")
    (let ((marker (copy-marker (point-min))))
      (cl-letf (((symbol-function 'org-agenda-redo) #'ignore)
                ((symbol-function 'cashpw-time-next-week-tag) (lambda () "2026w23")))
        (with-temp-buffer
          (insert "  Task 1\n")
          (put-text-property (point-min) (line-end-position) 'org-marker marker)
          (goto-char (point-min))
          (cashpw/org-agenda-assign-to-next-week)
          (with-current-buffer (marker-buffer marker)
            (org-with-point-at marker
              (should (equal (org-get-tags) '("work" "2026w23"))))))))))

(ert-deftest cashpw/work-buganizer-check-on-state-change-bypass-test ()
  "Test that declining the Buganizer prompt tags the entry with 'needs_bug'."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test Task\n")
    (goto-char (point-min))
    (let ((org-state "INPROGRESS")
          (noninteractive nil)
          (called-add-bug nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                ((symbol-function 'org-buganizer-add-bug-to-entry) (lambda () (interactive) (setq called-add-bug t))))
        (cashpw/work-buganizer-check-on-state-change)
        (should-not called-add-bug)
        (should (member "needs_bug" (org-get-tags)))))))

(ert-deftest cashpw/work-buganizer-check-on-state-change-link-test ()
  "Test that accepting the Buganizer prompt calls `org-buganizer-add-bug-to-entry'."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test Task\n")
    (goto-char (point-min))
    (let ((org-state "INPROGRESS")
          (noninteractive nil)
          (called-add-bug nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'org-buganizer-add-bug-to-entry) (lambda () (interactive) (setq called-add-bug t))))
        (cashpw/work-buganizer-check-on-state-change)
        (should called-add-bug)
        (should-not (member "needs_bug" (org-get-tags)))))))

(ert-deftest cashpw/work-buganizer-check-on-state-change-noop-test ()
  "Test that when buganizer_id is already present, no prompts are shown and no tags are added."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test Task\n  :PROPERTIES:\n  :buganizer_id: 12345\n  :END:\n")
    (goto-char (point-min))
    (let ((org-state "INPROGRESS")
          (called-y-or-n nil)
          (called-add-bug nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (setq called-y-or-n t) nil))
                ((symbol-function 'org-buganizer-add-bug-to-entry) (lambda () (interactive) (setq called-add-bug t))))
        (cashpw/work-buganizer-check-on-state-change)
        (should-not called-y-or-n)
        (should-not called-add-bug)
        (should-not (member "needs_bug" (org-get-tags)))))))

(ert-deftest cashpw/work-buganizer-check-on-state-change-wrong-state-test ()
  "Test that transitioning to a state other than 'INPROGRESS' does nothing."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test Task\n")
    (goto-char (point-min))
    (let ((org-state "WAITING")
          (called-y-or-n nil)
          (called-add-bug nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (setq called-y-or-n t) nil))
                ((symbol-function 'org-buganizer-add-bug-to-entry) (lambda () (interactive) (setq called-add-bug t))))
        (cashpw/work-buganizer-check-on-state-change)
        (should-not called-y-or-n)
        (should-not called-add-bug)
        (should-not (member "needs_bug" (org-get-tags)))))))
