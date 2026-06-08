;;; cashpw-time-test.el --- Tests for cashpw-time.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'cashpw-time)

;;; Helper Macros

(defmacro cashpw-time-test-with-mock-time (time-string &rest body)
  "Execute BODY with `current-time' mocked to return the time parsed from TIME-STRING.
TIME-STRING should be in a format accepted by `date-to-time', e.g., \"2026-05-25 12:00:00\"."
  (declare (indent 1))
  `(let ((mock-time (date-to-time ,time-string)))
     (cl-letf (((symbol-function 'current-time) (lambda () mock-time)))
       ,@body)))

;;; Test Cases

(ert-deftest cashpw-time-test-iso-week-to-time ()
  "Test ISO week to time conversions."
  (let ((time (cashpw-iso-week-to-time 2026 22 1))) ; ISO Week 22, Day 1 (Monday)
    (cl-destructuring-bind (_sec _min _hour day month year &rest _args)
        (decode-time time)
      (should (= day 25))
      (should (= month 5))
      (should (= year 2026))))

  (let ((time (cashpw-iso-week-to-time 2026 22 7))) ; ISO Week 22, Day 7 (Sunday)
    (cl-destructuring-bind (_sec _min _hour day month year &rest _args)
        (decode-time time)
      (should (= day 31))
      (should (= month 5))
      (should (= year 2026)))))

(ert-deftest cashpw-time-test-iso-week-bounds ()
  "Test start and end of ISO week calculations."
  ;; Beginning of week (Monday)
  (let ((start (cashpw-iso-beginning-of-week 2026 22)))
    (cl-destructuring-bind (_sec _min _hour day month year day-of-week &rest _args)
        (decode-time start)
      (should (= day 25))
      (should (= month 5))
      (should (= year 2026))
      (should (= day-of-week 1)))) ; Monday

  ;; End of week (Sunday)
  (let ((end (cashpw-iso-end-of-week 2026 22)))
    (cl-destructuring-bind (_sec _min _hour day month year day-of-week &rest _args)
        (decode-time end)
      (should (= day 31))
      (should (= month 5))
      (should (= year 2026))
      (should (= day-of-week 0))))) ; Sunday

(ert-deftest cashpw-time-test-time-overwrite ()
  "Test `cashpw-time--overwrite' individual component overrides."
  (let* ((base-time (date-to-time "2026-05-25 12:34:56"))
         ;; Overwrite hour and minute
         (overwritten (cashpw-time--overwrite base-time :hours 14 :minutes 0)))
    (cl-destructuring-bind (sec min hour day month year &rest _args)
        (decode-time overwritten)
      (should (= sec 56))
      (should (= min 0))
      (should (= hour 14))
      (should (= day 25))
      (should (= month 5))
      (should (= year 2026)))))

(ert-deftest cashpw-time-test-time-at-hh-mm ()
  "Test `cashpw-time-at-hh-mm' sets time components correctly."
  (let* ((base-time (date-to-time "2026-05-25 12:34:56"))
         (new-time (cashpw-time-at-hh-mm base-time 15 45)))
    (cl-destructuring-bind (sec min hour day month year &rest _args)
        (decode-time new-time)
      (should (= sec 0))
      (should (= min 45))
      (should (= hour 15))
      (should (= day 25))
      (should (= month 5))
      (should (= year 2026)))))

(ert-deftest cashpw-time-test-relative-today-tomorrow ()
  "Test today and tomorrow time instantiations with mock time."
  (cashpw-time-test-with-mock-time "2026-05-25 12:00:00"
    ;; Today at HH:MM
    (let ((today (cashpw-time-today-at-hh-mm 9 30)))
      (cl-destructuring-bind (sec min hour day month year &rest _args)
          (decode-time today)
        (should (= sec 0))
        (should (= min 30))
        (should (= hour 9))
        (should (= day 25))
        (should (= month 5))
        (should (= year 2026))))

    ;; Tomorrow at HH:MM
    (let ((tomorrow (cashpw-time-tomorrow-at-hh-mm 10 15)))
      (cl-destructuring-bind (sec min hour day month year &rest _args)
          (decode-time tomorrow)
        (should (= sec 0))
        (should (= min 15))
        (should (= hour 10))
        (should (= day 26))
        (should (= month 5))
        (should (= year 2026))))))

(ert-deftest cashpw-time-test-end-of-day ()
  "Test `cashpw-time--end-of-day' caps HH:MM:SS."
  (let* ((base-time (date-to-time "2026-05-25 12:34:56"))
         (eod (cashpw-time--end-of-day base-time)))
    (cl-destructuring-bind (sec min hour day month year &rest _args)
        (decode-time eod)
      (should (= sec 59))
      (should (= min 59))
      (should (= hour 23))
      (should (= day 25))
      (should (= month 5))
      (should (= year 2026)))))

(ert-deftest cashpw-time-test-future-past-p ()
  "Test future and past predicates."
  (cashpw-time-test-with-mock-time "2026-05-25 12:00:00"
    (let ((future-time (date-to-time "2026-05-25 12:00:01"))
          (past-time (date-to-time "2026-05-25 11:59:59"))
          (current (current-time)))
      ;; Future predicate
      (should (cashpw-time-future-p future-time))
      (should-not (cashpw-time-future-p past-time))
      (should-not (cashpw-time-future-p current))

      ;; Past predicate
      (should (cashpw-time-past-p past-time))
      (should-not (cashpw-time-past-p future-time))
      (should-not (cashpw-time-past-p current)))))

(ert-deftest cashpw-time-test-tomorrow-p ()
  "Test tomorrow predicate."
  (cashpw-time-test-with-mock-time "2026-05-25 12:00:00"
    (let ((today-midday (date-to-time "2026-05-25 12:30:00"))
          (tomorrow-early (date-to-time "2026-05-26 00:01:00"))
          (tomorrow-late (date-to-time "2026-05-26 23:59:00"))
          (day-after-tomorrow (date-to-time "2026-05-27 12:00:00")))
      (should-not (cashpw-time-tomorrow-p today-midday))
      (should (cashpw-time-tomorrow-p tomorrow-early))
      (should (cashpw-time-tomorrow-p tomorrow-late))
      (should-not (cashpw-time-tomorrow-p day-after-tomorrow)))))

(ert-deftest cashpw-time-test-same-day-today-yesterday-p ()
  "Test same day, today, and yesterday predicates."
  (cashpw-time-test-with-mock-time "2026-05-25 12:00:00"
    (let ((today-midday (date-to-time "2026-05-25 12:30:00"))
          (today-evening (date-to-time "2026-05-25 23:00:00"))
          (yesterday (date-to-time "2026-05-24 12:00:00"))
          (tomorrow (date-to-time "2026-05-26 12:00:00")))
      ;; Same day
      (should (cashpw-time-same-day-p today-midday today-evening))
      (should-not (cashpw-time-same-day-p today-midday yesterday))

      ;; Today predicate
      (should (cashpw-time-today-p today-midday))
      (should (cashpw-time-today-p today-evening))
      (should-not (cashpw-time-today-p yesterday))
      (should-not (cashpw-time-today-p tomorrow))

      ;; Yesterday predicate
      (should (cashpw-time-yesterday-p yesterday))
      (should-not (cashpw-time-yesterday-p today-midday))
      (should-not (cashpw-time-yesterday-p tomorrow)))))

(ert-deftest cashpw-time-test-zero-out-hh-mm-ss ()
  "Test `cashpw-time--zero-out-hh-mm-ss' sets HH:MM:SS to 0."
  (let* ((base-time (date-to-time "2026-05-25 12:34:56"))
         (zeroed (cashpw-time--zero-out-hh-mm-ss base-time)))
    (cl-destructuring-bind (sec min hour day month year &rest _args)
        (decode-time zeroed)
      (should (= sec 0))
      (should (= min 0))
      (should (= hour 0))
      (should (= day 25))
      (should (= month 5))
      (should (= year 2026)))))

(ert-deftest cashpw-time-test-greater-than-p ()
  "Test `cashpw-time-greater-than-p' comparison logic."
  (let ((early (date-to-time "2026-05-25 12:00:00"))
        (late (date-to-time "2026-05-25 13:00:00")))
    (should (cashpw-time-greater-than-p late early))
    (should-not (cashpw-time-greater-than-p early late))
    (should-not (cashpw-time-greater-than-p early early))))

(ert-deftest cashpw-time-test-days-between ()
  "Test `cashpw-time-days-between' returns correct day differences."
  (let ((day1 (date-to-time "2026-05-25 12:00:00"))
        (day2 (date-to-time "2026-05-28 12:00:00"))
        (day3 (date-to-time "2026-05-24 01:00:00")))
    (should (= (cashpw-time-days-between day1 day2) 3))
    (should (= (cashpw-time-days-between day1 day3) -1))
    (should (= (cashpw-time-days-between day1 day1) 0))))

(ert-deftest cashpw-time-test-get-week-times ()
  "Test `cashpw-time--get-week-times' returns correct 7 days of the week."
  (let* ((base-time (date-to-time "2026-05-25 12:00:00")) ; Monday
         (week-times (cashpw-time--get-week-times base-time)))
    (should (= (length week-times) 7))
    ;; Check each day of the week
    (dotimes (i 7)
      (cl-destructuring-bind (_sec _min _hour day month year day-of-week &rest _args)
          (decode-time (nth i week-times))
        (should (= day (+ 25 i)))
        (should (= month 5))
        (should (= year 2026))
        ;; day-of-week: Monday=1, ..., Saturday=6, Sunday=0
        (should (= day-of-week (mod (+ 1 i) 7)))))))

(ert-deftest cashpw-time-test-week-tags ()
  "Test `cashpw-time-current-week-tag' and `cashpw-time-next-week-tag'."
  ;; Standard Mid-Year
  (cashpw-time-test-with-mock-time "2026-05-25 12:00:00"
    (should (equal (cashpw-time-current-week-tag) "2026w22"))
    (should (equal (cashpw-time-next-week-tag) "2026w23")))

  ;; Pre-Wrap Week 52
  (cashpw-time-test-with-mock-time "2026-12-21 12:00:00"
    (should (equal (cashpw-time-current-week-tag) "2026w52"))
    (should (equal (cashpw-time-next-week-tag) "2026w53")))

  ;; Wrap Week 53 Mon
  (cashpw-time-test-with-mock-time "2026-12-28 12:00:00"
    (should (equal (cashpw-time-current-week-tag) "2026w53"))
    (should (equal (cashpw-time-next-week-tag) "2027w01")))

  ;; Wrap Week 53 Sun
  (cashpw-time-test-with-mock-time "2027-01-03 12:00:00"
    (should (equal (cashpw-time-current-week-tag) "2026w53"))
    (should (equal (cashpw-time-next-week-tag) "2027w01")))

  ;; Post-Wrap Week 01
  (cashpw-time-test-with-mock-time "2027-01-04 12:00:00"
    (should (equal (cashpw-time-current-week-tag) "2027w01"))
    (should (equal (cashpw-time-next-week-tag) "2027w02")))

  ;; DST Spring Forward Transition Boundary (US Eastern Time)
  ;; Sunday March 1, 2026 is week 9. DST starts Sunday, March 8, 2026.
  ;; Next week should be week 10 (March 2 to March 8).
  (let ((old-tz (getenv "TZ")))
    (unwind-protect
        (progn
          (set-time-zone-rule "America/New_York")
          (cashpw-time-test-with-mock-time "2026-03-01 23:30:00"
            (should (equal (cashpw-time-current-week-tag) "2026w09"))
            (should (equal (cashpw-time-next-week-tag) "2026w10"))))
      (set-time-zone-rule old-tz)))

  ;; Midnight week boundary transition
  (cashpw-time-test-with-mock-time "2026-03-01 23:59:59"
    (should (equal (cashpw-time-current-week-tag) "2026w09"))
    (should (equal (cashpw-time-next-week-tag) "2026w10")))

  (cashpw-time-test-with-mock-time "2026-03-02 00:00:01"
    (should (equal (cashpw-time-current-week-tag) "2026w10"))
    (should (equal (cashpw-time-next-week-tag) "2026w11")))

  ;; Test with explicit time parameter (no mocking needed)
  (let ((explicit-time (date-to-time "2026-05-25 12:00:00")))
    (should (equal (cashpw-time-current-week-tag explicit-time) "2026w22"))
    (should (equal (cashpw-time-next-week-tag explicit-time) "2026w23"))))

(ert-deftest cashpw-time-test-upcoming-weekday ()
  "Test `cashpw-time-upcoming-weekday` for various targets and base times."
  ;; Target Monday (1)
  (cashpw-time-test-with-mock-time "2026-05-25 12:00:00" ; Monday
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 1)) "2026-05-25"))
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 2)) "2026-05-26")) ; Tue
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 5)) "2026-05-29")) ; Fri
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 6)) "2026-05-30")) ; Sat
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 7)) "2026-05-31"))) ; Sun

  (cashpw-time-test-with-mock-time "2026-05-26 12:00:00" ; Tuesday
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 1)) "2026-06-01")) ; Next Mon
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 2)) "2026-05-26")) ; Today Tue
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 5)) "2026-05-29")) ; Fri
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 6)) "2026-05-30")) ; Sat
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 7)) "2026-05-31"))) ; Sun

  (cashpw-time-test-with-mock-time "2026-05-31 12:00:00" ; Sunday (ISO 7)
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 1)) "2026-06-01")) ; Next Mon
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 5)) "2026-06-05")) ; Next Fri
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 6)) "2026-06-06")) ; Next Sat
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-weekday 7)) "2026-05-31")))) ; Today Sun

(ert-deftest cashpw-time-test-upcoming-specific-weekdays ()
  "Test specific weekday wrappers."
  (cashpw-time-test-with-mock-time "2026-05-25 12:00:00" ; Monday
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-monday)) "2026-05-25"))
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-tuesday)) "2026-05-26"))
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-wednesday)) "2026-05-27"))
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-thursday)) "2026-05-28"))
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-friday)) "2026-05-29"))
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-saturday)) "2026-05-30"))
    (should (equal (format-time-string "%Y-%m-%d" (cashpw-time-upcoming-sunday)) "2026-05-31"))))

(ert-deftest cashpw-time-test-upcoming-weekday-string ()
  "Test `cashpw-time-upcoming-weekday-string`."
  (cashpw-time-test-with-mock-time "2026-05-25 12:00:00" ; Monday
    (should (equal (cashpw-time-upcoming-friday-string) "2026-05-29 Fri"))
    (should (equal (cashpw-time-upcoming-sunday-string) "2026-05-31 Sun"))))

;;; cashpw-time-test.el ends here
