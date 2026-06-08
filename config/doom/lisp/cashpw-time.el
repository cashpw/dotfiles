;;; cashpw-time.el --- Time helper utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 cashweaver

;; Author: cashweaver
;; Keywords: calendar, time, utilities
;; Package-Requires: ((emacs "29.1") (dash "2.19.0"))
;; Version: 0.1.0

;;; Commentary:

;; Provides custom Emacs Lisp functions and macros for date/time calculations,
;; conversions, and comparisons.

;;; Code:

(require 'calendar)
(require 'cal-iso)
(require 'cl-lib)
(require 'dash)

;;; Constants

(defconst cashpw-time--day-number-sunday 0)
(defconst cashpw-time--day-number-monday 1)
(defconst cashpw-time--day-number-tuesday 2)
(defconst cashpw-time--day-number-wednesday 3)
(defconst cashpw-time--day-number-thursday 4)
(defconst cashpw-time--day-number-friday 5)
(defconst cashpw-time--day-number-saturday 6)

(defconst cashpw-time--month-number-january 1)
(defconst cashpw-time--month-number-february 2)
(defconst cashpw-time--month-number-march 3)
(defconst cashpw-time--month-number-april 4)
(defconst cashpw-time--month-number-may 5)
(defconst cashpw-time--month-number-june 6)
(defconst cashpw-time--month-number-july 7)
(defconst cashpw-time--month-number-august 8)
(defconst cashpw-time--month-number-september 9)
(defconst cashpw-time--month-number-october 10)
(defconst cashpw-time--month-number-november 11)
(defconst cashpw-time--month-number-december 12)

;;; Functions

(defun cashpw-iso-week-to-time (year week day)
  "Convert ISO YEAR, WEEK, and DAY to an Emacs Lisp time value."
  (apply #'encode-time
         (append '(0 0 0)
                  (-select-by-indices
                   '(1 0 2)
                   (calendar-gregorian-from-absolute (calendar-iso-to-absolute
                                                      (list week day year)))))))

(defun cashpw-iso-beginning-of-week (year week)
  "Convert ISO YEAR and WEEK to elisp time for first day (Monday) of week."
  (cashpw-iso-week-to-time year week 1))

(defun cashpw-iso-end-of-week (year week)
  "Convert ISO YEAR and WEEK to elisp time for last day (Sunday) of week."
  (cashpw-iso-week-to-time year week 7))

(cl-defun cashpw-time--overwrite (time
                                  &key
                                  seconds
                                  minutes
                                  hours
                                  day
                                  month
                                  year
                                  day-of-week
                                  daylight-savings-time-p
                                  utc-offset)
  "Return TIME with components overwritten.
Keyword arguments SECONDS, MINUTES, HOURS, DAY, MONTH, YEAR,
DAY-OF-WEEK, DAYLIGHT-SAVINGS-TIME-P, and UTC-OFFSET allow overriding
individual components of the decoded TIME."
  (cl-destructuring-bind
      (prev-seconds
       prev-minutes
       prev-hours
       prev-day
       prev-month
       prev-year
       prev-day-of-week
       prev-daylight-savings-time-p
       prev-utc-offset)
      (decode-time time)
    (encode-time (or seconds prev-seconds)
                 (or minutes prev-minutes)
                 (or hours prev-hours)
                 (or day prev-day)
                 (or month prev-month)
                 (or year prev-year)
                 (or day-of-week prev-day-of-week)
                 (or daylight-savings-time-p prev-daylight-savings-time-p)
                 (or utc-offset prev-utc-offset))))

(defun cashpw-time-at-hh-mm (time hh mm)
  "Return a TIME at HH:MM:00."
  (cl-destructuring-bind (_seconds
                          _minutes
                          _hours
                          days
                          months
                          years
                          day-of-week
                          daylight-savings-time-p
                          utc-offset)
      (decode-time time)
    (encode-time 0
                 mm
                 hh
                 days
                 months
                 years
                 day-of-week
                 daylight-savings-time-p
                 utc-offset)))

(defun cashpw-time-today-at-hh-mm (hh mm)
  "Return a time object for the current day at HH:MM."
  (cashpw-time-at-hh-mm (current-time) hh mm))

(defun cashpw-time-tomorrow-at-hh-mm (hh mm)
  "Return a time object for tomorrow at HH:MM."
  (cashpw-time-at-hh-mm (time-add (current-time) (days-to-time 1)) hh mm))

(defun cashpw-time--end-of-day (time)
  "Return TIME with maximum hours, minutes, and seconds."
  (cl-destructuring-bind
      (_seconds
       _minutes
       _hours
       days
       months
       years
       day-of-week
       daylight-savings-time-p
       utc-offset)
      (decode-time time)
    (encode-time
     59
     59
     23
     days
     months
     years
     day-of-week
     daylight-savings-time-p
     utc-offset)))

(defun cashpw-time-future-p (time)
  "Return non-nil if TIME occurs in the future."
  (and
   (not (time-equal-p time (current-time)))
   (not (time-less-p time (current-time)))))

(defun cashpw-time-past-p (time)
  "Return non-nil if TIME occurs in the past."
  (time-less-p time (current-time)))

(defun cashpw-time-tomorrow-p (time)
  "Return non-nil if TIME occurs tomorrow."
  (let ((tomorrow (+ 1 (time-to-days (current-time)))))
    (= (time-to-days time) tomorrow)))

(defun cashpw-time-same-day-p (time-a time-b)
  "Return non-nil if TIME-A and TIME-B are on the same day."
  (when (and time-a time-b)
    (= (time-to-days time-a) (time-to-days time-b))))

(defun cashpw-time-today-p (time)
  "Return non-nil if TIME occurs today."
  (cashpw-time-same-day-p time (current-time)))

(defun cashpw-time-yesterday-p (time)
  "Return non-nil if TIME occurred yesterday."
  (cashpw-time-same-day-p time (time-subtract (current-time) (days-to-time 1))))

(defun cashpw-time--zero-out-hh-mm-ss (time)
  "Return TIME with hours, minutes, and seconds set to 0."
  (cl-destructuring-bind
      (_seconds
       _minutes
       _hours
       days
       months
       years
       day-of-week
       daylight-savings-time-p
       utc-offset)
      (decode-time time)
    (encode-time
     0
     0
     0
     days
     months
     years
     day-of-week
     daylight-savings-time-p
     utc-offset)))

(defun cashpw-time-greater-than-p (a b)
  "Return non-nil if time value A is greater than time value B."
  (and (not (time-equal-p a b))
       (not (time-less-p a b))))
(defun cashpw-time-days-between (time1 time2)
  "Return the number of days between TIME1 and TIME2."
  (- (time-to-days time2) (time-to-days time1)))

(defun cashpw-time--get-week-times (time)
  "Return a list of 7 time values representing each day of the week containing TIME (Monday to Sunday)."
  (let* ((decoded (decode-time time))
         (dow (decoded-time-weekday decoded))
         (days-since-monday (if (= dow 0) 6 (- dow 1)))
         (monday-time (time-subtract time (days-to-time days-since-monday)))
         (monday-decoded (decode-time monday-time))
         (monday-day (decoded-time-day monday-decoded))
         (monday-month (decoded-time-month monday-decoded))
         (monday-year (decoded-time-year monday-decoded)))
    (mapcar (lambda (day-offset)
              (encode-time 0 0 0
                           (+ monday-day day-offset)
                           monday-month
                           monday-year))
            '(0 1 2 3 4 5 6))))

(defun cashpw-time-week-tag (&optional time)
  "Return the ISO-8601 week tag for TIME in YYYYwWW format.
If TIME is nil, use the current time."
  ;; Use %G and %V (ISO-8601 year/week) instead of %Y/%W to prevent boundary
  ;; discrepancies where calendar year and ISO week-numbering year differ.
  (format-time-string "%Gw%V" (or time (current-time))))

(defun cashpw-time-current-week-tag (&optional time)
  "Return the ISO-8601 week tag for TIME (default current time) in YYYYwWW format."
  (cashpw-time-week-tag time))

(defun cashpw-time-next-week-tag (&optional time)
  "Return the ISO-8601 week tag for one week after TIME in YYYYwWW format.
If TIME is nil, use the current time."
  (let* ((decoded (decode-time (or time (current-time))))
         (day (decoded-time-day decoded)))
    (setf (decoded-time-day decoded) (+ day 7))
    (setf (decoded-time-dst decoded) -1)
    (setf (decoded-time-zone decoded) nil)
    (cashpw-time-week-tag (encode-time decoded))))

(defun cashpw-time--iso-weekday (&optional time)
  "Return the ISO weekday for TIME (1-7, Monday=1, Sunday=7).
If TIME is nil, use the current time."
  (let* ((decoded (decode-time (or time (current-time))))
         (dow (decoded-time-weekday decoded)))
    (if (= dow 0) 7 dow)))

(defun cashpw-time-upcoming-weekday (target-weekday &optional time)
  "Return the time value for the upcoming TARGET-WEEKDAY from TIME.
TARGET-WEEKDAY should be an integer representing the day of the week
using ISO convention (1-7, Monday=1, Sunday=7).
If TIME (default current time) is on or before TARGET-WEEKDAY in the
same ISO week, return TARGET-WEEKDAY of that week.
Otherwise, return TARGET-WEEKDAY of the next week."
  (let* ((now (or time (current-time)))
         (today-iso-dow (cashpw-time--iso-weekday now)))
    (if (<= today-iso-dow target-weekday)
        ;; This week's target weekday
        (nth (- target-weekday 1) (cashpw-time--get-week-times now))
      ;; Next week's target weekday
      (nth (- target-weekday 1) (cashpw-time--get-week-times (time-add now (days-to-time 7)))))))

(defun cashpw-time-upcoming-monday (&optional time)
  "Return the upcoming Monday from TIME."
  (cashpw-time-upcoming-weekday 1 time))

(defun cashpw-time-upcoming-tuesday (&optional time)
  "Return the upcoming Tuesday from TIME."
  (cashpw-time-upcoming-weekday 2 time))

(defun cashpw-time-upcoming-wednesday (&optional time)
  "Return the upcoming Wednesday from TIME."
  (cashpw-time-upcoming-weekday 3 time))

(defun cashpw-time-upcoming-thursday (&optional time)
  "Return the upcoming Thursday from TIME."
  (cashpw-time-upcoming-weekday 4 time))

(defun cashpw-time-upcoming-friday (&optional time)
  "Return the upcoming Friday from TIME."
  (cashpw-time-upcoming-weekday 5 time))

(defun cashpw-time-upcoming-saturday (&optional time)
  "Return the upcoming Saturday from TIME."
  (cashpw-time-upcoming-weekday 6 time))

(defun cashpw-time-upcoming-sunday (&optional time)
  "Return the upcoming Sunday from TIME."
  (cashpw-time-upcoming-weekday 7 time))

(defun cashpw-time-upcoming-weekday-string (target-weekday &optional time)
  "Return the upcoming TARGET-WEEKDAY date as YYYY-MM-DD Day."
  (format-time-string "%Y-%m-%d %a" (cashpw-time-upcoming-weekday target-weekday time)))

(defun cashpw-time-upcoming-monday-string (&optional time)
  "Return the upcoming Monday date as YYYY-MM-DD Day."
  (cashpw-time-upcoming-weekday-string 1 time))

(defun cashpw-time-upcoming-tuesday-string (&optional time)
  "Return the upcoming Tuesday date as YYYY-MM-DD Day."
  (cashpw-time-upcoming-weekday-string 2 time))

(defun cashpw-time-upcoming-wednesday-string (&optional time)
  "Return the upcoming Wednesday date as YYYY-MM-DD Day."
  (cashpw-time-upcoming-weekday-string 3 time))

(defun cashpw-time-upcoming-thursday-string (&optional time)
  "Return the upcoming Thursday date as YYYY-MM-DD Day."
  (cashpw-time-upcoming-weekday-string 4 time))

(defun cashpw-time-upcoming-friday-string (&optional time)
  "Return the upcoming Friday date as YYYY-MM-DD Day."
  (cashpw-time-upcoming-weekday-string 5 time))

(defun cashpw-time-upcoming-saturday-string (&optional time)
  "Return the upcoming Saturday date as YYYY-MM-DD Day."
  (cashpw-time-upcoming-weekday-string 6 time))

(defun cashpw-time-upcoming-sunday-string (&optional time)
  "Return the upcoming Sunday date as YYYY-MM-DD Day."
  (cashpw-time-upcoming-weekday-string 7 time))

(defun cashpw-time-iso-week-to-time (year week)
  "Return time value for the Monday of ISO WEEK in YEAR."
  (let* ((jan-4 (encode-time 0 0 0 4 1 year))
         (jan-4-decoded (decode-time jan-4))
         (jan-4-dow (decoded-time-weekday jan-4-decoded))
         (offset (if (= jan-4-dow 0) -6 (- 1 jan-4-dow)))
         (w1-mon-day (+ 4 offset))
         (target-day (+ w1-mon-day (* 7 (1- week)))))
    (encode-time 0 0 0 target-day 1 year)))

(provide 'cashpw-time)
;;; cashpw-time.el ends here

