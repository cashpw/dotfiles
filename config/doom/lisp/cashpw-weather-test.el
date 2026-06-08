;;; cashpw-weather-test.el --- Tests for cashpw-weather.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'cashpw-weather)
(require 'cashpw-time)

;;; Helper Macros

(defmacro cashpw-weather-test-with-mock-time (time-string &rest body)
  "Execute BODY with `current-time' mocked to return the time parsed from TIME-STRING."
  (declare (indent 1))
  `(let ((mock-time (date-to-time ,time-string)))
     (cl-letf (((symbol-function 'current-time) (lambda () mock-time)))
       ,@body)))

;;; Test Cases

(ert-deftest cashpw-weather-test-interpolate-time ()
  "Test linear interpolation of time for a target temperature."
  (let ((time-a (date-to-time "2026-05-25 12:00:00"))
        (time-b (date-to-time "2026-05-25 13:00:00")))
    ;; Target is exactly temp-a
    (should (time-equal-p
             (cashpw-weather-interpolate-time-for-temperature time-a 70.0 time-b 80.0 70.0)
             time-a))
    ;; Target is exactly temp-b
    (should (time-equal-p
             (cashpw-weather-interpolate-time-for-temperature time-a 70.0 time-b 80.0 80.0)
             time-b))
    ;; Target is halfway between
    (should (time-equal-p
             (cashpw-weather-interpolate-time-for-temperature time-a 70.0 time-b 80.0 75.0)
             (date-to-time "2026-05-25 12:30:00")))
    ;; Target is 1/4 way
    (should (time-equal-p
             (cashpw-weather-interpolate-time-for-temperature time-a 70.0 time-b 80.0 72.5)
             (date-to-time "2026-05-25 12:15:00")))
    ;; Temperature doesn't change, target matches constant temp
    (should (time-equal-p
             (cashpw-weather-interpolate-time-for-temperature time-a 70.0 time-b 70.0 70.0)
             time-a))
    ;; Temperature doesn't change, target does NOT match constant temp
    (should-not (cashpw-weather-interpolate-time-for-temperature time-a 70.0 time-b 70.0 75.0))))

(ert-deftest cashpw-weather-test-get-points-crossing-threshold ()
  "Test finding points before and after crossing threshold."
  (let ((time-temps
         (list
          (cons (date-to-time "2026-05-25 12:00:00") 68.0)
          (cons (date-to-time "2026-05-25 13:00:00") 69.5)
          (cons (date-to-time "2026-05-25 14:00:00") 71.0) ; Crosses 70 here (into)
          (cons (date-to-time "2026-05-25 15:00:00") 72.0)
          (cons (date-to-time "2026-05-25 16:00:00") 69.0) ; Crosses 70 here (out of)
          (cons (date-to-time "2026-05-25 17:00:00") 68.0))))

    ;; Cross into 70
    (let ((result (cashpw-weather-get-points-before-and-after-crossing-into-threshold time-temps 70.0)))
      (should (equal (car result) (nth 1 time-temps))) ; 69.5
      (should (equal (cdr result) (nth 2 time-temps)))) ; 71.0

    ;; Cross out of 70
    (let ((result (cashpw-weather-get-points-before-and-after-crossing-out-of-threshold time-temps 70.0)))
      (should (equal (car result) (nth 3 time-temps))) ; 72.0
      (should (equal (cdr result) (nth 4 time-temps)))) ; 69.0

    ;; No crossing (threshold too high)
    (let ((result-into (cashpw-weather-get-points-before-and-after-crossing-into-threshold time-temps 80.0))
          (result-out (cashpw-weather-get-points-before-and-after-crossing-out-of-threshold time-temps 80.0)))
      (should-not (car result-into))
      (should-not (cdr result-into))
      (should-not (car result-out))
      (should-not (cdr result-out)))))

(ert-deftest cashpw-weather-test-cross-threshold-times ()
  "Test calculation of crossing times."
  (let ((time-temps
         (list
          (cons (date-to-time "2026-05-25 12:00:00") 68.0)
          (cons (date-to-time "2026-05-25 13:00:00") 69.0)
          (cons (date-to-time "2026-05-25 14:00:00") 72.0) ; Crosses 70 between 13:00 and 14:00
          (cons (date-to-time "2026-05-25 15:00:00") 74.0)
          (cons (date-to-time "2026-05-25 16:00:00") 68.0) ; Crosses 70 between 15:00 and 16:00
          (cons (date-to-time "2026-05-25 17:00:00") 66.0))))

    ;; Cross into 70.0
    ;; 13:00 is 69.0, 14:00 is 72.0. Range is 3.0 degrees.
    ;; Target 70.0 is 1.0 degree above 69.0.
    ;; Time should be 13:00 + 1/3 of an hour (20 mins) -> 13:20:00
    (should (time-equal-p
             (cashpw-weather-cross-into-threshold-time 70.0 time-temps)
             (date-to-time "2026-05-25 13:20:00")))

    ;; Cross out of 70.0
    ;; 15:00 is 74.0, 16:00 is 68.0. Range is -6.0 degrees.
    ;; Target 70.0 is 4.0 degrees below 74.0.
    ;; Time should be 15:00 + 4/6 of an hour (40 mins) -> 15:40:00
    (should (time-equal-p
             (cashpw-weather-cross-out-of-threshold-time 70.0 time-temps)
             (date-to-time "2026-05-25 15:40:00")))))

(ert-deftest cashpw-weather-test-insert-todos ()
  "Test that TODOs are inserted correctly into the mocked todos file."
  (let ((temp-file (make-temp-file "cashpw-weather-test-todos")))
    (unwind-protect
        (let ((cashpw-weather-todos-file temp-file))
          ;; Mock org-insert-todo to just write to buffer or verify it's called.
          ;; Actually, we want to verify it calls cashpw/org-insert-todo with correct args.
          ;; Since we don't have real org-mode active or cashpw/org-insert-todo defined in test environment
          ;; (unless we load it), we can mock `cashpw-weather--org-insert-todo`.
          (cl-letf (((symbol-function 'cashpw-weather--org-insert-todo)
                     (cl-function
                      (lambda (text &key priority effort category start-time &allow-other-keys)
                        (insert (format "TODO: %s, Priority: %s, Effort: %s, Category: %s, Start: %s\n"
                                        text priority effort category
                                        (format-time-string "%Y-%m-%d %H:%M:%S" start-time)))))))
            
            (cashpw-weather-insert-close-windows-todo (date-to-time "2026-05-25 15:40:00"))
            (cashpw-weather-insert-open-windows-todo (date-to-time "2026-05-25 13:20:00"))

            (with-current-buffer (find-file-noselect temp-file)
              (goto-char (point-min))
              (should (search-forward "TODO: Close the windows, Priority: 1, Effort: 5m, Category: Home, Start: 2026-05-25 15:40:00" nil t))
              (goto-char (point-min))
              (should (search-forward "TODO: Open the windows, Priority: 1, Effort: 5m, Category: Home, Start: 2026-05-25 13:20:00" nil t)))))
      (delete-file temp-file))))

(ert-deftest cashpw-weather-test-insert-todays-todos-workflow ()
  "Test the full workflow of fetching forecast and inserting TODOs."
  (cashpw-weather-test-with-mock-time "2026-05-25 05:00:00"
    (let ((temp-file (make-temp-file "cashpw-weather-test-todos"))
          ;; Mock Google Weather API response data
          (mock-data
           '((forecastHours .
                            [((interval . ((startTime . "2026-05-25T06:00:00Z"))) (temperature . ((degrees . 68.0))))
                             ((interval . ((startTime . "2026-05-25T07:00:00Z"))) (temperature . ((degrees . 69.0))))
                             ((interval . ((startTime . "2026-05-25T08:00:00Z"))) (temperature . ((degrees . 72.0)))) ; Cross 70 (into) between 7 and 8 (07:20)
                             ((interval . ((startTime . "2026-05-25T09:00:00Z"))) (temperature . ((degrees . 75.0))))
                             ((interval . ((startTime . "2026-05-25T10:00:00Z"))) (temperature . ((degrees . 73.0))))
                             ((interval . ((startTime . "2026-05-25T11:00:00Z"))) (temperature . ((degrees . 68.0)))) ; Cross 70 (out) between 10 and 11 (10:36)
                             ((interval . ((startTime . "2026-05-25T12:00:00Z"))) (temperature . ((degrees . 66.0))))]))))
      (unwind-protect
          (let ((cashpw-weather-todos-file temp-file)
                (cashpw-weather-google-api-key "mock-key")
                (inserted-todos nil))
            
            ;; Mock the network request function to immediately succeed with mock data
            (cl-letf (((symbol-function 'cashpw-weather-google-get-hourly-forecast)
                       (lambda (success-fn)
                         (funcall success-fn :data mock-data)))
                      ;; Mock org-insert-todo to capture arguments
                      ((symbol-function 'cashpw-weather-insert-open-windows-todo)
                       (lambda (time)
                         (push (cons 'open time) inserted-todos)))
                      ((symbol-function 'cashpw-weather-insert-close-windows-todo)
                       (lambda (time)
                         (push (cons 'close time) inserted-todos))))

              (cashpw-weather-insert-todays-open-close-window-todos 70.0 70.0)

              (should (= (length inserted-todos) 2))
              ;; Check open window time (cross out of 70.0)
              ;; 10:00 is 73.0, 11:00 is 68.0. Range 5.0. Target 70.0 is 3.0 below 73.0.
              ;; 10:00 + 3/5 hour (36 mins) -> 10:36:00
              (let ((open-todo (assoc 'open inserted-todos)))
                (should open-todo)
                ;; We need to adjust for timezone if parse-time-string assumes UTC or local.
                ;; parse-time-string "2026-05-25T10:00:00Z" has 'Z' (UTC).
                ;; `encode-time` in `cashpw-weather-google-get-hourly-forecast-temps` uses default timezone (likely local).
                ;; For test simplicity, let's assume the parsed time is converted consistently.
                ;; Actually, `parse-time-string` returns a list where the last element is timezone offset.
                ;; `encode-time` handles it.
                ;; Let's verify the relative difference instead of absolute if timezone is tricky,
                ;; or just ensure it matches the expected interpolated time.
                ;; The mock data times are UTC.
                ;; "2026-05-25T10:00:00Z" -> 10:00 UTC
                ;; "2026-05-25T11:00:00Z" -> 11:00 UTC
                ;; Interpolated time should be 10:36 UTC.
                (should (time-equal-p
                         (cdr open-todo)
                         (encode-time (parse-time-string "2026-05-25T10:36:00Z")))))

              ;; Check close window time (cross into 70.0)
              ;; 07:00 is 69.0, 08:00 is 72.0. Range 3.0. Target 70.0 is 1.0 above 69.0.
              ;; 07:00 + 1/3 hour (20 mins) -> 07:20:00 UTC
              (let ((close-todo (assoc 'close inserted-todos)))
                (should close-todo)
                (should (time-equal-p
                         (cdr close-todo)
                         (encode-time (parse-time-string "2026-05-25T07:20:00Z")))))))
        (delete-file temp-file)))))

(provide 'cashpw-weather-test)
;;; cashpw-weather-test.el ends here
