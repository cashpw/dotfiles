;;; cashpw-weather.el --- Weather utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 cashweaver

;; Author: cashweaver
;; Keywords: calendar, weather, utilities
;; Package-Requires: ((emacs "29.1") (dash "2.19.0") (request "0.3.11") (cashpw-time "0.1.0"))
;; Version: 0.1.0

;;; Commentary:

;; Provides weather forecast integration (Google Weather) and utilities to
;; calculate temperature threshold crossings for home automation (e.g., opening/closing windows).

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'request)
(require 'cashpw-time)

(defgroup cashpw-weather nil
  "Weather utilities for cashpw."
  :group 'calendar)

(defcustom cashpw-weather-google-api-key nil
  "API key for Google Weather."
  :type '(choice (const nil) string)
  :group 'cashpw-weather)

(defcustom cashpw-weather-latitude "37.283"
  "Latitude for weather forecasts."
  :type 'string
  :group 'cashpw-weather)

(defcustom cashpw-weather-longitude "-121.86"
  "Longitude for weather forecasts."
  :type 'string
  :group 'cashpw-weather)

(defcustom cashpw-weather-todos-file nil
  "Path to the personal todos file.
If nil, defaults to `cashpw/path--personal-todos` if bound."
  :type '(choice (const nil) file)
  :group 'cashpw-weather)

(defcustom cashpw-weather-debug nil
  "Non-nil means enable debug logging for cashpw-weather."
  :type 'boolean
  :group 'cashpw-weather)

(defvar cashpw-weather-todo-timer nil
  "Timer for scheduled weather TODO updates.")

;;; Helper Functions

(defun cashpw-weather--log-debug (format-string &rest args)
  "Log formatted, with ARGS, FORMAT-STRING if debug is enabled."
  (cond
   ((fboundp 'cashpw/log-debug)
    (apply #'cashpw/log-debug format-string args))
   (cashpw-weather-debug
    (apply #'message (concat "cashpw-weather: " format-string) args))))

(defun cashpw-weather--todos-file ()
  "Return the path to the todos file."
  (or cashpw-weather-todos-file
      (bound-and-true-p cashpw/path--personal-todos)
      (error "cashpw-weather-todos-file is not set")))

(defun cashpw-weather--org-insert-todo (&rest args)
  "Wrapper for `cashpw/org-insert-todo`."
  (if (fboundp 'cashpw/org-insert-todo)
      (apply #'cashpw/org-insert-todo args)
    (error "cashpw/org-insert-todo is not defined")))

;;; Forecast API

(defun cashpw-weather-google-get-hourly-forecast (success-fn)
  "Get weather forecast, then call SUCCESS-FN."
  (unless cashpw-weather-google-api-key
    (error "cashpw-weather-google-api-key is not set"))
  (request
    "https://weather.googleapis.com/v1/forecast/hours:lookup"
    :parser 'json-read
    :success success-fn
    :params
    `(("key" . ,cashpw-weather-google-api-key)
      ("hours" . "48")
      ("unitsSystem" . "IMPERIAL")
      ("location.latitude" . ,cashpw-weather-latitude)
      ("location.longitude" . ,cashpw-weather-longitude))))

(defun cashpw-weather-google-get-hourly-forecast-temps (data)
  "Return list of \"(<time> . <temperature>)\" for hourly forecast DATA."
  (--map
   (let ((start-time (encode-time
                      (parse-time-string (alist-get 'startTime (alist-get 'interval it)))))
         (temperature (alist-get 'degrees (alist-get 'temperature it))))
     (cons start-time temperature))
   (alist-get 'forecastHours data)))

(defun cashpw-weather-openweather-get-hourly-forecast-temps (data)
  "Return list of \"(<time> . <temperature>)\" for openweather hourly forecast DATA."
  (--map
   (cons
    (seconds-to-time (alist-get 'dt it))
    ;; High and low are equal for hourly forecasts
    (alist-get 'temp_max (alist-get 'main it)))
   (alist-get 'list data)))

;;; Calculation Functions

(defun cashpw-weather--get-points-before-and-after-crossing-threshold
    (time-temps temp-threshold &optional into-threshold)
  "Return TIME-TEMPS before and after crossing INTO-THRESHOLD TEMP-THRESHOLD.

TIME-TEMPS is a list of \"(<time> . <temperature>)\"."
  (let ((i 1)
        before-crossing-threshold
        after-crossing-threshold)
    (when (> (length time-temps) 1)
      (while (and (< i (length time-temps))
                  (not before-crossing-threshold)
                  (not after-crossing-threshold))
        (let ((previous-time-temp (nth (1- i) time-temps))
              (time-temp (nth i time-temps)))
          (when (and (not before-crossing-threshold)
                     (not after-crossing-threshold)
                     (if into-threshold
                         (<= (cdr previous-time-temp) temp-threshold)
                       (>= (cdr previous-time-temp) temp-threshold))
                     (if into-threshold
                         (>= (cdr time-temp) temp-threshold)
                       (<= (cdr time-temp) temp-threshold)))
            (setq
             before-crossing-threshold previous-time-temp
             after-crossing-threshold time-temp)))
        (cl-incf i)))
    (cons before-crossing-threshold after-crossing-threshold)))

(defun cashpw-weather-get-points-before-and-after-crossing-into-threshold
    (time-temps temp-threshold)
  "Return TIME-TEMPS before and after crossing into TEMP-THRESHOLD."
  (cashpw-weather--get-points-before-and-after-crossing-threshold
   time-temps temp-threshold
   t))

(defun cashpw-weather-get-points-before-and-after-crossing-out-of-threshold
    (time-temps temp-threshold)
  "Return TIME-TEMPS before and after crossing out of TEMP-THRESHOLD."
  (cashpw-weather--get-points-before-and-after-crossing-threshold
   time-temps temp-threshold
   nil))

(defun cashpw-weather-interpolate-time-for-temperature
    (time-a temp-a time-b temp-b target-temp)
  "Calculate the time for a TARGET-TEMP via linear interpolation between TIME-A/TEMP-A and TIME-B/TEMP-B.

This function assumes a linear relationship between time and
temperature.  TIME-A and TIME-B are time values; TEMP-A, TEMP-B,
and TARGET-TEMP are numbers.

It returns the calculated time as a time value.  If TEMP-A and
TEMP-B are identical but do not match TARGET-TEMP, it returns nil."
  (cashpw-weather--log-debug "interpolate: %s, %s; %s, %s; %s"
                      (format-time-string "%F %T%Z" time-a)
                      temp-a
                      (format-time-string "%F %T%Z" time-b)
                      temp-b
                      target-temp)
  (cond
   ((= temp-a target-temp)
    time-a)
   ((= temp-b target-temp)
    time-b)
   (t
    (let ((temp-range (- temp-b temp-a)))
      (if (zerop temp-range)
          ;; If temperature does not change, result is valid only if
          ;; target-temp matches the constant temperature.
          (when (= target-temp temp-a)
            time-a)
        ;; Standard case: perform linear interpolation.
        (let* ((time-range (time-subtract time-b time-a))
               (target-offset (- target-temp temp-a))
               (proportion (/ (float target-offset) temp-range)))
          (time-add
           time-a
           (seconds-to-time (* proportion (time-to-seconds time-range))))))))))

(defun cashpw-weather-cross-into-threshold-time
    (temperature-threshold time-temps)
  "Return the approximate time at which TIME-TEMPS crosses into TEMPERATURE-THRESHOLD."
  (when-let*
      ((before-after
        (cashpw-weather-get-points-before-and-after-crossing-into-threshold
         time-temps
         temperature-threshold))
       (before (car before-after))
       (after (cdr before-after)))
    (cashpw-weather--log-debug
     "(cross into) Tomorrow hourly forecast temperatures: %s"
     (--map
      (format "\n%s: %s"
              (format-time-string "%F %T%Z" (car it))
              (cdr it))
      time-temps))
    (cashpw-weather--log-debug
     "(cross into) before: %s, %s"
     (format-time-string "%F %T%Z" (car before))
     (cdr before))
    (cashpw-weather--log-debug
     "(cross into) after: %s, %s"
     (format-time-string "%F %T%Z" (car after))
     (cdr after))
    (cashpw-weather-interpolate-time-for-temperature
     (car before) (cdr before) (car after) (cdr after) temperature-threshold)))

(defun cashpw-weather-cross-out-of-threshold-time
    (temperature-threshold time-temps)
  "Return the approximate time at which TIME-TEMP crosses out of TEMPERATURE-THRESHOLD."
  (when-let*
      ((before-after
        (cashpw-weather-get-points-before-and-after-crossing-out-of-threshold
         time-temps
         temperature-threshold))
       (before (car before-after))
       (after (cdr before-after)))
    (cashpw-weather--log-debug
     "(cross into) Tomorrow hourly forecast temperatures: %s"
     (--map
      (format "\n%s: %s"
              (format-time-string "%F %T%Z" (car it))
              (cdr it))
      time-temps))
    (cashpw-weather--log-debug
     "(cross into) before: %s, %s"
     (format-time-string "%F %T%Z" (car before))
     (cdr before))
    (cashpw-weather--log-debug
     "(cross into) after: %s, %s"
     (format-time-string "%F %T%Z" (car after))
     (cdr after))
    (cashpw-weather-interpolate-time-for-temperature
     (car before) (cdr before) (car after) (cdr after) temperature-threshold)))

;;; Action Functions

(defun cashpw-weather-insert-close-windows-todo (time)
  "Insert todo to close the windows at TIME."
  (let ((todos-file (cashpw-weather--todos-file)))
    (with-current-buffer (find-file-noselect todos-file)
      (save-excursion
        (cashpw-weather--org-insert-todo
         "Close the windows"
         :point-or-marker (point-max)
         :priority 1
         :category "Home"
         :effort "5m"
         :start-time time
         :include-hh-mm t)))))

(defun cashpw-weather-insert-open-windows-todo (time)
  "Insert todo to open the windows at TIME."
  (let ((todos-file (cashpw-weather--todos-file)))
    (with-current-buffer (find-file-noselect todos-file)
      (save-excursion
        (cashpw-weather--org-insert-todo
         "Open the windows"
         :point-or-marker (point-max)
         :priority 1
         :category "Home"
         :effort "5m"
         :start-time time
         :include-hh-mm t)))))

(defun cashpw-weather-insert-todays-open-close-window-todos
    (open-window-threshold-temp close-window-threshold-temp)
  "Insert open/close window todos for today based on OPEN-WINDOW-THRESHOLD-TEMP and CLOSE-WINDOW-THRESHOLD-TEMP."
  (cashpw-weather-google-get-hourly-forecast
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let* ((time-temps
              (--filter
               (cashpw-time-today-p (car it))
               (cashpw-weather-google-get-hourly-forecast-temps data)))
             (close-window-time
              (cashpw-weather-cross-into-threshold-time
               close-window-threshold-temp time-temps))
             (open-window-time
              (cashpw-weather-cross-out-of-threshold-time
               open-window-threshold-temp time-temps)))
        (when open-window-time
          (cashpw-weather-insert-open-windows-todo open-window-time))
        (when close-window-time
          (cashpw-weather-insert-close-windows-todo close-window-time)))))))

(defun cashpw-weather-schedule-insert-todos (time)
  "Schedule `cashpw-weather-insert-*-windows-todo' at TIME."
  (when cashpw-weather-todo-timer
    (cancel-timer cashpw-weather-todo-timer))
  (setq cashpw-weather-todo-timer
        (run-at-time
         time nil
         (lambda ()
           (cashpw-weather-insert-todays-open-close-window-todos 74 74)
           (cashpw-weather-schedule-insert-todos
            (cashpw-time-tomorrow-at-hh-mm 5 00))))))

(provide 'cashpw-weather)
;;; cashpw-weather.el ends here
