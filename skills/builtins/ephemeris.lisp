;;;; skills/builtins/ephemeris.lisp
;;;;
;;;; Built-in skill: solar ephemeris calculations.
;;;; Computes sunrise, sunset, solar noon, day length, and twilight times
;;;; for any location and date.
;;;;
;;;; Implements the USNO algorithm (Almanac for Computers, 1990)
;;;; from edwilliams.org.
;;;; Accuracies within ~1 minute for sunrise/sunset.
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; --- Utilities ---

(defun deg->rad (degrees)
  "Convert degrees to radians."
  (* degrees (/ pi 180.0d0)))

(defun rad->deg (radians)
  "Convert radians to degrees."
  (* radians (/ 180.0d0 pi)))

(defun sin-deg (degrees)
  "Sine of degrees."
  (sin (deg->rad degrees)))

(defun cos-deg (degrees)
  "Cosine of degrees."
  (cos (deg->rad degrees)))

(defun asin-deg (x)
  "Arcsine, returning degrees."
  (rad->deg (asin x)))

(defun acos-deg (x)
  "Arccosine, returning degrees."
  (rad->deg (acos x)))

(defun atan-deg (y x)
  "Arctangent (atan2), returning degrees."
  (rad->deg (atan y x)))

(defun normalize-degrees (deg)
  "Normalize degrees to range [0, 360)."
  (let ((d (mod deg 360.0d0)))
    (if (< d 0) (+ d 360.0d0) d)))

;;; --- Day of year calculations ---

(defun day-of-year (month day)
  "Compute day of year (1-366) from month and day."
  (let ((days-in-months #(0 31 59 90 120 151 181 212 243 273 304 334)))
    (+ (aref days-in-months (- month 1)) day)))

;;; --- Lunar phase calculations ---

(defun lunar-phase-for-date (year month day)
  "Compute lunar phase (0.0-1.0) and phase name for a given date.
   Returns (values phase-fraction phase-name illumination-percent).
   
   Uses the known new moon of January 6, 2000 (JD 2451550.1)
   and the lunar synodic month of 29.530588861 days.
   
   Phase values:
   - 0.0-0.125: New Moon / Waxing Crescent
   - 0.125-0.25: Waxing Crescent
   - 0.25-0.375: First Quarter / Waxing Gibbous
   - 0.375-0.5: Waxing Gibbous
   - 0.5-0.625: Full Moon / Waning Gibbous
   - 0.625-0.75: Waning Gibbous
   - 0.75-0.875: Last Quarter / Waning Crescent
   - 0.875-1.0: Waning Crescent"
  
  ;; Reference new moon: January 6, 2000, 18:14:00 UTC (JD 2451550.261)
  ;; Lunar synodic month: 29.530588861 days
  (let* ((reference-jd 2451550.261d0)
         (synodic-month 29.530588861d0)
         
         ;; Calculate Julian Day Number for the given date
         ;; Simplified formula (accurate for years 1900-2100)
         (a (floor (/ (- 14 month) 12)))
         (y (+ year 4800 (- a)))
         (m (+ month (* 12 a) -3))
         (jdn (+ day
                (floor (/ (+ (* 153 m) 2) 5))
                (* 365 y)
                (floor (/ y 4))
                (- (floor (/ y 100)))
                (floor (/ y 400))
                -32045))
         (jd (coerce jdn 'double-float))
         
         ;; Days since reference new moon
         (days-since-ref (- jd reference-jd))
         
         ;; Lunar phase: fractional position in synodic month [0, 1)
         (phase (mod (/ days-since-ref synodic-month) 1.0d0))
         
         ;; Illumination: ranges from 0 (new) to 1 (full) and back
         ;; Uses cosine to smooth the brightness curve
         (illumination (* 50.0d0 (- 1.0d0 (cos (* 2.0d0 pi phase)))))
         
         ;; Determine phase name
         (phase-name
          (cond
            ((< phase 0.0625d0) "New Moon")
            ((< phase 0.1875d0) "Waxing Crescent")
            ((< phase 0.3125d0) "First Quarter")
            ((< phase 0.4375d0) "Waxing Gibbous")
            ((< phase 0.5625d0) "Full Moon")
            ((< phase 0.6875d0) "Waning Gibbous")
            ((< phase 0.8125d0) "Last Quarter")
            (t "Waning Crescent"))))
    
    (values phase phase-name (round illumination))))

(defun lunar-phase-description (phase-name illumination)
  "Return a human-readable description of the lunar phase.
   E.g., 'Waxing Crescent (42% illuminated)'."
  (format nil "~A (~D% illuminated)" phase-name illumination))

;;; --- Sunrise/Sunset using USNO Algorithm ---

(defun sun-mean-anomaly (approx-time)
  "USNO Step 2: Sun's mean anomaly from approximate time."
  (* (- (* 0.9856d0 approx-time) 3.289d0)))

(defun sun-true-longitude (mean-anomaly)
  "USNO Step 3: Sun's true longitude from mean anomaly."
  (normalize-degrees (+ mean-anomaly
                        (* 1.916d0 (sin-deg mean-anomaly))
                        (* 0.020d0 (sin-deg (* 2.0d0 mean-anomaly)))
                        282.634d0)))

(defun sun-right-ascension-hours (true-longitude)
  "USNO Step 4: Sun's right ascension in hours from true longitude.
   Adjusts RA to the same quadrant as the true longitude."
  (let* ((ra-raw (atan-deg (* 0.91764d0 (tan (deg->rad true-longitude))) 1.0d0))
         (l-quad (floor (/ true-longitude 90.0d0)))
         (ra-quad (floor (/ ra-raw 90.0d0))))
    (/ (+ ra-raw (- (* 90.0d0 l-quad) (* 90.0d0 ra-quad)))
       15.0d0)))

(defun sun-declination (true-longitude)
  "USNO Step 5: Sun's declination as (values sin-dec cos-dec)."
  (let ((sin-dec (* 0.39782d0 (sin-deg true-longitude))))
    (values sin-dec (cos (asin sin-dec)))))

(defun sun-hour-angle-cos (sin-dec cos-dec latitude &optional (zenith 90.833d0))
  "USNO Step 6: Cosine of the sun's local hour angle.
   ZENITH defaults to 90°50' for standard sunrise/sunset."
  (/ (- (cos-deg zenith) (* sin-dec (sin-deg latitude)))
     (* cos-dec (cos-deg latitude))))

(defun sunrise-sunset-for-date (year month day latitude longitude)
  "Compute sunrise and sunset times (in hours UTC) for a given date and location.
   Based on USNO algorithm (Almanac for Computers, 1990).

   Returns (values sunrise-hours sunset-hours day-length-hours solar-noon-hours).
   For polar 24-hour day: (values 0.0 24.0 24.0 12.0).
   For polar 24-hour night: (values NIL NIL 0.0 12.0)."
  (declare (ignore year))
  (let* ((n (day-of-year month day))
         (lng-hour (/ longitude 15.0d0))
         ;; Step 1: Approximate times for rise, set, and noon
         (t-rise (+ n (/ (- 6.0d0 lng-hour) 24.0d0)))
         (t-set  (+ n (/ (- 18.0d0 lng-hour) 24.0d0)))
         (t-noon (+ n (/ (- 12.0d0 lng-hour) 24.0d0)))
         ;; Steps 2-4: Mean anomaly → true longitude → RA hours
         (m-rise (sun-mean-anomaly t-rise))
         (m-set  (sun-mean-anomaly t-set))
         (m-noon (sun-mean-anomaly t-noon))
         (l-rise (sun-true-longitude m-rise))
         (l-set  (sun-true-longitude m-set))
         (l-noon (sun-true-longitude m-noon))
         (ra-hours-rise (sun-right-ascension-hours l-rise))
         (ra-hours-set  (sun-right-ascension-hours l-set))
         (ra-hours-noon (sun-right-ascension-hours l-noon)))
    ;; Step 5-6: Declination and hour angle
    (multiple-value-bind (sin-dec-rise cos-dec-rise) (sun-declination l-rise)
      (multiple-value-bind (sin-dec-set cos-dec-set) (sun-declination l-set)
        (let ((cos-h-rise (sun-hour-angle-cos sin-dec-rise cos-dec-rise latitude))
              (cos-h-set  (sun-hour-angle-cos sin-dec-set cos-dec-set latitude)))
          (if (or (> (abs cos-h-rise) 1.0d0) (> (abs cos-h-set) 1.0d0))
              ;; Polar region: 24-hour day or night
              (if (>= sin-dec-rise 0.0d0)
                  (values 0.0d0 24.0d0 24.0d0 12.0d0)
                  (values nil nil 0.0d0 12.0d0))
              ;; Steps 7-9: Hour angle → local mean time → UTC
              (let* ((h-rise (/ (normalize-degrees (- 360.0d0 (acos-deg cos-h-rise))) 15.0d0))
                     (h-set  (/ (acos-deg cos-h-set) 15.0d0))
                     (sunrise-utc (mod (- (+ h-rise ra-hours-rise (* -0.06571d0 t-rise) -6.622d0)
                                          lng-hour)
                                       24.0d0))
                     (sunset-utc  (mod (- (+ h-set ra-hours-set (* -0.06571d0 t-set) -6.622d0)
                                          lng-hour)
                                       24.0d0))
                     (noon-utc    (mod (- (+ ra-hours-noon (* -0.06571d0 t-noon) -6.622d0)
                                          lng-hour)
                                       24.0d0))
                     (day-length  (- (if (< sunset-utc sunrise-utc)
                                         (+ sunset-utc 24.0d0)
                                         sunset-utc)
                                     sunrise-utc)))
                (values sunrise-utc sunset-utc day-length noon-utc))))))))

(defun format-time-hms (hours)
  "Format decimal hours as HH:MM:SS string."
  (when hours
    (let* ((h (floor hours))
           (remainder (* (- hours h) 60.0d0))
           (m (floor remainder))
           (s (floor (* (- remainder m) 60.0d0))))
      (format nil "~2,'0D:~2,'0D:~2,'0D" h m s))))

;;; --- Public API ---

(defun ephemeris-plist (year month day latitude longitude)
  "Compute ephemeris data for a given date and location.
    Returns a plist with :sunrise :sunset :solar-noon :day-length,
    and lunar data: :lunar-phase :lunar-phase-name :lunar-illumination."
  (multiple-value-bind (sunrise sunset day-length solar-noon)
      (sunrise-sunset-for-date year month day latitude longitude)
    (multiple-value-bind (phase phase-name illumination)
        (lunar-phase-for-date year month day)
      (declare (ignore phase))
      (list :sunrise (format-time-hms sunrise)
            :sunset (format-time-hms sunset)
            :solar-noon (format-time-hms solar-noon)
            :day-length (when day-length
                         (format nil "~D:~2,'0D:~2,'0D"
                                 (floor day-length)
                                 (floor (* (mod day-length 1.0d0) 60.0d0))
                                 (floor (* (mod (* (mod day-length 1.0d0) 60.0d0) 1.0d0)
                                          60.0d0))))
            :lunar-phase-name phase-name
            :lunar-illumination illumination))))

(defun current-ephemeris (latitude longitude)
  "Get ephemeris data for today at given location.
    Returns a plist with :sunrise :sunset :solar-noon :day-length,
    and lunar data: :lunar-phase-name :lunar-illumination."
  (multiple-value-bind (s mi h d mo y)
      (decode-universal-time (get-universal-time))
    (declare (ignore s mi h))
    (ephemeris-plist y mo d latitude longitude)))

(defun ephemeris-report (latitude longitude &key (stream t))
  "Print ephemeris data for today in human-friendly format."
  (let ((today (multiple-value-list (decode-universal-time (get-universal-time)))))
    (destructuring-bind (s mi h d mo y &rest rest) today
      (declare (ignore s mi h rest))
      (let ((eph (ephemeris-plist y mo d latitude longitude)))
        (format stream "~&Ephemeris data for ~A°, ~A°~%" latitude longitude)
        (format stream "~&Solar Data:~%")
        (format stream "  Sunrise:    ~A UTC~%" (getf eph :sunrise))
        (format stream "  Sunset:     ~A UTC~%" (getf eph :sunset))
        (format stream "  Solar Noon: ~A UTC~%" (getf eph :solar-noon))
        (format stream "  Day Length: ~A~%" (getf eph :day-length))
        (format stream "~&Lunar Data:~%")
        (format stream "  Phase:        ~A~%" (getf eph :lunar-phase-name))
        (format stream "  Illumination: ~D%~%" (getf eph :lunar-illumination))))))
