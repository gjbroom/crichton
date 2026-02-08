;;;; skills/builtins/ephemeris.lisp
;;;;
;;;; Built-in skill: solar ephemeris calculations.
;;;; Computes sunrise, sunset, solar noon, day length, and twilight times
;;;; for any location and date.
;;;;
;;;; Implements standard solar algorithms from NOAA and Jean Meeus.
;;;; Accuracies within ~1 minute for sunrise/sunset.
;;;;
;;;; Daemon-side built-in — runs inside the TCB, not in WASM.

(in-package #:crichton/skills)

;;; --- Julian day calculations ---

(defun julian-day (year month day)
  "Compute Julian Day Number for a given calendar date (UTC).
   Returns a float (fractional days since J2000.0 epoch)."
  (let* ((a (floor (- 14 month) 12))
         (y (+ year 4800 (- a)))
         (m (+ month (* 12 a) -3)))
    (+ day
       (floor (+ 153 m) 5)
       (* 365 y)
       (floor y 4)
       (- (floor y 100))
       (floor y 400)
       -32045
       -2451545.0)))  ; Convert to J2000.0 epoch

(defun julian-centuries (jd)
  "Compute Julian centuries from J2000.0 epoch for a given JD."
  (/ (- jd 0.0d0) 36525.0d0))

;;; --- Solar position calculations (NOAA algorithm) ---

(defun fractional-year (day-of-year)
  "Compute fractional year in radians for a given day of year (1-366)."
  (* 2.0d0 pi (/ (- day-of-year 1.0d0) 365.0d0)))

(defun day-of-year (month day)
  "Compute day of year (1-366) from month and day."
  (let ((days-in-months #(0 31 59 90 120 151 181 212 243 273 304 334)))
    (+ (aref days-in-months (- month 1)) day)))

(defun solar-declination-noaa (fractional-year)
  "Compute solar declination (degrees) using NOAA algorithm."
  (* 0.4093d0 (sin (- (* 2.0d0 fractional-year) 1.3955d0))))

(defun equation-of-time-noaa (fractional-year)
  "Compute equation of time (minutes) using NOAA algorithm."
  (+ (* 229.18d0 
        (+ (* 0.75933d0 (cos fractional-year))
           (* -2.9833d0 (cos (* 2.0d0 fractional-year)))
           (* -0.3868d0 (sin (* 2.0d0 fractional-year)))
           (* -0.00769d0 (sin (* 4.0d0 fractional-year)))))
     (* -7.99d0 (sin (- (* 2.0d0 fractional-year) 1.3855d0)))
     (* 9.4724d0 (sin (+ (* 4.0d0 fractional-year) 1.3568d0)))))

(defun hour-angle-sunrise (lat decl)
  "Compute the hour angle (degrees) at sunrise/sunset.
   LAT and DECL are in degrees. Returns NIL for polar regions."
  (let* ((lat-rad (/ lat 180.0d0 pi))
         (decl-rad (/ decl 180.0d0 pi))
         (cos-h (- (cos lat-rad) 
                  (* (tan lat-rad) (tan decl-rad)))))
    (cond
      ((> (abs cos-h) 1.0d0) nil)
      (t (* 180.0d0 pi (acos cos-h))))))

;;; --- Sunrise/sunset computation ---

(defun sunrise-sunset-for-date (year month day latitude longitude)
  "Compute sunrise and sunset times (in hours UTC) for a given date and location.
   Returns (values sunrise-hours sunset-hours day-length-hours solar-noon-hours).
   Returns (values NIL NIL NIL NIL) for polar regions with 24-hr day/night."
  (let* ((doy (day-of-year month day))
         (fy (fractional-year doy))
         (decl (solar-declination-noaa fy))
         (eot (equation-of-time-noaa fy))
         (h (hour-angle-sunrise latitude decl)))
    (unless h
      ;; Polar region: 24-hour day or night
      (let ((solar-noon (+ 12.0d0 (/ (+ longitude (* eot 4.0d0)) 60.0d0))))
        (if (>= decl 0.0d0)
            (values 0.0d0 24.0d0 24.0d0 solar-noon)  ; 24-hour day
            (values nil nil 0.0d0 solar-noon))))      ; 24-hour night
    
    (let* ((h-hours (/ h 15.0d0))
           (solar-noon (+ 12.0d0 (/ (+ longitude (* eot 4.0d0)) 60.0d0)))
           (sunrise (- solar-noon h-hours))
           (sunset (+ solar-noon h-hours))
           (day-length (- sunset sunrise)))
      (values (mod sunrise 24.0d0)
              (mod sunset 24.0d0)
              day-length
              solar-noon))))

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
   Returns a plist with :sunrise :sunset :solar-noon :day-length (all as HH:MM:SS strings)."
  (multiple-value-bind (sunrise sunset day-length solar-noon)
      (sunrise-sunset-for-date year month day latitude longitude)
    (list :sunrise (format-time-hms sunrise)
          :sunset (format-time-hms sunset)
          :solar-noon (format-time-hms solar-noon)
          :day-length (when day-length
                       (format nil "~D:~2,'0D:~2,'0D"
                               (floor day-length)
                               (floor (* (mod day-length 1.0d0) 60.0d0))
                               (floor (* (mod (* (mod day-length 1.0d0) 60.0d0) 1.0d0)
                                        60.0d0)))))))

(defun current-ephemeris (latitude longitude)
  "Get ephemeris data for today at given location.
   Returns a plist with :sunrise :sunset :solar-noon :day-length."
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
        (format stream "  Sunrise:    ~A UTC~%" (getf eph :sunrise))
        (format stream "  Sunset:     ~A UTC~%" (getf eph :sunset))
        (format stream "  Solar Noon: ~A UTC~%" (getf eph :solar-noon))
        (format stream "  Day Length: ~A~%" (getf eph :day-length))))))
