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

;;; --- Sunrise/Sunset using USNO Algorithm ---

(defun sunrise-sunset-for-date (year month day latitude longitude)
  "Compute sunrise and sunset times (in hours UTC) for a given date and location.
   Based on USNO algorithm (Almanac for Computers, 1990).
   
   Returns (values sunrise-hours sunset-hours day-length-hours solar-noon-hours).
   Returns (values NIL NIL NIL NIL) for polar regions with 24-hr day/night.
   
   - sunrise-hours: time of sunrise in hours UTC
   - sunset-hours: time of sunset in hours UTC
   - day-length-hours: length of day in hours
   - solar-noon-hours: time of solar noon in hours UTC"
  
  ;; Step 1: Calculate day of year
  (let* ((n (day-of-year month day))
         (lng-hour (/ longitude 15.0d0)))
    
    ;; Calculate for both sunrise and sunset to get both times and solar noon
    ;; For sunrise: t = N + ((6 - lngHour) / 24)
    ;; For sunset: t = N + ((18 - lngHour) / 24)
    ;; For solar noon: approximately N + (12 - lngHour)/24
    
    (let* ((t-rise (+ n (/ (- 6.0d0 lng-hour) 24.0d0)))
           (t-set (+ n (/ (- 18.0d0 lng-hour) 24.0d0)))
           (t-noon (+ n (/ (- 12.0d0 lng-hour) 24.0d0)))
           
           ;; Step 2: Calculate Sun's mean anomaly M = (0.9856 * t) - 3.289
           (m-rise (* (- (* 0.9856d0 t-rise) 3.289d0)))
           (m-set (* (- (* 0.9856d0 t-set) 3.289d0)))
           (m-noon (* (- (* 0.9856d0 t-noon) 3.289d0)))
           
           ;; Step 3: Calculate Sun's true longitude
           ;; L = M + (1.916 * sin(M)) + (0.020 * sin(2 * M)) + 282.634
           (l-rise (normalize-degrees (+ m-rise (* 1.916d0 (sin-deg m-rise))
                                         (* 0.020d0 (sin-deg (* 2.0d0 m-rise)))
                                         282.634d0)))
           (l-set (normalize-degrees (+ m-set (* 1.916d0 (sin-deg m-set))
                                        (* 0.020d0 (sin-deg (* 2.0d0 m-set)))
                                        282.634d0)))
           (l-noon (normalize-degrees (+ m-noon (* 1.916d0 (sin-deg m-noon))
                                         (* 0.020d0 (sin-deg (* 2.0d0 m-noon)))
                                         282.634d0)))
           
           ;; Step 4: Calculate Sun's right ascension
           ;; RA = atan(0.91764 * tan(L))
           ;; Must be adjusted to correct quadrant
           (ra-rise-raw (atan-deg (* 0.91764d0 (tan (deg->rad l-rise))) 1.0d0))
           (ra-set-raw (atan-deg (* 0.91764d0 (tan (deg->rad l-set))) 1.0d0))
           (ra-noon-raw (atan-deg (* 0.91764d0 (tan (deg->rad l-noon))) 1.0d0))
           
           ;; Adjust RA to correct quadrant
           (l-quad-rise (floor (/ l-rise 90.0d0)))
           (l-quad-set (floor (/ l-set 90.0d0)))
           (l-quad-noon (floor (/ l-noon 90.0d0)))
           
           (ra-quad-rise (floor (/ ra-rise-raw 90.0d0)))
           (ra-quad-set (floor (/ ra-set-raw 90.0d0)))
           (ra-quad-noon (floor (/ ra-noon-raw 90.0d0)))
           
           (ra-rise (+ ra-rise-raw (- (* 90.0d0 l-quad-rise) (* 90.0d0 ra-quad-rise))))
           (ra-set (+ ra-set-raw (- (* 90.0d0 l-quad-set) (* 90.0d0 ra-quad-set))))
           (ra-noon (+ ra-noon-raw (- (* 90.0d0 l-quad-noon) (* 90.0d0 ra-quad-noon))))
           
           ;; Convert RA to hours (divide by 15)
           (ra-hours-rise (/ ra-rise 15.0d0))
           (ra-hours-set (/ ra-set 15.0d0))
           (ra-hours-noon (/ ra-noon 15.0d0))
           
           ;; Step 5: Calculate Sun's declination
           ;; sinDec = 0.39782 * sin(L)
           ;; cosDec = cos(asin(sinDec))
           (sin-dec-rise (* 0.39782d0 (sin-deg l-rise)))
           (sin-dec-set (* 0.39782d0 (sin-deg l-set)))
           (sin-dec-noon (* 0.39782d0 (sin-deg l-noon)))
           
           (cos-dec-rise (cos (asin sin-dec-rise)))
           (cos-dec-set (cos (asin sin-dec-set)))
           (cos-dec-noon (cos (asin sin-dec-noon)))
           
           ;; Step 6: Calculate Sun's local hour angle
           ;; cosH = (cos(zenith) - (sinDec * sin(latitude))) / (cosDec * cos(latitude))
           ;; For standard sunrise/sunset, zenith = 90°50' ≈ 90.833°
           (zenith 90.833d0)
           (cos-h-rise (/ (- (cos-deg zenith) (* sin-dec-rise (sin-deg latitude)))
                         (* cos-dec-rise (cos-deg latitude))))
           (cos-h-set (/ (- (cos-deg zenith) (* sin-dec-set (sin-deg latitude)))
                        (* cos-dec-set (cos-deg latitude))))
           (cos-h-noon (/ (- (cos-deg 0.0d0) (* sin-dec-noon (sin-deg latitude)))
                         (* cos-dec-noon (cos-deg latitude)))))
      
      ;; Check for polar regions (no sunrise/sunset)
      (if (or (> (abs cos-h-rise) 1.0d0) (> (abs cos-h-set) 1.0d0))
          ;; Polar region: 24-hour day or night
          (if (>= sin-dec-rise 0.0d0)
              (values 0.0d0 24.0d0 24.0d0 12.0d0)  ; 24-hour day, solar noon at 12 UTC
              (values nil nil 0.0d0 12.0d0))         ; 24-hour night
          
          ;; Normal case: both sunrise and sunset
          (let* (;; Step 7: Calculate H and convert to hours
                 ;; For sunrise: H = 360 - acos(cosH), then H = H / 15
                 ;; For sunset: H = acos(cosH), then H = H / 15
                 (h-rise (/ (normalize-degrees (- 360.0d0 (acos-deg cos-h-rise))) 15.0d0))
                 (h-set (/ (acos-deg cos-h-set) 15.0d0))
                 (h-noon (/ (acos-deg cos-h-noon) 15.0d0))
                 
                 ;; Step 8: Calculate local mean time
                 ;; T = H + RA - (0.06571 * t) - 6.622
                 (t-rise (+ h-rise ra-hours-rise (* -0.06571d0 t-rise) -6.622d0))
                 (t-set (+ h-set ra-hours-set (* -0.06571d0 t-set) -6.622d0))
                 (t-noon (+ ra-hours-noon (* -0.06571d0 t-noon) -6.622d0))
                 
                 ;; Step 9: Adjust back to UTC
                 ;; UT = T - lngHour
                 (ut-rise (- t-rise lng-hour))
                 (ut-set (- t-set lng-hour))
                 (ut-noon (- t-noon lng-hour))
                 
                 ;; Normalize to [0, 24)
                 (sunrise-utc (mod ut-rise 24.0d0))
                 (sunset-utc (mod ut-set 24.0d0))
                 (solar-noon-utc (mod ut-noon 24.0d0))
                 
                 ;; Day length is sunset - sunrise
                 (day-length (- (if (< sunset-utc sunrise-utc)
                                     (+ sunset-utc 24.0d0)
                                     sunset-utc)
                               sunrise-utc)))
            
            (values sunrise-utc sunset-utc day-length solar-noon-utc))))))

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
