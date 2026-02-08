;;;; skills/builtins/weather.lisp
;;;;
;;;; Built-in skill: Environment Canada weather from api.weather.gc.ca
;;;; Fetches current conditions and forecasts for Canadian cities via the
;;;; MSC GeoMet OGC API (citypageweather-realtime collection).
;;;;
;;;; This is a daemon-side built-in — it runs inside the TCB, not in WASM.
;;;; It exists as a concrete example of daemon-mediated HTTP and will
;;;; inform the design of the WASM http_request host function later.
;;;;
;;;; API: https://api.weather.gc.ca/collections/citypageweather-realtime
;;;; Data license: Environment and Climate Change Canada End-use Licence

(in-package #:crichton/skills)

(defparameter *weather-api-base*
  "https://api.weather.gc.ca/collections/citypageweather-realtime/items"
  "Base URL for the MSC citypageweather-realtime OGC API.")

(defparameter *weather-default-city* "Victoria"
  "Default city for weather queries. Override via config [weather] city.")

;;; --- API access ---

(defun percent-encode-query (str)
  "Minimal percent-encoding for URL query values. Encodes spaces and non-ASCII."
  (with-output-to-string (out)
    (loop for c across str
          do (cond
               ((char= c #\Space) (write-string "%20" out))
               ((char= c #\&) (write-string "%26" out))
               ((char= c #\=) (write-string "%3D" out))
               ((char= c #\+) (write-string "%2B" out))
               ((char= c #\#) (write-string "%23" out))
               (t (write-char c out))))))

(defun weather-api-url (city-query &key (lang "en") (limit 1))
  "Build the API URL for a city weather query."
  (format nil "~A?f=json&lang=~A&limit=~D&q=~A"
          *weather-api-base* lang limit
          (percent-encode-query city-query)))

(defun fetch-weather-json (city-query)
  "Fetch weather data for CITY-QUERY from api.weather.gc.ca.
   Returns the parsed JSON as nested hash-tables/vectors, or signals an error."
  (let ((url (weather-api-url city-query)))
    (log:info "Fetching weather for ~S from ~A" city-query *weather-api-base*)
    (handler-case
        (multiple-value-bind (body status headers)
            (dex:get url :headers '(("Accept" . "application/json")))
          (declare (ignore headers))
          (unless (= status 200)
            (error "Weather API returned HTTP ~D" status))
          (shasht:read-json body))
      (error (c)
        (error "Weather fetch failed for ~S: ~A" city-query c)))))

;;; --- JSON navigation helpers ---
;;; The API nests values under en/fr language keys.

(defun jget (obj &rest keys)
  "Navigate nested hash-tables/vectors by KEYS. Returns NIL if any key is missing."
  (let ((current obj))
    (dolist (key keys current)
      (cond
        ((null current) (return nil))
        ((hash-table-p current)
         (setf current (gethash key current)))
        ((and (vectorp current) (integerp key) (< key (length current)))
         (setf current (aref current key)))
        (t (return nil))))))

(defun jen (obj &rest keys)
  "Like JGET but appends \"en\" as the final key — extracts the English value."
  (apply #'jget obj (append keys (list "en"))))

;;; --- Data extraction ---

(defun extract-current-conditions (props)
  "Extract current conditions from the feature properties.
   Returns a plist of human-readable weather data."
  (let ((cc (jget props "currentConditions")))
    (when cc
      (list :city (jen props "name")
            :region (jen props "region")
            :condition (jen cc "condition")
            :temperature (jen cc "temperature" "value")
            :temp-units (jen cc "temperature" "units")
            :humidity (jen cc "relativeHumidity" "value")
            :wind-speed (jen cc "wind" "speed" "value")
            :wind-units (jen cc "wind" "speed" "units")
            :wind-dir (jen cc "wind" "direction" "value")
            :wind-gust (jen cc "wind" "gust" "value")
            :pressure (jen cc "pressure" "value")
            :pressure-units (jen cc "pressure" "units")))))

(defun extract-forecasts (props &key (count 6))
  "Extract forecast entries from the feature properties.
   Returns a list of plists, up to COUNT entries."
  (let* ((fg (jget props "forecastGroup"))
         (forecasts (when fg (jget fg "forecasts"))))
    (when (and forecasts (vectorp forecasts))
      (loop for i below (min count (length forecasts))
            for f = (aref forecasts i)
            collect
            (let ((temps (jget f "temperatures" "temperature")))
              (list :period (jen f "period" "textForecastName")
                    :summary (jen f "abbreviatedForecast" "textSummary")
                    :detail (jen f "textSummary")
                    :temperature (when (and temps (vectorp temps) (plusp (length temps)))
                                   (jen (aref temps 0) "value"))
                    :temp-units (when (and temps (vectorp temps) (plusp (length temps)))
                                  (jen (aref temps 0) "units"))
                    :temp-class (when (and temps (vectorp temps) (plusp (length temps)))
                                  (jen (aref temps 0) "class"))))))))

(defun extract-warnings (props)
  "Extract active weather warnings from the feature properties.
   Returns a list of warning description strings, or NIL if none."
  (let ((warnings (jget props "warnings")))
    (when (and warnings (vectorp warnings) (plusp (length warnings)))
      (loop for i below (length warnings)
            for w = (aref warnings i)
            collect (or (jen w "description") (jen w "type") "Warning")))))

;;; --- Formatted output ---

(defun format-current-conditions (cc &optional (stream *standard-output*))
  "Format current conditions plist CC as a human-readable report."
  (when cc
    (format stream "~&~A (~A)~%" (getf cc :city) (getf cc :region))
    (format stream "  Conditions: ~A~%" (or (getf cc :condition) "N/A"))
    (format stream "  Temperature: ~A°~A~%"
            (or (getf cc :temperature) "?") (or (getf cc :temp-units) "C"))
    (format stream "  Humidity: ~A%~%" (or (getf cc :humidity) "?"))
    (let ((speed (getf cc :wind-speed))
          (dir (getf cc :wind-dir))
          (gust (getf cc :wind-gust))
          (units (getf cc :wind-units)))
      (format stream "  Wind: ~A ~A ~A~@[ gusting ~A~] ~A~%"
              (or dir "") (or speed "calm") (or units "km/h")
              (when (and gust (numberp gust) (plusp gust)) gust)
              (if (and gust (numberp gust) (plusp gust)) (or units "") "")))
    (format stream "  Pressure: ~A ~A~%"
            (or (getf cc :pressure) "?") (or (getf cc :pressure-units) "kPa"))))

(defun format-forecasts (forecasts &optional (stream *standard-output*))
  "Format forecast plists as a human-readable report."
  (when forecasts
    (format stream "~&Forecast:~%")
    (dolist (f forecasts)
      (let ((temp (getf f :temperature))
            (units (getf f :temp-units))
            (cls (getf f :temp-class)))
        (format stream "  ~A: ~A~@[ (~A ~A°~A)~]~%"
                (or (getf f :period) "?")
                (or (getf f :summary) "?")
                (when temp cls) temp units)))))

(defun format-warnings (warnings &optional (stream *standard-output*))
  "Format active weather warnings."
  (when warnings
    (format stream "~&** WARNINGS **~%")
    (dolist (w warnings)
      (format stream "  ~A~%" w))))

;;; --- Public interface ---

(defun weather-report (&key (city nil) (forecasts 6) (stream *standard-output*))
  "Fetch and display a weather report for CITY (default from config or Victoria).
   Returns the raw feature properties hash-table."
  (let* ((city-name (or city
                        (crichton/config:config-section-get :weather :city)
                        *weather-default-city*))
         (data (fetch-weather-json city-name))
         (features (jget data "features"))
         (feature (when (and features (vectorp features) (plusp (length features)))
                    (aref features 0)))
         (props (when feature (jget feature "properties"))))
    (unless props
      (error "No weather data found for ~S" city-name))
    (let ((warnings (extract-warnings props))
          (cc (extract-current-conditions props))
          (fc (extract-forecasts props :count forecasts)))
      (format-warnings warnings stream)
      (format-current-conditions cc stream)
      (format-forecasts fc stream)
      props)))

(defun weather-conditions (&key (city nil))
  "Fetch current conditions for CITY. Returns a plist.
   Suitable for programmatic use by other daemon components."
  (let* ((city-name (or city
                        (crichton/config:config-section-get :weather :city)
                        *weather-default-city*))
         (data (fetch-weather-json city-name))
         (features (jget data "features"))
         (feature (when (and features (vectorp features) (plusp (length features)))
                    (aref features 0)))
         (props (when feature (jget feature "properties"))))
    (unless props
      (error "No weather data found for ~S" city-name))
    (extract-current-conditions props)))
