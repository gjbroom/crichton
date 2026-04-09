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

(defparameter *weather-api-host* "api.weather.gc.ca"
  "Host for egress policy checking.")

(defparameter *weather-default-city* "Victoria"
  "Default city for weather queries. Override via config [weather] city.")

;;; --- Retry configuration ---

(defparameter *max-weather-retries* 3
  "Maximum number of automatic retries on transient weather API failures.")

(defparameter *weather-backoff-base* 1
  "Base delay in seconds for exponential backoff; doubles each retry.")

(defun check-weather-egress ()
  "Signal an error if the network egress policy prohibits the weather API host.
   Checks [network] allowed_builtin_http_domains in config.  If the list is absent,
   no restriction applies (built-ins are TCB-trusted).  If set, the weather host
   must appear in it."
  (let ((allowed (crichton/config:config-section-get :network :allowed-builtin-http-domains)))
    (when (and allowed
               (not (member *weather-api-host* allowed :test #'string-equal)))
      (error "Egress denied: ~A not in [network] allowed_builtin_http_domains"
             *weather-api-host*))))

;;; --- Network retry infrastructure ---

(defun weather-error-retryable-p (condition)
  "Return T if CONDITION represents a transient network error worth retrying."
  (or
   ;; Network connection failures
   (and (typep condition 'usocket:socket-error))
   ;; dexador timeout errors
   (and (typep condition 'dex:http-request-failed)
        (let ((status (dex:response-status condition)))
          ;; Retry on 5xx server errors, 429 rate limit, 503 service unavailable
          (or (>= status 500)
              (= status 429)
              (= status 503))))
   ;; General network timeout conditions
   (search "timeout" (format nil "~A" condition) :test #'char-equal)
   (search "connection" (format nil "~A" condition) :test #'char-equal)))

(defmacro with-weather-retry (&body body)
  "Execute BODY, retrying on transient network errors with exponential backoff.
   Sleeps (* *weather-backoff-base* 2^attempt) seconds between retries.
   Re-signals the original error after *max-weather-retries* attempts are exhausted."
  (let ((attempt (gensym "ATTEMPT"))
        (c (gensym "COND"))
        (delay (gensym "DELAY")))
    `(loop for ,attempt from 0
           do (handler-case
                  (return (progn ,@body))
                (error (,c)
                  (if (and (< ,attempt *max-weather-retries*)
                           (weather-error-retryable-p ,c))
                      (let ((,delay (* *weather-backoff-base*
                                       (expt 2 ,attempt))))
                        (log:warn "Weather API error; retrying in ~,1Fs (attempt ~D/~D): ~A"
                                  ,delay (1+ ,attempt) *max-weather-retries* ,c)
                        (sleep ,delay))
                      (error ,c)))))))

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
   Returns the parsed JSON as nested hash-tables/vectors, or signals an error.
   Automatically retries transient network failures with exponential backoff."
  (check-weather-egress)
  (let ((url (weather-api-url city-query)))
    (log:info "Fetching weather for ~S from ~A" city-query *weather-api-base*)
    (with-weather-retry
      (handler-case
          (multiple-value-bind (body status headers)
              (dex:get url 
                       :headers '(("Accept" . "application/json"))
                       :connect-timeout 10
                       :read-timeout 30)
            (declare (ignore headers))
            (cond
              ((= status 404)
               ;; 404 is not retryable - city not found
               (error "Weather data not found for city: ~S" city-query))
              ((/= status 200)
               ;; Other non-200 status codes may be retryable
               (error "Weather API returned HTTP ~D" status))
              (t
               (handler-case
                   (shasht:read-json body)
                 (error (c)
                   (error "Failed to parse weather JSON response: ~A" c))))))
        (error (c)
          ;; Let the retry macro handle this
          (error c))))))

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
   Returns the raw feature properties hash-table.
   Automatically retries transient network failures."
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
   Suitable for programmatic use by other daemon components.
   Automatically retries transient network failures."
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