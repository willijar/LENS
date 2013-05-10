

(in-package :lens)

(defvar *vector-output* nil "Stream to write out vector Statistics")
(defvar *scalar-output* nil "Stream to write out scalar statistics")

(defclass result-filter(component)
  ((delegates :type list :initform nil :accessor delegates
              :documentation "List of delegates to received filtered signals")))

(defgeneric report-result(object stream)
  (:documentation "Write report to stream"))

(defgeneric accept(filter value)
  (:documentation "Return a filtered value - if null delegates not called."))

(defmethod receive-signal((listener result-filter) signal source value)
  (let ((v (accept filter value)))
    (when v
    (dolist(d (delegates listener))
      (receive-signal d signal source v)))))

(defclass warmup-period-filter(result-filter)
  ())

(defmethod accept((f warmup-period-filter) value)
  (when (>= (simulation-time) (warmup-period *simulation*)) v))

(defclass expression-filter(result-filter)
  ((expression :type function :initarg :expression)))

(defmethod accept((f expression-filter) value)
  (funcall (slot-value f 'expression) value))

(defclass result-recorder(component)
  ((title :type string :initarg title :parameter t)))

(defmethod info((r result-recorder))
  (with-output-to-string(os) (report-result r os)))

(defmethod title((recorder result-recorder))
  (if (slot-boundp recorder 'title)
      (slot-value recorder 'title)
      (string (name recorder))))

(defclass scalar-recorder(result-recorder)
  ((value :type number :initarg :initvalue)))

(defmethod receive-signal((recorder vector-recorder) signal source (value timestamped))
  (receive-signal recorder signal source (timestamped-value value)))

(defmethod finish((r scalar-recorder))
  (when *scalar-output* (report-result r *scalar-output*)))

(defmethod report-result((r scalar-recorder) stream)
  (format stream "~A ~A ~A~%" (result-name r)
          (full-name (owner r)) (slot-value r 'value)))

(defclass vector-recorder(result-recorder)
  ((last-time :initform 0 :type time-type
              :documentation "to ensure increasing timestamp order")
   (vector :initform (make-array 1024 :element-type 'timestamped-value
                                 :adjustable t :fill-counter 0)
           :initarg :vector)))

(defmethod receive-signal((recorder vector-recorder) signal source value)
  (with-slots(last-time vector) recorder
    (let ((now (simulation-time)))
      (assert (< last-time now)
              "~A cannot record data with earlier timestamp (t=~A)"
              recorder last-time)
      (setf last-time now))
    (vector-push-extend (make-timestamped :value value) vector)))

(defmethod receive-signal((recorder vector-recorder) signal source
                          (value timestamped))
  (with-slots(last-time vector) recorder
    (let ((now (timestamped-value value)))
      (assert (< last-time now)
              "~A cannot record data with earlier timestamp (t=~A)"
              recorder last-time)
      (setf last-time now))
    (vector-push-extend value vector)))

(defmethod finish((r vector-recorder))
  (when *vector-output* (report-result r *vector-output*)))

(defmethod report-result((r vector-recorder) stream)
    (format stream "~A ~A~%" (result-name r)
            (full-name (owner r)))
    (map nil
         #'(lambda(v)
             (format stream "~A ~A~%"
                     (timestamp-time v) (timestamp-value v)))
         (slot-value r 'vector)))

(defclass count-recorder(scalar-recorder)
  ())

(defmethod receive-signal((listener count-recorder) signal source value)
  (declare (ignore signal source value))
  (incf (slot-value listener 'value)))

(defclass last-value-recorder(scalar-recorder)())

(defmethod receive-signal((r last-value-recorder) signal source value)
  (declare (ignore signal source))
  (setf (slot-value r 'value) value))

(defclass sum-recorder(scalar-recorder)())

(defmethod receive-signal((listener sum-recorder) signal source (value number))
  (declare (ignore signal source))
  (incf (slot-value listener 'value) value))

(defclass mean-recorder(scalar-recorder)
  ((sum :type number :initform 0)
   (count :type number :initform 0)))

(defmethod receive-signal((r mean-recorder) signal source (value number))
  (with-slots(sum count value) r
    (incf count)
    (incf sum value)
    (setf value (/ sum count))))

(defclass min-recorder(scalar-recorder)())

(defmethod receive-signal((r min-recorder) signal source (value number))
  (when (or (slot-unboundp r 'value) (< value (slot-value r 'value)))
    (setf (slot-value r 'value) value)))

(defclass max-recorder(scalar-recorder)())

(defmethod receive-signal((r max-recorder) signal source (value number))
  (when (or (slot-unboundp r 'value) (> value (slot-value r 'value)))
    (setf (slot-value r 'value) value)))

(defclass time-average-recorder(scalar-recorder)
  ((start-time :initform -1 :type timetype)
   (last-time :type timetype)))

(defmethod receive-signal((r time-average-recorder) signal source value)
  (multiple-value-bind(time new-value)
      (etypecase value
        (timestamped (values (timestamped-time value) (timestamped-value value)))
        (number (values (simulation-time) value)))
    (with-slots(start-time last-time value) r
      (if (< start-time 0)
          (setf start-time time)
          (incf value (* last-value (- time last-time))))
      (setf last-time now
            last-value new-value))))

(defmethod finish :before ((r time-average-recorder))
  "Take account of last interval"
  (incf value (* last-value (- (simulation-time) last-time))))

(defclass stddev-recorder(result-recorder)
  ((num :type integer :initform 0)
   (min :type number)
   (max :type number)
   (sum :type number :initform 0)
   (sqrsum :type number :initform 0))
  (:documentation "Statistics class to collect min, max, mean, and standard deviation."))

(defmethod receive-signal((r stddev-recorder) signal source (value number))
  (with-slots(num min max sum sqrsum) r
    (setf min (if (slot-boundp r 'min) value (min value min)))
    (setf max (if (slot-boundp r 'max) value (max value max)))
    (incf num)
    (incf sum value)
    (incf sqrsum (* value value))))

(defmethod report((r stddev-recorder) stream)
  (format stream "~A ~A~%" (class-name (class-of r)) (full-path r))
  (with-slots(num min max sum sqrsum)
      (if (zerop num)
          (format stream "n=0~%")
          (format stream "n=~A~% mean=~A~% stddev=~A~% min=~A~% max=~A~%"
                  num (/ sum num) (sqrt (/ sqrsum num)) min max))))

;(defclass weighted-stddev-recorder(stddev-recorder)())