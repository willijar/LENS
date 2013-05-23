

(in-package :lens)

(defgeneric title(instance)
  (:documentation "Return the publishable title for a result"))

(defgeneric record(recorder time value)
  (:documentation "Must be implemented for all result recorders"))

(defgeneric report(recorder stream)
  (:documentation "Report results from recorder to stream"))

(defstruct weighted ;; structure for collecting weighted values
  (weight 1.0 :type double-float)
  (value))

(defmethod record(recorder time (value weighted))
  "By default most recorders only want value"
  (record recorder time (weighted-value value)))

(defvar *statistic-filter-generators* (make-hash-table)
  "Mapping betetween statistic filter names and function generators")

(defvar *result-recorders* (make-hash-table)
  "Mapping between statistic recorder name and classes")

(defmacro define-statistic-filter(name (var &rest statevars) &body body)
  "Define and register a statistic filter function. var is the
numberic value to be processed, statevars are the state value
definitions as per let"
  (setf
   (gethash name *statistic-filter-generators*)
    (function (lambda()
      (eval
      `(let (,@statevars)
          (function (lambda(,var)
            (or (progn ,@body)
                (throw 'filter-abort ,name))))))))))

(defun make-statistic-filter(name)
  (let ((f (gethash name *statistic-filter-generators*)))
    (when f (funcall f))))

(defmacro define-result-recorder(name &rest rest)
  (multiple-value-bind(classname name)
      (if (listp name) (values (first name) (second name)) (values name name))
    `(eval-when(:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *result-recorders*)
             (defclass ,classname ,@rest)))))

(defun make-result-recorder(spec owner)
  (multiple-value-bind(name args)
      (if (listp spec) (values (car spec) (cdr spec)) (values spec))
    (let ((class (gethash name *result-recorders*)))
      (if class
          (apply #'make-instance class
                 (nconc (list :name name :owner owner) args))
          (error "Unknown result recorder ~A" name)))))

(defclass statistic-listener(listener)
  ((title :type string :reader title)
   (signal-values :type list :initform nil :accessor signal-values
                  :documentation "Cache of last signal values received")
   (source :type function :initform nil :initarg :source :reader source
           :documentation "Filter function for this statistic")
   (recorders :initarg :recorder :initform nil :reader recorders
              :documentation "The result recorders for this statistic"))
  (:documentation "Listener for statistics"))

(defun filter-code-walk(instance expression)
  "Return filter code from a filter expression"
  (let ((signals nil))
    (labels ((do-expand(form)
              (typecase form
                (symbol
                 (if (signal-id form)
                     (progn
                       (pushnew form signals)
                       `(signal-value ',form))
                     form))
               (list
                (let* ((name (first form))
                       (args (rest form))
                       (filter (make-statistic-filter name)))
                  (if filter
                      `(funcall ,filter ,@(mapcar #'do-expand args))
                      `(,name ,@(mapcar #'do-expand args)))))
               (t form))))
      (values
       (eval
        `(flet((signal-value(name)
                 (or (getf (signal-values ,instance) name)
                     (throw 'filter-abort name))))
           (lambda() ,(do-expand expression))))
       signals))))

(defmethod initialize-instance :after ((instance statistic-listener)
                                       &key statistic &allow-other-keys)
  (setf (slot-value instance 'title)
        (or (getf statistic :title) (string (name instance))))
  (let* ((spec
          (multiple-value-bind(v f-p)
              (read-parameter instance 'result-recording-modes 'read)
            (if f-p v (list :default))))
         (recording-modes
          (append (when (member :default spec) (getf statistic :default))
                  (when (member :all spec) (getf statistic :optional)))))
    (loop :for a :on spec :by #'cdr
       :when (eql (car a) +)
       :do (pushnew (second a) recording-modes)
       :when (eql (car a) -)
       :do (setf recording-modes (delete (second a) recording-modes :test #'equal)))
    (setf (slot-value instance 'recorders)
            (mapcar
             #'(lambda(recorder-mode)
                 (make-result-recorder recorder-mode instance))
             recording-modes)))
    (multiple-value-bind(filter signals)
        (filter-code-walk instance
                          (or (getf statistic :source) (name instance)))
      (setf (slot-value instance 'source)  filter)
      (dolist(signal signals)
        (subscribe (owner instance) signal instance))))

(defclass result-recorder(owned-object)
  ()
  (:documentation "A base class for all result recorders"))

(defmethod title((instance result-recorder))
  (format nil "~A[~A]"
          (title (owner instance))
          (class-name (class-of instance))))

;; map listener receive-signal with source onto statistic receive-signal with time
(defmethod receive-signal((listener statistic-listener) signal
                          (source component) (value timestamped))
  (receive-signal listener signal
                  (timestamped-time value) (timestamped-value value)))

(defmethod receive-signal((listener statistic-listener) signal
                          (source component) (value number))
  (receive-signal listener signal (simulation-time) value))

(defmethod receive-signal((listener statistic-listener) signal (time real)
                          value)
  (unless (>= time (warmup-period *simulation*))
    (return-from receive-signal))
  (setf (getf (signal-values listener) signal) value)
  (catch 'filter-abort
    (let ((value (funcall (source listener))))
      (dolist(recorder (recorders listener))
        (record recorder time value)))))

(defmethod info((r result-recorder))
  (with-output-to-string(os) (report r os)))

(defclass scalar-recorder(result-recorder)
  ())

(defgeneric recorded-value(scalar-recorder)
  (:documentation "Return the value to record for a scalar recorder"))

(defmethod finish((r scalar-recorder))
  (let ((os (scalar-stream *simulation*)))
    (when (and os (scalar-recording r))
      (report r os))))

(defmethod report((r scalar-recorder) stream)
  (format stream "scalar ~A ~A ~e~%"
          (full-path-string (owner r)) (title r) (recorded-value r)))

(define-result-recorder (vector-recorder vector)(result-recorder)
  ((last-time :initform -1 :type time-type :accessor last-time
              :documentation "to ensure increasing timestamp order")
   (recorded-vector :initform (make-array 1024 :element-type 'timestamped
                                 :adjustable t :fill-pointer 0)
                    :reader recorded-vector)))

(defmethod record((recorder vector-recorder) time (value real))
  (with-slots(last-time recorded-vector) recorder
    (assert (< last-time time)
            ()
            "~A cannot record data with earlier timestamp (t=~A)"
            recorder last-time)
    (setf last-time time)
  (vector-push-extend (make-timestamped :time time :value value)
                      recorded-vector)))

(defmethod finish((r vector-recorder))
  (let ((os (vector-stream *simulation*)))
    (when (and os (vector-recording r))
      (report r os))))

(defmethod report((r vector-recorder) stream)
  (format stream "vector ~A ~A~%" (full-path-string (owner r)) (title r))
  (map nil
       #'(lambda(v)
           (format stream "~e~t~e~%" (timestamped-time v) (timestamped-value v)))
       (recorded-vector r)))

(define-statistic-filter count(value (count 0))
  (declare (ignore value))
  (incf count))

(define-statistic-filter sum(value (sum 0))
  (incf sum value))

(define-statistic-filter mean(value (sum 0) (count 0))
  (/ (incf sum value) (incf count)))

(define-statistic-filter min(value min)
  (when (or (not min) (< value min)) (setf min value)))

(define-statistic-filter max(value max)
  (when (or (not max) (> value max)) (setf max value)))

(define-statistic-filter constant0(value) 0)

(define-statistic-filter constant1(value) 1)

(define-result-recorder (count-recorder count)(scalar-recorder)
  ((count :accessor recorded-value :initform 0)))

(defmethod record((recorder count-recorder) time value)
  (declare (ignore time value))
  (incf (recorded-value recorder)))

(define-result-recorder sum(scalar-recorder)
  ((sum :accessor recorded-value :initform 0)))

(defmethod record((recorder sum) time value)
  (declare (ignore time))
  (incf (recorded-value recorder) value))

(define-result-recorder mean(scalar-recorder)
  ((sum :initform 0)
   (count :initform 0)))

(defmethod record((recorder mean) time value)
  (declare (ignore time))
  (with-slots(sum count) recorder
    (incf sum value)
    (incf count)))

(defmethod recorded-value((recorder mean))
  (with-slots(sum count) recorder
    (unless (zerop count)
      (/ sum count))))

(define-result-recorder last-value(scalar-recorder)
  ((value :initform nil :accessor recorded-value)))

(defmethod record ((r last-value) time value)
  (declare (ignore time))
  (setf (recorded-value r) value))

(define-result-recorder (min-recorder min)(last-value)())

(defmethod record ((r min-recorder) time value)
  (declare (ignore time))
  (when (or (not (recorded-value r)) (< value (recorded-value r)))
    (setf (recorded-value r) value)))

(define-result-recorder (max-recorder max)(last-value)())

(defmethod record ((r max-recorder) time value)
  (declare (ignore time))
  (when (or (not (recorded-value r)) (> value (recorded-value r)))
    (setf (recorded-value r) value)))

(define-result-recorder timeavg(scalar-recorder)
  ((start-time :initform -1 :type timetype)
   (last-time :type timetype)
   (weighted-sum :type real :initform 0)
   (last-value :type real :initform 0)))

(defmethod record((r timeavg) time value)
  (with-slots(start-time last-time last-value weighted-sum) r
    (if (< start-time 0)
        (setf start-time time)
        (incf weighted-sum (* last-value (- time last-time))))
    (setf last-time time
          last-value value)))

(defmethod finish :before ((r timeavg))
  "Take account of last interval"
  (record r (simulation-time) nil))

(defmethod recorded-value((r timeavg))
  (with-slots(weighted-sum start-time last-time) r
    (/ weighted-sum (- last-time start-time))))

(define-result-recorder stddev(scalar-recorder)
  ((count :type integer :initform 0 :reader result-count)
   (min :type real :initform nil :reader result-min)
   (max :type real :initform nil :reader result-max)
   (sum :type real :initform 0 :reader result-sum)
   (sqrsum :type real :initform 0 :reader result-sqrsum)))

(defgeneric result-mean(r)
  (:method((r stddev))
    (with-slots(sum count) r
      (unless (zerop count) (/ sum count)))))

(defgeneric result-variance(r)
  (:method((r stddev))
    (with-slots(sqrsum sum count) r
      (unless (<= count 1)
        (/ (- sqrsum (/ (* sum sum) count)) (1- count))))))

(defgeneric result-stddev(r)
  (:method((r stddev))
    (let ((v (result-variance r)))
      (when v (sqrt v)))))

(defmethod record((r stddev) time (value real))
  (declare (ignore time))
  (with-slots(min max sum count sqrsum) r
      (when (or (not min) (< value min)) (setf min value))
      (when (or (not max) (> value max)) (setf max value))
      (incf count)
      (incf sum value)
      (incf sqrsum (* value value))))

(defmethod report((r stddev) stream)
  (format stream "statistic ~A ~A~%" (full-path-string (owner r)) (title r))
  (with-slots(min max sum count sqrsum) r
    (format stream "field count ~e~%field mean ~e~%field stddev ~e~%field sum ~e~%field sqrsum ~e~%field min ~e~%field max ~e~%"
            (result-count r)
            (result-mean r)
            (result-stddev r)
            (result-sum r)
            (result-sqrsum r)
            (result-min r)
            (result-max r))))

(define-result-recorder weighted-stddev(stddev)
  ((sum-weights :type real :initform 0)
   (sum-weighted-vals :type real :initform 0)
   (sum-squared-weights :type real :initform 0)
   (sum-weights-squared-vals :type real :initform 0)))

(defmethod result-mean((r weighted-stddev))
  (with-slots(sum-weights sum-weighted-vals) r
    (unless (zerop sum-weights) (/ sum-weights sum-weighted-vals))))

(defmethod result-variance((r weighted-stddev))
  (with-slots(count sum-weights sum-weighted-vals
                    sum-squared-weights
                    sum-weights-squared-vals) r
    (unless (<= count 1)
      (/ (- (* sum-weights sum-weights-squared-vals)
            (* sum-weighted-vals sum-weighted-vals))
         (- (* sum-weights sum-weights)
            sum-squared-weights)))))

(defmethod record((r weighted-stddev) time value)
  (call-next-method)
  (multiple-value-bind(value weight)
      (etypecase value
        (real (values value 1))
        (weighted (values (weighted-weight value) (weighted-value value))))
    (with-slots(sum-weights sum-weighted-vals
                sum-squared-weights sum-weights-squared-vals) r
      (incf sum-weights weight)
      (incf sum-weighted-vals (* weight value))
      (incf sum-squared-weights (* weight weight))
      (incf sum-weights-squared-vals (* weight value value)))))

(defmethod report((r weighted-stddev) stream)
  (call-next-method)
  (with-slots(sum-weights sum-weighted-vals sum-squared-weights
                          sum-weights-squared-vals) r
  (format stream "field weights ~e~% field weightedSum ~e~%field sqrSumWeights ~e~%field weightedSqrSum ~e~%"
          sum-weights sum-weighted-vals sum-squared-weights
          sum-weights-squared-vals)))

(defun add-statistics(sim)
  (labels((do-add-statistics(module)
            (loop :for a :on (properties module) :by #'cddr
               :when (eql (car a) :statistic)
               :do (make-instance 'statistic-listener
                                  :owner module
                                  :name (car (second a))
                                  :statistic (rest (second a))))
            (when (typep module 'compound-module)
              (for-each-submodule module #'do-add-statistics))))
    (do-add-statistics (network sim))))

(eval-when(:load-toplevel :execute)
  (pushnew #'add-statistics *simulation-init-hooks*))

;; ;; TODO add densityestbase and then histogram types

;; (defclass density-estimation(stddev)
;;   ((range-min :initarg :min :initform nil :type real :accessor range-min)
;;    (range-max :initarg :max :initform nil :type real :accessor range-max)
;;    (range-ext-factor :initarg :range-exp-factor :initform 2.0
;;                      :type real :reader range-ext-factor
;;                      :documentation "Factor to expand range by")
;;    (num-cells :initarg :num-cells :initform 10 :reader num-cells
;;               :documentation "How man cells to use.")
;;    (cell-size :initform nil :reader cell-size
;;               :reader density-estimation-transformed-p
;;               :documentation "Cell size once scale determined.")
;;    (array :type (array real *) :reader firstvals :reader cells
;;           :documentation "Pre-collected observations or cells")
;;    (underflow-cell :initform 0 :type integer :accessor underflow-cell
;;                    :documentation "Number of observations below range-min")
;;    (overflow-cell :initform 0 :type integer :accessor overflow-cell
;;                   :documentation "Number of observations above range-max"))
;;   (:documentation "Base class for density estimation classes.

;;  For the histogram classes, you need to specify the number of cells
;;  and the range. Range can either be set explicitly or you can choose
;;  automatic range determination.

;;  Automatic range estimation works in the following way:

;;  1.  The first num_firstvals observations are stored.
;;  2.  After having collected a given number of observations, the actual
;;      histogram is set up. The range (*min*, *max*) of the
;;      initial values is expanded *range_ext_factor* times, and
;;      the result will become the histogram's range (*rangemin*,
;;      *rangemax*). Based on the range, the cells are layed out.
;;      Then the initial values that have been stored up to this point
;;      will be transferred into the new histogram structure and their
;;      store is deleted -- this is done by the transform() function.

;;    You may also explicitly specify the lower or upper limit and have
;;    the other end of the range estimated automatically. The setRange...()
;;    member functions of cDensityEstBase deal with setting
;;    up the histogram range. It also provides pure virtual functions
;;    transform() etc.

;;    Subsequent observations are placed in the histogram structure.
;;    If an observation falls out of the histogram range, the *underflow*
;;    or the *overflow* *cell* is incremented."))



;; (defun density-estimation-transform(instance)
;;   (with-slots(range-min range-max range-ext-factor cell-size array) instance
;;     (unless (and range-min range-max)
;;       (let ((min (reduce #'min (firstvals instance)))
;;             (max (reduce #'max (firstvals instance))))
;;         (cond
;;           (range-max ;; only min needs determining
;;            (setf range-min
;;                  (if (<= range-max min)
;;                      (- range-max 1.0)
;;                      (- range-max (* (- range-max min) range-ext-factor)))))
;;           (range-min
;;            (setf range-max
;;                  (if (>= range-in max)
;;                      (+ range-min 1.0)
;;                      (+ range-min (* (- max range-min) range-ext-factor)))))
;;           (t
;;            (let ((c (/ (+ max min) 2))
;;                  (r (* (- max min) range-ext-factor)))
;;              (when (zerop r) (setf r 1.0))
;;              (setf range-min (- c (/ r 2))
;;                    range-max (+ c (/ r 2))))))))
;;     (let ((firstvals (when (slot-boundp instance 'array) array)))
;;       (setf array (make-array (num-cells instance) :element-type 'integer
;;                               :initial-element 0))
;;       (setf cell-size (/ (- range-max range-min) (num-cells instance)))
;;       (when firstvals (map nil #'(lambda(v) (record instance 0 v)) firstvals)))))

;; (defmethod initialize-instance :after
;;     ((instance density-estimation) &key
;;      (num-firstvals 100) &allow-other-keys)
;;   (with-slots(range-min range-max array cell-size) instance
;;     (if (and range-max range-min)
;;         (density-estimation-transform instance)
;;         (progn
;;           (setf array (make-array (num-cells instance) :element-type 'integer))
;;           (setf cell-size (/ (- range-max range-min) (num-cells instance))))
;;         (setf array (make-array num-firstvals :element-type 'float-double
;;                                 :fill-pointer 0)))))

;; (defmethod record((instance density-estimation) time value)
;;   (call-next-method)
;;   (when (not (density-estimation-transformed-p instance))
;;     (let ((firstvals (firstvals instance)))
;;       (if (< (length firstvals) (array-total-size firstvals))
;;           (progn
;;             (vector-push value firstvals)
;;             (return-from record))
;;           (density-estimation-transform instance))))
;;   (with-slots(range-min range-max num-cells)
;;       (let ((k (floor (- val rangemin))))
;;         (cond
;;           ((or (< k 0) (< value range-min))
;;            (incf (underflow-cell instance)))
;;           ((or (>= k num-cells) (>= value range-max))
;;            (incf (overflow-cell instance)))
;;           (t (incf (aref (cell instance) k)))))))

;; (defmethod report((r density-estimation) stream)
;;   (call-next-method)
;;   (unless (density-estimation-transformed-p r)
;;     (density-estimation-transform r))
;;   (format stream "attr unit s~%") ;; TODO sllow specification of attributes in initargs
;;   (format stream "bin -INF ~D~%" (underflow-cell r))
;;   (let ((b (range-min r)))
;;     (dotimes(k (length (cells r)))
;;       (format stream "bin ~e ~D~%" b (aref (cells r) k))
;;       (incf b (cell-size r))))
;;   (format stream "bin +INF ~D~%" (overflow-cell r)))


;; ;; TODO getcdf, getpdf random etc from cdensityestbase and chistogram