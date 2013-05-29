(in-package :lens.wsn)

(defgeneric measure(physical-process measurand location time)
  (:documentation "Return the real value of specified measurand from a
  physical process at given time and location"))

(defclass physical-process(module)
  ((description :type string
                :parameter t :initarg :description
                :documentation "Description of physical process - default ")
   (function
    :parameter t :type function
    :initform #'(lambda(m c tm) (declare (ignore m c tm)) (uniform 0.0 1.0))
    :properties (:format (eval :type function))
    :reader physical-process-function
    :documentation "a real function of measurand, location and time"))
  (:metaclass module-class))

(defmethod description((physical-process physical-process))
  (if (slot-boundp physical-process 'description)
      (slot-value  physical-process 'description)
      (index physical-process)))

(defmethod measure((physical-process physical-process)
                   (measurand (eql 'temperature))
                   (location coord) (time real))
  (funcall (physical-process-function physical-process)
           measurand location time))

(defclass scenario-physical-process(physical-process)
  ((k :parameter t :type real :initform 0.25 :initarg :k
                     :documentation "multiplicative parameter (k)")
   (a :parameter t :type real :initform 1.0 :initarg :a
                      :documentation "attenuation exponent (a)")
   (sources :parameter t :type list :properties (:format read)
            :reader sources
            :documentation "List showing how sources evolve over time"
            :initform
            `(((make-coord :x 10.0 :y 10.0) (0.0 30.5) (5.0 45) (10.0 7.3)))))
  (:default-initargs :description 'fire)
  (:metaclass module-class))

(defstruct (snapshot (:type list)) time value)

(defmethod measure((process scenario-physical-process)
                   (measurand (eql 'temperature))
                   (location coord)
                   time)
  (let ((k (slot-value process 'k))
        (a (slot-value process 'a)))
    (reduce
     #'+
     (mapcar
      #'(lambda(source)
          (let ((distance (distance location (car source)))
                (value

                 (loop :for r :on (rest source)
                    :for a = (first r)
                    :for b = (second r)
                    :until (not b)
                    :finally (return (snapshot-value a))
                    :when (>= time (snapshot-time a))
                    :do
                    (let ((dt (- (snapshot-time b) (snapshot-time a)))
                          (dx (- (snapshot-value b) (snapshot-value a))))
                      (return (+ (snapshot-value a)
                                 (* (- time (snapshot-time a)) (/ dx dt))))))))
            (* value (expt (1+ (* k distance)) (- a)))))
      (sources process)))))



