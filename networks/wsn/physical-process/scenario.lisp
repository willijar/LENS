(in-package :lens.wsn)


(defclass scenario-physical-process(physical-process)
  ((k :parameter t :type real :initform 0.25 :initarg :k
                     :documentation "multiplicative parameter (k)")
   (a :parameter t :type real :initform 1.0 :initarg :a
                      :documentation "attenuation exponent (a)")
   (sources :parameter t :type list :properties (:format read)
            :reader sources
            :documentation "List showing how sources evolve over time"
            :initform
            `((,(make-coord 10.0 10.0) (0.0 . 30.5) (5.0 . 45) (10.0 . 7.3)))))
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
                    :for a = (car r)
                    :for b = (cdr r)
                    :until (not r)
                    :finally (return (snapshot-value a))
                    :when (>= time (snapshot-time a))
                    :do
                    (let ((dt (- (snapshot-time b) (snapshot-time a)))
                          (dx (- (snapshot-value b) (snapshot-value a))))
                      (return (+ (snapshot-value a)
                                 (* (- time (snapshot-time a)) (/ dx dt))))))))
            (* value (expt (1+ (* k distance)) (- a)))))
      (sources process)))))