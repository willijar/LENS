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
            `((,(make-coord 10.0 10.0) (0.0 . 30.5) (5.0 . 45) (12.0 . 7.3)))))
  (:default-initargs :description "fire")
  (:metaclass module-class))

(defmethod measure((process scenario-physical-process)
                   (measurand (eql 'temperature))
                   (location coord)
                   time)
  (let ((k (slot-value process 'k))
        (a (slot-value process 'a)))
    (reduce
     #'+
     (sources process)
     :key
     #'(lambda(source)
          (let ((distance (distance location (car source)))
                (value
                 (loop :for r :on (rest source)
                    :for a = (car r)
                    :until (not (cdr r))
                    :finally (return 0)
                    :when (and (>= time (car a)) (< time (car (cadr r))))
                    :do
                    (progn
                    (let* ((b (cadr r))
                           (dt (- (car b) (car a)))
                           (dx (- (cdr b) (cdr a)))
                           (coeff (/ (- time (car a)) dt)))
                      (return (+ (cdr a) (* coeff dx)) ))))))
            (* value (expt (1+ (* k distance)) (- a))))))))
