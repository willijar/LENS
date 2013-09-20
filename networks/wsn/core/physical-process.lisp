(in-package :lens.wsn)

(defgeneric measure(physical-process measurand location time)
  (:documentation "Return the real value of specified measurand from a
  physical process at given time and location"))

(defclass physical-process(module)
  ((description :type string
                :parameter t :initarg :description :reader description
                :documentation "Description of physical process - default ")
   (function
    :parameter t :type function
    :initform #'(lambda(m c tm) (declare (ignore m c tm)) (uniform 0.0 1.0))
    :properties (:format (eval :type function))
    :reader physical-process-function
    :documentation "a real function of measurand, location and time"))
  (:metaclass module-class))

(defmethod configure :after((instance physical-process))
  (unless (slot-boundp instance 'description)
    (setf (slot-value  instance 'description)
          (format nil "~A[~D]" (class-name (class-of instance))
                  (index instance)))))

(defmethod measure :around ((physical-process physical-process)
                            measurand
                            location time)
  (let ((*context* physical-process))
    (call-next-method)))

(defmethod measure((physical-process physical-process)
                   (measurand (eql 'temperature))
                   (location coord) (time real))
    (funcall (physical-process-function physical-process)
             measurand location time))
