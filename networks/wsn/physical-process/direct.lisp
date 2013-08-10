(in-package :lens.wsn)

(defclass direct-node-physical-process(physical-process)
  ((default-value :initform 0.0 :parameter t :type real)
   (assigned-values :type list :initform nil :parameter t))
  (:metaclass module-class)
  (:documentation "Physical process where value is assigned per node"))

(defmethod measure((instance direct-node-physical-process)
                   measurand location time)
  (declare (ignore measurand location time))
  (with-slots(default-value values) instance
    (getf values (nodeid (node *context*)) default-value)))

