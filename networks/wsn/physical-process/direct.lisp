(in-package :lens.wsn)

(defclass direct-node-physical-process(physical-process)
  ((default-value :initform 0.0 :parameter t :type real)
   (assigned-values :type read :initform nil :parameter t
                    :documentation "Assigned values in a range specification"))
  (:metaclass module-class)
  (:documentation "Physical process where value is assigned per node"))

(defmethod measure((instance direct-node-physical-process)
                   measurand location time)
  (declare (ignore measurand location time))
  (with-slots(default-value assigned-values) instance
    (if (range-list-p assigned-values)
        (or (range-getf assigned-values (nodeid (node *context*)))
            default-value)
        default-value)))

