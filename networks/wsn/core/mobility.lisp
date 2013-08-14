(in-package :lens.wsn)

(defstruct orientation
  (phi 0.0 :type float :read-only t)
  (theta 0.0 :type float :read-only t))

(defmethod dfv::parse-input((spec (eql 'orientation)) value
                            &key &allow-other-keys)
  (let ((coords
         (dfv::parse-input 'list value
                      :type '(number :coerce-to float)
                      :min-length 2 :max-length 2)))
    (make-orientation :phi (first coords)
                      :theta (second coords))))

(defclass mobility(wsn-module)
  ((location :type coord :parameter t :initarg :location :reader location
             :documentation "current location initalized from parameter file")
   (orientation
    :type orientation :parameter t :initarg :orientation :reader orientation
    :initform (make-orientation)
    :documentation "current orientation initialized from parameter gile")
   (static-p :initform t :initarg :static-p :reader static-p))
  (:metaclass module-class))

(defgeneric (setf location)(location instance)
  (:documentation "Change location in mobility manager")
  (:method((location coord) (instance mobility))
    (assert (not (static-p instance))
            ()
            "Attempt to change location of static node ~A" (node instance))
    (tracelog "changed location to ~A" location)
    (setf (slot-value instance 'location) location)
    (emit instance 'node-move)
    location))

(defmethod initialize and ((instance mobility) &optional (stage 0))
  (case stage
    (0  (tracelog "initial location is ~A" (location instance))))
  t)