(in-package :lens.wsn)

(defclass no-mobility(mobility)
  ()
  (:metaclass module-class)
  (:default-initargs :static-p t)
  (:documentation "Just an alias for mobility for onsistency in
  parameter naming"))
