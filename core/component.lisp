;; Metaclass for component classes that take paramaters
;; Copyright (C) 2013 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of LENS

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :lens)

(defvar *simulation-configuration*)

(defgeneric parameter-source(instance)
  (:documentation "Return the parameter source associated with an instance"))

(defgeneric read-parameter(full-path source format)
  (:documentation "Actually read a fully named parameter from source
  using specified format"))

(defgeneric parameter-slot-read(slot instance)
  (:documentation "Read a paramater slot for a paramater-class instance"))

(defgeneric format-from-type(type)
  (:documentation "Given a type declaration return a format declaration")
  (:method((type list)) (first type))
  (:method(type) type))

;; TODO - diferentiate between default values and initvalues for paramater slots
;; ensure do not initialise paramater slots from default values before checking
;; paramater source - then if not inited from paramater source reinit from slot
;; i.e. :initform for paramaters should be default value.

(defclass parameter-slot()
  ((format :initarg :format :initform nil :reader slot-definition-format
           :documentation "Format for reading/writing parameter")))

(defmethod initialize-instance :after ((slot parameter-slot) &key parameter type &allow-other-keys)
  (declare (ignore parameter))
  (unless (slot-boundp slot 'format)
    (setf (slot-value slot 'format)
          (format-from-type type))))

(defclass parameter-direct-slot-definition
    (standard-direct-slot-definition parameter-slot)
  ((volatile :initform nil :initarg :volatile :type boolean
             :reader slot-definition-volatile
             :documentation "If true parameter is reread every time it
             is needed during simulation")))

(defclass parameter-effective-slot-definition
    (standard-effective-slot-definition parameter-slot)
  ())

(defclass parameter-volatile-effective-slot-definition(parameter-effective-slot-definition)
  ())

(defclass parameter-class(standard-class)
  ()
  (:documentation "Metaclass for parsed objects"))

(defmethod direct-slot-definition-class ((class parameter-class)
                                         &rest initargs)
  (if (getf initargs :parameter)
      (find-class 'parameter-direct-slot-definition)
      (call-next-method)))

(defmethod sb-pcl::compute-effective-slot-definition-initargs
    ((class parameter-class) direct-slot-definitions)
  (let ((initargs (call-next-method))
        (slot (first direct-slot-definitions)))
    (when (typep slot 'parameter-direct-slot-definition)
        (setf (getf initargs :parameter) t)
        (setf (getf initargs :format) (slot-definition-format slot))
        (setf (getf initargs :volatile) (slot-definition-volatile slot)))
    initargs))

(defmethod effective-slot-definition-class
    ((class parameter-class) &rest  initargs)
   (if (getf initargs :parameter)
       (if (getf initargs :volatile)
           (find-class 'parameter-volatile-effective-slot-definition)
           (find-class 'parameter-effective-slot-definition))
       (call-next-method)))

(defmethod sb-mop:validate-superclass ((class parameter-class)
                                       (superclass standard-class))
  t)

(defmethod slot-value-using-class
    ((class parameter-class) instance
     (slot parameter-volatile-effective-slot-definition))
  (multiple-value-bind(v defined-p)
          (parameter-slot-read slot instance)
    (cond
      (defined-p (eval v))
      ((slot-definition-initfunction slot)
       (funcall (slot-definition-initfunction slot)))
      ((slot-definition-initform slot))
      ((slot-unbound class instance (slot-definition-name slot))))))

(defmethod slot-boundp-using-class
    ((class parameter-class) instance
     (slot-definition parameter-effective-slot-definition))
  (multiple-value-bind(v v-p)
      (parameter-slot-read slot-definition instance)
    (declare (ignore v))
    (or v-p (call-next-method))))

(defclass parameter-object()
  ()
  (:metaclass parameter-class))

(defmethod shared-initialize :after ((instance parameter-object) slot-names
                                     &rest initargs &key &allow-other-keys)
  (format t "slot-names=~A~%initargs=~A~%" slot-names initargs)
  ;; we read parameter values from source for parameter slots with no initargs
  ;; only. Read value will overwrite initform value which behaves as fallback
  ;; default
  (dolist(slot-definition (class-slots (class-of instance)))
    (when (and (typep slot-definition 'parameter-effective-slot-definition)
               (not (typep slot-definition
                           'parameter-volatile-effective-slot-definition))
               (or (eql slot-names t)
                   (member (slot-definition-name slot-definition) slot-names))
               (every #'(lambda(arg) (eql (getf arg initargs 'none) 'none))
                      (slot-definition-initargs slot-definition)))
      (multiple-value-bind(v defined-p)
          (parameter-slot-read slot-definition instance)
        (when defined-p
          (setf (slot-value instance (slot-definition-name slot-definition))
                (eval v)))))))

(defmethod parameter-slot-read((slot parameter-effective-slot-definition)
                               instance)
;; need to change between class allocation using class names and instance names
;; using instance name!!
  (read-parameter (concatenate 'string (full-path instance) "."
                               (string (slot-definition-name slot)))
                  (parameter-source instance)
                  (slot-definition-format slot)))

(defclass property-object()
  ((properties :initform nil :initarg :properties
               :documentation "plist of plists"))
  (:documentation "An object which can properties associated with them"))

(defgeneric property(key instance)
  (:documentation "Return a named property list for instance")
  (:method((key symbol) (instance property-object))
    (getf key (slot-value instance 'properties))))

(defclass signal-record
    ((signalID :initarg :id :initform 0 :reader signalID
               :type integer :documentation "Unique signal id")
     (listeners :type array :initform (make-array 0) :reader listeners)))

(defclass component(named-object object-array property-object parameter-object)
  ((signal-table :type (array signal-record *) :initform (make-array)
                 :reader signal-table)
   (signal-has-local-listeners
    :type bit-vector
    :initform (make-array 64 :element-type 'bit)
    :reader signal-has-local-listeners
    :documentation "bit[k]==1: signalID k has local listeners")
   (signal-has-ancestor-listeners
    :type bit-vector
    :initform (make-array 64 :element-type 'bit)
    :reader signal-has-ancestor-listeners
    :documentation "bit[k]==1: signalID k has listener in any ancestor component")
   (signalIDs :type hash-table :initform (make-hash-table :test 'equal)
              :reader signalIDs
              :documentation "Mapping name to signal ids")
   (signalNames :type hash-table :initform (make-hash-table)
                :reader signalNames
                :documentation "Mapping signal id to name")
   (lastSignalID :initform 0 :allocation :class)
   (rngMapping :initform #() :paramater t :reader rngMapping
               :documentation "Mapping from module local rng to global rng"))
  (:documentation "Base class for all component types - modules & channels")
  (:metaclass parameter-class))

(defun RNG(k instance)



(defmethod parameter-source((instance component))
  *simulation-configuration*)

