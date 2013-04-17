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
  ;;(declare (ignore initargs))
  (format t "slot-names=~A~%initargs=~A~%" slot-names initargs)
  (dolist(slot-definition (class-slots (class-of instance)))
    (when (and (typep slot-definition 'parameter-effective-slot-definition)
               (not (typep slot-definition
                           'parameter-volatile-effective-slot-definition))
               (or (eql slot-names t)
                   (member (slot-definition-name slot-definition) slot-names)))
      (multiple-value-bind(v defined-p)
          (parameter-slot-read slot-definition instance)
        (when defined-p
          (setf (slot-value instance (slot-definition-name slot-definition))
                (eval v)))))))

(defmethod parameter-slot-read((slot parameter-effective-slot-definition)
                               instance)
  (read-parameter (concatenate 'string (full-path instance) "."
                               (string (slot-definition-name slot)))
                  (parameter-source instance)
                  (slot-definition-format slot)))

(defmethod parameter-source((instance component))
  *simulation-configuration*)

