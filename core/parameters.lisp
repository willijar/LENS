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
  using specified format")
  (:method(full-path source format)
    (case (first (last full-path))
      (b
       (values (format nil "Debug-read ~A ~A" format full-path) t))
      (d (values '(random 10.0) t))))
  (:method(full-path (source trie) format)
    (multiple-value-bind(txt found-p) (trie-match full-path trie)
      (values (when found-p (dfv:parse-input format txt)) found-p))))

(defgeneric parameter-slot-read(slot instance config)
  (:documentation "Read a parameter slot for a parameter-class
  instance from a configuration"))

(defgeneric format-from-type(type)
  (:documentation "Given a type declaration return a format declaration")
  (:method((type list)) (first type))
  (:method(type) type))

;; TODO - diferentiate between default values and initvalues for parameter slots
;; ensure do not initialise parameter slots from default values before checking
;; parameter source - then if not inited from parameter source reinit from slot
;; i.e. :initform for paramaters should be default value.

(defclass parameter-slot()
  ((format :initarg :format :reader slot-definition-format
           :documentation "Format for reading/writing parameter")))

(defmethod initialize-instance :after ((slot parameter-slot)
                                       &key parameter type &allow-other-keys)
  (declare (ignore parameter))
  (unless (slot-boundp slot 'format)
    (setf (slot-value slot 'format) (format-from-type type))))

;; The directs slot type identifies whether this should be a volatile slot
(defclass parameter-direct-slot-definition
    (standard-direct-slot-definition parameter-slot)
  ((volatile :initform nil :initarg :volatile :type boolean
             :reader slot-definition-volatile
             :documentation "If true parameter is reread every time it
             is needed during simulation")))

;; We use different types of effective slots for volatile or non-volatile
;; Volatile slots will always contain a function to return the value
(defclass parameter-effective-slot-definition
    (standard-effective-slot-definition parameter-slot)
  ())

(defclass parameter-volatile-effective-slot-definition
    (parameter-effective-slot-definition)
  ())


(defclass parameter-class(standard-class)
  ()
  (:documentation "Metaclass for classes which have slots initialised
  from an external source"))

(defmethod direct-slot-definition-class ((class parameter-class)
                                         &rest initargs)
  "If we specify either :parameter or :volatile in a slot definition then
it is a parameter direct slot"
  (if (or (getf initargs :parameter) (getf initargs :volatile))
      (find-class 'parameter-direct-slot-definition)
      (call-next-method)))

(defmethod sb-pcl::compute-effective-slot-definition-initargs
    ((class parameter-class) direct-slot-definitions)
  "Only if the highest priority direct slot is a parameter slot do we need
any parameter initargs - they aren't inherited."
;; TODO if volatile add specific slot-value method for this class and slot-name
  (let ((initargs (call-next-method))
        (slot (first direct-slot-definitions)))
    (cond
      ((typep slot 'parameter-direct-slot-definition)
       (setf (getf initargs :parameter) t)
       (setf (getf initargs :format) (slot-definition-format slot))
       (when (slot-definition-volatile slot)
         (setf (getf initargs :volatile) t)
         (setf (getf initargs :type) 'function)))
      (t
       (remf initargs :parameter)))
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

(defclass parameter-object()
  ()
  (:metaclass parameter-class))

;; as per Pascal Constanza ensure parameter-object is default direct superclass
(defmethod initialize-instance :around
    ((class parameter-class) &rest initargs
     &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for superclass in direct-superclasses
         thereis (subtypep superclass 'parameter-object))
      (call-next-method)
      (apply #'call-next-method class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'parameter-object)))
             initargs)))

(defmethod reinitialize-instance :around
    ((class parameter-class) &rest initargs
     &key (direct-superclasses () direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      (if (loop for superclass in direct-superclasses
             thereis (subtypep superclass 'parameter-object))
          (call-next-method)
          (apply #'call-next-method class
                 :direct-superclasses
                 (append direct-superclasses
                         (list (find-class 'parameter-object)))
                 initargs))
      (call-next-method)))

(defgeneric configure(entity  config &optional slot-names all-keys)
  (:method((object parameter-object)  config
           &optional (slot-names t) (all-keys nil))
    (dolist(slot (class-slots (class-of object)))
      (when (typep slot 'parameter-slot)
        (let ((slot-initargs (slot-definition-initargs slot))
              (slot-name (slot-definition-name slot)))
          (when (and (or (eql slot-names 't) (member slot-name slot-names))
                     (not (get-properties all-keys slot-initargs)))
            (multiple-value-bind(value read-p)
                (parameter-slot-read slot object config)
              (cond
            (read-p
              (setf (slot-value object slot-name) value))
            ((and (typep slot 'parameter-volatile-effective-slot-definition)
                  (slot-definition-initfunction slot))
              (setf (slot-value object slot-name)
                    (slot-definition-initfunction slot)))))))))))

(defmethod shared-initialize :before ((object parameter-object) slot-names
                                      &rest all-keys &key config)
  (declare (dynamic-extent all-keys config))
  ;; read parameters first if no key specified to initialise parameter
  ;; if no key specified for parameter read it from config file
  (configure object config slot-names all-keys))

(defgeneric volatile-slot-value(entity alot-name)
  (:documentation "slot-value for volatile slots - ensure context is object")
  (:method((entity parameter-object) (slot-name symbol))
    (let ((*context* entity))
      (funcall (slot-value entity slot-name)))))

;; this would be semantically what shared-intialize does
;; (defmethod shared-initialize
;;   (declare (dynamic-extent all-keys))
;;   (loop
;;      :for slot :in (class-slots (class-of object))
;;      :for slot-initargs = (slot-definition-initargs slot)
;;      :for slot-name = (slot-definition-name slot)
;;      ;; first we initalise using initargs if specified
;;      :when slot-initargs :do
;;      (multiple-value-bind
;;            (indicator value)
;;          (get-properties all-keys slot-initargs)
;;        (when indicator
;;          (setf (slot-value object slot-name)
;;                value)))
;;      ;; then we check for initforms and use them if
;;      :when (and (not (slot-boundp object slot-name))
;;                 (or (eq slot-names 't) (member slot-name slot-names))) :do
;;      (let ((slot-initfunction (slot-definition-initfunction slot)))
;;        (when (typep slot 'parameter-slot) ;; look up value in parameter slot
;;          (multiple-value-bind(value found-p)
;;              (parameter-slot-read slot object)
;;            (when found-p
;;              (setf (slot-value object slot-name) value))))
;;        ;; default if ordinary slot or no parameter
;;        (when (and slot-initfunction  (not (slot-boundp object slot-name)))
;;          (setf (slot-value object slot-name)
;;                (funcall slot-initfunction)))))
;;   object)

(defmethod parameter-slot-read((slot parameter-effective-slot-definition)
                               instance config)
;; need to change between class allocation using class names and instance names
;; using instance name!!
  (multiple-value-bind(value read-p)
      (read-parameter
       (append (full-path instance) (list (slot-definition-name slot)))
       (parameter-source instance)
       (slot-definition-format slot))
  (when read-p
    (values
     (if (and (not (functionp value))
              (typep slot 'parameter-volatile-effective-slot-definition))
         #'(lambda() (eval value))
        value)
    t))))

;; (defclass property-object()
;;   ((properties :initform nil :initarg :properties
;;                :documentation "plist of plists"))
;;   (:documentation "An object which can properties associated with them"))

;; (defgeneric property(key instance)
;;   (:documentation "Return a named property list for instance")
;;   (:method((key symbol) (instance property-object))
;;     (getf key (slot-value instance 'properties))))
