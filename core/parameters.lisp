;; Metaclass for component classes that take parameters
;; Copyright (C) 2013 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of LENS

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; Parameters are read at point of objecvt creation - object must
;; therefore have full-path available at this point to know its
;; address in heirarchy.

;; Note properties are also covered in ths base
;; class as parameters can have properties as can all objects which
;; can take parameters.

;;; Code:

(in-package :lens)

(defgeneric configuration(instance)
  (:documentation "Return the parameter source associated with an instance"))

(defgeneric read-parameter(full-path source format)
  (:documentation "Actually read a fully named parameter from source
  using specified format")
  (:method((full-path list) source format)
    (case (first (last full-path))
      (b
       (values (format nil "Debug-read ~A ~A" format full-path) t))
      (d (values '(random 10.0) t))))
  (:method((full-path list) (trie trie) format)
    (multiple-value-bind(txt found-p) (trie-match full-path trie)
      (values (when found-p (dfv:parse-input format txt)) found-p))))

(defgeneric format-from-type(type)
  (:documentation "Given a type declaration return a format declaration")
  (:method((type list)) (first type))
  (:method(type) type)
  (:method((type (eql 'time-type))) 'time-period))

(defgeneric finalize-parameters(entity)
  (:documentation "May be overwridden to perform additional checks")
  (:method(entity) (declare (ignore entity))))
;; TODO - diferentiate between default values and initvalues for parameter slots
;; ensure do not initialise parameter slots from default values before checking
;; parameter source - then if not inited from parameter source reinit from slot
;; i.e. :initform for paramaters should be default value.

(defclass parameter-slot()
  ((properties :initform nil
               :initarg :properties :reader slot-definition-properties
               :documentation "plist of properties for this parameter")))

(defmethod initialize-instance :after
    ((slot parameter-slot) &key parameter &allow-other-keys)
  (declare (ignore parameter)))

(defgeneric slot-definition-format(slot)
  (:documentation "Return the parse/writing format for a given slot")
  (:method((slot parameter-slot))
    (let ((format (getf (slot-definition-properties slot) :format)))
      (or
       (when (subtypep (slot-definition-type slot) 'number)
          (let ((units  (getf (slot-definition-properties slot) :units)))
            (when (and units (not format))
              `(dfv:eng :units units))))
       format
       (format-from-type (slot-definition-type slot))))))

(defun property-union(list1 list2)
  "Returns a merged property list combining properties from list1 and
list2. list1 property will have priority excepti if the property
values are themselves a list in which case the result is list2 value
appended onto end of list1 value"
  (let ((result (copy-list list2)))
    (loop :for a :on list1 :by #'cddr
       :for k = (car a)
       :for v1 = (cadr a)
       :for v2 = (getf list2 k)
       :when v1
       :do (setf (getf result k)
                 (if (and (listp v1) (listp v2))
                     (append v1 v2)
                     v1)))
    result))

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
  ((properties :type list :reader properties
               :documentation "The properties for this class"))
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
  (let ((initargs (call-next-method))
        (slot (first direct-slot-definitions)))
    (cond
      ((typep slot 'parameter-direct-slot-definition)
       (setf (getf initargs :parameter) t)
       (setf (getf initargs :properties)
             (reduce #'property-union
                 (mapcan
                  #'(lambda(slot)
                      (when (typep slot 'parameter-slot)
                        (list (slot-definition-properties slot))))
                  direct-slot-definitions)))
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
  ())

(defmethod properties((obj parameter-object))
  (properties (class-of obj)))

(defmethod read-parameter((obj parameter-object) name format)
  (read-parameter (nconc (full-path obj) (list name))
                  (configuration obj)
                  format))

(defmethod read-parameter((obj parameter-object) (path list) format)
  (read-parameter (nconc (full-path obj) path)
                  (configuration obj)
                  format))

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

(defmethod initalize-instance :after ((class parameter-class)
                                      &key direct-superclasses
                                      properties
                                      &allow-other-keys)
  (dolist(slot (class-direct-slots class))
    (when (slot-definition-volatile slot)
      (dolist(gf (slot-definition-readers slot))
        ;; not ideal but MOP use of add-method etc not well documented
        (eval
         `(defmethod :around ,(generic-function-name gf)
            ((entity ,(class-name class)))
            (funcall (call-next-method)))))))
  ;; deal with property inheritance
  (setf (slot-value class 'properties)
        (reduce #'property-union
                (cons properties
                      (mapcan #'(lambda(super)
                                  (when (typep super class)
                                    (list (properties class))))
                              direct-superclasses)))))

(defgeneric configure(entity config &optional slot-names all-keys)
  (:documentation "Configure an objects slots from configuration data")
  (:method((object parameter-object)  config
           &optional (slot-names t) (all-keys nil))
    (let* ((full-path (full-path object))
           (full-path-last (last full-path)))
    (dolist(slot (class-slots (class-of object)))
      (when (typep slot 'parameter-slot)
        (let ((slot-initargs (slot-definition-initargs slot))
              (slot-name (slot-definition-name slot)))
          (when (and (or (eql slot-names 't) (member slot-name slot-names))
                     (not (get-properties all-keys slot-initargs)))
            (rplacd full-path-last slot-name)
            (multiple-value-bind(value read-p)
                (read-parameter full-path (configuration object)
                                (slot-definition-format slot))
              (cond
                (read-p
                 (setf (slot-value object slot-name)
                   (if (and (not (functionp value))
                            (typep slot 'parameter-volatile-effective-slot-definition))
                       #'(lambda() (eval value))
                       value)))
                ((and (typep slot 'parameter-volatile-effective-slot-definition)
                      (slot-definition-initfunction slot))
                 (setf (slot-value object slot-name)
                       (slot-definition-initfunction slot))))))))))))

(defmethod shared-initialize :before ((object parameter-object) slot-names
                                      &rest all-keys &key config)
  (declare (dynamic-extent all-keys config))
  ;; read parameters first if no key specified to initialise parameter
  ;; if no key specified for parameter read it from config file
  (configure object config slot-names all-keys))

(defmethod initialize-instance :after ((object parameter-object)
                                       &key &allow-other-keys)
  (finalize-parameters object))


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


