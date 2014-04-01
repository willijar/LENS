;; Metaclass for component classes that take parameters
;; Copyright (C) 2013-2014 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; LENS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Classes based on the p-rameter-class metaclass allow use of the
;; :parameter and :volatile arguments in slot definitions to specify parameters
;; and whether they need to be evaluated on every access or once only

;; Parameters are read at point of object creation - instance must
;; therefore have full-path available at this point to know its
;; address in heirarchy.

;; Note properties are also covered in ths base class as parameters
;; can have properties as can all instances which can take parameters.
;; This allows for extensions such as display parameters etc.

;;; Code:

(in-package :lens)

(defgeneric configuration(instance)
  (:documentation "Return the configuration trie with an instance. By
  default this will be the configuration read at the start of the
  simulation.")
  (:method(instance)
    (declare (ignore instance))
    (configuration *simulation*)))

(defgeneric read-parameter(full-path source format)
  (:documentation "Loopup a fully named parameter from a configuration
  trie and convert from string to internal representation using
  [[parse-input]] with the specified format. If the format is a
  pathname it may be relative to the path of the source file from
  which the parameter was read.")
  (:method((full-path list) (trie trie) format)
    (multiple-value-bind(txt found-p source) (trie-match full-path trie)
      (when found-p
        (restart-case
            (values
             (dfv:parse-input format txt) found-p source)
           (ignore()
             :report (lambda(os) (format os "Invalid parameter
             format at ~A. ~A expected. Ignore and use default value."
             source format))
             (values nil))
           (use-value(v)
             :interactive (lambda() (format t "Enter a new value: ")
                                   (eval (read)))
             :report "Use a new value"
             (values v t)))))))

(defmethod read-parameter((full-path list) (trie trie)
                          (format (eql 'pathname)))
  (multiple-value-bind(p found-p source) (call-next-method)
    (if source
        (values (merge-pathnames p (parameter-source-pathname source))
                found-p
                source)
        (values p found-p source))))

(defgeneric format-from-type(type)
  (:documentation "Given a type declaration return a format declaration suitable for use in [[parse-input]] to covert a parameter string to internal representation.")
  (:method((type list)) (cons (format-from-type (first type)) (rest type)))
  (:method((type symbol))
    (if (subtypep type 'number)
        `(number-or-expression :type ,type)
        type))
  (:method((type (eql 'symbol)))
    `(symbol :convert ,#'string-upcase :package ,*package*))
  (:method((type (eql 'time-type))) type))

(defmethod parse-input((spec (eql 'time-type)) value &key &allow-other-keys)
    (coerce
     (handler-bind
         ((dfv::invalid-format
           #'(lambda(c)
               (declare (ignore c))
               (invoke-restart 'dfv::use-value nil))))
       (cond
         ((ignore-errors (parse-input 'dfv:eng value :units "s")))
         ((ignore-errors (parse-input 'number value)))
         ((multiple-value-bind(n p) (parse-integer value :junk-allowed t)
            (when (and n (= p (1- (length value))))
              (let ((lc (char value (1- (length value)))))
                (case lc
                  (#\s n)
                  (#\m (* n 60))
                  (#\h (* n 60 60))
                  (#\d (* n 24 60 60))
                  (t (dfv::invalid-format-error
                      spec value
                      "~A is not a recognized time unit" lc)))))))
         (t (parse-input 'number-or-expression value))))
     'time-type))

(defmethod parse-input((spec (eql 'coord)) value &key &allow-other-keys)
  (let ((coords
         (parse-input 'list value
                      :type '(number :coerce-to float)
                      :min-length 2 :max-length 3)))
    (make-coord (first coords) (second coords) (or (third coords) 0.0))))

(defmethod parse-input((spec (eql 'number-or-expression)) value
                       &key type &allow-other-keys)
  (let* ((expr (read-from-string value))
         (v (if (numberp expr) expr (eval expr))))
    (if type (coerce v type) v)))

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

(defgeneric slot-definition-parameter-name(slot)
  (:method((slot parameter-slot))
    (getf (slot-definition-properties slot) :parameter-name (slot-definition-name slot))))

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

(defmethod  slot-definition-format((slot parameter-volatile-effective-slot-definition))
  'read)

(defgeneric properties(instance)
  (:documentation "Return an a-list of properties associated with an
  instance of a [[parameter-object]]. These may be used to specify
  parameters that may be used outside the simulation itself such as
  statistics gathers for components or display properties etc. The may
  be specified on a per class or per instance basis with instance
  overriding class values."))

(defclass parameter-class(standard-class)
  ((properties :initform nil :type list :reader properties
               :documentation "The properties for this class"))
  (:documentation "Metaclass for classes which have slots initialised
  from an external parameter source.

* Additional class options

- properties :: an /alist/

The /properties/ class option specifies the default set of properties
all classes of this metaclass will take as a default. These are in
addiiton to the instance properties that may be specified. The
instance properties take precedence.

* Additional slot options

- parameter :: a =boolean=
- volatile :: a =boolean=
- properties :: an /alist/

If /parameter/ is true then this is a parameter slot and the value for
this slot will be initialised from the simulation configuration file
as a priority over the default value specified in the =:initform= slot
option. If /volatile/ is specified for a parameter slot then the
parameter will be evaluated upon every initialisation allowing random
initialisation for different instances. The /properties/ of a
parameter slot specify additional properties in an alist. By default
the following properties are currently understood.

- format :: specifies a format form to used in [[parse-input]] when reading the parameter - overrides the default reading format for the slot type.

"))

(defmethod direct-slot-definition-class ((class parameter-class)
                                         &rest initargs)
  "If we specify either :parameter or :volatile in a slot definition then
it is a parameter direct slot"
  (if (or (getf initargs :parameter) (getf initargs :volatile))
      (find-class 'parameter-direct-slot-definition)
      (call-next-method)))

(defmethod sb-pcl::compute-effective-slot-definition-initargs
    ((class parameter-class) direct-slot-definitions)
  "Only if the highest priority direct slot is a parameter slot do we
need any parameter initargs - they aren't inherited."
  (let ((initargs (call-next-method))
        (slot (first direct-slot-definitions))
        (parameter-slot-definitions
         (mapcan
          #'(lambda(slot)
              (when (typep slot 'parameter-slot)
                (list (slot-definition-properties slot))))
          direct-slot-definitions)))
    (cond
      (parameter-slot-definitions
       (setf (getf initargs :parameter) t)
       (setf (getf initargs :properties)
             (reduce #'property-union parameter-slot-definitions))
       (when (and (typep slot 'parameter-slot) (slot-definition-volatile slot))
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

(defmethod shared-initialize :after ((class parameter-class) slot-names
                                      &key direct-superclasses
                                      properties
                                   &allow-other-keys)
  (declare (ignore slot-names))
  ;; necessary so defmethod can find class
  (setf (find-class (class-name class)) class)
  ;; define around methods for volatile parameters that call
  ;; underlying function to obtain value.
  (dolist(slot (class-direct-slots class))
    (when (and (typep slot 'parameter-slot) (slot-definition-volatile slot))
      (dolist(gf (slot-definition-readers slot))
        (eval
         `(defmethod ,gf  :around
            ((entity ,(class-name class)))
            (let ((*context* entity))
              (funcall (call-next-method))))))))
  ;; deal with property inheritance from superclasses
  (setf (slot-value class 'properties)
        (nconc properties
              (mapcan #'(lambda(super)
                          (when (typep super 'parameter-class)
                            (copy-list (properties super))))
                      direct-superclasses))))

(defclass parameter-object()
  ((properties :initarg :properties :initform nil
               :documentation "Per instance property list"))
  (:metaclass parameter-class)
  (:documentation
"Base class for all components which can have slots initialoised from
parameters. See the [[parameter-class]] metaclass for the additional
slot options available and their affect. The =:properties= /initarg/
may be used to specify instance specific properties (see
[[properties]])." ))

(defmethod properties((obj parameter-object))
  (append (slot-value obj 'properties) (properties (class-of obj))))

(defmethod read-parameter(obj (name symbol) format)
  (read-parameter (nconc (full-path obj) (list name))
                  (configuration obj)
                  format))

(defmethod read-parameter(obj (path list) format)
  (read-parameter (nconc (full-path obj) path)
                  (configuration obj)
                  format))

(defgeneric configure(instance)
  (:documentation "Configure an instances unbound parameter slots from
  configuration data. Called after initialization from initargs but
  before initialization from initforms (defaults)")
  (:method((instance parameter-object))
    (let* ((full-path (copy-list (full-path instance)))
           (full-path-last (last full-path)))
    (dolist(slot (class-slots (class-of instance)))
      (when (typep slot 'parameter-slot)
        (let ((slot-name (slot-definition-name slot)))
          (when (not (slot-boundp instance slot-name))
            (setf (cdr full-path-last)
                  (list (slot-definition-parameter-name slot)))
            (multiple-value-bind(value read-p)
                (read-parameter full-path (configuration instance)
                                (slot-definition-format slot))
              (cond
                (read-p
                 (setf (slot-value instance slot-name)
                   (if (and (not (functionp value))
                            (typep slot 'parameter-volatile-effective-slot-definition))
                       #'(lambda() (eval value))
                       value)))
                ((and (typep slot 'parameter-volatile-effective-slot-definition)
                      (slot-definition-initfunction slot))
                 (setf (slot-value instance slot-name)
                       (slot-definition-initfunction slot))))))))))))

(defmethod shared-initialize((instance parameter-object) slotnames
                             &rest initargs &key &allow-other-keys)
  (declare (dynamic-extent initargs))
  (apply #'call-next-method instance (cons nil initargs))
  (configure instance)
  (call-next-method instance t))
