(in-package :lens)

(defclass paramater(named-object)
  ((type :initform integer :initarg :type :reader parameter-type)
   (value)
   (default-value :initform 0 :initarg :default)
   (volatile-p :type boolean :initform nil :initarg :volatile :reader volatile-p)))

(defclass parameter-class(standard-class)
  ((parameter-definitions :initform nil :initarg :paramaters
                          (closer-mop::
)
  (:documentation "Metaclass for parsed objects"))

(defclass object-with-parameters(named-object)
  ((paramater-definitions
  ((parameters :type list :initform nil)))

(defgeneric parameter-definitions(object)
  (:documentation "Return a list of parameter definitions for an object")
  (:method-combination list))

(defmethod initialize-instance :after ((object object-with-parameters)
                                       &key &allow-other-keys)
  (setf (slot-value object 'parameters)
        (mapcan
         #'(lambda(defs)
             (mapcan
              #'(lambda(def)
                  (list
                   (make-instance 'parameter
                                  :name (first def)
                                  :default-value (second def)
                                  :type (getf (cddr def) :type)
                                  :volatile (getf (cddr def) :volatile))))


(defgeneric parameter(name object)
  (:documentation "Return the named parameter value for object"))


  (defstruct


(defclass lens-object()
  ()
  (:documentation "Lightweight base class which it is recommended to use for any class for which generic functions are specialised. May own other object but owned objects must be subclassed from owned-object."))

(defclass owned-object(lens-object)
  ((owner :accessor owner :type lens-object :initform nil)))

(defclass object-array(owned-object)
  ((vec :type array
        :initform (make-array 2 :element-type owned-object
                              :adjustable t)))
  (take-ownership :type boolean :initform t :initarg :take-ownership
                  :accessor take-ownership))

(defvar *default-owner* (make-instance 'object-array))
1
(defgeneric default-owner((object owned-object)) *default-owner*)

(defgeneric yield-ownership(owner object new-owner)
  (:documentation "Internal")
  (:method((owner lens-object) (object owned-object) new-owner)
    (error "~A is currently in ~A, it cannot be inserted into ~A"
    object owner new-owner)))

(defgeneric name(o)
  (:documentation "return object's name. It should never return NIL.")
  (:method(o)
    "This default implementation just returns an empty string"
    ""))

(defgeneric full-name(o)
  (:documentation "When this object is part of a vector (like a submodule can be part of a module vector, or a gate can be part of a gate vector), this method
returns the object's name with the index in brackets;")
  (:method(o)
    "Default implementation just returns name"
    (name o)))

(defgeneric full-path(o)
  (:documentation "Returns the full path of the object in the object hierarchy,
like 'net.host[2].tcp.winsize'.")
  (:method(o)
    "if there is an owner object, this method returns the owner's fullPath
     plus this object's fullName, separated by a dot; otherwise it simply
     returns full-name."
    (let ((owner (owner o)))
      (if owner
          (concatenate 'string (full-path owner) "." (full-name o))
          (full-name o)))))

(defgeneric info(o)
  (:documentation "Produce a one-line description of object.
The string appears in the graphical user interface (Tkenv) e.g. when
the object is displayed in a listbox. The returned string should
possibly be at most 80-100 characters long, and must not contain
newline.")
  (:method(o) (full-path o)))

(defmethod print-object((obj lens-object) stream)
  (print-unreadable-object(obj stream :key t :type t)
    (format stream "~A ~A" (full-name obj) (info obj))))

(defgeneric detailed-info(o)
  (:documentation  "Return detailed, multi-line, arbitrarily long
description of the object. The string appears in the graphical
user interface (Tkenv) together with other object data (e.g. class name)
wherever it is feasible to display a multi-line string.")
  (:method(o) (info o)))

(defgeneric dup(o)
  (:documentation "Should be redefined in subclasses to create an
  exact copy of this object. The default implementation just throws an
  error, to indicate that the method was not redefined.")
  (:method(o)
    (error "The dup method is not defined for class ~S"
           (class-name (class-of o)))))

(defgeneric take(object owned-object)
  (:documentation "Makes this object the owner of 'object'.
The function called by the container object when it takes ownership
of the obj object that is inserted into it.")
  (:method :before ((object lens-object) (owned-object owned-object))
    (yield-ownership (owner owned-object) owned-object object))
  (:method ((object lens-object) (owned-object owned-object))))

(defgeneric drop(parent child)
  (:documentation "Releases ownership of `object', giving it back to
its default owner. The function called by the container object when
obj is removed from the container -- releases the ownership of the
object and hands it over to its default owner.")
  (:method :before ((object lens-object) (owned-object owned-object))
    (unless (eql (owner owned-object) object)
      (error "drop(): not owner of object (~A)" owned-object)))
  (:method ((object lens-object) (owned-object owned-object))
    (do-insert child (default-owner child))))

(defgeneric serialise(o stream)
  (:documentation "Serialise an object into a stream")
  (:method :around ((o standard-object) (os stream))
      (write (class-name (class-of o)) os)
      (call-next-method o os))
  (:method(o (os stream))
    (write o os :readably t)))

(defgeneric do-insert(object sequence)
  (:method((obj owned-object) (sequence object-array))
    (assert (not (eql obj sequence)))
    (vector-push object (slot-value sequence 'vec))))

(defmethod take((sequence object-array) (object owned-object))
  (do-insert object sequence))

(defmethod drop((sequence object-array) (object owned-object))
  (yield-ownership sequence object (default-owner object))
  (do-insert object (default-owner object)))

(defmethod yield-ownership((sequence object-array) (object owned-object) new-owner)
  (assert (eql (owner object) sequence))
  (setf (owner object) sequence)
  (let ((p (position object sequence))
        (v (slot-value sequence 'vec)))
    (setf (aref v p) object)
    (vector-pop v)))

(defmethod info((sequence object-array))
  (format nil "n=" (array-dimension (slot-value sequence 'vec) 0)))

(defmethod for-each-child((sequence object-array) operator)
  (for-each-child (slot-value sequence 'vec) operator))

(defclass named-object(lens-object)
  ((name :type string :initform "" :reader name :initarg :name)))

(defgeneric owner(child)
  (:documentaion " May be redefined to return an owner or parent object.")
  (:method(o) nil))

(defgeneric for-each-child(parent operator)
  (:documentation "Enables traversing the object tree, performing some operation on each object.")
  (:method((parent sequence) (operator function))
    (map 'nil operator parent)))

(defgeneric find-object(parent name &optional (deep t))
  (:documentation "Finds the object with the given name. This function
  is useful when called on subclasses that are containears. This method
  finds the object with the given name in a container object and
  returns a pointer to it or NULL if the object has not been found. If
  deep is false, only objects directly contained will be searched,
  otherwise the function searches the whole subtree for the object. It
  uses the forEachChild() mechanism.")
  (for-each-child
   parent
   #'(lambda(child)
       (when (equal name (name child))
         (return-from find-object child))
       (when deep
         (let ((found (find-object child name deep)))
           (when found (return-from find-object found)))))))

(defgeneric do-insert(object sequence)
  (:method((obj owned-object) (sequence object-array))
    (assert (not (eql obj sequence)))
    (vector-push object (slot-value sequence 'vec))))

(defmethod take((sequence object-array) (object owned-object))
  (do-insert object sequence))

(defmethod drop((sequence object-array) (object owned-object))
  (yield-ownership sequence object (default-owner object))
  (do-insert object (default-owner object)))

(defmethod yield-ownership((sequence object-array) (object owned-object) new-owner)
  (assert (eql (owner object) sequence))
  (setf (owner object) sequence)
  (let ((p (position object sequence))
        (v (slot-value sequence 'vec)))
    (setf (aref v p) object)
    (vector-pop v)))

(defmethod info((sequence object-array))
  (format nil "n=" (array-dimension (slot-value sequence 'vec) 0)))

(defmethod for-each-child((sequence object-array) operator)
  (for-each-child (slot-value sequence 'vec) operator))