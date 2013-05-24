(in-package :lens)

(defclass lens-object()
  ()
  (:documentation "Lightweight base class."))

(defclass named-object(lens-object)
  ((name :type symbol :initarg :name :accessor name
         :documentation "Name of this object")))

(defclass owned-object(named-object)
  ((owner :initarg :owner :type named-object :initform nil
          :accessor owner)))

(defgeneric index(owned-object)
  (:documentation "In in an array of objects return the index")
  (:method((o owned-object)) nil))

(defgeneric parent-module(object)
  (:documentation "Return the parent module of this object - not always owner")
  (:method((entity owned-object)) (owner entity)))

(defgeneric full-name(o)
  (:documentation "When this object is part of a vector (like a
submodule can be part of a module vector, or a gate can be part of a
gate vector), this method returns the object's name with the index in
brackets;")
  (:method((o named-object))
    (cons (name o)
          (let ((i (index o))) (when i (list i))))))


(defgeneric full-path(o)
  (:documentation "Returns the full path of the object in the object
hierarchy, like '(net host 2 tcp winsize)'.")
  (:method((o named-object))
    "if there is an owner object, this method returns the owner's fullPath
     plus this object's fullName, separated by a dot; otherwise it simply
     returns full-name."
    (nconc (full-path (parent-module o)) (full-name o))))

(defun full-path-string(o)
  (format nil "~{~A~^.~}" (rest (full-path o))))

(defgeneric for-each-child(parent operator)
  (:documentation "Enables traversing the object tree, performing some
  operation on each object.")
  (:method((parent sequence) (operator function))
    (map 'nil operator parent)))

(defgeneric info(o)
  (:documentation "Produce a one-line description of object.
The string appears in the graphical user interface (Tkenv) e.g. when
the object is displayed in a listbox. The returned string should
possibly be at most 80-100 characters long, and must not contain
newline.")
  (:method((o named-object)) (format nil "~{~A~^.~}" (rest (full-path o)))))

(defmethod print-object((obj named-object) stream)
  (print-unreadable-object(obj stream :identity t :type t)
    (format stream "~A~@[[~A]~]" (first (full-name obj))
            (second (full-name obj)) )))

(defgeneric detailed-info(o)
  (:documentation  "Return detailed, multi-line, arbitrarily long
description of the object. The string appears in the graphical
user interface (Tkenv) together with other object data (e.g. class name)
wherever it is feasible to display a multi-line string.")
  (:method(o) (info o)))

(defgeneric duplicate(object &optional duplicate)
  (:documentation "Should be redefined in subclasses to create an
  exact copy of this object. The default implementation just throws an
  error, to indicate that the method was not redefined. The second
  argument, if defined should be the instance that the object is being
  duplicated into.")
  (:method(o &optional duplicate)
    (declare (ignore duplicate))
    (error "The duplicate method is not defined for class ~S"
           (class-name (class-of o))))
  (:method((entity named-object) &optional duplicate)
    (when duplicate
      (copy-slots '(name) entity duplicate))))

(defgeneric serialise(o stream)
  (:documentation "Serialise an object into a stream")
  (:method :around ((o standard-object) (os stream))
      (write (class-name (class-of o)) :stream os)
      (call-next-method o os))
  (:method(o (os stream))
    (write o :stream os :readably t)))

(defmethod info((sequence sequence))
  (format nil "n=~D" (length sequence)))

(defgeneric find-object(parent name &optional deep)
  (:documentation "Finds the object with the given name. This function
  is useful when called on subclasses that are containers. This method
  finds the object with the given name in a container object and
  returns a pointer to it or NULL if the object has not been found. If
  deep is false, only objects directly contained will be searched,
  otherwise the function searches the whole subtree for the object. It
  uses the for-each-child() mechanism.")
  (:method (parent name &optional (deep t))
    (for-each-child
     parent
     #'(lambda(child)
         (when (equal name (name child))
           (return-from find-object child))
         (when deep
           (let ((found (find-object child name deep)))
             (when found (return-from find-object found))))))))