;; Base classes for all LENS objects
;; Copyright (C) 2014 Dr. John A.R. Williams

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

;; Provides objuect naiming and ownership as well as searching through a
;; network of objects and duplication.

;;; Code:

(in-package :lens)

(defclass lens-object()
  ()
  (:documentation "Lightweight base class for all LENS objects."))

(defclass named-object(lens-object)
  ((name :type symbol :initarg :name :accessor name
         :documentation "Name of this object - used when addressing
         the object internally or through simulation paramaters.")
   (index :initarg :index :reader index :type fixnum
          :documentation "Position in an object vector (if it is in an
          object array)")))

(defclass owned-object(named-object)
  ((owner :initarg :owner :type named-object :initform nil :accessor owner
          :documentation "Object which owns this in object heirarchy")))

(defgeneric parent-module(object)
  (:documentation "Return the parent module of this object - not always owner")
  (:method((entity owned-object)) (owner entity)))

(defgeneric full-name(o)
  (:documentation "When this object is part of a vector (like a
submodule can be part of a module vector, or a gate can be part of a
gate vector), this method returns the object's name with the index in
brackets;")
  (:method((o named-object))
    (cons (name o) (when (slot-boundp o 'index) (list (index o))))))

(defgeneric full-path(o)
  (:documentation "Returns the full path of the object in the object
hierarchy, like '(net host 2 tcp winsize)'. This path could be used as
an address to locate the object later in the network.

If there is an owner object, this method returns the owner's [[full-path]]
with this object's [[full-name]] appended, otherwise it simply
returns full-name.")
  (:method((o named-object))
    "."
    (nconc (full-path (parent-module o)) (full-name o))))

(defun full-path-string(o)
  "Convert a the [[full-path]] into a dotted strign format suitable as
a parameter adssress."
  (format nil "~{~A~^.~}" (rest (full-path o))))

(defgeneric for-each-child(parent operator)
  (:documentation "Enables traversing the object tree, performing some
  operation on each object. Tyhe default [[module]] and
  [[compound-module]] provide implementations that will recurse over
  gates, submodules and channels if stored in the usual
  way. Implementations may wish to overwrite if storing some
  subelements that may be considered as children differently.")
  (:method((parent sequence) (operator function))
    (map 'nil operator parent))
  (:method(dummy operator)
    (declare (ignore dummy operator))))

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
  duplicated into. By default this will be a new instance of the same
  class as the object to be duplicated.

For packets this does a shallow copy i.e. copies fields only and does
not recurse into the encapsulated packets.")
  (:method :around((object standard-object) &optional duplicate)
      (if duplicate
          (call-next-method)
          (call-next-method object (make-instance (class-of object)))))
  (:method(o &optional duplicate)
    (declare (ignore duplicate))
    (error "The duplicate method is not defined for class ~S"
           (class-name (class-of o))))
  (:method((entity named-object) &optional duplicate)
    (when duplicate
      (copy-slots '(name) entity duplicate))))

(defgeneric serialise(o stream)
  (:documentation "Serialise an object into a stream. Fur future use.")
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
  uses the [[for-each-child]] mechanism.")
  (:method (parent name &optional (deep t))
    (for-each-child
     parent
     #'(lambda(child)
         (when (equal name (name child))
           (return-from find-object child))
         (when deep
           (let ((found (find-object child name deep)))
             (when found (return-from find-object found))))))))
