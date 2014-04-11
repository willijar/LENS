;; Some commonly used definitions and protocols
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

;;

(in-package :lens)

;; Basic types

(defconstant +c+ 299792458d0 "Speed of Light in m/sec")
*standard-output*
(defvar *simulation* nil "The global [[simulation]] instance")

(defparameter *context* nil
  "Current global component context in which evaluation is taking
  place. It is used to determine mapping for random number streams and
  for providing tracing context. This should be bound for the extent
  of exposed interfaces to components. The kernel binds it around
  the [[handle-message]] and [[initialize]] methods. ")

(defun sec(stream arg &optional colon-p at-p
                 (d 3) (padchar #\space) (exponentchar #\E))
  "* Arguments
- stream :: An output stream designator.
- arg :: time format argument
- colon-p :: ignored
- at-p :: ignored
- D :: number of digits to print after decimal point
- padchar :: character to print leading the output
- exponentchar :: character to print before exponent.

* Description

Simulation Time formatter function outputs a time argument =arg= to =stream=
in engineering format.

* Example
;;; (format nil \"~3/lens:sec/\" 0.5689)
;;; => \"568.900ms\"

* See also
- [[dfv:eng]]
"
  (declare (ignore colon-p at-p))
  (dfv:eng stream arg t t d padchar exponentchar)
   (write-char #\s stream))

(defvar *time-format* "~7/lens::sec/" "The time format control used
when tracing.")

;; base class for in simulation errors (not program errors)
(define-condition simulation-condition(condition)())

;; Some basic macros
(defmacro while (test &body body)
  "A while loop - repeat body while test is true"
  `(do ()
    ((not ,test))
    ,@body))

(defmacro until (test &body body)
  "Repeat body until test returns true"
  `(do ()
    (,test)
    ,@body))

(defmacro filter(test lst &key (key '#'identity))
  "* Arguments
- test :: a designator for a function of one argument which returns a
          generalised boolean
- lst :: a proper list
- key :: a designator for a function of one argument

* Returns

- result :: a list

* Description
Return a list of the elements in =lst= for which =test= (applied to =key=)
is true."
  `(mapcan #'(lambda(it)
               (when (funcall ,test (funcall ,key it))
                 (list it)))
    ,lst))



(defmacro for ((var start stop) &body body)
 "* Arguments
- var ::  a variable name (not evaluated)
- start :: an integer (evaluated)
- stop :: an integer (evaluated)
- body :: a list of +forms+

* Description

Iterate from the value supplied by =start= upto but not including the
value supplied by =end= setting =var= to each value in turn before
evaluating the +body+"
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
      ((>= ,var ,gstop))
      ,@body)))


(defun set-slots(instance defs)
"* Arguments
- instance :: a class instance
- defs :: a list of lists

* Description

Used to set multiple slot values in a class instance. defs is a list
of slot setting options. The first element of each option is the
slot name and the second element is the value to set the slot to."
  (dolist(def defs)
    (if (listp def)
        (setf (slot-value instance (first def)) (second def))
        (setf (slot-value instance def) nil))))

(defun copy-slots(slots source destination)
  "* Arguments
- slots :: a list of slot names
- source :: a class instance
- destination :: a class instance

* Returns
- destination :: a class instance

* Description
Copies named slot values shallowly from source to destination
returning the modifed destination object."
  (dolist(slot slots)
    (if (slot-boundp source slot)
        (setf (slot-value destination slot) (slot-value source slot))
        (slot-makunbound destination slot)))
  destination)

(defun reinitialise-slots(slot-names instance)
  "* Arguments
- slots-names :: a list of slot names
- source :: a class instance

* Description

Reset the names slots in instance to their initial values as defined
in the slot definitions for the instance class"
  (let ((slots (closer-mop::class-slots (class-of instance))))
    (dolist(slot-name slot-names)
      (setf (slot-value instance slot-name)
            (funcall
             (closer-mop::slot-definition-initfunction
              (find slot-name slots
                    :key #'closer-mop::slot-definition-name)))))))

(defun wstrim(string)
"* Arguments
- string :: a string

* Returns
- trimmed :: a string

* Description

Returns a new string with leading and trailing white space removed."
(string-trim '(#\space #\tab) string))

(defun property-union(list1 list2)
  "* Arguments
- list1 :: a property list
- list2 :: a property list

* Returns
- list :: a property list

* Description

This is intended to implement property inheritence.

Returns a merged property list combining properties from list1 and
list2. list1 property will have priority except if the property
values are themselves a list in which case the result is list2 value
appended onto end of list1 value."
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

(defstruct (coord (:constructor make-coord(&optional x y z))
                  (:print-object (lambda(c os)
                                   (format os "~,2f,~,2f~:[,~,2f~;~]"
                                           (coord-x c)
                                           (coord-y c)
                                           (zerop (coord-z c))
                                           (coord-z c)))))
  "A spatial coordinate"
  (x 0.0 :type float :read-only t)
  (y 0.0 :type float :read-only t)
  (z 0.0 :type float :read-only t))

(defun coord+(a b)
  "Add two [[coord]]s together returning result [[coord]]"
  (make-coord (+ (coord-x a) (coord-x b))
              (+ (coord-y a) (coord-y b))
              (+ (coord-z a) (coord-z b))))

(defun coord-(a b)
  "Return a-b for [[coord]]s"
  (make-coord (- (coord-x a) (coord-x b))
              (- (coord-y a) (coord-y b))
              (- (coord-z a) (coord-z b))))

(defun coord*(a b)
  "Return element wise (scalar) multiplation of [[coord]]s a and b."
  (make-coord (* (coord-x a) b)
              (* (coord-y a) b)
              (* (coord-z a) b)))

(defun coord-op(op &rest coords)
  "Given a function and a set of coordinates apply op to each set of
ordinates"
  (apply
   #'make-coord
   (mapcar #'(lambda(f) (coerce (apply op (mapcar f coords)) 'float))
           (load-time-value (list #'coord-x #'coord-y #'coord-z)))))

(defgeneric distance(a b)
  (:documentation "Return the Euclidean distance between two entities
  =a= and =b=")
  (:method((a coord) (b coord))
    (let ((d (coord- a b)))
      (sqrt (+ (* (coord-x d) (coord-x d))
               (* (coord-y d) (coord-y d))
               (* (coord-z d) (coord-z d)))))))

(defun range-getf(spec index)
  "* Arguments
- spec :: a range property list
- index :: a number

* Results
- result :: a value corresponding to =index= in =spec= or =nil= if no match
- index :: the lower index of range corresponding to match

* Description

Range property lists are plists where the property indicator is
either a number indicating a single value or a cons of a lower and
upper range.

[[range-getf]] finds the property in =spec= that either === the =index= or where=index= lies between the upper and lower bound of the specified range.

Property ranges are typically used to specify parameters that vary depending on an index.

* Examples

;;; (range-getf '(1  A (2 6) B 7 C (8 10) D) 1)
;;; => A,1
;;; (range-getf '(1  A (2 6) B 7 C (8 10) D) 5)
;;; => B,2
;;; (range-getf '(1  A (2 6) B 7 C (8 10) D) 0)
;;; =>nil,0
"
  (let ((default nil))
    (or
     (loop :for a :on spec :by #'cddr
        :for key = (car a)
        :for value = (cadr a)
        :do
        (typecase key
          (number (when (= key index)
                    (return-from range-getf (values value key))))
          (list (when (<= (car key) index (cadr key))
                  (return-from range-getf (values value (car key)))))
          (symbol (when (eql key t) (setf default value)))))
     (values default 0))))

(defun range-list-p(spec)
  "Return true if =spec= is a valid range property list.

* See Also
[[range-getf]]"
  (and (listp spec)
       (let ((first (first spec)))
         (or (listp first) (numberp first) (eql first t)))))
