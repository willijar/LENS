;; Some commonly used definitions and protocols
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:


;;; Code:

(in-package :lens)

;; Basic types

(defconstant +c+ 299792458d0 "Speed of Light in m/sec")

(defvar *simulation* nil "The global simulation instance")

(defparameter *context* nil
  "Current global context in which evaluation (e.g. random functions)
  is to be done.")

(defvar *time-format* "~7,4f"  "Time output format control")

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
  "Return a list of the elements in `lst` for which `test` (applied to `key`)
is true.

Arguments:

- `test`: a designator for a function of one argument which returns a
          generalised boolean
- `lst`: a proper list
- `key`: a designator for a function of one argument

Returns:

- `result`: a list"
  `(mapcan #'(lambda(it)
               (when (funcall ,test (funcall ,key it))
                 (list it)))
    ,lst))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
      ((>= ,var ,gstop))
      ,@body)))

(defun set-slots(instance defs)
  (dolist(def defs)
    (if (listp def)
        (setf (slot-value instance (first def)) (second def))
        (setf (slot-value instance def) nil))))

(defun copy-slots(slots source destination)
  "Copies anemd slot values shallowly from source to destination
returning the modifed destination object."
  (dolist(slot slots)
    (if (slot-boundp source slot)
        (setf (slot-value destination slot) (slot-value source slot))
        (slot-makunbound destination slot)))
  destination)

(defun wstrim(string) (string-trim '(#\space #\tab) string))

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

(defstruct (coord (:constructor make-coord(&optional x y z))
                  (:print-object (lambda(c os)
                                   (format os "~,2f,~,2f~:[,~,2f~;~]"
                                           (coord-x c)
                                           (coord-y c)
                                           (zerop (coord-z c))
                                           (coord-z c)))))
  (x 0.0 :type float :read-only t)
  (y 0.0 :type float :read-only t)
  (z 0.0 :type float :read-only t))

(defun coord+(a b)
  (make-coord (+ (coord-x a) (coord-x b))
              (+ (coord-y a) (coord-y b))
              (+ (coord-z a) (coord-z b))))

(defun coord-(a b)
  (make-coord (- (coord-x a) (coord-x b))
              (- (coord-y a) (coord-y b))
              (- (coord-z a) (coord-z b))))

(defun coord*(a b)
  (make-coord (* (coord-x a) b)
              (* (coord-y a) b)
              (* (coord-z a) b)))

(defun coord-op(op &rest coords)
  "Given a function and a set of coordinates apply op to each set of ordinates"
  (apply
   #'make-coord
   (mapcar #'(lambda(f) (coerce (apply op (mapcar f coords)) 'float))
           (load-time-value (list #'coord-x #'coord-y #'coord-z)))))

(defgeneric distance(a b)
  (:method((a coord) (b coord))
    (let ((d (coord- a b)))
      (sqrt (+ (* (coord-x d) (coord-x d))
               (* (coord-y d) (coord-y d))
               (* (coord-z d) (coord-z d)))))))

(defun range-getf(spec index)
  "Helper function allowing range based plists - returns value and lower index"
  (let ((default nil))
    (or
     (loop :for a :on spec :by #'cddr
        :for key = (car a)
        :for value = (cadr a)
        :do
        (typecase key
          (number (when (= key index) (return (values value key))))
          (list (when (<= (car key) index (cadr key))
                  (return (values value (car key)))))
          (symbol (when (eql key t) (setf default value)))))
     (values default 0))))

(defun range-list-p(spec)
  (and (listp spec)
       (let ((first (first spec)))
         (or (listp first) (numberp first) (eql first t)))))