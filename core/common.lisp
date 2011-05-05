;; Some commonly used definitions and protocols
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:


;;; Code:

(in-package :common)

;; Basic types

(defconstant +c+ 299792458d0 "Speed of Light in m/sec")

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

(defmacro when-bind ((var expr) &body body)
  "Bind VAR to VALUE of expression, execute body if true"
  `(let ((,var ,expr))
    (when ,var
      ,@body)))

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

(defmacro defenumeration (typename (&rest items))
  (let ((items (loop :for item :in items
                     :for count :from 0
                     :collect (if (consp item) item (list item count)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      ,@(loop :for item :in items
              :collect
              `(defconstant ,(first item) ,@(rest item)))
      ,@(if (listp typename)
            `((deftype ,(first typename)() ',@(rest typename))
              (defvar ,(first typename) ',(mapcar #'first items)))
            `((deftype ,typename () '(member ,@(mapcar #'second items)))
              (defvar ,typename ',(mapcar #'first items)))))))

(defgeneric uid(entity)
  (:documentation "Return the unique id of an entity"))

(defgeneric node(entity)
  (:documentation "Return the node associated with an entity"))

(defgeneric name(entity)
  (:documentation "Return a descriptive name of an entity - used in tracing")
  (:method((obj standard-object))
    (class-name (class-of obj))))

(defgeneric (setf name)(value entity)
  (:documentation "Set the descriptive name of an entity"))

(defgeneric start(entity)
  (:documentation "Start an entity"))

(defgeneric stop(entity &key abort)
  (:documentation "Stop a running entity.
 If keyword abort is true - also abort current action"))

(defgeneric busy-p(entity)
  (:documentation "Return true if an entity is busy"))

(defgeneric length-bytes(pdu)
  (:documentation "Return length of simulation entity in bytes"))

(defvar *reset-hooks* nil
  "List of hooks to call during a reset")

(defgeneric reset(entity)
  (:documentation "Reset an entity to same state as start of
 simulation i.e.  clear all packets etc.")
  (:method ((entities sequence))
    (map nil #'reset entities))
  (:method ((entity (eql :all)))
    (reset (node:nodes))
    (dolist(h *reset-hooks*)
      (typecase h
        (function (funcall h))
        (t (reset h))))))

(defgeneric copy(entity &optional destination)
  (:documentation "Create an return a (deep) copy of an entity.
Optional argument destination is for inherited implementation use only.")
  (:method((entity (eql nil)) &optional destination)
    (declare (ignore destination))
    nil)
  (:method((entity number) &optional destination)
    (declare (ignore destination))
    entity)
  (:method((entity symbol) &optional destination)
    (declare (ignore destination))
    entity)
  (:method((entity list) &optional destination)
    (declare (ignore destination))
    (map 'list #'copy entity))
  (:method((v vector) &optional
           (copy (make-array (array-total-size v)
                             :element-type (array-element-type v)
                             :initial-element (array-element-type v)
                             :adjustable (adjustable-array-p v)
                             :fill-pointer (when (array-has-fill-pointer-p v)
                                             (fill-pointer v)))))
      ;; note we copy all elements - even those past the fill pointer
      (do((i 0 (1+ i)))
         ((= i (array-total-size v))
          copy)
        (setf (aref copy i) (copy (aref v i))))))

(defun copy-slots(src dst &key
                  (unbound-only t)
                  (instance-only t)
                  excluding
                  (slot-names
                   (mapcan
                    #'(lambda(slot)
                        (let ((name (slot-definition-name slot)))
                          (when (and
                                 (or (not instance-only)
                                     (eql (slot-definition-allocation slot)
                                          :instance))
                                 (not (and unbound-only
                                           (slot-boundp dest name)))
                                 (not (member name excluding)))
                            (list name))))
                    (class-slots (class-of src)))))
  "Copy slot values using copy (including unbound) from src instance
to dst instance. Return dst (copy) instance. Keyword aguments:

slot-names   : if provided the list of slot names to be copied. Otherwise
               this is determined by remaining keyword arguments.
unbound-only : if true only undound slots in destination are copied (default t)
instance-only: if true only instance allocated slots are copied (default t)
excluding    : list of slot names to be excluded"
  (dolist(slot-name slot-names)
    (if (slot-boundp src slot-name)
      (setf (slot-value dest slot-name) (copy (slot-value src  slot-name)))
      (slot-makunbound dest slot-name)))
  dst)

(defmethod copy((obj standard-object) &optional
                (copy (allocate-instance (class-of obj))))
  (copy-slots obj copy :unbound-only t :instance-only t))

(defclass immutable()
  ()
  (:documentation "Base for classes which are immutable and so don't have to be deep copied."))

(defmethod copy((obj immutable)  &optional destination)
  (declare (ignore destination))
  obj)

(defmacro trace-accessor((slotname (objvar type)
                          &optional (slotvar (gensym))) &rest body)
  "Define method to add tracing code to standard slot writer function.
It is assumed that slot and accessor have the same name."
  `(defmethod (setf ,slotname)(,slotvar (,objvar ,type))
     ,@(or body
           `((format *standard-trace* "~A ~A ~A->~A~%"
                     ,objvar ',slotname (slot-value  ,objvar ',slotname)
                     ,slotvar)))
     (setf (slot-value ,objvar ',slotname) ,slotvar)))

(defmacro untrace-accessor((slotname (objvar type)))
   "Define method to remove tracig code and replace with a setf slot
It is assumed that slot and accessor have the same name."
  (let ((gv (gensym)))
  `(defmethod (setf ,slotname)(,gv (,objvar ,type))
     (setf (slot-value ,objvar ',slotname) ,gv))))

(defun cl-user::print-eng(os arg &optional colon-p at-p
                 (d 2) (padchar #\space) (exponentchar #\e))
  "Formatter which outputs its numerical argument `arg` in engineering format
to stream `os`.
It takes arguments d,padchar,exponentchar where
d is the number of decimal places to display after the decimal point
padchar is the character to pad the start of the number
exponentchar is the character to use to display between radix and exponent
It also takes the : modifier which will cause it to output the exponent
as an SI units prefix rather than a number.

Arguments:

- `os`: an output stream designator
- `arg`: a number
- `colon-p`: a generalised boolean (default false)
- `at-p`: a generalised boolean (default false) - ignored
- `d`: an integer (default 2)
- `padchar`: a character (default `space`)
- `exponentchar`: a character (default `e`))

Result:

nil

Examples:

`(format nil \"~/print-eng/\" 35000) => \"35.00e+3\"`
"
  (declare (ignore at-p))
  (if (numberp arg)
      (let* ((units "YZEPTGMk munfazy")
             ;; note use u instead of \mu for 1e-6 so utf-8 not needed
             (order (if (zerop arg) 0 (floor (log arg 10) 3)))
             (scale (* 3 order))
             (radix-format
              (if (or (zerop d) (integerp arg))
                  (format nil "~~,'~CD" padchar)
                  (format nil "~~,~@[~D~],,,'~CF"
                           d  padchar)))
             (radix (/ arg (expt 10 scale))))
        (when (zerop d) (setf radix (round radix)))
        (if (and colon-p (< -1 (- 8 order) (length units)))
            (format os "~@? ~:[~C~;~]"
                    radix-format
                    radix
                    (zerop scale)
                    (char units (- 8 order)))
            (format os "~@?~:[~C~@D~;~]"
                    radix-format
                    radix
                    (zerop scale)
                    exponentchar
                    scale)))
      (princ arg os)))

;;; Notifications mechanism

(defclass notifier()
  ((notifications :initform (make-instance 'alg:list-queue)
                  :documentation "Queue of notifcations to call when not busy"))
  (:documentation "Base for entities which can call notifications"))

(defgeneric add-notification(arg notifier notification)
  (:documentation "Add a notification to end of queue")
  (:method(arg (notification function) (notifier notifier) )
    (alg:enqueue (cons notification arg) (slot-value notifier 'notifications))))

(defgeneric delete-notifications(arg notifier)
  (:documentation"Delete notifications to arg on notifier")
  (:method( arg (notifier notifier))
    (alg:delete-if #'(lambda(p) (eql arg (cdr p)))
                   (slot-value notifier 'notifications))))

(defgeneric do-notifications(notifier)
  (:method((notifier notifier))
    (with-slots(notifications) notifier
      (while (not (or (busy-p notifier) (alg:empty-p notifications)))
        (let ((p (alg:dequeue notifications)))
          (funcall (car p) (cdr p)))))))

;; taking things up and down

(defgeneric up-p(entity)
  (:documentation "Return true if entity is up (operational)"))

(defgeneric mkup(entity &key inform-routing)
  (:documentation "Ensure entity is in up state, informing routing if state has changed and inform-routing keyword is true"))

(defgeneric mkdown(entity &key inform-routing)
  (:documentation "Ensure entity is in down state, informing routing
  if state has changed and inform-routing keyword is true"))

(define-condition lens-error(simple-error)
  ()
  (:documentation "Base for simulation internal errors"))

(declaim (inline lens-error))
(defun lens-error(format-control &rest args)
  (signal 'lens-error :format-control format-control :format-arguments args))

(defstruct location
  (x 0.0 :type short-float :read-only t)
  (y 0.0 :type short-float :read-only t)
  (z 0.0 :type short-float :read-only t))

(defparameter +origin+ (if (boundp '+origin+) +origin+ (make-location)))

(defgeneric location(entity)
  (:documentation "Return the location of an entity"))

(defgeneric distance(a b)
  (:documentation "Return distance between a and b")
  (:method((a location) (b location))
    (let ((dx (- (location-x a) (location-x b)))
          (dy (- (location-y a) (location-y b)))
          (dz (- (location-z a) (location-z b))))
      (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
  (:method(a b)
    (distance (location a) (location b))))

