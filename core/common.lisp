;; $Id$
;; Some commonly used definitions and protocols
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:


;;; Code:

(in-package :common)

;; Basic types
(deftype counter() "Type for counting objects" 'fixnum)
(deftype octet() "8-bit byte type" '(unsigned-byte 8))
(deftype word() "16-bit word type" '(unsigned-byte 16))
(deftype seq() "a sequence number type" 'fixnum)
(deftype fid() "a flow id number type" 'fixnum)
(defconstant +c+ 299792458 "Speed of Light in m/sec")

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
      ,(if (listp typename)
          `(deftype ,(first typename)() ',@(rest typename))
          `(deftype ,typename () '(member ,@(mapcar #'second items)))))))

(defgeneric uid(entity)
  (:documentation "Return the unique id of an entity"))

(defgeneric node(entity)
  (:documentation "Return the node associated with an entity"))

(defgeneric name(entity)
  (:documentation "Return a descriptive name of an entity"))

(defgeneric (setf name)(value entity)
  (:documentation "Set the descriptive name of an entity"))

(defgeneric start(entity)
  (:documentation "Start an entity"))

(defgeneric stop(entity &key abort)
  (:documentation "Stop a running entity.
 If keyword abort is true - also abort current action"))

(defgeneric busy-p(entity)
  (:documentation "Return true if an entity is busy"))

(defvar *reset-hooks* nil
  "List of hooks to call during a reset")

(defgeneric reset(entity)
  (:documentation "Reset an entity to same state as start of
 simulation i.e.  clear all packets etc.")
  (:method ((entities sequence))
    (map nil #'reset entities))
  (:method ((entity (eql :all)))
    (dolist(h *reset-hooks*)
      (typecase h
        (list (apply (first h) (rest h)))
        (function (funcall h))
        (t (reset h))))))

(defgeneric copy(entity)
  (:documentation "Create an return a (deep - no shared structure)
 copy of an entity")
  (:method((entity (eql nil))) nil)
  (:method((entity number)) entity)
  (:method((entity symbol)) entity)
  (:method((entity list)) (map 'list #'copy entity))
  (:method((v vector))
    (let ((copy (make-array (array-total-size v)
                            :element-type (array-element-type v)
                            :initial-element (array-element-type v)
                            :adjustable (adjustable-array-p v)
                            :fill-pointer (when (array-has-fill-pointer-p v)
                                            (fill-pointer v)))))
      ;; note we copy all elements - even those past the fill pointer
      (do((i 0 (1+ i)))
         ((= i (array-total-size v))
          copy)
        (setf (aref copy i) (copy (aref v i)))))))

(defun copy-with-slots(original slots &optional (copy (allocate-instance (class-of original))))
  "Given an original instance allocate and return a new instance of
the same class and set specified slots of copy to the same values as
the slots of the original."
    (dolist(slot slots)
      (if (slot-boundp original slot)
          (setf (slot-value copy slot) (copy (slot-value original slot)))
          (slot-makunbound copy slot)))
    copy)

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

(defgeneric add-notify(notifier notification)
  (:documentation "Add a notification to end of queue")
  (:method((notification function) (notifier notifier) )
    (alg:push notification (slot-value notifier 'notifications))))

(defgeneric do-notifications(notifier)
  (:method((notifier notifier))
    (with-slots(notifications) notifier
      (while (not (or (busy-p notifier) (alg:empty-p notifications)))
        (funcall (alg:pop notifications))))))