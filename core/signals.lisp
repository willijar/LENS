;; Signals and Listeners
;; Copyright (C) 2013 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of LENS

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; LENS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(in-package :lens)

(defconstant +SIGNAL-CACHE-SIZE+ 64
  "Length of bit vectors for caching signal listeners.")

(defvar *signals*
  (make-array +SIGNAL-CACHE-SIZE+
              :element-type 'symbol
              :fill-pointer 0
              :adjustable t)
  "Array mapping signal index to a the signal symbols.")

(defun register-signal(symbol &optional documentation)
  "*Arguments
- symbol :: a =symbol=

* Optional Arguments

- documentation :: a =string=

* Returns

- signal-id :: an =integer=

* Description

Register signal denoted by /symbol/ and optionally recording the
/documentation/. If the named signal has not previously been
registered it will be allocated a new integer /signal-id/ stored in
the =signal-id= property of the symbol and it will be added onto the
end of the global [[*signals*]] array. /documentation/ is stored in
the =signal-doc= symbol.

[[register-signal]] is intended to be called as a top level form.

The first [[+signal-cache-size+]] signals registered are cached by
[[entity-with-signals]] objects and therefore the signals which are
most heavily used should be registered first when loading the
simulation code."
  (if documentation
      (setf (get symbol 'signal-doc) documentation)
      (remprop symbol 'signal-doc))
  (setf (get symbol 'signal-id)
        (or (position symbol *signals*)
            (vector-push-extend symbol *signals*))))

(eval-when(:load-toplevel :execute)
  (register-signal 'PRE-MODEL-CHANGE
                   "Signal emitted before a change in a simulation model")
  (register-signal 'POST-MODEL-CHANGE
                   "Signal emitted after a change in a simulation model"))

(declaim (inline signal-id))
(defun signal-id(symbol)
  "Return the integer id of the named signal."
  (get symbol 'signal-id))

(defgeneric receive-signal(listener signal source value)
  (:documentation "* Arguments

- listener :: an [[entity-with-signals]] instance.
- signal :: a signal designator (=symbol= or =integer=)
- source :: an [[entity-with-signals]] instance.
- value :: signalled value

* Description

A call of [[emit]] with signal value /signal/ from
[entity-with-signals]] object/source/ and value /value/ will result in
[[receive-signal]] being called in the /source/ object and all ancestor
objects in the simulation heirarchy that have registered to receive
this /signal/ using [[subscribe]]

All objects which wish to receive signals must specialise this method.
")
  (:method :around(listener signal source-component value)
    (declare (ignore signal source-component value))
    (let ((*context* listener))
      (call-next-method))))

(defclass entity-with-signals(owned-object)
  ((signal-table
    :type hash-table :initform (make-hash-table)
    :reader signal-table
    :documentation "Hash by signal of lists of registered listeners
    for this entity.")
   (has-local-listeners
    :type simple-bit-vector
    :initform (make-array +SIGNAL-CACHE-SIZE+ :element-type 'bit
                          :initial-element 0)
    :documentation "A bit map recording which signals have local listeners.")
   (has-ancestor-listeners
    :type simple-bit-vector
    :documentation "A bit map recording which signals have ancestor
    listeners."))
  (:documentation "An entity which can [[subscribe]] to and [[emit]] and
  [[receive-signal]] signals."))

(defmethod initialize-instance :after ((instance entity-with-signals)
                                       &key &allow-other-keys)
  (let ((owner (owner instance)))
    (setf (slot-value instance 'has-ancestor-listeners)
          (if (typep owner 'entity-with-signals)
              (bit-ior (slot-value owner 'has-ancestor-listeners)
                       (slot-value owner 'has-local-listeners))
              (make-array +SIGNAL-CACHE-SIZE+ :element-type 'bit
                          :initial-element 0)))))

(defgeneric listeners(entity signal)
  (:documentation "* Arguments

- entity :: a [[entity-with-signals]]
- signal :: a =symbol= or =t=

* Description

Return list of local listeners to signal denoted by /signal/ for /entity/.
If /signal/ is t return all listeners in /entity/.")
  (:method((entity entity-with-signals) (signal symbol))
    (gethash signal (signal-table entity)))
  (:method((entity  entity-with-signals) (signal (eql t)))
    (let ((listeners nil))
      (maphash #'(lambda(k v)
                   (declare (ignore k))
                   (setf listeners (union listeners v)))
           (signal-table entity))
      listeners)))

(defun may-have-listeners(entity signal-id)
  "* Arguments

- entity :: an [[entity-with-signals]]
- signal-id :: a positive =integer= or =symbol= signal identifier

* Description

Return true if /entity/ may have listeners for signal
/signal-id/. That is if the corresponding signal has local or ancestor
listeners according to the /entity/ cache/ or the signal is outside
the cache range.

It is intented that this is an efficient check that may be used to
eliminate uneccessary calculations of values and calls to [[emit]]."
  (declare (fixnum signal-id))
  (or (>= signal-id +SIGNAL-CACHE-SIZE+)
      (= 1 (bit (slot-value entity 'has-local-listeners) signal-id))
      (= 1 (bit (slot-value entity 'has-ancestor-listeners) signal-id))))

(defun may-have-local-listeners(entity signal-id)
"* Arguments

- entity :: an [[entity-with-signals]]
- signal-id :: a positive =integer= signal identifier

* Description

Return true if /entity/ may have local listeners for signal
/signal-id/. That is if the corresponding signal has local or ancestor
listeners according to the /entity/ cache/ or the signal is outside
the cache range."
  (declare (fixnum signal-id))
  (or (>= signal-id +SIGNAL-CACHE-SIZE+)
      (= 1 (bit (slot-value entity 'has-local-listeners) signal-id))))

(defun may-have-ancestor-listeners(entity signal-id)
"* Arguments

- entity :: an [[entity-with-signals]]
- signal-id :: a positive =integer= signal identifier

* Description

Return true if /entity/ may have ancestor listeners for signal
/signal-id/. That is if the corresponding signal has local or ancestor
listeners according to the /entity/ cache/ or the signal is outside
the cache range."
  (declare (fixnum signal-id))
  (or (>= signal-id +SIGNAL-CACHE-SIZE+)
      (= 1 (bit (slot-value entity 'has-ancestor-listeners) signal-id))))

;; this macro will convert a quoted symbol intoi its registered integer.
(define-compiler-macro may-have-listeners(&whole form entity arg)
  (if (and (listp arg) (eql (first arg) 'quote) (symbolp (second arg)))
      `(may-have-listeners ,entity (load-time-value (symbol-id (second arg))))
      form))

(defgeneric emit(entity signal &optional value)
  (:documentation "* Arguments

- entity :: an [[entity-with-signals]]
- signal-id :: a signal identifier

* Optional Arguments

- value :: a value

* Description

Emit the optional /value/ with a signal. If the given signal has
listeners in this component /entity/ or in it's ancestor components,
their appropriate [[receive-signal]] methods get called.")
  (:method((entity entity-with-signals) (signal symbol) &optional value)
    (let ((signalid (signal-id signal)))
      (when (may-have-listeners entity signalid)
        (let ((notification-stack nil))
        ;; ensure listener only receives one notification
        (labels
            ((fire(source)
               (when (may-have-local-listeners source signalid)
                 (dolist(listener
                          (gethash signal (slot-value source 'signal-table)))
                   (unless (member listener notification-stack)
                     (push listener notification-stack)
                     (receive-signal listener signal entity value))))
               (when (and (may-have-ancestor-listeners source signalid)
                          (parent-module source))
                 (fire (parent-module source)))))
          (fire entity)))))))

(defgeneric has-listeners(entity signal)
  (:documentation "* Arguments

- entity :: an [[entity-with-signals]]
- signal-id :: =symbol= signal identifier

* Description

Return true if /entity/ has any listeners for signal designated by
/signal-id/.

For some signals this method has a significant overhead (linear to the
number of hierarchy levels in the network). [[may-have-listeners]] may
be more appropriate in most cases.")
  (:method((entity entity-with-signals) (signal symbol))
    (let ((signal-id (signal-id signal)))
      (if (< signal-id +SIGNAL-CACHE-SIZE+)
        (labels((compute-has-listeners(entity)
                  (when entity
                    (if (gethash signal (slot-value entity 'signal-table))
                        (return-from has-listeners t)
                        (compute-has-listeners (parent-module entity))))))
          (compute-has-listeners entity))
        (or (= 1 (bit (slot-value entity 'has-local-listeners) signal-id))
            (= 1 (bit (slot-value entity 'has-ancestor-listeners) signal-id)))))))

(defgeneric subscribe(entity signal listener)
  (:documentation "* Arguments

- entity :: an [[entity-with-signals]]
- signal-id :: =symbol= signal identifier
- listener :: an object

* Description

Adds a listener (callback object) that will be notified using the
[[receive signal]] method when the given signal is emitted (see
[[emit]] methods). It is an error to subscribe the same listener twice
to the same signal. The order in which listeners will be notified is
undefined.")
  (:method((entity entity-with-signals) (signal symbol) listener)
    (let ((signalid (signal-id signal)))
      (assert signalid () "~A not a valid signal" signal)
      (assert
       (not (member listener
                    (gethash signal (slot-value entity 'signal-table))))
       ()
       "Listener ~A already subscribed to ~A" listener signal)
      (push listener (gethash signal (slot-value entity 'signal-table)))
      (when (<  signalid +SIGNAL-CACHE-SIZE+)
        (setf (bit (slot-value entity 'has-local-listeners) signalid) 1)
        (labels
            ((ancestor-listener-added(ancestor)
               (for-each-child
                ancestor
                #'(lambda(child)
                    (when (typep child 'entity-with-signals)
                      (setf (bit (slot-value child 'has-ancestor-listeners)
                                 signalid)
                            1)
                      (ancestor-listener-added child))))))
          (ancestor-listener-added entity))))))

(defgeneric unsubscribe(entity signal listener)
  (:documentation "* Arguments

- entity :: an [[entity-with-signals]]
- signal-id :: =symbol= signal identifier
- listener :: an object

* Description

Removes the given /listener/ from the subscription list for signal
designated by /signal-id/ in /entity/ . It has no effect if the
given listener was not subscribed using [[subscribe]].")
  (:method ((entity entity-with-signals) (signal symbol) listener)
    (let ((signalid (signal-id signal)))
      (assert signalid () "~A not a valid signal" signal)
      (with-slots(signal-table) entity
        (setf (gethash signal signal-table)
              (delete listener (gethash signal signal-table)))
        (unless (gethash signal signal-table)
          (remhash signal signal-table))
        (when (<  signalid +SIGNAL-CACHE-SIZE+)
          (setf (bit (slot-value entity 'has-local-listeners) signalid) 0)
          (labels
              ((ancestor-listener-removed(ancestor)
                 (for-each-child
                  ancestor
                    #'(lambda(child)
                        (setf (bit (slot-value entity 'has-ancestor-listeners)
                                   signalid)
                              0)
                        (unless (gethash signal
                                         (slot-value child 'signal-table))
                          (ancestor-listener-removed child))))))
            (ancestor-listener-removed entity)))))))

(defgeneric subscribed-p(entity signal listener)
  (:documentation "* Arguments

- entity :: an [[entity-with-signals]]
- signal-id :: =symbol= signal identifier
- listener :: an object

* Description

Removes the given /listener/ from the subscription list for signal
designated by /signal-id/ in /entity/ . It has no effect if the
given listener was not subscribed using [[subscribe]].

Returns true if the given /listener/ is subscribed to the signal
designated by /signal-id/ at /entity/ component (i.e. it does not look
at listeners subscribed at ancestor components).")
  (:method((entity entity-with-signals) (signal symbol) listener)
    (member listener (gethash signal (slot-value entity 'signal-table)))))

(defgeneric repair-signal-flags(component)
  (:documentation "* Arguments

- component :: an [[entity-with-signals]]

* Description

Adjusts has-ancestor-listeners bits in the component's subtree; It
must be called when the component's ownership changes.")
  (:method((component entity-with-signals))
    (let ((parent (parent-module component)))
      (when parent
        (setf (slot-value component 'has-ancestor-listeners)
              (map 'bit-vector #'logand
                   (slot-value parent 'has-ancestor-listeners)
                   (slot-value parent 'has-local-listeners)))))))

(defmethod finish ((instance entity-with-signals))
  (map nil #'finish (listeners instance t)))
