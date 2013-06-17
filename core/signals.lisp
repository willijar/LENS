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
  "Length of bit vectors for caching signal listeners")

(defvar *signals*
  (make-array +SIGNAL-CACHE-SIZE+
              :element-type 'symbol
              :fill-pointer 0
              :adjustable t))

(defun register-signal(symbol &optional documentation)
  (if documentation
      (setf (get symbol 'signal-doc) documentation)
      (remprop symbol 'signal-doc))
  (setf (get symbol 'signal-id)
        (or (position symbol *signals*)
            (vector-push-extend symbol *signals*))))

(eval-when(:load-toplevel :execute)
  (register-signal 'PRE-MODEL-CHANGE)
  (register-signal 'POST-MODEL-CHANGE))

(declaim (inline signal-id))
(defun signal-id(symbol) (get symbol 'signal-id))

(defclass listener(owned-object)
  ()
  (:documentation "An object which can received signals"))

(defgeneric receive-signal(listener signal source-component value)
  (:documentation "All listeners must implement this to carry out work")
  (:method :around(listener signal source-component value)
    (declare (ignore signal source-component value))
    (let ((*context* listener))
      (call-next-method))))

(defmethod finish((listener listener)))

(defclass entity-with-signals(owned-object)
  ((signal-table
    :type hash-table :initform (make-hash-table)
    :reader signal-table
    :documentation "Hash by signal of lists of registered listeners
    for this entity")
   (has-local-listeners
    :type simple-bit-vector
    :initform (make-array +SIGNAL-CACHE-SIZE+ :element-type 'bit
                          :initial-element 0)
    :documentation "bit[k]==1: signalID k has local listeners")
   (has-ancestor-listeners
    :type simple-bit-vector
    :documentation "bit[k]==1: signalID k has listeners in any
    ancestor component"))
  (:documentation "An entity which can register listeners"))

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
  (:documentation "Return list of listeners for a particular signal -
  or all listeners in entity if signal is 't")
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
  (declare (fixnum signal-id))
  (or (>= signal-id +SIGNAL-CACHE-SIZE+)
      (= 1 (bit (slot-value entity 'has-local-listeners) signal-id))
      (= 1 (bit (slot-value entity 'has-ancestor-listeners) signal-id))))

(defun may-have-local-listeners(entity signal-id)
  (declare (fixnum signal-id))
  (or (>= signal-id +SIGNAL-CACHE-SIZE+)
      (= 1 (bit (slot-value entity 'has-local-listeners) signal-id))))

(defun may-have-ancestor-listeners(entity signal-id)
  (declare (fixnum signal-id))
  (or (>= signal-id +SIGNAL-CACHE-SIZE+)
      (= 1 (bit (slot-value entity 'has-ancestor-listeners) signal-id))))

(define-compiler-macro may-have-listeners(&whole form entity arg)
  (if (and (listp arg) (eql (first arg) 'quote) (symbolp (second arg)))
      `(may-have-listeners ,entity (load-time-value (symbol-id (second arg))))
      form))

(defgeneric emit(entity signal &optional value)
  (:documentation "Emit an optional value as a signal. If the given
 signal has listeners in this component or in ancestor components,
 their appropriate receive-signal() methods get called.")
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
  (:documentation "Return true if the given signal has any
    listeners. For some signals this method has a significant
    overhead (linear to the number of hierarchy levels in the
    network).")
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
  (:documentation "Adds a listener (callback object) that will be
     notified when a given signal is emitted (see emit() methods). It
     is an error to subscribe the same listener twice to the same
     signal. The order in which listeners will be notified is
     undefined, so it is not necessarily the same order in which
     listeners were subscribed.")
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
  (:documentation "Removes the given listener. It has no effect if the
  given listener is not subscribed.")
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
  (:documentation "Returns true if the given listener is subscribed to
  the given signal at this component (i.e. it does not look at
  listeners subscribed at ancestor components).")
  (:method((entity entity-with-signals) (signal symbol) listener)
    (member listener (gethash signal (slot-value entity 'signal-table)))))

(defgeneric repair-signal-flags(component)
  (:documentation " adjusts has-ancestor-listeners bits in the
    component's subtree; to be called when the component's ownership
    changes")
  (:method((component entity-with-signals))
    (let ((parent (parent-module component)))
      (when parent
        (setf (slot-value component 'has-ancestor-listeners)
              (map 'bit-vector #'logand
                   (slot-value parent 'has-ancestor-listeners)
                   (slot-value parent 'has-local-listeners)))))))

(defmethod finish((instance entity-with-signals))
  (map nil #'finish (listeners instance t)))