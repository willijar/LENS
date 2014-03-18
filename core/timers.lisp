;; with-timers module mixin and related timer message types
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

;;

;;; Code:

(in-package :lens)

(defclass with-timers()
  ((timers :type list :initform nil
           :documentation "Active Timers which aren't cached in slots"))
  (:documentation "Mixin class with timer handling"))

(defclass timer-message(message)
  ())

(defmethod print-object((m timer-message) stream)
  (print-unreadable-object(m stream :type t :identity t)
    (format stream "~A ~:[~;~A~]" (name m) (scheduled-p m) (arrival-time m))))

(defgeneric timer(module name)
  (:method((module with-timers) (name symbol))
    (if (slot-exists-p module name)
        (slot-value module name)
        (find name (slot-value module 'timers) :key #'name))))

(defgeneric set-timer(module timer interval &optional timer-name)
  (:documentation "Schedule a timer")
  (:method(module (timer message) interval  &optional name)
    (when (scheduled-p timer)
      (cancel-timer module timer))
    (when name (setf (name timer) name))
    (if (owner timer)
        (assert (eql (owner timer) module))
        (setf (owner timer) module))
    (schedule-at module timer :delay interval)
    timer)
  (:method((module with-timers) (timer-name symbol) interval
           &optional (name timer-name))
    (let ((timer (timer module name)))
      (if timer
          (cancel-timer module timer)
          (setf timer (make-instance 'timer-message :owner module :name name)))
      (push timer (slot-value module 'timers))
      (set-timer module timer interval name) )))

(defgeneric cancel-timer(instance timer)
  (:method(instance (timer timer-message))
    (cancel timer))
  (:method((instance with-timers) (name symbol))
    (let ((timer (timer instance name)))
      (when timer (cancel timer))))
  (:method((instance with-timers) (timer timer-message))
    (cancel timer)
    (setf (slot-value instance 'timers)
          (delete timer (slot-value instance 'timers) :key #'name))))

(defmethod cancel :after((timer timer-message))
  (let ((owner (owner timer)))
    (when (and owner (not (slot-exists-p owner (name timer))))
      (with-slots(timers) owner
        (setf timers (delete timer timers))))))

(defgeneric handle-timer(module timer-name)
  (:documentation "Called when a timer message arrives with the message name"))

(defmethod handle-message((module with-timers) (message timer-message))
  (handle-timer module (name message)))
