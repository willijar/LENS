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
  (:documentation "Mixin class with dynamic timer handling. See
  [[set-timer]], [[handle-timer]] and [[cancel-timer]] for additional
  functionality provided for this class."))

(defclass timer-message(message)
  ()
  (:documentation "Class for timer messages. Components which subclass
  [[with-timers]] will receive these messages via [[handle-timer]]"))

(defmethod print-object((m timer-message) stream)
  (print-unreadable-object(m stream :type t :identity t)
    (format stream "~A ~:[~;~A~]" (name m) (scheduled-p m) (arrival-time m))))

(defgeneric timer(module name)
  (:documentation "* Arguments

- module :: an instance of [[with-timers]]
- name :: a =symbol=

* Description

Return the timer message with name /name/ associated with
/module/. If the /module/ has a slot with slot-name /name/ then
the slot value will be returned - otherwise the /timers/ slot of the
component will be search for the named timer.")
  (:method((module with-timers) (name symbol))
    (if (slot-exists-p module name)
        (slot-value module name)
        (find name (slot-value module 'timers) :key #'name))))

(defgeneric set-timer(module timer interval &optional timer-name)
  (:documentation "* Arguments

- module :: an instance of [[with-timers]]
- timer :: a timer designator (either =symbol= or [[timer-message]])
- interval :: a [time-type]]

* Optional Arguments
- timer-name :: a =symbol=

* Description

Schedule a timer designated by /timer/ to be fired after simulation
interval /interval/. [[handle-message]] will be called after
/interval/ with the /module/ and /timer-name/ as its arguments. If
/timer/ is a [[timer-message]] it will be givne the name /timer-name/.

If a timer designated by /timer/ already exists it will be cancelled
and rescheduled otherwise a new [[timer-message]] will be created with
the given name and added onto the =timers= slot list of /module/.

For efficiency if classes have a slot with slot-name /timer-name/ it
is assumed that this will contain the [[timer-message]] to be used. In
this case the slot definition should definine (make-instance
'timer-message) as the initform.")
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

(defgeneric cancel-timer(module timer)
  (:documentation "* Arguments

- module :: an instance of [[with-timers]]
- timer :: a timer designator (either =symbol= or [[timer-message]])

* Description

Cancel an already schedule timer designated by /timer/ associated with
/module/.  If the associated [[timer-message]] was created when
scheduled [[set-timer]] it will be deleted at this point.
")
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
  (:documentation "* Arguments

- module :: an instance of [[with-timers]]
- timer-name :: a =symbol=

* Description

Called when the timer desgnated by /timer-name/ asociated with module
/module/ is fired. Modules will typically should specialise on this
generic function to implment their timer behaviours using the =eql=
specializer form for timer-name."))

(defmethod handle-message((module with-timers) (message timer-message))
  (handle-timer module (name message)))
