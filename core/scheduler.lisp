;; LENS Simulation Scheduler and event handling
;; Copyright 2007 Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See the LICENSE file provided or <http://www.gnu.org/licenses>

;;; Commentary:

;;

;;; Code:

(in-package :scheduler)

(defvar *scheduler* nil "The global scheduler instance")
(defvar *reset-hooks* nil "List of hooks to call to reset simulation -
called after all entities created before running simulation")
(defvar *time-format* "~6,2f"  "Time output format control")

(declaim (inline scheduler))
(defun scheduler() *scheduler*)

(deftype time-type() 'double-float)

(defclass event()
  ((rank :type 'fixnum :initform -1 :accessor rank
         :documentation "Rank in priority queue")
   (event-time :initarg :time :type time-type :accessor event-time
	 :initform -1.0d0
	 :documentation "simulation time at which event handler is to be accled")
   (handler :accessor handler :initarg :handler
            :documentation "The handler function for this event"))
  (:documentation "Class representing a scheduled event"))

(defmethod print-object((event event) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "T=~,2f" (event-time event) )))

(defgeneric handle(entity)
  (:documentation "Method called by scheduler on a scheduled entity")
  (:method((f function)) (funcall f))
  (:method((form list)) (apply (first form) (rest form)))
  (:method((event event))
    #+debug(format *debug-io* "handle: ~,3f: ~S~%"
                   (simulation-time) (handler event))
    (handle (handler event))))

(defclass scheduler ()
  ((clock :type time-type :initform 0.0d0
	  :accessor clock :initarg :start-time
	  :documentation "simulator virtual time")
   (halted :type boolean :initform t :accessor halted)
   (thread :initform nil :reader scheduler-thread
	    :documentation "Thread running scheduler")
   (event-queue :initform
                (make-binary-heap 1024 :extend-size 1.4
                                  :element-type 'event
                                  :key-fn #'event-time
                                  :comp-fn #'<
                                  :index #'(lambda(a &optional b)
                                             (if b (setf (rank a) b)
                                                 (rank a)))))))

(defmethod print-object((scheduler scheduler) stream)
  (print-unreadable-object (scheduler stream :type t :identity t)
     (format stream "time:~f~:[~; HALTED~] ~D pending events"
	     (clock scheduler) (halted scheduler)
       (size (event-queue scheduler)))))

(defgeneric schedule(delay handler)
  (:documentation "Schedule event on current scheduler withy delay")
  (:method(delay (handler event))
    (assert (not (active-p handler)) ()
            "Attempt to schedule an already active ~A timer"
            (name timer))
    (assert (>= delay 0)
                   (delay)
                   "Attempt to schedule ~S ~D seconds in the past"
                   handler delay)
    (let ((scheduler (scheduler)))
      (setf (event-time handler) (+ delay (clock scheduler)))
      (enqueue hander (event-queue scheduler)))
    handler)
  (:method (delay handler)
    (schedule delay (make-event 'event :handler handler))))

(defgeneric cancel(event)
  (:documentation "Cancel a scheduled event")
  (:method ((event event))
    (alg::delete event (event-queue event))))

(defmethod stop ((scheduler scheduler) &key abort)
  (setf (slot-value scheduler 'halted) t)
  (when abort
    (when (slot-value scheduler 'thread)
      (ignore-errors (kill-thread (slot-value scheduler 'thread)))
      (setf (slot-value scheduler 'thread) nil))))

(defmethod reset((scheduler scheduler))
  (stop scheduler :abort t)
  (setf (clock scheduler) 0.0d0)
  (let ((q (event-queue scheduler)))
    (while (not (empty-p q)) (dequque q))))

(defun run(scheduler &key (granularity 10000) step )
  "Execute the scheduler returning the running
thread. granularity is the number of event to dispatch before
yielding the thread (default 10000). If granularity is nil all events
are dispatched in current thread"
  (flet((run(scheduler)
          (setf (halted scheduler) nil)
          (loop
           :with c = 1
           :with q = (event-queue scheduler)
           :for event = (dequeue q)
           :while (and event (not (halted scheduler)))
           :do (progn
                 (setf (clock scheduler) (event-time event))
                 (when step (funcall step (handler event)))
                 (handle event))
           :when (and granularity
                      (= (setf c (mod (1+ c) granularity)) 0))
           :do (yield-thread))
          (setf (halted scheduler) t)))
    (stop scheduler :abort t)
    (if granularity
        (setf (slot-value scheduler 'thread)
              (make-thread
               #'(lambda()
                   (run scheduler)
                   (setf (slot-value scheduler 'thread) nil))
               "LENS Scheduler"))
        (funcall #'run scheduler))))


(defmethod status(scheduler)
  (if (scheduler-thread scheduler)
      (if (halted scheduler) :halting :running)
      :halted))

;;interactive scheduling commands

(declaim (inline simulation-time))
(defun simulation-time(&optional (scheduler (scheduler))) (clock scheduler))

(eval-when(:load-toplevel :execute)
  (unless *scheduler* (setf *scheduler* (make-instance 'scheduler)))
  (pushnew *scheduler* *reset-hooks*))

;; Mixin class to maintain timers. Timers are named
;; functions which take the object instance as the first argument
;; and any number of other arguments.

;; implementation subject to change for efficieny reasons (i.e. to
;; reduce length of scheduler queue) so subclasses should not access timers
;; slot directly.

(defclass timer(event)
  ((name :initarg :name :reader name)
   (manager :initarg :manager :reader timer-manager))
  (:documentation "A timer event"))

(defgeneric active-p(event)
  (:documentation "Return true if entiti is active")
  (:method((event event) (>= (rank event) 0))))

(defmethod handle((timer timer))
  (apply (name timer) (cons (timer-manager timer) (handler timer))))

(defclass timers-manager()
  ()
  (:documentation "Class which maintaines a set of timers"))

(defgeneric timers(object)
  (:documentation "Return list of timer names managed by an object"))

(defmethod initialize-instance :after ((m timers-manager) &key &allow-other-keys)
  (dolist(timer (timers m))
    (setf (slot-value m timer) (make-instance 'timer :name timer :manager m))))

(defun schedule-timer(delay timer object &rest args)
  "Schedule a timer function on object instance time seconds in future."
  (let ((timer (slot-value object timer)))
    (setf (handler timer) args)
    (schedule delay timer)))

(defmethod reset :after ((object timers-manager))
  (dolist(event (timers object)) (cancel event)))


(defmacro with-delay((delay) &body body)
  "Execute body scheduled by delay seconds, or immediately if delay is
0 The body is used to fporm a closure - care should be taken that the
variables captured by that closure cannot be changed between the
lexecial and execution context"
  (let ((f (gensym "delayed-body"))
        (d (gensym)))
    `(flet((,f() ,@body))
      (let ((,d ,delay))
        (if (zerop ,d) (funcall #',f) (schedule ,d #',f))))))