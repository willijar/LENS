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

(defclass scheduler ()
  ((clock :type time-type :initform 0.0d0
	  :accessor clock :initarg :start-time
	  :documentation "simulator virtual time")
   (halted :type boolean :initform t :accessor halted)
   (thread :initform nil :reader scheduler-thread
	    :documentation "Thread running scheduler")
   (last-event-uid :type counter :initform 0 :accessor last-event-uid
                   :documentation "uid for last scheduled event ")))

(defclass event()
  ((event-time :initarg :time :type time-type :accessor event-time
	 :initform -1.0d0
	 :documentation "simulation time at which event handler is to be accled")
   (event-uid
    :initform (incf (last-event-uid (scheduler)))
    :type counter :reader event-uid
    :documentation
    "unique id for events to ensure events are deterministically sorted")
   (handler :reader handler :initarg :handler
            :documentation "The handler function for this event"))
  (:documentation "Class representing a scheduled event"))

(defmethod print-object((event event) stream)
  (print-unreadable-object (event stream :type t :identity t)
     (format stream "~D T=~,2f"
	     (event-uid event)
	     (event-time event) )))

(defun event<(a b)
  (declare (type event a) (type event b))
  (or (< (event-time a) (event-time b))
      (when (= (event-time a) (event-time b))
        (< (event-uid a) (event-uid b)))))

(defgeneric handle(entity)
  (:documentation "Method called by scheduler on a scheduled entity")
  (:method((f function)) (funcall f))
  (:method((form list)) (apply (first form) (rest form)))
  (:method((event event))
    #+debug(format *debug-io* "handle: ~,3f: ~S~%"
                   (simulation-time) (handler event))
    (handle (handler event))))

(defmethod print-object((scheduler scheduler) stream)
  (print-unreadable-object (scheduler stream :type t :identity t)
     (format stream "time:~f~:[~; HALTED~] last-event-id:~D"
	     (clock scheduler) (halted scheduler)
	     (last-event-uid scheduler))))

(defun schedule(delay handler)
  (assert (>= delay 0)
          (delay)
          "Attempt to schedule ~S ~D seconds in the past"
          handler delay)
  (let* ((scheduler (scheduler))
         (event (make-instance 'event
                               :time (+ (clock scheduler) delay)
                               :handler handler)))
    #+debug(format *debug-io* "Schedule: ~,3d (~,4f): ~S~%"
                   delay (+ (clock scheduler) delay) handler)
    (queues:insert event scheduler)
    event))

(defgeneric cancel(event)
  (:documentation "Cancel a scheduled event")
  (:method ((event event)) (extract event (scheduler)))
  (:method(handler)
    (extract #'(lambda(event) (eql (handler event) handler)) (scheduler))))

(defun cancel-if(test)
  "Cancel an event on queue for which test, a function which takes a
handler, returns true. Returns canceled event"
  (extract
   #'(lambda(event) (funcall test (handler event)))
   *scheduler*))

(defmethod stop ((scheduler scheduler) &key abort)
  (setf (slot-value scheduler 'halted) t)
  (when abort
    (when (slot-value scheduler 'thread)
      (ignore-errors (kill-thread (slot-value scheduler 'thread)))
      (setf (slot-value scheduler 'thread) nil))))

(defmethod reset((scheduler scheduler))
  (stop scheduler :abort t)
  (setf (clock scheduler) 0.0d0)
  (setf (slot-value scheduler 'last-event-uid) 0)
  (while (extract-head scheduler)))

(defun run(scheduler &key (granularity 10000) step )
  "Execute the scheduler returning the running
thread. granularity is the number of event to dispatch before
yielding the thread (default 10000). If granularity is nil all events
are dispatched in current thread"
  (flet((run(scheduler)
          (setf (halted scheduler) nil)
          (loop
           :with c = 1
           :for event = (extract-head scheduler)
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

(defclass heap-scheduler(scheduler priority-queue)
  ()
  (:default-initargs
   :size 1024 :extend-by 1024
   :element-type 'event :test #'event<
   :adjustable t)
  (:documentation "The priority queue"))

(defmethod print-object((scheduler heap-scheduler) stream)
  (print-unreadable-object (scheduler stream :type t :identity t)
     (format stream "~:[~;HALTED ~]T=~,2f ~D/~D events pending"
	     (halted scheduler)
	     (clock scheduler)
       (queues:size scheduler )
	     (last-event-uid scheduler))))

(defmethod reset((scheduler heap-scheduler))
  (call-next-method)
  (setf (fill-pointer (queues::heap scheduler)) 0))


;;interactive scheduling commands

(declaim (inline simulation-time))
(defun simulation-time(&optional (scheduler (scheduler))) (clock scheduler))

(eval-when(:load-toplevel :execute)
  (unless *scheduler* (setf *scheduler* (make-instance 'heap-scheduler)))
  (pushnew *scheduler* *reset-hooks*))

;; Mixin class to maintain timers. Timers are named
;; functions which take the object instance as the first argument
;; and any number of other arguments.

;; implementation subject to change for efficieny reasons (i.e. to
;; reduce length of scheduler queue) so subclasses should not access timers
;; slot directly.

(defclass timer(event)
  ()
  (:documentation "A timer event"))

(defclass timers-manager()
  ((timers :initform nil :type list :accessor timers
           :documentation "Sequence of current timers"))
  (:documentation "Class which maintaines a set of timers"))

(defmethod handle((event timer))
  (let ((object (second (handler event))))
    (setf (timers object) (delete event (timers object)))
    (call-next-method)))

(defgeneric find-timer(timer object &key from-end)
  (:documentation "Find  and return a timer event for object")
  (:method(timer (object timers-manager) &key from-end)
    (find-if #'(lambda(h) (eql (first h) timer) )
             (timers object)
             :from-end from-end
             :key #'handler)))

(defgeneric schedule-timer(time timer object &rest args)
  (:documentation "Schedule a timer function on object instance time
seconds in future.")
  (:method(delay timer (object timers-manager) &rest args)
    (assert (>= delay 0)
          (delay)
          "Attempt to schedule ~S ~D seconds in the past"
          timer delay)
    (let* ((scheduler (scheduler))
           (event (make-instance 'timer
                                 :time (+ (clock scheduler) delay)
                                 :handler `(,timer ,object ,@args))))
      (setf (timers object)
            (merge 'list (timers object) (list event) #'< :key #'event-time))
      (queues:insert event scheduler)
      event)))

(defgeneric cancel-timer(timer object)
  (:documentation "Cancel all named timers for object")
  (:method(timer (object timers-manager))
    (setf (timers object)
          (mapcan
           #'(lambda(event)
               (if (eql (first (handler event)) timer)
                   (progn (cancel event) nil)
                   (list event)))
           (timers object)))))

(defmethod cancel((event timer))
  (cancel-timer (first (handler event)) (second (handler event))))

(defgeneric cancel-all-timers(object)
  (:documentation  "Cancel all timers on object")
  (:method((object timers-manager))
    (dolist(event (timers object)) (cancel event))
    (set (timers object) nil)))

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