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
(defvar *time-format* "~6,3f"  "Time output format control")

(declaim (inline scheduler))
(defun scheduler() *scheduler*)

(deftype time-type() 'double-float)

(defclass event()
  ((rank :type fixnum :initform -1
         :documentation "Rank in priority queue")
   (event-time :initarg :time :type time-type :accessor event-time
	 :initform -1.0d0
	 :documentation "simulation time at which event is to be handled")
   (event-id :type integer :documentation "Used to ensure events with
   same time are scheduled in order of scheduling" :reader uid)
   (last-event-id :type integer :initform 0 :allocation :class))
  (:documentation "Class representing a scheduled event"))

(defclass simple-event(event)
  ((handler :reader handler :initarg :handler
            :documentation "The handler function for this event"))
  (:documentation "Class for simple events where handler is invoked"))

(defun event-rank(a &optional b)
  (if b (setf (slot-value a 'rank) b) (slot-value a 'rank)))

(defun event<(a b)
  (let ((ta (event-time a))
        (tb (event-time b)))
    (cond
      ((< ta tb))
      ((= ta tb) (< (slot-value a 'event-id) (slot-value b 'event-id))))))

(defmethod busy-p((event event)) (>= (slot-value event 'rank) 0))

(defmethod print-object((event event) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "~D T=~,5f" (uid event) (event-time event) )))

(defgeneric handle(entity)
  (:documentation "Method called by scheduler on a scheduled entity")
  (:method((f function)) (funcall f))
  (:method((form list)) (apply (first form) (rest form)))
  (:method((event simple-event)) (handle (handler event))))

(defclass scheduler ()
  ((clock :type time-type :initform 0.0d0
	  :accessor clock :initarg :start-time
	  :documentation "simulator virtual time")
   (halted :type boolean :initform t :accessor halted)
   (thread :initform nil :reader scheduler-thread
	    :documentation "Thread running scheduler")
   (event-queue
    :initform
    (make-binary-heap
     :initial-size 1024
     :extend-size 1.4
     :element-type 'event
     :comp-fn #'event<
     :index #'event-rank))))

(defmethod print-object((scheduler scheduler) stream)
  (print-unreadable-object (scheduler stream :type t :identity t)
     (format stream "time:~f~:[~; HALTED~] ~D pending events"
	     (clock scheduler) (halted scheduler)
       (alg:size (slot-value scheduler 'event-queue)))))

(defgeneric schedule(delay handler)
  (:documentation "Schedule event on current scheduler withy delay")
  (:method((delay number) (event event))
    (when (busy-p event)
      (error "Attempt to schedule an already active event ~A" event))
    (when (< delay 0)
      (error  "Attempt to schedule ~S ~D seconds in the past"
              event delay))
    (let ((scheduler (scheduler)))
      (setf (slot-value event 'event-time) (+ delay (simulation-time))
            (slot-value event 'event-id)
            (incf (slot-value event 'last-event-id)))
      (enqueue event (slot-value scheduler 'event-queue)))
    event)
  (:method ((delay number) handler)
    (schedule delay (make-instance 'simple-event :handler handler))))

(defgeneric cancel(event scheduler)
  (:documentation "Cancel an event")
  (:method((event event) (scheduler scheduler))
    (when (>= (slot-value event 'rank) 0)
      (alg:delete event (slot-value scheduler 'event-queue)))))

(defmethod stop((event event) &key &allow-other-keys)
  (cancel event (scheduler)))

(defmethod reset((event event))
  (stop event))

(defmethod stop ((scheduler scheduler) &key abort)
  (setf (slot-value scheduler 'halted) t)
  (when abort
    (when (slot-value scheduler 'thread)
      (ignore-errors (kill-thread (slot-value scheduler 'thread)))
      (setf (slot-value scheduler 'thread) nil))))

(defmethod reset((scheduler scheduler))
  (stop scheduler :abort t)
  (setf (clock scheduler) 0.0d0)
  (let ((q  (slot-value scheduler 'event-queue)))
    (while (not (empty-p q)) (dequeue q))))

(defun run(scheduler &key (granularity 10000) step )
  "Execute the scheduler returning the running
thread. granularity is the number of event to dispatch before
yielding the thread (default 10000). If granularity is nil all events
are dispatched in current thread"
  (flet((run(scheduler)
          (setf (halted scheduler) nil)
          (loop
           :with c = 1
           :with q =  (slot-value scheduler 'event-queue)
           :while (not (or (empty-p q) (halted scheduler)))
           :for event = (dequeue q)
           :do (progn
                 (setf (clock scheduler) (event-time event))
                 (when step (funcall step event))
                 (handle event))
           :when (and granularity
                      (= (setf c (mod (1+ c) granularity)) 0))
           :do (yield-thread))
          (format lens-user::*user-output*
                  "~%-- Simulation halted at ~,4f~%" (simulation-time))
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

(defmethod busy-p((scheduler scheduler))
  (and (scheduler-thread scheduler) (not (halted scheduler))))

;;interactive scheduling commands

(declaim (inline simulation-time))
(defun simulation-time(&optional (scheduler (scheduler))) (clock scheduler))

(eval-when(:load-toplevel :execute)
  (unless *scheduler* (setf *scheduler* (make-instance 'scheduler)))
  (pushnew *scheduler* *reset-hooks*))

;; Support for timers - these are named slots in an instancer of
;; 'with-timers' which have type timer. They are scheduled as any
;; other event, can only be scheduled once and the generic function
;; timeout is called with timer name and instance value after the
;; scheduled delay.

(defclass timer(event)
  ((name :initarg :name :reader name
         :documentation "Slot name for instance in handler object")
   (timer-delay :initarg :delay :reader timer-delay
                :documentation "Default delay for this timer")
   (time-scheduled :type time-type))
  (:documentation "A timer event"))

(defmethod copy ((timer timer) &optional
                 (copy (make-instance (class-of timer))))
  ;; we initialise scheduler used slots as normal and copy other slots only
  (copy-slots timer copy :slot-names '(name timer-delay handler)))

(defmethod schedule((delay number) (timer timer))
  (setf (slot-value timer 'time-scheduled) (simulation-time))
  (call-next-method))

(defmethod handle((timer timer))
  (timeout (name timer) (handler timer)))

(defclass with-timers()
  ()
  (:documentation "Class which maintaines a set of timers"))

(defmethod schedule((timer symbol) (entity with-timers))
  (let ((timer (slot-value entity timer)))
    (schedule (timer-delay timer) timer)))

(defgeneric timers(entity)
  (:documentation "Return timers associated with entity")
  (:method((obj with-timers))
    (mapcan #'(lambda(slot)
                (when (subtypep (slot-definition-type slot) 'timer)
                  (list (slot-definition-name slot))))
            (class-slots (class-of obj)))))

(declaim (inline timer))
(defun timer(name entity) (slot-value entity name))

(defgeneric timeout(timer handler)
  (:documentation "Called when a timer has finished. timer is the name
  of the timer (slot) in the handler object"))

(defmethod cancel((timer symbol) (object with-timers))
  (cancel (slot-value object timer) (scheduler)))

(defmethod cancel((timer (eql :all)) (object with-timers))
  (dolist(timer (timers object))
    (cancel timer object)))

(defmethod initialize-instance :after ((obj with-timers) &key &allow-other-keys)
  (dolist(timer-name (timers obj))
    (if (slot-boundp obj timer-name)
        (let ((timer (slot-value obj timer-name)))
          (setf (slot-value timer 'handler) obj
                (slot-value timer 'name) timer-name))
        (setf (slot-value obj timer-name)
              (make-instance 'timer :name timer-name :handler obj)))))

(defmethod copy :around ((object with-timers) &optional
                         (copy (call-next-method)))
  (dolist(timer (timers copy))
    (setf (slot-value (slot-value copy timer) 'handler) copy)))

(defmethod reset ((object with-timers))
  (cancel :all object))

(defmacro with-delay((delay) &body body)
  "Execute body scheduled by delay seconds, or immediately if delay is
0 The body is used to form a closure - care should be taken that the
variables captured by that closure cannot be changed between the
lexical and execution context"
  (let ((f (gensym "delayed-body"))
        (d (gensym)))
    `(flet((,f() ,@body))
      (let ((,d ,delay))
        (if (zerop ,d) (funcall #',f) (schedule ,d #',f))))))