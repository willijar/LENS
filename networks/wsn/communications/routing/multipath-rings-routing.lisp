(in-package :lens.wsn)

(defclass multipath-rings-routing(routing)
  ((header-overhead :initform 14)
   (buffer-size :initform 32)
   (max-net-frame-size :initform 0)
   (setup-frame-size
    :type fixnum :parameter t :initform 13 :initarg :setup-frame-size
    :reader setup-frame-size)
   (setup-timeout
    :type time-type :parameter t :initform 50d-3 :initarg :setup-timeout
    :reader setup-timeout)
   (current-sink-id :initform nil :type integer :reader current-sink-id)
   (current-level :initform nil :type integer :reader current-level)
   (setup-timer :type message :initform nil)
   (connected-p :type boolean :initform nil :accessor connected-p)
   (tmp-level :type fixnum :documentation "Best level received during setup")
   (tmp-sink-id :documentation "Used during setup"))
  (:metaclass module-class))

(defclass multipath-rings-packet(routing-packet)
  ((sink-id :type integer :initarg :sink-id :reader sink-id)
   (level :type fixnum :initarg :sender-level :initarg :level :initform 0
          :reader level :accessor sender-level)))

(defmethod duplicate((packet multipath-rings-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(sink-id level) packet duplicate))

(defclass multipath-rings-setup-packet(multipath-rings-packet)
  ())

(defmethod startup((instance multipath-rings-routing))
  (when (sink-p instance)
    (setf (current-sink-id instance) (network-address instance)
          (current-level instance) 0)
    (send-topology-setup-packet instance))))

(defun send-topology-setup-packet((instance multipath-rings-routing))
  (to-mac
   instance
   (make-instance
    'multipath-rings-routing-setup-packup
    :source (network-address instance)
    :destination broadcast-network-address
    :sink-id (current-sink-id instance)
    :sender-level (current-sender-level instance))
   broadcast-mac-address))

(defun send-control-message((instance multipath-rings-routing) kind)
  (send instance
        (make-instance 'network-control-message :command kind)
        'application))

(defmethod handle-message((instance multipath-rings-routing)
                          (message message))
  (with-slots(setup-timer tmp-level tmp-sink-id current-level current-sink-id)
      instance
  (cond
    ((eql message setup-timer)
     (setf (slot-value instance 'setup-timer) nil)
     (cond
       ((not (slot-boundp instance 'tmp-level))
        (set-timer instance
                   (setf setup-timer (make-instance 'message))
                   (setup-timeout instance)))
       ((not current-level)
        (setf current-level (1+ tmp-level)
              current-sink-id tmp-sink-id)
        (cond
          ((connected-p instance)
           (send-control-message 'tree-level-updated)
           (tracelog "Reconnected to ~A at level ~A"
                     current-sink-id current-level))
          (t
           (setf (connected-p instance) t)
           (send-control-message 'connected-to-tree)
           (tracelog "Connected to ~A at level ~A"
                     current-sink-id current-level)
           (process-buffered-packet instance)))
        (send-topology-setup-packet)))
     (cond
       ((sink-p instance)
        (setf tmp-level 0
              tml-sink-id (network-address self)))
       (t
        (slot-makunbound instance 'tmp-level))))
    (t (call-next-method)))))

(defun process-buffered-packet(instance)
  (while (not (empty-p (buffer instance)))
    (to-mac instance (dequeue (buffer instance)) broadcast-mac-address)))

(defmethod handle-message((instance multipath-rings-routing)
                          (packet application-packet))
  ;; from application layer
  (let* ((destination (destination (control-info packet)))
         (routing-packet
          (encapsulate
           (make-instance 'multipath-rings-packet
                          :name (class-name (class-of instance))
                          :header-overhead (header-overhead instance)
                          :sequence-number (next-sequence-number instance)
                          :source (network-address instance)
                          :destination destination
                          :sink-id (current-sink-id instance)
                          :sender-level (current-level instance))
           packet)))
    (cond
      ((member destination '(sink parent))
       (enqueue routing-packet instance)
       (if (connected-p instance)
           (process-buffered-packet instance)
           (send-control-instance 'not-connected)))
      (t
       (to-mac instance routing-packet broadcast-mac-address)))))

(defmethod handle-message ((instance multipath-rings-routing)
                           (packet multipath-rings-setup-packet))
  ;; from mac layer
  (unless (sink-p instance)
    (with-slots(setup-timer tmp-level tmp-sink-id) instance
      (cond
        (setup-timer
         (setf tmp-level (sender-level packet)
               tmp-sink-id (sink-id packet)
               setup-timer nil)
        (t
         (set-timer instance
                    (setf setup-timer (make-instance 'message))
                    (setup-timeout instance))
         (slot-makunbound instance 'tmp-level)))))))

(defmethod handle-message ((instance multipath-rings-routing)
                           (packet multipath-rings-packet))
  ;; data packet
  (flet((to-app(packet)
          (if (duplicate-p packet (packet-history instance))
              (tracelog "Discarding duplicate pakcet from ~A" (source packet))
              (send instance (decapsulate packet) 'application))))
    (case (destination packet)
      (sink
       (when (= (sender-level packet) (1+ (current-level instance)))
         (cond
           ((eql (sink-id packet) (network-address instance))
            (to-app packet))
           ((eql (sink-id packet) (current-sink-id instance))
            ;; rebroadcast this packet since we are not its destination
						;; copy of the packet is created and sender level field is
						;; updated before calling toMacLayer() function
            (let ((dup-packet (duplicate packet)))
              (setf (sender-level packet) (current-sender-level instance))
              (to-mac instance dup-packet broadcast-mac-address))))))
      (parent
       (when (and (eql (sender-level packet) (1+ (current-level instance)))
                  (eql (sink-id packet) (current-sink-id instance)))
         (to-app packet)))
      (t (call-next-method)))))
