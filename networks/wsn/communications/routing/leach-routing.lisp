(in-package :lens.wsn)

(defstruct cluster-head-info
  (src nil)
  (rssi 0.0 :type float))

(defclass leach-routing(routing)
  ((header-overhead :initform 14)
   (buffer-size :initform 32)
   (max-net-frame-size :initform 0)
   (percentage :type real :parameter t :reader percentage)
   (round-length :type time-type :parameter t :reader round-length)
   (slot-length :type real :parameter t :reader slot-length)
   (adv-packet-size
    :type fixnum :parameter t :initform 9 :reader adv-packet-size)
   (join-packet-size
    :type fixnum :parameter t :initform 9 :reader join-packet-size)
   (tdma-packet-size
    :type fixnum :parameter t :initform 150 :reader tdma-packet-size)
   (data-packet-size
    :type fixnum :parameter t :initform 9 :reader data-packet-size)
   (start-round-timer :initform (make-instance 'message :name 'start-round)
                      :reader start-round-timer)
   (send-adv-timer :initform (make-instance 'message :name 'send-adv)
                      :reader send-adv-timer)
   (join-ch-timer :initform (make-instance 'message :name 'join-ch)
                      :reader join-ch-timer)
   (make-tdma-timer :initform (make-instance 'message :name 'make-tdma)
                      :reader make-tdma-timer)
   (start-slot-timer :initform (make-instance 'message :name 'start-slot)
                      :reader start-slot-timer)
   (end-slot-timer :initform (make-instance 'message :name 'end-slot)
                   :reader end-slot-timer)
   (round-number :type fixnum :initform 0 :accessor round-number)
   (probability :type float :initform 0.0 :accessor probability)
   (sensibility :type float :parameter t :reader sensibility
                :documentation "dBm")
   (aggr-consumption :type float :parameter t :reader aggr-consumption)
   (buffer-aggregate
    :type array :initform (make-array 0 :adjustable t :fill-pointer 0)
    :reader buffer-aggregate)
   (cluster-members
    :type array :initform (make-array 0 :adjustable t :fill-pointer 0)
    :reader cluster-members)
   (cluster-head-candidates :type list :initform nil
                            :accessor cluster-head-candidates)
   (tmp-tx-buffer :type list :initform nil
                  :reader tmp-tx-buffer)
   (powers
  (:metaclass module-class))

(defclass leach-routing-packet(routing-packet)
  ((schedule :type array :reader schedule))

(defmethod duplicate((packet leach-routing-packet) &optional duplicate)
  (let ((duplicate (copy-next-method)))
    (setf (slot-value duplicate 'schedule) (copy-seq (schedule packet)))
    duplicate))

(defmethod startup((instance leach-routing))
  (setf (cluster-head-candidates instance) nil
        (round-number instance) 0
        (probability instance) 0.0
        (fill-pointer (buffer-aggregate instance)) 0
        (fill-pointer (cluster-members instance)) 0)
  (setf (powers instance)
        (sort
         (mapcar #'tx-level-output-powers
                 (tx-levels (submodule (parent instance) 'radio)))
         #'>)

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
