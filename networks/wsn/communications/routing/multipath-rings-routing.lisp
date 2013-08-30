(in-package :lens.wsn)

(defstruct mprings-sink
  (id 0 :type integer)
  (level 0 :type fixnum)) ;; used to store sink data

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
   (current-sink :initform nil :type mprings-sink :reader current-sink)
   (connected-p :type boolean :initform nil :accessor connected-p)
   (tmp-sink :initform nil :type mprings-sink
             :documentation "Used during setup"))
  (:metaclass module-class))

(defmethod sink-network-address((instance routing))
  (sink-network-address (submodule (parent-module instance) 'application)))

(defmethod parent-network-address((instance routing))
  (parent-network-address (submodule (parent-module instance) 'application)))

(defclass multipath-rings-routing-packet(routing-packet)
  ((sink :type mprings-sink :initarg :sink :accessor sink))
  (:documentation "name is either data or topology-setup.
	DATA packet overhead contains all fields, making its total size 13 bytes
	SETUP packet does not contain sequence number field, making its size 12 bytes"))

(defclass multipath-rings-routing-control-message(network-control-message)
  ())

(defmethod duplicate((packet multipath-rings-routing-packet) &optional duplicate)
  (setf (sink duplicate) (copy-mprings-sink (sink packet)))
  (call-next-method))

(defmethod startup((instance multipath-rings-routing))
  (when (sink-p instance)
    (setf (slot-value instance 'current-sink)
          (make-mprings-sink :id  (nodeid (node instance))))
    (setf (connected-p instance) t)
    (send-topology-setup-packet instance)))

(defun send-topology-setup-packet(instance)
  (to-mac
   instance
   (make-instance
    'multipath-rings-routing-packet
    :name 'topology-setup
    :sink (copy-mprings-sink (current-sink instance))
    :source (network-address instance)
    :destination broadcast-network-address)
   broadcast-mac-address))

(defun send-control-message(instance kind)
  (send instance
        (make-instance 'multipath-rings-routing-control-message
                       :command kind
                       :argument (current-sink instance))
        'application))

(defun process-buffered-packets(instance)
  (while (not (empty-p (buffer instance)))
    (to-mac instance (dequeue (buffer instance)) broadcast-mac-address)))

(defmethod handle-timer((instance multipath-rings-routing)
                        (timer (eql 'topology-setup)))
  (with-slots(tmp-sink current-sink connected-p) instance
    (cond
      ((not tmp-sink)
       (set-timer instance 'topology-setup (setup-timeout instance)))
      ((not current-sink)
       ;; Broadcast to all nodes of currentLevel-1
       (setf current-sink
             (make-mprings-sink :id (mprings-sink-id tmp-sink)
                              :level (1+ (mprings-sink-level tmp-sink))))
       (if connected-p
           (progn
             (send-control-message instance 'tree-level-updated)
             (tracelog "Reconnected to ~A" current-sink))
           (progn
             (setf connected-p t)
             (send-control-message instance 'connected-to-tree)
             (tracelog "Connected to ~A" current-sink)
             (process-buffered-packets instance)))
       (send-topology-setup-packet instance)))
    (setf tmp-sink
          (when (sink-p instance)
            (make-mprings-sink :id (nodeid (node instance)) :level 0)))))

(defmethod handle-message((instance multipath-rings-routing)
                          (packet application-packet))
  ;; from application layer
  (let* ((destination (destination (control-info packet)))
         (routing-packet
          (encapsulate
           (make-instance 'multipath-rings-packet
                          :name 'data
                          :header-overhead (header-overhead instance)
                          :source (network-address instance)
                          :destination destination
                          :sink (copy-mprings-sink (current-sink instance)))
           packet)))
    (cond
      ((eql destination (sink-network-address instance))
       (setf (slot-value instance 'sequence-number)
             (next-sequence-number instance))
       (when (enqueue routing-packet instance)
         (if (connected-p instance)
           (process-buffered-packets instance)
           (send-control-message instance 'not-connected))))
      (t
       (to-mac instance routing-packet broadcast-mac-address)))))

(defmethod handle-message ((instance multipath-rings-routing)
                           (packet multipath-rings-routing-packet))
  ;; from mac layer
  (ecase (name packet)
    (topology-setup
       (unless (sink-p instance)
         (with-slots(tmp-sink net-setup-timeout) instance
           (when (not (timer instance 'topology-setup))
             (set-timer instance 'topology-setup net-setup-timeout)
             (setf tmp-sink nil))
           (when (or (not tmp-sink)
                     (> (mprings-sink-level tmp-sink)
                        (mprings-sink-level packet)))
             (setf tmp-sink (copy-mprings-sink (sink packet)))))))
    (data
     (let ((destination (destination packet))
           (sender-level (mprings-sink-level (sink packet)))
           (sink-id (mprings-sink-id (sink packet)))
           (current-level (mprings-sink-level (current-sink instance)))
           (current-sink-id  (mprings-sink-id (current-sink instance))))
       (cond
         ((or (eql destination (network-address instance))
              (eql destination broadcast-network-address))
          (send instance (decapsulate packet) 'application))
         ((eql destination (sink-network-address instance))
          (when (eql sender-level (1+ current-level))
            (cond
              ((eql sink-id (nodeid (node instance)))
               ;;Packet is for this node, if filter passes, forward it to application
               (if (duplicate-p packet (packet-history instance))
                   (tracelog "Discarding duplicate packet from ~A" (source packet))
                   (send instance (decapsulate packet) 'application)))
              ((eql sink-id current-sink-id)
               ;; We want to rebroadcast this packet since we are not
               ;; its destination. For this, a copy of the packet is
               ;; created and sender level field is updated before
               ;; calling toMacLayer() function
               (let ((dup (duplicate packet)))
                 (setf (mprings-sink-level (sink packet))
                       (mprings-sink-level (current-sink instance)))
                 (to-mac instance dup broadcast-mac-address))))))
         ((eql destination (parent-network-address instance))
          (when (and (eql sink-id current-sink-id)
                     (eql sender-level (1+ current-level)))
             (if (duplicate-p packet (packet-history instance))
                 (tracelog "Discarding duplicate packet from ~A" (source packet))
                 (send instance (decapsulate packet) 'application)))))))))
