(in-package :lens.wsn)

(defclass wsn-packet(packet)
  ((header-overhead
    :initform 0 :accessor header-overhead :initarg :header-overhead :initarg :byte-length
    :documentation "In bytes")
   (source :initarg :source :accessor source
           :documentation "the  source address of the received packet")
   (destination
    :initarg :destination :accessor destination
    :documentation "the destination address of the packet to be sent")
   (sequence-number :initarg :seqnum :initarg :sequence-number
                    :reader sequence-number
                    :documentation "a field to distinguish between packets"))
  (:documentation "Base class for network and link layer packets"))

(defmethod print-object((instance wsn-packet) stream)
  (print-unreadable-object(instance stream :type t :identity nil)
    (when (slot-boundp instance 'name)
      (format stream "~A" (name instance)))
    (when (slot-boundp instance 'sequence-number)
      (format stream " #~D" (sequence-number instance)))
    (format stream " (~D bytes)" (byte-length instance))))

(defmethod duplicate((packet wsn-packet) &optional duplicate)
  (call-next-method)
  (copy-slots
   '(header-overhead source destination sequence-number)
   packet duplicate))

(defmethod byte-length((packet wsn-packet))
  (+ (header-overhead packet)
     (if (slot-boundp packet 'lens::encapsulated-packet)
         (byte-length (slot-value packet 'lens::encapsulated-packet))
         0)))

(defmethod bit-length((packet wsn-packet)) (* 8 (byte-length packet)))

(defclass comms-module(wsn-module)
  ((buffer :type packet-buffer :reader buffer)
   (buffer-size :parameter t :initform 32 :type integer :initarg :buffer-size)
   (packet-history
    :type history-buffer
    :initform (make-instance 'history-buffer
                             :element-type 'packet
                             :key #'(lambda(p)
                                      (cons (source p)
                                            (sequence-number p))))
    :reader packet-history)
   (last-sequence-number :initform -1 :type integer
                         :accessor last-sequence-number)
   (header-overhead
    :type integer :parameter t :initform 10 :reader header-overhead
    :properties (:units "B")
    :documentation "in bytes"))
  (:metaclass module-class))

(defmethod handle-message :around ((module comms-module) (message message))
  (unless (disabled-p module) (call-next-method)))

(defmethod build-submodules((module comms-module))
  (let ((buffer-size (slot-value module 'buffer-size)))
    (setf (slot-value module 'buffer)
            (make-instance 'packet-buffer :owner module :name 'buffer
                           :buffer-size buffer-size))))

(defmethod for-each-submodule((module comms-module) (operator function))
  (funcall operator (buffer module)))

(defmethod shutdown((module comms-module))
  (call-next-method)
  (let ((buffer (buffer module)))
    (while (not (empty-p buffer)))
    (emit buffer 'drop (dequeue (queue buffer)))
    (setf (slot-value buffer 'byte-length) 0)
    (emit buffer 'buffer-length buffer)))

(defgeneric state(instance)
  (:documentation "Return current state of instance"))

(defgeneric set-state(state-machine state &optional description)
  (:documentation "Set new state of a protocol
  implementation. Description may be used to add comments for
  tracing. print-state-transitions parameter slot switches on tracing of this.
state is stored in slate slot.")
  (:method((instance wsn-module) new-state &optional description)
    (with-slots(state print-state-transitions) instance
      (unless (eql state new-state)
        (when print-state-transitions
          (tracelog "state changed from ~A to ~A~@[, reason: ~A~]"
                    state new-state description))
        (setf state new-state)))))
