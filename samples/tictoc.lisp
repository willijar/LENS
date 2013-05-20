(in-package :lens)

(defclass Txc1(module)
  ()
  (:gates
   (in :input)
   (out :output))
  (:metaclass module-class))

(defmethod initialize((module Txc1) &optional (stage 0))
  (when (and (zerop stage) (eql (name module) 'tic))
    (send module (make-instance 'message :name 'TicTocMsg) 'out))
  t)

(defmethod handle-message((module Txc1) msg)
  (send module msg 'out))

(defclass TicToc1(network)
  ()
  (:submodules
   (tic Txc1)
   (toc Txc1))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc2(Txc1)
  ()
  (:properties :display (:icon "block/routing"))
  (:metaclass module-class))

(defmethod initialize((module Txc2) &optional (stage 0))
  (when (and (zerop stage) (eql (name module) 'tic))
    (write-line "Sending initial message" *trace-output*)
    (send module (make-instance 'message :name 'TicTocMsg) 'out))
  t)

(defmethod handle-message((module Txc2) msg)
  (format *trace-output* "Received message ~A, sending it out again.~%"
          msg)
  (call-next-method))

(defclass TicToc2(network)
  ()
  (:submodules
   (tic Txc2 :properties (:display (:color "cyan")))
   (toc Txc2 :properties (:display (:color "gold"))))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc3(Txc2)
  ((counter :initform 10 :accessor counter))
  (:metaclass module-class))

(defmethod handle-message((module Txc3) msg)
  (decf (counter module))
  (if (zerop (counter module))
      (format *trace-output*
              "~A's counter reached zero, not resending message.~%"
              module)
      (progn
        (format *trace-output*
                "~A's counter is ~D, sending back message.~%"
                module (counter module))
        (call-next-method))))

(defclass TicToc3(network)
  ()
  (:submodules
   (tic Txc3)
   (toc Txc3))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc4(Txc3)
  ((send-msg-on-init
    :parameter t :initarg :send-msg-on-init :initform nil :type boolean
    :reader send-msg-on-init
    :documentation "Whether module should send message on initialization"))
  (:metaclass module-class))

(defmethod finalize-parameters((instance Txc4))
  (let ((limit (read-parameter instance "limit" 'integer)))
    (when limit (setf (counter instance) limit))))

(defmethod initialize((module Txc4) &optional (stage 0))
  (when (and (zerop stage) (send-msg-on-init module))
    (write-line "Sending initial message" *trace-output*)
    (send module (make-instance 'message :name 'TicTocMsg) 'out))
  t)

(defclass TicToc4(network)
  ()
  (:submodules
   (tic Txc4 :send-msg-on-init t :properties (:display (:color "cyan")))
   (toc Txc4 :send-msg-on-init nil :properties (:display (:color "gold"))))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc5(Txc4)
  ()
  (:metaclass module-class))

