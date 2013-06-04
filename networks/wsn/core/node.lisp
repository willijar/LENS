(in-package :lens.wsn)

(defclass node(compound-module)
  ((owner :reader network)
   (num-sensors :parameter t :initform 1 :type integer :reader num-sensors)
   (network-address :parameter t :reader network-address)
   (startup-offset
    :parameter t :type time-type :initform 0.0d0 :reader startup-offset
    :documentation "Node offset startup delay in seconds")
   (startup-randomization
    :parameter t :type time-type :initform 0.05 :reader startup-randomization
    :documentation "node startup randomisation, in seconds"))
  (:gates
   (wireless-channel :inout))
  (:submodules
   (sensors num-sensors sensor)
   (application application)
   (communications communications)
   (mobility mobility)
   (resources resources))
  (:connections
   (<=> (application network) (communications application))
   (<=> (communications wireless-channel) wireless-channel))
  (:metaclass compound-module-class))

(defmethod build-connections((node node))
  (call-next-method)
  (let ((application (submodule node 'application)))
    (map nil
         #'(lambda(sensor)
             (connect (gate sensor 'application :direction :output)
                      (gate application 'sensor :direction :input :index '++))
             (connect (gate application 'sensor :direction :output :index '++)
                      (gate sensor 'application :direction :input)))
         (submodule node 'sensors))))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (unless (slot-boundp node 'network-address)
    (setf (slot-value node 'network-address) (index node)))
  (subscribe node 'out-of-memory node)
  (subscribe node 'out-of-energy node))

(defmethod initialize((node node) &optional (stage 0))
  (case stage
    (0
     (schedule-at node (make-instance 'message :name 'node-startup)
                  :delay  (+ (startup-offset node)
                             (* (uniform 0 1) (startup-randomization node))))))
  (call-next-method))

(defmethod handle-message((node node) message)
  (case (name message)
    (node-startup (emit node 'node-startup))
    (node-shutdown (emit node 'node-shutdown))
    (t (call-next-method))))

(defmethod receive-signal((node node) signal source value)
  (case signal
    ((out-of-energy out-of-memory)
     ;; if out of energy or memory stop everything
     (emit node 'node-shutdown value))))

(defmethod location((node node)) (location (submodule node 'mobility)))