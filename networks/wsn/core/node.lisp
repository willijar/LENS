(in-package :lens.wsn)

(defclass node(compound-module)
  ((owner :reader network)
   (index :reader nodeid)
   (network-address :parameter t :reader network-address)
   (startup-offset
    :parameter t :type time-type :initform 0.0d0 :reader startup-offset
    :documentation "Node offset startup delay in seconds")
   (startup-randomization
    :parameter t :type time-type :initform 0.05d0
    :reader startup-randomization
    :documentation "node startup randomisation, in seconds"))
  (:gates
   (receive :input))
  (:submodules
   (sensor #'(lambda(node) (num-physical-processes (network node))) sensor)
   (application application)
   (communications communications)
   (mobility mobility)
   (resources resources))
  (:connections
   (<=> (application network) (communications application))
   (<= (communications receive) receive))
  (:metaclass compound-module-class))

(defmethod print-object((node node) os)
  (print-unreadable-object(node os :type t :identity nil)
    (when (slot-boundp node 'index)
      (format os "~D~:[(~A)~;~]"
              (nodeid node)
              (eql (nodeid node) (network-address node))
              (network-address node) ))))

(defmethod build-connections((node node))
  (call-next-method)
  (let ((application (submodule node 'application)))
    (map nil
         #'(lambda(sensor)
             (connect (gate sensor 'application :direction :output)
                      (gate application 'sensor :direction :input :index '++))
             (connect (gate application 'sensor :direction :output :index '++)
                      (gate sensor 'application :direction :input)))
         (submodule node 'sensor))))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (unless (slot-boundp node 'network-address)
    (setf (slot-value node 'network-address) (nodeid node)))
  (subscribe node 'out-of-memory node)
  (subscribe node 'out-of-energy node))

(defmethod initialize list ((node node) &optional (stage 0))
  (case stage
    (0
     (let ((delay (+ (startup-offset node)
                     (* (uniform 0.0 (startup-randomization node))))))
       (tracelog "Delaying startup to ~:/eng/s" delay)
       (schedule-at
        node (make-instance 'message :name 'node-startup)
        :delay delay))))
  t)

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