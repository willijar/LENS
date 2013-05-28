(in-package :lens.wsn)

(defclass wireless-channel(module)
  ((path-loss-exponent
    :parmeter t :type real :initform 2.4 :initarg :path-loss-exponent
    :accessor path-loss-exponent
    :documentation " how fast is the signal strength fading")
   (PLd0
    :parameter t :type real :initform 55 :initarg :pld0 :accessor PLd0
    :documentation "path loss at reference distance d0 (in dBm)")
   (d0
    :parameter t :type real :initform 1.0 :initarg :d0 :accessor d0
    :documentation "reference distance d0 (in meters)")
   (sigma
    :parameter t :type real :initform 4.0 :initarg :sigma :accessor sigma
    :documentation "how variable is the average fade for nodes at the same distance from each other. std of a gaussian random variable")
   (bidirectional-sigma
    :parameter t :type real :initform 1.0 :initarg :bidirectional-sigma
    :accessor bidirectional-sigma
    :documentation "how variable is the average fade for link B->A if we know the fade of link A->B. std of a gaussian random variable")
   (signal-delivery-threshold
    :parameter t :type real :initform -100 :initarg :signal-delivery-threshold
    :accessor signal-delivery-threshold
    :documentation "threshold in dBm above which, wireless channel module
 is delivering signal messages to radio modules of  individual nodes")
   (path-loss-map-file
    :parameter t :type pathname :initarg :path-loss-map-file
    :accessor path-loss-map-file
    :documentation "describes a map of the connectivity based on pathloss if defined, then the parameters above become irrelevant")
   (temporal-model-parameters-file
    :parameter t :type pathname :initarg :temporal-model-parameters-file
    :accessor temporal-model-parameters-file
    :documentation "the filename that contains all parameters for  the temporal channel variation model"))
  (:gates
   (toNode :output 0)
   (fromNode :input 0)
   (fromMobilityModule :input))
  (:metaclass module-class)
  (:documentation "The wireless channel module simulates the wireless medium. Nodes sent packets to it and according to various conditions (fading, interference etc) it is decided which nodes can receive this packet."))

(defclass wireless-channel-signal-begin(message)
  ((nodeid :type integer :initarg :nodeid :reader nodeid)
   (power-dBm :type real :initarg :power-dBm :reader power-dBm)
   (frequency :type real :initarg :frequency :reader frequency)
   (bandwidth :type real :initarg :bandwidth :reader bandwidth)
   (modulation-type :initarg :modulation-type :reader modulation-type)
   (encoding-type :initarg :encoding-type :reader encoding-type)))

(defclass wireless-channel-signal-end(message)
  ((nodeid :type integer :initarg :nodeid :reader nodeid)))

(defclass wireless-channel-node-move-msg(message)
  ((nodeid :type integer :initarg :nodeid :reader nodeid)
   (location :type node-location :initarg :location :reader location)))
