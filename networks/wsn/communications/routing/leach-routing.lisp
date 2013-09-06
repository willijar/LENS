(in-package :lens.wsn)

(defpackage :lens.wsn.routing.leach
  (:use :cl :cl-user :lens :lens.wsn)
  (:export #:leach-routing))

(use-package '(:lens.wsn.routing.leach) :lens.wsn)

(in-package :lens.wsn.routing.leach)

(defstruct cluster-head-info
  (src nil)
  (rssi 0.0 :type float))

(defclass leach-routing-packet(routing-packet) ())
(defclass leach-adv-packet(leach-routing-packet) ())
(defclass leach-join-packet(leach-routing-packet) ())
(defclass leach-tdma-packet(leach-routing-packet)
  ((schedule :initarg :schedule :type array)))

(defclass leach-data-packet(leach-routing-packet) ())

(defmethod duplicate((pkt leach-tdma-packet) &optional duplicate)
  (setf (slot-value duplicate 'schedule)
        (slot-value pkt 'schedule))
  (call-next-method))

(defclass aggregate-application-packet(application-packet)
  ())

(defmethod byte-length((pkt aggregate-application-packet))
  (reduce #'+ (mapcar #'byte-length (payload pkt))))

(deftype leach-routing-timer()
  '(member start-round send-adv join-ch make-tdma start-slot end-slot))

(defclass leach-routing(routing)
  ((sink-network-address :type integer :parameter t
                         :reader sink-network-address)
   (header-overhead :initform 14)
   (buffer-size :initform 32)
   (max-net-frame-size :initform 0)
   (percentage :type real :parameter t)
   (round-length :type time-type :parameter t)
   (slot-length :type real :parameter t)

   ;; routing layer packet sizes
   (adv-packet-size :type fixnum :parameter t :initform 9)
   (join-packet-size :type fixnum :parameter t :initform 9)
   (tdma-packet-size :type fixnum :parameter t :initform 150)
   (data-packet-size  :type fixnum :parameter t :initform 9)

   ;; implementation values
   (round-number :type fixnum :initform 0 :accessor round-number)
   (probability :type float :initform 0.0 :accessor probability)
   (sensibility :type float :parameter t :reader sensibility
                :documentation "dBm")
   (aggr-consumption
    :type float :parameter t :reader aggr-consumption
    :documentation "Energy per bit used in transmitting aggregate data packet from cluster head")
   (aggregate-buffer
    :type list :initform nil
    :documentation "Stacked up application packets for sending as aggregate")
   (temp-tx-buffer
    :type list :initform nil
    :documentation "Temp buffer for packets received before cluster formed")
   (cluster-members
    :type list :initform nil :reader cluster-members)
   (cluster-head-candidates
    :type list :initform nil :accessor cluster-head-candidates)
   (powers :type list :initform nil :reader powers)
   (cluster-length :type fixnum :initform 0)

   ;; type
   (cluster-head-p :type boolean :initform nil :reader cluster-head-p)
   (end-form-cluster :type boolean :initform nil)
   (ct-p :type boolean :initform nil))
  (:metaclass module-class))

(defmethod startup((instance leach-routing))
  (dolist(slot-name
           '(cluster-members cluster-head-candidates round-number probability
             cluster-head-p end-form-cluster ct-p cluster-length))
    (setf (slot-value instance slot-name)
          (funcall
           (closer-mop::slot-definition-initfunction
           (find slot-name (closer-mop::class-slots (class-of instance))
                 :key #'closer-mop::slot-definition-name)))))
  (setf (slot-value instance 'powers)
        (sort
         (mapcar #'lens.wsn::tx-level-output-power
                 (lens.wsn::tx-levels (submodule (owner instance) 'radio)))
         #'<))
  (when (not (sink-p instance))
    (set-timer instance 'start-round 0)))

(defmethod handle-message((instance leach-routing)
                          (packet application-packet))
  ;; from application layer
  (unless (sink-p instance)
    (if (cluster-head-p instance)
        (push packet (slot-value instance 'aggregate-buffer))
        (let ((routing-packet
               (encapsulate
                (make-instance
                 'leach-data-packet
                 :name 'data
                 :header-overhead (header-overhead instance)
                 :sequence-number (next-sequence-number instance)
                 :source (network-address instance)
                 :destination (destination (control-info packet)))
                packet)))
          (with-slots(end-form-cluster temp-tx-buffer) instance
            (if end-form-cluster
                (enqueue routing-packet instance)
                (push routing-packet temp-tx-buffer)))))))

(defmethod handle-message((instance leach-routing) (pkt leach-data-packet))
  (cond
    ((and (cluster-head-p instance)
          (eql (destination pkt) (network-address instance)))
     (push (decapsulate pkt) (slot-value instance 'aggregate-buffer)))
    ((and (sink-p instance)
          (eql (destination pkt) (sink-network-address instance)))
     (send instance (decapsulate pkt) 'application))))

(defmethod handle-message((instance leach-routing) (pkt leach-adv-packet))
  (when (not (or (cluster-head-p instance) (sink-p instance)))
    (push (make-cluster-head-info :src (source pkt)
                                  :rssi (rssi (control-info pkt)))
          (slot-value instance 'cluster-head-candidates))))

(defmethod handle-message((instance leach-routing) (pkt leach-join-packet))
  (when (and (cluster-head-p instance)
             (eql (destination pkt) (network-address instance)))
    (push (source pkt) (slot-value instance 'cluster-members))))

(defmethod handle-message((instance leach-routing) (pkt leach-tdma-packet))
  (when (not (or (cluster-head-p instance) (sink-p instance)))
    (let ((schedule (slot-value pkt 'schedule)))
      (setf (slot-value instance 'cluster-length) (length schedule))
      (dotimes(i (length schedule))
        (when (eql (aref schedule i) (network-address instance))
          (to-radio instance '(set-state . sleep))
          (set-timer instance 'start-slot
                     (* i (slot-value instance 'slot-length))))))))

(defmethod handle-timer :before ((instance leach-routing) timer)
  (check-type timer leach-routing-timer))

(defmethod handle-timer((instance leach-routing) (timer (eql 'start-round)))
  (to-radio instance '(set-state . rx))
  (to-radio instance `(set-tx-output ,(reduce #'max (powers instance))))
  (with-slots(end-form-cluster cluster-head-candidates cluster-members
              round-number round-length percentage ct-p cluster-head-p
              probability) instance
    (setf end-form-cluster nil
          cluster-head-candidates nil
          cluster-members nil)
    (cancel-timer instance 'start-slot)
    (when (>= round-number (/ 1 percentage))
      (setf ct-p nil
            cluster-head-p nil))
    (let ((rnd (uniform 0 1))
          (timer (uniform 0 1)))
      (when cluster-head-p
        (setf ct-p nil
              cluster-head-p nil))
      (setf probability
            (cond
              (ct-p 0)
              ((>= round-number (1- (/ 1 percentage)))
               1)
              ((/ percentage
                  (- 1 (* percentage (mod round-number (/ 1 percentage))))))))
      (when (< rnd probability)
        (set-timer instance 'send-adv timer)
        (set-timer instance 'make-tdma (+ 2d0 timer))
        (setf cluster-head-p t))
      (unless cluster-head-p
        (set-timer instance 'join-ch (+ 1.0 timer)))
      (incf round-number)
      (set-timer instance 'start-round round-length))))

(defmethod handle-timer((instance leach-routing) (timer (eql 'send-adv)))
  (to-mac
   instance
   (make-instance 'leach-adv-packet
                  :header-overhead (slot-value instance 'adv-packet-size)
                  :source (network-address instance)
                  :destination broadcast-network-address)
   broadcast-mac-address))

(defmethod handle-timer((instance leach-routing) (timer (eql 'join-ch)))
  (with-slots(cluster-head-candidates) instance
    (when cluster-head-candidates
      (setf cluster-head-candidates
            (sort cluster-head-candidates #'>
                  :key #'cluster-head-info-rssi))
      (to-mac
       instance
       (make-instance
        'leach-join-packet
        :header-overhead (slot-value instance 'join-packet-size)
        :source (network-address instance)
        :destination (cluster-head-info-src (first cluster-head-candidates)))
       broadcast-mac-address))))

(defmethod handle-timer((instance leach-routing) (timer (eql 'make-tdma)))
  (with-slots(cluster-head-candidates cluster-members sensibility
              slot-length cluster-length) instance
    (if cluster-members
        (progn
          (to-mac
           instance
           (make-instance
            'leach-tdma-packet
            :header-overhead (slot-value instance 'tdma-packet-size)
            :source (network-address instance)
            :destination broadcast-network-address
            :schedule (coerce cluster-members 'array))
           broadcast-mac-address)
          (set-timer instance 'start-slot (* cluster-length slot-length)))
        (set-timer instance 'start-slot slot-length))))

(defmethod handle-timer((instance leach-routing) (timer (eql 'start-slot)))
  (with-slots(slot-length cluster-length cluster-members
               cluster-head-candidates) instance
    (set-timer instance 'start-slot
               (if (and (cluster-head-p instance) (not cluster-members))
                   slot-length
                   (* cluster-length slot-length)))
    (cond
      ((cluster-head-p instance)
       (send-aggregate instance)
       (process-buffered-packet instance))
      (t
       (level-tx-power
        instance
        (- (reduce #'max (powers instance))
           (cluster-head-info-rssi (first cluster-head-candidates))))
       (process-buffered-packet instance)
       (set-timer instance 'end-slot slot-length)))))

(defmethod handle-timer((instance leach-routing) (timer (eql 'end-slot)))
  (when (not (or (sink-p instance) (cluster-head-p instance)))
    (to-radio instance '(set-state . sleep))))

(defun send-aggregate(instance)
  (with-slots(aggregate-buffer) instance
    (when aggregate-buffer
    (let ((aggr-packet
           (encapsulate
            (make-instance
             'leach-data-packet
             :source (network-address instance)
             :destination (sink-network-address instance)
             :byte-length (+ 4 (header-overhead instance)))
            (make-instance
             'application-packet
             :byte-length (reduce #'+ aggregate-buffer :key #'byte-length)
             :payload (reverse aggregate-buffer)))))
      ;; draw energy based on size of data
    (emit instance 'energy-consumed
          (* (aggr-consumption instance)  (bit-length aggr-packet)))
    (enqueue aggr-packet instance)
    (setf aggregate-buffer nil)))))

(defun process-buffered-packet(instance)
  (let ((dst (cluster-head-info-src
              (first (slot-value instance 'cluster-head-candidates)))))
    (dolist(pkt (slot-value instance 'temp-tx-buffer))
      (setf (destination pkt) dst)
      (enqueue pkt instance)))
  (setf (slot-value instance 'temp-tx-buffer) nil)
  (while (not (empty-p (buffer instance)))
    (to-mac instance (dequeue (buffer instance)))))

(defun level-tx-power(instance link-budget)
  (to-radio
   instance
   `(set-tx-output . ,(find-if #'(lambda(p) (> p link-budget))
                               (powers instance)))))

