(in-package :lens.wsn)

(defstruct cell
  (coord (make-coord) :type coord)
  (occupation nil :type list)
  (path-loss nil :type list))

(defstruct path-loss
  (destination nil :type (or cell nil))
  (avg-path-loss 0.0 :type float)
  (last-observed-difference-from-avg 0.0 :type float)
  (last-observation-time (simulation-time) :type time-type))

(register-signal
 'fade-depth
 "Signaled to record changes in signal power")

(defclass wireless-channel(module)
  ((cell-size
    :type coord :parameter t :initform (make-coord 5.0 5.0 1.0)
    :reader cell-size
    :documentation "Size of cells in each dimension (for mobility)")
   (field
    :type coord :reader field
    :documentation "wireless coverage field (may be larger than network field")
   (path-loss-exponent
    :parameter t :type real :initform 2.4 :initarg :path-loss-exponent
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
    :documentation "how variable is the average fade for nodes at the
    same distance from each other. std of a gaussian random variable")
   (bidirectional-sigma
    :parameter t :type real :initform 1.0 :initarg :bidirectional-sigma
    :accessor bidirectional-sigma
    :documentation "how variable is the average fade for link B->A if
    we know the fade of link A->B. std of a gaussian random variable")
   (signal-delivery-threshold
    :parameter t :type real :initform -100.0 :initarg :signal-delivery-threshold
    :accessor signal-delivery-threshold
    :documentation "threshold in dBm above which, wireless channel
    module is delivering signal messages to radio modules of
    individual nodes")
   (path-loss-map-file
    :parameter t :type pathname :initarg :path-loss-map-file
    :accessor path-loss-map-file
    :documentation "describes a map of the connectivity based on
    pathloss if defined, then the parameters above become irrelevant")
   (temporal-model-parameters-file
    :parameter t :type pathname :initarg :temporal-model-parameters-file
    :accessor temporal-model-parameters-file
    :documentation "the filename that contains all parameters for the
    temporal channel variation model")
   (max-tx-power :type real :initform 0.0 :accessor max-tx-power)
   (receivers
    :type array :reader receivers
    :documentation "an array of lists of receiver gateways affected by
    ongoing transmission.")
   (cells
    :type array :reader cells
    :documentation "an array of cell entities with node occupation and
    path-loss to other cells")
   (location-cells :type hash-table :initform (make-hash-table)
                   :documentation "Cached location cell by node instance"))
  (:default-initargs :num-rngs 3)
  (:gates
   (fromNodes :input))
  (:properties
   (:statistic (fade-depth :default (histogram))))
  (:metaclass module-class)
  (:documentation "The wireless channel module simulates the wireless
  medium. Nodes sent packets to it and according to various
  conditions (fading, interference etc) it is decided which nodes can
  receive this packet."))

(defun coord-cell(coord module)
  "Return row major aref  of cell indicies corresponding to coord for wireless
channel module"
  (let ((cells (cells module)))
    (row-major-aref
     cells
     (apply #'array-row-major-index
            cells
            (mapcar
             #'(lambda(f)
                 (let* ((coord (funcall f coord))
                        (field (funcall f (field module)))
                        (a (max 0 (min coord field))))
                   (when (> coord field)
                     (tracelog "Warning at initialization: node position out of boinds in ~A dimension!" f))
                   (floor a (funcall f (cell-size module)))))
             (load-time-value (list #'coord-x #'coord-y #'coord-z)))))))

(defun location-cell(channel instance)
  (or (gethash instance (slot-value channel 'location-cells))
      (index (node instance))))

(defun (setf location-cell)(index channel instance)
  (setf (gethash instance (slot-value channel 'location-cells)) index))

(defmethod initialize list ((wireless wireless-channel) &optional (stage 0))
  (let* ((nodes (nodes (network *simulation*))))
    (case stage
      (0
       (map 'nil #'(lambda(node) (subscribe node 'node-move wireless)) nodes)
       (setf (slot-value wireless 'receivers)
             (make-array (length nodes)
                         :element-type 'list
                         :initial-element nil))
       nil)
      (1
       (if (every #'(lambda(node) (static-p (submodule node 'mobility)))
                  nodes)
           (progn  ;; all static then one node per cell
             (setf (slot-value wireless 'cells) (make-array (length nodes)))
             (map 'nil
                  #'(lambda(node)
                      (let ((idx (index node)))
                        (setf (location-cell wireless node)
                              (setf (aref (cells wireless) idx)
                                    (make-cell :coord (location node)
                                               :occupation (list node))))))
                  nodes))
           (with-slots(field cell-size cells) wireless
             (setf field
                   (coord-op #'(lambda(f) (if (<= f 0.0) 1.0 f))
                          (field (network wireless))))
             (setf cell-size
                   (coord-op #'(lambda(f c)  (if (<= c 0.0) f (min f c)))
                             field cell-size))
             (let ((dimensions
                    (let ((d (coord-op #'/ field cell-size)))
                      (mapcar #'ceiling
                              (list (coord-x d) (coord-y d) (coord-z d))))))
               (setf (slot-value wireless 'cells) (make-array dimensions))
               (dotimes(i (first dimensions))
                 (let ((x (* i (coord-x cell-size))))
                   (dotimes(j (second dimensions))
                     (let ((y (* j (coord-y cell-size))))
                       (dotimes(k (third dimensions))
                         (let ((z (* k (coord-z cell-size))))
                           (setf (aref cells i j k)
                                 (make-cell
                                  :coord (make-coord x y z))))))))))
               (map 'nil
                    #'(lambda(node)
                        (let ((cell (coord-cell (location node) wireless)))
                          (setf (location-cell wireless node) cell)
                          (push node (cell-occupation cell))))
                    nodes)))
       (with-slots(max-tx-power signal-delivery-threshold
                   d0 PLd0 sigma path-loss-exponent bidirectional-sigma)
           wireless
         (let* ((distance-threshold
                 (expt 10 (/ (- max-tx-power signal-delivery-threshold PLd0
                                (* -3 sigma))
                             (* 10.0 path-loss-exponent))))
                (cells (cells wireless))
                (no-cells (reduce #'* (array-dimensions cells))))
           (for (i 0 no-cells)
             (let ((celli (row-major-aref cells i)))
               ;; path loss to self is zero
               (push (make-path-loss :destination celli :avg-path-loss 0.0)
                     (cell-path-loss (row-major-aref cells i)))
               (for (j (1+ i) no-cells)
                 (let* ((cellj (row-major-aref cells j))
                        (distance (distance (cell-coord celli)
                                            (cell-coord cellj))))
                   (when (<= distance distance-threshold)
                     (multiple-value-bind(PLd bidirection-pathloss-jitter)
                         (if (< distance (/ d0 10.0))
                             (values 0.0 0.0)
                             (values (+ PLd0 (* 10.0 path-loss-exponent
                                                (log (/ distance d0) 10.0))
                                        (normal 0.0 sigma))
                                     (/ (normal 0.0 bidirectional-sigma) 2.0)))
                       (when (>=
                              (- max-tx-power Pld bidirection-pathloss-jitter)
                              signal-delivery-threshold)
                         (push (make-path-loss
                                :destination cellj
                                :avg-path-loss
                                (+ Pld bidirection-pathloss-jitter))
                               (cell-path-loss (row-major-aref cells i))))
                       (when (>= (- max-tx-power
                                    (- Pld bidirection-pathloss-jitter))
                                 signal-delivery-threshold)
                         (push (make-path-loss
                                :destination celli
                                :avg-path-loss
                                (- Pld bidirection-pathloss-jitter))
                               (cell-path-loss (row-major-aref cells j))))))))))
           (tracelog "Number of space cells: ~A" no-cells)
           (tracelog "Each cell affects ~f other cells on average."
                     (/ (loop :for i :from 0 :below no-cells
                           :sum (length  (cell-path-loss (row-major-aref cells i))))
                        no-cells))))
       (parse-path-loss-map wireless)
       ;; TODO temporal model and maybe generate on demand.
       t))))

(defun parse-path-loss-map(wireless-channel)
  "Format is list of lists of transmitter id and cons's of receiver id
and pathloss e.g. ((transmitterid (receiverid . loss) (receiverid
. loss) ... ) ... )"
  (unless (slot-boundp wireless-channel 'path-loss-map-file)
    (return-from parse-path-loss-map))
  (let ((path-loss-map
         (with-open-file(is (merge-pathnames
                             (path-loss-map-file wireless-channel)))
           (read is)))
        (cells (cells wireless-channel)))
    (flet ((idx(id)  (apply #'array-row-major-index cells id)))
      (dolist(row path-loss-map)
        (let ((src (row-major-aref cells (idx (car row)))))
          (dolist(col (rest row))
            (let ((dest (row-major-aref cells (idx (car col)))))
              (update-path-loss-element src dest (cdr dest)))))))))

(defun update-path-loss-element(src-cell dest-cell pathloss-db)
  (let ((path-loss (find dest-cell (cell-path-loss src-cell)
                         :key #'path-loss-destination)))
    (if path-loss
        (setf (path-loss-avg-path-loss path-loss) pathloss-db)
        (push (make-path-loss :destination dest-cell :avg-path-loss pathloss-dB)
              (cell-path-loss src-cell)))))

;; we encapsulate transmitted data in wireless-end message

(defclass wireless-signal-start(message)
  ((src :initarg :src :reader src
       :documentation "Source ID for this signal")
   (power-dBm :type real :initarg :power-dBm :accessor power-dBm)
   (carrier-frequency :type real :initarg :carrier-frequency
                      :reader carrier-frequency)
   (bandwidth :type real :initarg :bandwidth :reader bandwidth)
   (modulation :initarg :modulation :reader modulation)
   (encoding :initarg :encoding :reader encoding)))

(defmethod print-object((msg wireless-signal-start) os)
  (print-unreadable-object(msg os :type t :identity nil)
    (format os "~5,2fdBm from ~A" (power-dbm msg)  (nodeid (node (src msg))))))

(defmethod duplicate((original wireless-signal-start) &optional
                     (duplicate (make-instance 'wireless-signal-start)))
  (call-next-method)
  (copy-slots
   '(src power-dbm carrier-frequency bandwidth modulation encoding)
   original duplicate))

(defclass wireless-signal-end(packet)
  ((src :initarg :src :reader src
        :documentation "Source ID must match signal start source id")
   (header-overhead :type integer :initform 0 :reader header-overhead
                    :initarg :header-overhead)))

(defmethod byte-length((pkt wireless-signal-end))
  (+ (header-overhead pkt)
     (byte-length (slot-value pkt 'lens::encapsulated-packet))))

(defmethod bit-length((pkt wireless-signal-end))
  (* 8 (byte-length pkt)))

(defmethod print-object((msg wireless-signal-end) os)
  (print-unreadable-object(msg os :type t :identity nil)
    (format os "from ~A"  (nodeid (node (src msg))))))

(defmethod duplicate((original wireless-signal-end) &optional
                     (duplicate (make-instance 'wireless-signal-end)))
  (call-next-method)
  (copy-slots '(src) original duplicate))

(defmethod receive-signal((wireless wireless-channel) (signal (eql 'node-move))
                          (mobility mobility) value)
  (declare (ignore value))
  (let* ((node (node mobility))
         (old-cell (location-cell wireless node))
         (new-cell (coord-cell (location mobility) wireless)))
    (unless (eql old-cell new-cell)
      (setf (cell-occupation old-cell)
            (delete node (cell-occupation old-cell)))
      (push node (cell-occupation new-cell))
      (setf (location-cell wireless (node mobility)) new-cell))))

(defmethod handle-message((wireless wireless-channel)
                          (message wireless-signal-start))
  (let* ((src-node (node (src message)))
         (cell-tx (location-cell wireless src-node))
         (nodeid (nodeid src-node))
         (reception-count 0))
    (dolist(path-loss (cell-path-loss cell-tx))
      (unless (cell-occupation (path-loss-destination path-loss))
        (go next))
      (let ((current-signal-received
             (- (power-dbm message) (path-loss-avg-path-loss path-loss))))
        ;; TODO temporal model
        (when (< current-signal-received (signal-delivery-threshold wireless))
          (go next))
            ;; go through all nodes in that cell and send copy of message
        (dolist(node (cell-occupation (path-loss-destination path-loss)))
          (unless (eql node src-node)
            (incf reception-count)
            (let ((msgcopy (duplicate message))
                  (receiver (gate node 'receive :direction :input)))
              (setf (power-dbm msgcopy) current-signal-received)
              (send-direct wireless receiver msgcopy)
              (push receiver (aref (receivers wireless) nodeid))))))
      next)
    (when reception-count
      (tracelog "Signal from ~A reached ~D other nodes."
                src-node reception-count))))

(defmethod handle-message((wireless wireless-channel)
                          (message wireless-signal-end))
  (let ((src-node (node (src message))))
    (dolist(receiver (aref (receivers wireless) (nodeid src-node)))
      (send-direct wireless receiver (duplicate message)))
    (setf (aref (receivers wireless) (nodeid src-node)) nil)))
