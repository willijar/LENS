(in-package :lens.wsn)

(defclass log-distance(module)
  ((exponent
    :parameter t :type real :initform 2.4 :initarg :exponent
    :reader exponent
    :documentation " how fast is the signal strength fading")
   (PLd0
    :parameter t :type real :initform 55 :initarg :pld0 :reader PLd0
    :documentation "path loss at reference distance d0 (in dBm)")
   (d0
    :parameter t :type real :initform 1.0 :initarg :d0 :reader d0
    :documentation "reference distance d0 (in meters)")
   (sigma
    :parameter t :type real :initform 4.0 :initarg :sigma :reader sigma
    :documentation "how variable is the average fade for nodes at the
    same distance from each other. std of a gaussian random variable")
   (bidirectional-sigma
    :parameter t :type real :initform 1.0 :initarg :bidirectional-sigma
    :accessor bidirectional-sigma
    :documentation "how variable is the average fade for link B->A if
    we know the fade of link A->B. std of a gaussian random variable"))
    (:metaclass module-class)
  (:documentation "Implementation of a log distance based path loss model"))

(defmethod initialize list ((model log-distance)  &optional (stage 0))
  (ecase stage
    (0 nil)
    (1
     (let* ((channel (owner model))
            (cells (cells channel))
            (n (reduce #'* (array-dimensions cells)))
            (signal-delivery-threshold (signal-delivery-threshold channel))
            (max-tx-power (max-tx-power channel))
            (d0 (d0 model))
            (PLd0 (PLd0 model))
            (exponent (exponent model))
            (sigma (sigma model))
            (bidirectional-sigma (bidirectional-sigma model))
            (distance-threshold
             (expt 10 (/ (- max-tx-power signal-delivery-threshold PLd0 (* -3 sigma))
                         (* 10.0 exponent)))))
       (break "distance-threshold=~A signal-delivery-threshold=~A")
       (for(i 0 n)
         (let ((celli (row-major-aref cells i)))
           (push (make-path-loss :destination celli :avg-path-loss 0.0)
                 (cell-path-loss celli))
           (for(j (1+ i) n)
             (let* ((cellj (row-major-aref cells j))
                    (distance (distance (cell-coord celli) (cell-coord cellj))))
               (when (<= distance distance-threshold)
                 (multiple-value-bind(PLd bidirection-pathloss-jitter)
                     (if (< distance (/ d0 10.0))
                         (values 0.0 0.0)
                         (values(+ PLd0 (* 10.0 exponent
                                           (log (/ distance d0) 10.0))
                                   (normal 0.0 sigma))
                                (/ (normal 0.0 bidirectional-sigma) 2.0)))
                   (when (>= (- max-tx-power (+ Pld bidirection-pathloss-jitter))
                             signal-delivery-threshold)
                     (push (make-path-loss
                            :destination cellj
                            :avg-path-loss (+ Pld bidirection-pathloss-jitter))
                           (cell-path-loss celli)))
                   (when (>= (- max-tx-power (- Pld bidirection-pathloss-jitter))
                             signal-delivery-threshold)
                     (push (make-path-loss
                            :destination celli
                            :avg-path-loss (- Pld bidirection-pathloss-jitter))
                           (cell-path-loss cellj))))))))))
     t)))

(defclass loss-map(module)
  ((path :parameter t :type pathname :reader path :initform nil
         :documentation "Path to file with path loss map parameters"))
  (:metaclass module-class)
  (:documentation "Implementation of a file based channel temporal model"))

(defmethod initialize list ((model loss-map) &optional (stage 0))
  (case stage
    (0 nil)
    (1
     (let ((path-loss-map
            (with-open-file(is (merge-pathnames (path model)))
              (read is)))
           (cells (cells (owner model))))
       (flet ((idx(id) (apply #'array-row-major-index cells id)))
         (dolist(row path-loss-map)
           (let ((src (row-major-aref cells (idx (car row)))))
             (dolist(col (rest row))
               (let ((dest (row-major-aref cells (idx (car col)))))
                 (push (make-path-loss :destination dest
                                       :avg-path-loss (cdr dest))
                       (cell-path-loss src)) ))))))
     t)))
