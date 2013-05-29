(in-package :lens.wsn)

(defclass mobility(module)
  ((location :parameter t :type coord :initarg :location :reader location
             :documentation "starting location")
   (cell :type integer :documentation "cell ID coresponding to  x,y,z")
   (theta :parameter t :initform 0.0d0 :type double-float
          :documentation "Orientation provided by angles")
   (phi :parameter t :initform 0.0d0 :type double-float))
  (:metaclass module-class))

(defgeneric node(instance)
  (:method((instance mobility)) (owner instance)))

(defgeneric (setf location)(location instance)
  (:documentation "Change location in mobility manager")
  (:method((location coord) (instance mobility))
    (let ((old (location instance)))
      (setf (slot-value instance 'location) location)
      (emit instance 'node-move
            (make-instance 'node-move :from old :to location)))))

(defmethod initialize((instance mobility) &optional stage)
  (when (zerop stage)
    (parse-deployment instance)
    (eventlog "~A initial location is ~A" (node instance) (location instance)))
  t)

(defun parse-deployment(instance)
  (let* ((network (network instance))
         (node (node instance))
         (xlen (field-x network))
         (ylen (field-y network))
         (zlen (field-z network))
         (location (location instance)))
    (multiple-value-bind(deployment start-index)
        (range-getf (deployment network) (index node))
      (cond
        ((not deployment)
         (setf (coord-x location) (read-parameter node 'xCoor 'real)
               (coord-y location) (read-parameter node 'yCoor 'real)
               (coord-z location) (read-parameter node 'zCoor 'real)))
        ((eql deployment 'uniform)
         (setf (coord-x location) (uniform 0 xlen)
               (coord-y location) (uniform 0 ylen)
               (coord-z location) (uniform 0 zlen)))
        ((eql deployment 'center)
         (setf (coord-x location) (/ xlen 2)
               (coord-y location) (/ ylen 2)
               (coord-z location) (/ zlen 2)))
        ((and (listp deployment) (member (first deployment) '(grid randomized)))
         (let ((gridi (- (index node) start-index))
               (gridx (second deployment))
               (gridy (third deployment))
               (gridz (fourth deployment)))
           (setf (coord-x location)
                 (* (mod gridi gridx) (/ xlen (1- gridx)))
                 (coord-z location)
                 (* (mod (floor gridi gridx) gridy) (/ ylen (1- gridy))))
           (when (and gridy zlen)
             (setf (coord-z location)
                   (* (mod (floor gridi (* gridx gridy)) gridz)
                           (/ zlen (1- gridz)))))
           (when (eql (first deployment) 'randomized)
             (setf (coord-x location)
                   (max (+ (coord-x location)
                           (normal 0 (* 0.3 (/ xlen gridx))))
                        xlen)
                   (coord-y location)
                   (max (+ (coord-y location)
                           (normal 0 (* 0.3 (/ ylen gridy))))
                        ylen))
             (when (and gridy zlen)
               (setf (coord-z location)
                   (max (+ (coord-z location)
                           (normal 0 (* 0.3 (/ zlen gridz))))
                        zlen))))))
        (t (error "Unknown deployment parameter: ~A" deployment))))))

(defclass line-mobility(mobility)
  ((start-location :type coord :reader start-location)
   (destination
    :parameter t :type coord :initform #S(coord)
    :initarg :destination-location)
   (delta :type coord :documentation "Vector delta from start to end")
   (distance :type real
             :documentation "Distance from start to end")
   (speed :parameter t :type real :initform 1 :initarg :speed
          :documentation "Speed of motion")
   (update-interval
    :parameter t :type time-type :reader update-interval :initform 1d-3
    :documentation "Interval for position updates along trajectory")
   (periodic-update-message
    :type message :reader periodic-update-message
    :initform (make-instance 'message :name 'mobility-periodic-update)))
  (:metaclass module-class))

(defmethod initialize((instance line-mobility) &optional (stage 0))
  (prog1
      (call-next-method)
    (when (zerop stage)
      (with-slots(start-location distance delta location) instance
        (setf start-location location
              distance       (distance location destination)
              delta          (coord- destination location)))
      (schedule-at instance (periodic-update-message instance)
                   :time (+ (simulation-time) (update-interval instance))))))

(defmethod handle-message((instance line-mobility) message)
  (if (eql message (periodic-update-message instance))
      (with-slots(start-location destination speed
                                    delta) instance
       (let ((distance-travelled (* (simulation-time) speed)))
         (multiple-value-bind(n d)
             (floor distance-travelled distance)
           (setf (location instance)
                 (coord+ start-location
                         (coord* delta (if (evenp n) d (1- d)))))))
       (schedule-at instance message
                    :time (+ (simulation-time) (update-interval instance))))
      (error 'unknown-message :module instance :message message)))

