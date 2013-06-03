(in-package :lens.wsn)

(defclass mobility(wsn-module)
  ((location :parameter t :type coord :initarg :location :reader location
             :documentation "starting location")
   (cell :type integer :documentation "cell ID coresponding to  x,y,z")
   (theta :parameter t :initform 0.0d0 :type double-float
          :documentation "Orientation provided by angles")
   (phi :parameter t :initform 0.0d0 :type double-float))
  (:metaclass module-class))

(defgeneric (setf location)(location instance)
  (:documentation "Change location in mobility manager")
  (:method((location coord) (instance mobility))
    (let ((old (location instance)))
      (setf (slot-value instance 'location) location)
      (emit instance 'node-move
            (make-instance 'node-move :from old :to location)))))

(defmethod configure :after ((instance mobility))
  (parse-deployment instance)
  (eventlog "~A initial location is ~A" (node instance) (location instance)))

(defun parse-deployment(instance)
  (let* ((network (network instance))
         (node (node instance))
         (xlen (coord-x (field network)))
         (ylen (coord-y (field network)))
         (zlen (coord-z (field network))))
    (multiple-value-bind(deployment start-index)
        (range-getf (deployment network) (index node))
      (setf
       (slot-value instance 'location)
       (cond
         ((not deployment)
          (read-parameter node 'coord 'coord))
         ((eql deployment 'uniform)
          (make-coord :x (uniform 0 xlen)
                      :y (uniform 0 ylen)
                      :z (uniform 0 zlen)))
         ((eql deployment 'center)
          (make-coord :x (/ xlen 2) :y (/ ylen 2) :z (/ zlen 2)))
         ((and (listp deployment)
               (member (first deployment) '(grid randomized)))
          (let* ((gridi (- (index node) start-index))
                 (gridx (second deployment))
                 (gridy (third deployment))
                 (gridz (fourth deployment))
                 (grid
                  (make-coord
                   :x (* (mod gridi gridx) (/ xlen (1- gridx)))
                   :y (* (mod (floor gridi gridx) gridy) (/ ylen (1- gridy)))
                   :z (if (and gridz zlen)
                          (* (mod (floor gridi (* gridx gridy)) gridz)
                             (/ zlen (1- gridz)))
                          0.0))))
            (if (eql (first deployment) 'randomized)
                (flet((rn(len grid)
                        (let ((s (/ len grid)))
                          (min (- s) (max s (normal 0 (* 0.3 s)))))))
                  (coord+
                   grid
                   (make-coord
                    :x (rn xlen gridx)
                    :y (rn ylen gridy)
                    :z (if (and gridz zlen) (rn zlen gridz) 0.0))))
                grid)))
         (t (error "Unknown deployment parameter: ~A" deployment)))))))
