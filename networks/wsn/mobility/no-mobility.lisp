(in-package :lens.wsn)

(defclass no-mobility(mobility)
  ()
  (:metaclass module-class)
  (:default-initargs :static-p t)
  (:documentation "Just an alias for mobility for onsistency in
  parameter naming"))

(defmethod initialize-instance :after ((instance no-mobility)
                                       &key &allow-other-keys)
  (parse-deployment instance))

(defun parse-deployment(instance)
  (let* ((node (node instance))
         (network (network node))
         (xlen (coord-x (field network)))
         (ylen (coord-y (field network)))
         (zlen (coord-z (field network))))
    (multiple-value-bind(deployment start-index)
        (let ((deployment (deployment network)))
          (if (range-list-p deployment)
              (range-getf (deployment network) (index node))
              (values deployment 0)))
      (when deployment
        (setf
         (slot-value instance 'location)
         (cond
           ((eql deployment 'uniform)
            (make-coord (uniform 0 xlen) (uniform 0 ylen) (uniform 0 zlen)))
           ((eql deployment 'center)
            (make-coord (/ xlen 2) (/ ylen 2) (/ zlen 2)))
           ((and (listp deployment)
                 (member (first deployment) '(grid randomized)))
            (let* ((gridi (- (index node) start-index))
                   (gridx (second deployment))
                   (gridy (third deployment))
                   (gridz (fourth deployment))
                   (grid
                    (make-coord
                     (* (mod gridi gridx) (/ xlen (1- gridx)))
                     (* (mod (floor gridi gridx) gridy) (/ ylen (1- gridy)))
                     (if (and gridz zlen)
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
                      (rn xlen gridx)
                      (rn ylen gridy)
                      (if (and gridz zlen) (rn zlen gridz) 0.0))))
                grid)))
         (t (error "Unknown deployment parameter: ~A" deployment))))))))