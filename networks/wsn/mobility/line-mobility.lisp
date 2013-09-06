(in-package :lens.wsn)

(defclass line-mobility(mobility)
  ((start-location :type coord :reader start-location)
   (destination
    :parameter t :type coord :initform (make-coord 1.0 1.0)
    :initarg :destination-location)
   (delta :type coord :documentation "Vector delta from start to end")
   (distance :type real
             :documentation "Distance from start to end")
   (speed :parameter t :type real :initform 1 :initarg :speed
          :documentation "Speed of motion")
   (update-interval
    :parameter t :type time-type :reader update-interval :initform 1d0
    :documentation "Interval for position updates along trajectory")
   (update :type timer-message :initform (make-instance 'timer-message)))
  (:metaclass module-class)
  (:default-initargs :static-p nil))

(defmethod initialize-instance :after ((instance line-mobility)
                                       &key &allow-other-keys)
  (with-slots(start-location distance delta location destination) instance
       (setf start-location location
             distance       (distance location destination)
             delta          (coord- destination location))))

(defmethod initialize list ((instance line-mobility) &optional (stage 0))
  (case stage
    (0 (set-timer instance 'update 0d0)))
  t)

(defmethod shutdown((instance line-mobility))
  (cancel-timer instance 'update))

(defmethod handle-timer((instance line-mobility) (timer (eql 'update)))
  (with-slots(start-location distance speed delta) instance
    (let ((distance-travelled
           (coerce (* (simulation-time) speed) 'single-float)))
      (multiple-value-bind(n d)
          (floor (/ distance-travelled distance))
        (setf (location instance)
              (coord+ start-location
                      (coord* delta (if (evenp n) d (- 1 d)))))))
    (set-timer instance 'update (update-interval instance))))

