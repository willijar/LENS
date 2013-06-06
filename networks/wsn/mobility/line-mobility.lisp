(in-package :lens.wsn)

(defclass line-mobility(mobility)
  ((start-location :type coord :reader start-location)
   (destination
    :parameter t :type coord :initform (make-coord :x 1.0d0 :y 1.0d0)
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
  (:metaclass module-class)
  (:default-initargs :static-p nil))

(defmethod initialize-instance :after ((instance line-mobility)
                                       &key &allow-other-keys)
  (with-slots(start-location distance delta location destination) instance
       (setf start-location location
             distance       (distance location destination)
             delta          (coord- destination location))))

(defmethod startup((instance line-mobility))
  (schedule-at instance (periodic-update-message instance) :delay 0)
  (schedule-at instance (periodic-update-message instance)
               :delay (update-interval instance)))

(defmethod shutdown((instance line-mobility))
  (cancel (periodic-update-message instance)))

(defmethod handle-message((instance line-mobility) message)
  (if (eql message (periodic-update-message instance))
      (with-slots(start-location distance speed delta) instance
        (let ((distance-travelled (* (simulation-time) speed)))
          (multiple-value-bind(n d)
              (floor distance-travelled distance)
            (setf (location instance)
                  (coord+ start-location
                          (coord* delta (if (evenp n) d (1- d)))))))
        (schedule-at instance message :delay (update-interval instance)))
      (warn 'unknown-message :module instance :message message)))

