;; Mobility model for motion along a line
;; Copyright (C) 2014 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; LENS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
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

