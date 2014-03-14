;; Physical process and measurement for Wireless Sensor Networks
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

(defgeneric measure(physical-process measurand location time)
  (:documentation "Return the real value of specified measurand from a
  physical process at given time and location"))

(defclass physical-process(module)
  ((description :type string
                :parameter t :initarg :description :reader description
                :documentation "Description of physical process - default ")
   (function
    :parameter t :type function
    :initform #'(lambda(m c tm) (declare (ignore m c tm)) (uniform 0.0 1.0))
    :properties (:format (eval :type function))
    :reader physical-process-function
    :documentation "a real function of measurand, location and time"))
  (:metaclass module-class))

(defmethod configure :after((instance physical-process))
  (unless (slot-boundp instance 'description)
    (setf (slot-value  instance 'description)
          (string (class-name (class-of instance))))))

;; (defmethod measure :around ((physical-process physical-process)
;;                             measurand
;;                             location time)
;;   (let ((*context* physical-process))
;;     (call-next-method)))

(defmethod measure((physical-process physical-process)
                   (measurand (eql 'temperature))
                   (location coord) (time real))
  (funcall (physical-process-function physical-process)
           measurand location time))
