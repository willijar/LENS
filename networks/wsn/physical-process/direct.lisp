;; Physical proceess assigning values to nodes directly
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

(defclass direct-node-physical-process(physical-process)
  ((default-value :initform 0.0 :parameter t :type real)
   (assigned-values :type read :initform nil :parameter t
                    :documentation "Assigned values in a range specification"))
  (:metaclass module-class)
  (:documentation "Physical process where value is assigned per node"))

(defmethod measure((instance direct-node-physical-process)
                   measurand location time)
  (declare (ignore measurand location time))
  (with-slots(default-value assigned-values) instance
    (if (range-list-p assigned-values)
        (or (range-getf assigned-values (nodeid (node *context*)))
            default-value)
        default-value)))

