;; bypass-routing - a transparent routing layer implementation
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

(defclass bypass-routing(routing)
  ((header-overhead :initform 10)
   (buffer-size :initform 32)
   (max-net-frame-size :initform 0))
  (:metaclass module-class))

(defmethod handle-message((instance bypass-routing) (packet application-packet))
  ;; from application layer
  (let* ((destination (destination (control-info packet)))
         (routing-packet
          (encapsulate
           (make-instance 'routing-packet
                          :name (class-name (class-of instance))
                          :header-overhead (header-overhead instance)
                          :source (network-address (node instance))
                          :destination destination
                          :sequence-number (next-sequence-number instance))
           packet)))
    (to-mac instance routing-packet
            (resolve-network-address instance destination))))
