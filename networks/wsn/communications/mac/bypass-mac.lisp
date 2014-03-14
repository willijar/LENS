;; bypass MAC - a transparent MAC layer
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

(defclass bypass-mac(mac)
  ((header-overhead :initform 8)
   (buffer-size :initform 0)
   (max-mac-frame-size :initform 0))
  (:metaclass module-class))

(defmethod handle-message((instance bypass-mac) (packet routing-packet))
  ;; from routing layer
  (let ((mac-packet (encapsulate instance packet)))
    (setf (destination mac-packet) (next-hop (control-info packet)))
    (to-radio instance mac-packet)
    (to-radio instance '(set-state . tx))))

(defmethod handle-message((instance bypass-mac) (packet mac-packet))
  ;; from radio layer
    (when (or (eql (destination packet) (mac-address instance))
              (eql (destination packet) broadcast-mac-address))
      (let ((routing-packet (decapsulate packet)))
        (setf (control-info routing-packet)
              (make-instance 'net-mac-control-info
                             :rssi (rssi (control-info packet))
                             :lqi (lqi (control-info packet))))
        (to-network instance routing-packet))))
