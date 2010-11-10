;; $Id$
;; Link Layer
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer2)

(defenumeration (link-layer-frame-type (unsigned-byte 32))
  ((ll-data  #x0001)
   (ll-ack   #x0010)))

(defclass link-layer-header(pdu)
  ((frame-type :type link-layer-frame-type :reader frame-type
               :initarg :frame-type)
   (seqno :type seqno :reader seqno :initarg :seqno
          :documentation "Sequence Number")
   (ackno :type seqno :reader ackno :initarg :ackno
          :documentation "Acknowledgement number")
   (bopno :type seqno :reader bopno :initarg :bopno
          :documentation "Beginning of packet seqno")
   (eopno :type seqno :reader eopno :initarg :eopno
          :documentation "End of packet seqno")
   (packet-size :type (integer 0)  :reader packet-size
                :documentation "Size of packet")
   (send-time :type time-type :reader send-time
              :documentation "Time the packet is sent")))


(defclass link-layer()
  ((seqno :type seqno :reader seqno :initarg :seqno
          :documentation "Sequence Number")
   (ackno :type seqno :reader ackno :initarg :ackno
          :documentation "Acknowledgement number received so far")
   (destination-address :type mac-addr :reader destination-address
                        :documentation "Destination mac address")
   (mac :type mac :reader mac :initarg mac
        :documentation "MAC object")
