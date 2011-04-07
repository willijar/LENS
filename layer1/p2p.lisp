;; A simple point-to-point link connecting two interfaces
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer1)

(defclass point-to-point(link)
  ((delay
    :type real :initarg :delay :initform *default-delay*
    :documentation "Link Propagation Delay in sec")
   (bit-error-rate  :initarg :bit-error-rate
                    :initform *default-ber*
                    :documentation "Bit Error Rate for this link")
   (simplex-p :type boolean :initarg :simplex-p :initform nil :reader simplex-p
              :documentation "If true this is a simplex link")
   (unidirectional-p :type boolean :initarg :unidirectional-p :initform nil
                     :reader unidirectional-p
                     :documentation "If true this link is unidirectional")
   (local-interface
    :type interface :initarg :local-interface :reader local-interface
    :documentation "The local (sending) interface of this link")
   (peer-interface
    :initform nil :type interface
    :initarg :peer-interface :reader peer-interface
    :reader default-peer-interface
    :documentation "The remote (receiving) interface on this link"))
  (:documentation "A serial point to point link."))

(defmethod send :before ((link point-to-point) packet interface
                         &key &allow-other-keys)
  (when (and (unidirectional-p link)
             (not (eql interface (local-interface link))))
    (error "Attempt to send packet the wrong way along a unidirectional link"))
  (when (and (simplex-p link)
             (slot-value (local-interface link) 'tx-packet)
             (slot-value (peer-interface link) 'tx-packet))
    (error "Attempt to send a packet in both directions over a simplex link")))

(defmethod busy-p((link point-to-point))
  ;; Note this ignores propagation delay between peers for a simplex link
  (let ((l (slot-value (local-interface link) 'tx-packet))
        (p (slot-value (peer-interface link) 'tx-packet)))
    (cond((unidirectional-p link)  l)
         ((simplex-p link) (or l p)))))

(defmethod interfaces((link point-to-point))
  (list (local-interface link) (peer-interface link)))

(defmethod peer-interfaces((link point-to-point) interface)
  (if (eql interface (local-interface link))
      (list (peer-interface link))
      (unless (unidirectional-p link)
        (list (local-interface link)))))

(defun point-to-point(a b &key
                      (delay *default-delay*)
                      (bandwidth *default-bandwidth*)
                      (bit-error-rate *default-ber*)
                      simplex-p
                      unidirectional-p
                      (local-address (network-address a))
                      (peer-address (network-address b))
                      local-mask
                      peer-mask)
  (make-instance
   'point-to-point
   :delay delay
   :bandwidth bandwidth
   :bit-error-rate  bit-error-rate
   :simplex simplex-p
   :unidirectional unidirectional-p
   :local-interface (make-instance
                     'interface
                     :node a
                     :network-address local-address
                     :network-mask local-mask)
   :peer-interface (make-instance
                    'interface
                    :node b
                    :network-address peer-address
                    :network-mask peer-mask)))
