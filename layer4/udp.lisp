;; UDP implementation
;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :protocol.layer4)

(defclass udp-header(pdu)
  ((src-port :type (unsigned-byte 16)
             :initform 0 :accessor src-port :initarg :src-port)
   (dst-port :type  (unsigned-byte 16)
             :initform 0 :accessor dst-port :initarg :dst-port)
   (msg-size :type  (unsigned-byte 16)
             :initform 0 :accessor msg-size :initarg :msg-size)
   (checksum :type  (unsigned-byte 16)
             :initform 0 :accessor checksum :initarg :checksum)
   (sequence-number
    :type integer :initform 0 :reader sequence-number
    :initarg :sequence-number
    :documentation "Sequence id's are not part of UDP, but are useful
    for performing statistics in the simulation"))
  (:documentation "UDP PDU class"))

(defmethod name((h udp-header)) "UDP")

(defmethod length-bytes((h udp-header)) 8)

(defmethod trace-format ((h udp-header))
  '(src-port dst-port msg-size (checksum "~4,'0X") sequence-number))

(defclass udp-pending()
  ((data :initarg :data :reader data)
   (dst-address :initarg :dst-address :reader dst-address)
   (dst-port :initarg :dst-port :reader dst-port)
   (bytes-sent :initform 0 :accessor bytes-sent))
  (:documentation "Record of pending udp requests"))

(defmethod print-object((p udp-pending) stream)
  (print-unreadable-object(p stream :type t :identity t)
    (format stream "~D/~D bytes sent"
            (bytes-sent p) (length-bytes (data p)))))

(defclass udp-dmux(protocol-dmux)
  ()
  (:documentation "Protocol demultiplexer for received UDP packets"))

(layer4:register-protocol 'udp-dmux 17)

(defclass udp(protocol)
  ((protocol-number :type fixnum :initform 17 :allocation :class
                    :reader protocol-number)
   (max-seg-size :initform 556 :initarg :max-seg-size :accessor max-seg-size
        :documentation "Maximum segment size to use")
   (sequence-number :type integer :initform 0 :accessor sequence-number
                    :documentation "Sequence number of next packet")
   (pending-data :type list :initform nil :accessor pending-data
                 :documentation "Queue of pending data"))
  (:documentation "A model of the User Datagram Protocol."))

(defmethod copy((udp udp) &optional (copy (allocate-instance (class-of udp))))
  (setf (pending-data copy) nil)
  (call-next-method udp copy))

(defmethod default-trace-detail((protocol udp))
  `(type src-port dst-port msg-size))

(defmethod open-connection(peer-address peer-port (udp udp))
  (call-next-method)
  (connection-complete (application udp) udp))

(defmethod close-connection((udp udp))
  (call-next-method)
  (connection-closed (layer5:application udp) udp))

(defmethod receive((udp udp) (packet packet) (layer3 layer3:ipv4)
                   &key src-address &allow-other-keys)
  (let ((udphdr (pop-pdu packet)))
    (receive (layer5:application udp) (pop-pdu packet) udp
             :src-address src-address
             :src-port (src-port udphdr)
             :fid (fid packet)
             :packet-created (packet:created packet)
             :sequence-number (sequence-number udphdr))))

(defmethod send((udp udp) (data layer5:data) application
                &key (dst-address (peer-address udp))
                (dst-port (peer-port udp)) &allow-other-keys)
  (declare (ignore application))
  (assert (and dst-address dst-port)
          (dst-address dst-port)
          "send of data to ~A:~A invalid for UDP" dst-address dst-port)
  (setf (pending-data udp)
        (nconc
         (pending-data udp)
         (list
          (make-instance 'udp-pending
                         :data data
                         :dst-port dst-port
                         :dst-address dst-address))))
  (udp-send-pending udp))

(defmethod reset((udp udp))
  (setf (pending-data udp) nil))

(defun udp-send-pending(udp)
  (dolist(pd (pending-data udp))
    (when-bind (interface (find-interface (dst-address pd) (node udp)))
     (loop
        (let ((data
               (layer5:data-subseq
                (data pd) (bytes-sent pd) (max-seg-size udp))))
          (unless (layer1:buffer-available-p
                   (+ 28 (length-bytes data)) interface)
            (add-notification udp #'udp-send-pending interface)
            (return))
          (incf (bytes-sent pd) (length-bytes data))
          (let ((packet (make-instance 'packet :data data :fid (fid udp))))
            (push-pdu
             (make-instance 'udp-header
                            :src-port (or (local-port udp) 0)
                            :dst-port (dst-port pd)
                            :msg-size (length-bytes data)
                            :sequence-number (incf (sequence-number udp)))
             packet)
            (send (layer3:find-protocol 'layer3:ipv4 (node udp)) packet udp
                  :dst-address (dst-address pd)
                  :src-address (or (local-address udp)
                                   (network-address (node udp)))
                  :ttl (ttl udp)
                  :tos (tos udp))
            (sent (application udp) (length-bytes data) udp)))
        (when (= (bytes-sent pd) (length-bytes (data pd)))
          (setf (pending-data udp) (delete pd (pending-data udp)))
          (return)))))
  ;; udp will call connection closed once all data has been sent
  (unless (or (pending-data udp) (connected-p udp))
    (dolist(interface (interfaces (node udp)))
      (delete-notifications udp interface))))
