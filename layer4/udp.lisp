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
  ((protocol-number :initform 17 :accessor protocol-number :allocation :class)
   (src-port :type ipport :initform 0 :accessor src-port :initarg :src-port)
   (dst-port :type ipport :initform 0 :accessor dst-port :initarg :dst-port)
   (msg-size :type word :initform 0 :accessor msg-size :initarg :msg-size)
   (checksum :type word :accessor checksum :initarg :checksum)
   (sequence-number
    :type seq :initform 0 :reader sequence-number
    :initarg :sequence-number
    :documentation "Sequence id's are not part of UDP, but are useful for performing statistics in the simulation"))
  (:documentation "UDP PDU class"))

(defmethod length-bytes((h udp-header)) 8)

(defmethod copy((h udp-header))
  (copy-with-slots h '(src-port dst-port msg-size checksum seq)))

(defmethod pdu-trace((pdu udp-header) detail os &key packet text)
  (declare (ignore packet))
  (format os " ~@[~A~] L4-UDP" text)
  (write-pdu-slots
   pdu
   '(src-port dst-port msg-size (checksum "~4,'0X") seq)
   detail
   os))

(defvar *default-udp-packet-size* 512 "Default UDP datagram size in octets")

(defclass udp-pending()
  ((data :initarg :data :reader data)
   (dst-address :initarg :dst-address :reader dst-address)
   (dst-port :initarg :dst-port :reader dst-port)
   (bytes-sent :initform 0 :accessor bytes-sent))
  (:documentation "Record of pending udp requests"))

(defmethod print-object((p udp-pending) stream)
  (print-unreadable-object(p stream :type t :identity t)
    (format stream "~:/print-eng/bytes/~:/print-eng/bytes sent"
            (bytes-sent p) (length-bytes (data p)))))

(defclass udp-dmux(protocol-dmux)
  ()
  (:documentation "Protocol demultiplexer for received UDP packets"))

(register-protocol 'udp-dmux 17)

(defclass udp(protocol-implementation)
  ((protocol-number :type 'fixnum :initform 17 :allocation :class
                    :reader protocol-number)
   (packet-size :initform *default-udp-packet-size*
                :initarg :packet-size :accessor packet-size
                :documentation "Size of packets")
   (sequence-number :type seq :initform 0 :accessor sequence-number
        :documentation "Sequence number of next packet")
   (pending-data :type list :initform nil :accessor pending-data
                 :documentation "Queue of pending data"))
  (:documentation "A model of the User Datagram Protocol."))

(defmethod default-trace-detail((protocol udp))
  `(src-port dst-port msg-size))

(defmethod open-connection(peer-address peer-port (udp udp))
  (call-next-method)
  (when-bind(application (layer5:application udp))
    (connection-complete application udp :failure nil)))

(defmethod receive((udp udp) (packet packet) (layer3 layer3:ipv4)
                   &key src-address &allow-other-keys)
  (let ((udphdr (pop-pdu packet)))
    (receive (layer5:application udp) (pop-pdu packet) udp
             :src-address src-address
             :src-port (src-port udphdr)
             :fid (fid packet)
             :packet-created (packet:created packet)
             :sequence-number (sequence-number uphdr))))

(defmethod send(application data (udp udp)
                &key (dst-address (peer-address udp))
                (dst-port (peer-port udp)) &allow-other-keys)
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
                         :dst-address dst-address)
          (pending-data udp))))
  (udp-send-pending udp))

(defmethod close-connection((udp udp))
  (call-next-method))

(defmethod reset((udp udp))
  (dolist(interface (interfaces (node udp)))
    (delete-notifications udp interface))
  (while (dequeue (pending-data udp))))

(defun udp-send-pending(udp)
  (dolist(pd (pending-data udp))
    (when-bind (interface (find-interface (dst-address pd) (node udp)))
      (loop
           (let ((data (if (and (= (bytes-sent pd) 0)
                                (< (length-bytes (data pd))
                                   (packet-size udp)))
                           (data pd)
                           (layer5:copy-from-offset
                            (packet-size udp) (bytes-sent pd) (data pd) ))))
             (unless (layer1:buffer-available-p
                      (+ 28 (length-bytes data)) interface)
               (add-notification udp #'udp-send-pending interface)
               (return))
             (let ((packet (make-instance 'packet :data data :fid (fid udp))))
               (incf (bytes-sent pd) (length-bytes data))
               (push-pdu
                (make-instance 'udp-header
                               :src-port (local-port udp)
                               :dst-port (dst-port pd)
                               :msg-size (length-bytes data)
                               :sequence-number (incf (sequence-number udp)))
                packet)
               (send (find-protocol 'layer3:ipv4 (node udp)) packet udp
                     :dst-address (dst-address pd)
                     :src-address (src-address udp)
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
